open Lcd

module CQ = Cqueue

(** A wrapper for the LCD buttons. *)

type button =
  | Select
  | Right
  | Down
  | Up
  | Left

let button_of_constant x =
  if x = LCD.select then Select
  else if x = LCD.right then Right
  else if x = LCD.down then Down
  else if x = LCD.up then Up
  else if x = LCD.left then Left
  else assert false
;;

(** The types of the messages that a thread sends to another. One type of
 * message for everyone. *)

type message =
  | ButtonsPressed of button list
  | ButtonPressed of button
  | DisplayImmediate of string
  | DisplayBackground of string

(** The various message queues *)

let menu_queue: message CQ.t =
  CQ.create ()
;;

let display_queue: message CQ.t =
  CQ.create ()
;;

(** Our display logic. *)

let send_display_msg ?(background: unit option) (msg: string): unit =
  CQ.add display_queue begin
    match background with
    | Some () ->
        DisplayBackground msg
    | None ->
        DisplayImmediate msg
  end
;;

let display_thread () =
  let rec loop () =
    lwt msg = CQ.take display_queue in
    begin match msg with
    | DisplayImmediate msg ->
        LCD.clear ();
        LCD.home ();
        LCD.message msg;
    | DisplayBackground _ ->
        failwith "TODO"
    | _ ->
        ()
    end;
    loop ()
  in
  loop ()
;;


(** Our menu logic. *)

type menu =
  (string * entry) list

and entry =
  | Menu of menu
  | Func of (unit -> unit Lwt.t)

let main_menu : menu = [
  "Radios", Menu [
    "FIP", Func (fun () -> Lwt_io.printl "entry 1");
    "France Culture", Func (fun () -> Lwt_io.printl "entry 2");
  ];

  "PulseAudio", Menu [
    "Equalizer", Func (fun () -> Lwt_io.printl "entry 5");
    "Stream Name", Func (fun () -> Lwt_io.printl "entry 10");
  ];
]

let handle_menu (m: menu) =
  let rec menu before entry after =
    send_display_msg (fst entry);
    let continue () = menu before entry after in
    lwt msg = CQ.take menu_queue in
    match msg with
    | ButtonPressed Down ->
        begin match after with
        | next_entry :: other_entries ->
            menu (entry :: before) next_entry other_entries
        | [] ->
            start_menu (List.rev (entry :: before))
        end
    | ButtonPressed Left ->
        Lwt.return ()
    | ButtonPressed Right ->
        begin match snd entry with
        | Menu m ->
            lwt () = start_menu m in
            continue ()
        | Func f ->
            lwt () = f () in
            continue ()
        end;
    | ButtonPressed Up ->
        begin match before with
        | prev_entry :: other_entries ->
            menu other_entries prev_entry (entry :: after)
        | [] ->
            begin match List.rev (entry :: after) with
            | hd :: tl ->
                menu tl hd []
            | _ ->
                assert false
            end
        end
    | _ ->
        continue ()

  and start_menu (m: menu): unit Lwt.t =
    match m with
    | entry :: entries ->
        menu [] entry entries
    | [] ->
        failwith "No empty menus"

  in

  start_menu m
;;

let menu_thread () =
  handle_menu main_menu
;;

(** The thread that reads the LCD's buttons and pushes them to the various
 * message queues. *)

let send_button_messages q buttons =
  CQ.add q (ButtonsPressed buttons);
  CQ.add q (ButtonPressed (List.hd buttons))
;;

let button_thread () =
  let r = ref (LCD.buttons_pressed ()) in
  let buttons = LCD.([select; right; down; up; left]) in
  let button_became_pressed old_r new_r btn =
    let v = 1 lsl btn in
    (old_r land v = 0) && (new_r land v <> 0)
  in
  let rec loop () =
    let new_r = LCD.buttons_pressed () in
    if new_r <> !r then begin
      Printf.eprintf "%s → %s\n%!" (Lib.print_binary !r) (Lib.print_binary new_r);
      let buttons =
        List.filter (button_became_pressed !r new_r) buttons |>
        List.map button_of_constant;
      in
      if List.length buttons > 0 then
        send_button_messages menu_queue buttons;
    end;
    r := new_r;
    lwt () = Lwt_unix.sleep 0.001 in
    loop ()
  in
  loop ()
;;

let main: unit Lwt.t =
  let busnum = if Pi.get_revision () = 2 then 1 else 0 in
  LCD.init ~busnum ();
  LCD.blink true;
  LCD.cursor true;
  LCD.backlight LCD.yellow;

  Misc.draw_caml_logo ();

  lwt () = Lwt_unix.sleep 1. in

  LCD.backlight LCD.teal;

  Lwt.choose [
    (* This one isn't meant to terminate. *)
    button_thread ();
    (* This one either. *)
    display_thread ();
    (* So any of them terminates, it's this one, meaning the program's over! *)
    menu_thread ()
  ]

let _ =
  Lwt_main.run main;

  LCD.clear ();
  LCD.home ();
  LCD.message "Exited"
;;

