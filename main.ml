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

let clear_display_msg () =
  send_display_msg ~background:() ""
;;

let lcd_width = 16;;

let display_thread () =
  let split_lines lines =
    match Lib.split lines '\n' with
    | [ line1 ] ->
        line1, ""
    | [ line1; line2 ] ->
        line1, line2
    | _ ->
        failwith "Bad usage for split_lines"
  in

  let scroll (background_txt: string) (msg: string) (loop: bool): message Lwt.t =
    let msg = Misc.fix_accents msg in
    let line1, line2 = split_lines msg in
    let display str =
      LCD.clear ();
      LCD.home ();
      LCD.message str;
    in
    if msg = "" then
      (* Default value: we sleep, which results in the [loop] function waiting
       * for a message and nothing else. *)
      fst (Lwt.wait ())
    else if String.length line1 <= lcd_width && String.length line2 <= lcd_width then begin
      display msg;
      lwt () = Lwt_unix.sleep 1. in
      Lwt.return (DisplayBackground background_txt)
    end else
      (* Normalize by padding with spaces so that both have the same width. *)
      let width = max (String.length line1) (String.length line2) in
      let line1, line2 =
        line1 ^ String.make (width - String.length line1) ' ',
        line2 ^ String.make (width - String.length line2) ' '
      in
      let line1, line2 =
        if loop then
          line1 ^ " " ^ line1, line2 ^ " " ^ line2
        else
          let whitespace = String.make lcd_width ' ' in
          line1 ^ whitespace, line2 ^ whitespace
      in
      let rec scroll offset =
        display (
          String.sub line1 offset lcd_width ^ "\n" ^
          String.sub line2 offset lcd_width
        );
        lwt () = Lwt_unix.sleep 0.2 in
        if offset = width - lcd_width && not loop then
          (* Once we're done, the next action is: back to looping the background
           * text. *)
          Lwt.return (DisplayBackground background_txt)
        else if offset = width && loop then
          scroll 0
        else
          scroll (offset + 1)
      in
      scroll 0

  in


  let rec take_msg () =
    lwt next_msg = CQ.take display_queue in
    match next_msg with
    | DisplayImmediate _
    | DisplayBackground _ ->
        Lwt.return next_msg
    | _ ->
        take_msg ()
  in


  let rec loop background_txt msg =
    begin match msg with
    | DisplayImmediate txt ->
        lwt next_msg = Lwt.pick [
          take_msg ();
          scroll background_txt txt false
        ] in
        loop background_txt next_msg
    | DisplayBackground txt ->
        lwt next_msg = Lwt.pick [
          take_msg ();
          scroll txt txt true
        ] in
        loop txt next_msg
    | _ ->
        assert false
    end
  in

  lwt msg = take_msg () in
  loop "" msg
;;


(** FIP ! *)

let thread_fip () =
  send_display_msg ~background:() "Détendez-vous, vous êtes sur FIP!";
  lwt () = Lwt_unix.sleep 2. in
  send_display_msg "Mer de cristal, vous naviguez sur FIP!\n    (Oh yeah)";
  let rec loop () =
    lwt msg = CQ.take menu_queue in
    match msg with
    | ButtonPressed _ ->
        Lwt.return ()
    | _ ->
        loop ()
  in
  loop ()


(** Our menu logic. *)

type menu =
  (string * entry) list

and entry =
  | Menu of menu
  | Func of (unit -> unit Lwt.t)

let main_menu : menu = [
  "1. Radios", Menu [
    "1.1 FIP", Func thread_fip;
    "1.2 France Culture", Func (fun () -> Lwt_io.printl "entry 2");
  ];

  "2. PulseAudio", Menu [
    "2.1 Equalizer", Func (fun () -> Lwt_io.printl "entry 5");
    "2.2 Stream Name", Func (fun () -> Lwt_io.printl "entry 10");
  ];
]

let handle_menu (m: menu) =
  let rec menu before entry after =
    clear_display_msg ();
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

  lwt () = Lwt_unix.sleep 3. in

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
  LCD.blink false;
  LCD.cursor false;
  LCD.backlight LCD.off;
;;

