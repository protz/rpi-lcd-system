open Message
open Lcd

module CQ = Cqueue


(** Our menu logic. *)

type menu =
  (string * entry) list

and entry =
  | Menu of menu
  | Func of (unit -> unit Lwt.t)

let main_menu : menu = [
  "1. Radios", Menu [
    "1.1 FIP", Func Fip.thread;
    "1.2 France Culture", Func (fun () -> Lwt_io.printl "entry 2");
  ];

  "2. PulseAudio", Menu [
    "2.1 Equalizer", Func (fun () -> Lwt_io.printl "entry 5");
    "2.2 Stream Name", Func (fun () -> Lwt_io.printl "entry 10");
  ];

  "3. Test", Menu [
    "3.1 Test autoscroll", Func (fun () ->
      (* Since we're bypassing the display thread for this test, wait for a
       * while the display thread is quiet. *)
      lwt () = Lwt_unix.sleep 2. in
      let str = Misc.fix_accents "Détendez-vous, vous êtes sur FIP !" in
      LCD.home ();
      LCD.message (String.sub str 0 16);
      LCD.autoscroll true;
      let l = String.length str in
      let rec loop i =
        lwt () = Lwt_unix.sleep 0.5 in
        if i = l then begin
          LCD.message " ";
          lwt () = Lwt_unix.sleep 0.5 in
          LCD.autoscroll false;
          Lwt.return ()
        end else begin
          LCD.message (String.sub str i 1);
          loop (i + 1)
        end
      in
      loop 16
      (* The conclusion of this test is: using the autoscroll features from the
       * LCD doesn't give visually better results than re-writing everything by
       * hand as the display thread does. *)
    );
  ];

]

let handle_menu (m: menu) =
  let rec menu before entry after =
    Display.clear ();
    Display.display (fst entry);
    let continue () = menu before entry after in
    lwt msg = CQ.take Button.queue in
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
            Printf.printf "[menu] function done\n%!";
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

let main: unit Lwt.t =
  let busnum = if Pi.get_revision () = 2 then 1 else 0 in
  LCD.init ~busnum ();
  LCD.backlight LCD.yellow;

  Misc.draw_caml_logo ();

  lwt () = Lwt_unix.sleep 3. in

  LCD.backlight LCD.teal;

  Lwt.choose [
    (* This one isn't meant to terminate. *)
    Button.thread ();
    (* This one either. *)
    Display.thread ();
    (* So any of them terminates, it's this one, meaning the program's over! *)
    menu_thread ()
  ]

let _ =
  Lwt_main.run main;

  LCD.clear ();
  LCD.backlight LCD.off;
;;

