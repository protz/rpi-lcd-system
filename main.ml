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
    "1.1 FIP", Func Radios.thread_fip;
    "1.2 France Q", Func Radios.thread_franceq;
  ];

  "2. PulseAudio", Menu [
    "2.1 Equalizer", Func (fun () -> Lwt_io.printl "entry 5");
    "2.2 Stream Name", Func Pulse.thread;
  ];

  "3. Misc", Menu [
    "3.1 Time 1", Func Heure.thread1;
    "3.2 Time 2", Func Heure.thread2;
  ];

  "4. Test", Menu [
    "4.1 Autoscroll", Func (fun () ->
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
    "4.2 Segments", Func (fun () ->
      (* Since we're bypassing the display thread for this test, wait for a
       * while the display thread is quiet. *)
      lwt () = Lwt_unix.sleep 0.5 in
      LCD.clear ();
      LCD.home ();
      Misc.write_special_twodigits (1, 2) 0;
      LCD.move_cursor_abs (5, 0);
      LCD.message "heures";
      LCD.move_cursor_abs (5, 1);
      LCD.message ":50 minutes";
      Button.wait_for_button ()
    );
  ];

  "5. System", Menu [
    "5.1 Power off", Func (fun () -> lwt _ = Lwt_unix.system "sudo /sbin/shutdown -h now" in Lwt.return ());
    "5.2 Reboot", Func (fun () -> lwt _ = Lwt_unix.system "sudo /sbin/shutdown -r now" in Lwt.return ());
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
            Display.display "\n  ... starting";
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
  let rec loop () =
    lwt () = handle_menu main_menu in

    LCD.clear ();
    LCD.backlight LCD.off;
    LCD.message "Display is off\nPress to wakeup";

    lwt () = Button.wait_for_button () in

    LCD.backlight LCD.teal;

    loop ()
  in

  loop()
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
;;

let _ =
  (* This variable is not set if we're being run from /etc/rc.local. *)
  Unix.putenv "PULSE_SERVER" "localhost:4713";

  Lwt_main.run main;
;;

