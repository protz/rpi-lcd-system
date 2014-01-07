open Lcd
open Message

let thread () =
  let f () =
    let tm = Unix.localtime (Unix.time ()) in
    let digit1 = tm.Unix.tm_hour / 10 in
    let digit2 = tm.Unix.tm_hour mod 10 in
    LCD.clear ();
    LCD.home ();
    Misc.write_special_twodigits (digit1, digit2) 0;
    LCD.move_cursor_abs (5, 0);
    LCD.message ("heure" ^ if tm.Unix.tm_hour > 1 then "s" else "");
    LCD.move_cursor_abs (4, 1);
    LCD.message (Printf.sprintf ":%d minute%s" tm.Unix.tm_min
      (if tm.Unix.tm_hour > 1 then "s" else ""));
  in
  let rec loop () =
    Display.display_special f;
    lwt () = Lwt_unix.sleep 60. in
    loop ()
  in
  Lwt.pick [
    Button.wait_for_button ();
    loop ()
  ]

