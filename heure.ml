open Lcd
open Message

let thread1 () =
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
    LCD.message (Printf.sprintf ":%2d minute%s" tm.Unix.tm_min
      (if tm.Unix.tm_min > 1 then "s" else ""));
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

let thread2 () =
  let f () =
    let tm = Unix.localtime (Unix.time ()) in
    let digit1 = tm.Unix.tm_hour / 10 in
    let digit2 = tm.Unix.tm_hour mod 10 in
    let digit3 = tm.Unix.tm_min / 10 in
    let digit4 = tm.Unix.tm_min mod 10 in
    let digit5 = tm.Unix.tm_sec / 10 in
    let digit6 = tm.Unix.tm_sec mod 10 in
    LCD.clear ();
    LCD.home ();
    Misc.load_blocks ();
    Misc.block_digit digit1 0;
    Misc.block_digit digit2 2;
    Misc.block_colon 4;
    Misc.block_digit digit3 5;
    Misc.block_digit digit4 7;
    Misc.block_colon 9;
    Misc.block_digit digit5 10;
    Misc.block_digit digit6 12;
  in
  let rec loop () =
    Display.display_special f;
    lwt () = Lwt_unix.sleep 1. in
    loop ()
  in
  Lwt.pick [
    Button.wait_for_button ();
    loop ()
  ]

