(** A wrapper for the LCD buttons. *)

open Message
open Lcd

module CQ = Cqueue

let button_of_constant x =
  if x = LCD.select then Select
  else if x = LCD.right then Right
  else if x = LCD.down then Down
  else if x = LCD.up then Up
  else if x = LCD.left then Left
  else assert false
;;

(** The various message queues *)

let queue: Message.t CQ.t =
  CQ.create ()
;;

(** The thread that reads the LCD's buttons and pushes them to the various
 * message queues. *)

let send_button_messages q buttons =
  CQ.add q (ButtonsPressed buttons);
  CQ.add q (ButtonPressed (List.hd buttons))
;;

let thread () =
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
        send_button_messages queue buttons;
    end;
    r := new_r;
    lwt () = Lwt_unix.sleep 0.001 in
    loop ()
  in
  loop ()
;;

let wait_for_button () =
  let rec loop () =
    lwt msg = CQ.take queue in
    match msg with
    | ButtonPressed _ ->
        Lwt.return ()
    | _ ->
        loop ()
  in
  loop ()
;;
