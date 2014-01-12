open Message
open Lcd

module CQ = Cqueue

let queue: Message.t CQ.t =
  CQ.create ()
;;

(** Our display logic. *)

let display ?(background: unit option) (msg: string): unit =
  CQ.add queue begin
    match background with
    | Some () ->
        DisplayBackground (String msg)
    | None ->
        DisplayImmediate msg
  end
;;

let display_special f =
  CQ.add queue (DisplayBackground (Special f))

let clear () =
  display ~background:() ""
;;

let lcd_width = 16;;

let thread () =
  let split_lines lines =
    match Lib.split lines '\n' with
    | [ line1 ] ->
        line1, ""
    | [ line1; line2 ] ->
        line1, line2
    | _ ->
        failwith "Bad usage for split_lines"
  in

  let scroll (background_txt: background) (msg: string) (loop: bool): Message.t Lwt.t =
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
      (* If we're supposed to loop this message (i.e. it's a background scroll),
       * then wait for a little bit otherwise there's an infinite loop of scroll
       * / loop *)
      lwt () = if loop then Lwt_unix.sleep 0.5 else Lwt.return () in
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
        lwt () = Lwt_unix.sleep 0.5 in
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
    lwt next_msg = CQ.take queue in
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
        lwt next_msg = scroll background_txt txt false in
        loop background_txt next_msg
    | DisplayBackground (String txt as background) ->
        lwt next_msg = Lwt.pick [
          take_msg ();
          scroll background txt true
        ] in
        loop background next_msg
    | DisplayBackground (Special f as background) ->
        f ();
        lwt next_msg = take_msg () in
        loop background next_msg
    | _ ->
        assert false
    end
  in

  lwt msg = take_msg () in
  loop (String "") msg
;;

