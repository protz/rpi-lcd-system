let json_url =
  "http://www.fipradio.fr/sites/default/files/direct-large.json"

type t = {
  mutable artist: string;
  mutable title: string;
  mutable album: string;
  mutable year: string;
  mutable cover_url: string;
}
 
type state = Before | Inside | After

exception Network_error

(* Fetch the JSON file from FIP's website, and then extract the raw HTML from it. *)
let rec fetch_html (json_url: string): string Lwt.t =
  (* The JSON has a very specific structure, so it's easy to extract the html
   * bits from it. *)
  lwt json = Cohttp_lwt_unix.Client.get (Uri.of_string json_url) in
  lwt json =
    match json with
    | Some (headers, body) ->
        let headers = Cohttp_lwt_unix.Client.Response.headers headers in
        begin match Cohttp.Header.get headers "location" with
        | Some loc ->
            Printf.printf "[fip] following redirect\n%!";
            fetch_html loc
        | None ->
            Cohttp_lwt_body.string_of_body body
        end
    | None ->
        Printf.printf "[fip] network error\n%!";
        raise Network_error
  in
  Printf.printf "[fip] got %d bytes\n%!" (String.length json);
  let json = Yojson.Safe.from_string json in
  let html =
    match json with
    | `Assoc ["html", `String s; _] -> s
    | _ -> assert false
  in
  Lwt.return html
;;

let polling_thread () =
  (* We keep a current state, and try to refresh it every five seconds. We use
   * structural comparison on entries to determine whether something changed or
   * not. *)
  let current_entry = ref {
    artist = "";
    title = "";
    year = "";
    cover_url = "";
    album = ""
  } in

  let rec loop () =
    Printf.printf "[fip] loop\n%!";
    try_lwt
      (* The entry that we're about to build. *)
      let entry = { artist = ""; title = ""; year = ""; cover_url = ""; album = "" } in
      (* We have a state machine. We're either before the current entry, inside
       * it, or we've gone past it. *)
      let state = ref Before in

      (* Fetch the html, split along the lines. *)
      lwt html = fetch_html json_url in
      let lines = Pcre.split ~pat:"\r?\n" html in

      (* Regexp-foo to fill in the current entry. *)
      List.iter (fun line ->
        if Pcre.pmatch ~pat:"class='direct-current'" line then
          state := Inside;

        let pat = "<div class=\"(artiste|titre|album|annee)\">([^<]+)" in

        begin try match Pcre.extract_opt ~pat line with
        | [| _whole; Some c; Some contents |] when !state = Inside ->
            if c = "artiste" then entry.artist <- contents;
            if c = "titre" then entry.title <- contents;
            if c = "album" then entry.album <- contents;
            if c = "annee" then entry.year <- contents;
        | _ ->
            ()
        with Not_found ->
          ()
        end;

        let pat = "<img src=\"\\([^\"]+\\)" in

        begin try match Pcre.extract_opt ~pat line with
        | [| _whole; Some url |] when !state = Inside ->
            entry.cover_url <- url
        | _ ->
            ()
        with Not_found ->
          ()
        end;

        if Pcre.pmatch ~pat:"class='direct-next'" line then
          state := After;
      ) lines;


      (* Did the current song change? *)
      if entry <> !current_entry then begin
        (* We have our new entry. *)
        current_entry := entry;

        (* Emit the actual notification. *)
        let msg = Printf.sprintf "%s \n%s - %s %s "
          !current_entry.title
          !current_entry.artist
          !current_entry.album
          !current_entry.year
        in
        Display.display ~background:() msg;
      end;

      lwt () = Lwt_unix.sleep 5. in
      loop ()

    with
    | Network_error
    | Yojson.Json_error _ ->
        Printf.printf "[fip] error\n%!";
        lwt () = Lwt_unix.sleep 5. in
        loop ()

  in

  loop ()
;;

let thread () =
  Printf.printf "[fip] starting fip thread\n%!";
  Lwt.pick [
    polling_thread ();
    Button.wait_for_button ();
  ]

