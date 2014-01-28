
type radio = {
  name: string;
  json_url: string;
  process_json: Yojson.Safe.json -> t;
  mp3_url: string;
}

and t = {
  mutable artist: string;
  mutable title: string;
  mutable album: string;
  mutable year: string;
  mutable cover_url: string;
}
 
type state = Before | Inside | After

exception Network_error

(* Fetch a JSON file. *)
let rec fetch_json (json_url: string): Yojson.Safe.json Lwt.t =
  (* The JSON has a very specific structure, so it's easy to extract the html
   * bits from it. *)
  lwt json = Cohttp_lwt_unix.Client.get (Uri.of_string json_url) in
  match json with
  | Some (headers, body) ->
      let headers = Cohttp_lwt_unix.Client.Response.headers headers in
      begin match Cohttp.Header.get headers "location" with
      | Some loc ->
          Printf.printf "[fip] following redirect\n%!";
          fetch_json loc
      | None ->
          lwt response = Cohttp_lwt_body.string_of_body body in
          let json = Yojson.Safe.from_string response in
          Lwt.return json
      end
  | None ->
      Printf.printf "[fip] network error\n%!";
      raise Network_error
;;

let process_json_fip (json: Yojson.Safe.json) =
  let html =
    match json with
    | `Assoc ["html", `String s; _] -> s
    | _ -> assert false
  in

  let lines = Pcre.split ~pat:"\r?\n" html in

  (* The entry that we're about to build. *)
  let entry = { artist = ""; title = ""; year = ""; cover_url = ""; album = "" } in

  (* We have a state machine. We're either before the current entry, inside
   * it, or we've gone past it. *)
  let state = ref Before in

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

  (* No entry! *)
  if !state = Before then begin
    entry.artist <- "Détendez-vous";
    entry.title <- "Vous êtes sur FIP!";
    entry.album <- "(En direct)";
    entry.year <- "";
  end;

  entry
;;

let fip = {
  name = "FIP";
  process_json = process_json_fip;
  json_url = "http://www.fipradio.fr/sites/default/files/direct-large.json";
  mp3_url = "http://mp3.live.tv-radio.com/fip/all/fiphautdebit.mp3";
};;

let process_json_franceq json =
  let assert_find_assoc json key =
    match json with
    | `Assoc entries ->
        List.assoc key entries
    | _ -> raise Not_found
  in
  let assert_list json =
    match json with
    | `List x -> x
    | _ -> raise Not_found
  in
  let assert_intlit json =
    match json with
    | `Intlit x -> float_of_string x
    | _ -> raise Not_found
  in
  let assert_string json =
    match json with
    | `String x -> x
    | _ -> raise Not_found
  in
  let timeline = assert_find_assoc json "timeline" in
  let emissions = assert_find_assoc timeline "emissions" in
  let emissions = assert_list emissions in
  let module M = struct exception Found of t end in
  let now = Unix.time () in
  try
    List.iter (fun emission ->
      let t_start = assert_intlit @@ assert_find_assoc emission "heure_debut" in
      let t_end = assert_intlit @@ assert_find_assoc emission "heure_fin" in
      if t_start <= now && now <= t_end then begin
        let entry = { artist = ""; title = ""; album = ""; year = ""; cover_url = ""; } in
        begin try
          entry.album <- assert_string @@ assert_find_assoc emission "title";
        with Not_found -> ();
        end;
        begin try match assert_find_assoc emission "personnes" with
          | `Assoc ((_, `String author) :: _) -> entry.artist <- author
          | _ -> raise Not_found
        with Not_found -> ();
        end;
        begin try
          let diffusions = assert_list @@ assert_find_assoc emission "diffusions" in
          List.iter (fun (diffusion: Yojson.Safe.json) ->
            let t_start = assert_intlit @@ assert_find_assoc diffusion "timestamp_debut" in
            let t_end = assert_intlit @@ assert_find_assoc diffusion "timestamp_fin" in
            if t_start <= now && now <= t_end then
              entry.title <- assert_string @@ assert_find_assoc diffusion "title";
          ) diffusions;
        with Not_found -> ();
        end;
        raise (M.Found entry)
      end;
    ) emissions;
    {
      artist = "Émission inconnue";
      title = "France Culture";
      album = "Le Direct";
      year = "";
      cover_url = ""
    };
  with M.Found e ->
    e
;;

let franceq = {
  name = "France Culture";
  process_json = process_json_franceq;
  json_url = "http://www.franceculture.fr/sites/default/files/rf_player/player-direct.json";
  mp3_url = "http://mp3lg.tdf-cdn.com/franceculture/all/franceculturehautdebit.mp3";
};;

let polling_thread radio =
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
    try_lwt

      (* Fetch the html, split along the lines. *)
      lwt json = fetch_json radio.json_url in

      let entry = radio.process_json json in

      (* Did the current song change? *)
      if entry <> !current_entry then begin
        (* We have our new entry. *)
        current_entry := entry;

        let sep = if !current_entry.artist <> "" then " - "  else "" in

        (* Emit the actual notification. *)
        let msg = Printf.sprintf "%s%s%s \n%s %s "
          !current_entry.artist
          sep
          !current_entry.title
          !current_entry.album
          !current_entry.year
        in
        Display.display ~background:() msg;
      end;

      lwt () = Lwt_unix.sleep 5. in
      loop ()

    with
    | e ->
        Printf.printf "[radio] exception %s\n%s\n%!"
          (Printexc.to_string e)
          (Printexc.get_backtrace ());
        lwt () = Lwt_unix.sleep 5. in
        loop ()
  in

  loop ()
;;

let launch_mplayer url =
  let child = Unix.fork () in
  if child = 0 then
    Unix.execvp "mplayer" [|
      "mplayer";
      url
    |]
  else
    child
;;

let thread radio =
  Printf.printf "[radio] starting %s thread\n%!" radio.name;
  let pid = launch_mplayer radio.mp3_url in
  lwt () = Lwt.pick [
    (* Doesn't terminate. *)
    polling_thread radio;
    (* Button press = exit. *)
    Button.wait_for_button ();
  ] in
  Unix.kill pid 15;
  Lwt.return ()

let thread_fip () =
  thread fip
;;

let thread_franceq () =
  thread franceq
;;
