
type radio = {
  name: string;
  json_url: string;
  process: string -> t;
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

(* Fetch a file. *)
let rec fetch_url (url: string): string Lwt.t =
  lwt contents = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
  match contents with
  | Some (headers, body) ->
      let headers = Cohttp_lwt_unix.Client.Response.headers headers in
      begin match Cohttp.Header.get headers "location" with
      | Some loc ->
          Printf.printf "[fip] following redirect\n%!";
          fetch_url loc
      | None ->
          lwt response = Cohttp_lwt_body.string_of_body body in
          Lwt.return response
      end
  | None ->
      Printf.printf "[fip] network error\n%!";
      raise Network_error
;;

(* -------------------------------- Json Helpers ---------------------------- *)

let assert_find_assoc json key =
  match json with
  | `Assoc entries ->
      List.assoc key entries
  | _ -> raise Not_found
;;
let assert_list json =
  match json with
  | `List x -> x
  | _ -> raise Not_found
;;
let assert_intlit json =
  match json with
  | `Intlit x -> float_of_string x
  | _ -> raise Not_found
;;
let assert_string json =
  match json with
  | `String x -> x
  | _ -> raise Not_found
;;
let key_string_or json key default =
  try begin match assert_find_assoc json key with
  | `String x -> x
  | _ -> default
  end with Not_found ->
    default
;;

(* -------------------------------- FIP ------------------------------------- *)

let process_fip (response: string) =
  let json = Yojson.Safe.from_string response in
  try
    let current = assert_find_assoc json "current" in
    let song = assert_find_assoc current "song" in
    let artist = key_string_or song "interpreteMorceau" "?" |> Misc.capitalize in
    let title = key_string_or song "titre" "?" |> Misc.capitalize in
    let album = key_string_or song "titreAlbum" "?" |> Misc.capitalize in
    let year = key_string_or song "anneeEditionMusique" "?" in
    let year = "(" ^ year ^ ")" in
    let cover_url =
      try
        let visuel = assert_find_assoc song "visuel" in
        key_string_or visuel "medium" ""
      with Not_found ->
        ""
    in

    { artist; title; album; year; cover_url }

  with e ->
    Printf.printf "%s\n%!" (Printexc.to_string e);
    Printexc.print_backtrace stdout;

    {
      artist = "Détendez-vous";
      title = "Vous êtes sur FIP";
      album = "(En direct)";
      year = "";
      cover_url = ""
    }
;;

let fip = {
  name = "FIP";
  process = process_fip;
  json_url = "http://www.fipradio.fr/sites/default/files/import_si/si_titre_antenne/FIP_player_current.json";
  mp3_url = "http://mp3.live.tv-radio.com/fip/all/fiphautdebit.mp3";
};;

(* -------------------------------- France Q -------------------------------- *)

let process_franceq response =
  let json = Yojson.Safe.from_string response in
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
  process = process_franceq;
  json_url = "http://www.franceculture.fr/sites/default/files/rf_player/player-direct.json";
  mp3_url = "http://mp3lg.tdf-cdn.com/franceculture/all/franceculturehautdebit.mp3";
};;

(* -------------------------------- Indie Pop Rocks! ------------------------ *)

let process_indiepop json =
  let rex = Pcre.regexp ~flags:[ `MULTILINE ]
    ">([^<]+)</a></td><td>([^<]+)</td><td><a[^>]+>([^<]*)</a></td>$"
  in
  match Pcre.extract_opt ~rex json with
  | [| Some _everything; Some artist; Some title; Some album |] ->
      { artist; title; year = ""; album; cover_url = "" }
  | _ ->
      raise Not_found
;;

let soma_indiepop = {
  name = "Indie Pop Rocks!";
  process = process_indiepop;
  json_url = "http://somafm.com/recent/indiepop.html";
  mp3_url = "http://ice.somafm.com/indiepop"
};;

(* -------------------------------- 7-Inch Soul ----------------------------- *)

let process_7inch json =
  let rex = Pcre.regexp ~flags:[ `MULTILINE ]
    ">([^<]+)</a></td><td>([^<]+)</td><td>([^<]+)</td>$"
  in
  match Pcre.extract_opt ~rex json with
  | [| Some _everything; Some artist; Some title; Some _single |] ->
      { artist; title; year = ""; album = ""; cover_url = "" }
  | _ ->
      raise Not_found
;;

let soma_7inch = {
  name = "7-Inch Soul";
  process = process_7inch;
  json_url = "http://somafm.com/recent/7soul.html";
  mp3_url = "http://uwstream2.somafm.com:7770"
};;

(* -------------------------------- Main routines --------------------------- *)

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
      lwt response = fetch_url radio.json_url in

      let entry = radio.process response in

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

let thread_soma_indiepop () =
  thread soma_indiepop
;;
let thread_soma_7inch () =
  thread soma_7inch
;;
