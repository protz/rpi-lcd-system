let get_sink_props prot inet_addr =
  let open Unix in

  let fd = socket prot SOCK_STREAM 0 in
  setsockopt_float fd SO_RCVTIMEO 1.;
  connect fd (ADDR_INET (inet_addr, 4712));
  let cmd = "list-sink-inputs\n" in
  if send fd cmd 0 (String.length cmd) [] <> String.length cmd then
    failwith "send failed";

  let bufs = ref [] in
  let buf = String.make 1024 ' ' in
  let count = ref 0 in
  begin try
    while begin
      count := recv fd buf 0 1024 [];
      !count <> 0
    end do
      bufs := String.sub buf 0 !count :: !bufs;
    done;
  with Unix_error (EAGAIN, _, _) ->
    ()
  end;
  let output = String.concat "" (List.rev !bufs) in

  shutdown fd SHUTDOWN_ALL;

  let rex = Pcre.regexp
    ~flags:[ `UTF8; `MULTILINE ]
    "^\t*([^ \n]+) = \"([^\"]+)\"$"
  in
  let properties = Pcre.extract_all
    ~rex
    output
  in
  let properties = Array.to_list properties in
  let properties = List.map (fun x -> x.(1), x.(2)) properties in

  properties
;;

let is_ipv4 = Pcre.pmatch ~pat:"\\d+\\.\\d+\\.\\d+\\.\\d+";;

let find_title_artist () =
  let open Unix in
  let local_props = get_sink_props PF_INET inet_addr_loopback in
  try
    Some (List.assoc "media.title" local_props, List.assoc "media.artist" local_props)
  with Not_found ->
    Printf.printf "[pulse] no local data\n%!";
    let candidates = 
      List.map (fun (k, v) ->
        if k = "native-protocol.peer" then
          try
            let r = Pcre.extract ~pat:"from (?:\\[([^\\]]+)\\]:|([^\\:]+))" v in
            if r.(1) <> "" then r.(1) else r.(2)
          with Not_found ->
            ""
        else
          ""
      ) local_props
    in
    let candidates = List.filter (fun x -> x <> "" && x <> "::1" && x <> "127.0.0.1") candidates in
    Printf.printf "[pulse] %d candidates\n%!" (List.length candidates);
    match candidates with
    | h :: _ ->
        begin try
          Printf.printf "[pulse] candidate is %s\n%!" h;
          let prot = if is_ipv4 h then PF_INET else PF_INET6 in
          let addr = inet_addr_of_string h in
          let remote_props = get_sink_props prot addr in
          begin try
            Some (List.assoc "media.title" remote_props, List.assoc "media.artist" remote_props)
          with Not_found ->
            Printf.printf "[pulse] didn't find media.title\n%!";
            None
          end
        with Not_found ->
            None
        end
    | [] ->
        None

let polling_thread () =
  let curr_artist = ref "___" and curr_title = ref "___" in
  let rec loop () =
    begin match find_title_artist () with
    | None ->
        if !curr_artist <> "" && !curr_title <> "" then begin
          curr_title := "";
          curr_artist := "";
          Display.display ~background:() "Nothing to display  "
        end
    | Some (title, artist) ->
        if title <> !curr_title || artist <> !curr_artist then begin
          curr_title := title;
          curr_artist := artist;
          let msg = Printf.sprintf "%s\n%s" title artist in
          Display.display ~background:() msg
        end
    end;
    lwt () = Lwt_unix.sleep 5. in
    loop ()
  in
  loop ()
    

let thread () =
  lwt () = Lwt.pick [
    (* Doesn't terminate. *)
    polling_thread ();
    (* Button press = exit. *)
    Button.wait_for_button ();
  ] in
  Lwt.return ()

