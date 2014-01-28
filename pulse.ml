let get_sink_props () =
  let open Unix in

  let fd = socket PF_INET SOCK_STREAM 0 in
  setsockopt_float fd SO_RCVTIMEO 1.;
  connect fd (ADDR_INET (inet_addr_of_string "127.0.0.1", 4712));
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

let thread_current_sink () =
  let props = get_sink_props () in
  try
    let title, artist =
      List.assoc "media.title" props, List.assoc "media.artist" props
    in
    ...
