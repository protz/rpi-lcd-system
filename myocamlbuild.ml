open Ocamlbuild_plugin;;
open Command;;

let _ =
  dispatch (fun event ->
    match event with
    | After_rules ->
        flag ["ocaml"; "compile"]
          (S[A "-ppopt"; A "-lwt-debug"]);
    | _ -> ()
  );
;;
