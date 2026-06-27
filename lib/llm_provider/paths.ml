let cwd () = Sys.getcwd ()

let home_dir () =
  match Sys.getenv_opt "HOME" with
  | None -> None
  | Some "" -> None
  | Some s -> Some s
;;

let user_config_file filename =
  match home_dir () with
  | None -> None
  | Some home -> Some (Filename.concat home (".config/oas/" ^ filename))
;;
