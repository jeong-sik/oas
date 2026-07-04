(* Xiaomi MiMo public API and Token Plan regional gateways are vendor-canonical
   hosts. Match exact [Uri.host] values only; protocol/path selection remains in
   the provider config, so [/anthropic] is not normalized into OpenAI chat here. *)

let canonical =
  [ "api.xiaomimimo.com"
  ; "token-plan-cn.xiaomimimo.com"
  ; "token-plan-sgp.xiaomimimo.com"
  ; "token-plan-ams.xiaomimimo.com"
  ]
;;

let base_url_targets base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host ->
    let host = String.lowercase_ascii host in
    List.exists (String.equal host) canonical
;;
