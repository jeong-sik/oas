(** Minimal LLM call test — no MASC, no tools, just one API round-trip *)
open Agent_sdk

let () =
  Printf.printf "Testing LLM call via anthropic-proxy...\n%!";
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let provider = Provider.local_qwen () in
  Printf.printf "Provider: base_url=%s model=%s\n%!"
    (match provider.provider with
     | Provider.Local { base_url } -> base_url
     | _ -> "non-local")
    provider.model_id;
  let config = {
    Types.default_config with
    model = Types.Custom provider.model_id;
    system_prompt = Some "You are a helpful assistant. Reply in one sentence.";
    max_turns = 1;
    max_tokens = 100;
  } in
  let messages = [{ Types.role = Types.User; content = [Types.Text "Say hello in exactly 5 words."] }] in
  Printf.printf "Sending request...\n%!";
  match Api.create_message ~sw ~net ~provider ~config:{ config = config; messages = []; turn_count = 0; usage = Types.empty_usage } ~messages () with
  | Ok resp ->
    Printf.printf "Response OK: model=%s stop_reason=%s\n%!" resp.Types.model
      (match resp.stop_reason with
       | Types.EndTurn -> "end_turn"
       | Types.StopToolUse -> "tool_use"
       | Types.MaxTokens -> "max_tokens"
       | Types.StopSequence -> "stop_sequence"
       | Types.Unknown s -> s);
    List.iter (fun block ->
      match block with
      | Types.Text t -> Printf.printf "Text: %s\n%!" t
      | _ -> Printf.printf "Other block\n%!"
    ) resp.content;
    (match resp.usage with
     | Some (inp, out) -> Printf.printf "Usage: in=%d out=%d\n%!" inp out
     | None -> Printf.printf "No usage data\n%!")
  | Error e ->
    Printf.printf "Error: %s\n%!" e
