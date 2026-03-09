(** Agent implementation using Eio structured concurrency.
    Supports hooks, context, guardrails, sessions, skills, and handoffs. *)

open Types

type t = {
  mutable state : agent_state;
  tools : Tool.t list;
  base_url : string;
  provider : Provider.config option;
  net : [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  hooks : Hooks.hooks;
  context : Context.t;
  guardrails : Guardrails.t;
  session : Session.t;
}

let create ~net ?(config = default_config) ?(tools = []) ?(base_url = Api.default_base_url)
    ?provider ?(hooks = Hooks.empty) ?context ?(guardrails = Guardrails.default)
    ?session () =
  let state = {
    config;
    messages = [];
    turn_count = 0;
    usage = empty_usage;
  } in
  let context = Option.value context ~default:(Context.create ()) in
  let session = Option.value session ~default:(Session.create ()) in
  { state; tools; base_url; provider; net; hooks; context; guardrails; session }

let emit_session_end agent result =
  let _ =
    Hooks.invoke agent.hooks.session_end
      (Hooks.SessionEnd { session = agent.session; result })
  in
  result

let authorize_tool_use agent (tool : Tool.t) input =
  match Guardrails.permission_for_tool agent.guardrails tool.schema with
  | Guardrails.Authorized -> Ok ()
  | Guardrails.Rejected reason -> Error reason
  | Guardrails.Requires_confirmation reason ->
      let decision =
        Hooks.invoke agent.hooks.permission_request
          (Hooks.PermissionRequest {
             tool_name = tool.schema.name;
             tool_kind = tool.schema.kind;
             input;
             permission_mode = agent.guardrails.permission_mode;
             reason;
           })
      in
      (match decision with
       | Hooks.Allow -> Ok ()
       | Hooks.Deny msg -> Error msg
       | Hooks.Skip -> Error "tool blocked by permission hook"
       | Hooks.Ask | Hooks.Continue ->
           Error (Printf.sprintf "permission required for tool '%s': %s"
                    tool.schema.name reason)
       | Hooks.Override _ -> Ok ())

let apply_output_limit agent content =
  Guardrails.apply_output_limit agent.guardrails content |> fst

(** Execute tools in parallel using Eio fibers.
    Each fiber catches exceptions so one tool failure does not cancel siblings. *)
let execute_tools agent tool_uses =
  Eio.Fiber.List.map (fun block ->
    match block with
    | ToolUse (id, name, input) ->
        (try
           let decision =
             Hooks.invoke agent.hooks.pre_tool_use
               (Hooks.PreToolUse { tool_name = name; input })
           in
           match decision with
           | Hooks.Skip -> (id, "Tool execution skipped by hook", false)
           | Hooks.Override value -> (id, value, false)
           | Hooks.Deny msg -> (id, msg, true)
           | Hooks.Ask -> (id, "Tool execution deferred by hook", true)
           | Hooks.Allow | Hooks.Continue ->
               let tool_opt =
                 List.find_opt (fun (tool : Tool.t) -> tool.schema.name = name) agent.tools
               in
               match tool_opt with
               | None -> (id, "Tool not found", true)
               | Some tool ->
                   (match authorize_tool_use agent tool input with
                    | Error reason -> (id, reason, true)
                    | Ok () ->
                        let result = Tool.execute ~context:agent.context tool input in
                        let result =
                          match result with
                          | Ok output -> Ok (apply_output_limit agent output)
                          | Error _ as err -> err
                        in
                        let _ =
                          Hooks.invoke agent.hooks.post_tool_use
                            (Hooks.PostToolUse {
                               tool_name = name;
                               input;
                               output = result;
                             })
                        in
                        match result with
                        | Ok output -> (id, output, false)
                        | Error err -> (id, err, true))
         with exn ->
           let msg =
             Printf.sprintf "Tool '%s' raised: %s" name (Printexc.to_string exn)
           in
           (id, msg, true))
    | _ -> ("", "", true))
    tool_uses

let run_turn ~sw ?clock agent =
  let _ =
    Hooks.invoke agent.hooks.before_turn
      (Hooks.BeforeTurn {
         turn = agent.state.turn_count;
         messages = agent.state.messages;
       })
  in

  let visible_tools = Guardrails.filter_tools agent.guardrails agent.tools in
  let tool_schemas = List.map Tool.schema_to_json visible_tools in
  let tools_json = if tool_schemas = [] then None else Some tool_schemas in

  match
    Api.create_message ~sw ~net:agent.net ~base_url:agent.base_url ?provider:agent.provider
      ?clock ~config:agent.state ~messages:agent.state.messages ?tools:tools_json ()
  with
  | Error e -> Error e
  | Ok response ->
      let usage =
        match response.usage with
        | Some (input_tokens, output_tokens) ->
            add_usage agent.state.usage input_tokens output_tokens
        | None -> { agent.state.usage with api_calls = agent.state.usage.api_calls + 1 }
      in
      let _ =
        Hooks.invoke agent.hooks.after_turn
          (Hooks.AfterTurn { turn = agent.state.turn_count; response })
      in
      agent.state <- {
        agent.state with
        messages = agent.state.messages @ [ { role = Assistant; content = response.content } ];
        turn_count = agent.state.turn_count + 1;
        usage;
      };
      Session.record_turn agent.session;

      match response.stop_reason with
      | StopToolUse ->
          let tool_uses =
            List.filter (function ToolUse _ -> true | _ -> false) response.content
          in
          let count = List.length tool_uses in
          if Guardrails.exceeds_limit agent.guardrails count then (
            let msg =
              Printf.sprintf "Tool call limit exceeded: %d calls in one turn" count
            in
            agent.state <- {
              agent.state with
              messages =
                agent.state.messages @ [ { role = User; content = [ Text msg ] } ];
            };
            Ok `ToolsExecuted
          ) else (
            let results = execute_tools agent tool_uses in
            let tool_results =
              List.map (fun (id, content, is_error) -> ToolResult (id, content, is_error))
                results
            in
            agent.state <- {
              agent.state with
              messages =
                agent.state.messages @ [ { role = User; content = tool_results } ];
            };
            Ok `ToolsExecuted
          )
      | EndTurn | MaxTokens | StopSequence ->
          let _ =
            Hooks.invoke agent.hooks.on_stop
              (Hooks.OnStop { reason = response.stop_reason; response })
          in
          Ok (`Complete response)
      | Unknown reason ->
          Error (Printf.sprintf "Unrecognized stop_reason from API: %s" reason)

let run ~sw ?clock agent user_prompt =
  let _ =
    Hooks.invoke agent.hooks.session_start
      (Hooks.SessionStart { session = agent.session; prompt = user_prompt })
  in
  let _ =
    Hooks.invoke agent.hooks.user_prompt_submit
      (Hooks.UserPromptSubmit { session = agent.session; prompt = user_prompt })
  in
  agent.state <- {
    agent.state with
    messages = agent.state.messages @ [ { role = User; content = [ Text user_prompt ] } ];
  };

  let rec loop () =
    if agent.state.turn_count >= agent.state.config.max_turns then
      emit_session_end agent (Error "Max turns exceeded")
    else
      match run_turn ~sw ?clock agent with
      | Error e -> emit_session_end agent (Error e)
      | Ok (`Complete response) -> emit_session_end agent (Ok response)
      | Ok `ToolsExecuted -> loop ()
  in
  loop ()

let find_handoff_in_messages messages =
  let last_assistant =
    List.rev messages |> List.find_opt (fun message -> message.role = Assistant)
  in
  match last_assistant with
  | None -> None
  | Some assistant_message ->
      List.fold_left
        (fun acc block ->
          match acc with
          | Some _ -> acc
          | None ->
              match block with
              | ToolUse (id, name, input) when Handoff.is_handoff_tool name ->
                  let target_name = Handoff.target_name_of_tool name in
                  let prompt =
                    match input with
                    | `Assoc pairs ->
                        (match List.assoc_opt "prompt" pairs with
                         | Some (`String s) -> s
                         | _ -> "Continue the conversation.")
                    | _ -> "Continue the conversation."
                  in
                  Some (id, target_name, prompt)
              | _ -> None)
        None assistant_message.content

let replace_tool_result messages ~tool_id ~content ~is_error =
  let replace_in_content blocks =
    List.map (function
      | ToolResult (id, _, _) when id = tool_id -> ToolResult (id, content, is_error)
      | block -> block)
      blocks
  in
  let rec rewrite rev_prefix = function
    | [] ->
        List.rev ({ role = User; content = [ ToolResult (tool_id, content, is_error) ] } :: rev_prefix)
    | ((message : message) :: rest) ->
        let has_tool_result =
          List.exists
            (function ToolResult (id, _, _) when id = tool_id -> true | _ -> false)
            message.content
        in
        if message.role = User && has_tool_result then
          List.rev_append rev_prefix ({ message with content = replace_in_content message.content } :: rest)
        else
          rewrite (message :: rev_prefix) rest
  in
  rewrite [] (List.rev messages)

let run_with_handoffs ~sw ?clock agent ~targets user_prompt =
  let handoff_tools = List.map Handoff.make_handoff_tool targets in
  let all_tools = agent.tools @ handoff_tools in
  let agent_with_handoffs = { agent with tools = all_tools } in

  let rec loop () =
    if agent_with_handoffs.state.turn_count >= agent_with_handoffs.state.config.max_turns
    then emit_session_end agent_with_handoffs (Error "Max turns exceeded")
    else
      match run_turn ~sw ?clock agent_with_handoffs with
      | Error e -> emit_session_end agent_with_handoffs (Error e)
      | Ok (`Complete response) -> emit_session_end agent_with_handoffs (Ok response)
      | Ok `ToolsExecuted ->
          (match find_handoff_in_messages agent_with_handoffs.state.messages with
           | None -> loop ()
           | Some (tool_id, target_name, prompt) ->
               let target_opt =
                 List.find_opt (fun (target : Handoff.handoff_target) -> target.name = target_name)
                   targets
               in
               match target_opt with
               | None ->
                   let err_msg = Printf.sprintf "Unknown handoff target: %s" target_name in
                   agent_with_handoffs.state <- {
                     agent_with_handoffs.state with
                     messages =
                       replace_tool_result agent_with_handoffs.state.messages
                         ~tool_id ~content:err_msg ~is_error:true;
                   };
                   loop ()
               | Some target ->
                   let _ =
                     Hooks.invoke agent_with_handoffs.hooks.subagent_start
                       (Hooks.SubagentStart {
                          session = agent_with_handoffs.session;
                          target_name;
                          prompt;
                        })
                   in
                   let sub =
                     create ~net:agent.net ~config:target.config ~tools:target.tools
                       ~base_url:agent.base_url ?provider:agent.provider
                       ?hooks:target.hooks ?guardrails:target.guardrails
                       ?session:target.session ()
                   in
                   (match run ~sw ?clock sub prompt with
                    | Error e ->
                        let err_msg =
                          Printf.sprintf "Handoff to %s failed: %s" target_name e
                        in
                        let _ =
                          Hooks.invoke agent_with_handoffs.hooks.subagent_stop
                            (Hooks.SubagentStop {
                               session = agent_with_handoffs.session;
                               target_name;
                               result = Error e;
                             })
                        in
                        agent_with_handoffs.state <- {
                          agent_with_handoffs.state with
                          messages =
                            replace_tool_result agent_with_handoffs.state.messages
                              ~tool_id ~content:err_msg ~is_error:true;
                        };
                        loop ()
                    | Ok sub_response ->
                        let text =
                          List.fold_left
                            (fun acc block ->
                              match block with
                              | Text s when acc = "" -> s
                              | Text s -> acc ^ "\n" ^ s
                              | _ -> acc)
                            "" sub_response.content
                        in
                        let _ =
                          Hooks.invoke agent_with_handoffs.hooks.subagent_stop
                            (Hooks.SubagentStop {
                               session = agent_with_handoffs.session;
                               target_name;
                               result = Ok text;
                             })
                        in
                        agent_with_handoffs.state <- {
                          agent_with_handoffs.state with
                          messages =
                            replace_tool_result agent_with_handoffs.state.messages
                              ~tool_id ~content:text ~is_error:false;
                        };
                        loop ()))
  in

  let _ =
    Hooks.invoke agent_with_handoffs.hooks.session_start
      (Hooks.SessionStart { session = agent_with_handoffs.session; prompt = user_prompt })
  in
  let _ =
    Hooks.invoke agent_with_handoffs.hooks.user_prompt_submit
      (Hooks.UserPromptSubmit {
         session = agent_with_handoffs.session;
         prompt = user_prompt;
       })
  in
  agent_with_handoffs.state <- {
    agent_with_handoffs.state with
    messages =
      agent_with_handoffs.state.messages
      @ [ { role = User; content = [ Text user_prompt ] } ];
  };
  loop ()

let run_with_subagents ~sw ?clock agent ~specs user_prompt =
  let targets =
    List.map
      (Subagent.to_handoff_target
         ~parent_config:agent.state.config
         ~base_tools:agent.tools
         ~base_guardrails:agent.guardrails
         ~base_hooks:agent.hooks
         ~base_session:agent.session)
      specs
  in
  run_with_handoffs ~sw ?clock agent ~targets user_prompt
