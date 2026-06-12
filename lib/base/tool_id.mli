(** Typed tool identifier.

    Closed Variant for builtins, escape hatches for MCP and user-supplied tools.
    See RFC-OAS-008 for the original rationale. PR-2 ships only the identifier
    and string conversion. The wiring into the (CDAL-resident) classifier
    that RFC-OAS-008 PR-3 originally proposed has been superseded by
    RFC-OAS-009 v2: OAS core no longer references CDAL modules, and the
    builtin variants here are scheduled for removal in RFC-OAS-012 once

    CDAL migrates to the downstream coordinator (RFC-OAS-011). *)

type t =
  (* Read-only — file & code navigation *)
  | Search
  | List_dir
  | Find_file
  | Read_file
  | Find_symbol
  | Get_symbols_overview
  | Find_referencing_symbols
  | Search_for_pattern
  (* Read-only — notebook *)
  | Notebook_read
  (* Read-only — browser observation *)
  | Read_console_messages
  | Read_network_requests
  | Get_page_text
  | Read_page
  | Tabs_context_mcp
  (* Read-only — task queries *)
  | Task_list
  | Task_get
  | Task_output
  (* Local-mutation — file editing *)
  | Create_text_file
  | Replace_content
  | Rename_symbol
  | Insert_after_symbol
  | Insert_before_symbol
  | Replace_symbol_body
  | Notebook_edit
  (* Local-mutation — task & team management *)
  | Task_create
  | Task_update
  | Task_stop
  | Team_create
  | Team_delete
  (* External-effect — HITL *)
  | Ask_user_question
  (* External-effect — browser interaction *)
  | Navigate
  | Computer
  | Find
  | Form_input
  | Javascript_tool
  | Tabs_create_mcp
  | Upload_image
  (* Shell-dynamic *)
  | Execute_shell_command
  (* Escape hatches *)
  | Mcp of
      { server : string
      ; tool : string
      }
  | User of string
[@@deriving show]

(** Structural equality. [Mcp] compares by [server] and [tool] fields;
    [User name] compares by exact (lowercased) name. *)
val equal : t -> t -> bool

(** Stable string form. Builtins and [Mcp] round-trip with [of_string].
    [User name] returns the (already lowercased) name. *)
val to_string : t -> string

(** Total. Lowercases input, matches builtins, parses [mcp__server__tool] into
    [Mcp], otherwise returns [User <lowercased>]. Never raises. *)
val of_string : string -> t

(** Every builtin constructor in declaration order. Excludes [Mcp _] and
    [User _]. Originally introduced by RFC-OAS-008 PR-2 for parity with
    the (CDAL-resident) builtin classifier; scheduled for removal in
    RFC-OAS-012 alongside the CDAL migration in RFC-OAS-011. *)
val all_builtins : t list
