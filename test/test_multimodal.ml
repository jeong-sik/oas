(** Tests for Image and Document content block serialization round-trips *)

open Agent_sdk

(* Helper: compare content_block via show string since no eq deriving *)
let check_block msg expected actual =
  Alcotest.(check string) msg
    (Types.show_content_block expected)
    (Types.show_content_block actual)

(* ------------------------------------------------------------------ *)
(* Round-trip: Image                                                    *)
(* ------------------------------------------------------------------ *)

let test_image_round_trip () =
  let img = Types.Image {
    media_type = "image/png";
    data = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==";
    source_type = "base64";
  } in
  let json = Api.content_block_to_json img in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "image round-trip" img parsed
  | None -> Alcotest.fail "content_block_of_json returned None for Image"

(* ------------------------------------------------------------------ *)
(* Round-trip: Document                                                 *)
(* ------------------------------------------------------------------ *)

let test_document_round_trip () =
  let doc = Types.Document {
    media_type = "application/pdf";
    data = "JVBERi0xLjQKMSAwIG9iago8PAovVHlwZSAvQ2F0YWxvZwo+Pg==";
    source_type = "base64";
  } in
  let json = Api.content_block_to_json doc in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "document round-trip" doc parsed
  | None -> Alcotest.fail "content_block_of_json returned None for Document"

(* ------------------------------------------------------------------ *)
(* Parsing: Image with nested source                                    *)
(* ------------------------------------------------------------------ *)

let test_image_parse_nested_source () =
  let json = `Assoc [
    ("type", `String "image");
    ("source", `Assoc [
      ("type", `String "base64");
      ("media_type", `String "image/jpeg");
      ("data", `String "abc123");
    ]);
  ] in
  match Api.content_block_of_json json with
  | Some (Types.Image { media_type; data; source_type }) ->
    Alcotest.(check string) "media_type" "image/jpeg" media_type;
    Alcotest.(check string) "data" "abc123" data;
    Alcotest.(check string) "source_type" "base64" source_type
  | Some _ -> Alcotest.fail "expected Image variant"
  | None -> Alcotest.fail "content_block_of_json returned None"

(* ------------------------------------------------------------------ *)
(* Parsing: Document with nested source                                 *)
(* ------------------------------------------------------------------ *)

let test_document_parse_nested_source () =
  let json = `Assoc [
    ("type", `String "document");
    ("source", `Assoc [
      ("type", `String "base64");
      ("media_type", `String "application/pdf");
      ("data", `String "pdf_data_here");
    ]);
  ] in
  match Api.content_block_of_json json with
  | Some (Types.Document { media_type; data; source_type }) ->
    Alcotest.(check string) "media_type" "application/pdf" media_type;
    Alcotest.(check string) "data" "pdf_data_here" data;
    Alcotest.(check string) "source_type" "base64" source_type
  | Some _ -> Alcotest.fail "expected Document variant"
  | None -> Alcotest.fail "content_block_of_json returned None"

(* ------------------------------------------------------------------ *)
(* Malformed Image/Document JSON                                        *)
(* ------------------------------------------------------------------ *)

let test_malformed_image_missing_source () =
  let json = `Assoc [("type", `String "image")] in
  match Api.content_block_of_json json with
  | None -> ()
  | exception _ -> ()
  | Some _ -> Alcotest.fail "expected None or exception for malformed Image"

let test_malformed_document_missing_source () =
  let json = `Assoc [("type", `String "document")] in
  match Api.content_block_of_json json with
  | None -> ()
  | exception _ -> ()
  | Some _ -> Alcotest.fail "expected None or exception for malformed Document"

(* ------------------------------------------------------------------ *)
(* Mixed content: Text + Image + Document                               *)
(* ------------------------------------------------------------------ *)

let test_mixed_content_serialization () =
  let blocks = [
    Types.Text "Here is an image:";
    Types.Image {
      media_type = "image/png";
      data = "base64data";
      source_type = "base64";
    };
    Types.Document {
      media_type = "application/pdf";
      data = "pdfdata";
      source_type = "base64";
    };
  ] in
  let json_list = List.map Api.content_block_to_json blocks in
  let parsed = List.filter_map Api.content_block_of_json json_list in
  Alcotest.(check int) "all 3 blocks parsed" 3 (List.length parsed);
  List.iter2 (fun expected actual ->
    check_block "mixed content" expected actual
  ) blocks parsed

(* ------------------------------------------------------------------ *)
(* JSON structure verification                                          *)
(* ------------------------------------------------------------------ *)

let test_image_json_structure () =
  let img = Types.Image {
    media_type = "image/webp";
    data = "webpdata";
    source_type = "base64";
  } in
  let json = Api.content_block_to_json img in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "top-level type" "image"
    (json |> member "type" |> to_string);
  let source = json |> member "source" in
  Alcotest.(check string) "source.type" "base64"
    (source |> member "type" |> to_string);
  Alcotest.(check string) "source.media_type" "image/webp"
    (source |> member "media_type" |> to_string);
  Alcotest.(check string) "source.data" "webpdata"
    (source |> member "data" |> to_string)

let test_document_json_structure () =
  let doc = Types.Document {
    media_type = "text/plain";
    data = "textdata";
    source_type = "base64";
  } in
  let json = Api.content_block_to_json doc in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "top-level type" "document"
    (json |> member "type" |> to_string);
  let source = json |> member "source" in
  Alcotest.(check string) "source.type" "base64"
    (source |> member "type" |> to_string);
  Alcotest.(check string) "source.media_type" "text/plain"
    (source |> member "media_type" |> to_string)

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Multimodal" [
    "round_trip", [
      Alcotest.test_case "image base64 round-trip" `Quick test_image_round_trip;
      Alcotest.test_case "document pdf round-trip" `Quick test_document_round_trip;
    ];
    "parsing", [
      Alcotest.test_case "image nested source" `Quick test_image_parse_nested_source;
      Alcotest.test_case "document nested source" `Quick test_document_parse_nested_source;
    ];
    "malformed", [
      Alcotest.test_case "image missing source" `Quick test_malformed_image_missing_source;
      Alcotest.test_case "document missing source" `Quick test_malformed_document_missing_source;
    ];
    "mixed", [
      Alcotest.test_case "text + image + document" `Quick test_mixed_content_serialization;
    ];
    "json_structure", [
      Alcotest.test_case "image json structure" `Quick test_image_json_structure;
      Alcotest.test_case "document json structure" `Quick test_document_json_structure;
    ];
  ]
