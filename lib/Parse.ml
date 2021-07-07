(* Generated by ocaml-tree-sitter. *)

(*
   Disable warning 42:
     "this use of Foo relies on type-directed disambiguation,
     it will not compile with OCaml 4.00 or earlier."
*)
[@@@warning "-42"]

(* Disable warnings against unused variables. *)
[@@@warning "-26-27-32"]

open Tree_sitter_bindings
open Tree_sitter_run

let debug = ref false

type mt = Run.matcher_token

external create_parser :
  unit -> Tree_sitter_API.ts_parser = "octs_create_parser_vue"

let ts_parser = create_parser ()

let parse_source_string ?src_file contents =
  Tree_sitter_parsing.parse_source_string ?src_file ts_parser contents

let parse_source_file src_file =
  Tree_sitter_parsing.parse_source_file ts_parser src_file

let extras = [
]

let children_regexps : (string * Run.exp option) list = [
  "pat_98d585a", None;
  "implicit_end_tag", None;
  "erroneous_end_tag_name", None;
  "attribute_name", None;
  "directive_argument", None;
  "start_tag_name", None;
  "comment", None;
  "raw_text", None;
  "pat_58fbb2e", None;
  "style_start_tag_name", None;
  "text_fragment", None;
  "end_tag_name", None;
  "script_start_tag_name", None;
  "directive_name", None;
  "directive_modifier", None;
  "directive_shorthand", None;
  "directive_dynamic_argument_value", None;
  "template_start_tag_name", None;
  "attribute_value", None;
  "interpolation_text", None;
  "erroneous_end_tag",
  Some (
    Seq [
      Token (Literal "</");
      Token (Name "erroneous_end_tag_name");
      Token (Literal ">");
    ];
  );
  "quoted_attribute_value",
  Some (
    Alt [|
      Seq [
        Token (Literal "'");
        Opt (
          Token (Name "pat_58fbb2e");
        );
        Token (Literal "'");
      ];
      Seq [
        Token (Literal "\"");
        Opt (
          Token (Name "pat_98d585a");
        );
        Token (Literal "\"");
      ];
    |];
  );
  "text",
  Some (
    Alt [|
      Token (Name "text_fragment");
      Token (Literal "{{");
    |];
  );
  "end_tag",
  Some (
    Seq [
      Token (Literal "</");
      Token (Name "end_tag_name");
      Token (Literal ">");
    ];
  );
  "directive_modifiers",
  Some (
    Repeat1 (
      Seq [
        Token (Literal ".");
        Token (Name "directive_modifier");
      ];
    );
  );
  "directive_dynamic_argument",
  Some (
    Seq [
      Token (Literal "[");
      Opt (
        Token (Name "directive_dynamic_argument_value");
      );
      Token (Literal "]");
    ];
  );
  "interpolation",
  Some (
    Seq [
      Token (Literal "{{");
      Opt (
        Token (Name "interpolation_text");
      );
      Token (Literal "}}");
    ];
  );
  "attribute",
  Some (
    Seq [
      Token (Name "attribute_name");
      Opt (
        Seq [
          Token (Literal "=");
          Alt [|
            Token (Name "attribute_value");
            Token (Name "quoted_attribute_value");
          |];
        ];
      );
    ];
  );
  "directive_attribute",
  Some (
    Seq [
      Alt [|
        Seq [
          Token (Name "directive_name");
          Opt (
            Seq [
              Token (Literal ":");
              Alt [|
                Token (Name "directive_argument");
                Token (Name "directive_dynamic_argument");
              |];
            ];
          );
        ];
        Seq [
          Token (Name "directive_shorthand");
          Alt [|
            Token (Name "directive_argument");
            Token (Name "directive_dynamic_argument");
          |];
        ];
      |];
      Opt (
        Token (Name "directive_modifiers");
      );
      Opt (
        Seq [
          Token (Literal "=");
          Alt [|
            Token (Name "attribute_value");
            Token (Name "quoted_attribute_value");
          |];
        ];
      );
    ];
  );
  "style_start_tag",
  Some (
    Seq [
      Token (Literal "<");
      Token (Name "style_start_tag_name");
      Repeat (
        Alt [|
          Token (Name "attribute");
          Token (Name "directive_attribute");
        |];
      );
      Token (Literal ">");
    ];
  );
  "self_closing_tag",
  Some (
    Seq [
      Token (Literal "<");
      Token (Name "start_tag_name");
      Repeat (
        Alt [|
          Token (Name "attribute");
          Token (Name "directive_attribute");
        |];
      );
      Token (Literal "/>");
    ];
  );
  "script_start_tag",
  Some (
    Seq [
      Token (Literal "<");
      Token (Name "script_start_tag_name");
      Repeat (
        Alt [|
          Token (Name "attribute");
          Token (Name "directive_attribute");
        |];
      );
      Token (Literal ">");
    ];
  );
  "template_start_tag",
  Some (
    Seq [
      Token (Literal "<");
      Token (Name "template_start_tag_name");
      Repeat (
        Alt [|
          Token (Name "attribute");
          Token (Name "directive_attribute");
        |];
      );
      Token (Literal ">");
    ];
  );
  "start_tag",
  Some (
    Seq [
      Token (Literal "<");
      Token (Name "start_tag_name");
      Repeat (
        Alt [|
          Token (Name "attribute");
          Token (Name "directive_attribute");
        |];
      );
      Token (Literal ">");
    ];
  );
  "style_element",
  Some (
    Seq [
      Token (Name "style_start_tag");
      Opt (
        Token (Name "raw_text");
      );
      Token (Name "end_tag");
    ];
  );
  "script_element",
  Some (
    Seq [
      Token (Name "script_start_tag");
      Opt (
        Token (Name "raw_text");
      );
      Token (Name "end_tag");
    ];
  );
  "element",
  Some (
    Alt [|
      Seq [
        Token (Name "start_tag");
        Repeat (
          Token (Name "node");
        );
        Alt [|
          Token (Name "end_tag");
          Token (Name "implicit_end_tag");
        |];
      ];
      Token (Name "self_closing_tag");
    |];
  );
  "node",
  Some (
    Alt [|
      Token (Name "comment");
      Token (Name "text");
      Token (Name "interpolation");
      Token (Name "element");
      Token (Name "template_element");
      Token (Name "script_element");
      Token (Name "style_element");
      Token (Name "erroneous_end_tag");
    |];
  );
  "template_element",
  Some (
    Seq [
      Token (Name "template_start_tag");
      Repeat (
        Token (Name "node");
      );
      Token (Name "end_tag");
    ];
  );
  "component",
  Some (
    Repeat (
      Alt [|
        Token (Name "comment");
        Token (Name "element");
        Token (Name "template_element");
        Token (Name "script_element");
        Token (Name "style_element");
      |];
    );
  );
]

let trans_pat_98d585a ((kind, body) : mt) : CST.pat_98d585a =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_implicit_end_tag ((kind, body) : mt) : CST.implicit_end_tag =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_erroneous_end_tag_name ((kind, body) : mt) : CST.erroneous_end_tag_name =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_attribute_name ((kind, body) : mt) : CST.attribute_name =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_directive_argument ((kind, body) : mt) : CST.directive_argument =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_start_tag_name ((kind, body) : mt) : CST.start_tag_name =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_comment ((kind, body) : mt) : CST.comment =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_raw_text ((kind, body) : mt) : CST.raw_text =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_pat_58fbb2e ((kind, body) : mt) : CST.pat_58fbb2e =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_style_start_tag_name ((kind, body) : mt) : CST.style_start_tag_name =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_text_fragment ((kind, body) : mt) : CST.text_fragment =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_end_tag_name ((kind, body) : mt) : CST.end_tag_name =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_script_start_tag_name ((kind, body) : mt) : CST.script_start_tag_name =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_directive_name ((kind, body) : mt) : CST.directive_name =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_directive_modifier ((kind, body) : mt) : CST.directive_modifier =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_directive_shorthand ((kind, body) : mt) : CST.directive_shorthand =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_directive_dynamic_argument_value ((kind, body) : mt) : CST.directive_dynamic_argument_value =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_template_start_tag_name ((kind, body) : mt) : CST.template_start_tag_name =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_attribute_value ((kind, body) : mt) : CST.attribute_value =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_interpolation_text ((kind, body) : mt) : CST.interpolation_text =
  match body with
  | Leaf v -> v
  | Children _ -> assert false

let trans_erroneous_end_tag ((kind, body) : mt) : CST.erroneous_end_tag =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2] ->
          (
            Run.trans_token (Run.matcher_token v0),
            trans_erroneous_end_tag_name (Run.matcher_token v1),
            Run.trans_token (Run.matcher_token v2)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_quoted_attribute_value ((kind, body) : mt) : CST.quoted_attribute_value =
  match body with
  | Children v ->
      (match v with
      | Alt (0, v) ->
          `SQUOT_opt_pat_58fbb2e_SQUOT (
            (match v with
            | Seq [v0; v1; v2] ->
                (
                  Run.trans_token (Run.matcher_token v0),
                  Run.opt
                    (fun v -> trans_pat_58fbb2e (Run.matcher_token v))
                    v1
                  ,
                  Run.trans_token (Run.matcher_token v2)
                )
            | _ -> assert false
            )
          )
      | Alt (1, v) ->
          `DQUOT_opt_pat_98d585a_DQUOT (
            (match v with
            | Seq [v0; v1; v2] ->
                (
                  Run.trans_token (Run.matcher_token v0),
                  Run.opt
                    (fun v -> trans_pat_98d585a (Run.matcher_token v))
                    v1
                  ,
                  Run.trans_token (Run.matcher_token v2)
                )
            | _ -> assert false
            )
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_text ((kind, body) : mt) : CST.text =
  match body with
  | Children v ->
      (match v with
      | Alt (0, v) ->
          `Text_frag (
            trans_text_fragment (Run.matcher_token v)
          )
      | Alt (1, v) ->
          `LCURLLCURL (
            Run.trans_token (Run.matcher_token v)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_end_tag ((kind, body) : mt) : CST.end_tag =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2] ->
          (
            Run.trans_token (Run.matcher_token v0),
            trans_end_tag_name (Run.matcher_token v1),
            Run.trans_token (Run.matcher_token v2)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_directive_modifiers ((kind, body) : mt) : CST.directive_modifiers =
  match body with
  | Children v ->
      Run.repeat1
        (fun v ->
          (match v with
          | Seq [v0; v1] ->
              (
                Run.trans_token (Run.matcher_token v0),
                trans_directive_modifier (Run.matcher_token v1)
              )
          | _ -> assert false
          )
        )
        v
  | Leaf _ -> assert false

let trans_directive_dynamic_argument ((kind, body) : mt) : CST.directive_dynamic_argument =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2] ->
          (
            Run.trans_token (Run.matcher_token v0),
            Run.opt
              (fun v ->
                trans_directive_dynamic_argument_value (Run.matcher_token v)
              )
              v1
            ,
            Run.trans_token (Run.matcher_token v2)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_interpolation ((kind, body) : mt) : CST.interpolation =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2] ->
          (
            Run.trans_token (Run.matcher_token v0),
            Run.opt
              (fun v -> trans_interpolation_text (Run.matcher_token v))
              v1
            ,
            Run.trans_token (Run.matcher_token v2)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_attribute ((kind, body) : mt) : CST.attribute =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1] ->
          (
            trans_attribute_name (Run.matcher_token v0),
            Run.opt
              (fun v ->
                (match v with
                | Seq [v0; v1] ->
                    (
                      Run.trans_token (Run.matcher_token v0),
                      (match v1 with
                      | Alt (0, v) ->
                          `Attr_value (
                            trans_attribute_value (Run.matcher_token v)
                          )
                      | Alt (1, v) ->
                          `Quoted_attr_value (
                            trans_quoted_attribute_value (Run.matcher_token v)
                          )
                      | _ -> assert false
                      )
                    )
                | _ -> assert false
                )
              )
              v1
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_directive_attribute ((kind, body) : mt) : CST.directive_attribute =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2] ->
          (
            (match v0 with
            | Alt (0, v) ->
                `Dire_name_opt_COLON_choice_dire_arg (
                  (match v with
                  | Seq [v0; v1] ->
                      (
                        trans_directive_name (Run.matcher_token v0),
                        Run.opt
                          (fun v ->
                            (match v with
                            | Seq [v0; v1] ->
                                (
                                  Run.trans_token (Run.matcher_token v0),
                                  (match v1 with
                                  | Alt (0, v) ->
                                      `Dire_arg (
                                        trans_directive_argument (Run.matcher_token v)
                                      )
                                  | Alt (1, v) ->
                                      `Dire_dyna_arg (
                                        trans_directive_dynamic_argument (Run.matcher_token v)
                                      )
                                  | _ -> assert false
                                  )
                                )
                            | _ -> assert false
                            )
                          )
                          v1
                      )
                  | _ -> assert false
                  )
                )
            | Alt (1, v) ->
                `Dire_shor_choice_dire_arg (
                  (match v with
                  | Seq [v0; v1] ->
                      (
                        trans_directive_shorthand (Run.matcher_token v0),
                        (match v1 with
                        | Alt (0, v) ->
                            `Dire_arg (
                              trans_directive_argument (Run.matcher_token v)
                            )
                        | Alt (1, v) ->
                            `Dire_dyna_arg (
                              trans_directive_dynamic_argument (Run.matcher_token v)
                            )
                        | _ -> assert false
                        )
                      )
                  | _ -> assert false
                  )
                )
            | _ -> assert false
            )
            ,
            Run.opt
              (fun v -> trans_directive_modifiers (Run.matcher_token v))
              v1
            ,
            Run.opt
              (fun v ->
                (match v with
                | Seq [v0; v1] ->
                    (
                      Run.trans_token (Run.matcher_token v0),
                      (match v1 with
                      | Alt (0, v) ->
                          `Attr_value (
                            trans_attribute_value (Run.matcher_token v)
                          )
                      | Alt (1, v) ->
                          `Quoted_attr_value (
                            trans_quoted_attribute_value (Run.matcher_token v)
                          )
                      | _ -> assert false
                      )
                    )
                | _ -> assert false
                )
              )
              v2
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_style_start_tag ((kind, body) : mt) : CST.style_start_tag =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2; v3] ->
          (
            Run.trans_token (Run.matcher_token v0),
            trans_style_start_tag_name (Run.matcher_token v1),
            Run.repeat
              (fun v ->
                (match v with
                | Alt (0, v) ->
                    `Attr (
                      trans_attribute (Run.matcher_token v)
                    )
                | Alt (1, v) ->
                    `Dire_attr (
                      trans_directive_attribute (Run.matcher_token v)
                    )
                | _ -> assert false
                )
              )
              v2
            ,
            Run.trans_token (Run.matcher_token v3)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_self_closing_tag ((kind, body) : mt) : CST.self_closing_tag =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2; v3] ->
          (
            Run.trans_token (Run.matcher_token v0),
            trans_start_tag_name (Run.matcher_token v1),
            Run.repeat
              (fun v ->
                (match v with
                | Alt (0, v) ->
                    `Attr (
                      trans_attribute (Run.matcher_token v)
                    )
                | Alt (1, v) ->
                    `Dire_attr (
                      trans_directive_attribute (Run.matcher_token v)
                    )
                | _ -> assert false
                )
              )
              v2
            ,
            Run.trans_token (Run.matcher_token v3)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_script_start_tag ((kind, body) : mt) : CST.script_start_tag =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2; v3] ->
          (
            Run.trans_token (Run.matcher_token v0),
            trans_script_start_tag_name (Run.matcher_token v1),
            Run.repeat
              (fun v ->
                (match v with
                | Alt (0, v) ->
                    `Attr (
                      trans_attribute (Run.matcher_token v)
                    )
                | Alt (1, v) ->
                    `Dire_attr (
                      trans_directive_attribute (Run.matcher_token v)
                    )
                | _ -> assert false
                )
              )
              v2
            ,
            Run.trans_token (Run.matcher_token v3)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_template_start_tag ((kind, body) : mt) : CST.template_start_tag =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2; v3] ->
          (
            Run.trans_token (Run.matcher_token v0),
            trans_template_start_tag_name (Run.matcher_token v1),
            Run.repeat
              (fun v ->
                (match v with
                | Alt (0, v) ->
                    `Attr (
                      trans_attribute (Run.matcher_token v)
                    )
                | Alt (1, v) ->
                    `Dire_attr (
                      trans_directive_attribute (Run.matcher_token v)
                    )
                | _ -> assert false
                )
              )
              v2
            ,
            Run.trans_token (Run.matcher_token v3)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_start_tag ((kind, body) : mt) : CST.start_tag =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2; v3] ->
          (
            Run.trans_token (Run.matcher_token v0),
            trans_start_tag_name (Run.matcher_token v1),
            Run.repeat
              (fun v ->
                (match v with
                | Alt (0, v) ->
                    `Attr (
                      trans_attribute (Run.matcher_token v)
                    )
                | Alt (1, v) ->
                    `Dire_attr (
                      trans_directive_attribute (Run.matcher_token v)
                    )
                | _ -> assert false
                )
              )
              v2
            ,
            Run.trans_token (Run.matcher_token v3)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_style_element ((kind, body) : mt) : CST.style_element =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2] ->
          (
            trans_style_start_tag (Run.matcher_token v0),
            Run.opt
              (fun v -> trans_raw_text (Run.matcher_token v))
              v1
            ,
            trans_end_tag (Run.matcher_token v2)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_script_element ((kind, body) : mt) : CST.script_element =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2] ->
          (
            trans_script_start_tag (Run.matcher_token v0),
            Run.opt
              (fun v -> trans_raw_text (Run.matcher_token v))
              v1
            ,
            trans_end_tag (Run.matcher_token v2)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let rec trans_element ((kind, body) : mt) : CST.element =
  match body with
  | Children v ->
      (match v with
      | Alt (0, v) ->
          `Start_tag_rep_node_choice_end_tag (
            (match v with
            | Seq [v0; v1; v2] ->
                (
                  trans_start_tag (Run.matcher_token v0),
                  Run.repeat
                    (fun v -> trans_node (Run.matcher_token v))
                    v1
                  ,
                  (match v2 with
                  | Alt (0, v) ->
                      `End_tag (
                        trans_end_tag (Run.matcher_token v)
                      )
                  | Alt (1, v) ->
                      `Impl_end_tag (
                        trans_implicit_end_tag (Run.matcher_token v)
                      )
                  | _ -> assert false
                  )
                )
            | _ -> assert false
            )
          )
      | Alt (1, v) ->
          `Self_clos_tag (
            trans_self_closing_tag (Run.matcher_token v)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

and trans_node ((kind, body) : mt) : CST.node =
  match body with
  | Children v ->
      (match v with
      | Alt (0, v) ->
          `Comm (
            trans_comment (Run.matcher_token v)
          )
      | Alt (1, v) ->
          `Text (
            trans_text (Run.matcher_token v)
          )
      | Alt (2, v) ->
          `Interp (
            trans_interpolation (Run.matcher_token v)
          )
      | Alt (3, v) ->
          `Elem (
            trans_element (Run.matcher_token v)
          )
      | Alt (4, v) ->
          `Temp_elem (
            trans_template_element (Run.matcher_token v)
          )
      | Alt (5, v) ->
          `Script_elem (
            trans_script_element (Run.matcher_token v)
          )
      | Alt (6, v) ->
          `Style_elem (
            trans_style_element (Run.matcher_token v)
          )
      | Alt (7, v) ->
          `Errons_end_tag (
            trans_erroneous_end_tag (Run.matcher_token v)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

and trans_template_element ((kind, body) : mt) : CST.template_element =
  match body with
  | Children v ->
      (match v with
      | Seq [v0; v1; v2] ->
          (
            trans_template_start_tag (Run.matcher_token v0),
            Run.repeat
              (fun v -> trans_node (Run.matcher_token v))
              v1
            ,
            trans_end_tag (Run.matcher_token v2)
          )
      | _ -> assert false
      )
  | Leaf _ -> assert false

let trans_component ((kind, body) : mt) : CST.component =
  match body with
  | Children v ->
      Run.repeat
        (fun v ->
          (match v with
          | Alt (0, v) ->
              `Comm (
                trans_comment (Run.matcher_token v)
              )
          | Alt (1, v) ->
              `Elem (
                trans_element (Run.matcher_token v)
              )
          | Alt (2, v) ->
              `Temp_elem (
                trans_template_element (Run.matcher_token v)
              )
          | Alt (3, v) ->
              `Script_elem (
                trans_script_element (Run.matcher_token v)
              )
          | Alt (4, v) ->
              `Style_elem (
                trans_style_element (Run.matcher_token v)
              )
          | _ -> assert false
          )
        )
        v
  | Leaf _ -> assert false

let parse_input_tree input_tree =
  let orig_root_node = Tree_sitter_parsing.root input_tree in
  let src = Tree_sitter_parsing.src input_tree in
  let errors = Run.extract_errors src orig_root_node in
  let root_node = Run.remove_extras ~extras orig_root_node in
  let matched_tree = Run.match_tree children_regexps src root_node in
  let opt_program = Option.map trans_component matched_tree in
  Parsing_result.create src opt_program errors

let string ?src_file contents =
  let input_tree = parse_source_string ?src_file contents in
  parse_input_tree input_tree

let file src_file =
  let input_tree = parse_source_file src_file in
  parse_input_tree input_tree
