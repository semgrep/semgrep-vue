(**
   Boilerplate to be used as a template when mapping the vue CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_end_tag_name (env : env) (tok : CST.end_tag_name) =
  (* end_tag_name *) token env tok

let map_implicit_end_tag (env : env) (tok : CST.implicit_end_tag) =
  (* implicit_end_tag *) token env tok

let map_attribute_name (env : env) (tok : CST.attribute_name) =
  (* pattern "[^<>\"'=/\\s]+" *) token env tok

let map_directive_name (env : env) (tok : CST.directive_name) =
  (* directive_name *) token env tok

let map_directive_shorthand (env : env) (tok : CST.directive_shorthand) =
  (* directive_shorthand *) token env tok

let map_raw_text (env : env) (tok : CST.raw_text) =
  (* raw_text *) token env tok

let map_directive_modifier (env : env) (tok : CST.directive_modifier) =
  (* pattern "[^<>\"'/=\\s.]+" *) token env tok

let map_directive_argument (env : env) (tok : CST.directive_argument) =
  (* pattern "[^<>\"'/=\\s.]+" *) token env tok

let map_imm_tok_prec_p1_lbrack (env : env) (tok : CST.imm_tok_prec_p1_lbrack) =
  (* "[" *) token env tok

let map_imm_tok_rbrack (env : env) (tok : CST.imm_tok_rbrack) =
  (* "]" *) token env tok

let map_imm_tok_prec_p1_colon (env : env) (tok : CST.imm_tok_prec_p1_colon) =
  (* ":" *) token env tok

let map_directive_dynamic_argument_value (env : env) (tok : CST.directive_dynamic_argument_value) =
  (* pattern "[^<>\"'/=\\s\\]]+" *) token env tok

let map_style_start_tag_name (env : env) (tok : CST.style_start_tag_name) =
  (* style_start_tag_name *) token env tok

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_interpolation_text (env : env) (tok : CST.interpolation_text) =
  (* interpolation_text *) token env tok

let map_script_start_tag_name (env : env) (tok : CST.script_start_tag_name) =
  (* script_start_tag_name *) token env tok

let map_imm_tok_prec_p1_dot (env : env) (tok : CST.imm_tok_prec_p1_dot) =
  (* "." *) token env tok

let map_pat_58fbb2e (env : env) (tok : CST.pat_58fbb2e) =
  (* pattern "[^']+" *) token env tok

let map_text_fragment (env : env) (tok : CST.text_fragment) =
  (* text_fragment *) token env tok

let map_attribute_value (env : env) (tok : CST.attribute_value) =
  (* pattern "[^<>\"'=\\s]+" *) token env tok

let map_start_tag_name (env : env) (tok : CST.start_tag_name) =
  (* start_tag_name *) token env tok

let map_template_start_tag_name (env : env) (tok : CST.template_start_tag_name) =
  (* template_start_tag_name *) token env tok

let map_pat_98d585a (env : env) (tok : CST.pat_98d585a) =
  (* pattern "[^\"]+" *) token env tok

let map_erroneous_end_tag_name (env : env) (tok : CST.erroneous_end_tag_name) =
  (* erroneous_end_tag_name *) token env tok

let map_end_tag (env : env) ((v1, v2, v3) : CST.end_tag) =
  let v1 = (* "</" *) token env v1 in
  let v2 = (* end_tag_name *) token env v2 in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_directive_modifiers (env : env) (xs : CST.directive_modifiers) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = map_imm_tok_prec_p1_dot env v1 in
    let v2 = (* pattern "[^<>\"'/=\\s.]+" *) token env v2 in
    R.Tuple [v1; v2]
  ) xs)

let map_text (env : env) (x : CST.text) =
  (match x with
  | `Text_frag tok -> R.Case ("Text_frag",
      (* text_fragment *) token env tok
    )
  | `LCURLLCURL tok -> R.Case ("LCURLLCURL",
      (* "{{" *) token env tok
    )
  )

let map_quoted_attribute_value (env : env) (x : CST.quoted_attribute_value) =
  (match x with
  | `SQUOT_opt_pat_58fbb2e_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_opt_pat_58fbb2e_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_pat_58fbb2e env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DQUOT_opt_pat_98d585a_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_opt_pat_98d585a_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_pat_98d585a env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_anon_choice_dire_arg_b33821e (env : env) (x : CST.anon_choice_dire_arg_b33821e) =
  (match x with
  | `Dire_arg tok -> R.Case ("Dire_arg",
      (* pattern "[^<>\"'/=\\s.]+" *) token env tok
    )
  | `Dire_dyna_arg (v1, v2, v3) -> R.Case ("Dire_dyna_arg",
      let v1 = map_imm_tok_prec_p1_lbrack env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* pattern "[^<>\"'/=\\s\\]]+" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_imm_tok_rbrack env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_anon_choice_attr_value_5986531 (env : env) (x : CST.anon_choice_attr_value_5986531) =
  (match x with
  | `Attr_value tok -> R.Case ("Attr_value",
      (* pattern "[^<>\"'=\\s]+" *) token env tok
    )
  | `Quoted_attr_value x -> R.Case ("Quoted_attr_value",
      map_quoted_attribute_value env x
    )
  )

let map_anon_choice_attr_a1991da (env : env) (x : CST.anon_choice_attr_a1991da) =
  (match x with
  | `Attr (v1, v2) -> R.Case ("Attr",
      let v1 = (* pattern "[^<>\"'=/\\s]+" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_anon_choice_attr_value_5986531 env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Dire_attr (v1, v2, v3) -> R.Case ("Dire_attr",
      let v1 =
        (match v1 with
        | `Dire_name_opt_imm_tok_prec_p1_colon_choice_dire_arg (v1, v2) -> R.Case ("Dire_name_opt_imm_tok_prec_p1_colon_choice_dire_arg",
            let v1 = (* directive_name *) token env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = map_imm_tok_prec_p1_colon env v1 in
                  let v2 = map_anon_choice_dire_arg_b33821e env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Dire_shor_choice_dire_arg (v1, v2) -> R.Case ("Dire_shor_choice_dire_arg",
            let v1 = (* directive_shorthand *) token env v1 in
            let v2 = map_anon_choice_dire_arg_b33821e env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_directive_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_anon_choice_attr_value_5986531 env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_start_tag (env : env) ((v1, v2, v3, v4) : CST.start_tag) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* start_tag_name *) token env v2 in
  let v3 =
    R.List (List.map (map_anon_choice_attr_a1991da env) v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_script_start_tag (env : env) ((v1, v2, v3, v4) : CST.script_start_tag) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* script_start_tag_name *) token env v2 in
  let v3 =
    R.List (List.map (map_anon_choice_attr_a1991da env) v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_template_start_tag (env : env) ((v1, v2, v3, v4) : CST.template_start_tag) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* template_start_tag_name *) token env v2 in
  let v3 =
    R.List (List.map (map_anon_choice_attr_a1991da env) v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_style_start_tag (env : env) ((v1, v2, v3, v4) : CST.style_start_tag) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* style_start_tag_name *) token env v2 in
  let v3 =
    R.List (List.map (map_anon_choice_attr_a1991da env) v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_script_element (env : env) ((v1, v2, v3) : CST.script_element) =
  let v1 = map_script_start_tag env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* raw_text *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_end_tag env v3 in
  R.Tuple [v1; v2; v3]

let map_style_element (env : env) ((v1, v2, v3) : CST.style_element) =
  let v1 = map_style_start_tag env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* raw_text *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_end_tag env v3 in
  R.Tuple [v1; v2; v3]

let rec map_element (env : env) (x : CST.element) =
  (match x with
  | `Start_tag_rep_node_choice_end_tag (v1, v2, v3) -> R.Case ("Start_tag_rep_node_choice_end_tag",
      let v1 = map_start_tag env v1 in
      let v2 = R.List (List.map (map_node env) v2) in
      let v3 =
        (match v3 with
        | `End_tag x -> R.Case ("End_tag",
            map_end_tag env x
          )
        | `Impl_end_tag tok -> R.Case ("Impl_end_tag",
            (* implicit_end_tag *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Self_clos_tag (v1, v2, v3, v4) -> R.Case ("Self_clos_tag",
      let v1 = (* "<" *) token env v1 in
      let v2 = (* start_tag_name *) token env v2 in
      let v3 =
        R.List (List.map (map_anon_choice_attr_a1991da env) v3)
      in
      let v4 = (* "/>" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_node (env : env) (x : CST.node) =
  (match x with
  | `Comm tok -> R.Case ("Comm",
      (* comment *) token env tok
    )
  | `Text x -> R.Case ("Text",
      map_text env x
    )
  | `Interp (v1, v2, v3) -> R.Case ("Interp",
      let v1 = (* "{{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* interpolation_text *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Elem x -> R.Case ("Elem",
      map_element env x
    )
  | `Temp_elem x -> R.Case ("Temp_elem",
      map_template_element env x
    )
  | `Script_elem x -> R.Case ("Script_elem",
      map_script_element env x
    )
  | `Style_elem x -> R.Case ("Style_elem",
      map_style_element env x
    )
  | `Errons_end_tag (v1, v2, v3) -> R.Case ("Errons_end_tag",
      let v1 = (* "</" *) token env v1 in
      let v2 = (* erroneous_end_tag_name *) token env v2 in
      let v3 = (* ">" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_template_element (env : env) ((v1, v2, v3) : CST.template_element) =
  let v1 = map_template_start_tag env v1 in
  let v2 = R.List (List.map (map_node env) v2) in
  let v3 = map_end_tag env v3 in
  R.Tuple [v1; v2; v3]

let map_component (env : env) (xs : CST.component) =
  R.List (List.map (fun x ->
    (match x with
    | `Comm tok -> R.Case ("Comm",
        (* comment *) token env tok
      )
    | `Elem x -> R.Case ("Elem",
        map_element env x
      )
    | `Temp_elem x -> R.Case ("Temp_elem",
        map_template_element env x
      )
    | `Script_elem x -> R.Case ("Script_elem",
        map_script_element env x
      )
    | `Style_elem x -> R.Case ("Style_elem",
        map_style_element env x
      )
    )
  ) xs)

let dump_tree root =
  map_component () root
  |> Tree_sitter_run.Raw_tree.to_string
  |> print_string
