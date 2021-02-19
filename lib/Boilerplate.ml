(**
   Boilerplate to be used as a template when mapping the scala CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_string_end (env : env) (tok : CST.string_end) =
  token env tok (* string_end *)

let map_string_middle (env : env) (tok : CST.string_middle) =
  token env tok (* string_middle *)

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  token env tok (* automatic_semicolon *)

let map_operator_identifier (env : env) (tok : CST.operator_identifier) =
  token env tok (* pattern "[^\\s\\w\\(\\)\\[\\]\\{\\}'\"`\\.;,]+" *)

let map_string_start (env : env) (tok : CST.string_start) =
  token env tok (* string_start *)

let map_multiline_string_start (env : env) (tok : CST.multiline_string_start) =
  token env tok (* multiline_string_start *)

let map_number (env : env) (tok : CST.number) =
  token env tok (* pattern [\d\.]+ *)

let map_modifiers (env : env) (xs : CST.modifiers) =
  List.map (fun x ->
    (match x with
    | `Abst tok -> token env tok (* "abstract" *)
    | `Final tok -> token env tok (* "final" *)
    | `Sealed tok -> token env tok (* "sealed" *)
    | `Impl tok -> token env tok (* "implicit" *)
    | `Lazy tok -> token env tok (* "lazy" *)
    | `Over tok -> token env tok (* "override" *)
    | `Priv tok -> token env tok (* "private" *)
    | `Prot tok -> token env tok (* "protected" *)
    )
  ) xs

let map_multiline_string_end (env : env) (tok : CST.multiline_string_end) =
  token env tok (* multiline_string_end *)

let map_multiline_string_middle (env : env) (tok : CST.multiline_string_middle) =
  token env tok (* multiline_string_middle *)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* pattern [a-zA-Z_]\w* *)

let map_simple_string (env : env) (tok : CST.simple_string) =
  token env tok (* simple_string *)

let map_semicolon (env : env) (x : CST.semicolon) =
  (match x with
  | `SEMI tok -> token env tok (* ";" *)
  | `Auto_semi tok -> token env tok (* automatic_semicolon *)
  )

let map_package_identifier (env : env) ((v1, v2) : CST.package_identifier) =
  let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "." *) in
      let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

let rec map_anon_choice_type_id_5555dfd (env : env) (x : CST.anon_choice_type_id_5555dfd) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Stable_id x -> map_stable_identifier env x
  )

and map_stable_identifier (env : env) ((v1, v2, v3) : CST.stable_identifier) =
  let v1 = map_anon_choice_type_id_5555dfd env v1 in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  todo env (v1, v2, v3)

let map_anon_choice_type_id_e16a528 (env : env) (x : CST.anon_choice_type_id_e16a528) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Op_id tok ->
      token env tok (* pattern "[^\\s\\w\\(\\)\\[\\]\\{\\}'\"`\\.;,]+" *)
  )

let map_anon_choice_type_id_ac5d537 (env : env) (x : CST.anon_choice_type_id_ac5d537) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Rena_id (v1, v2, v3) ->
      let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 = token env v2 (* "=>" *) in
      let v3 =
        (match v3 with
        | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | `Wild tok -> token env tok (* "_" *)
        )
      in
      todo env (v1, v2, v3)
  )

let map_stable_type_identifier (env : env) ((v1, v2, v3) : CST.stable_type_identifier) =
  let v1 = map_anon_choice_type_id_5555dfd env v1 in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  todo env (v1, v2, v3)

let map_import_selectors (env : env) ((v1, v2, v3, v4) : CST.import_selectors) =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_anon_choice_type_id_ac5d537 env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_anon_choice_type_id_ac5d537 env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

let map_import_expression (env : env) ((v1, v2) : CST.import_expression) =
  let v1 =
    (match v1 with
    | `Stable_id x -> map_stable_identifier env x
    | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "." *) in
        let v2 =
          (match v2 with
          | `Wild tok -> token env tok (* "_" *)
          | `Import_selecs x -> map_import_selectors env x
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

let rec map_annotated_type (env : env) ((v1, v2) : CST.annotated_type) =
  let v1 = map_simple_type env v1 in
  let v2 = List.map (map_annotation env) v2 in
  todo env (v1, v2)

and map_annotation (env : env) ((v1, v2, v3) : CST.annotation) =
  let v1 = token env v1 (* "@" *) in
  let v2 = map_simple_type env v2 in
  let v3 = List.map (map_arguments env) v3 in
  todo env (v1, v2, v3)

and map_anon_choice_comp_type_334563f (env : env) (x : CST.anon_choice_comp_type_334563f) =
  (match x with
  | `Comp_type x -> map_compound_type env x
  | `Infix_type x -> map_infix_type env x
  | `Anno_type x -> map_annotated_type env x
  )

and map_anon_choice_exp_20a4ff7 (env : env) (x : CST.anon_choice_exp_20a4ff7) =
  (match x with
  | `Exp x -> map_expression env x
  | `Choice_pack_clause x -> map_definition env x
  )

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_expression env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = map_anon_choice_exp_20a4ff7 env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = map_semicolon env v1 in
      let v2 = map_anon_choice_exp_20a4ff7 env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some x -> map_semicolon env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_block_ (env : env) ((v1, v2, v3) : CST.block_) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> map_block env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_case_block (env : env) (x : CST.case_block) =
  (match x with
  | `LCURL_RCURL (v1, v2) ->
      let v1 = token env v1 (* "{" *) in
      let v2 = token env v2 (* "}" *) in
      todo env (v1, v2)
  | `LCURL_rep1_case_clause_RCURL (v1, v2, v3) ->
      let v1 = token env v1 (* "{" *) in
      let v2 = List.map (map_case_clause env) v2 in
      let v3 = token env v3 (* "}" *) in
      todo env (v1, v2, v3)
  )

and map_case_clause (env : env) ((v1, v2, v3, v4, v5) : CST.case_clause) =
  let v1 = token env v1 (* "case" *) in
  let v2 = map_pattern env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_guard env x
    | None -> todo env ())
  in
  let v4 = token env v4 (* "=>" *) in
  let v5 =
    (match v5 with
    | Some x -> map_block env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_catch_clause (env : env) ((v1, v2) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let v2 = map_case_block env v2 in
  todo env (v1, v2)

and map_class_parameter (env : env) ((v1, v2, v3, v4, v5) : CST.class_parameter) =
  let v1 = List.map (map_annotation env) v1 in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Val tok -> token env tok (* "val" *)
        | `Var tok -> token env tok (* "var" *)
        )
    | None -> todo env ())
  in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> map_context_bound env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_class_parameters (env : env) ((v1, v2, v3, v4) : CST.class_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* "implicit" *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = map_class_parameter env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_class_parameter env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

and map_compound_type (env : env) ((v1, v2) : CST.compound_type) =
  let v1 = map_annotated_type env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "with" *) in
      let v2 = map_annotated_type env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_context_bound (env : env) ((v1, v2) : CST.context_bound) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_contravariant_type_parameter (env : env) ((v1, v2) : CST.contravariant_type_parameter) =
  let v1 = token env v1 (* "-" *) in
  let v2 = map_type_parameter env v2 in
  todo env (v1, v2)

and map_covariant_type_parameter (env : env) ((v1, v2) : CST.covariant_type_parameter) =
  let v1 = token env v1 (* "+" *) in
  let v2 = map_type_parameter env v2 in
  todo env (v1, v2)

and map_definition (env : env) (x : CST.definition) =
  (match x with
  | `Pack_clause (v1, v2, v3) ->
      let v1 = token env v1 (* "package" *) in
      let v2 = map_package_identifier env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_template_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Pack_obj (v1, v2, v3) ->
      let v1 = token env v1 (* "package" *) in
      let v2 = token env v2 (* "object" *) in
      let v3 = map_object_definition_ env v3 in
      todo env (v1, v2, v3)
  | `Class_defi (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 =
        (match v2 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some tok -> token env tok (* "case" *)
        | None -> todo env ())
      in
      let v4 = token env v4 (* "class" *) in
      let v5 = token env v5 (* pattern [a-zA-Z_]\w* *) in
      let v6 =
        (match v6 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v7 = List.map (map_class_parameters env) v7 in
      let v8 =
        (match v8 with
        | Some x -> map_extends_clause env x
        | None -> todo env ())
      in
      let v9 =
        (match v9 with
        | Some x -> map_template_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Import_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "import" *) in
      let v2 = map_import_expression env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_import_expression env v2 in
          todo env (v1, v2)
        ) v3
      in
      todo env (v1, v2, v3)
  | `Obj_defi (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "case" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "object" *) in
      let v3 = map_object_definition_ env v3 in
      todo env (v1, v2, v3)
  | `Trait_defi (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "trait" *) in
      let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_extends_clause env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_template_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  | `Val_defi (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 =
        (match v2 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "val" *) in
      let v4 = map_pattern env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_context_bound env x
        | None -> todo env ())
      in
      let v6 = token env v6 (* "=" *) in
      let v7 = map_expression env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Val_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 =
        (match v2 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "val" *) in
      let v4 = token env v4 (* pattern [a-zA-Z_]\w* *) in
      let v5 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
          todo env (v1, v2)
        ) v5
      in
      let v6 = token env v6 (* ":" *) in
      let v7 = map_type_ env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Var_defi (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 =
        (match v2 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "var" *) in
      let v4 = map_pattern env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_context_bound env x
        | None -> todo env ())
      in
      let v6 = token env v6 (* "=" *) in
      let v7 = map_expression env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Var_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 =
        (match v2 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "var" *) in
      let v4 = token env v4 (* pattern [a-zA-Z_]\w* *) in
      let v5 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
          todo env (v1, v2)
        ) v5
      in
      let v6 = token env v6 (* ":" *) in
      let v7 = map_type_ env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Type_defi (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 =
        (match v2 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "type" *) in
      let v4 = token env v4 (* pattern [a-zA-Z_]\w* *) in
      let v5 =
        (match v5 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v6 = token env v6 (* "=" *) in
      let v7 = map_type_ env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Func_defi (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 =
        (match v2 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "def" *) in
      let v4 = token env v4 (* pattern [a-zA-Z_]\w* *) in
      let v5 =
        (match v5 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v6 = List.map (map_parameters env) v6 in
      let v7 =
        (match v7 with
        | Some x -> map_context_bound env x
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | `EQ_exp (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | `Blk_ x -> map_block_ env x
        )
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Func_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 =
        (match v2 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "def" *) in
      let v4 = token env v4 (* pattern [a-zA-Z_]\w* *) in
      let v5 =
        (match v5 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v6 = List.map (map_parameters env) v6 in
      let v7 =
        (match v7 with
        | Some x -> map_context_bound env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `If_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_expression env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "else" *) in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Match_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "match" *) in
      let v3 = map_case_block env v3 in
      todo env (v1, v2, v3)
  | `Try_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = map_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_catch_clause env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_finally_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Call_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_arguments env v2 in
      let v3 =
        (match v3 with
        | Some x ->
            (match x with
            | `Blk_ x -> map_block_ env x
            | `Case_blk x -> map_case_block env x
            )
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Gene_func (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_type_arguments env v2 in
      todo env (v1, v2)
  | `Assign_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Str_tran_exp (v1, v2) ->
      let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 = map_string_ env v2 in
      todo env (v1, v2)
  | `Field_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
      todo env (v1, v2, v3)
  | `Inst_exp (v1, v2) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Infix_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_type_id_e16a528 env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Prefix_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `PLUS tok -> token env tok (* "+" *)
        | `DASH tok -> token env tok (* "-" *)
        | `BANG tok -> token env tok (* "!" *)
        | `TILDE tok -> token env tok (* "~" *)
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Tuple_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_expression env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_expression env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Case_blk x -> map_case_block env x
  | `Blk_ x -> map_block_ env x
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Num tok -> token env tok (* pattern [\d\.]+ *)
  | `Str x -> map_string_ env x
  )

and map_extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) =
  let v1 = token env v1 (* "extends" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_arguments env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_guard (env : env) ((v1, v2) : CST.guard) =
  let v1 = token env v1 (* "if" *) in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_infix_type (env : env) ((v1, v2, v3) : CST.infix_type) =
  let v1 = map_anon_choice_comp_type_334563f env v1 in
  let v2 = map_anon_choice_type_id_e16a528 env v2 in
  let v3 = map_anon_choice_comp_type_334563f env v3 in
  todo env (v1, v2, v3)

and map_interpolation (env : env) ((v1, v2) : CST.interpolation) =
  let v1 = token env v1 (* "$" *) in
  let v2 =
    (match v2 with
    | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | `Blk_ x -> map_block_ env x
    )
  in
  todo env (v1, v2)

and map_lower_bound (env : env) ((v1, v2) : CST.lower_bound) =
  let v1 = token env v1 (* ">:" *) in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_object_definition_ (env : env) ((v1, v2, v3) : CST.object_definition_) =
  let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 =
    (match v2 with
    | Some x -> map_extends_clause env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_template_body env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_param_type (env : env) (x : CST.param_type) =
  (match x with
  | `Type x -> map_type_ env x
  | `Lazy_param_type (v1, v2) ->
      let v1 = token env v1 (* "=>" *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  | `Repe_param_type (v1, v2) ->
      let v1 = map_type_ env v1 in
      let v2 = token env v2 (* "*" *) in
      todo env (v1, v2)
  )

and map_parameter (env : env) ((v1, v2, v3, v4) : CST.parameter) =
  let v1 = List.map (map_annotation env) v1 in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* ":" *) in
        let v2 = map_param_type env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_parameter_types (env : env) (x : CST.parameter_types) =
  (match x with
  | `Anno_type x -> map_annotated_type env x
  | `LPAR_opt_choice_type_rep_COMMA_choice_type_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = map_param_type env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_param_type env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `Comp_type x -> map_compound_type env x
  | `Infix_type x -> map_infix_type env x
  )

and map_parameters (env : env) ((v1, v2, v3, v4) : CST.parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* "implicit" *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = map_parameter env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_parameter env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Capt_pat (v1, v2, v3) ->
      let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 = token env v2 (* "@" *) in
      let v3 = map_pattern env v3 in
      todo env (v1, v2, v3)
  | `Tuple_pat (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_pattern env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_pattern env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Case_class_pat (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | `Stable_type_id x -> map_stable_type_identifier env x
        )
      in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = map_pattern env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_pattern env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Infix_pat (v1, v2, v3) ->
      let v1 = map_pattern env v1 in
      let v2 = map_anon_choice_type_id_e16a528 env v2 in
      let v3 = map_pattern env v3 in
      todo env (v1, v2, v3)
  | `Alt_pat (v1, v2, v3) ->
      let v1 = map_pattern env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_pattern env v3 in
      todo env (v1, v2, v3)
  | `Typed_pat (v1, v2, v3) ->
      let v1 = map_pattern env v1 in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Num tok -> token env tok (* pattern [\d\.]+ *)
  | `Str x -> map_string_ env x
  | `Wild tok -> token env tok (* "_" *)
  )

and map_simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Gene_type (v1, v2) ->
      let v1 = map_simple_type env v1 in
      let v2 = map_type_arguments env v2 in
      todo env (v1, v2)
  | `Proj_type (v1, v2, v3) ->
      let v1 = map_simple_type env v1 in
      let v2 = token env v2 (* "#" *) in
      let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
      todo env (v1, v2, v3)
  | `Tuple_type (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_ env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_type_ env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Stable_type_id x -> map_stable_type_identifier env x
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  )

and map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Simple_str tok -> token env tok (* simple_string *)
  | `Str_start_interp_rep_str_middle_interp_str_end (v1, v2, v3, v4) ->
      let v1 = token env v1 (* string_start *) in
      let v2 = map_interpolation env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* string_middle *) in
          let v2 = map_interpolation env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* string_end *) in
      todo env (v1, v2, v3, v4)
  | `Mult_str_start_interp_rep_mult_str_middle_interp_mult_str_end (v1, v2, v3, v4) ->
      let v1 = token env v1 (* multiline_string_start *) in
      let v2 = map_interpolation env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* multiline_string_middle *) in
          let v2 = map_interpolation env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* multiline_string_end *) in
      todo env (v1, v2, v3, v4)
  )

and map_template_body (env : env) ((v1, v2, v3) : CST.template_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> map_block env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Func_type (v1, v2, v3) ->
      let v1 = map_parameter_types env v1 in
      let v2 = token env v2 (* "=>" *) in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Comp_type x -> map_compound_type env x
  | `Infix_type x -> map_infix_type env x
  | `Anno_type x -> map_annotated_type env x
  )

and map_type_arguments (env : env) ((v1, v2, v3, v4) : CST.type_arguments) =
  let v1 = token env v1 (* "[" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)

and map_type_parameter (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | `Wild tok -> token env tok (* "_" *)
    | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    )
  in
  let v2 =
    (match v2 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_upper_bound env x
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_lower_bound env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some xs -> List.map (map_view_bound env) xs
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some xs -> List.map (map_context_bound env) xs
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

and map_type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = token env v1 (* "[" *) in
  let v2 = map_variant_type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_variant_type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)

and map_upper_bound (env : env) ((v1, v2) : CST.upper_bound) =
  let v1 = token env v1 (* "<:" *) in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_variant_type_parameter (env : env) ((v1, v2) : CST.variant_type_parameter) =
  let v1 = List.map (map_annotation env) v1 in
  let v2 =
    (match v2 with
    | `Cova_type_param x -> map_covariant_type_parameter env x
    | `Cont_type_param x ->
        map_contravariant_type_parameter env x
    | `Type_param x -> map_type_parameter env x
    )
  in
  todo env (v1, v2)

and map_view_bound (env : env) ((v1, v2) : CST.view_bound) =
  let v1 = token env v1 (* "<%" *) in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

let map_compilation_unit (env : env) (xs : CST.compilation_unit) =
  List.map (map_definition env) xs
