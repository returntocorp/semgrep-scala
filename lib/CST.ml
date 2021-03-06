(* Generated by ocaml-tree-sitter. *)
(*
   scala grammar

   entrypoint: compilation_unit
*)

open! Sexplib.Conv
open Tree_sitter_run

type string_end = Token.t
[@@deriving sexp_of]

type string_middle = Token.t
[@@deriving sexp_of]

type automatic_semicolon = Token.t
[@@deriving sexp_of]

type operator_identifier =
  Token.t (* pattern "[^\\s\\w\\(\\)\\[\\]\\{\\}'\"`\\.;,]+" *)
[@@deriving sexp_of]

type string_start = Token.t
[@@deriving sexp_of]

type multiline_string_start = Token.t
[@@deriving sexp_of]

type number = Token.t (* pattern [\d\.]+ *)
[@@deriving sexp_of]

type modifiers =
  [
      `Abst of Token.t (* "abstract" *)
    | `Final of Token.t (* "final" *)
    | `Sealed of Token.t (* "sealed" *)
    | `Impl of Token.t (* "implicit" *)
    | `Lazy of Token.t (* "lazy" *)
    | `Over of Token.t (* "override" *)
    | `Priv of Token.t (* "private" *)
    | `Prot of Token.t (* "protected" *)
  ]
    list (* one or more *)
[@@deriving sexp_of]

type multiline_string_end = Token.t
[@@deriving sexp_of]

type multiline_string_middle = Token.t
[@@deriving sexp_of]

type identifier = Token.t (* pattern [a-zA-Z_]\w* *)
[@@deriving sexp_of]

type simple_string = Token.t
[@@deriving sexp_of]

type semicolon = [
    `SEMI of Token.t (* ";" *)
  | `Auto_semi of automatic_semicolon (*tok*)
]
[@@deriving sexp_of]

type package_identifier = (
    identifier (*tok*)
  * (Token.t (* "." *) * identifier (*tok*)) list (* zero or more *)
)
[@@deriving sexp_of]

type anon_choice_type_id_5555dfd = [
    `Id of identifier (*tok*)
  | `Stable_id of stable_identifier
]

and stable_identifier = (
    anon_choice_type_id_5555dfd * Token.t (* "." *) * identifier (*tok*)
)
[@@deriving sexp_of]

type anon_choice_type_id_e16a528 = [
    `Id of identifier (*tok*)
  | `Op_id of operator_identifier (*tok*)
]
[@@deriving sexp_of]

type anon_choice_type_id_ac5d537 = [
    `Id of identifier (*tok*)
  | `Rena_id of (
        identifier (*tok*)
      * Token.t (* "=>" *)
      * [ `Id of identifier (*tok*) | `Wild of Token.t (* "_" *) ]
    )
]
[@@deriving sexp_of]

type stable_type_identifier = (
    anon_choice_type_id_5555dfd * Token.t (* "." *) * identifier (*tok*)
)
[@@deriving sexp_of]

type import_selectors = (
    Token.t (* "{" *)
  * anon_choice_type_id_ac5d537
  * (Token.t (* "," *) * anon_choice_type_id_ac5d537) list (* zero or more *)
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type import_expression = (
    [ `Stable_id of stable_identifier | `Id of identifier (*tok*) ]
  * (
        Token.t (* "." *)
      * [ `Wild of Token.t (* "_" *) | `Import_selecs of import_selectors ]
    )
      option
)
[@@deriving sexp_of]

type annotated_type = (simple_type * annotation list (* zero or more *))

and annotation = (
    Token.t (* "@" *)
  * simple_type
  * arguments list (* zero or more *)
)

and anon_choice_comp_type_334563f = [
    `Comp_type of compound_type
  | `Infix_type of infix_type
  | `Anno_type of annotated_type
]

and anon_choice_exp_20a4ff7 = [
    `Exp of expression
  | `Choice_pack_clause of definition
]

and arguments = (
    Token.t (* "(" *)
  * (expression * (Token.t (* "," *) * expression) list (* zero or more *))
      option
  * Token.t (* ")" *)
)

and block = (
    anon_choice_exp_20a4ff7
  * (semicolon * anon_choice_exp_20a4ff7) list (* zero or more *)
  * semicolon option
)

and block_ = (Token.t (* "{" *) * block option * Token.t (* "}" *))

and case_block = [
    `LCURL_RCURL of (Token.t (* "{" *) * Token.t (* "}" *))
  | `LCURL_rep1_case_clause_RCURL of (
        Token.t (* "{" *)
      * case_clause list (* one or more *)
      * Token.t (* "}" *)
    )
]

and case_clause = (
    Token.t (* "case" *)
  * pattern
  * guard option
  * Token.t (* "=>" *)
  * block option
)

and catch_clause = (Token.t (* "catch" *) * case_block)

and class_parameter = (
    annotation list (* zero or more *)
  * [ `Val of Token.t (* "val" *) | `Var of Token.t (* "var" *) ] option
  * identifier (*tok*)
  * context_bound option
  * (Token.t (* "=" *) * expression) option
)

and class_parameters = (
    Token.t (* "(" *)
  * Token.t (* "implicit" *) option
  * (
        class_parameter
      * (Token.t (* "," *) * class_parameter) list (* zero or more *)
    )
      option
  * Token.t (* ")" *)
)

and compound_type = (
    annotated_type
  * (Token.t (* "with" *) * annotated_type) list (* one or more *)
)

and context_bound = (Token.t (* ":" *) * type_)

and contravariant_type_parameter = (Token.t (* "-" *) * type_parameter)

and covariant_type_parameter = (Token.t (* "+" *) * type_parameter)

and definition = [
    `Pack_clause of (
        Token.t (* "package" *)
      * package_identifier
      * template_body option
    )
  | `Pack_obj of (
        Token.t (* "package" *) * Token.t (* "object" *) * object_definition_
    )
  | `Class_defi of (
        annotation list (* zero or more *)
      * modifiers option
      * Token.t (* "case" *) option
      * Token.t (* "class" *)
      * identifier (*tok*)
      * type_parameters option
      * class_parameters list (* zero or more *)
      * extends_clause option
      * template_body option
    )
  | `Import_decl of (
        Token.t (* "import" *)
      * import_expression
      * (Token.t (* "," *) * import_expression) list (* zero or more *)
    )
  | `Obj_defi of (
        Token.t (* "case" *) option
      * Token.t (* "object" *)
      * object_definition_
    )
  | `Trait_defi of (
        Token.t (* "trait" *)
      * identifier (*tok*)
      * type_parameters option
      * extends_clause option
      * template_body option
    )
  | `Val_defi of (
        annotation list (* zero or more *)
      * modifiers option
      * Token.t (* "val" *)
      * pattern
      * context_bound option
      * Token.t (* "=" *)
      * expression
    )
  | `Val_decl of (
        annotation list (* zero or more *)
      * modifiers option
      * Token.t (* "val" *)
      * identifier (*tok*)
      * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
      * Token.t (* ":" *)
      * type_
    )
  | `Var_defi of (
        annotation list (* zero or more *)
      * modifiers option
      * Token.t (* "var" *)
      * pattern
      * context_bound option
      * Token.t (* "=" *)
      * expression
    )
  | `Var_decl of (
        annotation list (* zero or more *)
      * modifiers option
      * Token.t (* "var" *)
      * identifier (*tok*)
      * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
      * Token.t (* ":" *)
      * type_
    )
  | `Type_defi of (
        annotation list (* zero or more *)
      * modifiers option
      * Token.t (* "type" *)
      * identifier (*tok*)
      * type_parameters option
      * Token.t (* "=" *)
      * type_
    )
  | `Func_defi of (
        annotation list (* zero or more *)
      * modifiers option
      * Token.t (* "def" *)
      * identifier (*tok*)
      * type_parameters option
      * parameters list (* zero or more *)
      * context_bound option
      * [ `EQ_exp of (Token.t (* "=" *) * expression) | `Blk_ of block_ ]
    )
  | `Func_decl of (
        annotation list (* zero or more *)
      * modifiers option
      * Token.t (* "def" *)
      * identifier (*tok*)
      * type_parameters option
      * parameters list (* zero or more *)
      * context_bound option
    )
]

and expression = [
    `If_exp of (
        Token.t (* "if" *)
      * parenthesized_expression
      * expression
      * (Token.t (* "else" *) * expression) option
    )
  | `Match_exp of (expression * Token.t (* "match" *) * case_block)
  | `Try_exp of (
        Token.t (* "try" *)
      * expression
      * catch_clause option
      * finally_clause option
    )
  | `Call_exp of (
        expression
      * arguments
      * [ `Blk_ of block_ | `Case_blk of case_block ] option
    )
  | `Gene_func of (expression * type_arguments)
  | `Assign_exp of (expression * Token.t (* "=" *) * expression)
  | `Paren_exp of parenthesized_expression
  | `Str_tran_exp of (identifier (*tok*) * string_)
  | `Field_exp of (expression * Token.t (* "." *) * identifier (*tok*))
  | `Inst_exp of (Token.t (* "new" *) * expression)
  | `Infix_exp of (expression * anon_choice_type_id_e16a528 * expression)
  | `Prefix_exp of (
        [
            `PLUS of Token.t (* "+" *)
          | `DASH of Token.t (* "-" *)
          | `BANG of Token.t (* "!" *)
          | `TILDE of Token.t (* "~" *)
        ]
      * expression
    )
  | `Tuple_exp of (
        Token.t (* "(" *)
      * expression
      * (Token.t (* "," *) * expression) list (* one or more *)
      * Token.t (* ")" *)
    )
  | `Case_blk of case_block
  | `Blk_ of block_
  | `Id of identifier (*tok*)
  | `Num of number (*tok*)
  | `Str of string_
]

and extends_clause = (Token.t (* "extends" *) * type_ * arguments option)

and finally_clause = (Token.t (* "finally" *) * expression)

and guard = (Token.t (* "if" *) * expression)

and infix_type = (
    anon_choice_comp_type_334563f * anon_choice_type_id_e16a528
  * anon_choice_comp_type_334563f
)

and interpolation = (
    Token.t (* "$" *)
  * [ `Id of identifier (*tok*) | `Blk_ of block_ ]
)

and lower_bound = (Token.t (* ">:" *) * type_)

and object_definition_ = (
    identifier (*tok*)
  * extends_clause option
  * template_body option
)

and param_type = [
    `Type of type_
  | `Lazy_param_type of (Token.t (* "=>" *) * type_)
  | `Repe_param_type of (type_ * Token.t (* "*" *))
]

and parameter = (
    annotation list (* zero or more *)
  * identifier (*tok*)
  * (Token.t (* ":" *) * param_type) option
  * (Token.t (* "=" *) * expression) option
)

and parameter_types = [
    `Anno_type of annotated_type
  | `LPAR_opt_choice_type_rep_COMMA_choice_type_RPAR of (
        Token.t (* "(" *)
      * (
            param_type
          * (Token.t (* "," *) * param_type) list (* zero or more *)
        )
          option
      * Token.t (* ")" *)
    )
  | `Comp_type of compound_type
  | `Infix_type of infix_type
]

and parameters = (
    Token.t (* "(" *)
  * Token.t (* "implicit" *) option
  * (parameter * (Token.t (* "," *) * parameter) list (* zero or more *))
      option
  * Token.t (* ")" *)
)

and parenthesized_expression = (
    Token.t (* "(" *) * expression * Token.t (* ")" *)
)

and pattern = [
    `Id of identifier (*tok*)
  | `Capt_pat of (identifier (*tok*) * Token.t (* "@" *) * pattern)
  | `Tuple_pat of (
        Token.t (* "(" *)
      * pattern
      * (Token.t (* "," *) * pattern) list (* one or more *)
      * Token.t (* ")" *)
    )
  | `Case_class_pat of (
        [
            `Id of identifier (*tok*)
          | `Stable_type_id of stable_type_identifier
        ]
      * Token.t (* "(" *)
      * (pattern * (Token.t (* "," *) * pattern) list (* zero or more *))
          option
      * Token.t (* ")" *)
    )
  | `Infix_pat of (pattern * anon_choice_type_id_e16a528 * pattern)
  | `Alt_pat of (pattern * Token.t (* "|" *) * pattern)
  | `Typed_pat of (pattern * Token.t (* ":" *) * type_)
  | `Num of number (*tok*)
  | `Str of string_
  | `Wild of Token.t (* "_" *)
]

and simple_type = [
    `Gene_type of (simple_type * type_arguments)
  | `Proj_type of (simple_type * Token.t (* "#" *) * identifier (*tok*))
  | `Tuple_type of (
        Token.t (* "(" *)
      * type_
      * (Token.t (* "," *) * type_) list (* zero or more *)
      * Token.t (* ")" *)
    )
  | `Stable_type_id of stable_type_identifier
  | `Id of identifier (*tok*)
]

and string_ = [
    `Simple_str of simple_string (*tok*)
  | `Str_start_interp_rep_str_middle_interp_str_end of (
        string_start (*tok*)
      * interpolation
      * (string_middle (*tok*) * interpolation) list (* zero or more *)
      * string_end (*tok*)
    )
  | `Mult_str_start_interp_rep_mult_str_middle_interp_mult_str_end of (
        multiline_string_start (*tok*)
      * interpolation
      * (multiline_string_middle (*tok*) * interpolation)
          list (* zero or more *)
      * multiline_string_end (*tok*)
    )
]

and template_body = (Token.t (* "{" *) * block option * Token.t (* "}" *))

and type_ = [
    `Func_type of (parameter_types * Token.t (* "=>" *) * type_)
  | `Comp_type of compound_type
  | `Infix_type of infix_type
  | `Anno_type of annotated_type
]

and type_arguments = (
    Token.t (* "[" *)
  * type_
  * (Token.t (* "," *) * type_) list (* zero or more *)
  * Token.t (* "]" *)
)

and type_parameter = (
    [ `Wild of Token.t (* "_" *) | `Id of identifier (*tok*) ]
  * type_parameters option
  * upper_bound option
  * lower_bound option
  * view_bound list (* zero or more *) option
  * context_bound list (* zero or more *) option
)

and type_parameters = (
    Token.t (* "[" *)
  * variant_type_parameter
  * (Token.t (* "," *) * variant_type_parameter) list (* zero or more *)
  * Token.t (* "]" *)
)

and upper_bound = (Token.t (* "<:" *) * type_)

and variant_type_parameter = (
    annotation list (* zero or more *)
  * [
        `Cova_type_param of covariant_type_parameter
      | `Cont_type_param of contravariant_type_parameter
      | `Type_param of type_parameter
    ]
)

and view_bound = (Token.t (* "<%" *) * type_)
[@@deriving sexp_of]

type compilation_unit = definition list (* zero or more *)
[@@deriving sexp_of]

type comment (* inlined *) = Token.t
[@@deriving sexp_of]

type wildcard (* inlined *) = Token.t (* "_" *)
[@@deriving sexp_of]

type type_identifier (* inlined *) = identifier (*tok*)
[@@deriving sexp_of]

type renamed_identifier (* inlined *) = (
    identifier (*tok*)
  * Token.t (* "=>" *)
  * [ `Id of identifier (*tok*) | `Wild of Token.t (* "_" *) ]
)
[@@deriving sexp_of]

type import_declaration (* inlined *) = (
    Token.t (* "import" *)
  * import_expression
  * (Token.t (* "," *) * import_expression) list (* zero or more *)
)
[@@deriving sexp_of]

type alternative_pattern (* inlined *) = (
    pattern * Token.t (* "|" *) * pattern
)
[@@deriving sexp_of]

type assignment_expression (* inlined *) = (
    expression * Token.t (* "=" *) * expression
)
[@@deriving sexp_of]

type call_expression (* inlined *) = (
    expression
  * arguments
  * [ `Blk_ of block_ | `Case_blk of case_block ] option
)
[@@deriving sexp_of]

type capture_pattern (* inlined *) = (
    identifier (*tok*) * Token.t (* "@" *) * pattern
)
[@@deriving sexp_of]

type case_class_pattern (* inlined *) = (
    [ `Id of identifier (*tok*) | `Stable_type_id of stable_type_identifier ]
  * Token.t (* "(" *)
  * (pattern * (Token.t (* "," *) * pattern) list (* zero or more *)) option
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type class_definition (* inlined *) = (
    annotation list (* zero or more *)
  * modifiers option
  * Token.t (* "case" *) option
  * Token.t (* "class" *)
  * identifier (*tok*)
  * type_parameters option
  * class_parameters list (* zero or more *)
  * extends_clause option
  * template_body option
)
[@@deriving sexp_of]

type field_expression (* inlined *) = (
    expression * Token.t (* "." *) * identifier (*tok*)
)
[@@deriving sexp_of]

type function_declaration (* inlined *) = (
    annotation list (* zero or more *)
  * modifiers option
  * Token.t (* "def" *)
  * identifier (*tok*)
  * type_parameters option
  * parameters list (* zero or more *)
  * context_bound option
)
[@@deriving sexp_of]

type function_definition (* inlined *) = (
    annotation list (* zero or more *)
  * modifiers option
  * Token.t (* "def" *)
  * identifier (*tok*)
  * type_parameters option
  * parameters list (* zero or more *)
  * context_bound option
  * [ `EQ_exp of (Token.t (* "=" *) * expression) | `Blk_ of block_ ]
)
[@@deriving sexp_of]

type function_type (* inlined *) = (
    parameter_types * Token.t (* "=>" *) * type_
)
[@@deriving sexp_of]

type generic_function (* inlined *) = (expression * type_arguments)
[@@deriving sexp_of]

type generic_type (* inlined *) = (simple_type * type_arguments)
[@@deriving sexp_of]

type if_expression (* inlined *) = (
    Token.t (* "if" *)
  * parenthesized_expression
  * expression
  * (Token.t (* "else" *) * expression) option
)
[@@deriving sexp_of]

type infix_expression (* inlined *) = (
    expression * anon_choice_type_id_e16a528 * expression
)
[@@deriving sexp_of]

type infix_pattern (* inlined *) = (
    pattern * anon_choice_type_id_e16a528 * pattern
)
[@@deriving sexp_of]

type instance_expression (* inlined *) = (Token.t (* "new" *) * expression)
[@@deriving sexp_of]

type lazy_parameter_type (* inlined *) = (Token.t (* "=>" *) * type_)
[@@deriving sexp_of]

type match_expression (* inlined *) = (
    expression * Token.t (* "match" *) * case_block
)
[@@deriving sexp_of]

type object_definition (* inlined *) = (
    Token.t (* "case" *) option
  * Token.t (* "object" *)
  * object_definition_
)
[@@deriving sexp_of]

type package_clause (* inlined *) = (
    Token.t (* "package" *)
  * package_identifier
  * template_body option
)
[@@deriving sexp_of]

type package_object (* inlined *) = (
    Token.t (* "package" *) * Token.t (* "object" *) * object_definition_
)
[@@deriving sexp_of]

type prefix_expression (* inlined *) = (
    [
        `PLUS of Token.t (* "+" *)
      | `DASH of Token.t (* "-" *)
      | `BANG of Token.t (* "!" *)
      | `TILDE of Token.t (* "~" *)
    ]
  * expression
)
[@@deriving sexp_of]

type projected_type (* inlined *) = (
    simple_type * Token.t (* "#" *) * identifier (*tok*)
)
[@@deriving sexp_of]

type repeated_parameter_type (* inlined *) = (type_ * Token.t (* "*" *))
[@@deriving sexp_of]

type string_transform_expression (* inlined *) = (
    identifier (*tok*) * string_
)
[@@deriving sexp_of]

type trait_definition (* inlined *) = (
    Token.t (* "trait" *)
  * identifier (*tok*)
  * type_parameters option
  * extends_clause option
  * template_body option
)
[@@deriving sexp_of]

type try_expression (* inlined *) = (
    Token.t (* "try" *)
  * expression
  * catch_clause option
  * finally_clause option
)
[@@deriving sexp_of]

type tuple_expression (* inlined *) = (
    Token.t (* "(" *)
  * expression
  * (Token.t (* "," *) * expression) list (* one or more *)
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type tuple_pattern (* inlined *) = (
    Token.t (* "(" *)
  * pattern
  * (Token.t (* "," *) * pattern) list (* one or more *)
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type tuple_type (* inlined *) = (
    Token.t (* "(" *)
  * type_
  * (Token.t (* "," *) * type_) list (* zero or more *)
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type type_definition (* inlined *) = (
    annotation list (* zero or more *)
  * modifiers option
  * Token.t (* "type" *)
  * identifier (*tok*)
  * type_parameters option
  * Token.t (* "=" *)
  * type_
)
[@@deriving sexp_of]

type typed_pattern (* inlined *) = (pattern * Token.t (* ":" *) * type_)
[@@deriving sexp_of]

type val_declaration (* inlined *) = (
    annotation list (* zero or more *)
  * modifiers option
  * Token.t (* "val" *)
  * identifier (*tok*)
  * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
  * Token.t (* ":" *)
  * type_
)
[@@deriving sexp_of]

type val_definition (* inlined *) = (
    annotation list (* zero or more *)
  * modifiers option
  * Token.t (* "val" *)
  * pattern
  * context_bound option
  * Token.t (* "=" *)
  * expression
)
[@@deriving sexp_of]

type var_declaration (* inlined *) = (
    annotation list (* zero or more *)
  * modifiers option
  * Token.t (* "var" *)
  * identifier (*tok*)
  * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
  * Token.t (* ":" *)
  * type_
)
[@@deriving sexp_of]

type var_definition (* inlined *) = (
    annotation list (* zero or more *)
  * modifiers option
  * Token.t (* "var" *)
  * pattern
  * context_bound option
  * Token.t (* "=" *)
  * expression
)
[@@deriving sexp_of]

let dump_tree root =
  sexp_of_compilation_unit root
  |> Print_sexp.to_stdout
