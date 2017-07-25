type program = declaration list
and declaration =
    Variable_dec of (type_t * declarator list)
  | Function_dec of (result_type * string * parameter list)
  | Function_def of
      (result_type * string * parameter list * declaration list *
       statement list)
and type_t = Types.typ
and declarator =
    Simple_declarator of string
  | Complex_declarator of (string * expression)
and result_type = type_t
and parameter =
    By_val_param of (type_t * string)
  | By_ref_param of (type_t * string)
and statement =
    Simple_expression of expression option
  | Statements of statement list
  | If_stmt of (expression * statement)
  | If_else_stmt of (expression * statement * statement)
  | For_loop of
      (string option * expression option * expression option *
       expression option * statement)
  | Branch of (string * string option)
  | Return of expression option
and expression =
    Id of string
  | Paren_expression of expression
  | Int of int
  | Double of float
  | Char of char
  | Bool of bool
  | String of string
  | Constant_exp of expression
  | Function_call of (string * expression list)
  | Array of (expression * expression)
  | Unary_op of (string * expression)
  | Binary_op of (expression * string * expression)
  | Prefix_unary_as of (string * expression)
  | Postfix_unary_as of (expression * string)
  | Binary_as of (expression * string * expression)
  | Casting of (type_t * expression)
  | Question of (expression * expression * expression)
  | New_op of (type_t * expression option)
  | Delete_op of expression
type final_typ =
    Type_none
  | Type_int
  | Type_double
  | Type_char
  | Type_bool
  | Type_string
  | Type_array of final_typ * int
type var_typ = Final_type of final_typ | Pointer of var_typ * final_typ
val program_tree : program option ref
val print_program : Format.formatter -> declaration list -> unit
val print_type_t : Format.formatter -> type_t -> unit
val print_declaration : Format.formatter -> declaration -> unit
val print_declarators : Format.formatter -> declarator list -> unit
val print_declarator : Format.formatter -> declarator -> unit
val print_parameters : Format.formatter -> parameter list -> unit
val print_parameter : Format.formatter -> parameter -> unit
val print_statements : Format.formatter -> statement list -> unit
val print_statement : Format.formatter -> statement -> unit
val print_expressions : Format.formatter -> expression list -> unit
val print_expression : Format.formatter -> expression -> unit
val pretty_print : Format.formatter -> declaration list option -> unit
val print_teliko : declaration list option -> unit
