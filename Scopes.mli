exception Terminate
val find_return : bool ref
val for_loop : int ref
val check_program : Ast.declaration list option -> unit
val check_declarations : Ast.declaration list -> unit
val check_declaration : Ast.declaration -> unit
val check_declarators : Ast.type_t -> Ast.declarator list -> unit
val check_declarator : Ast.type_t -> Ast.declarator -> unit
val registerParams : Symbol.entry -> Ast.parameter -> unit
val check_statements : Ast.statement list -> unit
val check_statement : Ast.statement -> unit
val check_fun_type : Types.typ -> Ast.type_t -> unit
val check_main : unit -> unit
val add_suffix : Ast.parameter list -> string
