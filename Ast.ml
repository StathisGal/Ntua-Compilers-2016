open Format
open Types
       
type program = declaration list and
     declaration =  |Variable_dec of (type_t * declarator list)
                    |Function_dec of (result_type * string * parameter list)
                    |Function_def of (result_type * string * parameter list * declaration list * statement list) and
     type_t = Types.typ and
     declarator = Simple_declarator of string
                |Complex_declarator of (string * expression) and
     result_type = type_t and
     parameter = By_val_param of (type_t * string)
               | By_ref_param of (type_t * string) and
     statement = Simple_expression  of expression option
               |Statements of statement list
               |If_stmt of (expression * statement)
               |If_else_stmt of (expression * statement * statement)   
               |For_loop of (string option * expression option * expression option  * expression option *  statement)
               |Branch of (string * string option)
               |Return of expression option and

     expression = Id of string
                |Paren_expression of expression
                |Int of int
                |Double of float
                |Char of char
                |Bool of bool 
                |String of string
                |Constant_exp of expression (* prepei na to allaksoume giati den ginetai na epistrefei expression *)
                |Function_call of (string * expression list)
                |Array of (expression * expression)
                |Unary_op of (string * expression)
                |Binary_op of (expression * string * expression)
                |Prefix_unary_as of (string * expression)
                |Postfix_unary_as of (expression * string)
                |Binary_as of (expression * string * expression)
                |Casting of (type_t * expression)
                |Question of (expression * expression * expression)
                |New_op of (type_t * expression option ) (* thelei apo mprosta to new gi auto kai evala to string *)
                |Delete_op of (expression)  (* to idio *)

(* final types of a var *)
 type final_typ = 
|Type_none
|Type_int
|Type_double
|Type_char
|Type_bool
|Type_string
|Type_array of final_typ * int

(* tyre of a var *)
type var_typ =
| Final_type of final_typ
| Pointer of var_typ * final_typ

let program_tree = ref None


let rec print_program out t = match t with
| [] -> (fprintf out "empty\n")
| h :: [] -> print_declaration out h
| h :: t ->
print_declaration out h;
print_newline ();
print_newline ();
print_program out t

and print_type_t out typ =   match typ with
  | TYPE_none ->
      fprintf out "<undefined (probably wrong)>"
  | TYPE_int ->
      fprintf out " int "
  | TYPE_double ->
      fprintf out " double "
  | TYPE_char -> 
      fprintf out " char "
  | TYPE_bool ->
      fprintf out " boolean "
  | TYPE_pointer (ty) ->
      fprintf out "pointer ";
      print_type_t out ty
  | TYPE_array (et, sz) ->
      print_type_t out et;
      if sz > 0 then
        fprintf out " [%d] " sz
      else
        fprintf out " [] "
  | TYPE_byte -> fprintf out "byte"
  | TYPE_proc ->
      fprintf out " void "
 
and print_declaration out t = match t with
| Variable_dec (t1, decs) ->
 fprintf out "Variable_dec( ";
 print_type_t out t1 ;
 print_declarators out decs;
 fprintf out ")";
 force_newline();
| Function_dec (t1, s1, pars) ->
  fprintf out "Function_dec ";
  print_type_t out t1;
  fprintf out  " %s( " s1;
  print_parameters out pars;
  fprintf out ") ";
  force_newline ();
| Function_def (t1, s1, par, dec, stm) ->
  fprintf out "Function_def";
  print_type_t out t1;
  fprintf out "  %s("  s1;
  print_parameters out par;
  fprintf out ")";
  force_newline ();
  fprintf out " {";
  force_newline ();
  open_hovbox 2;
  fprintf out "\t  val-fun declaration \n";
  print_program out dec; (* edw evala prin_program gt einai list *)
  force_newline ();
  fprintf out "\t stmts declaration \n";
  print_statements out stm;
  close_box ();
  force_newline ();
  fprintf out "} Function end"

and print_declarators out t = match t with
| [] -> ()
| [dec] -> print_declarator out dec;
| dec :: rest ->
print_declarator out dec;
fprintf out ", ";
print_declarators out rest

and print_declarator out t = match t with
| Simple_declarator t ->
fprintf out "Simple_declarator %s" t;
| Complex_declarator(s,e) ->
fprintf out "Complex_declarator %s  " s;
print_expression out e


and print_parameters out t = match t with
| [] -> ()
| [param] -> print_parameter out param
| param :: rest ->
print_parameter out param;
fprintf out ", ";
print_parameters out rest

and print_parameter out t = match t with
| By_val_param (s1,s2) ->
fprintf out "By_val_param ";
print_type_t out s1;
fprintf out " %s " s2;
| By_ref_param (s1,s2) ->
fprintf out "By_ref_param ";
print_type_t out s1;
fprintf out " %s "  s2

and print_statements out t = match t with
| [] -> ()
| [stm] ->
  print_statement out stm
| stm :: rest ->
  print_statement out stm;
force_newline ();
print_statements out rest

and print_statement out t = match t with
| Statements stms ->
  force_newline();	
  print_statements out stms;
| Simple_expression ex ->
  ( match ex with
    | None -> ()
    | Some expr -> 
    (force_newline();
    print_expression out expr;)
  );
| If_stmt (ex, stm) -> 
   fprintf out "If_stmt (";
   print_expression out ex;
   fprintf out ") then ";
   open_hovbox 2;
   print_statement out stm;
   close_box ();
| If_else_stmt (e,s1, s2) ->
   fprintf out "If_stmt (";
   print_expression out e;
   fprintf out ") then ";
   open_hovbox 2;
   print_statement out s1;
   close_box();
   force_newline ();
   fprintf out "else ";
   open_hovbox 2;
   print_statement out s2;
    close_box();
| For_loop (s1, e1, e2, e3, stmt) ->
  fprintf out "for (";
  ( match s1 with
  | None -> ()
  | Some s -> fprintf out "%s " s;
    );
  ( match e1 with
  | None -> ()
  | Some e -> print_expression out e;
  fprintf out "; ";
    );
  ( match e2 with
  | None -> ()
  | Some e -> print_expression out e;
  fprintf out "; ";
    );
  ( match e3 with
  | None -> ()
  | Some e -> print_expression out e;
    );
  fprintf out ")";
  force_newline ();
  open_hovbox 2;
  print_statement out stmt;
  force_newline ();
  close_box()
| Branch (s1, s2) ->
  fprintf out "Branch %s" s1;
  (match s2 with
   | None -> ()
   | Some s -> fprintf out  " %s " s;
   );
| Return exp ->
  ( match exp with
  | None -> fprintf out "return;\n";
  | Some e -> fprintf out "return ";
    print_expression out e;
  fprintf out ";\n";
    )

and print_expressions out t = match t with
| [] -> ()
| [exp] -> print_expression out exp;
| exp :: rest -> print_expression out exp;
fprintf out ", ";
print_expressions out rest

and print_expression out t = match t with
|  Paren_expression e ->
fprintf out "(";
print_expression out e;
fprintf out ")"
|  Id n ->
fprintf out "Id %s" n;
| Int n -> fprintf out "Int %d" n
| Double n -> fprintf out "Double %f" n
| Char n -> fprintf out "Char %c" n
| Bool n -> fprintf out "Bool %b" n
| String n -> fprintf out "String %s" n
| Constant_exp n ->                 (*edw exoume provlima *)
 print_expression out  n;
| Function_call (s, e) ->
   fprintf out ("Function_call (%s, ") s;
   print_expressions out e;
   fprintf out (" );");
| Array (t1, t2) ->  (* na dw giati einai expression * expression*)
   fprintf out "Array (";
   print_expression out t1;
   fprintf out "[";
   print_expression out t2;
   fprintf out "]";
   fprintf out ")"
| Unary_op (s, e) ->
   fprintf out "Unary_op (%s, " s;
   print_expression out e;
   fprintf out ")"
| Binary_op (t1,s,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  s;
   print_expression out t2;
   fprintf out ")"
| Prefix_unary_as (s, t) ->
   fprintf out "Prefix_unary_as (%s, " s;
   print_expression out t;
   fprintf out ")"
| Postfix_unary_as (t, s) ->
   fprintf out "Postfix_unary_as(";
   print_expression out t;
   fprintf out ", %s) " s
| Binary_as (t1, s, t2) ->
   fprintf out "Binary_as (";
   print_expression out t1;
   fprintf out " %s "  s;
   print_expression out t2;
   fprintf out ")"
| Casting (ty, e)->
   fprintf out "Casting( ";
   print_type_t out ty;
   print_expression out e;
   fprintf out ")"
|  Question  (t1, t2,t3) ->
    fprintf out "Question(";
    print_expression out t1;
    fprintf out " ? ";
    print_expression out t2;
    fprintf out " : ";
    print_expression out t3;
    fprintf out ")"
| New_op (y, ex) ->
   fprintf out "New( ";
   print_type_t  out  y;
   ( match ex with
     | None -> ()
     | Some exp -> print_expression out exp;
   );
   fprintf out ")"
| Delete_op  (t) ->
   fprintf out "Delete( ";
   print_expression out t;
   fprintf out ")"

and pretty_print out t = match t with
  | None -> printf "Empty"
  | Some tree -> print_program out tree

let print_teliko progr =
  force_newline ();
  printf  "*** Pretty Printing AST ***";
  force_newline ();
  printf "***************************";
  force_newline ();
  printf "%a" pretty_print  progr;
  force_newline ()
