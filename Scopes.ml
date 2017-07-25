open Format
open Ast
open Symbol
open Symbtest
open Types
open Identifier
open Str
open TypeInference

exception Terminate

(* to compile with Str we use str.cma in ocaml *)

let find_return =  ref false
let for_loop = ref 0

let rec  check_program t = match t with
  | None -> printf("empty");
  | Some tree ->  openScope TYPE_proc; check_declarations (tree); check_main(); closeScope ()

and  check_declarations t = match t with
  | [] -> ()
  | x::rest -> check_declaration x;
               check_declarations rest;

and check_declaration t = match t with
  | Variable_dec (ty, decs) ->
     check_declarators ty decs;
  | Function_dec (ty, name, params)->
     let fun_name = name in (* stopping support of same name functions *)
     (* let suffix = add_suffix params in *)
     (* let fun_name = String.concat "" [name;"_"; suffix] in *)
     (* let _ = Printf.printf "adding fun dec %s\n" fun_name in *)
     let t = ( newFunction (id_make fun_name) true) in
     openScope(ty);
     ignore (List.map (registerParams t) params);
     ignore (endFunctionHeader t (ty));
     ignore (forwardFunction t);
     closeScope();
  | Function_def (ty, name, params, decls, stms) ->
     let fun_name = name in (* stopping support of same name functions *)
     (* let suffix = add_suffix params in *)
     (* let fun_name = String.concat "" [name;"_"; suffix] in *)
     (* let _  = Printf.printf "adding %s\n" fun_name in *)
     let t = ( Symbol.newFunction (id_make fun_name) true) in (* t is fun entry (ty, t)=a, params *)
     ignore(openScope(ty));
     ignore(List.map (registerParams t) params);  
     ignore(endFunctionHeader t (ty));
     check_declarations decls;
     (try check_statements stms with Type_error _ -> raise (Type_error ""));
     if (equalType (!currentScope.sco_type) (TYPE_proc)) && not ( !find_return) then (  find_return := false)
     else if( !find_return) then ( find_return := false;)  else Error.error "Couldn't find return in non void function" ;
     closeScope()
     

and check_declarators ty decs = match decs with
  | [] -> ()
  | [dec] -> check_declarator ty dec
  | dec :: rest -> check_declarator ty dec;
                   check_declarators ty rest

(* if we have an array we must have an int expression and the type of the array *)
and check_declarator ty dec = match dec with
  |Simple_declarator name->
    ignore (newVariable (id_make name) ( ty) true);
  |Complex_declarator (name, exp) ->
    if (equalType (findType exp) TYPE_int) then 
      ignore (newVariable (id_make name) (TYPE_array(ty,0)) true) (*we don't care for the size just typechecking... *)
    else Printf.printf "Error in array declaration"


(*  t is the function entry aka t in our case *)
and registerParams t param  = match param with
  | By_val_param (typ, name)->
     ignore (newParameter (id_make name) (typ) PASS_BY_VALUE t true)      	  
  | By_ref_param (typ, name)->
     ignore (newParameter (id_make name) (typ) PASS_BY_REFERENCE t true)


and check_statements stms = match stms with
  | [] -> ()
  | stm :: rest ->
     let _ = try check_statement stm with Type_error _ -> Printf.printf "\nThe Error was found here:\n"; printf "%a\n" print_statement stm; raise (Type_error "") in
     check_statements rest;

and check_statement stm = 
  match stm with
  | Simple_expression None -> ()
  | Simple_expression Some exp -> ignore (findType exp)
  | Statements stm-> check_statements stm
  | If_stmt (exp, stm) ->
     let _ = try equalType (findType exp) TYPE_bool with Type_error _ -> raise (Type_error "") in(* prepei na dei ama einai typoy bool i exp *)
     check_statement stm;
  | If_else_stmt (exp, stm1, stm2) ->
     let _ = try equalType (findType exp) TYPE_bool with Type_error _ ->  raise (Type_error "") in
     check_statement stm1;
     check_statement stm2;
  | For_loop (tag,e1,e2,e3,s) ->
     (match e1 with
        None -> ()
       |Some e -> ignore (findType e)
   );
   (match e2 with
      None -> ()
     |Some e -> ignore (equalType (findType e) TYPE_bool)
   );
   (match e3 with
      None -> ()
     |Some e -> ignore (findType e)
   );
   for_loop:= !for_loop +1;
   check_statement s;
   for_loop:= !for_loop - 1;                
| Branch (str1, str2)->
   (* check_loop s1; (\* checks if we are in a loop *\) *)
   if (!for_loop == 0) then Error.error "Break or continue not inside a loop" else ()

| Return ex ->
   (match ex with
    | None ->
       ignore (
    	   let typos = !currentScope.sco_type in
	   check_fun_type (typos) (TYPE_proc))
    | Some expr ->
       ignore (check_fun_type (!currentScope.sco_type) (findType expr)));
(* ignore(Symbtest.printSymbolTable());    *)

and check_fun_type scope_typ typ = 
  if (equalType scope_typ typ) then
    find_return := true
  else
    ( Printf.printf("Return type and fun type are not the same\n");
      raise Terminate )

and check_main () = 
  let main = lookupEntry (id_make "main") LOOKUP_CURRENT_SCOPE true in  (*look for main_ if you want tou support functions with same name ;) *)
  match main.entry_info with
  | ENTRY_function _ -> ()
  | _ -> Error.error "Couldn't find main function :("

(* Or simply add new function main and try to catch an exception? *)

and add_suffix param_list = 
  let suffix = List.map (fun x -> match x with | By_val_param (t,_) -> TypeInference.convert_type_to_char t | By_ref_param (t,_) -> TypeInference.convert_type_to_char t) param_list in String.concat "" suffix 

  

  
