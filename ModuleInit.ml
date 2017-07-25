open Llvm
open Types
open Scopes
open ExpCodeGen


let context = global_context ()
let the_module = ExpCodeGen.the_module
let builder = ExpCodeGen.builder
let named_values: (string,llvalue) Hashtbl.t = ExpCodeGen.named_values
let fun_names = ExpCodeGen.fun_names
let global_decs : (Ast.declaration) list ref = ref []
let integer_type  = i16_type context
let null_type = i1_type context
let bool_type = i1_type context
let char_type = i8_type context
let double_type = ExpCodeGen.double_type
let returns = ref true

let rec init_module prog =
  match prog with
  | Some decs -> init_module_with_decs decs
  | None -> init_module_with_decs []

and init_module_with_decs x = match x with
    [] -> const_null null_type
  | x::rest -> ignore (init_module_with_decl x); init_module_with_decs rest

and init_module_with_decl x = match x with
  | Ast.Function_dec _ -> ()
  | Ast.Variable_dec(ty, decs) ->
     let typos = type_to_lltype ty in (*we want the type of the vars *)
     let value = init_value ty in (* NOT SURE IF WE WANT THE POINTER TO BE NULL AT FIRST *)
     let _ = match !env with
       | Global (_) -> let _ = List.iter (fun x -> match x with
                                                   | Ast.Simple_declarator(name) -> ignore(env := update_env name (!env))
                                                   | Ast.Complex_declarator(name, _) -> ignore(env := update_env name (!env)))
                                         decs
                       in 
                       global_decs := Ast.Variable_dec(ty,decs)::!global_decs;
                       Printf.printf "New length:%d\n" (List.length !global_decs)
       | Nested _ ->ignore( 
                        List.map (fun dec ->
                            match dec with
                            | Ast.Simple_declarator(name) ->
                               Printf.printf "I'm here adding %s\n" name;
                               let alloca = build_alloca typos name builder in
                               ignore(build_store value alloca builder);
                               Hashtbl.add named_values name alloca;
                               env := update_env name (!env)
                            | Ast.Complex_declarator(name, exp) ->
                               let leng = code_gen_exp exp in
                               let decl = build_alloca (pointer_type typos) name builder in
                               let alloca = build_array_malloc (typos) leng "allocatmp" builder in (* or build array malloc/alloca *)
                               let _ = build_store alloca decl builder in
                               Hashtbl.add named_values name decl;
                               env := update_env name !env
                          (* HAVE TO CHECK IT AGAIN *)
                          ) decs) in ()
     
  | Ast.Function_def(ty, name, parameters, decls, stms) ->
     let parameters_old = parameters in
     env:= Nested([],!env);
     let env_params = difference_with_env !env parameters in 
     update_env_with_params parameters !env; (*Should create side effect*)
     let _ = print_env_pars !env in
     let env_params_types = get_env_params_types env_params !global_decs in
     let _ = print_hashtbl named_values in
     let llenv = Array.of_list env_params_types in
     let llpars = Array.of_list (List.map param_type parameters) in
     let llpars = if(name = "main") then llpars else  Array.append llpars llenv in
     let env_params_to_passed = List.map (fun x -> Ast.By_ref_param (TYPE_none,x)) env_params in
     let parameters = if(name = "main") then parameters else parameters@env_params_to_passed in
     let fun_typ = type_to_lltype ty in (* THATS WRONG HAVE TO CHANGE WITH MATCH *)
     let ft = function_type fun_typ llpars in (* creates a function *)
     let fn_name = String.concat "_" (name::!fun_names) in
     let the_fun = (match lookup_function fn_name the_module with
                    | None -> fun_names:= name :: !fun_names;
                              declare_function fn_name ft the_module
                    | Some f -> fun_names := name :: !fun_names; delete_function f; declare_function fn_name ft the_module  ) in (* create a new basic block for the function *)
     let _ = Array.iteri(fun i a ->
                 let n = (List.nth parameters i) in
                 match n with
                 | Ast.By_val_param(ty, name) ->
                    Printf.printf "Adding by_val param %s\n" name;
                    let typ = type_to_lltype ty in
                    set_value_name name a;
                    let alloca = build_alloca typ name builder in
                    let _ = Hashtbl.add named_values name alloca in
                    delete_instruction alloca
                 | Ast.By_ref_param(ty, name) ->
                    Printf.printf "Adding by_ref param %s\n" name;
                    let _ = set_value_name name a in
                    Hashtbl.add named_values name a
               )(params the_fun)in
     let _ = if(name = "main") then ignore(init_module_with_decs !global_decs) else ()in
     let _ = init_module_with_decs decls in
     let _ = print_hashtbl named_values in
     if (ty = TYPE_proc) then
       returns := false (* we have set a ret point yes *)
    else
      returns := true;
    Array.iteri(fun i a ->
        let n = (List.nth parameters i) in (*used to be i*)
        if(i >= (List.length parameters_old)) then () else
          match n with
          | Ast.By_val_param(ty, name) ->
             Hashtbl.remove named_values name
          | Ast.By_ref_param(ty, name) ->
             Hashtbl.remove named_values name )
               (params the_fun);
    remove_variable_delcarations decls;
    clear_env env_params;
    env := remove_env !env; (* Go higher in env*)
    Printf.printf ("Closing scope...\n");
    print_env_pars !env;
    print_hashtbl named_values; 
                                
and  type_to_lltype ty = match ty with
    
  | Types.TYPE_double -> double_type
  | Types.TYPE_int -> integer_type
  | Types.TYPE_bool -> bool_type
  | Types.TYPE_char -> i8_type context (* NOT SURE *)
  | Types.TYPE_array (t,n) -> let typ =type_to_lltype t in
                              array_type typ n
  | Types.TYPE_pointer x -> let t = type_to_lltype x in
                            pointer_type t
  | Types.TYPE_proc -> void_type context
  | _ -> null_type                        

and init_value ty = (match ty with
                     | TYPE_int  -> (const_int integer_type 0)
                     | TYPE_double  -> const_float double_type 0.0
                     | TYPE_bool  -> const_int bool_type 0
                     | TYPE_char  -> (const_int char_type 0)(* const_string context "" *)
                     | TYPE_pointer x  ->
                        ( let lltype = type_to_lltype (TYPE_pointer x) in
                          let lltype = const_pointer_null lltype in
                          (* dump_value lltype; *) lltype)
                     |_ -> raise (Type_error "problem with value allocation"))


and remove_variable_delcarations x = match x with
    [] -> ()
  | x::rest ->
     match x with
     |Ast.Variable_dec(ty,decs) ->
       ignore(
           List.map (fun dec -> match dec with
                                | Ast.Simple_declarator(name)
                                  | Ast.Complex_declarator(name,_)-> (* let reg = Str.regexp "_env" in *)
                                   (* let _ =Printf.printf "Didn't delete %s\n" name in *)
                                   (* let _ = try ignore(Str.search_forward reg name 0);  *)
                                   (*         with Not_found -> Hashtbl.remove named_values name; Printf.printf "Deleted\n" in *)
                                   (* () *)
                                   Hashtbl.remove named_values name
                    ) decs
         )
     |_-> remove_variable_delcarations rest

and param_type par = match par with
  | Ast.By_val_param(ty,name) -> type_to_lltype ty
  | Ast.By_ref_param(ty,name) -> let typ = type_to_lltype ty in
                                 pointer_type typ
