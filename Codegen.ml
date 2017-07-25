open Llvm
open Types
open Scopes
open ExpCodeGen
open Ast
       
exception Error of string

let context = global_context ()
let the_module = ExpCodeGen.the_module
let builder =  ExpCodeGen.builder
let named_values:(string, llvalue) Hashtbl.t = ExpCodeGen.named_values
let integer_type  = i16_type context
let null_type = i1_type context
let bool_type = i8_type context
let char_type = i8_type context
let double_type = ExpCodeGen.double_type
let fun_names = ExpCodeGen.fun_names                        
let fun_bbs : llbasicblock list ref = ref []                                      
let returns = ref true
let continue_tags : (string *llbasicblock) list ref = ref []
let break_tags : (string * llbasicblock ) list ref = ref []
let old_bindings : (string * llvalue) list ref = ref []
let global_decs : (Ast.declaration) list ref = ref []
(* this fun takes the type of the elem and returns the lltype *)

                                                     
let rec type_to_lltype ty = match ty with
    
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

and print_decs decs =
  List.iter (fun x -> match x with
                      | Variable_dec _ -> Printf.printf "Variable_list\n"
                      | Function_dec (_,name,_) -> Printf.printf "Declaration of %s\n" name
                      | Function_def  (_,name,_,_,_) -> Printf.printf "Definition of %s\n" name) decs
            
and sort_decls dec_list =
  let (variables,rest) = List.partition (fun x -> match x with Variable_dec _ -> true | _ -> false) dec_list in
  let (definitions,declarations) = List.partition (fun x -> match x with Function_def _ -> true | _ -> false) rest in
  let dec_to_def defs dec =
    let def_eqs_dec  dec def =
      match def,dec with
      |Function_def (_,f,_,_,_), Function_dec(_,c,_) -> f = c
      | _ ,_ -> false
    in try List.find (def_eqs_dec dec) defs
       with Not_found -> dec
  in
  let def_to_dec def = match def with
    |Function_def (ty,name,args,_,_) -> Function_def(ty,name,args,[],[]) (*Function_dec initialy*)
    | _ -> raise Terminate in
  let alldecs = List.map def_to_dec definitions in
  let ordered_defs_and_lib_decs = List.map (dec_to_def definitions) declarations in
  let (ordered_defs,lib_decs) = List.partition (fun x -> match x with Function_def _ -> true | _ -> false) ordered_defs_and_lib_decs in
  (* let _ = Printf.printf "%d replacements\n" (List.length ordered_defs) in *)
  let rest_defs = List.filter (fun x -> let b = List.exists ((fun x y-> x = y) x) ordered_defs in not b ) definitions in
  let defs = sort_definitions(ordered_defs@rest_defs) in (* ordered_defs@rest_defs in *)
  lib_decs@variables@alldecs@defs

and dec_with_name name dec =
  match dec with Function_dec(_,n,_) -> n = name | _ -> false

  
and sort_definitions defs =
  let compare d1 d2 =
    match d1, d2 with
    |Function_def(_,n1,_,dec1,_), Function_def(_,n2,_,dec2,_) ->
      if(n1 = n2) then 0 else(
        let (defs,decs) = List.partition (fun x -> match x with Function_def _ -> true |_ -> false) dec2 in
        if(List.exists (dec_with_name n1) decs ) then -1
        else if(List.exists (fun x -> (compare d1 x) = -1) defs) then -1 (*a def is smaller than*)
        else (match d2, d1 with
              |Function_def(_,n1,_,dec1,_), Function_def(_,n2,_,dec2,_) ->
                if(n1 = n2) then 0 else(
                  let (defs,decs) = List.partition (fun x -> match x with Function_def _ -> true |_ -> false) dec2 in
                  if(List.exists (dec_with_name n1) decs ) then 1
                  else if(List.exists (fun x -> (compare d1 x) = -1) defs) then 1 else 0) (*a def is smaller than*)
              | _, _ -> 0)
      )
               
    (*used to be just 0*)
    |_,_ -> 1
  in List.sort compare defs
             
  
        
and param_type par = match par with
  | Ast.By_val_param(ty,name) -> type_to_lltype ty
  | Ast.By_ref_param(ty,name) -> let typ = type_to_lltype ty in
                                 pointer_type typ

and find_block stag tuple = match tuple with
  | [] -> raise Not_found
  | (tag, tag_bb):: rest -> if stag = tag then tag_bb
                            else
                              find_block stag rest
let rec print_list = function 
    [] -> ()
  | (e,s)::l -> print_string e ; Printf.printf " " ; print_list l
                                                                
let create_entry_block_alloca the_function var_name ty =
  let typ = type_to_lltype ty in
  let builder = builder_at context (instr_begin (entry_block the_function)) in (* we make a builder which points at the first instruction of the block and allocates memory there *)
  build_alloca typ var_name builder

let rec is_return_last_instuction stmts = match stmts with
  | [] -> false
  | [Ast.Return _] -> true
  | _::tail -> is_return_last_instuction tail
                                         
(* HAVE TO CHANGE IT TO UNIT *)
let rec codegen_program prog =
  match prog with
  | Some decs -> let decs = sort_decls decs in codegen_declars decs
  | None -> codegen_declars []
                            
and codegen_declars x = match x with
  |  [] -> const_null null_type
  | x::rest ->
     ignore(codegen_declaration x);
     codegen_declars rest
                     
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
                                       
                                       
and codegen_declaration x = match x with
  (* maybe we dont need the fun dec but never mind *)

  | Ast.Function_dec(ty, name, parameters) ->
     if(match !env with Global _ -> true | _ ->   let fn_name_t = String.concat "_" (name::List.tl(!fun_names)) in
                                                  match lookup_function fn_name_t the_module with None -> true | _ -> false) then(
       
       (* if(match !env with Global _ -> true | _ -> true) then( *)
       (* Printf.printf("Fun dec\n"); *)
       let llpars = Array.of_list (List.map param_type parameters) in   (* llpars is an array of the params *)
       let fun_typ = type_to_lltype ty in                           (* THATS WRONG HAVE TO CHANGE WITH MATCH *)
       let ft = function_type fun_typ llpars in
       let fn_name = String.concat "_" (name::!fun_names) in
       let f = declare_function fn_name ft the_module in
       Array.iteri ( fun i a ->
                     let p = (List.nth parameters i) in
                     match p with
                     | Ast.By_val_param(ty, name)->
                        (* HAVE TO SEE THE WAY WE TAKE IT *)
                        set_value_name name a;
                     (* Printf.printf "adding %s...\n" name; *)
                     (* Hashtbl.add named_values name a *)
                     | Ast.By_ref_param(ty, name)->
                        set_value_name name a;
                   (* Printf.printf "adding %s...\n" name; *)
                   (* Hashtbl.add named_values name a *)
                   )
                   (params f); f
     )else const_null null_type
  | Ast.Variable_dec(ty, decs) ->
     (* Printf.printf("Var dec\n"); *)
     (*let _ = block_parent (insertion_block builder) in*)
     let typos = type_to_lltype ty in (*we want the type of the vars *)
     let value = init_value ty in (* NOT SURE IF WE WANT THE POINTER TO BE NULL AT FIRST *)
     let _ = match !env with
       | Global (_) -> let _ = List.iter (fun x -> match x with
                                                   | Ast.Simple_declarator(name) -> ignore(env := update_env name (!env))
                                                   | Ast.Complex_declarator(name, _) -> ignore(env := update_env name (!env)))
                                         decs
                       in 
                       global_decs := Ast.Variable_dec(ty,decs)::!global_decs;
       (* Printf.printf "New length:%d\n" (List.length !global_decs) *)
       | Nested _ ->ignore( 
                        List.map (fun dec ->
                            match dec with
                            | Ast.Simple_declarator(name) ->
                               (* Printf.printf "I'm here adding %s\n" name; *)
                               let alloca = build_alloca typos name builder in
                               ignore(build_store value alloca builder);
                               Hashtbl.add named_values name alloca;
                               env := update_env name (!env)
                            | Ast.Complex_declarator(name, exp) ->
                               let leng = code_gen_exp exp in
                               (* dump_value leng; *)(* we have the length of the array in llvalue *)
                               let decl = build_alloca (pointer_type typos) name builder in
                               let alloca = build_array_malloc (typos) leng "allocatmp" builder in (* or build array malloc/alloca *)
                               let _ = build_store alloca decl builder in
                               Hashtbl.add named_values name decl;
                               env := update_env name !env
                          (* HAVE TO CHECK IT AGAIN *)
                          ) decs) in
     const_null null_type;

  | Ast.Function_def(ty, name, parameters, decls, stms) ->
     (* Printf.printf "Fun def\n"; *)
     let parameters_old = parameters in
     env:= Nested([],!env);
     let env_params = difference_with_env !env parameters in 
     update_env_with_params parameters !env; (*Should create side effect*)
     (* let _ = print_env_pars !env in *)
     let env_params_types = get_env_params_types env_params !global_decs in
     (* let _ = print_hashtbl named_values in *)
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
                    (* | Some f -> fun_names := name::!fun_names; f in *)
                    | Some f -> fun_names := name :: !fun_names; delete_function f; declare_function fn_name ft the_module  ) in
     (* create a new basic block for the function *)
     let label = String.concat "_" [name;"entry"] in
   
     let bb= append_block context label the_fun in
     fun_bbs := bb :: !fun_bbs;
     position_at_end bb builder; (* point at the end of the new created block *)
     (* Printf.printf "%s will be called with %d params\n" name (Array.length (params the_fun)); *)
     (* we initialize the params and add them to Hashtable *)
     let _ = Array.iteri(fun i a ->
                 let n = (List.nth parameters i) in
                 match n with
                 | Ast.By_val_param(ty, name) ->
                    (* Printf.printf "Adding by_val param %s\n" name; *)
                    let typ = type_to_lltype ty in
                    set_value_name name a;
                    let alloca = build_alloca typ name builder in
                    ignore(build_store a alloca builder);
                    Hashtbl.add named_values name alloca;
                 | Ast.By_ref_param(ty, name) ->
                    (* Printf.printf "Adding by_ref param %s\n" name; *)
                    let _ = set_value_name name a in
                    Hashtbl.add named_values name a
               (*let _ = match ty with |TYPE_none -> ignore(update_env name !env) | _ ->() in *) (*Add the env_params*)
               (* match ty with TYPE_none -> () | _ -> Hashtbl.add named_values name a) *)
               (*TYPE_none == env_variable already added. We don't want dubplicates*)
               )(params the_fun)in
     let _ = if(name = "main") then ignore(codegen_declars !global_decs) else ()in
     let decls = sort_decls decls in
     (* Printf.printf "decls length %d\n" (List.length decls); *)
     let _ = codegen_declars decls in
     (* let _ = print_hashtbl named_values in *)
     if (ty = TYPE_proc) then
       returns := false (* we have set a ret point yes *)
     else
       returns := true;
     let bb = List.hd !fun_bbs in
     position_at_end bb builder;
     let _ =  codegen_states stms  in

     fun_bbs := List.tl !fun_bbs;
     let next_bb = try List.hd !fun_bbs
                   with Failure ("hd") -> bb in
     fun_names := List.tl !fun_names;
     Array.iteri(fun i a ->
         let n = (List.nth parameters i) in (*used to be i*)
         if(i >= (List.length parameters_old)) then () else
           match n with
           | Ast.By_val_param(ty, name) ->
              Hashtbl.remove named_values name
           | Ast.By_ref_param(ty, name) ->
              Hashtbl.remove named_values name )
                (params the_fun);
     (*FREE DECLS*)
     remove_variable_delcarations decls;
     clear_env env_params;
     env := remove_env !env; (* Go higher in env*)
     (* Printf.printf ("Closing scope...\n"); *)
     (* print_env_pars !env; *)
     (* print_hashtbl named_values; *)
     if !returns = false then 
       (ignore(build_ret_void builder); position_at_end next_bb builder; the_fun)
     else
       (
         let _ = if (is_return_last_instuction stms) then () else ignore(build_ret (init_value ty) builder) in
         position_at_end next_bb builder; the_fun );


and codegen_states st = match st with
  | [] -> const_null null_type
  | x::rest -> ignore(codegen_statement x);
               codegen_states rest
                              
and codegen_statement st = match st with
  | Ast.Simple_expression(None) -> const_null integer_type
  | Ast.Simple_expression(Some exp) -> code_gen_exp exp
  | Ast.Statements sts -> codegen_states sts
  | Ast.If_stmt(cond, stm) ->
     let condition = code_gen_exp cond in
     let zero = if (String.contains(string_of_lltype(type_of condition)) '1') then  const_int (i1_type context) 0
                else const_int (bool_type) 0 in
     let cond_val = if (is_pointer cond) then build_load condition "loadcon" builder
                    else condition in
     let cond_val = build_icmp Icmp.Ne cond_val zero "ifcond" builder in


     let start_bb = insertion_block builder in
     let the_function = block_parent start_bb in

     (* to calculate the llvalue of current block *)
     let bef_bb = insertion_block builder in
     let _ = value_of_block bef_bb in
     
     let then_bb = append_block context "then" the_function in (* we create the then block *)
     position_at_end then_bb builder; (* we put builder at the end of blovk then_bb *)

     let _ = codegen_statement stm in  (* we create the code for the then statement *)
     let new_then_bb = insertion_block builder in (* we save the block in new_then_bb *)

     let merge_bb = append_block context "ifcont" the_function in (* we create the block after the if *)
     position_at_end merge_bb builder;

     (* let incoming = [(then_val, new_then_bb); (bef_bb_val,bef_bb)] in *)
     (* let phi = build_phi incoming "iftmp" builder in *)

     position_at_end start_bb builder; (* we return to the starting block *)
     ignore (build_cond_br cond_val then_bb merge_bb builder); (* we concat the two blocks *)

     position_at_end new_then_bb builder; ignore (build_br merge_bb builder); (* we set the then pointer to the endif block *)
     position_at_end merge_bb builder; (* we set the builder to the end of common block *)
     (* phi *)
     const_null null_type;
     

  | Ast.If_else_stmt(cond, stm1, stm2) ->
     let condition = code_gen_exp cond in
     
     let zero = if (String.contains(string_of_lltype(type_of condition)) '1') then  const_int (i1_type context) 0
                else const_int (bool_type) 0 in

     let cond_val = if (is_pointer cond) then build_load condition "loadcon" builder
                    else condition in

     let cond_val = build_icmp Icmp.Ne cond_val zero "ifelsecond" builder in

     let start_bb = insertion_block builder in (* start_bb contains the basic block *)
     let the_function = block_parent start_bb in
     let then_bb = append_block context "then" the_function in (* creates the then block *)
     position_at_end then_bb builder;

     let _ = codegen_statement stm1 in

     let new_then_bb = insertion_block builder in

     let else_bb = append_block context "else" the_function in
     position_at_end else_bb builder;
     
     let _ = codegen_statement stm2 in
     let new_else_bb = insertion_block builder in

     let merge_bb = append_block context "ifcont" the_function in
     position_at_end merge_bb builder;

     (* let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in *)
     (* let phi = build_phi incoming "iftmp" builder in *)
     position_at_end start_bb builder;
     ignore (build_cond_br cond_val then_bb else_bb builder);

     position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
     position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

     (* Finally, set the builder to the end of the merge block. *)
     position_at_end merge_bb builder;

     (* phi *)
     const_null null_type;
  (* e1 is i=0; e2 is i<10; e3=i++ and s the statements *)
  | Ast.For_loop(tag, e1, e2, e3, s) ->

     (* 1 we calculate the first exp *)
     let _ = match e1 with
       | None -> () (* NOT SURE IF IGNORE WORKS *)
       | Some exp -> ignore ( code_gen_exp exp) in

     (* now we make a new block in order to have only the condition *)
     let preheader_bb = insertion_block builder in
     let the_function = block_parent preheader_bb in

     let loop_cond_bb = append_block context "loopcondition" the_function in
     position_at_end preheader_bb builder;

     (* now we make the branch *)
     ignore (build_br loop_cond_bb builder);

     position_at_end loop_cond_bb builder;
     (* 2 now we have to check the condition *)
     let loop_cond =  match e2 with
       | None -> const_null null_type
       | Some exp -> let tmp = code_gen_exp exp in if (is_pointer exp) then build_load tmp "loadcon" builder else
                                                     tmp
                                                       
     in
     
     let zero = if (String.contains(string_of_lltype(type_of loop_cond)) '1') then  const_int (i1_type context) 0
                else const_int (bool_type) 0 in

     let cond_val = build_icmp Icmp.Ne loop_cond zero "loopcond" builder in

     (* let cond_val = loop_cond (\* build_fcmp Fcmp.One loop_cond zero "loopcond" builder *\) in *)

     (* 3 now we point to the block in which we are *)
     let preheader_bb = insertion_block builder in
     let the_function = block_parent preheader_bb in
     
     (* we create the loop block *)
     let loop_bb = append_block context "loopbody" the_function in
     position_at_end loop_bb builder;
     
     let step_bb= append_block context "loopstepblock" the_function in
     position_at_end step_bb builder;
     let _ =(match tag with
             | None -> let tag_bb = insertion_block builder in
                       continue_tags := ("$$$", tag_bb) :: !continue_tags
             | Some str -> let tag_bb = insertion_block builder in
                           continue_tags := (str, tag_bb) :: !continue_tags;
                           continue_tags := ("$$$",tag_bb) :: !continue_tags) in               

     let afterloop_bb = append_block context "afterloop" the_function in
     position_at_end afterloop_bb builder;
     (* we have to create the after loop block *)
     ignore ( match tag with
              | None -> let tag_bb = insertion_block builder in 
                        break_tags := ("$$$", tag_bb) :: !break_tags
              | Some str -> let tag_bb = insertion_block builder in
                            break_tags := (str, tag_bb) :: !break_tags;
                            break_tags := ("$$$",tag_bb) :: !break_tags
            );

     position_at_end loop_bb builder;
     ignore (codegen_statement s);
     ignore(build_br step_bb builder);

     position_at_end step_bb builder;
     (* now we calculate the step *)
     let _ = match e3 with
       | None -> const_null null_type 
       | Some exp -> code_gen_exp exp in

     
     position_at_end step_bb builder; (* we point to the afterloop block *)

     (* builder goes at the end of the loop block *)
     ignore(build_br loop_cond_bb builder);

     (* go again to check condition *)
     position_at_end loop_cond_bb builder;
     (* 4 now we again point at the end of the condition in order to check where to go *)
     position_at_end preheader_bb builder;
     ignore(build_cond_br cond_val loop_bb afterloop_bb builder);

     position_at_end afterloop_bb builder;
     continue_tags:= List.tl (!continue_tags);
     break_tags:= List.tl(!break_tags);
     const_null null_type
                
  | Ast.Branch(str1, str2)-> ( match str1 with
                               | "continue" -> (match str2 with
                                                | None -> ( try
                                                              let con_bb = find_block "$$$" (!continue_tags) in                
                                                              ignore ( build_br con_bb builder); const_null null_type
                                                            with Not_found->
                                                              (
                                                                Printf.printf "falseNotag"; const_null null_type))
                                                            
                                                | Some tag -> try
                                                              let con_bb = find_block tag (!continue_tags) in
                                                              ignore(build_br con_bb builder); const_null null_type
                                                            with Not_found ->
                                                              (Printf.printf "false"; const_null null_type)
                                               )
                               | "break" -> (match str2 with
                                               
                                             | None -> ( try
                                                           let br_bb = find_block "$$$" (!break_tags) in                
                                                           ignore ( build_br br_bb builder); const_null null_type
                                                         with Not_found->
                                                           (
                                                             Printf.printf "falseNotag"; const_null null_type))
                                             | Some tag -> try
                                                           let br_bb = find_block tag (!break_tags) in
                                                           ignore(build_br br_bb builder); const_null null_type
                                                         with Not_found ->
                                                           (Printf.printf "false"; const_null null_type)
                                                             
                                            )
                               | _ -> raise (Type_error "Something went wrong");
                             )
  (* in Return we have a variable name somehow and we save there the result *)
  | Ast.Return(ex) -> match ex with
                      | Some exp ->
                         let ret_val = code_gen_exp exp in
                         let ret_val =
                           if(ExpCodeGen.is_pointer exp) then build_load ret_val "ret" builder
                           else ret_val
                         in
                         build_ret ret_val builder 
                      | None -> returns := true;
                                build_ret_void builder

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




           

      
    
           



