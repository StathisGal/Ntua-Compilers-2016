open Symbol
open Types

check_scopes program =
  initSymbolTable 100 ;
  match program with
    None -> printf "Error"
   |Some program -> check_declarations program

check_declarations program = 
                      match program with 
                      [] -> ()
                      |x::xs -> check_declaration x; check_declarations xs

check_declaration dec = match dec with
                          Variable_dec (ty, var_list) ->
                          let rec addVars list = match var_list with
                              [] -> ()
                            | x::xs ->(
                               match x with
                                 Simple_declarator name -> 
                                 ignore (newVariable (id_make name) ty false);
                                 addVars xs
                               |Complex_declarator (name , exp) ->
                                  ignore (newVariable (id_make name) (ty,exp) false); 
                                  addVars xs
                            )
                           in addVars list 
                        |Function_dec (result, name , params) ->
                          newFunction
                        |Function_def (result, name, params, decs, stms) ->
                          let e =  newFunction (make_id name) true in
                          
                          
                                      

                              
   
  
