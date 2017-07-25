open Types
open Symbol
open Ast
open Identifier


exception Type_error of string 

let rec findType exp =
  match exp with
    Id name -> let e = lookupEntry (id_make name) LOOKUP_ALL_SCOPES true in
               (match e.entry_info with
                  ENTRY_variable variable_info ->  variable_info.variable_type
                | ENTRY_parameter parameter_info -> parameter_info.parameter_type
                | _ -> let _ = Error.error "%s:this is not a variable" name in (raise (Type_error ""))
               )

  | Int int -> TYPE_int
  | Double double -> TYPE_double
  | Char char -> TYPE_char
  | String "NULL" -> TYPE_pointer TYPE_none
  | String str -> TYPE_array (TYPE_char,0) (*Array (TYPE_pointer *)
  | Bool bool -> TYPE_bool (* ... *)
  | Constant_exp exp -> findType exp
  | Function_call (name, exps) ->
     let param_types = List.map findType exps in
     let actual_param_types = List.map convert_to_typical_types param_types in (*CRUCIAL ARRAY MUST BE TURNED INTO POINTERS WHEN PASSED AS PARAMETERS*)
     let fun_name = name in (* stopping supporting functions with same names and different parameters :( *)
     (* let _ = Printf.printf "Length of params %s:%d\n" name (List.length exps) in *)
     (* let suffix = create_suffix actual_param_types in *)
     (* let fun_name = String.concat "" [name;"_" ;suffix] in *)
     (* let _ = Printf.printf "looking for %s\n" fun_name in *)
     let e = lookupEntry (id_make fun_name) LOOKUP_ALL_SCOPES true in 
     (match e.entry_info with
        ENTRY_function function_info -> (
        let typical_param_types = List.map (function x -> match x.entry_info with ENTRY_parameter a -> a.parameter_type | _ -> Error.error "No reason to be here"; raise (Type_error "")) function_info.function_paramlist in
        (try 
           let result = List.map2 equalType actual_param_types typical_param_types in
           let correct = List.fold_left (&&)  true  result in
           if correct then function_info.function_result else (Error.error "fun %s: Mismatch between actual and typical params" name; raise (Type_error ""))
         with Invalid_argument _ ->
           Error.error "call of %s function has different length of params" name;
           raise (Type_error "")
        )
      )
      | _ -> raise (Type_error "This function has no result")
     )
  | Array (e1, e2) -> let t1 = findType e1 in
                      let t2 = findType e2 in
                      if (equalType t2 TYPE_int) then
                        (match t1 with 
                         | TYPE_array (ty,_) -> ty
                         | TYPE_pointer x -> x
                         | _ -> (Error.error "This is neither a pointer nor an array"; raise (Type_error ("")))
                        )
                      else 
                        (Error.error "Left expression in array form does not evaluate to integer type";
                         raise (Type_error "")
                        )


  | Unary_op (op,e) -> (match op with
                          "&" -> TYPE_pointer (findType e)
                        | "*" -> let ty = (findType e) in
                                 (match ty with 
                                    TYPE_pointer x -> x
                                  | _ -> raise (Type_error "Error finding pointers"))
                        | "-"
                          | "+" -> findType e
                        | "!" -> let ty = findType e in
                                 (match ty with
                                    TYPE_bool -> TYPE_bool
                                  | _ -> raise (Type_error "Operator ! should be used with boolean types"))
                        | _ -> raise (Type_error "Wrong operator")
                       )

  |Binary_op (e1, op, e2) -> let t1 = findType e1 in
                             let t2 = findType e2 in
                             (match op with
                              | "+"  
                                | "-" ->  (match t1, t2 with 
                                           |  TYPE_int, TYPE_int 
                                              | TYPE_double, TYPE_double 
                                              | TYPE_pointer _, TYPE_int -> t1
                                           | TYPE_int, TYPE_pointer _ -> t2
                                             | TYPE_array (t,_), TYPE_int -> TYPE_pointer t (*treating arrays as pointers*)
                                           | _,_ -> Error.error "%s %s %s Invalid arguments for binary operator" (convert_type_to_char t1) (convert_type_to_char t2) op ; raise (Type_error (""))
                                          )
                                            
                              | "*" 
                                | "/" ->  (match t1, t2 with 
                                             TYPE_int, TYPE_int 
                                           | TYPE_double, TYPE_double  -> t1
                                           | _,_ -> Error.error "%s: Invalid arguments for binary operator" op ; raise (Type_error "")
                                          )
                              | "%" -> (match t1, t2 with
                                          TYPE_int, TYPE_int -> t1
                                        | _, _ -> Error.error "%s: Invalid arguments for binary operator" op ; raise (Type_error "")
                                       )
                              | "=="
                                | "!=" ->  if (equalType t1 t2 ) then TYPE_bool else (Error.error "%s: Invalid arguments in comparison" op; raise (Type_error "" ))
                              |"<"
                               |">"
                               | "<="
                               | ">=" -> (match t1,t2 with
                                          |  TYPE_int, TYPE_int 
                                             | TYPE_double, TYPE_double
                                             | TYPE_pointer _ , TYPE_pointer _ 
                                             | TYPE_array _, TYPE_array _ (*treating arrays as pointers*)
                                             | TYPE_bool, TYPE_bool  -> TYPE_bool
                                          | _, _ -> (Error.error "%s Invalid arguments in comparison" op; raise (Type_error ""))
                                         )
                              |"&&"
                               | "||" -> (match t1, t2 with
                                            TYPE_bool, TYPE_bool -> TYPE_bool
                                           |_,_ -> (Error.error "%s: Invalid arguments in logical comparison" op; raise (Type_error ""))
                                         )
                              | "," -> t2
                              | _ -> (Error.error "%s: Unkown binary operator" op; raise (Type_error ""))
                             )


  |Prefix_unary_as (op, e) ->
    let ty = findType e in 
    (match ty with 
       TYPE_int 
      |TYPE_double 
       |TYPE_pointer _ -> ty
      | _ -> (Error.error "%s Unary assignement with wrong type" op; raise (Type_error ""))
    )
  |Postfix_unary_as (e, op) ->
    let ty = findType e in 
    (match ty with 
       TYPE_int 
      |TYPE_double 
       |TYPE_pointer _ -> ty
      | _ -> (Error.error "%s Unary assignement with wrong type" op; raise (Type_error ""))
    )
  |Binary_as (e1, op, e2) ->
    let t1 = findType e1 in 
    (match t1 with
     |TYPE_array (_,_) -> (Error.error "%s: Cannot assign value to array" op; raise (Type_error ""))
     | _ ->
        (let t2 = findType e2 in 
         if (e2 = ( String "NULL")) then t1 else
          (match op with 
          |  "=" -> if ( equalType t1 t2 ) then t1 else
                      (Error.error "%s Binary assignement wrong operands type1: %s type2: %s" op (convert_type_to_char t1)(convert_type_to_char t2);
                       let _ = Printf.printf "Error Here\n" in
                       let c1 = convert_type_to_char t1 in
                       let c2 = convert_type_to_char t2 in
                       let _ = Printf.printf "%s vs %s \n" c1 c2 in
                       let _ = match e1 with
                           Id(name) -> Printf.printf "->%s<-" name
                          |_ -> ()
                       in
                       let _ = match e2 with
                           Id(name) -> Printf.printf "->%s<-" name
                          |_ -> ()
                       in
                       raise (Type_error ""))
          | "*=" 
            | "/=" -> 
             (match t1,t2 with 
                TYPE_int , TYPE_int
              | TYPE_double, TYPE_double -> t1
              | _, _ -> (Error.error "%s: Binary assignment wrong types" op; raise (Type_error ""))
             )
          | "%=" ->
             (match t1,t2 with 
                TYPE_int, TYPE_int -> t1
              | _, _ -> (Error.error "%s Binary assignment wrong types" op; raise (Type_error ""))
             )
          | "+="
            | "-=" -> 
             (match t1, t2 with
                TYPE_int, TYPE_int
              | TYPE_double, TYPE_double 
                |TYPE_pointer _ , TYPE_int -> t1
              | _, _ -> (Error.error "%s Binary assignment wrong types" op; raise (Type_error ""))
             )
          |_ -> (Error.error "%s: Unkown binary operator" op; raise (Type_error ""))
         )

        )
    )


  |Casting (t1, e1) -> let _ = findType e1 in t1
  |Question (e1,e2,e3) -> let t1 = findType e1 in
                          let t2 = findType e2 in
                          let t3 = findType e3 in
                          (if (equalType t1 TYPE_bool) then 
                             (if (equalType t2 t3) then t2 else (Error.error "t1?t2:t3-> t2, t3 mismatch"; raise (Type_error "")))
                           else (Error.error "t1?t2:t3-> t1 is not boolean type"; raise (Type_error ""))
                          )
  |New_op (t, None) -> TYPE_pointer t (*Changed in Codegen *)
  |New_op (t, Some e) -> 
    if (equalType (findType e) TYPE_int) then TYPE_pointer t else (Error.error "Wrong types in new operator"; raise (Type_error ""))
  |Delete_op e ->
    let t = findType e in
    (match t with
       TYPE_pointer _ -> t
     | _ -> (Error.error "Wrong arguments in delete"; raise (Type_error ""))
    )
  |Paren_expression e -> findType e

and convert_to_typical_types t = 
  match t with 
  | TYPE_array (t,_) -> TYPE_pointer t
  | _ -> t 

and convert_type_to_char t =
  match t with
  | TYPE_int -> "i"
  | TYPE_double -> "d"
  | TYPE_array (t,_) -> String.concat "" ["a"; (convert_type_to_char t)]
  | TYPE_char -> "c"
  | TYPE_bool -> "b"
  | TYPE_pointer x -> String.concat "" ["p" ;(convert_type_to_char x)]
  | _ -> ""

and create_suffix type_list = 
  let suffix = List.map (fun x -> convert_type_to_char x) type_list in 
  String.concat "" suffix 


                


                
                
                
                
                
