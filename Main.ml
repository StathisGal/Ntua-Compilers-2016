open Ast
open Lexer
open Codegen
open Error
open Llvm
  
let _ =
  try
    let i = Array.length Sys.argv in 
    let arg_list = Array.to_list Sys.argv in
    let arg_list = List.tl arg_list in (* remove executable *)
    let has_opt opt el = opt = el in
    let (stdin_final,arg_list) = List.partition (has_opt "-f") arg_list in
    let stdin_final = match stdin_final with [] -> false | _ -> true in
    let (stdin_ir,arg_list) = List.partition (has_opt "-i") arg_list in
    let stdin_ir = match stdin_ir with [] -> false | _ -> true in
    let (ast, arg_list) = List.partition (has_opt "--ast") arg_list in
    let ast = match ast with [] -> false | _ -> true in
    let (_, arg_list) = List.partition (has_opt "-O") arg_list in (*We don't care about optimizations*)
    let program_name = if(stdin_final || stdin_ir) then "stdin" else (List.hd arg_list) in
    (*let _ = Printf.printf "Reading from %s\n" program_name in*)
    let channel = if(stdin_final || stdin_ir) then stdin else open_in(List.hd arg_list) in
    let lexbuf = Lexing.from_channel (channel) in
    let _ = SS.add (program_name) in
    let _ = Parser.start Lexer.edsger lexbuf in
    let _ = if (ast) then print_teliko !program_tree else () in
    let _ = try Scopes.check_program  !program_tree  with error -> Printf.printf "Semantic Error. Compilation aborted.\n"; exit(1) in
    let _ = codegen_program !program_tree  in
    let program_name = if(program_name = "stdin") then "stdin" else String.sub program_name 0  (String.length (program_name) - 4) in
    print_module (String.concat "" [program_name;".ll"]) Codegen.the_module;
    let _ = if(stdin_ir)then dump_module Codegen.the_module else () in exit(0)
  with Parsing.Parse_error ->
Printf.printf "Parsing error. Compilation aborted.\n"
