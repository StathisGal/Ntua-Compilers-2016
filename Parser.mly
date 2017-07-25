%{
open Ast
open Types
open Str

let rec antistoixise_types_fun t =
    let r  = Str.regexp "\\(char\\|int\\|bool\\|double\\|void\\)\\(\\**\\)" in
    	let tipos = Str.replace_first r "\\1" t 
	and point = Str.replace_first r "\\2" t in
	    check_pointer tipos point;

and check_pointer ty poin = match String.length (poin) with
| 0 -> (check_type ty);
| 1 -> TYPE_pointer (check_type ty)
| _ -> TYPE_pointer (check_pointer ty (String.sub poin 1 ((String.length poin)-1)))

and check_type ty = match ty with
| "void" -> TYPE_proc
| "int" -> TYPE_int
| "double" -> TYPE_double
| "bool" -> TYPE_bool
| "char" -> TYPE_char
| _ -> TYPE_none

%}

%token T_QUES
%token EOL
%token T_EOF
%token T_PLUS T_MINUS T_TIMES T_DIV T_MOD
%token <string>T_ID
%token <int > T_INT
%token <float> T_DOUBLE
%token <char> T_CHAR
%token <string> T_STRING
%token T_IF
%token T_BOOLTYPE        
%token T_BREAK      
%token T_CHARTYPE        
%token T_CONTINUE        
%token T_DELETE        
%token T_DOUBLETYPE
%token T_FALSE        
%token T_INTTYPE
%token T_NEW
%token T_NULL
%token T_RETURN
%token T_TRUE
%token T_ELSE
%token T_FOR
%token T_VOID
%token T_BYREF
%token T_SEMIC
%token T_PLUS T_MINUS        /* lowest precedence */
%token T_TIMES T_DIV T_MOD
%token T_PP  T_MM 
%token T_AMP T_EX
%token T_AND T_OR
%token T_ASS T_PEQ  T_MIEQ T_TEQ  T_DEQ T_MEQ
%token T_COMMA 
%token T_EQ T_DIF
%token T_BIG T_SMALL T_BEQ T_SEQ  T_QUES T_COLON
%token T_LP T_RP
%token T_RSB T_LSB
%token T_RB T_LB
        
/*--------------------------ORDER---------------------------*/
     /* Edw itan to EXPLIST */

      /*DANGLING IF */
        %nonassoc IF_STMNT
        %nonassoc T_ELSE 
      /* ------------*/
      %nonassoc DELETE
      %left T_COMMA 
      %nonassoc EXPLIST /*Na to ksanadoume ayto*/
      %nonassoc TIMES /*for type_t shift/reduce conflict */
      %right T_PEQ T_MIEQ T_TEQ T_DEQ T_MEQ /*ASSIGNMENT vs EQUALITY T_ASS used to be here*/

            %right ASSIGNMENT
      %right T_ASS
   %nonassoc T_QUES // nonassoc
      %right COMPIF
      %left T_OR
      %left T_AND
      %left LOGICAL_OPERATOR
      %nonassoc T_EQ T_DIF T_BIG T_SMALL T_SEQ T_BEQ

      %nonassoc COMPARE_OPERATOR
      %left T_PLUS T_MINUS
      %left ARITHMETIC_OPERATOR /* all binary_operators for shift/reduce*/
   
      %left T_TIMES T_DIV T_MOD
      %nonassoc CASTING /*bonus*/
      %nonassoc T_PP T_MM
      %nonassoc UNOP
      %nonassoc POSTFIX
      %nonassoc T_LSB T_RSB
     /*see http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual026.html how conflicts are resolved */
     %nonassoc PREFIX
     %start start             /* the entry point */
     %type <unit> start 
     %%
     start: program T_EOF {let t = !program_tree in
     match t with
     |None -> program_tree:= Some $1
     |Some p -> program_tree:= Some (p @ $1)}
                                                                                                                                                                                                                                                                                                        program:
      program declaration {$1 @ [$2]} 
      | declaration {[$1]}
      ;

        declaration:
          variable_declaration  {$1}
          |function_declaration  {$1}
          |function_definition {$1}
      ;                                                  

        variable_declaration: 
          type_t declarators T_SEMIC {Variable_dec ($1,$2)}   
      ;
 
        /* type_t: 
             basic_type {(Type $1)}
             |type_t T_TIMES %prec TIMES{Type ($1^$2)}
      ;
        */
          type_t:
            basic_type star %prec TIMES{ antistoixise_types_fun ( $1^$2) }
      ;
        star:
          {""}
          |star T_TIMES{($1^"*")}
      ;
                       
        basic_type: 
          T_INTTYPE  {"int"}
          | T_CHARTYPE  {"char"}
          | T_BOOLTYPE  {"bool"} 
          | T_DOUBLETYPE {"double"}
      ;

        declarator: 
          T_ID {Simple_declarator $1}
          | T_ID T_LSB constant_expression T_RSB {Complex_declarator ($1,Constant_exp $3)}
      ;
        
        declarators:
          declarator T_COMMA declarators {$1::$3}
          |declarator {[$1]}
      ;
        
        function_declaration:
          type_t T_ID T_LP T_RP T_SEMIC {Function_dec ($1,$2,[])} 
          |type_t T_ID T_LP parameter_list T_RP T_SEMIC {Function_dec($1,$2,$4)}
          |T_VOID T_ID T_LP T_RP T_SEMIC {Function_dec(TYPE_proc,$2,[])} 
          |T_VOID T_ID T_LP parameter_list T_RP T_SEMIC {Function_dec(TYPE_proc,$2,$4)}
                
      ;

        /*NEVER USED replace result_type with type_t | Void */
          /*result_type: 
         type_t {()}
         |T_VOID {()}
        ;*/

          parameter_list:
            parameter parameters {$1::$2}
        ;

           parameters :
             {[]}
             |T_COMMA parameter parameters {$2::$3}
         ;

           parameter : 
             T_BYREF type_t T_ID {By_ref_param ($2,$3)}
             |type_t T_ID {By_val_param ($1,$2)}
                     ;

                       function_definition: 
                         type_t T_ID T_LP T_RP T_LB declarations statements T_RB {Function_def($1,$2,[],$6,$7)}
                         |  type_t T_ID T_LP parameter_list T_RP T_LB declarations statements T_RB {Function_def($1,$2,$4,$7,$8)}

                         |T_VOID T_ID T_LP T_RP T_LB declarations statements T_RB {Function_def(TYPE_proc,$2,[],$6,$7)}
                         |T_VOID T_ID T_LP parameter_list T_RP T_LB declarations statements T_RB {Function_def(TYPE_proc,$2,$4,$7,$8)}
                     ;
                       
        declarations:
             {[]}
          |declaration declarations {$1::$2}
                     ;


       statements:
             {[]}
                   |statements statement {$1@[$2]}
                     ;
        statement: 
          T_SEMIC{Simple_expression None}
          | expression T_SEMIC {Simple_expression (Some $1)}
          | T_LB statements T_RB {Statements $2}
          | T_IF T_LP expression T_RP statement %prec IF_STMNT{If_stmt($3,$5)} 
          | T_IF T_LP expression T_RP statement T_ELSE statement {If_else_stmt($3,$5,$7)} 
          | optional_t_id_t_colon T_FOR T_LP optional_expression T_SEMIC optional_expression T_SEMIC optional_expression T_RP statement {For_loop($1,$4,$6,$8,$10)}
          | T_CONTINUE  T_SEMIC  {Branch ("continue",None)}
          | T_CONTINUE T_ID T_SEMIC  {Branch ("continue",Some $2)} 
          | T_BREAK  T_SEMIC  {Branch ("break",None)}
          | T_BREAK T_ID T_SEMIC {Branch ("break",Some $2)}
          | T_RETURN  T_SEMIC {Return None}
          | T_RETURN expression T_SEMIC {Return (Some $2)}
                     ;
        optional_t_id_t_colon:
             {None}
          |T_ID T_COLON {Some $1}
                     ;

        optional_expression:
             {None}
          |expression {Some $1}
                     ;
        expression:
            T_ID {Id $1}
          | T_LP expression T_RP  {Paren_expression $2}
          | T_TRUE {Bool true}| T_FALSE {Bool false} | T_NULL  {String "NULL"}
          | T_INT  {Int $1}| T_CHAR {Char $1} | T_DOUBLE  {Double $1}| T_STRING  {String $1}
          | T_ID T_LP T_RP  {Function_call ($1, [])}
          | T_ID T_LP expression_list T_RP {Function_call ($1,$3)}
          | expression T_LSB expression T_RSB %prec POSTFIX {Array ($1,$3)}
          | unary_operator expression %prec UNOP  {Unary_op ($1,$2)}
          //| expression binary_operator expression %OPERATOR {Binary_op ($1,$2,$3)} //added a few more
	  | expression ar_binary_operator expression %prec ARITHMETIC_OPERATOR {Binary_op ($1,$2,$3)}
	  | expression cmp_binary_operator expression %prec COMPARE_OPERATOR {Binary_op ($1,$2,$3)}
	  | expression log_binary_operator expression %prec LOGICAL_OPERATOR {Binary_op ($1,$2,$3)}
          | expression T_COMMA expression %prec EXPLIST {Binary_op ($1,",",$3)} /* used to be OPERATOR */
          | unary_assignment expression %prec PREFIX {Prefix_unary_as ($1,$2)}
          | expression unary_assignment %prec POSTFIX  {Postfix_unary_as($1,$2)}
          | expression binary_assignment expression %prec ASSIGNMENT{Binary_as ($1,$2,$3)}
          | T_LP type_t T_RP expression %prec CASTING {Casting ($2,$4)}
          | expression T_QUES expression T_COLON expression %prec COMPIF  { Question ($1,$3,$5)}
          | T_NEW type_t %prec POSTFIX {New_op ($2,None)} /*Changed it recently */
          | T_NEW basic_type star T_TIMES expression {Binary_op (New_op(antistoixise_types_fun ($2^$3),None),"*",$5)} /* Probably wrong */
          | T_NEW type_t T_LSB expression T_RSB %prec POSTFIX {New_op ($2,Some $4)}
          | T_DELETE expression %prec DELETE{Delete_op $2}
                                                                                                                                                                      ;

                                                                                                                                                                        expression_list:
	                                                                                                                                                                  expression  %prec EXPLIST {[$1]}
                                                                                                                                                                          |expression_list T_COMMA expression %prec EXPLIST  {$1 @ [$3]} /* to allaxa twra */
	;


constant_expression:
	expression {Constant_exp $1}
	;

unary_operator:
	T_AMP  {"&"}| T_TIMES {"*"} | T_PLUS  {"+"}| T_MINUS  {"-"}| T_EX {"!"}
	;

/*
binary_operator:
	T_TIMES  {"*"}| T_DIV  {"/"}| T_MOD  {"%"}| T_PLUS  {"+"}| T_MINUS  {"-"}| T_BIG  {">"}| T_SMALL  {"<"}| T_BEQ  {">="}| T_SEQ  {"<="}| T_EQ  {"=="}| T_DIF  {"!="}| T_AND  {"&&"}| T_OR  {"||"}*/
	/*| T_COMMA {","} removed because of shift/conflict*/
	

ar_binary_operator:
	T_TIMES  {"*"}| T_DIV  {"/"}| T_MOD  {"%"}| T_PLUS  {"+"}| T_MINUS  {"-"}
	;

cmp_binary_operator:
	T_BIG  {">"}| T_SMALL  {"<"}| T_BEQ  {">="}| T_SEQ  {"<="}| T_EQ  {"=="}| T_DIF  {"!="}
;
log_binary_operator:
	T_AND  {"&&"}| T_OR  {"||"}
;
unary_assignment:
	T_PP  {"++"}| T_MM {"--"}
	;

binary_assignment:
	T_ASS  {"="}| T_TEQ  {"*="}| T_DEQ  {"/="}| T_MEQ  {"%="}| T_PEQ  {"+="}| T_MIEQ {"-="};
