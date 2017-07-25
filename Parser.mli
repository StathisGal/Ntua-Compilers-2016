type token =
  | T_QUES
  | EOL
  | T_EOF
  | T_PLUS
  | T_MINUS
  | T_TIMES
  | T_DIV
  | T_MOD
  | T_ID of (string)
  | T_INT of (int )
  | T_DOUBLE of (float)
  | T_CHAR of (char)
  | T_STRING of (string)
  | T_IF
  | T_BOOLTYPE
  | T_BREAK
  | T_CHARTYPE
  | T_CONTINUE
  | T_DELETE
  | T_DOUBLETYPE
  | T_FALSE
  | T_INTTYPE
  | T_NEW
  | T_NULL
  | T_RETURN
  | T_TRUE
  | T_ELSE
  | T_FOR
  | T_VOID
  | T_BYREF
  | T_SEMIC
  | T_PP
  | T_MM
  | T_AMP
  | T_EX
  | T_AND
  | T_OR
  | T_ASS
  | T_PEQ
  | T_MIEQ
  | T_TEQ
  | T_DEQ
  | T_MEQ
  | T_COMMA
  | T_EQ
  | T_DIF
  | T_BIG
  | T_SMALL
  | T_BEQ
  | T_SEQ
  | T_COLON
  | T_LP
  | T_RP
  | T_RSB
  | T_LSB
  | T_RB
  | T_LB

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
