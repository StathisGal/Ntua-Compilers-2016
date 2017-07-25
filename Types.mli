type typ =
    TYPE_none
  | TYPE_int
  | TYPE_double
  | TYPE_byte
  | TYPE_char
  | TYPE_bool
  | TYPE_pointer of typ
  | TYPE_array of typ * int
  | TYPE_proc
val sizeOfType : typ -> int
val equalType : typ -> typ -> bool
