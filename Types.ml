type typ = TYPE_none
         | TYPE_int
         | TYPE_double
         | TYPE_byte
         | TYPE_char
         | TYPE_bool
         | TYPE_pointer of typ
         | TYPE_array of
             typ *
             int
         | TYPE_proc

let rec sizeOfType t =
   match t with
   | TYPE_int            -> 2
   | TYPE_char           -> 1
   | TYPE_pointer typ    -> 2
   | TYPE_byte           -> 1
   | TYPE_array (et, sz) -> sz * sizeOfType et
   | _                   -> 0

                              (* let rec equalType t1 t2 = *)
                              (*   | TYPE_pointer et1 , TYPE_array(et2,_) -> equalType et1 et2 *)
                              (*   | TYPE_array (et2) *)
                              (*     | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> equalType et1 et2 *)
                              (*   | _                                            -> t1 = t2 *)

let rec equalType t1 t2 =
  let t1 = match t1 with
    |TYPE_array (t1,_) -> TYPE_pointer t1
    | _ -> t1 in
  let t2 = match t2 with
    |TYPE_array (t2,_) -> TYPE_pointer t2
    | _ -> t2 in
  match t1,t2 with
  | TYPE_pointer TYPE_none, TYPE_pointer _ -> true
  | TYPE_pointer _ ,TYPE_pointer TYPE_none  -> true
  | _,_ -> t1 = t2
        
