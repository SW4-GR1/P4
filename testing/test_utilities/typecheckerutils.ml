open Frontend
open Frontend.Ast 


let dummy_loc_str = "line 0, start column -1, end column -1"
let mk_vtab = SymTab.fromList [] (* empty variable table *)
let mk_ftab = SymTab.fromList [] (* empty function table *)

let mk_error ?loc msg = 
  let loc_str = (match loc with 
    | Some loc -> loc
    | None -> dummy_loc_str) in
  (Typechecker.Error (loc_str, msg))