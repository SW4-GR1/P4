open SymTab
open Ttree
open Ptree

(* exception Type_error of string *)

type ty =
  |  Tint
type loc = Lexing.position * Lexing.position

type FunTable = (ty * ty list * loc) symTab
type VarTable = ty symTab

let type_to_string = function 
  | Tint -> "int"

let initFunTable : FunTable =
  SymTab.fromList [
    (* ("int" (Int_ty, [Int_ty, Int_ty], (0,0))); Example *)
  ]

let pp_funtype (args: ty list * res: ty) : string = 
  match args with
  | [] -> "() -> " ^ type_to_string
  | args -> (String.concat ", " (List.map type_to_string args))
            ^ " -> " type_to_string res


let program p = "Hello, World"