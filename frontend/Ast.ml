type binop = Add | Mul | Sub | Div

type expr =
  | EConst of int
  | EIdent of string
  | EBinop of binop * expr * expr

type stmt =
  | Ssimple of expr
  | Slist of stmt list
  | IF

(* function declaration *)
(* type fun = {
  name : string
  args : string list
  body : stmt
  } *)


(* program of list of function declarations, followed by a statement *)
type prog = {
  (* funDecs : fun list *)
  main : stmt
}