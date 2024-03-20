type 'a symTab = SymTab of (string * 'a) list

let empty () = SymTab []

let rec lookup n tab = 
  match tab with
  | SymTab [] -> None
  | SymTab ((n1, i1)::rest) ->
      if n = n1
      then Some i1
      else lookup n (SymTab rest) 

let bind n i (SymTab stab) = 
  SymTab ((n,i) :: stab)

let remove n (SymTab stab) =
    SymTab (List.filter (fun (x,_) -> x != n) stab)

let remove_many ns (SymTab stab) = 
  SymTab (List.filter (fun(x, _) -> 
    ! (List.exists (fun y -> y == x) ns)) stab)

let combine (SymTab t1) (SymTab t2) = 
  Symtab (t1 @ t2)

let fromList l = Symtab l

let toList (Symtab lst) = lst