type opcode = string

(* Opcodes defineret som Ocaml string constanter til generation af wat kode*)
let add : opcode = "add"
let sub : opcode = "sub"
let mul : opcode = "mul"
let div : opcode = "div"

(* modulo på signed integers *)
let rem_s : opcode = "rem_s"
let eq : opcode = "eq"
let ne : opcode = "ne"
let lt : opcode = "lt"
let gt : opcode = "gt"
let le : opcode = "le"
let ge : opcode = "ge"

(* De forskellige typer i wasm*)
type wasm_type = I32 | I64 | F32 | F64

(* De forskellige konstruktioner i webassembly, skal muligvis udvides med loops og funktions definitioner   *)
type wasm =
  | Nop
  | S of string (* En operand *)
  | Cat of wasm * wasm
  | Opcode of opcode * wasm_type * wasm list (* opcode, type, operands *)

(* Funktion til at konvertere vores "wasm" til en string, så vi kan skrive det til en fil *)
  let rec to_string w =
    match w with
      | Nop -> "nop"
      | S s -> Printf.sprintf "%s" s
      | Cat (w1, w2) -> Printf.sprintf "%s\n%s" (to_string w1) (to_string w2)
      | Opcode (opcode, wasm_type, operands) -> ins opcode wasm_type operands

(* Funktion til at lave en instruktion (opcode) om til en streng *)
  and ins opcode wasm_type operands =
    let type_str = match wasm_type with
      | I32 -> "i32"
      | I64 -> "i64"
      | F32 -> "f32"
      | F64 -> "f64"
    in
    let operands_str = String.concat " " (List.map (fun op -> match op with
    | S s -> Printf.sprintf "(%s)" s
    | _ -> to_string op
    ) operands) in
    Printf.sprintf "(%s.%s %s)" type_str opcode operands_str

(* Funktioner til at generere de forskellige opcodes (kan bruges i compile.ml) *)
let add_i32 a b = Opcode (add, I32, [a; b])
let sub_i32 a b = Opcode (sub, I32, [a; b])
let mul_i32 a b = Opcode (mul, I32, [a; b])
let div_i32 a b = Opcode (div, I32, [a; b])
let rem_s_i32 a b = Opcode (rem_s, I32, [a; b])
(* implementer med ydeligere opcodes*)



(* Tilføjer en main_func som der generes inde i indtil vi har vores egne funktioner på plads*)
let main_func w = 
  Cat (S "(func $main (export \"main\")", Cat (w, S ")"))

(* Bare kode til selve modulet som alt andet skal nestes inde i*)
let module_ w = 
  Cat (S "(module", Cat (w, S ")"))


(* Funktion der skriver vores "wasm" til en fil, ved at kalde to_string med vores compiled program*)
let write_wat filename w =
  let oc = open_out filename in
  let wat_string = to_string w in
  output_string oc wat_string;
  close_out oc;
  print_endline ("Successfully compiled to wat in " ^ filename);
  print_endline ("Generated WAT:\n" ^ wat_string)

(* ... and similar functions for generating WAT programs *)