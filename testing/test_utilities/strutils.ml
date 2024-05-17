open Str

let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s