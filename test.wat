(module
(func $main (export "main")
nop
(i32.eqz (i32.const 1))
(if
(i32.const 1)
(then
(i32.add (i32.const 2) (i32.const 2)))
(else
(i32.add (i32.const 3) (i32.const 4))))
))