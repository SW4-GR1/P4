(module
(func $main (export "main")
nop
(i32.sub (i32.add (i32.add (i32.const 2) (i32.mul (i32.const 4) (i32.const 8))) (i32.div (i32.const 4) (i32.const 2))) (i32.const 8))
(i32.add (i32.sub (i32.const 6) (i32.const 4)) (i32.mul (i32.const 3) (i32.const 8)))
(i32.add (i32.div (i32.const 8) (i32.const 2)) (i32.const 2))
(i32.rem_s (i32.const 8) (i32.const 2))
)
)