(module
(func $main (export "main") (result i32)

(local $k f32)
(local $f i32)
(local $z f32)
(local $y i32)
(local $x i32)

(set_local $x (i32.const 4))

(set_local $z (f32.const 4.200000))
(drop
(i32.add (i32.const 2) (i32.const 7)))
(if
(i32.const 1)
(then
(set_local $f (i32.const 2)))
(else
))
(i32.const 0)
))