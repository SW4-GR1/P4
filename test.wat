(module
(func $main (export "main") (result i32)

(local $z f32)
(local $y i32)
(local $x i32)
(local $i i32)

(set_local $x (i32.const 4))

(set_local $z (f32.const 4.200000))
(drop
(i32.add (i32.const 2) (i32.const 7)))
(block
(set_local $i (i32.const 0))
(loop
(br_if 1
(i32.ge_s (i32.const 4) (i32.const 2)))
(drop
(i32.add (i32.const 1) (i32.const 1)))
(set_local $i (i32.add (get_local $i) (i32.const 1)))
(get_local $i)
(br 0)))
(i32.const 0)
))