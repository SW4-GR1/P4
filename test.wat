(module
(func $main (export "main")
nop
nop
(i32.add (i32.const 4) (get_local $x))
(set_local $x (i32.add (get_local $x) (i32.const 1)))
(get_local $x)
(i32.add (i32.const 2) (i32.const 4))
(i32.sub (i32.const 6) (i32.const 4))
(i32.add (i32.mul (i32.const 5) (i32.const 5)) (i32.const 4))
(i32.div (i32.const 8) (i32.const 2))
(i32.rem_s (i32.const 8) (i32.const 2))
(f32.add (f32.const 2.500000) (f32.const 4.500000))
(f32.sub (f32.const 6.500000) (f32.const 4.500000))
(f32.mul (f32.const 5.500000) (f32.const 5.500000))
(f32.div (f32.const 8.500000) (f32.const 2.500000))
(i32.gt_s (i32.const 4) (i32.const 2))
(f32.gt (f32.const 4.200000) (f32.const 3.100000))
))