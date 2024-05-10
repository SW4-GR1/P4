(module
	
	(export "f" (func $f))
	
	(global $x (mut i32) (i32.const 4))
	(global $y (mut f32) (f32.const 7.400000))
	
	(func $f (param $a i32) (result i32)
		
		
		(if
		(f32.gt (get_global $y) (f32.const 7.200000))
		(then
		(set_global $x (i32.const 3)))
		(else
		(set_global $x (i32.const 5))))
		(return (get_global $x))))