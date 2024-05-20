(module
	
	(export "f" (func $f))
	
	(global $x (mut i32) (i32.const 50000))
	
	(func $f (param $a i32) (result i32)
		
		(local $xyz i32)
		
		(set_local $a (i32.add (get_local $a) (get_global $x)))
		(set_global $x (i32.add (get_global $x) (get_local $a)))
		(set_local $xyz (i32.div_s (i32.const 5) (i32.const 1)))
		(return (get_local $a)))
	(func $f  (result i32)
		
		
		(return (i32.const 2))))