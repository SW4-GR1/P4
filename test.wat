(module
	
	(export "f" (func $f))
	
	(global $x i32 (i32.const 4))
	
	(func $f (param $a i32) (result i32)
		
		
		(set_local $a (i32.add (get_local $a) (get_global $x)))
		(set_global $x (i32.add (get_global $x) (get_local $a)))
		(get_local $a)))