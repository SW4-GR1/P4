(module
	
	(export "f" (func $f))
	
	(global $x (mut i32) (i32.const 2))
	
	(func $f (param $a i32) (param $b i32) (result i32)
		
		
		(return (i32.mul (get_local $a) (get_local $b)))))