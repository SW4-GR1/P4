(module
	
	
	
	(func $f  (result i32)
		
		
		(return (i32.const 1)))
	(func $f (param $a i32) (param $b i32) (result i32)
		
		
		(drop
		(i32.add (get_local $a) (get_local $b)))
		(return (i32.const 8))))