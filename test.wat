(module
	
	
	(func $f (param $a i32) (param $b i32) (result i32)
		
		(local $c i32)
		
		
		(drop
		(i32.add (get_local $a) (get_local $b)))
		(drop
		(i32.add (i32.const 4) (i32.const 4)))
		(get_local $b))
	(func $h (param $b i32) (param $a i32) (result i32)
		
		(local $c i32)
		
		
		(drop
		(i32.add (get_local $a) (get_local $b)))
		(drop
		(i32.add (i32.const 4) (i32.const 5)))
		(get_local $a)))