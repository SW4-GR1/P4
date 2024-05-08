(module
	
	(local $x i32)
	
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
		(get_local $a))
	(set_local $x (i32.const 1))
	(block $loop_start
	(i32.lt_s (i32.add (get_local $x) (i32.const 1)) (i32.const 5))
	(if (result i32)
	(then
	(then
	(set_local $x (i32.add (get_local $x) (i32.const 1)))
	(get_local $x))
	(br $loop_start)
	)
	(else
	)
	)))