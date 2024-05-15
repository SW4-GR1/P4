(module
	
	
	
	(func $f (param $x i32) (result i32)
		
		(local $i i32)
		
		(block
		(set_local $i (i32.const 5))
		(loop
		(br_if 1
		(i32.ge_s (get_local $i) (get_local $x)))
		(set_local $x (i32.add (get_local $x) (i32.const 1)))
		(set_local $i (i32.add (get_local $i) (i32.const 1)))
		(br 0)))
		(block
		(loop
		(br_if 1
		(i32.le_s (get_local $x) (i32.const 0)))
		(set_local $x (i32.sub (get_local $x) (i32.const 1)))
		(br 0)))
		(return (get_local $x))))