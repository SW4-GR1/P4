(module
	
	
	
	(func $f  (result i32)
		
		(local $i i32)
		
		(block
		(set_local $i (i32.const 0))
		(loop
		(br_if 1
		(i32.ge_s (get_local $i) (i32.const 2)))
		(drop
		(i32.add (i32.const 2) (i32.const 2)))
		(set_local $i (i32.add (get_local $i) (i32.const 1)))
		(get_local $i)
		(br 0)))
		(return (i32.const 4))))