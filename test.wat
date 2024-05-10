(module
	
	(export "fibonacci" (func $fibonacci))
	
	
	(func $fibonacci (param $n i32) (result i64)
		
		(local $a i64)
		(local $b i64)
		(local $temp i64)
		(local $i i32)
		
		(set_local $a (i64.const 0))
		(set_local $b (i64.const 1))
		
		(block
		(set_local $i (i32.const 0))
		(loop
		(br_if 1
		(i32.ge_s (get_local $i) (get_local $n)))
		(set_local $temp (get_local $a))
		(set_local $a (get_local $b))
		(set_local $b (i64.add (get_local $temp) (get_local $b)))
		(set_local $i (i32.add (get_local $i) (i32.const 1)))
		(get_local $i)
		(br 0)))
		(return (get_local $b))))