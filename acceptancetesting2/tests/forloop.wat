(module
	
	(export "f" (func $f))
	
	
	(func $f (param $a i32) (param $b i32) (param $start_i i32) (result i64)
		
		(local $sum i64)
		(local $i i32)
		
		(set_local $sum (i64.const 1))
		(block
		(set_local $i (i32.const 1))
		(loop
		(br_if 1
		(i32.ge_s (get_local $i) (i32.const 11)))
		(set_local $sum (i64.add (get_local $sum) (i64.mul (i64.mul (i64.extend_i32_s (get_local $a)) (i64.extend_i32_s (get_local $i))) (i64.extend_i32_s (get_local $b)))))
		(set_local $i (i32.add (get_local $i) (i32.const 1)))
		(br 0)))
		(return (get_local $sum))))