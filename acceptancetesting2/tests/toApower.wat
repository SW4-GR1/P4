(module
	
	(export "f" (func $f))
	
	
	(func $f (param $base i32) (param $exponent i32) (result i32)
		
		(local $sum i32)
		(local $i i32)
		
		(set_local $sum (i32.const 1))
		(block
		(set_local $i (i32.const 0))
		(loop
		(br_if 1
		(i32.ge_s (get_local $i) (get_local $exponent)))
		(set_local $sum (i32.mul (get_local $sum) (get_local $base)))
		(set_local $i (i32.add (get_local $i) (i32.const 1)))
		(br 0)))
		(return (get_local $sum))))