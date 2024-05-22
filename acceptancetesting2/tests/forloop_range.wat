(module
	
	(export "f" (func $f))
	
	
	(func $f (param $start i32) (param $end i32) (result i64)
		
		(local $sum i64)
		(local $i i32)
		
		(set_local $sum (i64.const 0))
		(block
		(set_local $i (get_local $start))
		(loop
		(br_if 1
		(i32.gt_s (get_local $i) (get_local $end)))
		(set_local $sum (i64.add (get_local $sum) (i64.extend_i32_s (get_local $i))))
		(set_local $i (i32.add (get_local $i) (i32.const 1)))
		(br 0)))
		(return (get_local $sum))))