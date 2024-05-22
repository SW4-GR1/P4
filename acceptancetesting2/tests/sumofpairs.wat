(module
	
	(export "sumofpairs" (func $sumofpairs))
	
	
	(func $sumofpairs (param $n i32) (result i32)
		
		(local $sum i32)
		(local $j i32)
		(local $i i32)
		
		(set_local $sum (i32.const 0))
		(block
		(set_local $i (i32.const 1))
		(loop
		(br_if 1
		(i32.gt_s (get_local $i) (get_local $n)))
		(block
		(set_local $j (get_local $i))
		(loop
		(br_if 1
		(i32.gt_s (get_local $j) (get_local $n)))
		(set_local $sum (i32.add (get_local $sum) (i32.add (get_local $i) (get_local $j))))
		(set_local $j (i32.add (get_local $j) (i32.const 1)))
		(get_local $j)
		(br 0)))
		(set_local $i (i32.add (get_local $i) (i32.const 1)))
		(get_local $i)
		(br 0)))
		(return (get_local $sum))))