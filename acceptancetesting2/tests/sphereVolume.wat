(module
	
	(export "f" (func $f))
	
	
	(func $powerof (param $base f32) (param $exponent i32) (result f32)
		
		(local $sum f32)
		(local $i i32)
		
		(set_local $sum (f32.const 1.000000))
		(block
		(set_local $i (i32.const 0))
		(loop
		(br_if 1
		(i32.ge_s (get_local $i) (get_local $exponent)))
		(set_local $sum (f32.mul (get_local $sum) (get_local $base)))
		(set_local $i (i32.add (get_local $i) (i32.const 1)))
		(br 0)))
		(return (get_local $sum)))
	(func $f (param $radius f32) (result f32)
		
		(local $pi f32)
		
		(set_local $pi (f32.const 3.141590))
		(return (f32.mul (f32.mul (f32.div (f32.const 4.000000) (f32.const 3.000000)) (get_local $pi)) (call $powerof 
		
		(get_local $radius)
		(i32.const 3))))))