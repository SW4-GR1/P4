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
	(func $my_fabs (param $diff f32) (result f32)
		
		
		(if
		(f32.lt (get_local $diff) (f32.const 0.000000))
		(then
		(return (f32.sub (f32.const 0.000000) (get_local $diff))))
		)
		(return (get_local $diff)))
	(func $squareroot (param $number f32) (result f32)
		
		(local $epsilon f32)
		(local $diff f32)
		(local $guess f32)
		
		(set_local $epsilon (f32.const 0.000010))
		(set_local $guess (f32.const 1.000000))
		(set_local $diff (f32.sub (get_local $number) (call $powerof 
		
		(get_local $guess)
		(i32.const 2))))
		(block
		(loop
		(br_if 1
		(f32.le (call $my_fabs 
		
		(f32.sub (get_local $number) (call $powerof 
		
		(get_local $guess)
		(i32.const 2)))) (get_local $epsilon)))
		(set_local $guess (f32.div (f32.add (get_local $guess) (f32.div (get_local $number) (get_local $guess))) (f32.const 2.000000)))
		(br 0)))
		(return (get_local $guess)))
	(func $f (param $a f32) (result f32)
		
		
		(return (f32.mul (f32.div (f32.mul (f32.const 5.000000) (f32.add (f32.const 3.000000) (call $squareroot 
		
		(f32.const 5.000000)))) (f32.const 12.000000)) (call $powerof 
		
		(get_local $a)
		(i32.const 3))))))