(module
	(export "funca" (func $funca))
	
	(func $funcb (param $x i32) (result i32)
		
		
		(i32.add (get_local $x) (i32.const 1)))
	(func $funca (param $b i32) (param $c f32) (result i32)
		
		(local $a i32)
		(local $i i32)
		
		(set_local $a (i32.const 4))
		(block
		(set_local $i (i32.const 0))
		(loop
		(br_if 1
		(i32.ge_s (get_local $i) (i32.const 7)))
		(set_local $a (i32.add (get_local $a) (get_local $b)))
		(set_local $i (i32.add (get_local $i) (i32.const 1)))
		(get_local $i)
		(br 0)))
		(call $funcb 
		
		(get_local $a))))