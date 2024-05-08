(module
	
	
	(func $f (param $a i32) (param $b i32) (result i32)
		
		(local $c i32)
		
		
		(drop
		(i32.add (get_local $a) (get_local $b)))
		(drop
		(i32.add (i32.const 4) (i32.const 4)))
		(get_local $b))
	(func $h (param $b i32) (param $a i32) (result i32)
		
		(local $k i32)
		(local $x i32)
		(local $c i32)
		
		
		(set_local $x (i32.const 0))
		(drop
		(i32.add (get_local $a) (get_local $b)))
		(drop
		(i32.add (i32.const 4) (i32.const 5)))
		(block
		(set_local $k (i32.const 0))
		(loop
		(br_if 1
		(i32.ge_s (get_local $k) (i32.const 10)))
		(set_local $a (i32.add (get_local $a) (i32.const 4)))
		(set_local $k (i32.add (get_local $k) (i32.const 1)))
		(get_local $k)
		(br 0)))
		(block
		(loop
		(br_if 1
		(i32.ge_s (i32.add (get_local $x) (i32.const 1)) (i32.const 5)))
		(set_local $x (i32.add (get_local $x) (i32.const 1)))
		(get_local $x)
		(br 0)))
		(get_local $a)))