(module
	
	(export "f" (func $f))
	
	
	(func $f (param $dividend i32) (param $divisor i32) (result i32)
		
		
		(block
		(loop
		(br_if 1
		(i32.lt_s (get_local $dividend) (get_local $divisor)))
		(set_local $dividend (i32.sub (get_local $dividend) (get_local $divisor)))
		(br 0)))
		(return (get_local $dividend))))