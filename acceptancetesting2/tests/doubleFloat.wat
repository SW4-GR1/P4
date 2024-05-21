(module
	
	(export "f" (func $f))
	
	
	(func $f (param $a f32) (result f32)
		
		
		(return (f32.mul (f32.const 2.000000) (get_local $a)))))