(module
  (func $fibonacci (param $n i32) (result i64)
    (local $a i64)
    (local $b i64)
    (local $temp i64)
    (local $i i32)
    (set_local $a (i64.const 0))
    (set_local $b (i64.const 1))
    (set_local $i (i32.const 0))
    (block $block (loop $loop
      (br_if $block (i32.ge_u (get_local $i) (get_local $n)))
      (set_local $temp (get_local $a))
      (set_local $a (get_local $b))
      (set_local $b (i64.add (get_local $temp) (get_local $b)))
      (set_local $i (i32.add (get_local $i) (i32.const 1)))
      (br $loop)
    ))
    (get_local $a)
  )
  (export "fibonacci" (func $fibonacci))
)
