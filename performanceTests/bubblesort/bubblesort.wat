(module
  (memory 1 100)
  (export "memory" (memory 0))

  (func (export "sort") (param $offset i32) (param $size i32)
    (local $i i32)
    (local $j i32)
    (local $limit i32)
    (local $temp i32)
    

    (set_local $limit (i32.shl (get_local $size) (i32.const 2)))

    (set_local $i (i32.const 0))
    (block $exit (loop $loop
      (br_if $exit (i32.ge_s (get_local $i) (get_local $limit)))

      (set_local $j (i32.add (get_local $i) (i32.const 4)))
      (block $exit2 (loop $loop2
        (br_if $exit2 (i32.ge_s (get_local $j) (get_local $limit)))


        (if (i32.gt_s (i32.load (get_local $i)) (i32.load (get_local $j))) (then
            (set_local $temp (i32.load (get_local $i)))
            (i32.store (get_local $i) (i32.load (get_local $j)))
            (i32.store (get_local $j) (get_local $temp))
        ))

        (set_local $j (i32.add (get_local $j) (i32.const 4)))
        (br $loop2)
        ))

        (set_local $i (i32.add (get_local $i) (i32.const 4)))
        (br $loop)
        )
      )
    )
  )