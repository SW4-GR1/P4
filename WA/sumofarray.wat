
(module
    (memory 1)
    (export "memory" (memory 0))
    (export "sum" (func $0)) ;; Export the sum function

    (func $0 (param $ptr i32) (param $len i32) (result i32) ;; New sum function
        (local $i i32)
        (local $sum i32)
        (local.set $sum (i32.const 0))
        (block
        (loop
            (br_if 1 (i32.ge_u (local.get $i) (local.get $len)))
            (local.set $sum (i32.add (local.get $sum) (i32.load (i32.add (local.get $ptr) (i32.mul (local.get $i) (i32.const 4))))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br 0)
        )
        )
        (local.get $sum)
    )
)