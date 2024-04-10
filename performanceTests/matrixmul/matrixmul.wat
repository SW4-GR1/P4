(module 
    (memory 1000)
    (export "memory" (memory 0))

    (func (export "matrixMul") (param $matA i32) (param $matB i32) (param $rowsA i32) (param $colsA i32) (param $rowsB i32) (param $colsB i32) (result i32)
        
        (local $i i32)
        (local $j i32)
        (local $k i32)
        (local $sum i32)
        (local $matC i32)
        (local $test i32)

        (set_local $sum (i32.const 4))

        (set_local $sum
            (i32.add 
                (get_local $sum)
                (i32.mul 
                    (i32.load (i32.add (get_local $matA) (i32.mul (i32.add (i32.mul (i32.const 0) (get_local $colsA)) (i32.const 0)) (i32.const 4)))) 
                    (i32.load (i32.add (get_local $matB) (i32.mul (i32.add (i32.mul (i32.const 0) (get_local $colsB)) (i32.const 0)) (i32.const 4))))
                ))
        ) 
        

        (set_local $i (i32.const 0))
        (set_local $matC (i32.add (get_local $matB) (i32.mul (i32.mul (get_local $rowsB) (get_local $colsB)) (i32.const 4)))) ;; matC = matB + rowsB * colsB * 4 (bytes) - lokationen i linært memory hvor resultatet er gemt
        
        ;; Check if colsA and rowsB are equal
        (if (i32.ne (get_local $colsA) (get_local $rowsB))
            (then (return (i32.const 0)))  ;; If not equal, return 0

        )

        (block $exit (loop $outerloop
            (br_if $exit (i32.ge_u (get_local $i) (get_local $rowsA))) ;; If i >= rowsA, exit loop
            (set_local $j (i32.const 0))
            (block $exit2 (loop $innerloop
                (br_if $exit2 (i32.ge_u (get_local $j) (get_local $colsB))) ;; If j >= colsB, exit inner loop
                (set_local $sum (i32.const 0))
                (set_local $k (i32.const 0))
                (block $exit3 (loop $innerinnerloop
                    (br_if $exit3 (i32.ge_u (get_local $k) (get_local $colsA))) ;; If k >= colsA, exit inner inner loop
                    (set_local $sum
                        (i32.add 
                            (get_local $sum)
                            (i32.mul 
                                (i32.load (i32.add (get_local $matA) (i32.mul (i32.add (i32.mul (get_local $i) (get_local $colsA)) (get_local $k)) (i32.const 4)))) 
                                (i32.load (i32.add (get_local $matB) (i32.mul (i32.add (i32.mul (get_local $k) (get_local $colsB)) (get_local $j)) (i32.const 4))))
                            )
                        )   
                    )
                (set_local $k (i32.add (get_local $k) (i32.const 1)))
                (br $innerinnerloop)
                ))
                (i32.store 
                    (i32.add 
                        (get_local $matC) 
                        (i32.mul 
                            (i32.add 
                                (i32.mul 
                                    (get_local $i) 
                                    (get_local $colsB)
                                ) 
                                (get_local $j)
                            ) 
                            (i32.const 4)
                        )
                    ) ;; matC + (i * colsB + j) * 4
                    (get_local $sum)
                ) 
                
                (set_local $j (i32.add (get_local $j) (i32.const 1)))
                 ;; End of inner loop
                (br $innerloop)
            ))
            (set_local $i (i32.add (get_local $i) (i32.const 1)))
            (br $outerloop)
        ))
        get_local $matC ;; Location in linear memory where the result is stored
    )
)
