(module
  (import "env" "cos" (func $cos (param f32) (result f32)))
  (import "env" "sin" (func $sin (param f32) (result f32)))
  (import "env" "tan" (func $tan (param f32) (result f32)))

  (memory (export "memory") 1)

  (func (export "M3INV") (param $a i32) (param $ainv i32)
    (local $n i32)
    (local $k i32)
    (local $l i32)
    (local $tbase i32)
    (local $pivot f32)
    (local $tel f32)

    ;; T base offset
    (local.set $tbase (i32.const 1024))

    ;; Initialize T with A and zeros
    (local.set $k (i32.const 0))
    (block $k_done
      (loop $k_loop
        ;; T(k,0) = A(k,0)
        (f32.store
          (i32.add (local.get $tbase)
                   (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (i32.const 0)) (i32.const 4)))
          (f32.load
            (i32.add (local.get $a)
                     (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 3)) (i32.const 0)) (i32.const 4)))))

        ;; T(k,1) = A(k,1)
        (f32.store
          (i32.add (local.get $tbase)
                   (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (i32.const 1)) (i32.const 4)))
          (f32.load
            (i32.add (local.get $a)
                     (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 3)) (i32.const 1)) (i32.const 4)))))

        ;; T(k,2) = A(k,2)
        (f32.store
          (i32.add (local.get $tbase)
                   (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (i32.const 2)) (i32.const 4)))
          (f32.load
            (i32.add (local.get $a)
                     (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 3)) (i32.const 2)) (i32.const 4)))))

        ;; T(k,3..5) = 0
        (f32.store
          (i32.add (local.get $tbase)
                   (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (i32.const 3)) (i32.const 4)))
          (f32.const 0))
        (f32.store
          (i32.add (local.get $tbase)
                   (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (i32.const 4)) (i32.const 4)))
          (f32.const 0))
        (f32.store
          (i32.add (local.get $tbase)
                   (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (i32.const 5)) (i32.const 4)))
          (f32.const 0))

        ;; k++
        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br_if $k_loop (i32.lt_s (local.get $k) (i32.const 3)))
      )
    )

    ;; Set identity columns in T
    (f32.store (i32.add (local.get $tbase) (i32.const 12)) (f32.const 1))  ;; T(0,3)
    (f32.store (i32.add (local.get $tbase) (i32.const 40)) (f32.const 1))  ;; T(1,4)
    (f32.store (i32.add (local.get $tbase) (i32.const 68)) (f32.const 1))  ;; T(2,5)

    ;; Forward elimination
    (local.set $n (i32.const 0))
    (block $n_done
      (loop $n_loop
        ;; pivot = T(n,n)
        (local.set $pivot
          (f32.load
            (i32.add (local.get $tbase)
                     (i32.mul (i32.add (i32.mul (local.get $n) (i32.const 6)) (local.get $n)) (i32.const 4)))))

        (if (f32.eq (local.get $pivot) (f32.const 0))
          (then
            (local.set $l (i32.add (local.get $n) (i32.const 1)))
            (block $l_done0
              (loop $l_loop0
                (f32.store
                  (i32.add (local.get $tbase)
                           (i32.mul (i32.add (i32.mul (local.get $n) (i32.const 6)) (local.get $l)) (i32.const 4)))
                  (f32.const 0))
                (local.set $l (i32.add (local.get $l) (i32.const 1)))
                (br_if $l_loop0 (i32.lt_s (local.get $l) (i32.const 6)))
              )
            )
          )
          (else
            (local.set $l (i32.add (local.get $n) (i32.const 1)))
            (block $l_done1
              (loop $l_loop1
                (f32.store
                  (i32.add (local.get $tbase)
                           (i32.mul (i32.add (i32.mul (local.get $n) (i32.const 6)) (local.get $l)) (i32.const 4)))
                  (f32.div
                    (f32.load
                      (i32.add (local.get $tbase)
                               (i32.mul (i32.add (i32.mul (local.get $n) (i32.const 6)) (local.get $l)) (i32.const 4))))
                    (local.get $pivot)))
                (local.set $l (i32.add (local.get $l) (i32.const 1)))
                (br_if $l_loop1 (i32.lt_s (local.get $l) (i32.const 6)))
              )
            )
          )
        )

        ;; eliminate below
        (local.set $k (i32.add (local.get $n) (i32.const 1)))
        (block $k_done2
          (loop $k_loop2
            (local.set $tel
              (f32.load
                (i32.add (local.get $tbase)
                         (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (local.get $n)) (i32.const 4)))))

            (local.set $l (i32.add (local.get $n) (i32.const 1)))
            (block $l_done2
              (loop $l_loop2
                (f32.store
                  (i32.add (local.get $tbase)
                           (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (local.get $l)) (i32.const 4)))
                  (f32.sub
                    (f32.load
                      (i32.add (local.get $tbase)
                               (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (local.get $l)) (i32.const 4))))
                    (f32.mul (local.get $tel)
                             (f32.load
                               (i32.add (local.get $tbase)
                                        (i32.mul (i32.add (i32.mul (local.get $n) (i32.const 6)) (local.get $l)) (i32.const 4)))))))
                (local.set $l (i32.add (local.get $l) (i32.const 1)))
                (br_if $l_loop2 (i32.lt_s (local.get $l) (i32.const 6)))
              )
            )

            (local.set $k (i32.add (local.get $k) (i32.const 1)))
            (br_if $k_loop2 (i32.lt_s (local.get $k) (i32.const 3)))
          )
        )

        (local.set $n (i32.add (local.get $n) (i32.const 1)))
        (br_if $n_loop (i32.lt_s (local.get $n) (i32.const 3)))
      )
    )

    ;; Back-substitute
    (local.set $n (i32.const 2))
    (block $n_done3
      (loop $n_loop3
        (local.set $k (i32.sub (local.get $n) (i32.const 1)))
        (block $k_done3
          (loop $k_loop3
            (local.set $tel
              (f32.load
                (i32.add (local.get $tbase)
                         (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (local.get $n)) (i32.const 4)))))

            (local.set $l (i32.const 3))
            (block $l_done3
              (loop $l_loop3
                (f32.store
                  (i32.add (local.get $tbase)
                           (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (local.get $l)) (i32.const 4)))
                  (f32.sub
                    (f32.load
                      (i32.add (local.get $tbase)
                               (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (local.get $l)) (i32.const 4))))
                    (f32.mul (local.get $tel)
                             (f32.load
                               (i32.add (local.get $tbase)
                                        (i32.mul (i32.add (i32.mul (local.get $n) (i32.const 6)) (local.get $l)) (i32.const 4)))))))
                (local.set $l (i32.add (local.get $l) (i32.const 1)))
                (br_if $l_loop3 (i32.lt_s (local.get $l) (i32.const 6)))
              )
            )

            (local.set $k (i32.sub (local.get $k) (i32.const 1)))
            (br_if $k_loop3 (i32.ge_s (local.get $k) (i32.const 0)))
          )
        )

        (local.set $n (i32.sub (local.get $n) (i32.const 1)))
        (br_if $n_loop3 (i32.ge_s (local.get $n) (i32.const 1)))
      )
    )

    ;; Write AINV from T(:,3:5)
    (local.set $k (i32.const 0))
    (block $k_done4
      (loop $k_loop4
        (f32.store
          (i32.add (local.get $ainv)
                   (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 3)) (i32.const 0)) (i32.const 4)))
          (f32.load
            (i32.add (local.get $tbase)
                     (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (i32.const 3)) (i32.const 4)))))

        (f32.store
          (i32.add (local.get $ainv)
                   (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 3)) (i32.const 1)) (i32.const 4)))
          (f32.load
            (i32.add (local.get $tbase)
                     (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (i32.const 4)) (i32.const 4)))))

        (f32.store
          (i32.add (local.get $ainv)
                   (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 3)) (i32.const 2)) (i32.const 4)))
          (f32.load
            (i32.add (local.get $tbase)
                     (i32.mul (i32.add (i32.mul (local.get $k) (i32.const 6)) (i32.const 5)) (i32.const 4)))))

        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br_if $k_loop4 (i32.lt_s (local.get $k) (i32.const 3)))
      )
    )
  )

  (func (export "RATEKI3") (param $a i32) (param $r i32) (param $r_a i32)
    (local $c1 f32)
    (local $c2 f32)
    (local $s1 f32)
    (local $t2 f32)

    (local.set $c1 (call $cos (f32.load (local.get $a))))
    (local.set $c2 (call $cos (f32.load (i32.add (local.get $a) (i32.const 4)))))
    (local.set $s1 (call $sin (f32.load (local.get $a))))
    (local.set $t2 (call $tan (f32.load (i32.add (local.get $a) (i32.const 4)))))

    ;; R (row-major)
    (f32.store (i32.add (local.get $r) (i32.const 0)) (f32.const -1))
    (f32.store (i32.add (local.get $r) (i32.const 4)) (f32.mul (local.get $s1) (local.get $t2)))
    (f32.store (i32.add (local.get $r) (i32.const 8)) (f32.neg (f32.mul (local.get $c1) (local.get $t2))))

    (f32.store (i32.add (local.get $r) (i32.const 12)) (f32.const 0))
    (f32.store (i32.add (local.get $r) (i32.const 16)) (local.get $c1))
    (f32.store (i32.add (local.get $r) (i32.const 20)) (local.get $s1))

    (f32.store (i32.add (local.get $r) (i32.const 24)) (f32.const 0))
    (f32.store (i32.add (local.get $r) (i32.const 28)) (f32.div (local.get $s1) (local.get $c2)))
    (f32.store (i32.add (local.get $r) (i32.const 32)) (f32.neg (f32.div (local.get $c1) (local.get $c2))))

    ;; R_A (k = 0, row-major)
    (f32.store (i32.add (local.get $r_a) (i32.const 0)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 4)) (f32.mul (local.get $c1) (local.get $t2)))
    (f32.store (i32.add (local.get $r_a) (i32.const 8)) (f32.mul (local.get $s1) (local.get $t2)))

    (f32.store (i32.add (local.get $r_a) (i32.const 12)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 16)) (f32.neg (local.get $s1)))
    (f32.store (i32.add (local.get $r_a) (i32.const 20)) (local.get $c1))

    (f32.store (i32.add (local.get $r_a) (i32.const 24)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 28)) (f32.div (local.get $c1) (local.get $c2)))
    (f32.store (i32.add (local.get $r_a) (i32.const 32)) (f32.div (local.get $s1) (local.get $c2)))

    ;; R_A (k = 1, row-major)
    (f32.store (i32.add (local.get $r_a) (i32.const 36)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 40))
      (f32.div (local.get $s1) (f32.mul (local.get $c2) (local.get $c2))))
    (f32.store (i32.add (local.get $r_a) (i32.const 44))
      (f32.neg (f32.div (local.get $c1) (f32.mul (local.get $c2) (local.get $c2)))))

    (f32.store (i32.add (local.get $r_a) (i32.const 48)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 52)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 56)) (f32.const 0))

    (f32.store (i32.add (local.get $r_a) (i32.const 60)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 64))
      (f32.div (f32.mul (local.get $s1) (local.get $t2)) (local.get $c2)))
    (f32.store (i32.add (local.get $r_a) (i32.const 68))
      (f32.neg (f32.div (f32.mul (local.get $c1) (local.get $t2)) (local.get $c2))))

    ;; R_A (k = 2, row-major)
    (f32.store (i32.add (local.get $r_a) (i32.const 72)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 76)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 80)) (f32.const 0))

    (f32.store (i32.add (local.get $r_a) (i32.const 84)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 88)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 92)) (f32.const 0))

    (f32.store (i32.add (local.get $r_a) (i32.const 96)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 100)) (f32.const 0))
    (f32.store (i32.add (local.get $r_a) (i32.const 104)) (f32.const 0))
  )

  (func (export "ROTENS3") (param $a i32) (param $t i32) (param $t_a i32)
    (local $c1 f32)
    (local $c2 f32)
    (local $c3 f32)
    (local $s1 f32)
    (local $s2 f32)
    (local $s3 f32)

    (local.set $c1 (call $cos (f32.load (local.get $a))))
    (local.set $c2 (call $cos (f32.load (i32.add (local.get $a) (i32.const 4)))))
    (local.set $c3 (call $cos (f32.load (i32.add (local.get $a) (i32.const 8)))))

    (local.set $s1 (call $sin (f32.load (local.get $a))))
    (local.set $s2 (call $sin (f32.load (i32.add (local.get $a) (i32.const 4)))))
    (local.set $s3 (call $sin (f32.load (i32.add (local.get $a) (i32.const 8)))))

    ;; T (row-major)
    (f32.store (i32.add (local.get $t) (i32.const 0)) (f32.mul (local.get $c2) (local.get $c3)))
    (f32.store (i32.add (local.get $t) (i32.const 4))
      (f32.add (f32.neg (f32.mul (f32.mul (local.get $s1) (local.get $s2)) (local.get $c3)))
               (f32.mul (local.get $c1) (local.get $s3))))
    (f32.store (i32.add (local.get $t) (i32.const 8))
      (f32.add (f32.mul (f32.mul (local.get $c1) (local.get $s2)) (local.get $c3))
               (f32.mul (local.get $s1) (local.get $s3))))

    (f32.store (i32.add (local.get $t) (i32.const 12)) (f32.neg (f32.mul (local.get $c2) (local.get $s3))))
    (f32.store (i32.add (local.get $t) (i32.const 16))
      (f32.add (f32.mul (f32.mul (local.get $s1) (local.get $s2)) (local.get $s3))
               (f32.mul (local.get $c1) (local.get $c3))))
    (f32.store (i32.add (local.get $t) (i32.const 20))
      (f32.add (f32.neg (f32.mul (f32.mul (local.get $c1) (local.get $s2)) (local.get $s3)))
               (f32.mul (local.get $s1) (local.get $c3))))

    (f32.store (i32.add (local.get $t) (i32.const 24)) (f32.neg (local.get $s2)))
    (f32.store (i32.add (local.get $t) (i32.const 28)) (f32.neg (f32.mul (local.get $s1) (local.get $c2))))
    (f32.store (i32.add (local.get $t) (i32.const 32)) (f32.mul (local.get $c1) (local.get $c2)))

    ;; T_A (k = 0, row-major)
    (f32.store (i32.add (local.get $t_a) (i32.const 0)) (f32.const 0))
    (f32.store (i32.add (local.get $t_a) (i32.const 4))
      (f32.add (f32.neg (f32.mul (f32.mul (local.get $c1) (local.get $s2)) (local.get $c3)))
               (f32.neg (f32.mul (local.get $s1) (local.get $s3)))))
    (f32.store (i32.add (local.get $t_a) (i32.const 8))
      (f32.add (f32.neg (f32.mul (f32.mul (local.get $s1) (local.get $s2)) (local.get $c3)))
               (f32.mul (local.get $c1) (local.get $s3))))

    (f32.store (i32.add (local.get $t_a) (i32.const 12)) (f32.const 0))
    (f32.store (i32.add (local.get $t_a) (i32.const 16))
      (f32.add (f32.mul (f32.mul (local.get $c1) (local.get $s2)) (local.get $s3))
               (f32.neg (f32.mul (local.get $s1) (local.get $c3)))))
    (f32.store (i32.add (local.get $t_a) (i32.const 20))
      (f32.add (f32.mul (f32.mul (local.get $s1) (local.get $s2)) (local.get $s3))
               (f32.mul (local.get $c1) (local.get $c3))))

    (f32.store (i32.add (local.get $t_a) (i32.const 24)) (f32.const 0))
    (f32.store (i32.add (local.get $t_a) (i32.const 28)) (f32.neg (f32.mul (local.get $c1) (local.get $c2))))
    (f32.store (i32.add (local.get $t_a) (i32.const 32)) (f32.neg (f32.mul (local.get $s1) (local.get $c2))))

    ;; T_A (k = 1, row-major)
    (f32.store (i32.add (local.get $t_a) (i32.const 36)) (f32.neg (f32.mul (local.get $s2) (local.get $c3))))
    (f32.store (i32.add (local.get $t_a) (i32.const 40))
      (f32.neg (f32.mul (f32.mul (local.get $s1) (local.get $c2)) (local.get $c3))))
    (f32.store (i32.add (local.get $t_a) (i32.const 44))
      (f32.mul (f32.mul (local.get $c1) (local.get $c2)) (local.get $c3)))

    (f32.store (i32.add (local.get $t_a) (i32.const 48)) (f32.mul (local.get $s2) (local.get $s3)))
    (f32.store (i32.add (local.get $t_a) (i32.const 52))
      (f32.mul (f32.mul (local.get $s1) (local.get $c2)) (local.get $s3)))
    (f32.store (i32.add (local.get $t_a) (i32.const 56))
      (f32.neg (f32.mul (f32.mul (local.get $c1) (local.get $c2)) (local.get $s3))))

    (f32.store (i32.add (local.get $t_a) (i32.const 60)) (f32.neg (local.get $c2)))
    (f32.store (i32.add (local.get $t_a) (i32.const 64)) (f32.mul (local.get $s1) (local.get $s2)))
    (f32.store (i32.add (local.get $t_a) (i32.const 68)) (f32.neg (f32.mul (local.get $c1) (local.get $s2))))

    ;; T_A (k = 2, row-major)
    (f32.store (i32.add (local.get $t_a) (i32.const 72)) (f32.neg (f32.mul (local.get $c2) (local.get $s3))))
    (f32.store (i32.add (local.get $t_a) (i32.const 76))
      (f32.add (f32.mul (f32.mul (local.get $s1) (local.get $s2)) (local.get $s3))
               (f32.mul (local.get $c1) (local.get $c3))))
    (f32.store (i32.add (local.get $t_a) (i32.const 80))
      (f32.add (f32.neg (f32.mul (f32.mul (local.get $c1) (local.get $s2)) (local.get $s3)))
               (f32.mul (local.get $s1) (local.get $c3))))

    (f32.store (i32.add (local.get $t_a) (i32.const 84)) (f32.neg (f32.mul (local.get $c2) (local.get $c3))))
    (f32.store (i32.add (local.get $t_a) (i32.const 88))
      (f32.add (f32.mul (f32.mul (local.get $s1) (local.get $s2)) (local.get $c3))
               (f32.neg (f32.mul (local.get $c1) (local.get $s3)))))
    (f32.store (i32.add (local.get $t_a) (i32.const 92))
      (f32.add (f32.neg (f32.mul (f32.mul (local.get $c1) (local.get $s2)) (local.get $c3)))
               (f32.neg (f32.mul (local.get $s1) (local.get $s3)))))

    (f32.store (i32.add (local.get $t_a) (i32.const 96)) (f32.const 0))
    (f32.store (i32.add (local.get $t_a) (i32.const 100)) (f32.const 0))
    (f32.store (i32.add (local.get $t_a) (i32.const 104)) (f32.const 0))
  )
)
