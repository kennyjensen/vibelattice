;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (memory (export "memory") 1)

  (func $addr (param $base i32) (param $lda i32) (param $i i32) (param $j i32) (result i32)
    (i32.add
      (local.get $base)
      (i32.mul
        (i32.const 4)
        (i32.add (local.get $i) (i32.mul (local.get $j) (local.get $lda))))))

  (func $isamax (param $n i32) (param $sx i32) (param $incx i32) (result i32)
    (local $i i32)
    (local $ix i32)
    (local $smax f32)
    (local $imax i32)
    (if (i32.or (i32.lt_s (local.get $n) (i32.const 1))
                (i32.le_s (local.get $incx) (i32.const 0)))
      (then (return (i32.const 0))))
    (if (i32.eq (local.get $n) (i32.const 1))
      (then (return (i32.const 1))))
    (local.set $imax (i32.const 1))
    (local.set $smax (f32.abs (f32.load (local.get $sx))))
    (if (i32.eq (local.get $incx) (i32.const 1))
      (then
        (local.set $i (i32.const 1))
        (block $done
          (loop $loop
            (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
            (local.set $ix (i32.add (local.get $sx) (i32.mul (local.get $i) (i32.const 4))))
            (if (f32.gt (f32.abs (f32.load (local.get $ix))) (local.get $smax))
              (then
                (local.set $smax (f32.abs (f32.load (local.get $ix))))
                (local.set $imax (i32.add (local.get $i) (i32.const 1)))
              )
            )
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $loop)
          )
        )
        (return (local.get $imax))
      )
    )

    (local.set $ix (local.get $sx))
    (local.set $i (i32.const 0))
    (block $done2
      (loop $loop2
        (br_if $done2 (i32.ge_s (local.get $i) (local.get $n)))
        (if (f32.gt (f32.abs (f32.load (local.get $ix))) (local.get $smax))
          (then
            (local.set $smax (f32.abs (f32.load (local.get $ix))))
            (local.set $imax (i32.add (local.get $i) (i32.const 1)))
          )
        )
        (local.set $ix (i32.add (local.get $ix) (i32.mul (local.get $incx) (i32.const 4))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop2)
      )
    )
    (return (local.get $imax))
  )

  (func $sscal (param $n i32) (param $sa f32) (param $sx i32) (param $incx i32)
    (local $i i32)
    (local $ix i32)
    (if (i32.or (i32.le_s (local.get $n) (i32.const 0))
                (i32.le_s (local.get $incx) (i32.const 0)))
      (then (return)))
    (local.set $ix (local.get $sx))
    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (f32.store (local.get $ix) (f32.mul (local.get $sa) (f32.load (local.get $ix))))
        (local.set $ix (i32.add (local.get $ix) (i32.mul (local.get $incx) (i32.const 4))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func $saxpy (param $n i32) (param $sa f32) (param $sx i32) (param $incx i32) (param $sy i32) (param $incy i32)
    (local $i i32)
    (local $ix i32)
    (local $iy i32)
    (if (i32.le_s (local.get $n) (i32.const 0)) (then (return)))
    (if (f32.eq (local.get $sa) (f32.const 0)) (then (return)))
    (local.set $ix (local.get $sx))
    (local.set $iy (local.get $sy))
    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (f32.store (local.get $iy)
          (f32.add (f32.load (local.get $iy))
                   (f32.mul (local.get $sa) (f32.load (local.get $ix)))))
        (local.set $ix (i32.add (local.get $ix) (i32.mul (local.get $incx) (i32.const 4))))
        (local.set $iy (i32.add (local.get $iy) (i32.mul (local.get $incy) (i32.const 4))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func $sdot (param $n i32) (param $sx i32) (param $incx i32) (param $sy i32) (param $incy i32) (result f32)
    (local $i i32)
    (local $ix i32)
    (local $iy i32)
    (local $stemp f32)
    (local.set $stemp (f32.const 0))
    (if (i32.le_s (local.get $n) (i32.const 0)) (then (return (local.get $stemp))))
    (local.set $ix (local.get $sx))
    (local.set $iy (local.get $sy))
    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (local.set $stemp
          (f32.add (local.get $stemp)
                   (f32.mul (f32.load (local.get $ix)) (f32.load (local.get $iy)))))
        (local.set $ix (i32.add (local.get $ix) (i32.mul (local.get $incx) (i32.const 4))))
        (local.set $iy (i32.add (local.get $iy) (i32.mul (local.get $incy) (i32.const 4))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
    (return (local.get $stemp))
  )

  (func $sgefa (param $a i32) (param $lda i32) (param $n i32) (param $ipvt i32)
    (local $nm1 i32) (local $k i32) (local $kp1 i32) (local $l i32) (local $l0 i32) (local $j i32)
    (local $addr i32) (local $addr2 i32)
    (local $t f32)

    (local.set $nm1 (i32.sub (local.get $n) (i32.const 1)))
    (if (i32.lt_s (local.get $nm1) (i32.const 1))
      (then
        (i32.store (i32.add (local.get $ipvt) (i32.mul (i32.sub (local.get $n) (i32.const 1)) (i32.const 4))) (local.get $n))
        (return)
      )
    )

    (local.set $k (i32.const 0))
    (block $k_done
      (loop $k_loop
        (br_if $k_done (i32.ge_s (local.get $k) (local.get $nm1)))
        (local.set $kp1 (i32.add (local.get $k) (i32.const 1)))

        (local.set $addr (call $addr (local.get $a) (local.get $lda) (local.get $k) (local.get $k)))
        (local.set $l (i32.add (call $isamax (i32.sub (local.get $n) (local.get $k)) (local.get $addr) (i32.const 1)) (local.get $k)))
        (i32.store (i32.add (local.get $ipvt) (i32.mul (local.get $k) (i32.const 4))) (local.get $l))
        (local.set $l0 (i32.sub (local.get $l) (i32.const 1)))

        (local.set $addr (call $addr (local.get $a) (local.get $lda) (local.get $l0) (local.get $k)))
        (if (f32.ne (f32.load (local.get $addr)) (f32.const 0))
          (then
            (if (i32.ne (local.get $l0) (local.get $k))
              (then
                (local.set $addr2 (call $addr (local.get $a) (local.get $lda) (local.get $k) (local.get $k)))
                (local.set $t (f32.load (local.get $addr)))
                (f32.store (local.get $addr) (f32.load (local.get $addr2)))
                (f32.store (local.get $addr2) (local.get $t))
              )
            )

            (local.set $addr2 (call $addr (local.get $a) (local.get $lda) (local.get $k) (local.get $k)))
            (local.set $t (f32.div (f32.const -1) (f32.load (local.get $addr2))))
            (call $sscal (i32.sub (local.get $n) (local.get $kp1)) (local.get $t)
              (call $addr (local.get $a) (local.get $lda) (local.get $kp1) (local.get $k)) (i32.const 1))

            (local.set $j (local.get $kp1))
            (block $j_done
              (loop $j_loop
                (br_if $j_done (i32.ge_s (local.get $j) (local.get $n)))
                (local.set $addr (call $addr (local.get $a) (local.get $lda) (local.get $l0) (local.get $j)))
                (local.set $t (f32.load (local.get $addr)))
                (if (i32.ne (local.get $l0) (local.get $k))
                  (then
                    (local.set $addr2 (call $addr (local.get $a) (local.get $lda) (local.get $k) (local.get $j)))
                    (f32.store (local.get $addr) (f32.load (local.get $addr2)))
                    (f32.store (local.get $addr2) (local.get $t))
                  )
                )

                (call $saxpy (i32.sub (local.get $n) (local.get $kp1)) (local.get $t)
                  (call $addr (local.get $a) (local.get $lda) (local.get $kp1) (local.get $k)) (i32.const 1)
                  (call $addr (local.get $a) (local.get $lda) (local.get $kp1) (local.get $j)) (i32.const 1))

                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
            )
          )
        )

        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $k_loop)
      )
    )

    (i32.store (i32.add (local.get $ipvt) (i32.mul (i32.sub (local.get $n) (i32.const 1)) (i32.const 4))) (local.get $n))
  )

  (func $sgesl (param $a i32) (param $lda i32) (param $n i32) (param $ipvt i32) (param $b i32) (param $job i32)
    (local $nm1 i32) (local $k i32) (local $kb i32) (local $l i32) (local $addr i32) (local $addr2 i32)
    (local $t f32)

    (local.set $nm1 (i32.sub (local.get $n) (i32.const 1)))
    (if (i32.ne (local.get $job) (i32.const 0))
      (then (return))
    )

    (if (i32.ge_s (local.get $nm1) (i32.const 1))
      (then
        (local.set $k (i32.const 0))
        (block $k_done
          (loop $k_loop
            (br_if $k_done (i32.ge_s (local.get $k) (local.get $nm1)))
            (local.set $addr (i32.add (local.get $ipvt) (i32.mul (local.get $k) (i32.const 4))))
            (local.set $l (i32.sub (i32.load (local.get $addr)) (i32.const 1)))
            (local.set $addr2 (i32.add (local.get $b) (i32.mul (local.get $l) (i32.const 4))))
            (local.set $t (f32.load (local.get $addr2)))
            (if (i32.ne (local.get $l) (local.get $k))
              (then
                (local.set $addr (i32.add (local.get $b) (i32.mul (local.get $k) (i32.const 4))))
                (f32.store (local.get $addr2) (f32.load (local.get $addr)))
                (f32.store (local.get $addr) (local.get $t))
              )
            )
            (call $saxpy (i32.sub (local.get $n) (i32.add (local.get $k) (i32.const 1))) (local.get $t)
              (call $addr (local.get $a) (local.get $lda) (i32.add (local.get $k) (i32.const 1)) (local.get $k)) (i32.const 1)
              (i32.add (local.get $b) (i32.mul (i32.add (local.get $k) (i32.const 1)) (i32.const 4))) (i32.const 1))

            (local.set $k (i32.add (local.get $k) (i32.const 1)))
            (br $k_loop)
          )
        )
      )
    )

    (local.set $kb (i32.const 1))
    (block $kb_done
      (loop $kb_loop
        (br_if $kb_done (i32.gt_s (local.get $kb) (local.get $n)))
        (local.set $k (i32.sub (local.get $n) (local.get $kb)))
        (local.set $addr (call $addr (local.get $a) (local.get $lda) (local.get $k) (local.get $k)))
        (local.set $addr2 (i32.add (local.get $b) (i32.mul (local.get $k) (i32.const 4))))
        (f32.store (local.get $addr2) (f32.div (f32.load (local.get $addr2)) (f32.load (local.get $addr))))
        (local.set $t (f32.neg (f32.load (local.get $addr2))))
        (call $saxpy (local.get $k) (local.get $t)
          (call $addr (local.get $a) (local.get $lda) (i32.const 0) (local.get $k)) (i32.const 1)
          (local.get $b) (i32.const 1))
        (local.set $kb (i32.add (local.get $kb) (i32.const 1)))
        (br $kb_loop)
      )
    )
  )

  (func (export "LUDCMP") (param $nsiz i32) (param $n i32) (param $a i32) (param $indx i32) (param $work i32)
    (call $sgefa (local.get $a) (local.get $nsiz) (local.get $n) (local.get $indx))
  )

  (func (export "BAKSUB") (param $nsiz i32) (param $n i32) (param $a i32) (param $indx i32) (param $b i32)
    (call $sgesl (local.get $a) (local.get $nsiz) (local.get $n) (local.get $indx) (local.get $b) (i32.const 0))
  )
)
