;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (memory (export "memory") 1)

  (func (export "XERBLA") (param $srname i32) (param $info i32)
    nop
  )

  (func (export "SSCAL") (param $n i32) (param $sa f32) (param $sx i32) (param $incx i32)
    (local $i i32) (local $ix i32) (local $idx i32)
    (local.set $i (i32.const 0))
    (local.set $ix (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (local.set $idx (i32.add (local.get $sx) (i32.mul (local.get $ix) (i32.const 4))))
        (f32.store (local.get $idx) (f32.mul (local.get $sa) (f32.load (local.get $idx))))
        (local.set $ix (i32.add (local.get $ix) (local.get $incx)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func (export "DSCAL") (param $n i32) (param $da f64) (param $dx i32) (param $incx i32)
    (local $i i32) (local $ix i32) (local $idx i32)
    (local.set $i (i32.const 0))
    (local.set $ix (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (local.set $idx (i32.add (local.get $dx) (i32.mul (local.get $ix) (i32.const 8))))
        (f64.store (local.get $idx) (f64.mul (local.get $da) (f64.load (local.get $idx))))
        (local.set $ix (i32.add (local.get $ix) (local.get $incx)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func $idx (param $i i32) (param $j i32) (param $lda i32) (result i32)
    (i32.add (local.get $i) (i32.mul (local.get $j) (local.get $lda)))
  )

  (func $SLASWP (export "SLASWP") (param $n i32) (param $a i32) (param $lda i32)
                            (param $k1 i32) (param $k2 i32) (param $ipiv i32) (param $incx i32)
    (local $i i32) (local $j i32) (local $ix i32) (local $ip i32)
    (local $i1 i32) (local $i2 i32) (local $tmp f32)
    (local.set $ix (select (i32.sub (local.get $k1) (i32.const 1))
                           (i32.sub (local.get $k2) (i32.const 1))
                           (i32.gt_s (local.get $incx) (i32.const 0))))
    (local.set $i (local.get $k1))
    (block $done
      (loop $loop
        (br_if $done (i32.gt_s (local.get $i) (local.get $k2)))
        (local.set $ip (i32.load (i32.add (local.get $ipiv) (i32.mul (local.get $ix) (i32.const 4)))))
        (local.set $ip (i32.sub (local.get $ip) (i32.const 1)))
        (if (i32.ne (local.get $ip) (i32.sub (local.get $i) (i32.const 1)))
          (then
            (local.set $j (i32.const 0))
            (block $j_done
              (loop $j_loop
                (br_if $j_done (i32.ge_s (local.get $j) (local.get $n)))
                (local.set $i1 (call $idx (i32.sub (local.get $i) (i32.const 1)) (local.get $j) (local.get $lda)))
                (local.set $i2 (call $idx (local.get $ip) (local.get $j) (local.get $lda)))
                (local.set $tmp (f32.load (i32.add (local.get $a) (i32.mul (local.get $i1) (i32.const 4)))))
                (f32.store (i32.add (local.get $a) (i32.mul (local.get $i1) (i32.const 4)))
                           (f32.load (i32.add (local.get $a) (i32.mul (local.get $i2) (i32.const 4)))))
                (f32.store (i32.add (local.get $a) (i32.mul (local.get $i2) (i32.const 4))) (local.get $tmp))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
            )
          )
        )
        (local.set $ix (i32.add (local.get $ix) (local.get $incx)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func $DLASWP (export "DLASWP") (param $n i32) (param $a i32) (param $lda i32)
                            (param $k1 i32) (param $k2 i32) (param $ipiv i32) (param $incx i32)
    (local $i i32) (local $j i32) (local $ix i32) (local $ip i32)
    (local $i1 i32) (local $i2 i32) (local $tmp f64)
    (local.set $ix (select (i32.sub (local.get $k1) (i32.const 1))
                           (i32.sub (local.get $k2) (i32.const 1))
                           (i32.gt_s (local.get $incx) (i32.const 0))))
    (local.set $i (local.get $k1))
    (block $done
      (loop $loop
        (br_if $done (i32.gt_s (local.get $i) (local.get $k2)))
        (local.set $ip (i32.load (i32.add (local.get $ipiv) (i32.mul (local.get $ix) (i32.const 4)))))
        (local.set $ip (i32.sub (local.get $ip) (i32.const 1)))
        (if (i32.ne (local.get $ip) (i32.sub (local.get $i) (i32.const 1)))
          (then
            (local.set $j (i32.const 0))
            (block $j_done
              (loop $j_loop
                (br_if $j_done (i32.ge_s (local.get $j) (local.get $n)))
                (local.set $i1 (call $idx (i32.sub (local.get $i) (i32.const 1)) (local.get $j) (local.get $lda)))
                (local.set $i2 (call $idx (local.get $ip) (local.get $j) (local.get $lda)))
                (local.set $tmp (f64.load (i32.add (local.get $a) (i32.mul (local.get $i1) (i32.const 8)))))
                (f64.store (i32.add (local.get $a) (i32.mul (local.get $i1) (i32.const 8)))
                           (f64.load (i32.add (local.get $a) (i32.mul (local.get $i2) (i32.const 8)))))
                (f64.store (i32.add (local.get $a) (i32.mul (local.get $i2) (i32.const 8))) (local.get $tmp))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
            )
          )
        )
        (local.set $ix (i32.add (local.get $ix) (local.get $incx)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func (export "SGEMM") (param $transa i32) (param $transb i32)
                           (param $m i32) (param $n i32) (param $k i32)
                           (param $alpha f32) (param $a i32) (param $lda i32)
                           (param $b i32) (param $ldb i32)
                           (param $beta f32) (param $c i32) (param $ldc i32)
    (local $i i32) (local $j i32) (local $l i32)
    (local $sum f32) (local $aval f32) (local $bval f32)
    (local $aN i32) (local $bN i32)
    (local.set $aN (i32.eq (local.get $transa) (i32.const 78)))
    (local.set $bN (i32.eq (local.get $transb) (i32.const 78)))

    (local.set $j (i32.const 0))
    (block $j_done
      (loop $j_loop
        (br_if $j_done (i32.ge_s (local.get $j) (local.get $n)))
        (local.set $i (i32.const 0))
        (block $i_done
          (loop $i_loop
            (br_if $i_done (i32.ge_s (local.get $i) (local.get $m)))
            (local.set $sum (f32.const 0))
            (local.set $l (i32.const 0))
            (block $l_done
              (loop $l_loop
                (br_if $l_done (i32.ge_s (local.get $l) (local.get $k)))
                (local.set $aval
                  (f32.load
                    (i32.add (local.get $a)
                             (i32.mul (call $idx (select (local.get $i) (local.get $l) (local.get $aN))
                                               (select (local.get $l) (local.get $i) (local.get $aN))
                                               (local.get $lda))
                                       (i32.const 4)))))
                (local.set $bval
                  (f32.load
                    (i32.add (local.get $b)
                             (i32.mul (call $idx (select (local.get $l) (local.get $j) (local.get $bN))
                                               (select (local.get $j) (local.get $l) (local.get $bN))
                                               (local.get $ldb))
                                       (i32.const 4)))))
                (local.set $sum (f32.add (local.get $sum) (f32.mul (local.get $aval) (local.get $bval))))
                (local.set $l (i32.add (local.get $l) (i32.const 1)))
                (br $l_loop)
              )
            )
            (local.set $aval
              (f32.load (i32.add (local.get $c)
                                 (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldc)) (i32.const 4)))))
            (f32.store (i32.add (local.get $c)
                                (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldc)) (i32.const 4)))
                       (f32.add (f32.mul (local.get $alpha) (local.get $sum))
                                (f32.mul (local.get $beta) (local.get $aval))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i_loop)
          )
        )
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $j_loop)
      )
    )
  )

  (func (export "DGEMM") (param $transa i32) (param $transb i32)
                           (param $m i32) (param $n i32) (param $k i32)
                           (param $alpha f64) (param $a i32) (param $lda i32)
                           (param $b i32) (param $ldb i32)
                           (param $beta f64) (param $c i32) (param $ldc i32)
    (local $i i32) (local $j i32) (local $l i32)
    (local $sum f64) (local $aval f64) (local $bval f64)
    (local $aN i32) (local $bN i32)
    (local.set $aN (i32.eq (local.get $transa) (i32.const 78)))
    (local.set $bN (i32.eq (local.get $transb) (i32.const 78)))

    (local.set $j (i32.const 0))
    (block $j_done
      (loop $j_loop
        (br_if $j_done (i32.ge_s (local.get $j) (local.get $n)))
        (local.set $i (i32.const 0))
        (block $i_done
          (loop $i_loop
            (br_if $i_done (i32.ge_s (local.get $i) (local.get $m)))
            (local.set $sum (f64.const 0))
            (local.set $l (i32.const 0))
            (block $l_done
              (loop $l_loop
                (br_if $l_done (i32.ge_s (local.get $l) (local.get $k)))
                (local.set $aval
                  (f64.load
                    (i32.add (local.get $a)
                             (i32.mul (call $idx (select (local.get $i) (local.get $l) (local.get $aN))
                                               (select (local.get $l) (local.get $i) (local.get $aN))
                                               (local.get $lda))
                                       (i32.const 8)))))
                (local.set $bval
                  (f64.load
                    (i32.add (local.get $b)
                             (i32.mul (call $idx (select (local.get $l) (local.get $j) (local.get $bN))
                                               (select (local.get $j) (local.get $l) (local.get $bN))
                                               (local.get $ldb))
                                       (i32.const 8)))))
                (local.set $sum (f64.add (local.get $sum) (f64.mul (local.get $aval) (local.get $bval))))
                (local.set $l (i32.add (local.get $l) (i32.const 1)))
                (br $l_loop)
              )
            )
            (local.set $aval
              (f64.load (i32.add (local.get $c)
                                 (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldc)) (i32.const 8)))))
            (f64.store (i32.add (local.get $c)
                                (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldc)) (i32.const 8)))
                       (f64.add (f64.mul (local.get $alpha) (local.get $sum))
                                (f64.mul (local.get $beta) (local.get $aval))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i_loop)
          )
        )
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $j_loop)
      )
    )
  )

  ;; Basic GETRF/GETRS for single/double, column-major
  (func (export "SGETRF") (param $m i32) (param $n i32) (param $a i32) (param $lda i32) (param $ipiv i32) (param $info i32)
    (local $minmn i32) (local $j i32) (local $i i32) (local $k i32)
    (local $jp i32) (local $max f32) (local $val f32) (local $ajj f32)

    (local.set $minmn (select (local.get $m) (local.get $n) (i32.lt_s (local.get $m) (local.get $n))))
    (i32.store (local.get $info) (i32.const 0))

    (local.set $j (i32.const 0))
    (block $j_done
      (loop $j_loop
        (br_if $j_done (i32.ge_s (local.get $j) (local.get $minmn)))
        (local.set $jp (local.get $j))
        (local.set $max (f32.abs (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $j) (local.get $lda)) (i32.const 4))))))
        (local.set $i (i32.add (local.get $j) (i32.const 1)))
        (block $i_done
          (loop $i_loop
            (br_if $i_done (i32.ge_s (local.get $i) (local.get $m)))
            (local.set $val (f32.abs (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $lda)) (i32.const 4))))))
            (if (f32.gt (local.get $val) (local.get $max))
              (then (local.set $max (local.get $val)) (local.set $jp (local.get $i)))
            )
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i_loop)
          )
        )
        (i32.store (i32.add (local.get $ipiv) (i32.mul (local.get $j) (i32.const 4))) (i32.add (local.get $jp) (i32.const 1)))
        (if (f32.ne (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $jp) (local.get $j) (local.get $lda)) (i32.const 4)))) (f32.const 0))
          (then
            (if (i32.ne (local.get $jp) (local.get $j))
              (then
                (local.set $k (i32.const 0))
                (block $k_done
                  (loop $k_loop
                    (br_if $k_done (i32.ge_s (local.get $k) (local.get $n)))
                    (local.set $val (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $k) (local.get $lda)) (i32.const 4)))))
                    (f32.store (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $k) (local.get $lda)) (i32.const 4)))
                               (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $jp) (local.get $k) (local.get $lda)) (i32.const 4)))))
                    (f32.store (i32.add (local.get $a) (i32.mul (call $idx (local.get $jp) (local.get $k) (local.get $lda)) (i32.const 4))) (local.get $val))
                    (local.set $k (i32.add (local.get $k) (i32.const 1)))
                    (br $k_loop)
                  )
                )
              )
            )
            (if (i32.lt_s (local.get $j) (i32.sub (local.get $m) (i32.const 1)))
              (then
                (local.set $ajj (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $j) (local.get $lda)) (i32.const 4)))))
                (local.set $i (i32.add (local.get $j) (i32.const 1)))
                (block $i2_done
                  (loop $i2_loop
                    (br_if $i2_done (i32.ge_s (local.get $i) (local.get $m)))
                    (f32.store (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $lda)) (i32.const 4)))
                               (f32.div (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $lda)) (i32.const 4)))) (local.get $ajj)))
                    (local.set $i (i32.add (local.get $i) (i32.const 1)))
                    (br $i2_loop)
                  )
                )
              )
            )
          )
          (else
            (if (i32.eq (i32.load (local.get $info)) (i32.const 0))
              (then (i32.store (local.get $info) (i32.add (local.get $j) (i32.const 1))))
            )
          )
        )

        (if (i32.lt_s (local.get $j) (i32.sub (local.get $minmn) (i32.const 1)))
          (then
            (local.set $i (i32.add (local.get $j) (i32.const 1)))
            (block $i3_done
              (loop $i3_loop
                (br_if $i3_done (i32.ge_s (local.get $i) (local.get $m)))
                (local.set $val (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $lda)) (i32.const 4)))))
                (if (f32.ne (local.get $val) (f32.const 0))
                  (then
                    (local.set $k (i32.add (local.get $j) (i32.const 1)))
                    (block $k2_done
                      (loop $k2_loop
                        (br_if $k2_done (i32.ge_s (local.get $k) (local.get $n)))
                        (f32.store (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $k) (local.get $lda)) (i32.const 4)))
                                   (f32.sub (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $k) (local.get $lda)) (i32.const 4))))
                                            (f32.mul (local.get $val)
                                                     (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $k) (local.get $lda)) (i32.const 4)))))))
                        (local.set $k (i32.add (local.get $k) (i32.const 1)))
                        (br $k2_loop)
                      )
                    )
                  )
                )
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $i3_loop)
              )
            )
          )
        )

        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $j_loop)
      )
    )
  )

  (func (export "SGETRS") (param $trans i32) (param $n i32) (param $nrhs i32) (param $a i32) (param $lda i32)
                            (param $ipiv i32) (param $b i32) (param $ldb i32) (param $info i32)
    (local $k i32) (local $i i32) (local $j i32) (local $aik f32) (local $akk f32)
    (i32.store (local.get $info) (i32.const 0))
    (call $SLASWP (local.get $nrhs) (local.get $b) (local.get $ldb) (i32.const 1) (local.get $n) (local.get $ipiv) (i32.const 1))

    (local.set $k (i32.const 0))
    (block $k_done
      (loop $k_loop
        (br_if $k_done (i32.ge_s (local.get $k) (local.get $n)))
        (local.set $i (i32.add (local.get $k) (i32.const 1)))
        (block $i_done
          (loop $i_loop
            (br_if $i_done (i32.ge_s (local.get $i) (local.get $n)))
            (local.set $aik (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $k) (local.get $lda)) (i32.const 4)))))
            (if (f32.ne (local.get $aik) (f32.const 0))
              (then
                (local.set $j (i32.const 0))
                (block $j_done
                  (loop $j_loop
                    (br_if $j_done (i32.ge_s (local.get $j) (local.get $nrhs)))
                    (f32.store (i32.add (local.get $b) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldb)) (i32.const 4)))
                               (f32.sub (f32.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldb)) (i32.const 4))))
                                        (f32.mul (local.get $aik)
                                                 (f32.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $k) (local.get $j) (local.get $ldb)) (i32.const 4)))))))
                    (local.set $j (i32.add (local.get $j) (i32.const 1)))
                    (br $j_loop)
                  )
                )
              )
            )
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i_loop)
          )
        )
        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $k_loop)
      )
    )

    (local.set $k (i32.sub (local.get $n) (i32.const 1)))
    (block $k2_done
      (loop $k2_loop
        (br_if $k2_done (i32.lt_s (local.get $k) (i32.const 0)))
        (local.set $akk (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $k) (local.get $k) (local.get $lda)) (i32.const 4)))))
        (local.set $j (i32.const 0))
        (block $j2_done
          (loop $j2_loop
            (br_if $j2_done (i32.ge_s (local.get $j) (local.get $nrhs)))
            (f32.store (i32.add (local.get $b) (i32.mul (call $idx (local.get $k) (local.get $j) (local.get $ldb)) (i32.const 4)))
                       (f32.div (f32.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $k) (local.get $j) (local.get $ldb)) (i32.const 4)))) (local.get $akk)))
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (br $j2_loop)
          )
        )
        (local.set $i (i32.const 0))
        (block $i2_done
          (loop $i2_loop
            (br_if $i2_done (i32.ge_s (local.get $i) (local.get $k)))
            (local.set $aik (f32.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $k) (local.get $lda)) (i32.const 4)))))
            (if (f32.ne (local.get $aik) (f32.const 0))
              (then
                (local.set $j (i32.const 0))
                (block $j3_done
                  (loop $j3_loop
                    (br_if $j3_done (i32.ge_s (local.get $j) (local.get $nrhs)))
                    (f32.store (i32.add (local.get $b) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldb)) (i32.const 4)))
                               (f32.sub (f32.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldb)) (i32.const 4))))
                                        (f32.mul (local.get $aik)
                                                 (f32.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $k) (local.get $j) (local.get $ldb)) (i32.const 4)))))))
                    (local.set $j (i32.add (local.get $j) (i32.const 1)))
                    (br $j3_loop)
                  )
                )
              )
            )
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i2_loop)
          )
        )
        (local.set $k (i32.sub (local.get $k) (i32.const 1)))
        (br $k2_loop)
      )
    )
  )

  (func (export "DGETRF") (param $m i32) (param $n i32) (param $a i32) (param $lda i32) (param $ipiv i32) (param $info i32)
    (local $minmn i32) (local $j i32) (local $i i32) (local $k i32)
    (local $jp i32) (local $max f64) (local $val f64) (local $ajj f64)

    (local.set $minmn (select (local.get $m) (local.get $n) (i32.lt_s (local.get $m) (local.get $n))))
    (i32.store (local.get $info) (i32.const 0))

    (local.set $j (i32.const 0))
    (block $j_done
      (loop $j_loop
        (br_if $j_done (i32.ge_s (local.get $j) (local.get $minmn)))
        (local.set $jp (local.get $j))
        (local.set $max (f64.abs (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $j) (local.get $lda)) (i32.const 8))))))
        (local.set $i (i32.add (local.get $j) (i32.const 1)))
        (block $i_done
          (loop $i_loop
            (br_if $i_done (i32.ge_s (local.get $i) (local.get $m)))
            (local.set $val (f64.abs (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $lda)) (i32.const 8))))))
            (if (f64.gt (local.get $val) (local.get $max))
              (then (local.set $max (local.get $val)) (local.set $jp (local.get $i)))
            )
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i_loop)
          )
        )
        (i32.store (i32.add (local.get $ipiv) (i32.mul (local.get $j) (i32.const 4))) (i32.add (local.get $jp) (i32.const 1)))
        (if (f64.ne (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $jp) (local.get $j) (local.get $lda)) (i32.const 8)))) (f64.const 0))
          (then
            (if (i32.ne (local.get $jp) (local.get $j))
              (then
                (local.set $k (i32.const 0))
                (block $k_done
                  (loop $k_loop
                    (br_if $k_done (i32.ge_s (local.get $k) (local.get $n)))
                    (local.set $val (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $k) (local.get $lda)) (i32.const 8)))))
                    (f64.store (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $k) (local.get $lda)) (i32.const 8)))
                               (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $jp) (local.get $k) (local.get $lda)) (i32.const 8)))))
                    (f64.store (i32.add (local.get $a) (i32.mul (call $idx (local.get $jp) (local.get $k) (local.get $lda)) (i32.const 8))) (local.get $val))
                    (local.set $k (i32.add (local.get $k) (i32.const 1)))
                    (br $k_loop)
                  )
                )
              )
            )
            (if (i32.lt_s (local.get $j) (i32.sub (local.get $m) (i32.const 1)))
              (then
                (local.set $ajj (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $j) (local.get $lda)) (i32.const 8)))))
                (local.set $i (i32.add (local.get $j) (i32.const 1)))
                (block $i2_done
                  (loop $i2_loop
                    (br_if $i2_done (i32.ge_s (local.get $i) (local.get $m)))
                    (f64.store (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $lda)) (i32.const 8)))
                               (f64.div (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $lda)) (i32.const 8)))) (local.get $ajj)))
                    (local.set $i (i32.add (local.get $i) (i32.const 1)))
                    (br $i2_loop)
                  )
                )
              )
            )
          )
          (else
            (if (i32.eq (i32.load (local.get $info)) (i32.const 0))
              (then (i32.store (local.get $info) (i32.add (local.get $j) (i32.const 1))))
            )
          )
        )

        (if (i32.lt_s (local.get $j) (i32.sub (local.get $minmn) (i32.const 1)))
          (then
            (local.set $i (i32.add (local.get $j) (i32.const 1)))
            (block $i3_done
              (loop $i3_loop
                (br_if $i3_done (i32.ge_s (local.get $i) (local.get $m)))
                (local.set $val (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $lda)) (i32.const 8)))))
                (if (f64.ne (local.get $val) (f64.const 0))
                  (then
                    (local.set $k (i32.add (local.get $j) (i32.const 1)))
                    (block $k2_done
                      (loop $k2_loop
                        (br_if $k2_done (i32.ge_s (local.get $k) (local.get $n)))
                        (f64.store (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $k) (local.get $lda)) (i32.const 8)))
                                   (f64.sub (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $k) (local.get $lda)) (i32.const 8))))
                                            (f64.mul (local.get $val)
                                                     (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $j) (local.get $k) (local.get $lda)) (i32.const 8)))))))
                        (local.set $k (i32.add (local.get $k) (i32.const 1)))
                        (br $k2_loop)
                      )
                    )
                  )
                )
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $i3_loop)
              )
            )
          )
        )

        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $j_loop)
      )
    )
  )

  (func (export "DGETRS") (param $trans i32) (param $n i32) (param $nrhs i32) (param $a i32) (param $lda i32)
                            (param $ipiv i32) (param $b i32) (param $ldb i32) (param $info i32)
    (local $k i32) (local $i i32) (local $j i32) (local $aik f64) (local $akk f64)
    (i32.store (local.get $info) (i32.const 0))
    (call $DLASWP (local.get $nrhs) (local.get $b) (local.get $ldb) (i32.const 1) (local.get $n) (local.get $ipiv) (i32.const 1))

    (local.set $k (i32.const 0))
    (block $k_done
      (loop $k_loop
        (br_if $k_done (i32.ge_s (local.get $k) (local.get $n)))
        (local.set $i (i32.add (local.get $k) (i32.const 1)))
        (block $i_done
          (loop $i_loop
            (br_if $i_done (i32.ge_s (local.get $i) (local.get $n)))
            (local.set $aik (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $k) (local.get $lda)) (i32.const 8)))))
            (if (f64.ne (local.get $aik) (f64.const 0))
              (then
                (local.set $j (i32.const 0))
                (block $j_done
                  (loop $j_loop
                    (br_if $j_done (i32.ge_s (local.get $j) (local.get $nrhs)))
                    (f64.store (i32.add (local.get $b) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldb)) (i32.const 8)))
                               (f64.sub (f64.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldb)) (i32.const 8))))
                                        (f64.mul (local.get $aik)
                                                 (f64.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $k) (local.get $j) (local.get $ldb)) (i32.const 8)))))))
                    (local.set $j (i32.add (local.get $j) (i32.const 1)))
                    (br $j_loop)
                  )
                )
              )
            )
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i_loop)
          )
        )
        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $k_loop)
      )
    )

    (local.set $k (i32.sub (local.get $n) (i32.const 1)))
    (block $k2_done
      (loop $k2_loop
        (br_if $k2_done (i32.lt_s (local.get $k) (i32.const 0)))
        (local.set $akk (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $k) (local.get $k) (local.get $lda)) (i32.const 8)))))
        (local.set $j (i32.const 0))
        (block $j2_done
          (loop $j2_loop
            (br_if $j2_done (i32.ge_s (local.get $j) (local.get $nrhs)))
            (f64.store (i32.add (local.get $b) (i32.mul (call $idx (local.get $k) (local.get $j) (local.get $ldb)) (i32.const 8)))
                       (f64.div (f64.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $k) (local.get $j) (local.get $ldb)) (i32.const 8)))) (local.get $akk)))
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (br $j2_loop)
          )
        )
        (local.set $i (i32.const 0))
        (block $i2_done
          (loop $i2_loop
            (br_if $i2_done (i32.ge_s (local.get $i) (local.get $k)))
            (local.set $aik (f64.load (i32.add (local.get $a) (i32.mul (call $idx (local.get $i) (local.get $k) (local.get $lda)) (i32.const 8)))))
            (if (f64.ne (local.get $aik) (f64.const 0))
              (then
                (local.set $j (i32.const 0))
                (block $j3_done
                  (loop $j3_loop
                    (br_if $j3_done (i32.ge_s (local.get $j) (local.get $nrhs)))
                    (f64.store (i32.add (local.get $b) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldb)) (i32.const 8)))
                               (f64.sub (f64.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $i) (local.get $j) (local.get $ldb)) (i32.const 8))))
                                        (f64.mul (local.get $aik)
                                                 (f64.load (i32.add (local.get $b) (i32.mul (call $idx (local.get $k) (local.get $j) (local.get $ldb)) (i32.const 8)))))))
                    (local.set $j (i32.add (local.get $j) (i32.const 1)))
                    (br $j3_loop)
                  )
                )
              )
            )
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i2_loop)
          )
        )
        (local.set $k (i32.sub (local.get $k) (i32.const 1)))
        (br $k2_loop)
      )
    )
  )
)
