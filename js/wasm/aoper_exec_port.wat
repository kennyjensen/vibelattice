;; Work-in-progress native EXEC port (hand-written WAT).
;; Not wired into build yet; used to stage the aoper.wat rewrite.
(module
  (memory (export "memory") 1)

  (func $f32_load (param $base i32) (param $idx i32) (result f32)
    (f32.load
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))))

  (func $f32_store (param $base i32) (param $idx i32) (param $val f32)
    (f32.store
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))
      (local.get $val)))

  (func $f64_load (param $base i32) (param $idx i32) (result f64)
    (f64.load
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 8)))))

  (func $f64_store (param $base i32) (param $idx i32) (param $val f64)
    (f64.store
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 8)))
      (local.get $val)))

  ;; idxA(i, j, dim) = i + dim * j, with 1-based indices
  (func $idxA (param $i i32) (param $j i32) (param $dim i32) (result i32)
    (i32.add (local.get $i) (i32.mul (local.get $dim) (local.get $j))))

  ;; LUDCMP_COL for f32
  (func (export "LUDCMP_COL_f32") (param $dim i32) (param $n i32) (param $A i32) (param $INDX i32) (param $WORK i32)
    (local $i i32) (local $j i32) (local $k i32) (local $imax i32)
    (local $sum f32) (local $aamax f32) (local $dum f32) (local $piv f32)

    (local.set $i (i32.const 1))
    (block $i_done
      (loop $i_loop
        (br_if $i_done (i32.gt_s (local.get $i) (local.get $n)))
        (local.set $aamax (f32.const 0))
        (local.set $j (i32.const 1))
        (block $j_done
          (loop $j_loop
            (br_if $j_done (i32.gt_s (local.get $j) (local.get $n)))
            (local.set $sum
              (f32.abs (call $f32_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)))))
            (if (f32.gt (local.get $sum) (local.get $aamax))
              (then (local.set $aamax (local.get $sum))))
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (br $j_loop)
          )
        )
        (if (f32.eq (local.get $aamax) (f32.const 0))
          (then (call $f32_store (local.get $WORK) (local.get $i) (f32.const 0)))
          (else (call $f32_store (local.get $WORK) (local.get $i) (f32.div (f32.const 1) (local.get $aamax)))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $i_loop)
      )
    )

    (local.set $j (i32.const 1))
    (block $jj_done
      (loop $jj_loop
        (br_if $jj_done (i32.gt_s (local.get $j) (local.get $n)))

        (local.set $i (i32.const 1))
        (block $i2_done
          (loop $i2_loop
            (br_if $i2_done (i32.ge_s (local.get $i) (local.get $j)))
            (local.set $sum (call $f32_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim))))
            (local.set $k (i32.const 1))
            (block $k2_done
              (loop $k2_loop
                (br_if $k2_done (i32.ge_s (local.get $k) (local.get $i)))
                (local.set $sum
                  (f32.sub (local.get $sum)
                    (f32.mul
                      (call $f32_load (local.get $A) (call $idxA (local.get $i) (local.get $k) (local.get $dim)))
                      (call $f32_load (local.get $A) (call $idxA (local.get $k) (local.get $j) (local.get $dim))))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k2_loop)
              )
            )
            (call $f32_store (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)) (local.get $sum))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i2_loop)
          )
        )

        (local.set $aamax (f32.const 0))
        (local.set $imax (local.get $j))
        (local.set $i (local.get $j))
        (block $i3_done
          (loop $i3_loop
            (br_if $i3_done (i32.gt_s (local.get $i) (local.get $n)))
            (local.set $sum (call $f32_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim))))
            (local.set $k (i32.const 1))
            (block $k3_done
              (loop $k3_loop
                (br_if $k3_done (i32.ge_s (local.get $k) (local.get $j)))
                (local.set $sum
                  (f32.sub (local.get $sum)
                    (f32.mul
                      (call $f32_load (local.get $A) (call $idxA (local.get $i) (local.get $k) (local.get $dim)))
                      (call $f32_load (local.get $A) (call $idxA (local.get $k) (local.get $j) (local.get $dim))))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k3_loop)
              )
            )
            (call $f32_store (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)) (local.get $sum))
            (local.set $dum
              (f32.mul (call $f32_load (local.get $WORK) (local.get $i)) (f32.abs (local.get $sum))))
            (if (f32.ge (local.get $dum) (local.get $aamax))
              (then
                (local.set $aamax (local.get $dum))
                (local.set $imax (local.get $i))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i3_loop)
          )
        )

        (if (i32.ne (local.get $j) (local.get $imax))
          (then
            (local.set $k (i32.const 1))
            (block $k4_done
              (loop $k4_loop
                (br_if $k4_done (i32.gt_s (local.get $k) (local.get $n)))
                (local.set $dum (call $f32_load (local.get $A) (call $idxA (local.get $imax) (local.get $k) (local.get $dim))))
                (call $f32_store (local.get $A) (call $idxA (local.get $imax) (local.get $k) (local.get $dim))
                  (call $f32_load (local.get $A) (call $idxA (local.get $j) (local.get $k) (local.get $dim))))
                (call $f32_store (local.get $A) (call $idxA (local.get $j) (local.get $k) (local.get $dim)) (local.get $dum))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k4_loop)
              )
            )
            (call $f32_store (local.get $WORK) (local.get $imax) (call $f32_load (local.get $WORK) (local.get $j)))
          ))

        (i32.store (i32.add (local.get $INDX) (i32.mul (local.get $j) (i32.const 4))) (local.get $imax))

        (if (i32.ne (local.get $j) (local.get $n))
          (then
            (local.set $piv (call $f32_load (local.get $A) (call $idxA (local.get $j) (local.get $j) (local.get $dim))))
            (if (f32.eq (local.get $piv) (f32.const 0)) (then (local.set $piv (f32.const 1.0e-12))))
            (local.set $dum (f32.div (f32.const 1) (local.get $piv)))
            (local.set $i (i32.add (local.get $j) (i32.const 1)))
            (block $i4_done
              (loop $i4_loop
                (br_if $i4_done (i32.gt_s (local.get $i) (local.get $n)))
                (call $f32_store (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim))
                  (f32.mul (call $f32_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim))) (local.get $dum)))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $i4_loop)
              )
            )
          ))
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $jj_loop)
      )
    )
  )

  ;; BAKSUB_COL for f32
  (func (export "BAKSUB_COL_f32") (param $dim i32) (param $n i32) (param $A i32) (param $INDX i32) (param $B i32)
    (local $i i32) (local $j i32) (local $ii i32) (local $ll i32)
    (local $sum f32) (local $piv f32)

    (local.set $ii (i32.const 0))
    (local.set $i (i32.const 1))
    (block $i_done
      (loop $i_loop
        (br_if $i_done (i32.gt_s (local.get $i) (local.get $n)))
        (local.set $ll (i32.load (i32.add (local.get $INDX) (i32.mul (local.get $i) (i32.const 4)))))
        (local.set $sum (call $f32_load (local.get $B) (local.get $ll)))
        (call $f32_store (local.get $B) (local.get $ll) (call $f32_load (local.get $B) (local.get $i)))
        (if (i32.ne (local.get $ii) (i32.const 0))
          (then
            (local.set $j (local.get $ii))
            (block $j_done
              (loop $j_loop
                (br_if $j_done (i32.gt_s (local.get $j) (i32.sub (local.get $i) (i32.const 1))))
                (local.set $sum
                  (f32.sub (local.get $sum)
                    (f32.mul
                      (call $f32_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)))
                      (call $f32_load (local.get $B) (local.get $j)))))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
            )
          )
          (else
            (if (f32.ne (local.get $sum) (f32.const 0))
              (then (local.set $ii (local.get $i))))))
        (call $f32_store (local.get $B) (local.get $i) (local.get $sum))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $i_loop)
      )
    )

    (local.set $i (local.get $n))
    (block $k_done
      (loop $k_loop
        (br_if $k_done (i32.lt_s (local.get $i) (i32.const 1)))
        (local.set $sum (call $f32_load (local.get $B) (local.get $i)))
        (if (i32.lt_s (local.get $i) (local.get $n))
          (then
            (local.set $j (i32.add (local.get $i) (i32.const 1)))
            (block $j2_done
              (loop $j2_loop
                (br_if $j2_done (i32.gt_s (local.get $j) (local.get $n)))
                (local.set $sum
                  (f32.sub (local.get $sum)
                    (f32.mul
                      (call $f32_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)))
                      (call $f32_load (local.get $B) (local.get $j)))))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j2_loop)
              )
            )
          ))
        (local.set $piv (call $f32_load (local.get $A) (call $idxA (local.get $i) (local.get $i) (local.get $dim))))
        (if (f32.eq (local.get $piv) (f32.const 0)) (then (local.set $piv (f32.const 1.0e-12))))
        (call $f32_store (local.get $B) (local.get $i) (f32.div (local.get $sum) (local.get $piv)))
        (local.set $i (i32.sub (local.get $i) (i32.const 1)))
        (br $k_loop)
      )
    )
  )

  ;; LUDCMP_COL for f64
  (func (export "LUDCMP_COL_f64") (param $dim i32) (param $n i32) (param $A i32) (param $INDX i32) (param $WORK i32)
    (local $i i32) (local $j i32) (local $k i32) (local $imax i32)
    (local $sum f64) (local $aamax f64) (local $dum f64) (local $piv f64)

    (local.set $i (i32.const 1))
    (block $i_done
      (loop $i_loop
        (br_if $i_done (i32.gt_s (local.get $i) (local.get $n)))
        (local.set $aamax (f64.const 0))
        (local.set $j (i32.const 1))
        (block $j_done
          (loop $j_loop
            (br_if $j_done (i32.gt_s (local.get $j) (local.get $n)))
            (local.set $sum
              (f64.abs (call $f64_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)))))
            (if (f64.gt (local.get $sum) (local.get $aamax))
              (then (local.set $aamax (local.get $sum))))
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (br $j_loop)
          )
        )
        (if (f64.eq (local.get $aamax) (f64.const 0))
          (then (call $f64_store (local.get $WORK) (local.get $i) (f64.const 0)))
          (else (call $f64_store (local.get $WORK) (local.get $i) (f64.div (f64.const 1) (local.get $aamax)))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $i_loop)
      )
    )

    (local.set $j (i32.const 1))
    (block $jj_done
      (loop $jj_loop
        (br_if $jj_done (i32.gt_s (local.get $j) (local.get $n)))

        (local.set $i (i32.const 1))
        (block $i2_done
          (loop $i2_loop
            (br_if $i2_done (i32.ge_s (local.get $i) (local.get $j)))
            (local.set $sum (call $f64_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim))))
            (local.set $k (i32.const 1))
            (block $k2_done
              (loop $k2_loop
                (br_if $k2_done (i32.ge_s (local.get $k) (local.get $i)))
                (local.set $sum
                  (f64.sub (local.get $sum)
                    (f64.mul
                      (call $f64_load (local.get $A) (call $idxA (local.get $i) (local.get $k) (local.get $dim)))
                      (call $f64_load (local.get $A) (call $idxA (local.get $k) (local.get $j) (local.get $dim))))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k2_loop)
              )
            )
            (call $f64_store (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)) (local.get $sum))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i2_loop)
          )
        )

        (local.set $aamax (f64.const 0))
        (local.set $imax (local.get $j))
        (local.set $i (local.get $j))
        (block $i3_done
          (loop $i3_loop
            (br_if $i3_done (i32.gt_s (local.get $i) (local.get $n)))
            (local.set $sum (call $f64_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim))))
            (local.set $k (i32.const 1))
            (block $k3_done
              (loop $k3_loop
                (br_if $k3_done (i32.ge_s (local.get $k) (local.get $j)))
                (local.set $sum
                  (f64.sub (local.get $sum)
                    (f64.mul
                      (call $f64_load (local.get $A) (call $idxA (local.get $i) (local.get $k) (local.get $dim)))
                      (call $f64_load (local.get $A) (call $idxA (local.get $k) (local.get $j) (local.get $dim))))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k3_loop)
              )
            )
            (call $f64_store (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)) (local.get $sum))
            (local.set $dum
              (f64.mul (call $f64_load (local.get $WORK) (local.get $i)) (f64.abs (local.get $sum))))
            (if (f64.ge (local.get $dum) (local.get $aamax))
              (then
                (local.set $aamax (local.get $dum))
                (local.set $imax (local.get $i))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i3_loop)
          )
        )

        (if (i32.ne (local.get $j) (local.get $imax))
          (then
            (local.set $k (i32.const 1))
            (block $k4_done
              (loop $k4_loop
                (br_if $k4_done (i32.gt_s (local.get $k) (local.get $n)))
                (local.set $dum (call $f64_load (local.get $A) (call $idxA (local.get $imax) (local.get $k) (local.get $dim))))
                (call $f64_store (local.get $A) (call $idxA (local.get $imax) (local.get $k) (local.get $dim))
                  (call $f64_load (local.get $A) (call $idxA (local.get $j) (local.get $k) (local.get $dim))))
                (call $f64_store (local.get $A) (call $idxA (local.get $j) (local.get $k) (local.get $dim)) (local.get $dum))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k4_loop)
              )
            )
            (call $f64_store (local.get $WORK) (local.get $imax) (call $f64_load (local.get $WORK) (local.get $j)))
          ))

        (i32.store (i32.add (local.get $INDX) (i32.mul (local.get $j) (i32.const 4))) (local.get $imax))

        (if (i32.ne (local.get $j) (local.get $n))
          (then
            (local.set $piv (call $f64_load (local.get $A) (call $idxA (local.get $j) (local.get $j) (local.get $dim))))
            (local.set $dum (f64.div (f64.const 1) (local.get $piv)))
            (local.set $i (i32.add (local.get $j) (i32.const 1)))
            (block $i4_done
              (loop $i4_loop
                (br_if $i4_done (i32.gt_s (local.get $i) (local.get $n)))
                (call $f64_store (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim))
                  (f64.mul (call $f64_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim))) (local.get $dum)))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $i4_loop)
              )
            )
          ))
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $jj_loop)
      )
    )
  )

  ;; BAKSUB_COL for f64
  (func (export "BAKSUB_COL_f64") (param $dim i32) (param $n i32) (param $A i32) (param $INDX i32) (param $B i32)
    (local $i i32) (local $j i32) (local $ii i32) (local $ll i32)
    (local $sum f64) (local $piv f64)

    (local.set $ii (i32.const 0))
    (local.set $i (i32.const 1))
    (block $i_done
      (loop $i_loop
        (br_if $i_done (i32.gt_s (local.get $i) (local.get $n)))
        (local.set $ll (i32.load (i32.add (local.get $INDX) (i32.mul (local.get $i) (i32.const 4)))))
        (local.set $sum (call $f64_load (local.get $B) (local.get $ll)))
        (call $f64_store (local.get $B) (local.get $ll) (call $f64_load (local.get $B) (local.get $i)))
        (if (i32.ne (local.get $ii) (i32.const 0))
          (then
            (local.set $j (local.get $ii))
            (block $j_done
              (loop $j_loop
                (br_if $j_done (i32.gt_s (local.get $j) (i32.sub (local.get $i) (i32.const 1))))
                (local.set $sum
                  (f64.sub (local.get $sum)
                    (f64.mul
                      (call $f64_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)))
                      (call $f64_load (local.get $B) (local.get $j)))))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
            )
          )
          (else
            (if (f64.ne (local.get $sum) (f64.const 0))
              (then (local.set $ii (local.get $i))))))
        (call $f64_store (local.get $B) (local.get $i) (local.get $sum))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $i_loop)
      )
    )

    (local.set $i (local.get $n))
    (block $k_done
      (loop $k_loop
        (br_if $k_done (i32.lt_s (local.get $i) (i32.const 1)))
        (local.set $sum (call $f64_load (local.get $B) (local.get $i)))
        (if (i32.lt_s (local.get $i) (local.get $n))
          (then
            (local.set $j (i32.add (local.get $i) (i32.const 1)))
            (block $j2_done
              (loop $j2_loop
                (br_if $j2_done (i32.gt_s (local.get $j) (local.get $n)))
                (local.set $sum
                  (f64.sub (local.get $sum)
                    (f64.mul
                      (call $f64_load (local.get $A) (call $idxA (local.get $i) (local.get $j) (local.get $dim)))
                      (call $f64_load (local.get $B) (local.get $j)))))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j2_loop)
              )
            )
          ))
        (local.set $piv (call $f64_load (local.get $A) (call $idxA (local.get $i) (local.get $i) (local.get $dim))))
        (call $f64_store (local.get $B) (local.get $i) (f64.div (local.get $sum) (local.get $piv)))
        (local.set $i (i32.sub (local.get $i) (i32.const 1)))
        (br $k_loop)
      )
    )
  )
)
