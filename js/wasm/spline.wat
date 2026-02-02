;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (memory (export "memory") 1)

  (global $A_BASE i32 (i32.const 4096))
  (global $B_BASE i32 (i32.const 8096))
  (global $C_BASE i32 (i32.const 12096))

  (func $load_f32_at (param $base i32) (param $idx i32) (result f32)
    (f32.load
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))))

  (func $store_f32_at (param $base i32) (param $idx i32) (param $val f32)
    (f32.store
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))
      (local.get $val)))

  (func $TRISOL_core (param $a i32) (param $b i32) (param $c i32) (param $d i32) (param $kk i32)
    (local $k i32)
    (local $km i32)
    (local $aval f32)
    (local $tmp f32)

    (local.set $k (i32.const 1))
    (block $k_done
      (loop $k_loop
        (br_if $k_done (i32.ge_s (local.get $k) (local.get $kk)))
        (local.set $km (i32.sub (local.get $k) (i32.const 1)))
        (local.set $aval (call $load_f32_at (local.get $a) (local.get $km)))

        (call $store_f32_at (local.get $c) (local.get $km)
          (f32.div (call $load_f32_at (local.get $c) (local.get $km)) (local.get $aval)))
        (call $store_f32_at (local.get $d) (local.get $km)
          (f32.div (call $load_f32_at (local.get $d) (local.get $km)) (local.get $aval)))

        (local.set $tmp
          (f32.sub
            (call $load_f32_at (local.get $a) (local.get $k))
            (f32.mul (call $load_f32_at (local.get $b) (local.get $k))
                     (call $load_f32_at (local.get $c) (local.get $km)))))
        (call $store_f32_at (local.get $a) (local.get $k) (local.get $tmp))

        (local.set $tmp
          (f32.sub
            (call $load_f32_at (local.get $d) (local.get $k))
            (f32.mul (call $load_f32_at (local.get $b) (local.get $k))
                     (call $load_f32_at (local.get $d) (local.get $km)))))
        (call $store_f32_at (local.get $d) (local.get $k) (local.get $tmp))

        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $k_loop)
      )
    )

    (call $store_f32_at (local.get $d) (i32.sub (local.get $kk) (i32.const 1))
      (f32.div
        (call $load_f32_at (local.get $d) (i32.sub (local.get $kk) (i32.const 1)))
        (call $load_f32_at (local.get $a) (i32.sub (local.get $kk) (i32.const 1)))))

    (local.set $k (i32.sub (local.get $kk) (i32.const 2)))
    (block $bk_done
      (loop $bk_loop
        (br_if $bk_done (i32.lt_s (local.get $k) (i32.const 0)))
        (local.set $tmp
          (f32.sub
            (call $load_f32_at (local.get $d) (local.get $k))
            (f32.mul (call $load_f32_at (local.get $c) (local.get $k))
                     (call $load_f32_at (local.get $d) (i32.add (local.get $k) (i32.const 1))))))
        (call $store_f32_at (local.get $d) (local.get $k) (local.get $tmp))
        (local.set $k (i32.sub (local.get $k) (i32.const 1)))
        (br $bk_loop)
      )
    )
  )

  (func (export "TRISOL") (param $a i32) (param $b i32) (param $c i32) (param $d i32) (param $kk i32)
    (call $TRISOL_core (local.get $a) (local.get $b) (local.get $c) (local.get $d) (local.get $kk)))

  (func (export "SPLINE") (param $x i32) (param $xs i32) (param $s i32) (param $n i32)
    (local $i i32)
    (local $dsm f32)
    (local $dsp f32)
    (local $term1 f32)
    (local $term2 f32)
    (local $idx_last i32)

    (if (i32.gt_s (local.get $n) (i32.const 1000)) (then (return)))

    (local.set $i (i32.const 1))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (i32.sub (local.get $n) (i32.const 1))))
        (local.set $dsm
          (f32.sub
            (call $load_f32_at (local.get $s) (local.get $i))
            (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 1)))))
        (local.set $dsp
          (f32.sub
            (call $load_f32_at (local.get $s) (i32.add (local.get $i) (i32.const 1)))
            (call $load_f32_at (local.get $s) (local.get $i))))
        (call $store_f32_at (global.get $B_BASE) (local.get $i) (local.get $dsp))
        (call $store_f32_at (global.get $A_BASE) (local.get $i)
          (f32.mul (f32.const 2) (f32.add (local.get $dsm) (local.get $dsp))))
        (call $store_f32_at (global.get $C_BASE) (local.get $i) (local.get $dsm))
        (local.set $term1
          (f32.mul
            (f32.sub
              (call $load_f32_at (local.get $x) (i32.add (local.get $i) (i32.const 1)))
              (call $load_f32_at (local.get $x) (local.get $i)))
            (f32.div (local.get $dsm) (local.get $dsp))))
        (local.set $term2
          (f32.mul
            (f32.sub
              (call $load_f32_at (local.get $x) (local.get $i))
              (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1))))
            (f32.div (local.get $dsp) (local.get $dsm))))
        (call $store_f32_at (local.get $xs) (local.get $i)
          (f32.mul (f32.const 3) (f32.add (local.get $term1) (local.get $term2))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )

    (call $store_f32_at (global.get $A_BASE) (i32.const 0) (f32.const 2))
    (call $store_f32_at (global.get $C_BASE) (i32.const 0) (f32.const 1))
    (call $store_f32_at (local.get $xs) (i32.const 0)
      (f32.mul (f32.const 3)
        (f32.div
          (f32.sub (call $load_f32_at (local.get $x) (i32.const 1))
                   (call $load_f32_at (local.get $x) (i32.const 0)))
          (f32.sub (call $load_f32_at (local.get $s) (i32.const 1))
                   (call $load_f32_at (local.get $s) (i32.const 0))))))

    (local.set $idx_last (i32.sub (local.get $n) (i32.const 1)))
    (call $store_f32_at (global.get $B_BASE) (local.get $idx_last) (f32.const 1))
    (call $store_f32_at (global.get $A_BASE) (local.get $idx_last) (f32.const 2))
    (call $store_f32_at (local.get $xs) (local.get $idx_last)
      (f32.mul (f32.const 3)
        (f32.div
          (f32.sub
            (call $load_f32_at (local.get $x) (local.get $idx_last))
            (call $load_f32_at (local.get $x) (i32.sub (local.get $idx_last) (i32.const 1))))
          (f32.sub
            (call $load_f32_at (local.get $s) (local.get $idx_last))
            (call $load_f32_at (local.get $s) (i32.sub (local.get $idx_last) (i32.const 1)))))))

    (call $TRISOL_core (global.get $A_BASE) (global.get $B_BASE) (global.get $C_BASE) (local.get $xs) (local.get $n))
  )

  (func (export "SPLIND") (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (param $xs1 f32) (param $xs2 f32)
    (local $i i32)
    (local $dsm f32)
    (local $dsp f32)
    (local $term1 f32)
    (local $term2 f32)
    (local $idx_last i32)

    (if (i32.gt_s (local.get $n) (i32.const 1000)) (then (return)))

    (local.set $i (i32.const 1))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (i32.sub (local.get $n) (i32.const 1))))
        (local.set $dsm
          (f32.sub
            (call $load_f32_at (local.get $s) (local.get $i))
            (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 1)))))
        (local.set $dsp
          (f32.sub
            (call $load_f32_at (local.get $s) (i32.add (local.get $i) (i32.const 1)))
            (call $load_f32_at (local.get $s) (local.get $i))))
        (call $store_f32_at (global.get $B_BASE) (local.get $i) (local.get $dsp))
        (call $store_f32_at (global.get $A_BASE) (local.get $i)
          (f32.mul (f32.const 2) (f32.add (local.get $dsm) (local.get $dsp))))
        (call $store_f32_at (global.get $C_BASE) (local.get $i) (local.get $dsm))
        (local.set $term1
          (f32.mul
            (f32.sub
              (call $load_f32_at (local.get $x) (i32.add (local.get $i) (i32.const 1)))
              (call $load_f32_at (local.get $x) (local.get $i)))
            (f32.div (local.get $dsm) (local.get $dsp))))
        (local.set $term2
          (f32.mul
            (f32.sub
              (call $load_f32_at (local.get $x) (local.get $i))
              (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1))))
            (f32.div (local.get $dsp) (local.get $dsm))))
        (call $store_f32_at (local.get $xs) (local.get $i)
          (f32.mul (f32.const 3) (f32.add (local.get $term1) (local.get $term2))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )

    (if (f32.eq (local.get $xs1) (f32.const 999))
      (then
        (call $store_f32_at (global.get $A_BASE) (i32.const 0) (f32.const 2))
        (call $store_f32_at (global.get $C_BASE) (i32.const 0) (f32.const 1))
        (call $store_f32_at (local.get $xs) (i32.const 0)
          (f32.mul (f32.const 3)
            (f32.div
              (f32.sub (call $load_f32_at (local.get $x) (i32.const 1))
                       (call $load_f32_at (local.get $x) (i32.const 0)))
              (f32.sub (call $load_f32_at (local.get $s) (i32.const 1))
                       (call $load_f32_at (local.get $s) (i32.const 0))))))
      )
      (else
        (if (f32.eq (local.get $xs1) (f32.const -999))
          (then
            (call $store_f32_at (global.get $A_BASE) (i32.const 0) (f32.const 1))
            (call $store_f32_at (global.get $C_BASE) (i32.const 0) (f32.const 1))
            (call $store_f32_at (local.get $xs) (i32.const 0)
              (f32.mul (f32.const 2)
                (f32.div
                  (f32.sub (call $load_f32_at (local.get $x) (i32.const 1))
                           (call $load_f32_at (local.get $x) (i32.const 0)))
                  (f32.sub (call $load_f32_at (local.get $s) (i32.const 1))
                           (call $load_f32_at (local.get $s) (i32.const 0))))))
          )
          (else
            (call $store_f32_at (global.get $A_BASE) (i32.const 0) (f32.const 1))
            (call $store_f32_at (global.get $C_BASE) (i32.const 0) (f32.const 0))
            (call $store_f32_at (local.get $xs) (i32.const 0) (local.get $xs1))
          )
        )
      )
    )

    (local.set $idx_last (i32.sub (local.get $n) (i32.const 1)))
    (if (f32.eq (local.get $xs2) (f32.const 999))
      (then
        (call $store_f32_at (global.get $B_BASE) (local.get $idx_last) (f32.const 1))
        (call $store_f32_at (global.get $A_BASE) (local.get $idx_last) (f32.const 2))
        (call $store_f32_at (local.get $xs) (local.get $idx_last)
          (f32.mul (f32.const 3)
            (f32.div
              (f32.sub
                (call $load_f32_at (local.get $x) (local.get $idx_last))
                (call $load_f32_at (local.get $x) (i32.sub (local.get $idx_last) (i32.const 1))))
              (f32.sub
                (call $load_f32_at (local.get $s) (local.get $idx_last))
                (call $load_f32_at (local.get $s) (i32.sub (local.get $idx_last) (i32.const 1)))))))
      )
      (else
        (if (f32.eq (local.get $xs2) (f32.const -999))
          (then
            (call $store_f32_at (global.get $B_BASE) (local.get $idx_last) (f32.const 1))
            (call $store_f32_at (global.get $A_BASE) (local.get $idx_last) (f32.const 1))
            (call $store_f32_at (local.get $xs) (local.get $idx_last)
              (f32.mul (f32.const 2)
                (f32.div
                  (f32.sub
                    (call $load_f32_at (local.get $x) (local.get $idx_last))
                    (call $load_f32_at (local.get $x) (i32.sub (local.get $idx_last) (i32.const 1))))
                  (f32.sub
                    (call $load_f32_at (local.get $s) (local.get $idx_last))
                    (call $load_f32_at (local.get $s) (i32.sub (local.get $idx_last) (i32.const 1)))))))
          )
          (else
            (call $store_f32_at (global.get $A_BASE) (local.get $idx_last) (f32.const 1))
            (call $store_f32_at (global.get $B_BASE) (local.get $idx_last) (f32.const 0))
            (call $store_f32_at (local.get $xs) (local.get $idx_last) (local.get $xs2))
          )
        )
      )
    )

    (if (i32.and (i32.eq (local.get $n) (i32.const 2))
                 (i32.and (f32.eq (local.get $xs1) (f32.const -999))
                          (f32.eq (local.get $xs2) (f32.const -999))))
      (then
        (call $store_f32_at (global.get $B_BASE) (local.get $idx_last) (f32.const 1))
        (call $store_f32_at (global.get $A_BASE) (local.get $idx_last) (f32.const 2))
        (call $store_f32_at (local.get $xs) (local.get $idx_last)
          (f32.mul (f32.const 3)
            (f32.div
              (f32.sub
                (call $load_f32_at (local.get $x) (local.get $idx_last))
                (call $load_f32_at (local.get $x) (i32.sub (local.get $idx_last) (i32.const 1))))
              (f32.sub
                (call $load_f32_at (local.get $s) (local.get $idx_last))
                (call $load_f32_at (local.get $s) (i32.sub (local.get $idx_last) (i32.const 1)))))))
      )
    )

    (call $TRISOL_core (global.get $A_BASE) (global.get $B_BASE) (global.get $C_BASE) (local.get $xs) (local.get $n))
  )

  (func (export "SPLINA") (param $x i32) (param $xs i32) (param $s i32) (param $n i32)
    (local $i i32)
    (local $ds f32)
    (local $dx f32)
    (local $xs1 f32)
    (local $xs2 f32)
    (local $lend i32)

    (local.set $lend (i32.const 1))
    (local.set $xs1 (f32.const 0))
    (local.set $xs2 (f32.const 0))
    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (i32.sub (local.get $n) (i32.const 1))))
        (local.set $ds
          (f32.sub
            (call $load_f32_at (local.get $s) (i32.add (local.get $i) (i32.const 1)))
            (call $load_f32_at (local.get $s) (local.get $i))))
        (if (f32.eq (local.get $ds) (f32.const 0))
          (then
            (call $store_f32_at (local.get $xs) (local.get $i) (local.get $xs1))
            (local.set $lend (i32.const 1))
          )
          (else
            (local.set $dx
              (f32.sub
                (call $load_f32_at (local.get $x) (i32.add (local.get $i) (i32.const 1)))
                (call $load_f32_at (local.get $x) (local.get $i))))
            (local.set $xs2 (f32.div (local.get $dx) (local.get $ds)))
            (if (i32.ne (local.get $lend) (i32.const 0))
              (then
                (call $store_f32_at (local.get $xs) (local.get $i) (local.get $xs2))
                (local.set $lend (i32.const 0))
              )
              (else
                (call $store_f32_at (local.get $xs) (local.get $i)
                  (f32.mul (f32.const 0.5) (f32.add (local.get $xs1) (local.get $xs2))))
              )
            )
          )
        )
        (local.set $xs1 (local.get $xs2))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )

    (call $store_f32_at (local.get $xs) (i32.sub (local.get $n) (i32.const 1)) (local.get $xs1))
  )

  (func $find_interval (param $ss f32) (param $s i32) (param $n i32) (result i32)
    (local $ilow i32)
    (local $i i32)
    (local $imid i32)

    (local.set $ilow (i32.const 1))
    (local.set $i (local.get $n))
    (block $done
      (loop $loop
        (br_if $done (i32.le_s (i32.sub (local.get $i) (local.get $ilow)) (i32.const 1)))
        (local.set $imid (i32.div_s (i32.add (local.get $i) (local.get $ilow)) (i32.const 2)))
        (if (f32.lt (local.get $ss) (call $load_f32_at (local.get $s) (i32.sub (local.get $imid) (i32.const 1))))
          (then (local.set $i (local.get $imid)))
          (else (local.set $ilow (local.get $imid)))
        )
        (br $loop)
      )
    )
    (local.get $i)
  )

  (func (export "SEVAL") (param $ss f32) (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (result f32)
    (local $i i32)
    (local $ds f32)
    (local $t f32)
    (local $cx1 f32)
    (local $cx2 f32)

    (local.set $i (call $find_interval (local.get $ss) (local.get $s) (local.get $n)))
    (local.set $ds
      (f32.sub
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 1)))
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2)))))
    (local.set $t
      (f32.div
        (f32.sub (local.get $ss) (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2))))
        (local.get $ds)))
    (local.set $cx1
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 2))))
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))))
    (local.set $cx2
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 1))))
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))))

    (f32.add
      (f32.add
        (f32.mul (local.get $t) (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1))))
        (f32.mul (f32.sub (f32.const 1) (local.get $t)) (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2)))))
      (f32.mul
        (f32.sub (local.get $t) (f32.mul (local.get $t) (local.get $t)))
        (f32.sub
          (f32.mul (f32.sub (f32.const 1) (local.get $t)) (local.get $cx1))
          (f32.mul (local.get $t) (local.get $cx2)))))
  )

  (func (export "DEVAL") (param $ss f32) (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (result f32)
    (local $i i32)
    (local $ds f32)
    (local $t f32)
    (local $cx1 f32)
    (local $cx2 f32)
    (local $val f32)

    (local.set $i (call $find_interval (local.get $ss) (local.get $s) (local.get $n)))
    (local.set $ds
      (f32.sub
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 1)))
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2)))))
    (local.set $t
      (f32.div
        (f32.sub (local.get $ss) (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2))))
        (local.get $ds)))
    (local.set $cx1
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 2))))
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))))
    (local.set $cx2
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 1))))
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))))

    (local.set $val
      (f32.add
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))
        (f32.add
          (f32.mul (f32.add (f32.sub (f32.const 1) (f32.mul (f32.const 4) (local.get $t)))
                            (f32.mul (f32.const 3) (f32.mul (local.get $t) (local.get $t))))
                   (local.get $cx1))
          (f32.mul (local.get $t)
                   (f32.mul (f32.sub (f32.mul (f32.const 3) (local.get $t)) (f32.const 2))
                            (local.get $cx2))))))
    (f32.div (local.get $val) (local.get $ds))
  )

  (func (export "D2VAL") (param $ss f32) (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (result f32)
    (local $i i32)
    (local $ds f32)
    (local $t f32)
    (local $cx1 f32)
    (local $cx2 f32)
    (local $val f32)

    (local.set $i (call $find_interval (local.get $ss) (local.get $s) (local.get $n)))
    (local.set $ds
      (f32.sub
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 1)))
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2)))))
    (local.set $t
      (f32.div
        (f32.sub (local.get $ss) (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2))))
        (local.get $ds)))
    (local.set $cx1
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 2))))
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))))
    (local.set $cx2
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 1))))
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))))

    (local.set $val
      (f32.add
        (f32.mul (f32.sub (f32.mul (f32.const 6) (local.get $t)) (f32.const 4)) (local.get $cx1))
        (f32.mul (f32.sub (f32.mul (f32.const 6) (local.get $t)) (f32.const 2)) (local.get $cx2))))

    (f32.div (local.get $val) (f32.mul (local.get $ds) (local.get $ds)))
  )

  (func (export "SEVALL") (param $ss f32) (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (param $out i32)
    (local $i i32)
    (local $ds f32)
    (local $t f32)
    (local $f0 f32)
    (local $f1 f32)
    (local $f2 f32)
    (local $f3 f32)
    (local $xx f32)
    (local $xxs f32)
    (local $xxss f32)

    (local.set $i (call $find_interval (local.get $ss) (local.get $s) (local.get $n)))
    (local.set $ds
      (f32.sub
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 1)))
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2)))))
    (local.set $t
      (f32.div
        (f32.sub (local.get $ss) (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2))))
        (local.get $ds)))

    (local.set $f0 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))
    (local.set $f1
      (f32.mul (local.get $ds) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 2)))))
    (local.set $f2
      (f32.add
        (f32.neg (f32.mul (local.get $ds)
                  (f32.add
                    (f32.mul (f32.const 2) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 2))))
                    (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 1))))))
        (f32.mul (f32.const 3)
                 (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                          (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2)))))))
    (local.set $f3
      (f32.sub
        (f32.mul (local.get $ds)
                 (f32.add
                   (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 2)))
                   (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 1)))))
        (f32.mul (f32.const 2)
                 (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                          (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2)))))))

    (local.set $xx
      (f32.add (local.get $f0)
        (f32.mul (local.get $t)
          (f32.add (local.get $f1)
            (f32.mul (local.get $t)
              (f32.add (local.get $f2) (f32.mul (local.get $t) (local.get $f3))))))))
    (local.set $xxs
      (f32.add (local.get $f1)
        (f32.mul (local.get $t)
          (f32.add (f32.mul (f32.const 2) (local.get $f2))
                   (f32.mul (local.get $t) (f32.mul (f32.const 3) (local.get $f3)))))))
    (local.set $xxss
      (f32.add (f32.mul (f32.const 2) (local.get $f2))
               (f32.mul (local.get $t) (f32.mul (f32.const 6) (local.get $f3)))))

    (local.set $xxs (f32.div (local.get $xxs) (local.get $ds)))
    (local.set $xxss (f32.div (local.get $xxss) (f32.mul (local.get $ds) (local.get $ds))))

    (f32.store (local.get $out) (local.get $xx))
    (f32.store (i32.add (local.get $out) (i32.const 4)) (local.get $xxs))
    (f32.store (i32.add (local.get $out) (i32.const 8)) (local.get $xxss))
  )

  (func (export "CURV") (param $ss f32) (param $x i32) (param $xs i32) (param $y i32) (param $ys i32) (param $s i32) (param $n i32) (result f32)
    (local $i i32)
    (local $ds f32)
    (local $t f32)
    (local $cx1 f32)
    (local $cx2 f32)
    (local $cy1 f32)
    (local $cy2 f32)
    (local $xd f32)
    (local $xdd f32)
    (local $yd f32)
    (local $ydd f32)
    (local $den f32)

    (local.set $i (call $find_interval (local.get $ss) (local.get $s) (local.get $n)))
    (local.set $ds
      (f32.sub
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 1)))
        (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2)))))
    (local.set $t
      (f32.div
        (f32.sub (local.get $ss) (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 2))))
        (local.get $ds)))

    (local.set $cx1
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 2))))
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))))
    (local.set $cx2
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $xs) (i32.sub (local.get $i) (i32.const 1))))
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))))
    (local.set $xd
      (f32.add
        (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 2))))
        (f32.add
          (f32.mul (f32.add (f32.sub (f32.const 1) (f32.mul (f32.const 4) (local.get $t)))
                            (f32.mul (f32.const 3) (f32.mul (local.get $t) (local.get $t))))
                   (local.get $cx1))
          (f32.mul (local.get $t)
                   (f32.mul (f32.sub (f32.mul (f32.const 3) (local.get $t)) (f32.const 2))
                            (local.get $cx2))))))
    (local.set $xdd
      (f32.add
        (f32.mul (f32.sub (f32.mul (f32.const 6) (local.get $t)) (f32.const 4)) (local.get $cx1))
        (f32.mul (f32.sub (f32.mul (f32.const 6) (local.get $t)) (f32.const 2)) (local.get $cx2))))

    (local.set $cy1
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $ys) (i32.sub (local.get $i) (i32.const 2))))
        (f32.sub (call $load_f32_at (local.get $y) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $y) (i32.sub (local.get $i) (i32.const 2))))))
    (local.set $cy2
      (f32.sub
        (f32.mul (local.get $ds) (call $load_f32_at (local.get $ys) (i32.sub (local.get $i) (i32.const 1))))
        (f32.sub (call $load_f32_at (local.get $y) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $y) (i32.sub (local.get $i) (i32.const 2))))))
    (local.set $yd
      (f32.add
        (f32.sub (call $load_f32_at (local.get $y) (i32.sub (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $y) (i32.sub (local.get $i) (i32.const 2))))
        (f32.add
          (f32.mul (f32.add (f32.sub (f32.const 1) (f32.mul (f32.const 4) (local.get $t)))
                            (f32.mul (f32.const 3) (f32.mul (local.get $t) (local.get $t))))
                   (local.get $cy1))
          (f32.mul (local.get $t)
                   (f32.mul (f32.sub (f32.mul (f32.const 3) (local.get $t)) (f32.const 2))
                            (local.get $cy2))))))
    (local.set $ydd
      (f32.add
        (f32.mul (f32.sub (f32.mul (f32.const 6) (local.get $t)) (f32.const 4)) (local.get $cy1))
        (f32.mul (f32.sub (f32.mul (f32.const 6) (local.get $t)) (f32.const 2)) (local.get $cy2))))

    (local.set $den
      (f32.sqrt
        (f32.mul
          (f32.add (f32.mul (local.get $xd) (local.get $xd))
                   (f32.mul (local.get $yd) (local.get $yd)))
          (f32.mul
            (f32.add (f32.mul (local.get $xd) (local.get $xd))
                     (f32.mul (local.get $yd) (local.get $yd)))
            (f32.add (f32.mul (local.get $xd) (local.get $xd))
                     (f32.mul (local.get $yd) (local.get $yd)))))))

    (f32.div
      (f32.sub (f32.mul (local.get $xd) (local.get $ydd))
               (f32.mul (local.get $yd) (local.get $xdd)))
      (local.get $den))
  )
)
