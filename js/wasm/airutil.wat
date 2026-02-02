;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (import "env" "cos" (func $cos (param f32) (result f32)))
  (import "env" "atan" (func $atan (param f32) (result f32)))

  (memory (export "memory") 1)

  (global $A_BASE i32 (i32.const 4096))
  (global $B_BASE i32 (i32.const 8096))
  (global $C_BASE i32 (i32.const 12096))
  (global $S_BASE i32 (i32.const 20000))
  (global $XP_BASE i32 (i32.const 22000))
  (global $YP_BASE i32 (i32.const 24000))

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

  (func $SPLIND (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (param $xs1 f32) (param $xs2 f32)
    (local $i i32)
    (local $dsm f32)
    (local $dsp f32)
    (local $term1 f32)
    (local $term2 f32)
    (local $idx_last i32)

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

  (func $SCALC (param $x i32) (param $y i32) (param $s i32) (param $n i32)
    (local $i i32)
    (local $dx f32)
    (local $dy f32)
    (local $tmp f32)

    (call $store_f32_at (local.get $s) (i32.const 0) (f32.const 0))
    (local.set $i (i32.const 1))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (local.set $dx
          (f32.sub (call $load_f32_at (local.get $x) (local.get $i))
                   (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))))
        (local.set $dy
          (f32.sub (call $load_f32_at (local.get $y) (local.get $i))
                   (call $load_f32_at (local.get $y) (i32.sub (local.get $i) (i32.const 1)))))
        (local.set $tmp
          (f32.sqrt
            (f32.add (f32.mul (local.get $dx) (local.get $dx))
                     (f32.mul (local.get $dy) (local.get $dy)))))
        (call $store_f32_at (local.get $s) (local.get $i)
          (f32.add (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 1))) (local.get $tmp)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func $SEVAL (param $ss f32) (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (result f32)
    (local $ilow i32)
    (local $i i32)
    (local $imid i32)
    (local $ds f32)
    (local $t f32)
    (local $cx1 f32)
    (local $cx2 f32)

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

  (func $DEVAL (param $ss f32) (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (result f32)
    (local $ilow i32)
    (local $i i32)
    (local $imid i32)
    (local $ds f32)
    (local $t f32)
    (local $cx1 f32)
    (local $cx2 f32)
    (local $val f32)

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

  (func $D2VAL (param $ss f32) (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (result f32)
    (local $ilow i32)
    (local $i i32)
    (local $imid i32)
    (local $ds f32)
    (local $t f32)
    (local $cx1 f32)
    (local $cx2 f32)
    (local $val f32)

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

  (func $SINVRT (param $si f32) (param $xi f32) (param $x i32) (param $xs i32) (param $s i32) (param $n i32) (result f32)
    (local $iter i32)
    (local $res f32)
    (local $resp f32)
    (local $ds f32)
    (local $sref f32)

    (local.set $sref
      (f32.sub (call $load_f32_at (local.get $s) (i32.sub (local.get $n) (i32.const 1)))
               (call $load_f32_at (local.get $s) (i32.const 0))))
    (local.set $iter (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $iter) (i32.const 10)))
        (local.set $res
          (f32.sub
            (call $SEVAL (local.get $si) (local.get $x) (local.get $xs) (local.get $s) (local.get $n))
            (local.get $xi)))
        (local.set $resp
          (call $DEVAL (local.get $si) (local.get $x) (local.get $xs) (local.get $s) (local.get $n)))
        (local.set $ds (f32.div (f32.neg (local.get $res)) (local.get $resp)))
        (local.set $si (f32.add (local.get $si) (local.get $ds)))
        (if (f32.lt (f32.abs (f32.div (local.get $ds) (local.get $sref))) (f32.const 1.0e-5))
          (then (return (local.get $si)))
        )
        (local.set $iter (i32.add (local.get $iter) (i32.const 1)))
        (br $loop)
      )
    )
    (local.get $si)
  )

  (func $SEGSPL (param $x i32) (param $xs i32) (param $s i32) (param $n i32)
    (local $iseg0 i32)
    (local $iseg i32)
    (local $nseg i32)
    (local $xptr i32)
    (local $xsptr i32)
    (local $sptr i32)

    (if (f32.eq (call $load_f32_at (local.get $s) (i32.const 0)) (call $load_f32_at (local.get $s) (i32.const 1)))
      (then (return))
    )
    (if (f32.eq (call $load_f32_at (local.get $s) (i32.sub (local.get $n) (i32.const 1)))
                (call $load_f32_at (local.get $s) (i32.sub (local.get $n) (i32.const 2))))
      (then (return))
    )

    (local.set $iseg0 (i32.const 0))
    (local.set $iseg (i32.const 1))
    (block $done
      (loop $loop
        (br_if $done (i32.gt_s (local.get $iseg) (i32.sub (local.get $n) (i32.const 3))))
        (if (f32.eq (call $load_f32_at (local.get $s) (local.get $iseg))
                    (call $load_f32_at (local.get $s) (i32.add (local.get $iseg) (i32.const 1))))
          (then
            (local.set $nseg (i32.add (i32.sub (local.get $iseg) (local.get $iseg0)) (i32.const 1)))
            (local.set $xptr (i32.add (local.get $x) (i32.mul (local.get $iseg0) (i32.const 4))))
            (local.set $xsptr (i32.add (local.get $xs) (i32.mul (local.get $iseg0) (i32.const 4))))
            (local.set $sptr (i32.add (local.get $s) (i32.mul (local.get $iseg0) (i32.const 4))))
            (call $SPLIND (local.get $xptr) (local.get $xsptr) (local.get $sptr) (local.get $nseg)
              (f32.const -999) (f32.const -999))
            (local.set $iseg0 (i32.add (local.get $iseg) (i32.const 1)))
          )
        )
        (local.set $iseg (i32.add (local.get $iseg) (i32.const 1)))
        (br $loop)
      )
    )

    (local.set $nseg (i32.sub (local.get $n) (local.get $iseg0)))
    (local.set $xptr (i32.add (local.get $x) (i32.mul (local.get $iseg0) (i32.const 4))))
    (local.set $xsptr (i32.add (local.get $xs) (i32.mul (local.get $iseg0) (i32.const 4))))
    (local.set $sptr (i32.add (local.get $s) (i32.mul (local.get $iseg0) (i32.const 4))))
    (call $SPLIND (local.get $xptr) (local.get $xsptr) (local.get $sptr) (local.get $nseg)
      (f32.const -999) (f32.const -999))
  )

  (func $LEFIND (param $x i32) (param $xp i32) (param $y i32) (param $yp i32) (param $s i32) (param $n i32) (result f32)
    (local $i i32)
    (local $sle f32)
    (local $sref f32)
    (local $iter i32)
    (local $res f32)
    (local $resp f32)
    (local $dsle f32)

    (local.set $sle (call $load_f32_at (local.get $s) (i32.const 0)))
    (local.set $i (i32.const 1))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (if (f32.gt (call $load_f32_at (local.get $x) (local.get $i))
                    (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1))))
          (then
            (local.set $sle (call $load_f32_at (local.get $s) (i32.sub (local.get $i) (i32.const 1))))
            (br $done)
          )
        )
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )

    (local.set $sref
      (f32.sub (call $load_f32_at (local.get $s) (i32.sub (local.get $n) (i32.const 1)))
               (call $load_f32_at (local.get $s) (i32.const 0))))
    (local.set $iter (i32.const 0))
    (block $iter_done
      (loop $iter_loop
        (br_if $iter_done (i32.ge_s (local.get $iter) (i32.const 20)))
        (local.set $res (call $DEVAL (local.get $sle) (local.get $x) (local.get $xp) (local.get $s) (local.get $n)))
        (local.set $resp (call $D2VAL (local.get $sle) (local.get $x) (local.get $xp) (local.get $s) (local.get $n)))
        (local.set $dsle (f32.div (f32.neg (local.get $res)) (local.get $resp)))
        (local.set $sle (f32.add (local.get $sle) (local.get $dsle)))
        (if (f32.lt (f32.abs (f32.div (local.get $dsle) (local.get $sref))) (f32.const 1.0e-5))
          (then (return (local.get $sle)))
        )
        (local.set $iter (i32.add (local.get $iter) (i32.const 1)))
        (br $iter_loop)
      )
    )
    (local.get $sle)
  )

  (func $NORMIT (param $sle f32) (param $x i32) (param $xp i32) (param $y i32) (param $yp i32) (param $s i32) (param $n i32) (result f32)
    (local $xle f32)
    (local $xte f32)
    (local $dnorm f32)
    (local $i i32)

    (local.set $xle (call $SEVAL (local.get $sle) (local.get $x) (local.get $xp) (local.get $s) (local.get $n)))
    (local.set $xte
      (f32.mul (f32.const 0.5)
               (f32.add
                 (call $load_f32_at (local.get $x) (i32.const 0))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $n) (i32.const 1))))))
    (local.set $dnorm (f32.div (f32.const 1) (f32.sub (local.get $xte) (local.get $xle))))

    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (call $store_f32_at (local.get $x) (local.get $i)
          (f32.mul (f32.sub (call $load_f32_at (local.get $x) (local.get $i)) (local.get $xle)) (local.get $dnorm)))
        (call $store_f32_at (local.get $y) (local.get $i)
          (f32.mul (call $load_f32_at (local.get $y) (local.get $i)) (local.get $dnorm)))
        (call $store_f32_at (local.get $s) (local.get $i)
          (f32.mul (call $load_f32_at (local.get $s) (local.get $i)) (local.get $dnorm)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
    (f32.mul (local.get $sle) (local.get $dnorm))
  )

  (func (export "GETCAM") (param $x i32) (param $y i32) (param $n i32)
    (param $xc i32) (param $yc i32) (param $tc i32) (param $nc i32) (param $lnorm i32)
    (local $sle f32)
    (local $xle f32)
    (local $yle f32)
    (local $xte f32)
    (local $su f32)
    (local $sl f32)
    (local $fnc1 f32)
    (local $i i32)
    (local $xout f32)
    (local $yu f32)
    (local $yl f32)
    (local $pi f32)
    (local $nc_use i32)

    (call $SCALC (local.get $x) (local.get $y) (global.get $S_BASE) (local.get $n))
    (call $SEGSPL (local.get $x) (global.get $XP_BASE) (global.get $S_BASE) (local.get $n))
    (call $SEGSPL (local.get $y) (global.get $YP_BASE) (global.get $S_BASE) (local.get $n))

    (local.set $sle
      (call $LEFIND (local.get $x) (global.get $XP_BASE) (local.get $y) (global.get $YP_BASE) (global.get $S_BASE) (local.get $n)))
    (if (i32.ne (local.get $lnorm) (i32.const 0))
      (then
        (local.set $sle
          (call $NORMIT (local.get $sle) (local.get $x) (global.get $XP_BASE) (local.get $y) (global.get $YP_BASE) (global.get $S_BASE) (local.get $n)))
      )
    )

    (local.set $xle (call $SEVAL (local.get $sle) (local.get $x) (global.get $XP_BASE) (global.get $S_BASE) (local.get $n)))
    (local.set $yle (call $SEVAL (local.get $sle) (local.get $y) (global.get $YP_BASE) (global.get $S_BASE) (local.get $n)))
    (local.set $xte
      (f32.mul (f32.const 0.5)
               (f32.add
                 (call $load_f32_at (local.get $x) (i32.const 0))
                 (call $load_f32_at (local.get $x) (i32.sub (local.get $n) (i32.const 1))))))

    (local.set $nc_use (local.get $nc))
    (if (i32.le_s (local.get $nc_use) (i32.const 0))
      (then (local.set $nc_use (i32.const 30)))
    )

    (local.set $su (f32.sub (local.get $sle) (f32.const 0.01)))
    (local.set $sl (f32.add (local.get $sle) (f32.const 0.01)))
    (local.set $fnc1 (f32.convert_i32_s (i32.sub (local.get $nc_use) (i32.const 1))))

    (call $store_f32_at (local.get $xc) (i32.const 0) (local.get $xle))
    (call $store_f32_at (local.get $yc) (i32.const 0) (local.get $yle))
    (call $store_f32_at (local.get $tc) (i32.const 0) (f32.const 0))

    (local.set $pi (f32.mul (f32.const 4) (call $atan (f32.const 1))))
    (local.set $i (i32.const 1))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $nc_use)))
        (local.set $xout
          (f32.add (local.get $xle)
                   (f32.mul
                     (f32.sub (local.get $xte) (local.get $xle))
                     (f32.mul (f32.const 0.5)
                              (f32.sub (f32.const 1)
                                       (call $cos
                                         (f32.div (f32.mul (local.get $pi) (f32.convert_i32_s (local.get $i))) (local.get $fnc1))))))))
        (local.set $su
          (call $SINVRT (local.get $su) (local.get $xout) (local.get $x) (global.get $XP_BASE) (global.get $S_BASE) (local.get $n)))
        (local.set $yu
          (call $SEVAL (local.get $su) (local.get $y) (global.get $YP_BASE) (global.get $S_BASE) (local.get $n)))
        (local.set $sl
          (call $SINVRT (local.get $sl) (local.get $xout) (local.get $x) (global.get $XP_BASE) (global.get $S_BASE) (local.get $n)))
        (local.set $yl
          (call $SEVAL (local.get $sl) (local.get $y) (global.get $YP_BASE) (global.get $S_BASE) (local.get $n)))

        (call $store_f32_at (local.get $xc) (local.get $i) (local.get $xout))
        (call $store_f32_at (local.get $yc) (local.get $i)
          (f32.mul (f32.const 0.5) (f32.add (local.get $yu) (local.get $yl))))
        (call $store_f32_at (local.get $tc) (local.get $i)
          (f32.sub (local.get $yu) (local.get $yl)))

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )
)
