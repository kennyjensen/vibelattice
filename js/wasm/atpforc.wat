;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (import "env" "sin_f32" (func $sin_f32 (param f32) (result f32)))
  (import "env" "cos_f32" (func $cos_f32 (param f32) (result f32)))
  (memory (export "memory") 1)

  (func $idx3 (param $r i32) (param $c i32) (result i32)
    (i32.add (local.get $r) (i32.mul (i32.const 3) (local.get $c)))
  )

  (func $idx2 (param $i i32) (param $j i32) (param $ncol i32) (result i32)
    (i32.add (i32.mul (local.get $i) (local.get $ncol)) (local.get $j))
  )

  (func $PGMAT (export "PGMAT")
    (param $mach f32) (param $alfa f32) (param $beta f32)
    (param $p i32) (param $p_m i32) (param $p_a i32) (param $p_b i32)
    (local $binv f32) (local $bi_m f32)
    (local $sina f32) (local $cosa f32) (local $sinb f32) (local $cosb f32)

    (local.set $binv
      (f32.div (f32.const 1)
        (f32.sqrt (f32.sub (f32.const 1) (f32.mul (local.get $mach) (local.get $mach))))))
    (local.set $bi_m (f32.mul (local.get $mach) (f32.mul (local.get $binv) (f32.mul (local.get $binv) (local.get $binv)))))

    (local.set $sina (call $sin_f32 (local.get $alfa)))
    (local.set $cosa (call $cos_f32 (local.get $alfa)))
    (local.set $sinb (call $sin_f32 (local.get $beta)))
    (local.set $cosb (call $cos_f32 (local.get $beta)))

    (f32.store (local.get $p) (f32.mul (f32.mul (local.get $cosa) (local.get $cosb)) (local.get $binv)))
    (f32.store (i32.add (local.get $p) (i32.const 4)) (f32.mul (f32.neg (local.get $sinb)) (local.get $binv)))
    (f32.store (i32.add (local.get $p) (i32.const 8)) (f32.mul (f32.mul (local.get $sina) (local.get $cosb)) (local.get $binv)))

    (f32.store (i32.add (local.get $p) (i32.const 12)) (f32.mul (local.get $cosa) (local.get $sinb)))
    (f32.store (i32.add (local.get $p) (i32.const 16)) (local.get $cosb))
    (f32.store (i32.add (local.get $p) (i32.const 20)) (f32.mul (local.get $sina) (local.get $sinb)))

    (f32.store (i32.add (local.get $p) (i32.const 24)) (f32.neg (local.get $sina)))
    (f32.store (i32.add (local.get $p) (i32.const 28)) (f32.const 0))
    (f32.store (i32.add (local.get $p) (i32.const 32)) (local.get $cosa))

    (f32.store (local.get $p_m) (f32.mul (f32.mul (local.get $cosa) (local.get $cosb)) (local.get $bi_m)))
    (f32.store (i32.add (local.get $p_m) (i32.const 4)) (f32.mul (f32.neg (local.get $sinb)) (local.get $bi_m)))
    (f32.store (i32.add (local.get $p_m) (i32.const 8)) (f32.mul (f32.mul (local.get $sina) (local.get $cosb)) (local.get $bi_m)))

    (f32.store (i32.add (local.get $p_m) (i32.const 12)) (f32.const 0))
    (f32.store (i32.add (local.get $p_m) (i32.const 16)) (f32.const 0))
    (f32.store (i32.add (local.get $p_m) (i32.const 20)) (f32.const 0))

    (f32.store (i32.add (local.get $p_m) (i32.const 24)) (f32.const 0))
    (f32.store (i32.add (local.get $p_m) (i32.const 28)) (f32.const 0))
    (f32.store (i32.add (local.get $p_m) (i32.const 32)) (f32.const 0))

    (f32.store (local.get $p_a) (f32.mul (f32.neg (local.get $sina)) (f32.mul (local.get $cosb) (local.get $binv))))
    (f32.store (i32.add (local.get $p_a) (i32.const 4)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 8)) (f32.mul (f32.mul (local.get $cosa) (local.get $cosb)) (local.get $binv)))

    (f32.store (i32.add (local.get $p_a) (i32.const 12)) (f32.mul (f32.neg (local.get $sina)) (local.get $sinb)))
    (f32.store (i32.add (local.get $p_a) (i32.const 16)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 20)) (f32.mul (local.get $cosa) (local.get $sinb)))

    (f32.store (i32.add (local.get $p_a) (i32.const 24)) (f32.neg (local.get $cosa)))
    (f32.store (i32.add (local.get $p_a) (i32.const 28)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 32)) (f32.neg (local.get $sina)))

    (f32.store (local.get $p_b) (f32.mul (f32.neg (local.get $cosa)) (f32.mul (local.get $sinb) (local.get $binv))))
    (f32.store (i32.add (local.get $p_b) (i32.const 4)) (f32.mul (f32.neg (local.get $cosb)) (local.get $binv)))
    (f32.store (i32.add (local.get $p_b) (i32.const 8)) (f32.mul (f32.neg (local.get $sina)) (f32.mul (local.get $sinb) (local.get $binv))))

    (f32.store (i32.add (local.get $p_b) (i32.const 12)) (f32.mul (local.get $cosa) (local.get $cosb)))
    (f32.store (i32.add (local.get $p_b) (i32.const 16)) (f32.neg (local.get $sinb)))
    (f32.store (i32.add (local.get $p_b) (i32.const 20)) (f32.mul (local.get $sina) (local.get $cosb)))

    (f32.store (i32.add (local.get $p_b) (i32.const 24)) (f32.const 0))
    (f32.store (i32.add (local.get $p_b) (i32.const 28)) (f32.const 0))
    (f32.store (i32.add (local.get $p_b) (i32.const 32)) (f32.const 0))
  )

  (func (export "TPFORC")
    (param $pi f32) (param $amach f32) (param $ysym f32) (param $zsym f32)
    (param $iysym i32) (param $izsym i32)
    (param $vrcorec f32) (param $vrcorew f32)
    (param $nstrip i32) (param $numax i32) (param $sref f32) (param $bref f32)
    (param $ijfrst i32) (param $nvstrp i32)
    (param $gam i32) (param $gam_u i32)
    (param $rv1 i32) (param $rv2 i32) (param $rc i32)
    (param $chord i32) (param $lssurf i32) (param $lncomp i32) (param $lfload i32)
    (param $gams i32) (param $gams_u i32)
    (param $rt1 i32) (param $rt2 i32) (param $rtc i32)
    (param $vy_u i32) (param $vz_u i32)
    (param $dwwake i32) (param $clff_u i32) (param $cyff_u i32) (param $cdff_u i32) (param $spanef_u i32)
    (param $out i32)
    (local $hpi f32)
    (local $jc i32) (local $jv i32) (local $i i32) (local $k i32) (local $n i32)
    (local $i1 i32) (local $nvc i32) (local $ic i32) (local $idx i32)
    (local $p0 f32) (local $p1 f32) (local $p2 f32)
    (local $gval f32)
    (local $dxt f32) (local $dyt f32) (local $dzt f32) (local $dst f32)
    (local $ny f32) (local $nz f32) (local $ycntr f32) (local $zcntr f32)
    (local $vy f32) (local $vz f32)
    (local $vyu f32) (local $vzu f32)
    (local $dsy f32) (local $dsz f32) (local $dsyz f32)
    (local $rcore f32)
    (local $dy1 f32) (local $dy2 f32) (local $dz1 f32) (local $dz2 f32)
    (local $rsq1 f32) (local $rsq2 f32)
    (local $clff f32) (local $cyff f32) (local $cdff f32)
    (local $spanef f32) (local $spanef_cl f32) (local $spanef_cy f32) (local $spanef_cd f32)
    (local $ar f32)
    (local $dyz f32)
    (local $tmp f32) (local $tmp2 f32) (local $tmp3 f32)

    (local.set $hpi (f32.div (f32.const 1) (f32.mul (f32.const 2) (local.get $pi))))

    (call $PGMAT (local.get $amach) (f32.const 0) (f32.const 0)
      (i32.const 0) (i32.const 64) (i32.const 128) (i32.const 192))

    (local.set $clff (f32.const 0))
    (local.set $cyff (f32.const 0))
    (local.set $cdff (f32.const 0))

    (local.set $n (i32.const 0))
    (block $nu_clear
      (loop $nu_loop
        (br_if $nu_clear (i32.ge_u (local.get $n) (local.get $numax)))
        (f32.store (i32.add (local.get $clff_u) (i32.mul (local.get $n) (i32.const 4))) (f32.const 0))
        (f32.store (i32.add (local.get $cyff_u) (i32.mul (local.get $n) (i32.const 4))) (f32.const 0))
        (f32.store (i32.add (local.get $cdff_u) (i32.mul (local.get $n) (i32.const 4))) (f32.const 0))
        (f32.store (i32.add (local.get $spanef_u) (i32.mul (local.get $n) (i32.const 4))) (f32.const 0))
        (local.set $n (i32.add (local.get $n) (i32.const 1)))
        (br $nu_loop)
      )
    )

    (local.set $jc (i32.const 0))
    (block $gams_end
      (loop $gams_loop
        (br_if $gams_end (i32.ge_u (local.get $jc) (local.get $nstrip)))
        (f32.store (i32.add (local.get $gams) (i32.mul (local.get $jc) (i32.const 4))) (f32.const 0))
        (local.set $n (i32.const 0))
        (block $gamu_end
          (loop $gamu_loop
            (br_if $gamu_end (i32.ge_u (local.get $n) (local.get $numax)))
            (f32.store (i32.add (local.get $gams_u)
              (i32.mul (call $idx2 (local.get $jc) (local.get $n) (local.get $numax)) (i32.const 4))) (f32.const 0))
            (local.set $n (i32.add (local.get $n) (i32.const 1)))
            (br $gamu_loop)
          )
        )
        (local.set $i1 (i32.load (i32.add (local.get $ijfrst) (i32.mul (local.get $jc) (i32.const 4)))))
        (local.set $nvc (i32.load (i32.add (local.get $nvstrp) (i32.mul (local.get $jc) (i32.const 4)))))
        (local.set $i (i32.const 0))
        (block $sum_end
          (loop $sum_loop
            (br_if $sum_end (i32.ge_u (local.get $i) (local.get $nvc)))
            (local.set $idx (i32.add (local.get $i1) (local.get $i)))
            (f32.store (i32.add (local.get $gams) (i32.mul (local.get $jc) (i32.const 4)))
              (f32.add (f32.load (i32.add (local.get $gams) (i32.mul (local.get $jc) (i32.const 4))))
                       (f32.load (i32.add (local.get $gam) (i32.mul (local.get $idx) (i32.const 4))))))
            (local.set $n (i32.const 0))
            (block $sumu_end
              (loop $sumu_loop
                (br_if $sumu_end (i32.ge_u (local.get $n) (local.get $numax)))
                (local.set $gval (f32.load (i32.add (local.get $gam_u)
                  (i32.mul (call $idx2 (local.get $idx) (local.get $n) (local.get $numax)) (i32.const 4)))))
                (f32.store (i32.add (local.get $gams_u)
                  (i32.mul (call $idx2 (local.get $jc) (local.get $n) (local.get $numax)) (i32.const 4)))
                  (f32.add (f32.load (i32.add (local.get $gams_u)
                    (i32.mul (call $idx2 (local.get $jc) (local.get $n) (local.get $numax)) (i32.const 4))))
                    (local.get $gval)))
                (local.set $n (i32.add (local.get $n) (i32.const 1)))
                (br $sumu_loop)
              )
            )
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $sum_loop)
          )
        )
        (local.set $jc (i32.add (local.get $jc) (i32.const 1)))
        (br $gams_loop)
      )
    )

    (local.set $jc (i32.const 0))
    (block $rt_end
      (loop $rt_loop
        (br_if $rt_end (i32.ge_u (local.get $jc) (local.get $nstrip)))
        (local.set $ic (i32.sub
          (i32.add (i32.load (i32.add (local.get $ijfrst) (i32.mul (local.get $jc) (i32.const 4))))
                   (i32.load (i32.add (local.get $nvstrp) (i32.mul (local.get $jc) (i32.const 4)))))
          (i32.const 1)))
        (local.set $k (i32.const 0))
        (block $k_end
          (loop $k_loop
            (br_if $k_end (i32.ge_u (local.get $k) (i32.const 3)))
            (local.set $p0 (f32.load (i32.add (i32.const 0) (i32.mul (call $idx3 (local.get $k) (i32.const 0)) (i32.const 4)))))
            (local.set $p1 (f32.load (i32.add (i32.const 0) (i32.mul (call $idx3 (local.get $k) (i32.const 1)) (i32.const 4)))))
            (local.set $p2 (f32.load (i32.add (i32.const 0) (i32.mul (call $idx3 (local.get $k) (i32.const 2)) (i32.const 4)))))

            (f32.store (i32.add (local.get $rt1) (i32.mul (call $idx3 (local.get $k) (local.get $jc)) (i32.const 4)))
              (f32.add (f32.add
                (f32.mul (local.get $p0) (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 0) (local.get $ic)) (i32.const 4)))))
                (f32.mul (local.get $p1) (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 1) (local.get $ic)) (i32.const 4))))))
                (f32.mul (local.get $p2) (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 2) (local.get $ic)) (i32.const 4)))))))

            (f32.store (i32.add (local.get $rt2) (i32.mul (call $idx3 (local.get $k) (local.get $jc)) (i32.const 4)))
              (f32.add (f32.add
                (f32.mul (local.get $p0) (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 0) (local.get $ic)) (i32.const 4)))))
                (f32.mul (local.get $p1) (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 1) (local.get $ic)) (i32.const 4))))))
                (f32.mul (local.get $p2) (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 2) (local.get $ic)) (i32.const 4)))))))

            (f32.store (i32.add (local.get $rtc) (i32.mul (call $idx3 (local.get $k) (local.get $jc)) (i32.const 4)))
              (f32.add (f32.add
                (f32.mul (local.get $p0) (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 0) (local.get $ic)) (i32.const 4)))))
                (f32.mul (local.get $p1) (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 1) (local.get $ic)) (i32.const 4))))))
                (f32.mul (local.get $p2) (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 2) (local.get $ic)) (i32.const 4)))))))

            (local.set $k (i32.add (local.get $k) (i32.const 1)))
            (br $k_loop)
          )
        )
        (local.set $jc (i32.add (local.get $jc) (i32.const 1)))
        (br $rt_loop)
      )
    )

    (local.set $jc (i32.const 0))
    (block $main_end
      (loop $main_loop
        (br_if $main_end (i32.ge_u (local.get $jc) (local.get $nstrip)))

        (local.set $dxt (f32.sub (f32.load (i32.add (local.get $rt2) (i32.mul (call $idx3 (i32.const 0) (local.get $jc)) (i32.const 4))))
                                (f32.load (i32.add (local.get $rt1) (i32.mul (call $idx3 (i32.const 0) (local.get $jc)) (i32.const 4))))))
        (local.set $dyt (f32.sub (f32.load (i32.add (local.get $rt2) (i32.mul (call $idx3 (i32.const 1) (local.get $jc)) (i32.const 4))))
                                (f32.load (i32.add (local.get $rt1) (i32.mul (call $idx3 (i32.const 1) (local.get $jc)) (i32.const 4))))))
        (local.set $dzt (f32.sub (f32.load (i32.add (local.get $rt2) (i32.mul (call $idx3 (i32.const 2) (local.get $jc)) (i32.const 4))))
                                (f32.load (i32.add (local.get $rt1) (i32.mul (call $idx3 (i32.const 2) (local.get $jc)) (i32.const 4))))))
        (local.set $dst (f32.sqrt (f32.add (f32.mul (local.get $dyt) (local.get $dyt))
                                           (f32.mul (local.get $dzt) (local.get $dzt)))))
        (local.set $ny (f32.div (f32.neg (local.get $dzt)) (local.get $dst)))
        (local.set $nz (f32.div (local.get $dyt) (local.get $dst)))
        (local.set $ycntr (f32.load (i32.add (local.get $rtc) (i32.mul (call $idx3 (i32.const 1) (local.get $jc)) (i32.const 4)))))
        (local.set $zcntr (f32.load (i32.add (local.get $rtc) (i32.mul (call $idx3 (i32.const 2) (local.get $jc)) (i32.const 4)))))

        (local.set $vy (f32.const 0))
        (local.set $vz (f32.const 0))

        (local.set $n (i32.const 0))
        (block $vyu_clear
          (loop $vyu_loop
            (br_if $vyu_clear (i32.ge_u (local.get $n) (local.get $numax)))
            (f32.store (i32.add (local.get $vy_u) (i32.mul (local.get $n) (i32.const 4))) (f32.const 0))
            (f32.store (i32.add (local.get $vz_u) (i32.mul (local.get $n) (i32.const 4))) (f32.const 0))
            (local.set $n (i32.add (local.get $n) (i32.const 1)))
            (br $vyu_loop)
          )
        )

        (local.set $jv (i32.const 0))
        (block $jv_end
          (loop $jv_loop
            (br_if $jv_end (i32.ge_u (local.get $jv) (local.get $nstrip)))

            (local.set $dsy (f32.sub (f32.load (i32.add (local.get $rt2) (i32.mul (call $idx3 (i32.const 1) (local.get $jv)) (i32.const 4))))
                                     (f32.load (i32.add (local.get $rt1) (i32.mul (call $idx3 (i32.const 1) (local.get $jv)) (i32.const 4))))))
            (local.set $dsz (f32.sub (f32.load (i32.add (local.get $rt2) (i32.mul (call $idx3 (i32.const 2) (local.get $jv)) (i32.const 4))))
                                     (f32.load (i32.add (local.get $rt1) (i32.mul (call $idx3 (i32.const 2) (local.get $jv)) (i32.const 4))))))
            (local.set $dsyz (f32.sqrt (f32.add (f32.mul (local.get $dsy) (local.get $dsy))
                                               (f32.mul (local.get $dsz) (local.get $dsz)))))

            (local.set $rcore (f32.const 0))
            (if (i32.ne
              (i32.load (i32.add (local.get $lncomp) (i32.mul (i32.load (i32.add (local.get $lssurf) (i32.mul (local.get $jc) (i32.const 4)))) (i32.const 4))))
              (i32.load (i32.add (local.get $lncomp) (i32.mul (i32.load (i32.add (local.get $lssurf) (i32.mul (local.get $jv) (i32.const 4)))) (i32.const 4))))
            )
              (then
                (local.set $rcore
                  (f32.max
                    (f32.mul (local.get $vrcorec) (f32.load (i32.add (local.get $chord) (i32.mul (local.get $jv) (i32.const 4)))))
                    (f32.mul (local.get $vrcorew) (local.get $dsyz))
                  )
                )
              )
            )

            (local.set $dy1 (f32.sub (local.get $ycntr) (f32.load (i32.add (local.get $rt1) (i32.mul (call $idx3 (i32.const 1) (local.get $jv)) (i32.const 4))))))
            (local.set $dy2 (f32.sub (local.get $ycntr) (f32.load (i32.add (local.get $rt2) (i32.mul (call $idx3 (i32.const 1) (local.get $jv)) (i32.const 4))))))
            (local.set $dz1 (f32.sub (local.get $zcntr) (f32.load (i32.add (local.get $rt1) (i32.mul (call $idx3 (i32.const 2) (local.get $jv)) (i32.const 4))))))
            (local.set $dz2 (f32.sub (local.get $zcntr) (f32.load (i32.add (local.get $rt2) (i32.mul (call $idx3 (i32.const 2) (local.get $jv)) (i32.const 4))))))

            (local.set $dyz (f32.add (f32.mul (local.get $dy1) (local.get $dy1)) (f32.mul (local.get $dz1) (local.get $dz1))))
            (local.set $rsq1 (f32.sqrt (f32.add (f32.mul (local.get $dyz) (local.get $dyz))
                                               (f32.mul (f32.mul (local.get $rcore) (local.get $rcore))
                                                        (f32.mul (local.get $rcore) (local.get $rcore))))))
            (local.set $dyz (f32.add (f32.mul (local.get $dy2) (local.get $dy2)) (f32.mul (local.get $dz2) (local.get $dz2))))
            (local.set $rsq2 (f32.sqrt (f32.add (f32.mul (local.get $dyz) (local.get $dyz))
                                               (f32.mul (f32.mul (local.get $rcore) (local.get $rcore))
                                                        (f32.mul (local.get $rcore) (local.get $rcore))))))

            (local.set $vy
              (f32.add (local.get $vy)
                (f32.mul (local.get $hpi)
                  (f32.mul (f32.load (i32.add (local.get $gams) (i32.mul (local.get $jv) (i32.const 4))))
                    (f32.sub (f32.div (local.get $dz1) (local.get $rsq1))
                             (f32.div (local.get $dz2) (local.get $rsq2)))))))
            (local.set $vz
              (f32.add (local.get $vz)
                (f32.mul (local.get $hpi)
                  (f32.mul (f32.load (i32.add (local.get $gams) (i32.mul (local.get $jv) (i32.const 4))))
                    (f32.add (f32.neg (f32.div (local.get $dy1) (local.get $rsq1)))
                             (f32.div (local.get $dy2) (local.get $rsq2)))))))

            (local.set $n (i32.const 0))
            (block $vynu_end
              (loop $vynu_loop
                (br_if $vynu_end (i32.ge_u (local.get $n) (local.get $numax)))
                (local.set $vyu (f32.load (i32.add (local.get $vy_u) (i32.mul (local.get $n) (i32.const 4)))))
                (local.set $vzu (f32.load (i32.add (local.get $vz_u) (i32.mul (local.get $n) (i32.const 4)))))
                (local.set $vyu
                  (f32.add (local.get $vyu)
                    (f32.mul (local.get $hpi)
                      (f32.mul (f32.load (i32.add (local.get $gams_u)
                        (i32.mul (call $idx2 (local.get $jv) (local.get $n) (local.get $numax)) (i32.const 4))))
                        (f32.sub (f32.div (local.get $dz1) (local.get $rsq1))
                                 (f32.div (local.get $dz2) (local.get $rsq2)))))))
                (local.set $vzu
                  (f32.add (local.get $vzu)
                    (f32.mul (local.get $hpi)
                      (f32.mul (f32.load (i32.add (local.get $gams_u)
                        (i32.mul (call $idx2 (local.get $jv) (local.get $n) (local.get $numax)) (i32.const 4))))
                        (f32.add (f32.neg (f32.div (local.get $dy1) (local.get $rsq1)))
                                 (f32.div (local.get $dy2) (local.get $rsq2)))))))
                (f32.store (i32.add (local.get $vy_u) (i32.mul (local.get $n) (i32.const 4))) (local.get $vyu))
                (f32.store (i32.add (local.get $vz_u) (i32.mul (local.get $n) (i32.const 4))) (local.get $vzu))
                (local.set $n (i32.add (local.get $n) (i32.const 1)))
                (br $vynu_loop)
              )
            )

            (local.set $jv (i32.add (local.get $jv) (i32.const 1)))
            (br $jv_loop)
          )
        )

        (f32.store (i32.add (local.get $dwwake) (i32.mul (local.get $jc) (i32.const 4)))
          (f32.neg (f32.add (f32.mul (local.get $ny) (local.get $vy)) (f32.mul (local.get $nz) (local.get $vz)))))

        (if (i32.load (i32.add (local.get $lfload)
              (i32.mul (i32.load (i32.add (local.get $lssurf) (i32.mul (local.get $jc) (i32.const 4)))) (i32.const 4))))
          (then
            (local.set $clff (f32.add (local.get $clff)
              (f32.mul (f32.const 2)
                (f32.mul (f32.load (i32.add (local.get $gams) (i32.mul (local.get $jc) (i32.const 4))))
                  (f32.div (local.get $dyt) (local.get $sref))))))
            (local.set $cyff (f32.sub (local.get $cyff)
              (f32.mul (f32.const 2)
                (f32.mul (f32.load (i32.add (local.get $gams) (i32.mul (local.get $jc) (i32.const 4))))
                  (f32.div (local.get $dzt) (local.get $sref))))))
            (local.set $cdff (f32.add (local.get $cdff)
              (f32.mul
                (f32.load (i32.add (local.get $gams) (i32.mul (local.get $jc) (i32.const 4))))
                (f32.div (f32.sub (f32.mul (local.get $dzt) (local.get $vy))
                                 (f32.mul (local.get $dyt) (local.get $vz)))
                         (local.get $sref)))))

            (local.set $n (i32.const 0))
            (block $clu_end
              (loop $clu_loop
                (br_if $clu_end (i32.ge_u (local.get $n) (local.get $numax)))

                (local.set $tmp (f32.load (i32.add (local.get $gams_u)
                  (i32.mul (call $idx2 (local.get $jc) (local.get $n) (local.get $numax)) (i32.const 4)))))

                (local.set $tmp2
                  (f32.add
                    (f32.load (i32.add (local.get $clff_u) (i32.mul (local.get $n) (i32.const 4))))
                    (f32.mul (f32.const 2)
                      (f32.mul (local.get $tmp)
                        (f32.div (local.get $dyt) (local.get $sref)))))
                )
                (f32.store (i32.add (local.get $clff_u) (i32.mul (local.get $n) (i32.const 4))) (local.get $tmp2))

                (local.set $tmp2
                  (f32.sub
                    (f32.load (i32.add (local.get $cyff_u) (i32.mul (local.get $n) (i32.const 4))))
                    (f32.mul (f32.const 2)
                      (f32.mul (local.get $tmp)
                        (f32.div (local.get $dzt) (local.get $sref)))))
                )
                (f32.store (i32.add (local.get $cyff_u) (i32.mul (local.get $n) (i32.const 4))) (local.get $tmp2))

                (local.set $tmp2 (f32.sub (f32.mul (local.get $dzt) (local.get $vy))
                                          (f32.mul (local.get $dyt) (local.get $vz))))
                (local.set $tmp3 (f32.sub
                  (f32.mul (local.get $dzt) (f32.load (i32.add (local.get $vy_u) (i32.mul (local.get $n) (i32.const 4)))))
                  (f32.mul (local.get $dyt) (f32.load (i32.add (local.get $vz_u) (i32.mul (local.get $n) (i32.const 4)))))))
                (local.set $tmp2 (f32.add
                  (f32.mul (local.get $tmp) (local.get $tmp2))
                  (f32.mul (f32.load (i32.add (local.get $gams) (i32.mul (local.get $jc) (i32.const 4)))) (local.get $tmp3))))
                (local.set $tmp2 (f32.div (local.get $tmp2) (local.get $sref)))
                (local.set $tmp3 (f32.load (i32.add (local.get $cdff_u) (i32.mul (local.get $n) (i32.const 4)))))
                (f32.store (i32.add (local.get $cdff_u) (i32.mul (local.get $n) (i32.const 4)))
                  (f32.add (local.get $tmp3) (local.get $tmp2)))

                (local.set $n (i32.add (local.get $n) (i32.const 1)))
                (br $clu_loop)
              )
            )
          )
        )

        (local.set $jc (i32.add (local.get $jc) (i32.const 1)))
        (br $main_loop)
      )
    )

    (local.set $ar (f32.div (f32.mul (local.get $bref) (local.get $bref)) (local.get $sref)))
    (if (f32.eq (local.get $cdff) (f32.const 0))
      (then
        (local.set $spanef (f32.const 0))
      )
      (else
        (local.set $spanef
          (f32.div
            (f32.add (f32.mul (local.get $clff) (local.get $clff))
                     (f32.mul (local.get $cyff) (local.get $cyff)))
            (f32.mul (local.get $pi) (f32.mul (local.get $ar) (local.get $cdff)))))
        (local.set $spanef_cl (f32.div (f32.mul (f32.const 2) (local.get $clff))
                                       (f32.mul (local.get $pi) (f32.mul (local.get $ar) (local.get $cdff)))))
        (local.set $spanef_cy (f32.div (f32.mul (f32.const 2) (local.get $cyff))
                                       (f32.mul (local.get $pi) (f32.mul (local.get $ar) (local.get $cdff)))))
        (local.set $spanef_cd (f32.neg (f32.div (local.get $spanef) (local.get $cdff))))
        (local.set $n (i32.const 0))
        (block $spanu_end
          (loop $spanu_loop
            (br_if $spanu_end (i32.ge_u (local.get $n) (local.get $numax)))
            (f32.store (i32.add (local.get $spanef_u) (i32.mul (local.get $n) (i32.const 4)))
              (f32.add
                (f32.add
                  (f32.mul (local.get $spanef_cl) (f32.load (i32.add (local.get $clff_u) (i32.mul (local.get $n) (i32.const 4)))))
                  (f32.mul (local.get $spanef_cy) (f32.load (i32.add (local.get $cyff_u) (i32.mul (local.get $n) (i32.const 4)))))
                )
                (f32.mul (local.get $spanef_cd) (f32.load (i32.add (local.get $cdff_u) (i32.mul (local.get $n) (i32.const 4)))))
              )
            )
            (local.set $n (i32.add (local.get $n) (i32.const 1)))
            (br $spanu_loop)
          )
        )
      )
    )

    (f32.store (local.get $out) (local.get $clff))
    (f32.store (i32.add (local.get $out) (i32.const 4)) (local.get $cyff))
    (f32.store (i32.add (local.get $out) (i32.const 8)) (local.get $cdff))
    (f32.store (i32.add (local.get $out) (i32.const 12)) (local.get $spanef))
  )
)
