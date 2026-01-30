(module
  (import "env" "sin_f32" (func $sin_f32 (param f32) (result f32)))
  (import "env" "cos_f32" (func $cos_f32 (param f32) (result f32)))
  (import "env" "log_f32" (func $log_f32 (param f32) (result f32)))
  (import "env" "pow_f32" (func $pow_f32 (param f32) (param f32) (result f32)))

  (memory (export "memory") 1)

  (func $load_f32_at (param $base i32) (param $idx i32) (result f32)
    (f32.load
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))))

  (func $store_f32_at (param $base i32) (param $idx i32) (param $val f32)
    (f32.store
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))
      (local.get $val)))

  (func $load_i32_at (param $base i32) (param $idx i32) (result i32)
    (i32.load
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))))

  (func $store_i32_at (param $base i32) (param $idx i32) (param $val i32)
    (i32.store
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))
      (local.get $val)))

  (func $ifloor (param $x f32) (result i32)
    (local $i i32)
    (local.set $i (i32.trunc_f32_s (local.get $x)))
    (if (f32.lt (f32.sub (local.get $x) (f32.convert_i32_s (local.get $i))) (f32.const 0))
      (then (local.set $i (i32.sub (local.get $i) (i32.const 1)))))
    (local.get $i))

  (func $iceiling (param $x f32) (result i32)
    (local $i i32)
    (local.set $i (i32.trunc_f32_s (local.get $x)))
    (if (f32.gt (f32.sub (local.get $x) (f32.convert_i32_s (local.get $i))) (f32.const 0))
      (then (local.set $i (i32.add (local.get $i) (i32.const 1)))))
    (local.get $i))

  (func $xinctbl (param $i i32) (result f32)
    (if (i32.eq (local.get $i) (i32.const 0)) (then (return (f32.const 0.1))))
    (if (i32.eq (local.get $i) (i32.const 1)) (then (return (f32.const 0.2))))
    (if (i32.eq (local.get $i) (i32.const 2)) (then (return (f32.const 0.25))))
    (if (i32.eq (local.get $i) (i32.const 3)) (then (return (f32.const 0.5))))
    (f32.const 1)
  )

  (func $AXISADJ (param $xmin_ptr i32) (param $xmax_ptr i32) (param $xspan_ptr i32)
                (param $deltax_ptr i32) (param $ntics_ptr i32)
    (local $xmin f32)
    (local $xmax f32)
    (local $xspan1 f32)
    (local $xpon f32)
    (local $xpon_i i32)
    (local $xspan f32)
    (local $xinc f32)
    (local $ntics i32)
    (local $deltax f32)
    (local $i i32)
    (local $tmp f32)

    (local.set $xmin (f32.load (local.get $xmin_ptr)))
    (local.set $xmax (f32.load (local.get $xmax_ptr)))

    (local.set $xspan1 (f32.sub (local.get $xmax) (local.get $xmin)))
    (if (f32.eq (local.get $xspan1) (f32.const 0))
      (then (local.set $xspan1 (f32.const 1))))

    (local.set $xpon (call $log_f32 (local.get $xspan1)))
    (local.set $xpon (f32.mul (local.get $xpon) (f32.const 0.43429448)))
    (local.set $xpon_i (i32.trunc_f32_s (local.get $xpon)))

    (local.set $tmp (call $pow_f32 (f32.const 10) (f32.convert_i32_s (local.get $xpon_i))))
    (local.set $xspan (f32.div (local.get $xspan1) (local.get $tmp)))

    (local.set $i (i32.const 0))
    (local.set $xinc (f32.const 0.1))
    (local.set $ntics (i32.const 1))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (i32.const 5)))
        (local.set $xinc (call $xinctbl (local.get $i)))
        (local.set $tmp (f32.add (f32.div (local.get $xspan) (local.get $xinc)) (f32.const 0.1)))
        (local.set $ntics (i32.add (i32.const 1) (i32.trunc_f32_s (local.get $tmp))))
        (br_if $done (i32.le_s (local.get $ntics) (i32.const 6)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )

    (local.set $deltax (f32.mul (local.get $xinc) (call $pow_f32 (f32.const 10) (f32.convert_i32_s (local.get $xpon_i)))))

    (local.set $tmp (f32.div (local.get $xmin) (local.get $deltax)))
    (local.set $xmin (f32.mul (local.get $deltax) (f32.convert_i32_s (call $ifloor (local.get $tmp)))))

    (local.set $tmp (f32.div (local.get $xmax) (local.get $deltax)))
    (local.set $xmax (f32.mul (local.get $deltax) (f32.convert_i32_s (call $iceiling (local.get $tmp)))))

    (local.set $xspan (f32.sub (local.get $xmax) (local.get $xmin)))
    (local.set $tmp (f32.add (f32.div (local.get $xspan) (local.get $deltax)) (f32.const 0.1)))
    (local.set $ntics (i32.add (i32.const 1) (i32.trunc_f32_s (local.get $tmp))))

    (f32.store (local.get $xmin_ptr) (local.get $xmin))
    (f32.store (local.get $xmax_ptr) (local.get $xmax))
    (f32.store (local.get $xspan_ptr) (local.get $xspan))
    (f32.store (local.get $deltax_ptr) (local.get $deltax))
    (i32.store (local.get $ntics_ptr) (local.get $ntics))
  )

  (func $VIEWPROJ (param $xyz_ptr i32) (param $n i32) (param $view_ptr i32)
    (local $i i32)
    (local $x f32) (local $y f32) (local $z f32)
    (local $rdoti f32) (local $rdotj f32) (local $rdotk f32)
    (local $xihat f32) (local $yihat f32) (local $zihat f32)
    (local $xjhat f32) (local $yjhat f32) (local $zjhat f32)
    (local $xkhat f32) (local $ykhat f32) (local $zkhat f32)
    (local $rinv f32)
    (local $rkx f32) (local $rky f32) (local $rkz f32)
    (local $vscal f32) (local $tmp f32)

    (local.set $rinv (call $load_f32_at (local.get $view_ptr) (i32.const 0)))
    (local.set $xihat (call $load_f32_at (local.get $view_ptr) (i32.const 1)))
    (local.set $yihat (call $load_f32_at (local.get $view_ptr) (i32.const 2)))
    (local.set $zihat (call $load_f32_at (local.get $view_ptr) (i32.const 3)))
    (local.set $xjhat (call $load_f32_at (local.get $view_ptr) (i32.const 4)))
    (local.set $yjhat (call $load_f32_at (local.get $view_ptr) (i32.const 5)))
    (local.set $zjhat (call $load_f32_at (local.get $view_ptr) (i32.const 6)))
    (local.set $xkhat (call $load_f32_at (local.get $view_ptr) (i32.const 7)))
    (local.set $ykhat (call $load_f32_at (local.get $view_ptr) (i32.const 8)))
    (local.set $zkhat (call $load_f32_at (local.get $view_ptr) (i32.const 9)))

    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))

        (local.set $x (call $load_f32_at (local.get $xyz_ptr) (i32.add (i32.mul (local.get $i) (i32.const 3)) (i32.const 0))))
        (local.set $y (call $load_f32_at (local.get $xyz_ptr) (i32.add (i32.mul (local.get $i) (i32.const 3)) (i32.const 1))))
        (local.set $z (call $load_f32_at (local.get $xyz_ptr) (i32.add (i32.mul (local.get $i) (i32.const 3)) (i32.const 2))))

        (local.set $rdoti
          (f32.add (f32.add (f32.mul (local.get $x) (local.get $xihat))
                             (f32.mul (local.get $y) (local.get $yihat)))
                   (f32.mul (local.get $z) (local.get $zihat))))
        (local.set $rdotj
          (f32.add (f32.add (f32.mul (local.get $x) (local.get $xjhat))
                             (f32.mul (local.get $y) (local.get $yjhat)))
                   (f32.mul (local.get $z) (local.get $zjhat))))
        (local.set $rdotk
          (f32.add (f32.add (f32.mul (local.get $x) (local.get $xkhat))
                             (f32.mul (local.get $y) (local.get $ykhat)))
                   (f32.mul (local.get $z) (local.get $zkhat))))

        (local.set $rkx (f32.mul (local.get $rdotk) (local.get $xkhat)))
        (local.set $rky (f32.mul (local.get $rdotk) (local.get $ykhat)))
        (local.set $rkz (f32.mul (local.get $rdotk) (local.get $zkhat)))

        (local.set $tmp
          (f32.add
            (f32.add
              (f32.mul (f32.sub (local.get $xkhat) (f32.mul (local.get $rinv) (local.get $rkx)))
                       (f32.sub (local.get $xkhat) (f32.mul (local.get $rinv) (local.get $rkx))))
              (f32.mul (f32.sub (local.get $ykhat) (f32.mul (local.get $rinv) (local.get $rky)))
                       (f32.sub (local.get $ykhat) (f32.mul (local.get $rinv) (local.get $rky)))))
            (f32.mul (f32.sub (local.get $zkhat) (f32.mul (local.get $rinv) (local.get $rkz)))
                     (f32.sub (local.get $zkhat) (f32.mul (local.get $rinv) (local.get $rkz))))))

        (local.set $vscal (f32.div (f32.const 1) (f32.sqrt (local.get $tmp))))

        (call $store_f32_at (local.get $xyz_ptr)
          (i32.add (i32.mul (local.get $i) (i32.const 3)) (i32.const 0))
          (f32.mul (local.get $vscal) (local.get $rdoti)))
        (call $store_f32_at (local.get $xyz_ptr)
          (i32.add (i32.mul (local.get $i) (i32.const 3)) (i32.const 1))
          (f32.mul (local.get $vscal) (local.get $rdotj)))
        (call $store_f32_at (local.get $xyz_ptr)
          (i32.add (i32.mul (local.get $i) (i32.const 3)) (i32.const 2))
          (f32.mul (local.get $vscal) (local.get $rdotk)))

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func $TETRAN (param $r_ptr i32) (param $tt_ptr i32) (param $rref_ptr i32) (param $dr_ptr i32)
    (local $rb1 f32) (local $rb2 f32) (local $rb3 f32)
    (local $r1 f32) (local $r2 f32) (local $r3 f32)

    (local.set $rb1 (f32.sub (call $load_f32_at (local.get $r_ptr) (i32.const 0))
                             (call $load_f32_at (local.get $rref_ptr) (i32.const 0))))
    (local.set $rb2 (f32.sub (call $load_f32_at (local.get $r_ptr) (i32.const 1))
                             (call $load_f32_at (local.get $rref_ptr) (i32.const 1))))
    (local.set $rb3 (f32.sub (call $load_f32_at (local.get $r_ptr) (i32.const 2))
                             (call $load_f32_at (local.get $rref_ptr) (i32.const 2))))

    (local.set $r1
      (f32.add
        (f32.add
          (f32.add
            (f32.mul (call $load_f32_at (local.get $tt_ptr) (i32.const 0)) (local.get $rb1))
            (f32.mul (call $load_f32_at (local.get $tt_ptr) (i32.const 3)) (local.get $rb2)))
          (f32.mul (call $load_f32_at (local.get $tt_ptr) (i32.const 6)) (local.get $rb3)))
        (f32.add (call $load_f32_at (local.get $rref_ptr) (i32.const 0))
                 (call $load_f32_at (local.get $dr_ptr) (i32.const 0)))))

    (local.set $r2
      (f32.add
        (f32.add
          (f32.add
            (f32.mul (call $load_f32_at (local.get $tt_ptr) (i32.const 1)) (local.get $rb1))
            (f32.mul (call $load_f32_at (local.get $tt_ptr) (i32.const 4)) (local.get $rb2)))
          (f32.mul (call $load_f32_at (local.get $tt_ptr) (i32.const 7)) (local.get $rb3)))
        (f32.add (call $load_f32_at (local.get $rref_ptr) (i32.const 1))
                 (call $load_f32_at (local.get $dr_ptr) (i32.const 1)))))

    (local.set $r3
      (f32.add
        (f32.add
          (f32.add
            (f32.mul (call $load_f32_at (local.get $tt_ptr) (i32.const 2)) (local.get $rb1))
            (f32.mul (call $load_f32_at (local.get $tt_ptr) (i32.const 5)) (local.get $rb2)))
          (f32.mul (call $load_f32_at (local.get $tt_ptr) (i32.const 8)) (local.get $rb3)))
        (f32.add (call $load_f32_at (local.get $rref_ptr) (i32.const 2))
                 (call $load_f32_at (local.get $dr_ptr) (i32.const 2)))))

    (call $store_f32_at (local.get $r_ptr) (i32.const 0) (local.get $r1))
    (call $store_f32_at (local.get $r_ptr) (i32.const 1) (local.get $r2))
    (call $store_f32_at (local.get $r_ptr) (i32.const 2) (local.get $r3))
  )

  (func $ROTENS3 (param $ang_ptr i32) (param $tt_ptr i32) (param $ttang_ptr i32)
    (local $a1 f32) (local $a2 f32) (local $a3 f32)
    (local $c1 f32) (local $c2 f32) (local $c3 f32)
    (local $s1 f32) (local $s2 f32) (local $s3 f32)
    (local $i i32)

    (local.set $a1 (call $load_f32_at (local.get $ang_ptr) (i32.const 0)))
    (local.set $a2 (call $load_f32_at (local.get $ang_ptr) (i32.const 1)))
    (local.set $a3 (call $load_f32_at (local.get $ang_ptr) (i32.const 2)))

    (local.set $c1 (call $cos_f32 (local.get $a1)))
    (local.set $c2 (call $cos_f32 (local.get $a2)))
    (local.set $c3 (call $cos_f32 (local.get $a3)))

    (local.set $s1 (call $sin_f32 (local.get $a1)))
    (local.set $s2 (call $sin_f32 (local.get $a2)))
    (local.set $s3 (call $sin_f32 (local.get $a3)))

    (call $store_f32_at (local.get $tt_ptr) (i32.const 0) (f32.mul (local.get $c2) (local.get $c3)))
    (call $store_f32_at (local.get $tt_ptr) (i32.const 1) (f32.mul (f32.const -1) (f32.mul (local.get $c2) (local.get $s3))))
    (call $store_f32_at (local.get $tt_ptr) (i32.const 2) (f32.mul (f32.const -1) (local.get $s2)))

    (call $store_f32_at (local.get $tt_ptr) (i32.const 3)
      (f32.add (f32.mul (f32.mul (f32.const -1) (local.get $s1)) (f32.mul (local.get $s2) (local.get $c3)))
               (f32.mul (local.get $c1) (local.get $s3))))
    (call $store_f32_at (local.get $tt_ptr) (i32.const 4)
      (f32.add (f32.mul (local.get $s1) (f32.mul (local.get $s2) (local.get $s3)))
               (f32.mul (local.get $c1) (local.get $c3))))
    (call $store_f32_at (local.get $tt_ptr) (i32.const 5)
      (f32.mul (f32.const -1) (f32.mul (local.get $s1) (local.get $c2))))

    (call $store_f32_at (local.get $tt_ptr) (i32.const 6)
      (f32.add (f32.mul (local.get $c1) (f32.mul (local.get $s2) (local.get $c3)))
               (f32.mul (local.get $s1) (local.get $s3))))
    (call $store_f32_at (local.get $tt_ptr) (i32.const 7)
      (f32.add (f32.mul (f32.const -1) (f32.mul (local.get $c1) (f32.mul (local.get $s2) (local.get $s3))))
               (f32.mul (local.get $s1) (local.get $c3))))
    (call $store_f32_at (local.get $tt_ptr) (i32.const 8) (f32.mul (local.get $c1) (local.get $c2)))

    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (i32.const 27)))
        (call $store_f32_at (local.get $ttang_ptr) (local.get $i) (f32.const 0))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func $TRIINIT (param $id i32) (param $nrows i32) (param $ncols i32) (param $pts_ptr i32)
                 (param $ntri_ptr i32) (param $tri_ptr i32)
    (local $it i32) (local $idx i32)
    (local $j i32) (local $k i32)
    (local $ip1 i32) (local $ip2 i32) (local $ip3 i32) (local $ip4 i32)
    (local $p1 i32) (local $p2 i32) (local $p3 i32) (local $p4 i32)
    (local $tcol i32)
    (local $v1 f32) (local $v2 f32) (local $v3 f32)

    (local.set $it (i32.load (local.get $ntri_ptr)))
    (local.set $idx (i32.sub (local.get $id) (i32.const 1)))

    (local.set $j (i32.const 1))
    (block $j_done
      (loop $j_loop
        (br_if $j_done (i32.gt_s (local.get $j) (local.get $ncols)))
        (local.set $k (i32.const 1))
        (block $k_done
          (loop $k_loop
            (br_if $k_done (i32.gt_s (local.get $k) (local.get $nrows)))
            (local.set $idx (i32.add (local.get $idx) (i32.const 1)))

            (local.set $ip1 (i32.add (i32.mul (i32.add (local.get $nrows) (i32.const 1))
                                              (i32.sub (local.get $j) (i32.const 1)))
                                     (local.get $k)))
            (local.set $ip2 (i32.add (local.get $ip1) (i32.const 1)))
            (local.set $ip3 (i32.add (local.get $ip1) (i32.add (local.get $nrows) (i32.const 1))))
            (local.set $ip4 (i32.add (local.get $ip3) (i32.const 1)))

            (local.set $p1 (i32.sub (local.get $ip1) (i32.const 1)))
            (local.set $p2 (i32.sub (local.get $ip2) (i32.const 1)))
            (local.set $p3 (i32.sub (local.get $ip3) (i32.const 1)))
            (local.set $p4 (i32.sub (local.get $ip4) (i32.const 1)))

            ;; first triangle
            (local.set $it (i32.add (local.get $it) (i32.const 1)))
            (local.set $tcol (i32.sub (local.get $it) (i32.const 1)))

            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 0))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p1) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 1))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p1) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 2))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p1) (i32.const 3)) (i32.const 2))))

            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 3))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p2) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 4))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p2) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 5))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p2) (i32.const 3)) (i32.const 2))))

            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 6))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p3) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 7))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p3) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 8))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p3) (i32.const 3)) (i32.const 2))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 0))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 3))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 6))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 9))
              (f32.min (local.get $v1) (f32.min (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 1))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 4))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 7))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 10))
              (f32.min (local.get $v1) (f32.min (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 2))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 5))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 8))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 11))
              (f32.min (local.get $v1) (f32.min (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 0))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 3))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 6))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 12))
              (f32.max (local.get $v1) (f32.max (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 1))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 4))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 7))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 13))
              (f32.max (local.get $v1) (f32.max (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 2))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 5))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 8))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 14))
              (f32.max (local.get $v1) (f32.max (local.get $v2) (local.get $v3))))

            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 15))
              (f32.convert_i32_s (local.get $idx)))

            ;; second triangle
            (local.set $it (i32.add (local.get $it) (i32.const 1)))
            (local.set $tcol (i32.sub (local.get $it) (i32.const 1)))

            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 0))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p3) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 1))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p3) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 2))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p3) (i32.const 3)) (i32.const 2))))

            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 3))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p2) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 4))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p2) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 5))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p2) (i32.const 3)) (i32.const 2))))

            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 6))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p4) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 7))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p4) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 8))
              (call $load_f32_at (local.get $pts_ptr) (i32.add (i32.mul (local.get $p4) (i32.const 3)) (i32.const 2))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 0))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 3))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 6))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 9))
              (f32.min (local.get $v1) (f32.min (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 1))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 4))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 7))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 10))
              (f32.min (local.get $v1) (f32.min (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 2))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 5))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 8))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 11))
              (f32.min (local.get $v1) (f32.min (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 0))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 3))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 6))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 12))
              (f32.max (local.get $v1) (f32.max (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 1))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 4))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 7))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 13))
              (f32.max (local.get $v1) (f32.max (local.get $v2) (local.get $v3))))

            (local.set $v1 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 2))))
            (local.set $v2 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 5))))
            (local.set $v3 (call $load_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 8))))
            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 14))
              (f32.max (local.get $v1) (f32.max (local.get $v2) (local.get $v3))))

            (call $store_f32_at (local.get $tri_ptr) (i32.add (i32.mul (local.get $tcol) (i32.const 16)) (i32.const 15))
              (f32.convert_i32_s (local.get $idx)))

            (local.set $k (i32.add (local.get $k) (i32.const 1)))
            (br $k_loop)
          )
        )
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $j_loop)
      )
    )

    (i32.store (local.get $ntri_ptr) (local.get $it))
  )

  (func (export "GLIMS")
    (param $lproj i32)
    (param $nsurf i32) (param $nbody i32) (param $nob i32) (param $lobplt i32)
    (param $lpltsurf_ptr i32) (param $jfrst_ptr i32) (param $nj_ptr i32)
    (param $rle1_ptr i32) (param $chord1_ptr i32)
    (param $lpltbody_ptr i32) (param $lfrst_ptr i32) (param $nl_ptr i32) (param $rl_ptr i32)
    (param $rob_ptr i32) (param $view_ptr i32) (param $pts_ptr i32)
    (param $xyzmin_ptr i32) (param $xyzmax_ptr i32)
    (local $k i32) (local $n i32) (local $j i32)
    (local $j1 i32) (local $jn i32)
    (local $l1 i32) (local $ln i32)
    (local $v1 f32) (local $v2 f32)

    (call $store_f32_at (local.get $xyzmin_ptr) (i32.const 0) (f32.const 1e20))
    (call $store_f32_at (local.get $xyzmin_ptr) (i32.const 1) (f32.const 1e20))
    (call $store_f32_at (local.get $xyzmin_ptr) (i32.const 2) (f32.const 1e20))
    (call $store_f32_at (local.get $xyzmax_ptr) (i32.const 0) (f32.const -1e20))
    (call $store_f32_at (local.get $xyzmax_ptr) (i32.const 1) (f32.const -1e20))
    (call $store_f32_at (local.get $xyzmax_ptr) (i32.const 2) (f32.const -1e20))

    (local.set $n (i32.const 0))
    (block $surf_done
      (loop $surf_loop
        (br_if $surf_done (i32.ge_s (local.get $n) (local.get $nsurf)))
        (if (i32.ne (call $load_i32_at (local.get $lpltsurf_ptr) (local.get $n)) (i32.const 0))
          (then
            (local.set $j1 (call $load_i32_at (local.get $jfrst_ptr) (local.get $n)))
            (local.set $jn (i32.sub (i32.add (call $load_i32_at (local.get $jfrst_ptr) (local.get $n))
                                              (call $load_i32_at (local.get $nj_ptr) (local.get $n)))
                                     (i32.const 1)))
            (local.set $j (local.get $j1))
            (block $j_done
              (loop $j_loop
                (br_if $j_done (i32.gt_s (local.get $j) (local.get $jn)))
                ;; point 1
                (call $store_f32_at (local.get $pts_ptr) (i32.const 0)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 1)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 2)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))
                ;; point 2
                (call $store_f32_at (local.get $pts_ptr) (i32.const 3)
                  (f32.add
                    (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0)))
                    (call $load_f32_at (local.get $chord1_ptr) (local.get $j))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 4)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 5)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))

                (if (i32.ne (local.get $lproj) (i32.const 0))
                  (then (call $VIEWPROJ (local.get $pts_ptr) (i32.const 2) (local.get $view_ptr))))

                (local.set $k (i32.const 0))
                (block $k_done
                  (loop $k_loop
                    (br_if $k_done (i32.ge_s (local.get $k) (i32.const 3)))
                    (local.set $v1 (call $load_f32_at (local.get $pts_ptr) (local.get $k)))
                    (local.set $v2 (call $load_f32_at (local.get $pts_ptr) (i32.add (local.get $k) (i32.const 3))))

                    (call $store_f32_at (local.get $xyzmin_ptr) (local.get $k)
                      (f32.min (call $load_f32_at (local.get $xyzmin_ptr) (local.get $k))
                               (f32.min (local.get $v1) (local.get $v2))))
                    (call $store_f32_at (local.get $xyzmax_ptr) (local.get $k)
                      (f32.max (call $load_f32_at (local.get $xyzmax_ptr) (local.get $k))
                               (f32.max (local.get $v1) (local.get $v2))))

                    (local.set $k (i32.add (local.get $k) (i32.const 1)))
                    (br $k_loop)
                  )
                )

                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
            )
          )
        )

        (local.set $n (i32.add (local.get $n) (i32.const 1)))
        (br $surf_loop)
      )
    )

    (local.set $n (i32.const 0))
    (block $body_done
      (loop $body_loop
        (br_if $body_done (i32.ge_s (local.get $n) (local.get $nbody)))
        (if (i32.ne (call $load_i32_at (local.get $lpltbody_ptr) (local.get $n)) (i32.const 0))
          (then
            (local.set $l1 (call $load_i32_at (local.get $lfrst_ptr) (local.get $n)))
            (local.set $ln (i32.sub (i32.add (call $load_i32_at (local.get $lfrst_ptr) (local.get $n))
                                              (call $load_i32_at (local.get $nl_ptr) (local.get $n)))
                                     (i32.const 1)))

            (call $store_f32_at (local.get $pts_ptr) (i32.const 0)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $l1) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 1)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $l1) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 2)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $l1) (i32.const 3)) (i32.const 2))))

            (call $store_f32_at (local.get $pts_ptr) (i32.const 3)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $ln) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 4)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $ln) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 5)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $ln) (i32.const 3)) (i32.const 2))))

            (if (i32.ne (local.get $lproj) (i32.const 0))
              (then (call $VIEWPROJ (local.get $pts_ptr) (i32.const 2) (local.get $view_ptr))))

            (local.set $k (i32.const 0))
            (block $k_done2
              (loop $k_loop2
                (br_if $k_done2 (i32.ge_s (local.get $k) (i32.const 3)))
                (local.set $v1 (call $load_f32_at (local.get $pts_ptr) (local.get $k)))
                (local.set $v2 (call $load_f32_at (local.get $pts_ptr) (i32.add (local.get $k) (i32.const 3))))

                (call $store_f32_at (local.get $xyzmin_ptr) (local.get $k)
                  (f32.min (call $load_f32_at (local.get $xyzmin_ptr) (local.get $k))
                           (f32.min (local.get $v1) (local.get $v2))))
                (call $store_f32_at (local.get $xyzmax_ptr) (local.get $k)
                  (f32.max (call $load_f32_at (local.get $xyzmax_ptr) (local.get $k))
                           (f32.max (local.get $v1) (local.get $v2))))

                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k_loop2)
              )
            )
          )
        )

        (local.set $n (i32.add (local.get $n) (i32.const 1)))
        (br $body_loop)
      )
    )

    (if (i32.ne (local.get $lobplt) (i32.const 0))
      (then
        (local.set $n (i32.const 0))
        (block $ob_done
          (loop $ob_loop
            (br_if $ob_done (i32.ge_s (local.get $n) (local.get $nob)))

            (call $store_f32_at (local.get $pts_ptr) (i32.const 0)
              (call $load_f32_at (local.get $rob_ptr) (i32.add (i32.mul (local.get $l1) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 1)
              (call $load_f32_at (local.get $rob_ptr) (i32.add (i32.mul (local.get $l1) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 2)
              (call $load_f32_at (local.get $rob_ptr) (i32.add (i32.mul (local.get $l1) (i32.const 3)) (i32.const 2))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 3)
              (call $load_f32_at (local.get $rob_ptr) (i32.add (i32.mul (local.get $ln) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 4)
              (call $load_f32_at (local.get $rob_ptr) (i32.add (i32.mul (local.get $ln) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 5)
              (call $load_f32_at (local.get $rob_ptr) (i32.add (i32.mul (local.get $ln) (i32.const 3)) (i32.const 2))))

            (if (i32.ne (local.get $lproj) (i32.const 0))
              (then (call $VIEWPROJ (local.get $pts_ptr) (i32.const 2) (local.get $view_ptr))))

            (local.set $k (i32.const 0))
            (block $k_done3
              (loop $k_loop3
                (br_if $k_done3 (i32.ge_s (local.get $k) (i32.const 3)))
                (local.set $v1 (call $load_f32_at (local.get $pts_ptr) (local.get $k)))
                (local.set $v2 (call $load_f32_at (local.get $pts_ptr) (i32.add (local.get $k) (i32.const 3))))
                (call $store_f32_at (local.get $xyzmin_ptr) (local.get $k)
                  (f32.min (call $load_f32_at (local.get $xyzmin_ptr) (local.get $k))
                           (f32.min (local.get $v1) (local.get $v2))))
                (call $store_f32_at (local.get $xyzmax_ptr) (local.get $k)
                  (f32.max (call $load_f32_at (local.get $xyzmax_ptr) (local.get $k))
                           (f32.max (local.get $v1) (local.get $v2))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k_loop3)
              )
            )

            (local.set $n (i32.add (local.get $n) (i32.const 1)))
            (br $ob_loop)
          )
        )
      )
    )
  )

  (func (export "GRLIMS")
    (param $lproj i32)
    (param $nsurf i32) (param $nbody i32)
    (param $lpltsurf_ptr i32) (param $jfrst_ptr i32) (param $nj_ptr i32)
    (param $rle1_ptr i32) (param $chord1_ptr i32)
    (param $rle2_ptr i32) (param $chord2_ptr i32)
    (param $lpltbody_ptr i32) (param $lfrst_ptr i32) (param $nl_ptr i32) (param $rl_ptr i32)
    (param $view_ptr i32) (param $pts_ptr i32)
    (param $tt_ptr i32) (param $xyzr_ptr i32) (param $dxyz_ptr i32)
    (param $xyzmin_ptr i32) (param $xyzmax_ptr i32)
    (local $k i32) (local $n i32) (local $j1 i32) (local $jn i32)
    (local $l1 i32) (local $ln i32) (local $v1 f32) (local $v2 f32)

    (call $store_f32_at (local.get $xyzmin_ptr) (i32.const 0) (f32.const 1e20))
    (call $store_f32_at (local.get $xyzmin_ptr) (i32.const 1) (f32.const 1e20))
    (call $store_f32_at (local.get $xyzmin_ptr) (i32.const 2) (f32.const 1e20))
    (call $store_f32_at (local.get $xyzmax_ptr) (i32.const 0) (f32.const -1e20))
    (call $store_f32_at (local.get $xyzmax_ptr) (i32.const 1) (f32.const -1e20))
    (call $store_f32_at (local.get $xyzmax_ptr) (i32.const 2) (f32.const -1e20))

    (local.set $n (i32.const 0))
    (block $surf_done
      (loop $surf_loop
        (br_if $surf_done (i32.ge_s (local.get $n) (local.get $nsurf)))
        (if (i32.ne (call $load_i32_at (local.get $lpltsurf_ptr) (local.get $n)) (i32.const 0))
          (then
            (local.set $j1 (call $load_i32_at (local.get $jfrst_ptr) (local.get $n)))
            (local.set $jn (i32.sub (i32.add (call $load_i32_at (local.get $jfrst_ptr) (local.get $n))
                                              (call $load_i32_at (local.get $nj_ptr) (local.get $n)))
                                     (i32.const 1)))

            ;; RLE1 at J1
            (call $store_f32_at (local.get $pts_ptr) (i32.const 0)
              (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j1) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 1)
              (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j1) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 2)
              (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j1) (i32.const 3)) (i32.const 2))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 3)
              (f32.add
                (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j1) (i32.const 3)) (i32.const 0)))
                (call $load_f32_at (local.get $chord1_ptr) (local.get $j1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 4)
              (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j1) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 5)
              (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j1) (i32.const 3)) (i32.const 2))))

            (call $TETRAN (local.get $pts_ptr) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $dxyz_ptr))
            (call $TETRAN (i32.add (local.get $pts_ptr) (i32.const 12)) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $dxyz_ptr))

            (if (i32.ne (local.get $lproj) (i32.const 0))
              (then (call $VIEWPROJ (local.get $pts_ptr) (i32.const 2) (local.get $view_ptr))))

            (local.set $k (i32.const 0))
            (block $k_done1
              (loop $k_loop1
                (br_if $k_done1 (i32.ge_s (local.get $k) (i32.const 3)))
                (local.set $v1 (call $load_f32_at (local.get $pts_ptr) (local.get $k)))
                (local.set $v2 (call $load_f32_at (local.get $pts_ptr) (i32.add (local.get $k) (i32.const 3))))
                (call $store_f32_at (local.get $xyzmin_ptr) (local.get $k)
                  (f32.min (call $load_f32_at (local.get $xyzmin_ptr) (local.get $k))
                           (f32.min (local.get $v1) (local.get $v2))))
                (call $store_f32_at (local.get $xyzmax_ptr) (local.get $k)
                  (f32.max (call $load_f32_at (local.get $xyzmax_ptr) (local.get $k))
                           (f32.max (local.get $v1) (local.get $v2))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k_loop1)
              )
            )

            ;; RLE2 at JN
            (call $store_f32_at (local.get $pts_ptr) (i32.const 0)
              (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $jn) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 1)
              (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $jn) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 2)
              (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $jn) (i32.const 3)) (i32.const 2))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 3)
              (f32.add
                (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $jn) (i32.const 3)) (i32.const 0)))
                (call $load_f32_at (local.get $chord2_ptr) (local.get $jn))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 4)
              (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $jn) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 5)
              (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $jn) (i32.const 3)) (i32.const 2))))

            (call $TETRAN (local.get $pts_ptr) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $dxyz_ptr))
            (call $TETRAN (i32.add (local.get $pts_ptr) (i32.const 12)) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $dxyz_ptr))

            (if (i32.ne (local.get $lproj) (i32.const 0))
              (then (call $VIEWPROJ (local.get $pts_ptr) (i32.const 2) (local.get $view_ptr))))

            (local.set $k (i32.const 0))
            (block $k_done2
              (loop $k_loop2
                (br_if $k_done2 (i32.ge_s (local.get $k) (i32.const 3)))
                (local.set $v1 (call $load_f32_at (local.get $pts_ptr) (local.get $k)))
                (local.set $v2 (call $load_f32_at (local.get $pts_ptr) (i32.add (local.get $k) (i32.const 3))))
                (call $store_f32_at (local.get $xyzmin_ptr) (local.get $k)
                  (f32.min (call $load_f32_at (local.get $xyzmin_ptr) (local.get $k))
                           (f32.min (local.get $v1) (local.get $v2))))
                (call $store_f32_at (local.get $xyzmax_ptr) (local.get $k)
                  (f32.max (call $load_f32_at (local.get $xyzmax_ptr) (local.get $k))
                           (f32.max (local.get $v1) (local.get $v2))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k_loop2)
              )
            )
          )
        )

        (local.set $n (i32.add (local.get $n) (i32.const 1)))
        (br $surf_loop)
      )
    )

    (local.set $n (i32.const 0))
    (block $body_done
      (loop $body_loop
        (br_if $body_done (i32.ge_s (local.get $n) (local.get $nbody)))
        (if (i32.ne (call $load_i32_at (local.get $lpltbody_ptr) (local.get $n)) (i32.const 0))
          (then
            (local.set $l1 (call $load_i32_at (local.get $lfrst_ptr) (local.get $n)))
            (local.set $ln (i32.sub (i32.add (call $load_i32_at (local.get $lfrst_ptr) (local.get $n))
                                              (call $load_i32_at (local.get $nl_ptr) (local.get $n)))
                                     (i32.const 1)))

            (call $store_f32_at (local.get $pts_ptr) (i32.const 0)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $l1) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 1)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $l1) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 2)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $l1) (i32.const 3)) (i32.const 2))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 3)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $ln) (i32.const 3)) (i32.const 0))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 4)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $ln) (i32.const 3)) (i32.const 1))))
            (call $store_f32_at (local.get $pts_ptr) (i32.const 5)
              (call $load_f32_at (local.get $rl_ptr) (i32.add (i32.mul (local.get $ln) (i32.const 3)) (i32.const 2))))

            (call $TETRAN (local.get $pts_ptr) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $dxyz_ptr))
            (call $TETRAN (i32.add (local.get $pts_ptr) (i32.const 12)) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $dxyz_ptr))

            (if (i32.ne (local.get $lproj) (i32.const 0))
              (then (call $VIEWPROJ (local.get $pts_ptr) (i32.const 2) (local.get $view_ptr))))

            (local.set $k (i32.const 0))
            (block $k_done3
              (loop $k_loop3
                (br_if $k_done3 (i32.ge_s (local.get $k) (i32.const 3)))
                (local.set $v1 (call $load_f32_at (local.get $pts_ptr) (local.get $k)))
                (local.set $v2 (call $load_f32_at (local.get $pts_ptr) (i32.add (local.get $k) (i32.const 3))))
                (call $store_f32_at (local.get $xyzmin_ptr) (local.get $k)
                  (f32.min (call $load_f32_at (local.get $xyzmin_ptr) (local.get $k))
                           (f32.min (local.get $v1) (local.get $v2))))
                (call $store_f32_at (local.get $xyzmax_ptr) (local.get $k)
                  (f32.max (call $load_f32_at (local.get $xyzmax_ptr) (local.get $k))
                           (f32.max (local.get $v1) (local.get $v2))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k_loop3)
              )
            )
          )
        )

        (local.set $n (i32.add (local.get $n) (i32.const 1)))
        (br $body_loop)
      )
    )
  )

  (func (export "AXLIMS")
    (param $gmin_ptr i32) (param $gmax_ptr i32)
    (param $axmin_ptr i32) (param $axmax_ptr i32) (param $axspan_ptr i32)
    (param $axdel_ptr i32) (param $naxann_ptr i32)
    (local $k i32) (local $axdmax f32) (local $tmp f32)
    (local $xmin_ptr i32) (local $xmax_ptr i32)

    (local.set $k (i32.const 0))
    (block $k_done
      (loop $k_loop
        (br_if $k_done (i32.ge_s (local.get $k) (i32.const 3)))
        (call $store_f32_at (local.get $axmin_ptr) (local.get $k)
          (f32.min (call $load_f32_at (local.get $gmin_ptr) (local.get $k)) (f32.const 0)))
        (call $store_f32_at (local.get $axmax_ptr) (local.get $k)
          (f32.max (call $load_f32_at (local.get $gmax_ptr) (local.get $k)) (f32.const 0)))
        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $k_loop)
      )
    )

    (local.set $axdmax (f32.max
      (f32.sub (call $load_f32_at (local.get $axmax_ptr) (i32.const 0)) (call $load_f32_at (local.get $axmin_ptr) (i32.const 0)))
      (f32.max
        (f32.sub (call $load_f32_at (local.get $axmax_ptr) (i32.const 1)) (call $load_f32_at (local.get $axmin_ptr) (i32.const 1)))
        (f32.sub (call $load_f32_at (local.get $axmax_ptr) (i32.const 2)) (call $load_f32_at (local.get $axmin_ptr) (i32.const 2))))))

    (local.set $k (i32.const 0))
    (block $k_done2
      (loop $k_loop2
        (br_if $k_done2 (i32.ge_s (local.get $k) (i32.const 3)))
        (local.set $tmp
          (f32.sub (call $load_f32_at (local.get $axmax_ptr) (local.get $k))
                   (call $load_f32_at (local.get $axmin_ptr) (local.get $k))))
        (if (f32.lt (local.get $tmp) (f32.mul (f32.const 0.25) (local.get $axdmax)))
          (then
            (call $store_f32_at (local.get $axmin_ptr) (local.get $k)
              (f32.min (call $load_f32_at (local.get $axmin_ptr) (local.get $k))
                       (f32.mul (f32.const -0.125) (local.get $axdmax))))
            (call $store_f32_at (local.get $axmax_ptr) (local.get $k)
              (f32.max (call $load_f32_at (local.get $axmax_ptr) (local.get $k))
                       (f32.mul (f32.const 0.125) (local.get $axdmax))))
          )
        )

        (local.set $xmin_ptr (i32.add (local.get $axmin_ptr) (i32.mul (local.get $k) (i32.const 4))))
        (local.set $xmax_ptr (i32.add (local.get $axmax_ptr) (i32.mul (local.get $k) (i32.const 4))))
        (call $AXISADJ
          (local.get $xmin_ptr)
          (local.get $xmax_ptr)
          (i32.add (local.get $axspan_ptr) (i32.mul (local.get $k) (i32.const 4)))
          (i32.add (local.get $axdel_ptr) (i32.mul (local.get $k) (i32.const 4)))
          (i32.add (local.get $naxann_ptr) (i32.mul (local.get $k) (i32.const 4))))

        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $k_loop2)
      )
    )
  )

  (func (export "HIDINIT")
    (param $lreset i32) (param $nsurf i32)
    (param $lpltsurf_ptr i32) (param $jfrst_ptr i32) (param $nj_ptr i32)
    (param $rle1_ptr i32) (param $chord1_ptr i32)
    (param $rle2_ptr i32) (param $chord2_ptr i32)
    (param $view_ptr i32) (param $pts_ptr i32)
    (param $ntri_ptr i32) (param $tri_ptr i32)
    (local $n i32) (local $j i32) (local $j1 i32) (local $jn i32)
    (local $id i32)

    (if (i32.ne (local.get $lreset) (i32.const 0))
      (then (i32.store (local.get $ntri_ptr) (i32.const 0))))

    (local.set $n (i32.const 0))
    (block $surf_done
      (loop $surf_loop
        (br_if $surf_done (i32.ge_s (local.get $n) (local.get $nsurf)))
        (if (i32.ne (call $load_i32_at (local.get $lpltsurf_ptr) (local.get $n)) (i32.const 0))
          (then
            (local.set $j1 (call $load_i32_at (local.get $jfrst_ptr) (local.get $n)))
            (local.set $jn (i32.sub (i32.add (call $load_i32_at (local.get $jfrst_ptr) (local.get $n))
                                              (call $load_i32_at (local.get $nj_ptr) (local.get $n)))
                                     (i32.const 1)))
            (local.set $j (local.get $j1))
            (block $j_done
              (loop $j_loop
                (br_if $j_done (i32.gt_s (local.get $j) (local.get $jn)))
                (local.set $id (i32.add (local.get $j) (i32.const 1)))

                (call $store_f32_at (local.get $pts_ptr) (i32.const 0)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 1)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 2)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 3)
                  (f32.add
                    (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0)))
                    (call $load_f32_at (local.get $chord1_ptr) (local.get $j))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 4)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 5)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))

                (call $store_f32_at (local.get $pts_ptr) (i32.const 6)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 7)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 8)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 9)
                  (f32.add
                    (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0)))
                    (call $load_f32_at (local.get $chord2_ptr) (local.get $j))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 10)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 11)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))

                (call $VIEWPROJ (local.get $pts_ptr) (i32.const 4) (local.get $view_ptr))
                (call $TRIINIT (local.get $id) (i32.const 1) (i32.const 1)
                  (local.get $pts_ptr) (local.get $ntri_ptr) (local.get $tri_ptr))

                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
            )
          )
        )
        (local.set $n (i32.add (local.get $n) (i32.const 1)))
        (br $surf_loop)
      )
    )
  )

  (func (export "HIDINITE")
    (param $lreset i32) (param $nsurf i32)
    (param $lpltsurf_ptr i32) (param $jfrst_ptr i32) (param $nj_ptr i32)
    (param $rle1_ptr i32) (param $chord1_ptr i32)
    (param $rle2_ptr i32) (param $chord2_ptr i32)
    (param $view_ptr i32) (param $pts_ptr i32)
    (param $ntri_ptr i32) (param $tri_ptr i32)
    (param $ang_ptr i32) (param $pos_ptr i32) (param $xyzr_ptr i32)
    (param $tt_ptr i32) (param $ttang_ptr i32)
    (local $n i32) (local $j i32) (local $j1 i32) (local $jn i32)
    (local $id i32)

    (if (i32.ne (local.get $lreset) (i32.const 0))
      (then (i32.store (local.get $ntri_ptr) (i32.const 0))))

    (call $ROTENS3 (local.get $ang_ptr) (local.get $tt_ptr) (local.get $ttang_ptr))

    (local.set $n (i32.const 0))
    (block $surf_done
      (loop $surf_loop
        (br_if $surf_done (i32.ge_s (local.get $n) (local.get $nsurf)))
        (if (i32.ne (call $load_i32_at (local.get $lpltsurf_ptr) (local.get $n)) (i32.const 0))
          (then
            (local.set $j1 (call $load_i32_at (local.get $jfrst_ptr) (local.get $n)))
            (local.set $jn (i32.sub (i32.add (call $load_i32_at (local.get $jfrst_ptr) (local.get $n))
                                              (call $load_i32_at (local.get $nj_ptr) (local.get $n)))
                                     (i32.const 1)))
            (local.set $j (local.get $j1))
            (block $j_done
              (loop $j_loop
                (br_if $j_done (i32.gt_s (local.get $j) (local.get $jn)))
                (local.set $id (i32.add (local.get $j) (i32.const 1)))

                (call $store_f32_at (local.get $pts_ptr) (i32.const 0)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 1)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 2)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 3)
                  (f32.add
                    (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0)))
                    (call $load_f32_at (local.get $chord1_ptr) (local.get $j))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 4)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 5)
                  (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))

                (call $store_f32_at (local.get $pts_ptr) (i32.const 6)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 7)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 8)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 9)
                  (f32.add
                    (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0)))
                    (call $load_f32_at (local.get $chord2_ptr) (local.get $j))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 10)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
                (call $store_f32_at (local.get $pts_ptr) (i32.const 11)
                  (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))

                (call $TETRAN (local.get $pts_ptr) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $pos_ptr))
                (call $TETRAN (i32.add (local.get $pts_ptr) (i32.const 12)) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $pos_ptr))
                (call $TETRAN (i32.add (local.get $pts_ptr) (i32.const 24)) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $pos_ptr))
                (call $TETRAN (i32.add (local.get $pts_ptr) (i32.const 36)) (local.get $tt_ptr) (local.get $xyzr_ptr) (local.get $pos_ptr))

                (call $VIEWPROJ (local.get $pts_ptr) (i32.const 4) (local.get $view_ptr))
                (call $TRIINIT (local.get $id) (i32.const 1) (i32.const 1)
                  (local.get $pts_ptr) (local.get $ntri_ptr) (local.get $tri_ptr))

                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
            )
          )
        )
        (local.set $n (i32.add (local.get $n) (i32.const 1)))
        (br $surf_loop)
      )
    )
  )
)
