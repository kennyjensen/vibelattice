(module
  (import "env" "cos" (func $cos (param f32) (result f32)))
  (import "env" "sin" (func $sin (param f32) (result f32)))
  (import "env" "atan" (func $atan (param f32) (result f32)))

  (memory (export "memory") 1)

  (global $D_BASE i32 (i32.const 4096))
  (global $T_BASE i32 (i32.const 4120))

  (func $load_f32_at (param $base i32) (param $idx i32) (result f32)
    (f32.load
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))))

  (func $store_f32_at (param $base i32) (param $idx i32) (param $val f32)
    (f32.store
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))
      (local.get $val)))

  (func (export "AKIMA") (param $x i32) (param $y i32) (param $n i32) (param $xx f32) (param $yy_out i32) (param $slp_out i32)
    (local $xordr f32)
    (local $ibot i32)
    (local $itop i32)
    (local $xxo f32)
    (local $nstep i32)
    (local $i i32)
    (local $xo f32)
    (local $j i32)
    (local $k i32)
    (local $a f32)
    (local $b f32)
    (local $xint f32)
    (local $xdif f32)
    (local $p0 f32)
    (local $p1 f32)
    (local $p2 f32)
    (local $p3 f32)
    (local $yy f32)
    (local $slp f32)

    (if (f32.eq (call $load_f32_at (local.get $x) (i32.const 0))
                (call $load_f32_at (local.get $x) (i32.sub (local.get $n) (i32.const 1))))
      (then
        (f32.store (local.get $yy_out) (call $load_f32_at (local.get $y) (i32.const 0)))
        (f32.store (local.get $slp_out) (f32.const 0))
        (return)
      )
    )

    (local.set $xordr (f32.const 1))
    (if (f32.gt (call $load_f32_at (local.get $x) (i32.const 0))
                (call $load_f32_at (local.get $x) (i32.sub (local.get $n) (i32.const 1))))
      (then (local.set $xordr (f32.const -1)))
    )

    (local.set $ibot (i32.const 1))
    (local.set $itop (local.get $n))
    (local.set $xxo (f32.mul (local.get $xx) (local.get $xordr)))

    (block $search_done
      (loop $search_loop
        (local.set $nstep
          (i32.div_s
            (i32.sub (local.get $itop) (local.get $ibot))
            (i32.const 2)))
        (local.set $i (i32.add (local.get $ibot) (local.get $nstep)))
        (local.set $xo
          (f32.mul (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1))) (local.get $xordr)))
        (if (f32.ge (local.get $xxo) (local.get $xo))
          (then (local.set $ibot (local.get $i)))
        )
        (if (f32.lt (local.get $xxo) (local.get $xo))
          (then (local.set $itop (local.get $i)))
        )
        (br_if $search_loop (i32.ne (local.get $nstep) (i32.const 0)))
      )
    )

    (local.set $i (local.get $ibot))

    (local.set $j (i32.const 0))
    (block $d_done
      (loop $d_loop
        (br_if $d_done (i32.ge_s (local.get $j) (i32.const 5)))
        (local.set $k (i32.add (local.get $i) (i32.sub (local.get $j) (i32.const 1))))
        (if (i32.and (i32.ge_s (i32.sub (local.get $k) (i32.const 1)) (i32.const 1))
                     (i32.le_s (local.get $k) (local.get $n)))
          (then
            (call $store_f32_at (global.get $D_BASE) (local.get $j)
              (f32.div
                (f32.sub (call $load_f32_at (local.get $y) (i32.sub (local.get $k) (i32.const 1)))
                         (call $load_f32_at (local.get $y) (i32.sub (local.get $k) (i32.const 2))))
                (f32.sub (call $load_f32_at (local.get $x) (i32.sub (local.get $k) (i32.const 1)))
                         (call $load_f32_at (local.get $x) (i32.sub (local.get $k) (i32.const 2))))))
          )
        )
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $d_loop)
      )
    )

    (if (i32.eq (local.get $n) (i32.const 2))
      (then
        (call $store_f32_at (global.get $D_BASE) (i32.const 1)
          (call $load_f32_at (global.get $D_BASE) (i32.const 2)))
      )
    )

    (if (i32.gt_s (i32.add (local.get $i) (i32.const 2)) (local.get $n))
      (then
        (call $store_f32_at (global.get $D_BASE) (i32.const 3)
          (f32.sub
            (f32.mul (f32.const 2) (call $load_f32_at (global.get $D_BASE) (i32.const 2)))
            (call $load_f32_at (global.get $D_BASE) (i32.const 1))))
      )
    )
    (if (i32.gt_s (i32.add (local.get $i) (i32.const 3)) (local.get $n))
      (then
        (call $store_f32_at (global.get $D_BASE) (i32.const 4)
          (f32.sub
            (f32.mul (f32.const 2) (call $load_f32_at (global.get $D_BASE) (i32.const 3)))
            (call $load_f32_at (global.get $D_BASE) (i32.const 2))))
      )
    )
    (if (i32.lt_s (i32.sub (local.get $i) (i32.const 1)) (i32.const 1))
      (then
        (call $store_f32_at (global.get $D_BASE) (i32.const 1)
          (f32.sub
            (f32.mul (f32.const 2) (call $load_f32_at (global.get $D_BASE) (i32.const 2)))
            (call $load_f32_at (global.get $D_BASE) (i32.const 3))))
      )
    )
    (if (i32.lt_s (i32.sub (local.get $i) (i32.const 2)) (i32.const 1))
      (then
        (call $store_f32_at (global.get $D_BASE) (i32.const 0)
          (f32.sub
            (f32.mul (f32.const 2) (call $load_f32_at (global.get $D_BASE) (i32.const 1)))
            (call $load_f32_at (global.get $D_BASE) (i32.const 2))))
      )
    )

    (local.set $j (i32.const 0))
    (block $t_done
      (loop $t_loop
        (br_if $t_done (i32.ge_s (local.get $j) (i32.const 2)))
        (local.set $a
          (f32.abs
            (f32.sub
              (call $load_f32_at (global.get $D_BASE) (i32.add (local.get $j) (i32.const 3)))
              (call $load_f32_at (global.get $D_BASE) (i32.add (local.get $j) (i32.const 2))))))
        (local.set $b
          (f32.abs
            (f32.sub
              (call $load_f32_at (global.get $D_BASE) (i32.add (local.get $j) (i32.const 1)))
              (call $load_f32_at (global.get $D_BASE) (local.get $j)))))
        (if (f32.eq (f32.add (local.get $a) (local.get $b)) (f32.const 0))
          (then
            (local.set $a (f32.const 1))
            (local.set $b (f32.const 1))
          )
        )
        (call $store_f32_at (global.get $T_BASE) (local.get $j)
          (f32.div
            (f32.add
              (f32.mul (local.get $a) (call $load_f32_at (global.get $D_BASE) (i32.add (local.get $j) (i32.const 1))))
              (f32.mul (local.get $b) (call $load_f32_at (global.get $D_BASE) (i32.add (local.get $j) (i32.const 2)))))
            (f32.add (local.get $a) (local.get $b))))
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $t_loop)
      )
    )

    (if (f32.eq (local.get $xx) (call $load_f32_at (local.get $x) (local.get $i)))
      (then
        (f32.store (local.get $yy_out) (call $load_f32_at (local.get $y) (local.get $i)))
        (f32.store (local.get $slp_out) (call $load_f32_at (global.get $T_BASE) (i32.const 1)))
        (return)
      )
    )

    (local.set $xint
      (f32.sub
        (call $load_f32_at (local.get $x) (local.get $i))
        (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))))
    (local.set $xdif
      (f32.sub (local.get $xx) (call $load_f32_at (local.get $x) (i32.sub (local.get $i) (i32.const 1)))))
    (local.set $p0 (call $load_f32_at (local.get $y) (i32.sub (local.get $i) (i32.const 1))))
    (local.set $p1 (call $load_f32_at (global.get $T_BASE) (i32.const 0)))
    (local.set $p2
      (f32.div
        (f32.sub
          (f32.sub
            (f32.mul (f32.const 3) (call $load_f32_at (global.get $D_BASE) (i32.const 2)))
            (f32.mul (f32.const 2) (call $load_f32_at (global.get $T_BASE) (i32.const 0))))
          (call $load_f32_at (global.get $T_BASE) (i32.const 1)))
        (local.get $xint)))
    (local.set $p3
      (f32.div
        (f32.sub
          (f32.add
            (call $load_f32_at (global.get $T_BASE) (i32.const 0))
            (call $load_f32_at (global.get $T_BASE) (i32.const 1)))
          (f32.mul (f32.const 2) (call $load_f32_at (global.get $D_BASE) (i32.const 2))))
        (f32.mul (local.get $xint) (local.get $xint))))

    (local.set $yy
      (f32.add (local.get $p0)
        (f32.mul (local.get $xdif)
          (f32.add (local.get $p1)
            (f32.mul (local.get $xdif)
              (f32.add (local.get $p2)
                       (f32.mul (local.get $xdif) (local.get $p3))))))))
    (local.set $slp
      (f32.add (local.get $p1)
        (f32.mul (local.get $xdif)
          (f32.add (f32.mul (f32.const 2) (local.get $p2))
                   (f32.mul (local.get $xdif) (f32.mul (f32.const 3) (local.get $p3)))))))

    (f32.store (local.get $yy_out) (local.get $yy))
    (f32.store (local.get $slp_out) (local.get $slp))
  )

  (func (export "TRP1") (param $n i32) (param $x i32) (param $y i32) (param $xtrp f32) (result f32)
    (local $i i32)
    (if (i32.lt_s (local.get $n) (i32.const 1))
      (then (return (f32.const 0)))
    )
    (if (i32.lt_s (local.get $n) (i32.const 2))
      (then (return (call $load_f32_at (local.get $y) (i32.const 0))))
    )

    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.or (f32.gt (call $load_f32_at (local.get $x) (i32.add (local.get $i) (i32.const 1))) (local.get $xtrp))
                              (i32.eq (i32.add (local.get $i) (i32.const 1)) (i32.sub (local.get $n) (i32.const 1)))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )

    (f32.add
      (call $load_f32_at (local.get $y) (local.get $i))
      (f32.mul
        (f32.sub (call $load_f32_at (local.get $y) (i32.add (local.get $i) (i32.const 1)))
                 (call $load_f32_at (local.get $y) (local.get $i)))
        (f32.div
          (f32.sub (local.get $xtrp) (call $load_f32_at (local.get $x) (local.get $i)))
          (f32.sub (call $load_f32_at (local.get $x) (i32.add (local.get $i) (i32.const 1)))
                   (call $load_f32_at (local.get $x) (local.get $i)))))))
  (func (export "NRMLIZ") (param $n i32) (param $x i32)
    (local $i i32)
    (local $dx f32)
    (local $x1 f32)

    (if (i32.le_s (local.get $n) (i32.const 1)) (then (return)))

    (local.set $dx
      (f32.sub
        (call $load_f32_at (local.get $x) (i32.sub (local.get $n) (i32.const 1)))
        (call $load_f32_at (local.get $x) (i32.const 0))))
    (if (f32.eq (local.get $dx) (f32.const 0))
      (then (local.set $dx (f32.const 1)))
    )

    (local.set $x1 (call $load_f32_at (local.get $x) (i32.const 0)))
    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (call $store_f32_at (local.get $x) (local.get $i)
          (f32.div
            (f32.sub (call $load_f32_at (local.get $x) (local.get $i)) (local.get $x1))
            (local.get $dx)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  (func (export "SPACER") (param $n i32) (param $pspace f32) (param $x i32)
    (local $pi f32)
    (local $pabs f32)
    (local $nabs i32)
    (local $pequ f32)
    (local $pcos f32)
    (local $psin f32)
    (local $k i32)
    (local $frac f32)
    (local $theta f32)
    (local $val f32)

    (local.set $pi (f32.const 3.1415926535))
    (local.set $pabs (f32.abs (local.get $pspace)))
    (local.set $nabs (i32.add (i32.trunc_f32_s (local.get $pabs)) (i32.const 1)))

    (if (i32.eq (local.get $nabs) (i32.const 1))
      (then
        (local.set $pequ (f32.sub (f32.const 1) (local.get $pabs)))
        (local.set $pcos (local.get $pabs))
        (local.set $psin (f32.const 0))
      )
      (else
        (if (i32.eq (local.get $nabs) (i32.const 2))
          (then
            (local.set $pequ (f32.const 0))
            (local.set $pcos (f32.sub (f32.const 2) (local.get $pabs)))
            (local.set $psin (f32.sub (local.get $pabs) (f32.const 1)))
          )
          (else
            (local.set $pequ (f32.sub (local.get $pabs) (f32.const 2)))
            (local.set $pcos (f32.const 0))
            (local.set $psin (f32.sub (f32.const 3) (local.get $pabs)))
          )
        )
      )
    )

    (local.set $k (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s (local.get $k) (local.get $n)))
        (local.set $frac
          (f32.div
            (f32.convert_i32_s (local.get $k))
            (f32.convert_i32_s (i32.sub (local.get $n) (i32.const 1)))))
        (local.set $theta (f32.mul (local.get $frac) (local.get $pi)))
        (if (f32.ge (local.get $pspace) (f32.const 0))
          (then
            (local.set $val
              (f32.add
                (f32.add
                  (f32.mul (local.get $pequ) (local.get $frac))
                  (f32.mul (local.get $pcos)
                           (f32.mul (f32.sub (f32.const 1) (call $cos (local.get $theta))) (f32.const 0.5))))
                (f32.mul (local.get $psin)
                         (f32.sub (f32.const 1) (call $cos (f32.mul (local.get $theta) (f32.const 0.5))))))
            )
          )
          (else
            (local.set $val
              (f32.add
                (f32.add
                  (f32.mul (local.get $pequ) (local.get $frac))
                  (f32.mul (local.get $pcos)
                           (f32.mul (f32.sub (f32.const 1) (call $cos (local.get $theta))) (f32.const 0.5))))
                (f32.mul (local.get $psin)
                         (call $sin (f32.mul (local.get $theta) (f32.const 0.5))))))
          )
        )
        (call $store_f32_at (local.get $x) (local.get $k) (local.get $val))
        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $loop)
      )
    )
  )
  (func (export "CSPACER") (param $nvc i32) (param $cspace f32) (param $claf f32) (param $xpt i32) (param $xvr i32) (param $xsr i32) (param $xcp i32)
    (local $pi f32)
    (local $acsp f32)
    (local $ncsp i32)
    (local $f0 f32) (local $f1 f32) (local $f2 f32)
    (local $dth1 f32) (local $dth2 f32) (local $dxc0 f32)
    (local $ivc i32)
    (local $xc0 f32) (local $xpt0 f32) (local $xvr0 f32) (local $xsr0 f32) (local $xcp0 f32)
    (local $th1 f32) (local $xpt1 f32) (local $xvr1 f32) (local $xsr1 f32) (local $xcp1 f32)
    (local $th2 f32) (local $xpt2 f32) (local $xvr2 f32) (local $xsr2 f32) (local $xcp2 f32)

    (local.set $pi (f32.mul (f32.const 4) (call $atan (f32.const 1))))
    (local.set $acsp (f32.abs (local.get $cspace)))
    (local.set $ncsp (i32.trunc_f32_s (local.get $acsp)))

    (if (i32.eq (local.get $ncsp) (i32.const 0))
      (then
        (local.set $f0 (f32.sub (f32.const 1) (local.get $acsp)))
        (local.set $f1 (local.get $acsp))
        (local.set $f2 (f32.const 0))
      )
      (else
        (if (i32.eq (local.get $ncsp) (i32.const 1))
          (then
            (local.set $f0 (f32.const 0))
            (local.set $f1 (f32.sub (f32.const 2) (local.get $acsp)))
            (local.set $f2 (f32.sub (local.get $acsp) (f32.const 1)))
          )
          (else
            (local.set $f0 (f32.sub (local.get $acsp) (f32.const 2)))
            (local.set $f1 (f32.const 0))
            (local.set $f2 (f32.sub (f32.const 3) (local.get $acsp)))
          )
        )
      )
    )

    (local.set $dth1
      (f32.div (local.get $pi)
               (f32.convert_i32_s (i32.add (i32.mul (local.get $nvc) (i32.const 4)) (i32.const 2)))))
    (local.set $dth2
      (f32.mul (f32.const 0.5)
               (f32.div (local.get $pi)
                        (f32.convert_i32_s (i32.add (i32.mul (local.get $nvc) (i32.const 4)) (i32.const 1))))))
    (local.set $dxc0 (f32.div (f32.const 1) (f32.convert_i32_s (i32.mul (local.get $nvc) (i32.const 4)))))

    (local.set $ivc (i32.const 1))
    (block $done
      (loop $loop
        (br_if $done (i32.gt_s (local.get $ivc) (local.get $nvc)))

        (local.set $xc0
          (f32.mul
            (f32.convert_i32_s (i32.mul (i32.sub (i32.mul (local.get $ivc) (i32.const 4)) (i32.const 4)) (i32.const 1)))
            (local.get $dxc0)))
        (local.set $xpt0 (local.get $xc0))
        (local.set $xvr0 (f32.add (local.get $xc0) (local.get $dxc0)))
        (local.set $xsr0 (f32.add (local.get $xc0) (f32.mul (f32.const 2) (local.get $dxc0))))
        (local.set $xcp0 (f32.add (local.get $xc0)
                                  (f32.add (local.get $dxc0)
                                           (f32.mul (f32.mul (f32.const 2) (local.get $dxc0)) (local.get $claf)))))

        (local.set $th1
          (f32.mul
            (f32.convert_i32_s (i32.sub (i32.mul (local.get $ivc) (i32.const 4)) (i32.const 3)))
            (local.get $dth1)))
        (local.set $xpt1 (f32.mul (f32.const 0.5) (f32.sub (f32.const 1) (call $cos (local.get $th1)))))
        (local.set $xvr1 (f32.mul (f32.const 0.5) (f32.sub (f32.const 1) (call $cos (f32.add (local.get $th1) (local.get $dth1))))))
        (local.set $xsr1 (f32.mul (f32.const 0.5) (f32.sub (f32.const 1) (call $cos (f32.add (local.get $th1) (f32.mul (f32.const 2) (local.get $dth1)))))))
        (local.set $xcp1 (f32.mul (f32.const 0.5)
                                  (f32.sub (f32.const 1)
                                           (call $cos (f32.add (local.get $th1)
                                                               (f32.add (local.get $dth1)
                                                                        (f32.mul (f32.mul (f32.const 2) (local.get $dth1)) (local.get $claf))))))))

        (if (f32.gt (local.get $cspace) (f32.const 0))
          (then
            (local.set $th2
              (f32.mul
                (f32.convert_i32_s (i32.sub (i32.mul (local.get $ivc) (i32.const 4)) (i32.const 3)))
                (local.get $dth2)))
            (local.set $xpt2 (f32.sub (f32.const 1) (call $cos (local.get $th2))))
            (local.set $xvr2 (f32.sub (f32.const 1) (call $cos (f32.add (local.get $th2) (local.get $dth2)))))
            (local.set $xsr2 (f32.sub (f32.const 1) (call $cos (f32.add (local.get $th2) (f32.mul (f32.const 2) (local.get $dth2))))))
            (local.set $xcp2 (f32.sub (f32.const 1)
                                      (call $cos (f32.add (local.get $th2)
                                                          (f32.add (local.get $dth2)
                                                                   (f32.mul (f32.mul (f32.const 2) (local.get $dth2)) (local.get $claf)))))))
          )
          (else
            (local.set $th2
              (f32.mul
                (f32.convert_i32_s (i32.sub (i32.mul (local.get $ivc) (i32.const 4)) (i32.const 4)))
                (local.get $dth2)))
            (local.set $xpt2 (call $sin (local.get $th2)))
            (local.set $xvr2 (call $sin (f32.add (local.get $th2) (local.get $dth2))))
            (local.set $xsr2 (call $sin (f32.add (local.get $th2) (f32.mul (f32.const 2) (local.get $dth2)))))
            (local.set $xcp2 (call $sin (f32.add (local.get $th2)
                                                 (f32.add (local.get $dth2)
                                                          (f32.mul (f32.mul (f32.const 2) (local.get $dth2)) (local.get $claf))))))
          )
        )

        (call $store_f32_at (local.get $xpt) (i32.sub (local.get $ivc) (i32.const 1))
          (f32.add (f32.add (f32.mul (local.get $f0) (local.get $xpt0))
                            (f32.mul (local.get $f1) (local.get $xpt1)))
                   (f32.mul (local.get $f2) (local.get $xpt2))))
        (call $store_f32_at (local.get $xvr) (i32.sub (local.get $ivc) (i32.const 1))
          (f32.add (f32.add (f32.mul (local.get $f0) (local.get $xvr0))
                            (f32.mul (local.get $f1) (local.get $xvr1)))
                   (f32.mul (local.get $f2) (local.get $xvr2))))
        (call $store_f32_at (local.get $xsr) (i32.sub (local.get $ivc) (i32.const 1))
          (f32.add (f32.add (f32.mul (local.get $f0) (local.get $xsr0))
                            (f32.mul (local.get $f1) (local.get $xsr1)))
                   (f32.mul (local.get $f2) (local.get $xsr2))))
        (call $store_f32_at (local.get $xcp) (i32.sub (local.get $ivc) (i32.const 1))
          (f32.add (f32.add (f32.mul (local.get $f0) (local.get $xcp0))
                            (f32.mul (local.get $f1) (local.get $xcp1)))
                   (f32.mul (local.get $f2) (local.get $xcp2))))

        (local.set $ivc (i32.add (local.get $ivc) (i32.const 1)))
        (br $loop)
      )
    )

    (call $store_f32_at (local.get $xpt) (i32.const 0) (f32.const 0))
    (call $store_f32_at (local.get $xpt) (local.get $nvc) (f32.const 1))
  )
)
