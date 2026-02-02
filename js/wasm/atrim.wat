;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (import "env" "sin_f32" (func $sin_f32 (param f32) (result f32)))
  (import "env" "cos_f32" (func $cos_f32 (param f32) (result f32)))
  (memory (export "memory") 1)

  (func $par_offset (param $i i32) (param $j i32) (param $stride i32) (result i32)
    (i32.mul
      (i32.const 4)
      (i32.add (local.get $i) (i32.mul (local.get $stride) (local.get $j)))))

  (func (export "TRMSET_CORE")
    (param $ktrim i32) (param $ir1 i32) (param $ir2 i32) (param $ir i32)
    (param $nvtot i32) (param $dtr f32) (param $cref f32) (param $bref f32) (param $sref f32) (param $unitl f32)
    (param $rho0 f32) (param $gee0 f32) (param $rmass0 f32)
    (param $iptot i32) (param $icmax i32) (param $ivtot i32)
    (param $parval i32) (param $conval i32) (param $icon i32) (param $itrim i32)
    (param $ipphi i32) (param $ipthe i32) (param $ipcl i32) (param $ipvee i32) (param $iprad i32)
    (param $iprhop i32) (param $ipgee i32) (param $ipfac i32) (param $ipmass i32)
    (param $ivalfa i32) (param $ivrotx i32) (param $ivroty i32) (param $ivrotz i32)
    (param $iccl i32) (param $icrotx i32) (param $icroty i32) (param $icrotz i32)
    (local $jr i32)
    (local $iv i32)
    (local $tmp_i i32)
    (local $addr i32)
    (local $crefd f32)
    (local $brefd f32)
    (local $srefd f32)
    (local $phi f32)
    (local $the f32)
    (local $cl f32)
    (local $vee f32)
    (local $rad f32)
    (local $rho f32)
    (local $gee f32)
    (local $fac f32)
    (local $rmass f32)
    (local $sinp f32)
    (local $cosp f32)
    (local $whx f32)
    (local $why f32)
    (local $whz f32)

    ;; If KTRIM=1 or 2 and CL is zero, use CL constraint if present.
    (if (i32.or (i32.eq (local.get $ktrim) (i32.const 1))
                (i32.eq (local.get $ktrim) (i32.const 2)))
      (then
        (local.set $addr
          (i32.add (local.get $parval)
            (call $par_offset (local.get $ipcl) (local.get $ir) (local.get $iptot))))
        (if (f32.eq (f32.load (local.get $addr)) (f32.const 0))
          (then
            (local.set $iv (i32.const 1))
            (block $iv_done
              (loop $iv_loop
                (br_if $iv_done (i32.gt_u (local.get $iv) (local.get $nvtot)))
                (local.set $addr
                  (i32.add (local.get $icon)
                    (call $par_offset (local.get $iv) (local.get $ir) (local.get $ivtot))))
                (if (i32.eq (i32.load (local.get $addr)) (local.get $iccl))
                  (then
                    (local.set $addr
                      (i32.add (local.get $conval)
                        (call $par_offset (local.get $iccl) (local.get $ir) (local.get $icmax))))
                    (local.set $tmp_i
                      (i32.add (local.get $parval)
                        (call $par_offset (local.get $ipcl) (local.get $ir) (local.get $iptot))))
                    (f32.store (local.get $tmp_i) (f32.load (local.get $addr)))
                    (br $iv_done)
                  )
                )
                (local.set $iv (i32.add (local.get $iv) (i32.const 1)))
                (br $iv_loop)
              )
            )
          )
        )
      )
    )

    ;; ITRIM(IR) = -KTRIM
    (local.set $addr
      (i32.add (local.get $itrim) (i32.mul (local.get $ir) (i32.const 4))))
    (i32.store (local.get $addr) (i32.sub (i32.const 0) (local.get $ktrim)))

    ;; defaults for rho, gee, mass
    (local.set $addr
      (i32.add (local.get $parval)
        (call $par_offset (local.get $iprhop) (local.get $ir) (local.get $iptot))))
    (if (f32.le (f32.load (local.get $addr)) (f32.const 0))
      (then (f32.store (local.get $addr) (local.get $rho0))))

    (local.set $addr
      (i32.add (local.get $parval)
        (call $par_offset (local.get $ipgee) (local.get $ir) (local.get $iptot))))
    (if (f32.le (f32.load (local.get $addr)) (f32.const 0))
      (then (f32.store (local.get $addr) (local.get $gee0))))

    (local.set $addr
      (i32.add (local.get $parval)
        (call $par_offset (local.get $ipmass) (local.get $ir) (local.get $iptot))))
    (if (f32.le (f32.load (local.get $addr)) (f32.const 0))
      (then (f32.store (local.get $addr) (local.get $rmass0))))

    (local.set $crefd (f32.mul (local.get $cref) (local.get $unitl)))
    (local.set $brefd (f32.mul (local.get $bref) (local.get $unitl)))
    (local.set $srefd (f32.mul (local.get $sref)
      (f32.mul (local.get $unitl) (local.get $unitl))))

    (local.set $jr (local.get $ir1))
    (block $jr_done
      (loop $jr_loop
        (br_if $jr_done (i32.gt_u (local.get $jr) (local.get $ir2)))

        (local.set $phi
          (f32.load (i32.add (local.get $parval)
            (call $par_offset (local.get $ipphi) (local.get $jr) (local.get $iptot)))))
        (local.set $the
          (f32.load (i32.add (local.get $parval)
            (call $par_offset (local.get $ipthe) (local.get $jr) (local.get $iptot)))))
        (local.set $cl
          (f32.load (i32.add (local.get $parval)
            (call $par_offset (local.get $ipcl) (local.get $jr) (local.get $iptot)))))
        (local.set $vee
          (f32.load (i32.add (local.get $parval)
            (call $par_offset (local.get $ipvee) (local.get $jr) (local.get $iptot)))))
        (local.set $rad
          (f32.load (i32.add (local.get $parval)
            (call $par_offset (local.get $iprad) (local.get $jr) (local.get $iptot)))))
        (local.set $rho
          (f32.load (i32.add (local.get $parval)
            (call $par_offset (local.get $iprhop) (local.get $jr) (local.get $iptot)))))
        (local.set $gee
          (f32.load (i32.add (local.get $parval)
            (call $par_offset (local.get $ipgee) (local.get $jr) (local.get $iptot)))))
        (local.set $fac
          (f32.load (i32.add (local.get $parval)
            (call $par_offset (local.get $ipfac) (local.get $jr) (local.get $iptot)))))
        (local.set $rmass
          (f32.load (i32.add (local.get $parval)
            (call $par_offset (local.get $ipmass) (local.get $jr) (local.get $iptot)))))

        (local.set $sinp (call $sin_f32 (f32.mul (local.get $phi) (local.get $dtr))))
        (local.set $cosp (call $cos_f32 (f32.mul (local.get $phi) (local.get $dtr))))

        (if (i32.eq (local.get $ktrim) (i32.const 1))
          (then
            (if (i32.and (f32.le (local.get $vee) (f32.const 0))
                         (f32.gt (local.get $cl) (f32.const 0)))
              (then
                (local.set $vee
                  (f32.sqrt
                    (f32.div
                      (f32.mul (f32.const 2) (f32.mul (local.get $rmass) (local.get $gee)))
                      (f32.mul (local.get $rho)
                        (f32.mul (local.get $srefd)
                          (f32.mul (local.get $cl) (local.get $cosp)))))))
                (f32.store
                  (i32.add (local.get $parval)
                    (call $par_offset (local.get $ipvee) (local.get $jr) (local.get $iptot)))
                  (local.get $vee))
              )
            )
            (if (i32.and (f32.le (local.get $cl) (f32.const 0))
                         (f32.gt (local.get $vee) (f32.const 0)))
              (then
                (local.set $cl
                  (f32.div
                    (f32.mul (f32.const 2) (f32.mul (local.get $rmass) (local.get $gee)))
                    (f32.mul (local.get $rho)
                      (f32.mul (local.get $srefd)
                        (f32.mul (local.get $vee)
                          (f32.mul (local.get $vee) (local.get $cosp)))))))
                (f32.store
                  (i32.add (local.get $parval)
                    (call $par_offset (local.get $ipcl) (local.get $jr) (local.get $iptot)))
                  (local.get $cl))
              )
            )

            (if (f32.eq (local.get $sinp) (f32.const 0))
              (then
                (local.set $rad (f32.const 0))
              )
              (else
                (local.set $rad
                  (f32.div
                    (f32.mul (f32.mul (local.get $vee) (local.get $vee)) (local.get $cosp))
                    (f32.mul (local.get $gee) (local.get $sinp))))
              )
            )
            (f32.store
              (i32.add (local.get $parval)
                (call $par_offset (local.get $iprad) (local.get $jr) (local.get $iptot)))
              (local.get $rad))

            (local.set $fac (f32.div (f32.const 1) (local.get $cosp)))
            (f32.store
              (i32.add (local.get $parval)
                (call $par_offset (local.get $ipfac) (local.get $jr) (local.get $iptot)))
              (local.get $fac))

            (local.set $the (f32.const 0))
            (f32.store
              (i32.add (local.get $parval)
                (call $par_offset (local.get $ipthe) (local.get $jr) (local.get $iptot)))
              (local.get $the))

            (local.set $whx (f32.const 0))
            (local.set $why (f32.const 0))
            (local.set $whz (f32.const 0))
            (if (f32.gt (local.get $rad) (f32.const 0))
              (then
                (local.set $why
                  (f32.div
                    (f32.mul (local.get $sinp) (local.get $crefd))
                    (f32.mul (f32.const 2) (local.get $rad))))
                (local.set $whz
                  (f32.div
                    (f32.mul (local.get $cosp) (local.get $brefd))
                    (f32.mul (f32.const 2) (local.get $rad))))
              )
            )

            (f32.store
              (i32.add (local.get $conval)
                (call $par_offset (local.get $iccl) (local.get $jr) (local.get $icmax)))
              (local.get $cl))
            (f32.store
              (i32.add (local.get $conval)
                (call $par_offset (local.get $icrotx) (local.get $jr) (local.get $icmax)))
              (local.get $whx))
            (f32.store
              (i32.add (local.get $conval)
                (call $par_offset (local.get $icroty) (local.get $jr) (local.get $icmax)))
              (local.get $why))
            (f32.store
              (i32.add (local.get $conval)
                (call $par_offset (local.get $icrotz) (local.get $jr) (local.get $icmax)))
              (local.get $whz))

            (i32.store
              (i32.add (local.get $icon)
                (call $par_offset (local.get $ivalfa) (local.get $jr) (local.get $ivtot)))
              (local.get $iccl))
            (i32.store
              (i32.add (local.get $icon)
                (call $par_offset (local.get $ivrotx) (local.get $jr) (local.get $ivtot)))
              (local.get $icrotx))
            (i32.store
              (i32.add (local.get $icon)
                (call $par_offset (local.get $ivroty) (local.get $jr) (local.get $ivtot)))
              (local.get $icroty))
            (i32.store
              (i32.add (local.get $icon)
                (call $par_offset (local.get $ivrotz) (local.get $jr) (local.get $ivtot)))
              (local.get $icrotz))
          )
        )

        (if (i32.eq (local.get $ktrim) (i32.const 2))
          (then
            (if (i32.and (f32.eq (local.get $rad) (f32.const 0))
                         (f32.gt (local.get $cl) (f32.const 0)))
              (then
                (local.set $rad
                  (f32.div (local.get $rmass)
                    (f32.mul (f32.const 0.5)
                      (f32.mul (local.get $rho)
                        (f32.mul (local.get $srefd) (local.get $cl))))))
                (f32.store
                  (i32.add (local.get $parval)
                    (call $par_offset (local.get $iprad) (local.get $jr) (local.get $iptot)))
                  (local.get $rad))
              )
            )

            (if (i32.and (f32.gt (local.get $rad) (f32.const 0))
                         (f32.eq (local.get $cl) (f32.const 0)))
              (then
                (local.set $cl
                  (f32.div (local.get $rmass)
                    (f32.mul (f32.const 0.5)
                      (f32.mul (local.get $rho)
                        (f32.mul (local.get $srefd) (local.get $rad))))))
                (f32.store
                  (i32.add (local.get $parval)
                    (call $par_offset (local.get $ipcl) (local.get $jr) (local.get $iptot)))
                  (local.get $cl))
              )
            )

            (if
              (i32.and
                (i32.and (f32.eq (local.get $fac) (f32.const 0))
                         (f32.gt (local.get $cl) (f32.const 0)))
                (i32.and (f32.gt (local.get $vee) (f32.const 0))
                         (f32.gt (local.get $gee) (f32.const 0))))
              (then
                (local.set $fac
                  (f32.div
                    (f32.mul (f32.mul (f32.const 0.5) (local.get $rho))
                      (f32.mul (f32.mul (local.get $vee) (local.get $vee))
                        (f32.mul (local.get $srefd) (local.get $cl))))
                    (f32.mul (local.get $rmass) (local.get $gee))))
                (f32.store
                  (i32.add (local.get $parval)
                    (call $par_offset (local.get $ipfac) (local.get $jr) (local.get $iptot)))
                  (local.get $fac))
              )
            )

            (if
              (i32.and
                (i32.and (f32.gt (local.get $fac) (f32.const 0))
                         (f32.gt (local.get $cl) (f32.const 0)))
                (i32.and (f32.eq (local.get $vee) (f32.const 0))
                         (f32.gt (local.get $gee) (f32.const 0))))
              (then
                (local.set $vee
                  (f32.sqrt
                    (f32.div
                      (f32.mul (local.get $fac) (f32.mul (local.get $rmass) (local.get $gee)))
                      (f32.mul (f32.const 0.5)
                        (f32.mul (local.get $rho)
                          (f32.mul (local.get $srefd) (local.get $cl)))))))
                (f32.store
                  (i32.add (local.get $parval)
                    (call $par_offset (local.get $ipvee) (local.get $jr) (local.get $iptot)))
                  (local.get $vee))
              )
            )

            (local.set $the (f32.const 0))
            (f32.store
              (i32.add (local.get $parval)
                (call $par_offset (local.get $ipthe) (local.get $jr) (local.get $iptot)))
              (local.get $the))

            (local.set $whx (f32.const 0))
            (local.set $why (f32.const 0))
            (local.set $whz (f32.const 0))
            (if (f32.gt (local.get $rad) (f32.const 0))
              (then
                (local.set $why
                  (f32.div (local.get $crefd)
                    (f32.mul (f32.const 2) (local.get $rad))))
              )
            )

            (f32.store
              (i32.add (local.get $conval)
                (call $par_offset (local.get $iccl) (local.get $jr) (local.get $icmax)))
              (local.get $cl))
            (f32.store
              (i32.add (local.get $conval)
                (call $par_offset (local.get $icrotx) (local.get $jr) (local.get $icmax)))
              (local.get $whx))
            (f32.store
              (i32.add (local.get $conval)
                (call $par_offset (local.get $icroty) (local.get $jr) (local.get $icmax)))
              (local.get $why))
            (f32.store
              (i32.add (local.get $conval)
                (call $par_offset (local.get $icrotz) (local.get $jr) (local.get $icmax)))
              (local.get $whz))

            (i32.store
              (i32.add (local.get $icon)
                (call $par_offset (local.get $ivalfa) (local.get $jr) (local.get $ivtot)))
              (local.get $iccl))
            (i32.store
              (i32.add (local.get $icon)
                (call $par_offset (local.get $ivrotx) (local.get $jr) (local.get $ivtot)))
              (local.get $icrotx))
            (i32.store
              (i32.add (local.get $icon)
                (call $par_offset (local.get $ivroty) (local.get $jr) (local.get $ivtot)))
              (local.get $icroty))
            (i32.store
              (i32.add (local.get $icon)
                (call $par_offset (local.get $ivrotz) (local.get $jr) (local.get $ivtot)))
              (local.get $icrotz))
          )
        )

        (local.set $jr (i32.add (local.get $jr) (i32.const 1)))
        (br $jr_loop)
      )
    )
  )
)
