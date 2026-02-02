;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (memory (export "memory") 1)

  (func $load_f32_at (param $base i32) (param $idx i32) (result f32)
    (f32.load (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))))

  (func $store_f32_at (param $base i32) (param $idx i32) (param $val f32)
    (f32.store (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4))) (local.get $val)))

  (func $load_i32_at (param $base i32) (param $idx i32) (result i32)
    (i32.load (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))))

  (func $store_i32_at (param $base i32) (param $idx i32) (param $val i32)
    (i32.store (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4))) (local.get $val)))

  (func $CROSS (param $u_ptr i32) (param $v_ptr i32) (param $w_ptr i32)
    (local $u1 f32) (local $u2 f32) (local $u3 f32)
    (local $v1 f32) (local $v2 f32) (local $v3 f32)

    (local.set $u1 (call $load_f32_at (local.get $u_ptr) (i32.const 0)))
    (local.set $u2 (call $load_f32_at (local.get $u_ptr) (i32.const 1)))
    (local.set $u3 (call $load_f32_at (local.get $u_ptr) (i32.const 2)))
    (local.set $v1 (call $load_f32_at (local.get $v_ptr) (i32.const 0)))
    (local.set $v2 (call $load_f32_at (local.get $v_ptr) (i32.const 1)))
    (local.set $v3 (call $load_f32_at (local.get $v_ptr) (i32.const 2)))

    (call $store_f32_at (local.get $w_ptr) (i32.const 0)
      (f32.sub (f32.mul (local.get $u2) (local.get $v3)) (f32.mul (local.get $u3) (local.get $v2))))
    (call $store_f32_at (local.get $w_ptr) (i32.const 1)
      (f32.sub (f32.mul (local.get $u3) (local.get $v1)) (f32.mul (local.get $u1) (local.get $v3))))
    (call $store_f32_at (local.get $w_ptr) (i32.const 2)
      (f32.sub (f32.mul (local.get $u1) (local.get $v2)) (f32.mul (local.get $u2) (local.get $v1))))
  )

  (func (export "MASINI")
    (param $rmass_ptr i32) (param $riner_ptr i32) (param $amass_ptr i32)
    (param $ainer_ptr i32) (param $xyzmass_ptr i32) (param $lmass_ptr i32)
    (local $k i32) (local $l i32)

    (f32.store (local.get $rmass_ptr) (f32.const 1))

    (local.set $k (i32.const 0))
    (block $k_done
      (loop $k_loop
        (br_if $k_done (i32.ge_s (local.get $k) (i32.const 3)))
        (local.set $l (i32.const 0))
        (block $l_done
          (loop $l_loop
            (br_if $l_done (i32.ge_s (local.get $l) (i32.const 3)))
            (call $store_f32_at (local.get $riner_ptr) (i32.add (local.get $k) (i32.mul (local.get $l) (i32.const 3))) (f32.const 0))
            (call $store_f32_at (local.get $amass_ptr) (i32.add (local.get $k) (i32.mul (local.get $l) (i32.const 3))) (f32.const 0))
            (call $store_f32_at (local.get $ainer_ptr) (i32.add (local.get $k) (i32.mul (local.get $l) (i32.const 3))) (f32.const 0))
            (local.set $l (i32.add (local.get $l) (i32.const 1)))
            (br $l_loop)
          )
        )
        (call $store_f32_at (local.get $riner_ptr) (i32.add (local.get $k) (i32.mul (local.get $k) (i32.const 3))) (f32.const 1))
        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $k_loop)
      )
    )

    (call $store_f32_at (local.get $xyzmass_ptr) (i32.const 0) (f32.const 0))
    (call $store_f32_at (local.get $xyzmass_ptr) (i32.const 1) (f32.const 0))
    (call $store_f32_at (local.get $xyzmass_ptr) (i32.const 2) (f32.const 0))

    (i32.store (local.get $lmass_ptr) (i32.const 0))
  )

  (func (export "UNITSET")
    (param $unitl f32) (param $unitm f32) (param $unitt f32)
    (param $unitf_ptr i32) (param $units_ptr i32) (param $unitv_ptr i32)
    (param $unita_ptr i32) (param $uniti_ptr i32) (param $unitd_ptr i32)
    (local $unitf f32) (local $units f32) (local $unitv f32)
    (local $unita f32) (local $uniti f32) (local $unitd f32)

    (local.set $unitf (f32.div (f32.mul (local.get $unitm) (local.get $unitl)) (f32.mul (local.get $unitt) (local.get $unitt))))
    (local.set $units (f32.mul (local.get $unitl) (local.get $unitl)))
    (local.set $unitv (f32.div (local.get $unitl) (local.get $unitt)))
    (local.set $unita (f32.div (local.get $unitl) (f32.mul (local.get $unitt) (local.get $unitt))))
    (local.set $uniti (f32.mul (local.get $unitm) (f32.mul (local.get $unitl) (local.get $unitl))))
    (local.set $unitd (f32.div (local.get $unitm) (f32.mul (local.get $unitl) (f32.mul (local.get $unitl) (local.get $unitl)))))

    (f32.store (local.get $unitf_ptr) (local.get $unitf))
    (f32.store (local.get $units_ptr) (local.get $units))
    (f32.store (local.get $unitv_ptr) (local.get $unitv))
    (f32.store (local.get $unita_ptr) (local.get $unita))
    (f32.store (local.get $uniti_ptr) (local.get $uniti))
    (f32.store (local.get $unitd_ptr) (local.get $unitd))
  )

  (func (export "APPGET")
    (param $nstrip i32) (param $unitl f32) (param $pi f32)
    (param $chord_ptr i32) (param $wstrip_ptr i32)
    (param $ensy_ptr i32) (param $ensz_ptr i32)
    (param $rle1_ptr i32) (param $rle2_ptr i32)
    (param $chord1_ptr i32) (param $chord2_ptr i32)
    (param $rle_ptr i32)
    (param $amass_ptr i32) (param $ainer_ptr i32)
    (local $j i32) (local $k i32) (local $l i32)
    (local $cr f32) (local $sr f32) (local $umag f32)
    (local $cperp f32) (local $appm f32) (local $appi f32)
    (local $massScale f32) (local $inerScale f32)

    (local $un_ptr i32) (local $us_ptr i32) (local $uc_ptr i32)
    (local $rm_ptr i32) (local $rxun_ptr i32)
    (local $tmp f32)

    (local.set $un_ptr (i32.const 1024))
    (local.set $us_ptr (i32.const 1040))
    (local.set $uc_ptr (i32.const 1056))
    (local.set $rm_ptr (i32.const 1072))
    (local.set $rxun_ptr (i32.const 1088))

    (local.set $k (i32.const 0))
    (block $k_done
      (loop $k_loop
        (br_if $k_done (i32.ge_s (local.get $k) (i32.const 3)))
        (local.set $l (i32.const 0))
        (block $l_done
          (loop $l_loop
            (br_if $l_done (i32.ge_s (local.get $l) (i32.const 3)))
            (call $store_f32_at (local.get $amass_ptr) (i32.add (local.get $k) (i32.mul (local.get $l) (i32.const 3))) (f32.const 0))
            (call $store_f32_at (local.get $ainer_ptr) (i32.add (local.get $k) (i32.mul (local.get $l) (i32.const 3))) (f32.const 0))
            (local.set $l (i32.add (local.get $l) (i32.const 1)))
            (br $l_loop)
          )
        )
        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $k_loop)
      )
    )

    (local.set $massScale (f32.mul (local.get $unitl) (f32.mul (local.get $unitl) (local.get $unitl))))
    (local.set $inerScale (f32.mul (local.get $massScale) (f32.mul (local.get $unitl) (local.get $unitl))))

    (local.set $j (i32.const 0))
    (block $j_done
      (loop $j_loop
        (br_if $j_done (i32.ge_s (local.get $j) (local.get $nstrip)))

        (local.set $cr (call $load_f32_at (local.get $chord_ptr) (local.get $j)))
        (local.set $sr (f32.mul (local.get $cr) (call $load_f32_at (local.get $wstrip_ptr) (local.get $j))))

        (call $store_f32_at (local.get $un_ptr) (i32.const 0) (f32.const 0))
        (call $store_f32_at (local.get $un_ptr) (i32.const 1) (call $load_f32_at (local.get $ensy_ptr) (local.get $j)))
        (call $store_f32_at (local.get $un_ptr) (i32.const 2) (call $load_f32_at (local.get $ensz_ptr) (local.get $j)))

        (call $store_f32_at (local.get $us_ptr) (i32.const 0)
          (f32.add
            (f32.sub
              (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0)))
              (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0))))
            (f32.mul (f32.const 0.5)
              (f32.sub (call $load_f32_at (local.get $chord2_ptr) (local.get $j))
                       (call $load_f32_at (local.get $chord1_ptr) (local.get $j))))))
        (call $store_f32_at (local.get $us_ptr) (i32.const 1)
          (f32.sub
            (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1)))
            (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1)))))
        (call $store_f32_at (local.get $us_ptr) (i32.const 2)
          (f32.sub
            (call $load_f32_at (local.get $rle2_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2)))
            (call $load_f32_at (local.get $rle1_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2)))))

        (local.set $umag
          (f32.sqrt
            (f32.add
              (f32.add
                (f32.mul (call $load_f32_at (local.get $us_ptr) (i32.const 0)) (call $load_f32_at (local.get $us_ptr) (i32.const 0)))
                (f32.mul (call $load_f32_at (local.get $us_ptr) (i32.const 1)) (call $load_f32_at (local.get $us_ptr) (i32.const 1))))
              (f32.mul (call $load_f32_at (local.get $us_ptr) (i32.const 2)) (call $load_f32_at (local.get $us_ptr) (i32.const 2))))))

        (if (f32.gt (local.get $umag) (f32.const 0))
          (then
            (call $store_f32_at (local.get $us_ptr) (i32.const 0)
              (f32.div (call $load_f32_at (local.get $us_ptr) (i32.const 0)) (local.get $umag)))
            (call $store_f32_at (local.get $us_ptr) (i32.const 1)
              (f32.div (call $load_f32_at (local.get $us_ptr) (i32.const 1)) (local.get $umag)))
            (call $store_f32_at (local.get $us_ptr) (i32.const 2)
              (f32.div (call $load_f32_at (local.get $us_ptr) (i32.const 2)) (local.get $umag)))
          )
        )

        (call $CROSS (local.get $us_ptr) (local.get $un_ptr) (local.get $uc_ptr))

        (call $store_f32_at (local.get $rm_ptr) (i32.const 0)
          (f32.add (call $load_f32_at (local.get $rle_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 0)))
                   (f32.mul (f32.const 0.5) (local.get $cr))))
        (call $store_f32_at (local.get $rm_ptr) (i32.const 1)
          (call $load_f32_at (local.get $rle_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 1))))
        (call $store_f32_at (local.get $rm_ptr) (i32.const 2)
          (call $load_f32_at (local.get $rle_ptr) (i32.add (i32.mul (local.get $j) (i32.const 3)) (i32.const 2))))

        (call $CROSS (local.get $rm_ptr) (local.get $un_ptr) (local.get $rxun_ptr))

        (local.set $cperp
          (f32.mul (local.get $cr)
            (f32.sub
              (f32.mul (call $load_f32_at (local.get $us_ptr) (i32.const 1)) (call $load_f32_at (local.get $un_ptr) (i32.const 2)))
              (f32.mul (call $load_f32_at (local.get $us_ptr) (i32.const 2)) (call $load_f32_at (local.get $un_ptr) (i32.const 1))))))

        (local.set $appm (f32.mul (local.get $sr) (f32.mul (f32.mul (f32.const 0.25) (local.get $pi)) (local.get $cperp))))
        (local.set $appi (f32.mul (local.get $sr)
          (f32.mul
            (f32.mul (f32.const 0.25) (local.get $pi))
            (f32.div (f32.mul (f32.mul (local.get $cperp) (local.get $cperp)) (local.get $cperp)) (f32.const 64)))))

        (local.set $k (i32.const 0))
        (block $k2_done
          (loop $k2_loop
            (br_if $k2_done (i32.ge_s (local.get $k) (i32.const 3)))
            (local.set $l (i32.const 0))
            (block $l2_done
              (loop $l2_loop
                (br_if $l2_done (i32.ge_s (local.get $l) (i32.const 3)))

                (local.set $tmp
                  (f32.add
                    (call $load_f32_at (local.get $amass_ptr) (i32.add (local.get $k) (i32.mul (local.get $l) (i32.const 3))))
                    (f32.mul (local.get $appm)
                      (f32.mul
                        (f32.mul (call $load_f32_at (local.get $un_ptr) (local.get $k))
                                 (call $load_f32_at (local.get $un_ptr) (local.get $l)))
                        (local.get $massScale)))))
                (call $store_f32_at (local.get $amass_ptr) (i32.add (local.get $k) (i32.mul (local.get $l) (i32.const 3))) (local.get $tmp))

                (local.set $tmp
                  (f32.add
                    (call $load_f32_at (local.get $ainer_ptr) (i32.add (local.get $k) (i32.mul (local.get $l) (i32.const 3))))
                    (f32.add
                      (f32.mul (local.get $appm)
                        (f32.mul
                          (f32.mul (call $load_f32_at (local.get $rxun_ptr) (local.get $k))
                                   (call $load_f32_at (local.get $rxun_ptr) (local.get $l)))
                          (local.get $inerScale)))
                      (f32.mul (local.get $appi)
                        (f32.mul
                          (f32.mul (call $load_f32_at (local.get $us_ptr) (local.get $k))
                                   (call $load_f32_at (local.get $us_ptr) (local.get $l)))
                          (local.get $inerScale))))))
                (call $store_f32_at (local.get $ainer_ptr) (i32.add (local.get $k) (i32.mul (local.get $l) (i32.const 3))) (local.get $tmp))

                (local.set $l (i32.add (local.get $l) (i32.const 1)))
                (br $l2_loop)
              )
            )
            (local.set $k (i32.add (local.get $k) (i32.const 1)))
            (br $k2_loop)
          )
        )

        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $j_loop)
      )
    )
  )

  (func (export "MASPUT")
    (param $rmass f32) (param $riner_ptr i32)
    (param $gee0 f32) (param $rho0 f32)
    (param $xyzmass_ptr i32) (param $unitl f32)
    (param $parval_ptr i32) (param $iptot i32) (param $ir1 i32) (param $ir2 i32)
    (param $ipmass i32) (param $ipixx i32) (param $ipiyy i32) (param $ipizz i32)
    (param $ipixy i32) (param $ipiyz i32) (param $ipizx i32)
    (param $ipgee i32) (param $iprho i32)
    (param $ipxcg i32) (param $ipycg i32) (param $ipzcg i32)
    (local $ir i32)
    (local $base i32)
    (local $tmp f32)

    (local.set $ir (local.get $ir1))
    (block $ir_done
      (loop $ir_loop
        (br_if $ir_done (i32.gt_s (local.get $ir) (local.get $ir2)))
        (local.set $base (i32.mul (local.get $iptot) (local.get $ir)))

        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipmass)) (local.get $rmass))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipixx))
          (call $load_f32_at (local.get $riner_ptr) (i32.const 0)))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipiyy))
          (call $load_f32_at (local.get $riner_ptr) (i32.const 4)))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipizz))
          (call $load_f32_at (local.get $riner_ptr) (i32.const 8)))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipixy))
          (call $load_f32_at (local.get $riner_ptr) (i32.const 3)))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipiyz))
          (call $load_f32_at (local.get $riner_ptr) (i32.const 7)))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipizx))
          (call $load_f32_at (local.get $riner_ptr) (i32.const 2)))

        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipgee)) (local.get $gee0))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $iprho)) (local.get $rho0))

        (local.set $tmp (f32.div (call $load_f32_at (local.get $xyzmass_ptr) (i32.const 0)) (local.get $unitl)))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipxcg)) (local.get $tmp))
        (local.set $tmp (f32.div (call $load_f32_at (local.get $xyzmass_ptr) (i32.const 1)) (local.get $unitl)))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipycg)) (local.get $tmp))
        (local.set $tmp (f32.div (call $load_f32_at (local.get $xyzmass_ptr) (i32.const 2)) (local.get $unitl)))
        (call $store_f32_at (local.get $parval_ptr) (i32.add (local.get $base) (local.get $ipzcg)) (local.get $tmp))

        (local.set $ir (i32.add (local.get $ir) (i32.const 1)))
        (br $ir_loop)
      )
    )
  )
)
