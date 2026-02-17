;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (import "env" "sysmat_js" (func $sysmat_js (param i32)))
  (import "env" "appmat_js" (func $appmat_js (param i32)))
  (import "env" "eigsol_js" (func $eigsol_js (param i32 f64 i32)))
  (memory (export "memory") 1)

  (func $i32_load (param $base i32) (param $idx i32) (result i32)
    (i32.load
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))))

  (func $f32_load (param $base i32) (param $idx i32) (result f32)
    (f32.load
      (i32.add (local.get $base) (i32.mul (local.get $idx) (i32.const 4)))))

  ;; Legacy entry kept for compatibility; use RUNCHK_NATIVE for direct wasm execution.
  (func (export "RUNCHK") (param $run i32)
    (drop (local.get $run)))

  ;; RUNCHK(state, run) operating on ICON array copied into wasm memory.
  ;; ICON indexing is 1-based: ICON[iv + IVMAX * run].
  (func (export "RUNCHK_NATIVE")
    (param $icon_ptr i32) (param $nvtot i32) (param $ivmax i32) (param $run i32)
    (result i32)
    (local $iv i32) (local $jv i32)
    (local $idx1 i32) (local $idx2 i32)
    (local $ic1 i32) (local $ic2 i32)

    (local.set $iv (i32.const 1))
    (block $fail
      (block $ok
        (loop $iv_loop
          (br_if $ok (i32.gt_s (local.get $iv) (local.get $nvtot)))
          (local.set $jv (i32.const 1))
          (block $jv_done
            (loop $jv_loop
              (br_if $jv_done (i32.gt_s (local.get $jv) (local.get $nvtot)))
              (if (i32.ne (local.get $iv) (local.get $jv))
                (then
                  (local.set $idx1
                    (i32.add (local.get $iv)
                      (i32.mul (local.get $ivmax) (local.get $run))))
                  (local.set $idx2
                    (i32.add (local.get $jv)
                      (i32.mul (local.get $ivmax) (local.get $run))))
                  (local.set $ic1 (call $i32_load (local.get $icon_ptr) (local.get $idx1)))
                  (local.set $ic2 (call $i32_load (local.get $icon_ptr) (local.get $idx2)))
                  (if (i32.eq (local.get $ic1) (local.get $ic2))
                    (then (br $fail)))
                )
              )
              (local.set $jv (i32.add (local.get $jv) (i32.const 1)))
              (br $jv_loop)
            )
          )
          (local.set $iv (i32.add (local.get $iv) (i32.const 1)))
          (br $iv_loop)
        )
      )
      (return (i32.const 1))
    )
    (i32.const 0))

  ;; Shared SYSMAT/APPMAT validity gate:
  ;; return 1 when VEE>0, MASS>0, IXX>0, IYY>0, IZZ>0; otherwise 0.
  (func $amode_precheck
    (param $parval_ptr i32) (param $iptot i32) (param $ir i32)
    (param $ipvee i32) (param $ipmass i32)
    (param $ipixx i32) (param $ipiyy i32) (param $ipizz i32)
    (result i32)
    (local $base i32)
    (local $vee f32) (local $mass f32)
    (local $ixx f32) (local $iyy f32) (local $izz f32)

    (local.set $base (i32.mul (local.get $iptot) (local.get $ir)))
    (local.set $vee (call $f32_load (local.get $parval_ptr) (i32.add (local.get $ipvee) (local.get $base))))
    (local.set $mass (call $f32_load (local.get $parval_ptr) (i32.add (local.get $ipmass) (local.get $base))))
    (local.set $ixx (call $f32_load (local.get $parval_ptr) (i32.add (local.get $ipixx) (local.get $base))))
    (local.set $iyy (call $f32_load (local.get $parval_ptr) (i32.add (local.get $ipiyy) (local.get $base))))
    (local.set $izz (call $f32_load (local.get $parval_ptr) (i32.add (local.get $ipizz) (local.get $base))))

    (if (f32.le (local.get $vee) (f32.const 0)) (then (return (i32.const 0))))
    (if (f32.le (local.get $mass) (f32.const 0)) (then (return (i32.const 0))))
    (if (f32.le (local.get $ixx) (f32.const 0)) (then (return (i32.const 0))))
    (if (f32.le (local.get $iyy) (f32.const 0)) (then (return (i32.const 0))))
    (if (f32.le (local.get $izz) (f32.const 0)) (then (return (i32.const 0))))
    (i32.const 1))

  (func (export "SYSMAT_PRECHECK")
    (param $parval_ptr i32) (param $iptot i32) (param $ir i32)
    (param $ipvee i32) (param $ipmass i32)
    (param $ipixx i32) (param $ipiyy i32) (param $ipizz i32)
    (result i32)
    (call $amode_precheck
      (local.get $parval_ptr) (local.get $iptot) (local.get $ir)
      (local.get $ipvee) (local.get $ipmass)
      (local.get $ipixx) (local.get $ipiyy) (local.get $ipizz)))

  (func (export "APPMAT_PRECHECK")
    (param $parval_ptr i32) (param $iptot i32) (param $ir i32)
    (param $ipvee i32) (param $ipmass i32)
    (param $ipixx i32) (param $ipiyy i32) (param $ipizz i32)
    (result i32)
    (call $amode_precheck
      (local.get $parval_ptr) (local.get $iptot) (local.get $ir)
      (local.get $ipvee) (local.get $ipmass)
      (local.get $ipixx) (local.get $ipiyy) (local.get $ipizz)))

  ;; EIGSOL gate: require NSYS > 0.
  (func (export "EIGSOL_PRECHECK") (param $nsys i32) (result i32)
    (if (i32.le_s (local.get $nsys) (i32.const 0))
      (then (return (i32.const 0))))
    (i32.const 1))

  (func (export "SYSMAT") (param $ir i32)
    (call $sysmat_js (local.get $ir)))
  (func (export "APPMAT") (param $ir i32)
    (call $appmat_js (local.get $ir)))
  ;; SYSSHO text formatting is handled in JS wrapper for now.
  (func (export "SYSSHO") (param $n i32)
    (drop (local.get $n)))
  (func (export "EIGSOL") (param $ir i32) (param $etol f64) (param $nsys i32)
    (call $eigsol_js (local.get $ir) (local.get $etol) (local.get $nsys)))
)
