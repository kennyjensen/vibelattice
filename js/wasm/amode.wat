;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (import "env" "runchk_js" (func $runchk_js (param i32)))
  (import "env" "sysmat_js" (func $sysmat_js (param i32)))
  (import "env" "appmat_js" (func $appmat_js (param i32)))
  (import "env" "syssho_js" (func $syssho_js (param i32)))
  (memory (export "memory") 1)

  (func (export "RUNCHK") (param $run i32)
    (call $runchk_js (local.get $run)))
  (func (export "SYSMAT") (param $ir i32)
    (call $sysmat_js (local.get $ir)))
  (func (export "APPMAT") (param $ir i32)
    (call $appmat_js (local.get $ir)))
  (func (export "SYSSHO") (param $n i32)
    (call $syssho_js (local.get $n)))
)
