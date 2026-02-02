;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (import "env" "exec_js" (func $exec_js (param i32) (param i32) (param i32) (param i32)))
  (memory (export "memory") 1)

  (func (export "EXEC")
    (param $niter i32) (param $info i32) (param $ir i32) (param $state_ptr i32)
    (call $exec_js (local.get $niter) (local.get $info) (local.get $ir) (local.get $state_ptr))
  )
)
