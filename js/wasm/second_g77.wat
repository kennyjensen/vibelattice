;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (import "env" "now" (func $now (result f64)))
  (func (export "SECONDS") (result f64)
    (call $now)
  )
)
