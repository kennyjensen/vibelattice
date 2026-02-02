;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (memory (export "memory") 1)

  (func (export "AVL_INIT") (param $out i32)
    (local $pi f32)
    (local.set $pi (f32.const 3.14159274))

    (f32.store (local.get $out) (f32.const 3.52))
    (f32.store (i32.add (local.get $out) (i32.const 4)) (local.get $pi))
    (f32.store (i32.add (local.get $out) (i32.const 8))
      (f32.div (local.get $pi) (f32.const 180)))

    (i32.store (i32.add (local.get $out) (i32.const 12)) (i32.const 4))
    (i32.store (i32.add (local.get $out) (i32.const 16)) (i32.const 7))
    (i32.store (i32.add (local.get $out) (i32.const 20)) (i32.const 19))
    (i32.store (i32.add (local.get $out) (i32.const 24)) (i32.const 20))
    (i32.store (i32.add (local.get $out) (i32.const 28)) (i32.const 22))

    (f32.store (i32.add (local.get $out) (i32.const 32)) (f32.const 1))

    (i32.store (i32.add (local.get $out) (i32.const 36)) (i32.const 1))
    (i32.store (i32.add (local.get $out) (i32.const 40)) (i32.const 1))
    (i32.store (i32.add (local.get $out) (i32.const 44)) (i32.const 1))
    (i32.store (i32.add (local.get $out) (i32.const 48)) (i32.const 1))
    (i32.store (i32.add (local.get $out) (i32.const 52)) (i32.const 1))
  )
)
