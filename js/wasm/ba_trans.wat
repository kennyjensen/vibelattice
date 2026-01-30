(module
  (import "env" "sin" (func $sin (param f32) (result f32)))
  (import "env" "cos" (func $cos (param f32) (result f32)))

  (memory (export "memory") 1)

  (func (export "BA2WA_MAT") (param $alfa f32) (param $beta f32) (param $binv f32)
                              (param $p i32) (param $p_a i32) (param $p_b i32)
    (local $sina f32) (local $cosa f32) (local $sinb f32) (local $cosb f32)

    (local.set $sina (call $sin (local.get $alfa)))
    (local.set $cosa (call $cos (local.get $alfa)))
    (local.set $sinb (call $sin (local.get $beta)))
    (local.set $cosb (call $cos (local.get $beta)))

    ;; P (row-major)
    (f32.store (local.get $p) (f32.mul (f32.mul (local.get $cosa) (local.get $cosb)) (local.get $binv)))
    (f32.store (i32.add (local.get $p) (i32.const 4)) (f32.mul (f32.neg (local.get $sinb)) (local.get $binv)))
    (f32.store (i32.add (local.get $p) (i32.const 8)) (f32.mul (f32.mul (local.get $sina) (local.get $cosb)) (local.get $binv)))

    (f32.store (i32.add (local.get $p) (i32.const 12)) (f32.mul (local.get $cosa) (local.get $sinb)))
    (f32.store (i32.add (local.get $p) (i32.const 16)) (local.get $cosb))
    (f32.store (i32.add (local.get $p) (i32.const 20)) (f32.mul (local.get $sina) (local.get $sinb)))

    (f32.store (i32.add (local.get $p) (i32.const 24)) (f32.neg (local.get $sina)))
    (f32.store (i32.add (local.get $p) (i32.const 28)) (f32.const 0))
    (f32.store (i32.add (local.get $p) (i32.const 32)) (local.get $cosa))

    ;; P_A
    (f32.store (local.get $p_a) (f32.mul (f32.neg (local.get $sina)) (local.get $cosb)))
    (f32.store (i32.add (local.get $p_a) (i32.const 4)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 8)) (f32.mul (local.get $cosa) (local.get $cosb)))

    (f32.store (i32.add (local.get $p_a) (i32.const 12)) (f32.mul (f32.neg (local.get $sina)) (local.get $sinb)))
    (f32.store (i32.add (local.get $p_a) (i32.const 16)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 20)) (f32.mul (local.get $cosa) (local.get $sinb)))

    (f32.store (i32.add (local.get $p_a) (i32.const 24)) (f32.neg (local.get $cosa)))
    (f32.store (i32.add (local.get $p_a) (i32.const 28)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 32)) (f32.neg (local.get $sina)))

    ;; P_B
    (f32.store (local.get $p_b) (f32.mul (f32.neg (local.get $cosa)) (local.get $sinb)))
    (f32.store (i32.add (local.get $p_b) (i32.const 4)) (f32.neg (local.get $cosb)))
    (f32.store (i32.add (local.get $p_b) (i32.const 8)) (f32.mul (f32.neg (local.get $sina)) (local.get $sinb)))

    (f32.store (i32.add (local.get $p_b) (i32.const 12)) (f32.mul (local.get $cosa) (local.get $cosb)))
    (f32.store (i32.add (local.get $p_b) (i32.const 16)) (f32.neg (local.get $sinb)))
    (f32.store (i32.add (local.get $p_b) (i32.const 20)) (f32.mul (local.get $sina) (local.get $cosb)))

    (f32.store (i32.add (local.get $p_b) (i32.const 24)) (f32.const 0))
    (f32.store (i32.add (local.get $p_b) (i32.const 28)) (f32.const 0))
    (f32.store (i32.add (local.get $p_b) (i32.const 32)) (f32.const 0))
  )

  (func (export "BA2SA_MAT") (param $alfa f32) (param $p i32) (param $p_a i32)
    (local $sina f32) (local $cosa f32)

    (local.set $sina (call $sin (local.get $alfa)))
    (local.set $cosa (call $cos (local.get $alfa)))

    (f32.store (local.get $p) (local.get $cosa))
    (f32.store (i32.add (local.get $p) (i32.const 4)) (f32.const 0))
    (f32.store (i32.add (local.get $p) (i32.const 8)) (local.get $sina))

    (f32.store (i32.add (local.get $p) (i32.const 12)) (f32.const 0))
    (f32.store (i32.add (local.get $p) (i32.const 16)) (f32.const 1))
    (f32.store (i32.add (local.get $p) (i32.const 20)) (f32.const 0))

    (f32.store (i32.add (local.get $p) (i32.const 24)) (f32.neg (local.get $sina)))
    (f32.store (i32.add (local.get $p) (i32.const 28)) (f32.const 0))
    (f32.store (i32.add (local.get $p) (i32.const 32)) (local.get $cosa))

    (f32.store (local.get $p_a) (f32.neg (local.get $sina)))
    (f32.store (i32.add (local.get $p_a) (i32.const 4)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 8)) (local.get $cosa))

    (f32.store (i32.add (local.get $p_a) (i32.const 12)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 16)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 20)) (f32.const 0))

    (f32.store (i32.add (local.get $p_a) (i32.const 24)) (f32.neg (local.get $cosa)))
    (f32.store (i32.add (local.get $p_a) (i32.const 28)) (f32.const 0))
    (f32.store (i32.add (local.get $p_a) (i32.const 32)) (f32.neg (local.get $sina)))
  )
)
