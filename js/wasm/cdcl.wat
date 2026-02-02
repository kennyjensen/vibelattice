;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (memory (export "memory") 1)

  (func (export "CDCL") (param $pol i32) (param $cl i32) (param $out i32)
    (local $clmin f32)
    (local $cdmin f32)
    (local $cl0 f32)
    (local $cd0 f32)
    (local $clmax f32)
    (local $cdmax f32)
    (local $clinc f32)
    (local $cdinc f32)
    (local $cdx1 f32)
    (local $cdx2 f32)
    (local $clfac f32)
    (local $clv f32)
    (local $cd f32)
    (local $cd_cl f32)
    (local $tmp f32)
    (local $t1 f32)
    (local $t2 f32)
    (local $den f32)
    (local $order_ok i32)

    (local.set $clmin (f32.load (local.get $pol)))
    (local.set $cdmin (f32.load (i32.add (local.get $pol) (i32.const 4))))
    (local.set $cl0   (f32.load (i32.add (local.get $pol) (i32.const 8))))
    (local.set $cd0   (f32.load (i32.add (local.get $pol) (i32.const 12))))
    (local.set $clmax (f32.load (i32.add (local.get $pol) (i32.const 16))))
    (local.set $cdmax (f32.load (i32.add (local.get $pol) (i32.const 20))))

    (local.set $cd (f32.const 0))
    (local.set $cd_cl (f32.const 0))

    (local.set $order_ok
      (i32.and
        (f32.lt (local.get $cl0) (local.get $clmax))
        (f32.lt (local.get $clmin) (local.get $cl0))))

    (if (i32.eqz (local.get $order_ok))
      (then
        (f32.store (local.get $out) (f32.const 0))
        (f32.store (i32.add (local.get $out) (i32.const 4)) (f32.const 0))
        (return)
      )
    )

    (local.set $clinc (f32.const 0.2))
    (local.set $cdinc (f32.const 0.05))

    (local.set $tmp (f32.sub (local.get $clmin) (local.get $cl0)))
    (local.set $cdx1
      (f32.div
        (f32.mul (f32.const 2)
                 (f32.mul (f32.sub (local.get $cdmin) (local.get $cd0))
                          (f32.sub (local.get $clmin) (local.get $cl0))))
        (f32.mul (local.get $tmp) (local.get $tmp))))

    (local.set $tmp (f32.sub (local.get $clmax) (local.get $cl0)))
    (local.set $cdx2
      (f32.div
        (f32.mul (f32.const 2)
                 (f32.mul (f32.sub (local.get $cdmax) (local.get $cd0))
                          (f32.sub (local.get $clmax) (local.get $cl0))))
        (f32.mul (local.get $tmp) (local.get $tmp))))

    (local.set $clfac (f32.div (f32.const 1) (local.get $clinc)))
    (local.set $clv (f32.load (local.get $cl)))

    (block $done
      ;; cl < clmin
      (if (f32.lt (local.get $clv) (local.get $clmin))
        (then
          (local.set $tmp (f32.sub (local.get $clv) (local.get $clmin)))
          (local.set $t1 (f32.mul (local.get $clfac) (local.get $clfac)))
          (local.set $t1 (f32.mul (local.get $cdinc) (local.get $t1)))
          (local.set $t2 (f32.mul (local.get $tmp) (local.get $tmp)))
          (local.set $cd (f32.add (local.get $cdmin) (f32.mul (local.get $t1) (local.get $t2))))
          (local.set $den (f32.sub (local.get $clmin) (local.get $cl0)))
          (local.set $t2 (f32.div (f32.sub (local.get $clv) (local.get $cl0)) (local.get $den)))
          (local.set $cd (f32.add (local.get $cd) (f32.mul (local.get $cdx1) (f32.sub (f32.const 1) (local.get $t2)))))
          (local.set $cd_cl (f32.mul (local.get $t1) (f32.mul (local.get $tmp) (f32.const 2))))
          (local.set $cd_cl (f32.sub (local.get $cd_cl) (f32.div (local.get $cdx1) (local.get $den))))
          (br $done)
        )
      )

      ;; cl < cl0
      (if (f32.lt (local.get $clv) (local.get $cl0))
        (then
          (local.set $tmp (f32.sub (local.get $clv) (local.get $cl0)))
          (local.set $den (f32.sub (local.get $clmin) (local.get $cl0)))
          (local.set $t1 (f32.mul (local.get $tmp) (local.get $tmp)))
          (local.set $t2 (f32.mul (local.get $den) (local.get $den)))
          (local.set $cd (f32.add (local.get $cd0)
            (f32.div (f32.mul (f32.sub (local.get $cdmin) (local.get $cd0)) (local.get $t1)) (local.get $t2))))
          (local.set $cd_cl (f32.div
            (f32.mul (f32.sub (local.get $cdmin) (local.get $cd0)) (f32.mul (local.get $tmp) (f32.const 2)))
            (local.get $t2)))
          (br $done)
        )
      )

      ;; cl < clmax
      (if (f32.lt (local.get $clv) (local.get $clmax))
        (then
          (local.set $tmp (f32.sub (local.get $clv) (local.get $cl0)))
          (local.set $den (f32.sub (local.get $clmax) (local.get $cl0)))
          (local.set $t1 (f32.mul (local.get $tmp) (local.get $tmp)))
          (local.set $t2 (f32.mul (local.get $den) (local.get $den)))
          (local.set $cd (f32.add (local.get $cd0)
            (f32.div (f32.mul (f32.sub (local.get $cdmax) (local.get $cd0)) (local.get $t1)) (local.get $t2))))
          (local.set $cd_cl (f32.div
            (f32.mul (f32.sub (local.get $cdmax) (local.get $cd0)) (f32.mul (local.get $tmp) (f32.const 2)))
            (local.get $t2)))
          (br $done)
        )
      )

      ;; cl >= clmax
      (local.set $tmp (f32.sub (local.get $clv) (local.get $clmax)))
      (local.set $t1 (f32.mul (local.get $clfac) (local.get $clfac)))
      (local.set $t1 (f32.mul (local.get $cdinc) (local.get $t1)))
      (local.set $t2 (f32.mul (local.get $tmp) (local.get $tmp)))
      (local.set $cd (f32.add (local.get $cdmax) (f32.mul (local.get $t1) (local.get $t2))))
      (local.set $den (f32.sub (local.get $clmax) (local.get $cl0)))
      (local.set $t2 (f32.div (f32.sub (local.get $clv) (local.get $cl0)) (local.get $den)))
      (local.set $cd (f32.sub (local.get $cd) (f32.mul (local.get $cdx2) (f32.sub (f32.const 1) (local.get $t2)))))
      (local.set $cd_cl (f32.mul (local.get $t1) (f32.mul (local.get $tmp) (f32.const 2))))
      (local.set $cd_cl (f32.add (local.get $cd_cl) (f32.div (local.get $cdx2) (local.get $den))))
    )

    (f32.store (local.get $out) (local.get $cd))
    (f32.store (i32.add (local.get $out) (i32.const 4)) (local.get $cd_cl))
  )
)
