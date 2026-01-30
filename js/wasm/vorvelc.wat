(module
  (memory (export "memory") 1)

  (func (export "VORVELC")
    (param $x f32) (param $y f32) (param $z f32)
    (param $lbound i32)
    (param $x1 f32) (param $y1 f32) (param $z1 f32)
    (param $x2 f32) (param $y2 f32) (param $z2 f32)
    (param $beta f32) (param $rcore f32)
    (param $out i32)
    (local $a1 f32) (local $a2 f32) (local $a3 f32)
    (local $b1 f32) (local $b2 f32) (local $b3 f32)
    (local $asq f32) (local $bsq f32)
    (local $amag f32) (local $bmag f32)
    (local $rcore2 f32) (local $rcore4 f32)
    (local $u f32) (local $v f32) (local $w f32)
    (local $axb1 f32) (local $axb2 f32) (local $axb3 f32)
    (local $axbsq f32) (local $adb f32) (local $alsq f32)
    (local $t f32) (local $t1 f32) (local $t2 f32) (local $den f32)
    (local $axisq f32) (local $rsq f32)
    (local $s1 f32) (local $s2 f32)

    (local.set $a1 (f32.div (f32.sub (local.get $x1) (local.get $x)) (local.get $beta)))
    (local.set $a2 (f32.sub (local.get $y1) (local.get $y)))
    (local.set $a3 (f32.sub (local.get $z1) (local.get $z)))

    (local.set $b1 (f32.div (f32.sub (local.get $x2) (local.get $x)) (local.get $beta)))
    (local.set $b2 (f32.sub (local.get $y2) (local.get $y)))
    (local.set $b3 (f32.sub (local.get $z2) (local.get $z)))

    (local.set $asq (f32.add (f32.add (f32.mul (local.get $a1) (local.get $a1))
                                      (f32.mul (local.get $a2) (local.get $a2)))
                             (f32.mul (local.get $a3) (local.get $a3))))

    (local.set $bsq (f32.add (f32.add (f32.mul (local.get $b1) (local.get $b1))
                                      (f32.mul (local.get $b2) (local.get $b2)))
                             (f32.mul (local.get $b3) (local.get $b3))))

    (local.set $amag (f32.sqrt (local.get $asq)))
    (local.set $bmag (f32.sqrt (local.get $bsq)))

    (local.set $rcore2 (f32.mul (local.get $rcore) (local.get $rcore)))
    (local.set $rcore4 (f32.mul (local.get $rcore2) (local.get $rcore2)))

    (local.set $u (f32.const 0))
    (local.set $v (f32.const 0))
    (local.set $w (f32.const 0))

    ;; bound leg
    (if (i32.ne (local.get $lbound) (i32.const 0))
      (then
        (if (f32.ne (f32.mul (local.get $amag) (local.get $bmag)) (f32.const 0))
          (then
            (local.set $axb1 (f32.sub (f32.mul (local.get $a2) (local.get $b3))
                                      (f32.mul (local.get $a3) (local.get $b2))))
            (local.set $axb2 (f32.sub (f32.mul (local.get $a3) (local.get $b1))
                                      (f32.mul (local.get $a1) (local.get $b3))))
            (local.set $axb3 (f32.sub (f32.mul (local.get $a1) (local.get $b2))
                                      (f32.mul (local.get $a2) (local.get $b1))))

            (local.set $axbsq (f32.add (f32.add (f32.mul (local.get $axb1) (local.get $axb1))
                                               (f32.mul (local.get $axb2) (local.get $axb2)))
                                      (f32.mul (local.get $axb3) (local.get $axb3))))

            (if (f32.ne (local.get $axbsq) (f32.const 0))
              (then
                (local.set $adb (f32.add (f32.add (f32.mul (local.get $a1) (local.get $b1))
                                                  (f32.mul (local.get $a2) (local.get $b2)))
                                         (f32.mul (local.get $a3) (local.get $b3))))

                (local.set $alsq (f32.sub (f32.add (local.get $asq) (local.get $bsq))
                                          (f32.mul (f32.const 2) (local.get $adb))))

                (local.set $t1 (f32.sub (local.get $bsq) (local.get $adb)))
                (local.set $t2 (f32.sub (local.get $asq) (local.get $adb)))

                (local.set $s1 (f32.add (f32.mul (local.get $bsq) (local.get $bsq)) (local.get $rcore4)))
                (local.set $s1 (f32.sqrt (f32.sqrt (local.get $s1))))
                (local.set $s2 (f32.add (f32.mul (local.get $asq) (local.get $asq)) (local.get $rcore4)))
                (local.set $s2 (f32.sqrt (f32.sqrt (local.get $s2))))

                (local.set $t1 (f32.div (local.get $t1) (local.get $s1)))
                (local.set $t2 (f32.div (local.get $t2) (local.get $s2)))

                (local.set $den (f32.mul (local.get $axbsq) (local.get $axbsq)))
                (local.set $t (f32.mul (local.get $alsq) (local.get $alsq)))
                (local.set $t (f32.mul (local.get $t) (local.get $rcore4)))
                (local.set $den (f32.add (local.get $den) (local.get $t)))
                (local.set $den (f32.sqrt (local.get $den)))

                (local.set $t (f32.div (f32.add (local.get $t1) (local.get $t2)) (local.get $den)))
                (local.set $u (f32.mul (local.get $axb1) (local.get $t)))
                (local.set $v (f32.mul (local.get $axb2) (local.get $t)))
                (local.set $w (f32.mul (local.get $axb3) (local.get $t)))
              )
            )
          )
        )
      )
    )

    ;; trailing leg A
    (if (f32.ne (local.get $amag) (f32.const 0))
      (then
        (local.set $axisq (f32.add (f32.mul (local.get $a3) (local.get $a3))
                                   (f32.mul (local.get $a2) (local.get $a2))))
        (local.set $rsq (local.get $axisq))
        (local.set $t (f32.sub (f32.const 1) (f32.div (local.get $a1) (local.get $amag))))
        (local.set $den (f32.add (f32.mul (local.get $rsq) (local.get $rsq)) (local.get $rcore4)))
        (local.set $t (f32.neg (f32.div (local.get $t) (f32.sqrt (local.get $den)))))
        (local.set $v (f32.add (local.get $v) (f32.mul (local.get $a3) (local.get $t))))
        (local.set $w (f32.sub (local.get $w) (f32.mul (local.get $a2) (local.get $t))))
      )
    )

    ;; trailing leg B
    (if (f32.ne (local.get $bmag) (f32.const 0))
      (then
        (local.set $axisq (f32.add (f32.mul (local.get $b3) (local.get $b3))
                                   (f32.mul (local.get $b2) (local.get $b2))))
        (local.set $rsq (local.get $axisq))
        (local.set $t (f32.sub (f32.const 1) (f32.div (local.get $b1) (local.get $bmag))))
        (local.set $den (f32.add (f32.mul (local.get $rsq) (local.get $rsq)) (local.get $rcore4)))
        (local.set $t (f32.div (local.get $t) (f32.sqrt (local.get $den))))
        (local.set $v (f32.add (local.get $v) (f32.mul (local.get $b3) (local.get $t))))
        (local.set $w (f32.sub (local.get $w) (f32.mul (local.get $b2) (local.get $t))))
      )
    )

    (local.set $u (f32.div (f32.mul (local.get $u) (f32.const 0.079577472)) (local.get $beta)))
    (local.set $v (f32.mul (local.get $v) (f32.const 0.079577472)))
    (local.set $w (f32.mul (local.get $w) (f32.const 0.079577472)))

    (f32.store (local.get $out) (local.get $u))
    (f32.store (i32.add (local.get $out) (i32.const 4)) (local.get $v))
    (f32.store (i32.add (local.get $out) (i32.const 8)) (local.get $w))
  )
)
