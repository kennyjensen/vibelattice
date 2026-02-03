;; Ported from AVL Fortran source by Mark Drela and Harold Youngren.
;; Derived work under GPL-2.0.
;; Original source: https://web.mit.edu/drela/Public/web/avl/
(module
  (memory (export "memory") 2)

  ;; packed vectors (x,y,z) contiguous
  ;; idx3(r, c) = r + 3*c
  (func $idx3 (param $r i32) (param $c i32) (result i32)
    (i32.add (local.get $r)
      (i32.mul (i32.const 3) (local.get $c)))
  )
  (func $idx2 (param $i i32) (param $j i32) (param $dim1 i32) (result i32)
    (i32.add (local.get $i) (i32.mul (local.get $dim1) (local.get $j)))
  )

  ;; packed vectors (x,y,z) contiguous for (i,j) grids
  ;; idx3c(r,i,j,dim1) = r + 3*(i + dim1*j)
  (func $idx3c (param $r i32) (param $i i32) (param $j i32) (param $dim1 i32) (result i32)
    (i32.add (local.get $r)
      (i32.mul (i32.const 3)
        (i32.add (local.get $i) (i32.mul (local.get $dim1) (local.get $j)))))
  )

  (func (export "CROSS") (param $u i32) (param $v i32) (param $w i32)
    (local $u1 f32) (local $u2 f32) (local $u3 f32)
    (local $v1 f32) (local $v2 f32) (local $v3 f32)
    (local.set $u1 (f32.load (local.get $u)))
    (local.set $u2 (f32.load (i32.add (local.get $u) (i32.const 4))))
    (local.set $u3 (f32.load (i32.add (local.get $u) (i32.const 8))))
    (local.set $v1 (f32.load (local.get $v)))
    (local.set $v2 (f32.load (i32.add (local.get $v) (i32.const 4))))
    (local.set $v3 (f32.load (i32.add (local.get $v) (i32.const 8))))
    (f32.store (local.get $w)
      (f32.sub (f32.mul (local.get $u2) (local.get $v3))
               (f32.mul (local.get $u3) (local.get $v2))))
    (f32.store (i32.add (local.get $w) (i32.const 4))
      (f32.sub (f32.mul (local.get $u3) (local.get $v1))
               (f32.mul (local.get $u1) (local.get $v3))))
    (f32.store (i32.add (local.get $w) (i32.const 8))
      (f32.sub (f32.mul (local.get $u1) (local.get $v2))
               (f32.mul (local.get $u2) (local.get $v1))))
  )

  (func (export "DOT") (param $u i32) (param $v i32) (result f32)
    (local $u1 f32) (local $u2 f32) (local $u3 f32)
    (local $v1 f32) (local $v2 f32) (local $v3 f32)
    (local.set $u1 (f32.load (local.get $u)))
    (local.set $u2 (f32.load (i32.add (local.get $u) (i32.const 4))))
    (local.set $u3 (f32.load (i32.add (local.get $u) (i32.const 8))))
    (local.set $v1 (f32.load (local.get $v)))
    (local.set $v2 (f32.load (i32.add (local.get $v) (i32.const 4))))
    (local.set $v3 (f32.load (i32.add (local.get $v) (i32.const 8))))
    (f32.add
      (f32.add (f32.mul (local.get $u1) (local.get $v1))
               (f32.mul (local.get $u2) (local.get $v2)))
      (f32.mul (local.get $u3) (local.get $v3)))
  )


  (func $VORVELC (export "VORVELC")
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
    (local $rc2 f32) (local $rc4 f32)
    (local $u f32) (local $v f32) (local $w f32)
    (local $axb1 f32) (local $axb2 f32) (local $axb3 f32)
    (local $axbsq f32) (local $adb f32) (local $alsq f32)
    (local $t f32) (local $rsq f32)
    (local $pi4inv f32)
    (local $tmp f32) (local $tmp2 f32) (local $num f32) (local $den f32)

    (local.set $pi4inv (f32.const 0.079577472))

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

    (local.set $rc2 (f32.mul (local.get $rcore) (local.get $rcore)))
    (local.set $rc4 (f32.mul (local.get $rc2) (local.get $rc2)))

    (local.set $u (f32.const 0))
    (local.set $v (f32.const 0))
    (local.set $w (f32.const 0))

    (if (i32.and (local.get $lbound)
                 (f32.ne (f32.mul (local.get $amag) (local.get $bmag)) (f32.const 0)))
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

            (local.set $tmp (f32.sub (local.get $bsq) (local.get $adb)))
            (local.set $tmp2 (f32.sqrt (f32.sqrt (f32.add (f32.mul (local.get $bsq) (local.get $bsq)) (local.get $rc4)))))
            (local.set $num (f32.div (local.get $tmp) (local.get $tmp2)))

            (local.set $tmp (f32.sub (local.get $asq) (local.get $adb)))
            (local.set $tmp2 (f32.sqrt (f32.sqrt (f32.add (f32.mul (local.get $asq) (local.get $asq)) (local.get $rc4)))))
            (local.set $num (f32.add (local.get $num) (f32.div (local.get $tmp) (local.get $tmp2))))

            (local.set $den (f32.sqrt (f32.add
              (f32.mul (local.get $axbsq) (local.get $axbsq))
              (f32.mul (f32.mul (local.get $alsq) (local.get $alsq)) (local.get $rc4))
            )))

            (local.set $t (f32.div (local.get $num) (local.get $den)))
            (local.set $u (f32.mul (local.get $axb1) (local.get $t)))
            (local.set $v (f32.mul (local.get $axb2) (local.get $t)))
            (local.set $w (f32.mul (local.get $axb3) (local.get $t)))
          )
        )
      )
    )

    (if (f32.ne (local.get $amag) (f32.const 0))
      (then
        (local.set $rsq (f32.add (f32.mul (local.get $a3) (local.get $a3))
                                 (f32.mul (local.get $a2) (local.get $a2))))
        (local.set $t
          (f32.neg
            (f32.div
              (f32.sub (f32.const 1) (f32.div (local.get $a1) (local.get $amag)))
              (f32.sqrt (f32.add (f32.mul (local.get $rsq) (local.get $rsq)) (local.get $rc4)))
            )
          )
        )
        (local.set $v (f32.add (local.get $v) (f32.mul (local.get $a3) (local.get $t))))
        (local.set $w (f32.sub (local.get $w) (f32.mul (local.get $a2) (local.get $t))))
      )
    )

    (if (f32.ne (local.get $bmag) (f32.const 0))
      (then
        (local.set $rsq (f32.add (f32.mul (local.get $b3) (local.get $b3))
                                 (f32.mul (local.get $b2) (local.get $b2))))
        (local.set $t
          (f32.div
            (f32.sub (f32.const 1) (f32.div (local.get $b1) (local.get $bmag)))
            (f32.sqrt (f32.add (f32.mul (local.get $rsq) (local.get $rsq)) (local.get $rc4)))
          )
        )
        (local.set $v (f32.add (local.get $v) (f32.mul (local.get $b3) (local.get $t))))
        (local.set $w (f32.sub (local.get $w) (f32.mul (local.get $b2) (local.get $t))))
      )
    )

    (local.set $u (f32.div (f32.mul (local.get $u) (local.get $pi4inv)) (local.get $beta)))
    (local.set $v (f32.mul (local.get $v) (local.get $pi4inv)))
    (local.set $w (f32.mul (local.get $w) (local.get $pi4inv)))

    (f32.store (local.get $out) (local.get $u))
    (f32.store (i32.add (local.get $out) (i32.const 4)) (local.get $v))
    (f32.store (i32.add (local.get $out) (i32.const 8)) (local.get $w))
  )
  (func $SRDVELC (export "SRDVELC")
    (param $x f32) (param $y f32) (param $z f32)
    (param $x1 f32) (param $y1 f32) (param $z1 f32)
    (param $x2 f32) (param $y2 f32) (param $z2 f32)
    (param $beta f32) (param $rcore f32)
    (param $uvws i32) (param $uvwd i32)
    (local $pi4inv f32)
    (local $r1_0 f32) (local $r1_1 f32) (local $r1_2 f32)
    (local $r2_0 f32) (local $r2_1 f32) (local $r2_2 f32)
    (local $rcsq f32)
    (local $r1sq f32) (local $r2sq f32)
    (local $r1sqeps f32) (local $r2sqeps f32)
    (local $r1eps f32) (local $r2eps f32)
    (local $rdr f32)
    (local $rxr1 f32) (local $rxr2 f32) (local $rxr3 f32)
    (local $xdx f32) (local $all f32) (local $den f32)
    (local $ai1 f32) (local $ai2 f32)
    (local $k i32) (local $j i32)
    (local $r1k f32) (local $r2k f32)
    (local $rr1 f32) (local $rr2 f32) (local $rrt f32)
    (local $aj1 f32) (local $aj2 f32)
    (local $r1j f32) (local $r2j f32)

    (local.set $pi4inv (f32.const 0.079577472))

    (local.set $r1_0 (f32.div (f32.sub (local.get $x1) (local.get $x)) (local.get $beta)))
    (local.set $r1_1 (f32.sub (local.get $y1) (local.get $y)))
    (local.set $r1_2 (f32.sub (local.get $z1) (local.get $z)))
    (local.set $r2_0 (f32.div (f32.sub (local.get $x2) (local.get $x)) (local.get $beta)))
    (local.set $r2_1 (f32.sub (local.get $y2) (local.get $y)))
    (local.set $r2_2 (f32.sub (local.get $z2) (local.get $z)))

    (local.set $rcsq (f32.mul (local.get $rcore) (local.get $rcore)))

    (local.set $r1sq (f32.add (f32.add (f32.mul (local.get $r1_0) (local.get $r1_0))
                                       (f32.mul (local.get $r1_1) (local.get $r1_1)))
                              (f32.mul (local.get $r1_2) (local.get $r1_2))))
    (local.set $r2sq (f32.add (f32.add (f32.mul (local.get $r2_0) (local.get $r2_0))
                                       (f32.mul (local.get $r2_1) (local.get $r2_1)))
                              (f32.mul (local.get $r2_2) (local.get $r2_2))))

    (local.set $r1sqeps (f32.add (local.get $r1sq) (local.get $rcsq)))
    (local.set $r2sqeps (f32.add (local.get $r2sq) (local.get $rcsq)))

    (local.set $r1eps (f32.sqrt (local.get $r1sqeps)))
    (local.set $r2eps (f32.sqrt (local.get $r2sqeps)))

    (local.set $rdr (f32.add (f32.add (f32.mul (local.get $r1_0) (local.get $r2_0))
                                      (f32.mul (local.get $r1_1) (local.get $r2_1)))
                             (f32.mul (local.get $r1_2) (local.get $r2_2))))

    (local.set $rxr1 (f32.sub (f32.mul (local.get $r1_1) (local.get $r2_2))
                              (f32.mul (local.get $r1_2) (local.get $r2_1))))
    (local.set $rxr2 (f32.sub (f32.mul (local.get $r1_2) (local.get $r2_0))
                              (f32.mul (local.get $r1_0) (local.get $r2_2))))
    (local.set $rxr3 (f32.sub (f32.mul (local.get $r1_0) (local.get $r2_1))
                              (f32.mul (local.get $r1_1) (local.get $r2_0))))

    (local.set $xdx (f32.add (f32.add (f32.mul (local.get $rxr1) (local.get $rxr1))
                                      (f32.mul (local.get $rxr2) (local.get $rxr2)))
                             (f32.mul (local.get $rxr3) (local.get $rxr3))))

    (local.set $all (f32.sub (f32.add (local.get $r1sq) (local.get $r2sq))
                             (f32.mul (f32.const 2) (local.get $rdr))))
    (local.set $den (f32.add (f32.mul (local.get $rcsq) (local.get $all)) (local.get $xdx)))

    (local.set $ai1 (f32.div (f32.sub (f32.div (f32.add (local.get $rdr) (local.get $rcsq)) (local.get $r1eps))
                                     (local.get $r2eps))
                             (local.get $den)))
    (local.set $ai2 (f32.div (f32.sub (f32.div (f32.add (local.get $rdr) (local.get $rcsq)) (local.get $r2eps))
                                     (local.get $r1eps))
                             (local.get $den)))

    (local.set $k (i32.const 0))
    (block $kbreak
      (loop $kloop
        (br_if $kbreak (i32.ge_u (local.get $k) (i32.const 3)))

        (if (i32.eq (local.get $k) (i32.const 0))
          (then
            (local.set $r1k (local.get $r1_0))
            (local.set $r2k (local.get $r2_0))
          )
          (else
            (if (i32.eq (local.get $k) (i32.const 1))
              (then
                (local.set $r1k (local.get $r1_1))
                (local.set $r2k (local.get $r2_1))
              )
              (else
                (local.set $r1k (local.get $r1_2))
                (local.set $r2k (local.get $r2_2))
              )
            )
          )
        )

        (f32.store (i32.add (local.get $uvws) (i32.mul (local.get $k) (i32.const 4)))
          (f32.add (f32.mul (local.get $r1k) (local.get $ai1))
                   (f32.mul (local.get $r2k) (local.get $ai2))))

        (local.set $rr1
          (f32.sub
            (f32.sub
              (f32.div (f32.add (local.get $r1k) (local.get $r2k)) (local.get $r1eps))
              (f32.div (f32.mul (local.get $r1k) (f32.add (local.get $rdr) (local.get $rcsq)))
                       (f32.mul (f32.mul (local.get $r1eps) (local.get $r1eps)) (local.get $r1eps)))
            )
            (f32.div (local.get $r2k) (local.get $r2eps))
          )
        )

        (local.set $rr2
          (f32.sub
            (f32.sub
              (f32.div (f32.add (local.get $r1k) (local.get $r2k)) (local.get $r2eps))
              (f32.div (f32.mul (local.get $r2k) (f32.add (local.get $rdr) (local.get $rcsq)))
                       (f32.mul (f32.mul (local.get $r2eps) (local.get $r2eps)) (local.get $r2eps)))
            )
            (f32.div (local.get $r1k) (local.get $r1eps))
          )
        )

        (local.set $rrt
          (f32.add
            (f32.mul (f32.const 2) (f32.mul (local.get $r1k) (f32.sub (local.get $r2sq) (local.get $rdr))))
            (f32.mul (f32.const 2) (f32.mul (local.get $r2k) (f32.sub (local.get $r1sq) (local.get $rdr))))
          )
        )

        (local.set $aj1 (f32.div (f32.sub (local.get $rr1) (f32.mul (local.get $ai1) (local.get $rrt))) (local.get $den)))
        (local.set $aj2 (f32.div (f32.sub (local.get $rr2) (f32.mul (local.get $ai2) (local.get $rrt))) (local.get $den)))

        (local.set $j (i32.const 0))
        (block $jbreak
          (loop $jloop
            (br_if $jbreak (i32.ge_u (local.get $j) (i32.const 3)))
            (if (i32.eq (local.get $j) (i32.const 0))
              (then
                (local.set $r1j (local.get $r1_0))
                (local.set $r2j (local.get $r2_0))
              )
              (else
                (if (i32.eq (local.get $j) (i32.const 1))
                  (then
                    (local.set $r1j (local.get $r1_1))
                    (local.set $r2j (local.get $r2_1))
                  )
                  (else
                    (local.set $r1j (local.get $r1_2))
                    (local.set $r2j (local.get $r2_2))
                  )
                )
              )
            )
            (f32.store
              (i32.add (local.get $uvwd)
                (i32.mul (call $idx3 (local.get $k) (local.get $j)) (i32.const 4)))
              (f32.neg (f32.add (f32.mul (local.get $aj1) (local.get $r1j))
                                (f32.mul (local.get $aj2) (local.get $r2j))))
            )
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (br $jloop)
          )
        )

        (f32.store
          (i32.add (local.get $uvwd)
            (i32.mul (call $idx3 (local.get $k) (local.get $k)) (i32.const 4)))
          (f32.sub
            (f32.sub
              (f32.load (i32.add (local.get $uvwd)
                (i32.mul (call $idx3 (local.get $k) (local.get $k)) (i32.const 4))))
              (local.get $ai1))
            (local.get $ai2))
        )

        (local.set $k (i32.add (local.get $k) (i32.const 1)))
        (br $kloop)
      )
    )

    (f32.store (local.get $uvws) (f32.div (f32.mul (f32.load (local.get $uvws)) (local.get $pi4inv)) (local.get $beta)))
    (f32.store (i32.add (local.get $uvws) (i32.const 4)) (f32.mul (f32.load (i32.add (local.get $uvws) (i32.const 4))) (local.get $pi4inv)))
    (f32.store (i32.add (local.get $uvws) (i32.const 8)) (f32.mul (f32.load (i32.add (local.get $uvws) (i32.const 8))) (local.get $pi4inv)))

    (local.set $j (i32.const 0))
    (block $lbreak
      (loop $lloop
        (br_if $lbreak (i32.ge_u (local.get $j) (i32.const 3)))
        (f32.store
          (i32.add (local.get $uvwd) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4)))
          (f32.div
            (f32.mul (f32.load (i32.add (local.get $uvwd) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
                     (local.get $pi4inv))
            (local.get $beta))
        )
        (f32.store
          (i32.add (local.get $uvwd) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4)))
          (f32.mul (f32.load (i32.add (local.get $uvwd) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4))))
                   (local.get $pi4inv))
        )
        (f32.store
          (i32.add (local.get $uvwd) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4)))
          (f32.mul (f32.load (i32.add (local.get $uvwd) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4))))
                   (local.get $pi4inv))
        )
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $lloop)
      )
    )
  )

  (func (export "SRDSET")
    (param $betm f32) (param $xyzref i32) (param $iysym i32)
    (param $nbody i32) (param $lfrst i32) (param $nldim i32)
    (param $nl i32) (param $rl i32) (param $radl i32)
    (param $src_u i32) (param $dbl_u i32)
    (local $pi f32)
    (local $ib i32) (local $ilseg i32) (local $l1 i32) (local $l2 i32) (local $l i32)
    (local $blen f32) (local $sdfac f32)
    (local $drl0 f32) (local $drl1 f32) (local $drl2 f32)
    (local $drlmag f32) (local $drlmi f32)
    (local $esl0 f32) (local $esl1 f32) (local $esl2 f32)
    (local $adel f32) (local $aavg f32)
    (local $rlref0 f32) (local $rlref1 f32) (local $rlref2 f32)
    (local $iu i32)
    (local $urel0 f32) (local $urel1 f32) (local $urel2 f32)
    (local $wrot0 f32) (local $wrot1 f32) (local $wrot2 f32)
    (local $us f32)
    (local $un0 f32) (local $un1 f32) (local $un2 f32)

    (local.set $pi (f32.const 3.14159265))

    (local.set $ib (i32.const 0))
    (block $ibreak
      (loop $iloop
        (br_if $ibreak (i32.ge_u (local.get $ib) (local.get $nbody)))

        (local.set $l1 (i32.sub (i32.load (i32.add (local.get $lfrst) (i32.mul (local.get $ib) (i32.const 4)))) (i32.const 1)))
        (local.set $l2 (i32.add (local.get $l1)
          (i32.sub (i32.load (i32.add (local.get $nl) (i32.mul (local.get $ib) (i32.const 4)))) (i32.const 1))))

        (local.set $blen
          (f32.abs
            (f32.sub
              (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l2)) (i32.const 4))))
              (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l1)) (i32.const 4))))
            )
          )
        )

        (local.set $sdfac (f32.const 1))
        (if (i32.eq (local.get $iysym) (i32.const 1))
          (then
            (if (f32.le
              (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l1)) (i32.const 4))))
              (f32.mul (f32.const 0.001) (local.get $blen))
            )
              (then
                (local.set $sdfac (f32.const 0.5))
              )
            )
          )
        )

        (local.set $ilseg (i32.const 0))
        (block $lbreak
          (loop $lsegloop
            (br_if $lbreak (i32.ge_u (local.get $ilseg)
              (i32.sub (i32.load (i32.add (local.get $nl) (i32.mul (local.get $ib) (i32.const 4)))) (i32.const 1))))

            (local.set $l1 (i32.add
              (i32.sub (i32.load (i32.add (local.get $lfrst) (i32.mul (local.get $ib) (i32.const 4)))) (i32.const 1))
              (local.get $ilseg)
            ))
            (local.set $l2 (i32.add (local.get $l1) (i32.const 1)))
            (local.set $l (local.get $l1))

            (local.set $drl0
              (f32.div
                (f32.sub
                  (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l2)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l1)) (i32.const 4))))
                )
                (local.get $betm)
              )
            )
            (local.set $drl1
              (f32.sub
                (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l2)) (i32.const 4))))
                (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l1)) (i32.const 4))))
              )
            )
            (local.set $drl2
              (f32.sub
                (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l2)) (i32.const 4))))
                (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l1)) (i32.const 4))))
              )
            )

            (local.set $drlmag (f32.sqrt (f32.add (f32.add (f32.mul (local.get $drl0) (local.get $drl0))
                                                        (f32.mul (local.get $drl1) (local.get $drl1)))
                                                 (f32.mul (local.get $drl2) (local.get $drl2)))))
            (local.set $drlmi (select (f32.div (f32.const 1) (local.get $drlmag)) (f32.const 0) (f32.ne (local.get $drlmag) (f32.const 0))))

            (local.set $esl0 (f32.mul (local.get $drl0) (local.get $drlmi)))
            (local.set $esl1 (f32.mul (local.get $drl1) (local.get $drlmi)))
            (local.set $esl2 (f32.mul (local.get $drl2) (local.get $drlmi)))

            (local.set $adel
              (f32.mul
                (f32.mul (local.get $pi)
                  (f32.sub
                    (f32.mul (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l2) (i32.const 4))))
                             (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l2) (i32.const 4))))
                    )
                    (f32.mul (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l1) (i32.const 4))))
                             (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l1) (i32.const 4))))
                    )
                  )
                )
                (local.get $sdfac)
              )
            )

            (local.set $aavg
              (f32.mul
                (f32.mul (local.get $pi) (f32.mul (f32.const 0.5)
                  (f32.add
                    (f32.mul (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l2) (i32.const 4))))
                             (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l2) (i32.const 4))))
                    )
                    (f32.mul (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l1) (i32.const 4))))
                             (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l1) (i32.const 4))))
                    )
                  )
                ))
                (local.get $sdfac)
              )
            )

            (local.set $rlref0
              (f32.sub
                (f32.mul (f32.const 0.5)
                  (f32.add
                    (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l2)) (i32.const 4))))
                    (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l1)) (i32.const 4))))
                  )
                )
                (f32.load (local.get $xyzref))
              )
            )
            (local.set $rlref1
              (f32.sub
                (f32.mul (f32.const 0.5)
                  (f32.add
                    (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l2)) (i32.const 4))))
                    (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l1)) (i32.const 4))))
                  )
                )
                (f32.load (i32.add (local.get $xyzref) (i32.const 4)))
              )
            )
            (local.set $rlref2
              (f32.sub
                (f32.mul (f32.const 0.5)
                  (f32.add
                    (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l2)) (i32.const 4))))
                    (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l1)) (i32.const 4))))
                  )
                )
                (f32.load (i32.add (local.get $xyzref) (i32.const 8)))
              )
            )

            (local.set $iu (i32.const 0))
            (block $iubreak
              (loop $iuloop
                (br_if $iubreak (i32.ge_u (local.get $iu) (i32.const 6)))

                (local.set $urel0 (f32.const 0))
                (local.set $urel1 (f32.const 0))
                (local.set $urel2 (f32.const 0))
                (local.set $wrot0 (f32.const 0))
                (local.set $wrot1 (f32.const 0))
                (local.set $wrot2 (f32.const 0))

                (if (i32.lt_u (local.get $iu) (i32.const 3))
                  (then
                    (if (i32.eq (local.get $iu) (i32.const 0)) (then (local.set $urel0 (f32.const 1))))
                    (if (i32.eq (local.get $iu) (i32.const 1)) (then (local.set $urel1 (f32.const 1))))
                    (if (i32.eq (local.get $iu) (i32.const 2)) (then (local.set $urel2 (f32.const 1))))
                  )
                  (else
                    (if (i32.eq (local.get $iu) (i32.const 3)) (then (local.set $wrot0 (f32.const 1))))
                    (if (i32.eq (local.get $iu) (i32.const 4)) (then (local.set $wrot1 (f32.const 1))))
                    (if (i32.eq (local.get $iu) (i32.const 5)) (then (local.set $wrot2 (f32.const 1))))
                    (local.set $urel0 (f32.sub (f32.mul (local.get $rlref1) (local.get $wrot2))
                                               (f32.mul (local.get $rlref2) (local.get $wrot1))))
                    (local.set $urel1 (f32.sub (f32.mul (local.get $rlref2) (local.get $wrot0))
                                               (f32.mul (local.get $rlref0) (local.get $wrot2))))
                    (local.set $urel2 (f32.sub (f32.mul (local.get $rlref0) (local.get $wrot1))
                                               (f32.mul (local.get $rlref1) (local.get $wrot0))))
                  )
                )

                (local.set $urel0 (f32.div (local.get $urel0) (local.get $betm)))

                (local.set $us
                  (f32.add
                    (f32.add (f32.mul (local.get $urel0) (local.get $esl0))
                             (f32.mul (local.get $urel1) (local.get $esl1)))
                    (f32.mul (local.get $urel2) (local.get $esl2))
                  )
                )

                (local.set $un0 (f32.sub (local.get $urel0) (f32.mul (local.get $us) (local.get $esl0))))
                (local.set $un1 (f32.sub (local.get $urel1) (f32.mul (local.get $us) (local.get $esl1))))
                (local.set $un2 (f32.sub (local.get $urel2) (f32.mul (local.get $us) (local.get $esl2))))

                (f32.store (i32.add (local.get $src_u)
                  (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))
                  (f32.mul (local.get $adel) (local.get $us))
                )

                (f32.store (i32.add (local.get $dbl_u)
                  (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))
                  (f32.mul (f32.mul (local.get $aavg) (local.get $un0)) (f32.mul (local.get $drlmag) (f32.const 2))))
                (f32.store (i32.add (local.get $dbl_u)
                  (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))
                  (f32.mul (f32.mul (local.get $aavg) (local.get $un1)) (f32.mul (local.get $drlmag) (f32.const 2))))
                (f32.store (i32.add (local.get $dbl_u)
                  (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))
                  (f32.mul (f32.mul (local.get $aavg) (local.get $un2)) (f32.mul (local.get $drlmag) (f32.const 2))))

                (local.set $iu (i32.add (local.get $iu) (i32.const 1)))
                (br $iuloop)
              )
            )

            (local.set $ilseg (i32.add (local.get $ilseg) (i32.const 1)))
            (br $lsegloop)
          )
        )

        (local.set $ib (i32.add (local.get $ib) (i32.const 1)))
        (br $iloop)
      )
    )
  )

  (func (export "VSRD")
    (param $betm f32) (param $iysym i32) (param $ysym f32) (param $izsym i32) (param $zsym f32) (param $srcore f32)
    (param $nbody i32) (param $lfrst i32) (param $nldim i32) (param $nl i32) (param $rl i32) (param $radl i32)
    (param $nu i32) (param $src_u i32) (param $dbl_u i32)
    (param $nc i32) (param $rc i32) (param $wc_u i32) (param $ncdim i32)
    (local $fysym f32) (local $fzsym f32) (local $yoff f32) (local $zoff f32)
    (local $i i32) (local $iu i32) (local $k i32)
    (local $ib i32) (local $ilseg i32) (local $l1 i32) (local $l2 i32) (local $l i32)
    (local $ravg f32) (local $rlavg f32) (local $rcore f32)
    (local $dx f32) (local $dy f32) (local $dz f32)
    (local $vs0 f32) (local $vs1 f32) (local $vs2 f32)
    (local $vd00 f32) (local $vd01 f32) (local $vd02 f32)
    (local $vd10 f32) (local $vd11 f32) (local $vd12 f32)
    (local $vd20 f32) (local $vd21 f32) (local $vd22 f32)
    (local $contrib f32)

    (local.set $fysym (f32.convert_i32_s (local.get $iysym)))
    (local.set $fzsym (f32.convert_i32_s (local.get $izsym)))
    (local.set $yoff (f32.mul (f32.const 2) (local.get $ysym)))
    (local.set $zoff (f32.mul (f32.const 2) (local.get $zsym)))

    (local.set $i (i32.const 0))
    (block $cibreak
      (loop $ciloop
        (br_if $cibreak (i32.ge_u (local.get $i) (local.get $nc)))
        (local.set $iu (i32.const 0))
        (block $ciubreak
          (loop $ciuloop
            (br_if $ciubreak (i32.ge_u (local.get $iu) (local.get $nu)))
            (f32.store (i32.add (local.get $wc_u)
              (i32.mul (call $idx3c (i32.const 0) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4)))
              (f32.const 0))
            (f32.store (i32.add (local.get $wc_u)
              (i32.mul (call $idx3c (i32.const 1) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4)))
              (f32.const 0))
            (f32.store (i32.add (local.get $wc_u)
              (i32.mul (call $idx3c (i32.const 2) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4)))
              (f32.const 0))
            (local.set $iu (i32.add (local.get $iu) (i32.const 1)))
            (br $ciuloop)
          )
        )

                (if (i32.ne (local.get $iysym) (i32.const 0))
                  (then
                    (call $SRDVELC
                      (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 0) (local.get $i)) (i32.const 4))))
                      (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 1) (local.get $i)) (i32.const 4))))
                      (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 2) (local.get $i)) (i32.const 4))))
                      (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l1)) (i32.const 4))))
                      (f32.sub (local.get $yoff)
                        (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l1)) (i32.const 4)))))
                      (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l1)) (i32.const 4))))
                      (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l2)) (i32.const 4))))
                      (f32.sub (local.get $yoff)
                        (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l2)) (i32.const 4)))))
                      (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l2)) (i32.const 4))))
                      (local.get $betm) (local.get $rcore)
                      (i32.const 1024) (i32.const 1040)
                    )

                    (local.set $vs0 (f32.load (i32.const 1024)))
                    (local.set $vs1 (f32.load (i32.add (i32.const 1024) (i32.const 4))))
                    (local.set $vs2 (f32.load (i32.add (i32.const 1024) (i32.const 8))))
                    (local.set $vd00 (f32.load (i32.const 1040)))
                    (local.set $vd01 (f32.load (i32.add (i32.const 1040) (i32.const 4))))
                    (local.set $vd02 (f32.load (i32.add (i32.const 1040) (i32.const 8))))
                    (local.set $vd10 (f32.load (i32.add (i32.const 1040) (i32.const 12))))
                    (local.set $vd11 (f32.load (i32.add (i32.const 1040) (i32.const 16))))
                    (local.set $vd12 (f32.load (i32.add (i32.const 1040) (i32.const 20))))
                    (local.set $vd20 (f32.load (i32.add (i32.const 1040) (i32.const 24))))
                    (local.set $vd21 (f32.load (i32.add (i32.const 1040) (i32.const 28))))
                    (local.set $vd22 (f32.load (i32.add (i32.const 1040) (i32.const 32))))

                    (local.set $iu (i32.const 0))
                    (block $iubreak_y
                      (loop $iuloop_y
                        (br_if $iubreak_y (i32.ge_u (local.get $iu) (local.get $nu)))
                        (local.set $k (i32.const 0))
                        (block $kbreak_y
                          (loop $kloop_y
                            (br_if $kbreak_y (i32.ge_u (local.get $k) (i32.const 3)))
                            (if (i32.eq (local.get $k) (i32.const 0))
                              (then
                                (local.set $contrib
                                  (f32.add
                                    (f32.mul (local.get $vs0) (f32.load (i32.add (local.get $src_u)
                                      (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                    (f32.add
                                      (f32.mul (local.get $vd00) (f32.load (i32.add (local.get $dbl_u)
                                        (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                      (f32.add
                                        (f32.neg (f32.mul (local.get $vd01) (f32.load (i32.add (local.get $dbl_u)
                                          (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                        (f32.mul (local.get $vd02) (f32.load (i32.add (local.get $dbl_u)
                                          (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                      )
                                    )
                                  )
                                )
                              )
                              (else
                                (if (i32.eq (local.get $k) (i32.const 1))
                                  (then
                                    (local.set $contrib
                                      (f32.add
                                        (f32.mul (local.get $vs1) (f32.load (i32.add (local.get $src_u)
                                          (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                        (f32.add
                                          (f32.mul (local.get $vd10) (f32.load (i32.add (local.get $dbl_u)
                                            (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                          (f32.add
                                            (f32.neg (f32.mul (local.get $vd11) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                            (f32.mul (local.get $vd12) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                          )
                                        )
                                      )
                                    )
                                  )
                                  (else
                                    (local.set $contrib
                                      (f32.add
                                        (f32.mul (local.get $vs2) (f32.load (i32.add (local.get $src_u)
                                          (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                        (f32.add
                                          (f32.mul (local.get $vd20) (f32.load (i32.add (local.get $dbl_u)
                                            (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                          (f32.add
                                            (f32.neg (f32.mul (local.get $vd21) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                            (f32.mul (local.get $vd22) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                            (f32.store
                              (i32.add (local.get $wc_u)
                                (i32.mul (call $idx3c (local.get $k) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4)))
                              (f32.add
                                (f32.load (i32.add (local.get $wc_u)
                                  (i32.mul (call $idx3c (local.get $k) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4))))
                                (f32.mul (local.get $contrib) (local.get $fysym))
                              )
                            )
                            (local.set $k (i32.add (local.get $k) (i32.const 1)))
                            (br $kloop_y)
                          )
                        )
                        (local.set $iu (i32.add (local.get $iu) (i32.const 1)))
                        (br $iuloop_y)
                      )
                    )
                  )
                )

                (if (i32.ne (local.get $izsym) (i32.const 0))
                  (then
                    (call $SRDVELC
                      (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 0) (local.get $i)) (i32.const 4))))
                      (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 1) (local.get $i)) (i32.const 4))))
                      (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 2) (local.get $i)) (i32.const 4))))
                      (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l1)) (i32.const 4))))
                      (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l1)) (i32.const 4))))
                      (f32.sub (local.get $zoff)
                        (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l1)) (i32.const 4)))))
                      (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l2)) (i32.const 4))))
                      (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l2)) (i32.const 4))))
                      (f32.sub (local.get $zoff)
                        (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l2)) (i32.const 4)))))
                      (local.get $betm) (local.get $rcore)
                      (i32.const 1024) (i32.const 1040)
                    )

                    (local.set $vs0 (f32.load (i32.const 1024)))
                    (local.set $vs1 (f32.load (i32.add (i32.const 1024) (i32.const 4))))
                    (local.set $vs2 (f32.load (i32.add (i32.const 1024) (i32.const 8))))
                    (local.set $vd00 (f32.load (i32.const 1040)))
                    (local.set $vd01 (f32.load (i32.add (i32.const 1040) (i32.const 4))))
                    (local.set $vd02 (f32.load (i32.add (i32.const 1040) (i32.const 8))))
                    (local.set $vd10 (f32.load (i32.add (i32.const 1040) (i32.const 12))))
                    (local.set $vd11 (f32.load (i32.add (i32.const 1040) (i32.const 16))))
                    (local.set $vd12 (f32.load (i32.add (i32.const 1040) (i32.const 20))))
                    (local.set $vd20 (f32.load (i32.add (i32.const 1040) (i32.const 24))))
                    (local.set $vd21 (f32.load (i32.add (i32.const 1040) (i32.const 28))))
                    (local.set $vd22 (f32.load (i32.add (i32.const 1040) (i32.const 32))))

                    (local.set $iu (i32.const 0))
                    (block $iubreak_z
                      (loop $iuloop_z
                        (br_if $iubreak_z (i32.ge_u (local.get $iu) (local.get $nu)))
                        (local.set $k (i32.const 0))
                        (block $kbreak_z
                          (loop $kloop_z
                            (br_if $kbreak_z (i32.ge_u (local.get $k) (i32.const 3)))
                            (if (i32.eq (local.get $k) (i32.const 0))
                              (then
                                (local.set $contrib
                                  (f32.add
                                    (f32.mul (local.get $vs0) (f32.load (i32.add (local.get $src_u)
                                      (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                    (f32.add
                                      (f32.mul (local.get $vd00) (f32.load (i32.add (local.get $dbl_u)
                                        (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                      (f32.add
                                        (f32.mul (local.get $vd01) (f32.load (i32.add (local.get $dbl_u)
                                          (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                        (f32.neg (f32.mul (local.get $vd02) (f32.load (i32.add (local.get $dbl_u)
                                          (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                      )
                                    )
                                  )
                                )
                              )
                              (else
                                (if (i32.eq (local.get $k) (i32.const 1))
                                  (then
                                    (local.set $contrib
                                      (f32.add
                                        (f32.mul (local.get $vs1) (f32.load (i32.add (local.get $src_u)
                                          (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                        (f32.add
                                          (f32.mul (local.get $vd10) (f32.load (i32.add (local.get $dbl_u)
                                            (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                          (f32.add
                                            (f32.mul (local.get $vd11) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                            (f32.neg (f32.mul (local.get $vd12) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                          )
                                        )
                                      )
                                    )
                                  )
                                  (else
                                    (local.set $contrib
                                      (f32.add
                                        (f32.mul (local.get $vs2) (f32.load (i32.add (local.get $src_u)
                                          (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                        (f32.add
                                          (f32.mul (local.get $vd20) (f32.load (i32.add (local.get $dbl_u)
                                            (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                          (f32.add
                                            (f32.mul (local.get $vd21) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                            (f32.neg (f32.mul (local.get $vd22) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                            (f32.store
                              (i32.add (local.get $wc_u)
                                (i32.mul (call $idx3c (local.get $k) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4)))
                              (f32.add
                                (f32.load (i32.add (local.get $wc_u)
                                  (i32.mul (call $idx3c (local.get $k) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4))))
                                (f32.mul (local.get $contrib) (local.get $fzsym))
                              )
                            )
                            (local.set $k (i32.add (local.get $k) (i32.const 1)))
                            (br $kloop_z)
                          )
                        )
                        (local.set $iu (i32.add (local.get $iu) (i32.const 1)))
                        (br $iuloop_z)
                      )
                    )

                    (if (i32.ne (local.get $iysym) (i32.const 0))
                      (then
                        (call $SRDVELC
                          (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 0) (local.get $i)) (i32.const 4))))
                          (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 1) (local.get $i)) (i32.const 4))))
                          (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 2) (local.get $i)) (i32.const 4))))
                          (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l1)) (i32.const 4))))
                          (f32.sub (local.get $yoff)
                            (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l1)) (i32.const 4)))))
                          (f32.sub (local.get $zoff)
                            (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l1)) (i32.const 4)))))
                          (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l2)) (i32.const 4))))
                          (f32.sub (local.get $yoff)
                            (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l2)) (i32.const 4)))))
                          (f32.sub (local.get $zoff)
                            (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l2)) (i32.const 4)))))
                          (local.get $betm) (local.get $rcore)
                          (i32.const 1024) (i32.const 1040)
                        )

                        (local.set $vs0 (f32.load (i32.const 1024)))
                        (local.set $vs1 (f32.load (i32.add (i32.const 1024) (i32.const 4))))
                        (local.set $vs2 (f32.load (i32.add (i32.const 1024) (i32.const 8))))
                        (local.set $vd00 (f32.load (i32.const 1040)))
                        (local.set $vd01 (f32.load (i32.add (i32.const 1040) (i32.const 4))))
                        (local.set $vd02 (f32.load (i32.add (i32.const 1040) (i32.const 8))))
                        (local.set $vd10 (f32.load (i32.add (i32.const 1040) (i32.const 12))))
                        (local.set $vd11 (f32.load (i32.add (i32.const 1040) (i32.const 16))))
                        (local.set $vd12 (f32.load (i32.add (i32.const 1040) (i32.const 20))))
                        (local.set $vd20 (f32.load (i32.add (i32.const 1040) (i32.const 24))))
                        (local.set $vd21 (f32.load (i32.add (i32.const 1040) (i32.const 28))))
                        (local.set $vd22 (f32.load (i32.add (i32.const 1040) (i32.const 32))))

                        (local.set $iu (i32.const 0))
                        (block $iubreak_yz
                          (loop $iuloop_yz
                            (br_if $iubreak_yz (i32.ge_u (local.get $iu) (local.get $nu)))
                            (local.set $k (i32.const 0))
                            (block $kbreak_yz
                              (loop $kloop_yz
                                (br_if $kbreak_yz (i32.ge_u (local.get $k) (i32.const 3)))
                                (if (i32.eq (local.get $k) (i32.const 0))
                                  (then
                                    (local.set $contrib
                                      (f32.add
                                        (f32.mul (local.get $vs0) (f32.load (i32.add (local.get $src_u)
                                          (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                        (f32.add
                                          (f32.mul (local.get $vd00) (f32.load (i32.add (local.get $dbl_u)
                                            (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                          (f32.add
                                            (f32.neg (f32.mul (local.get $vd01) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                            (f32.neg (f32.mul (local.get $vd02) (f32.load (i32.add (local.get $dbl_u)
                                              (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                          )
                                        )
                                      )
                                    )
                                  )
                                  (else
                                    (if (i32.eq (local.get $k) (i32.const 1))
                                      (then
                                        (local.set $contrib
                                          (f32.add
                                            (f32.mul (local.get $vs1) (f32.load (i32.add (local.get $src_u)
                                              (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                            (f32.add
                                              (f32.mul (local.get $vd10) (f32.load (i32.add (local.get $dbl_u)
                                                (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                              (f32.add
                                                (f32.neg (f32.mul (local.get $vd11) (f32.load (i32.add (local.get $dbl_u)
                                                  (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                                (f32.neg (f32.mul (local.get $vd12) (f32.load (i32.add (local.get $dbl_u)
                                                  (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                              )
                                            )
                                          )
                                        )
                                      )
                                      (else
                                        (local.set $contrib
                                          (f32.add
                                            (f32.mul (local.get $vs2) (f32.load (i32.add (local.get $src_u)
                                              (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                            (f32.add
                                              (f32.mul (local.get $vd20) (f32.load (i32.add (local.get $dbl_u)
                                                (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                              (f32.add
                                                (f32.neg (f32.mul (local.get $vd21) (f32.load (i32.add (local.get $dbl_u)
                                                  (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                                (f32.neg (f32.mul (local.get $vd22) (f32.load (i32.add (local.get $dbl_u)
                                                  (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4))))))
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                (f32.store
                                  (i32.add (local.get $wc_u)
                                    (i32.mul (call $idx3c (local.get $k) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4)))
                                  (f32.add
                                    (f32.load (i32.add (local.get $wc_u)
                                      (i32.mul (call $idx3c (local.get $k) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4))))
                                    (f32.mul (local.get $contrib) (f32.mul (local.get $fysym) (local.get $fzsym)))
                                  )
                                )
                                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                                (br $kloop_yz)
                              )
                            )
                            (local.set $iu (i32.add (local.get $iu) (i32.const 1)))
                            (br $iuloop_yz)
                          )
                        )
                      )
                    )
                  )
                )
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $ciloop)
      )
    )

    (local.set $ib (i32.const 0))
    (block $ibreak
      (loop $iloop
        (br_if $ibreak (i32.ge_u (local.get $ib) (local.get $nbody)))
        (local.set $ilseg (i32.const 0))
        (block $lbreak
          (loop $lsegloop
            (br_if $lbreak (i32.ge_u (local.get $ilseg)
              (i32.sub (i32.load (i32.add (local.get $nl) (i32.mul (local.get $ib) (i32.const 4)))) (i32.const 1))))

            (local.set $l1 (i32.add
              (i32.sub (i32.load (i32.add (local.get $lfrst) (i32.mul (local.get $ib) (i32.const 4)))) (i32.const 1))
              (local.get $ilseg)
            ))
            (local.set $l2 (i32.add (local.get $l1) (i32.const 1)))
            (local.set $l (local.get $l1))

            (local.set $ravg (f32.sqrt
              (f32.mul (f32.const 0.5)
                (f32.add
                  (f32.mul (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l2) (i32.const 4))))
                           (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l2) (i32.const 4))))
                  )
                  (f32.mul (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l1) (i32.const 4))))
                           (f32.load (i32.add (local.get $radl) (i32.mul (local.get $l1) (i32.const 4))))
                  )
                )
              )
            ))

            (local.set $dx (f32.sub
              (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l2)) (i32.const 4))))
              (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l1)) (i32.const 4))))
            ))
            (local.set $dy (f32.sub
              (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l2)) (i32.const 4))))
              (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l1)) (i32.const 4))))
            ))
            (local.set $dz (f32.sub
              (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l2)) (i32.const 4))))
              (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l1)) (i32.const 4))))
            ))
            (local.set $rlavg (f32.sqrt (f32.add (f32.add (f32.mul (local.get $dx) (local.get $dx))
                                                         (f32.mul (local.get $dy) (local.get $dy)))
                                                (f32.mul (local.get $dz) (local.get $dz)))))

            (local.set $rcore (select (f32.mul (local.get $srcore) (local.get $ravg))
                                      (f32.mul (local.get $srcore) (local.get $rlavg))
                                      (f32.gt (local.get $srcore) (f32.const 0))))

            (local.set $i (i32.const 0))
            (block $icbreak
              (loop $icloops
                (br_if $icbreak (i32.ge_u (local.get $i) (local.get $nc)))

                (call $SRDVELC
                  (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 0) (local.get $i)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 1) (local.get $i)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 2) (local.get $i)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l1)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l1)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l1)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 0) (local.get $l2)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 1) (local.get $l2)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rl) (i32.mul (call $idx3 (i32.const 2) (local.get $l2)) (i32.const 4))))
                  (local.get $betm) (local.get $rcore)
                  (i32.const 1024) (i32.const 1040)
                )

                (local.set $vs0 (f32.load (i32.const 1024)))
                (local.set $vs1 (f32.load (i32.add (i32.const 1024) (i32.const 4))))
                (local.set $vs2 (f32.load (i32.add (i32.const 1024) (i32.const 8))))
                (local.set $vd00 (f32.load (i32.const 1040)))
                (local.set $vd01 (f32.load (i32.add (i32.const 1040) (i32.const 4))))
                (local.set $vd02 (f32.load (i32.add (i32.const 1040) (i32.const 8))))
                (local.set $vd10 (f32.load (i32.add (i32.const 1040) (i32.const 12))))
                (local.set $vd11 (f32.load (i32.add (i32.const 1040) (i32.const 16))))
                (local.set $vd12 (f32.load (i32.add (i32.const 1040) (i32.const 20))))
                (local.set $vd20 (f32.load (i32.add (i32.const 1040) (i32.const 24))))
                (local.set $vd21 (f32.load (i32.add (i32.const 1040) (i32.const 28))))
                (local.set $vd22 (f32.load (i32.add (i32.const 1040) (i32.const 32))))

                (local.set $iu (i32.const 0))
                (block $iubreak2
                  (loop $iuloop2
                    (br_if $iubreak2 (i32.ge_u (local.get $iu) (local.get $nu)))
                    (local.set $k (i32.const 0))
                    (block $kbreak2
                      (loop $kloop2
                        (br_if $kbreak2 (i32.ge_u (local.get $k) (i32.const 3)))
                        (if (i32.eq (local.get $k) (i32.const 0))
                          (then
                            (local.set $contrib
                              (f32.add
                                (f32.mul (local.get $vs0) (f32.load (i32.add (local.get $src_u)
                                  (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                (f32.add
                                  (f32.mul (local.get $vd00) (f32.load (i32.add (local.get $dbl_u)
                                    (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                  (f32.add
                                    (f32.mul (local.get $vd01) (f32.load (i32.add (local.get $dbl_u)
                                      (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                    (f32.mul (local.get $vd02) (f32.load (i32.add (local.get $dbl_u)
                                      (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                  )
                                )
                              )
                            )
                          )
                          (else
                            (if (i32.eq (local.get $k) (i32.const 1))
                              (then
                                (local.set $contrib
                                  (f32.add
                                    (f32.mul (local.get $vs1) (f32.load (i32.add (local.get $src_u)
                                      (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                    (f32.add
                                      (f32.mul (local.get $vd10) (f32.load (i32.add (local.get $dbl_u)
                                        (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                      (f32.add
                                        (f32.mul (local.get $vd11) (f32.load (i32.add (local.get $dbl_u)
                                          (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                        (f32.mul (local.get $vd12) (f32.load (i32.add (local.get $dbl_u)
                                          (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                      )
                                    )
                                  )
                                )
                              )
                              (else
                                (local.set $contrib
                                  (f32.add
                                    (f32.mul (local.get $vs2) (f32.load (i32.add (local.get $src_u)
                                      (i32.mul (call $idx2 (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                    (f32.add
                                      (f32.mul (local.get $vd20) (f32.load (i32.add (local.get $dbl_u)
                                        (i32.mul (call $idx3c (i32.const 0) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                      (f32.add
                                        (f32.mul (local.get $vd21) (f32.load (i32.add (local.get $dbl_u)
                                          (i32.mul (call $idx3c (i32.const 1) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                        (f32.mul (local.get $vd22) (f32.load (i32.add (local.get $dbl_u)
                                          (i32.mul (call $idx3c (i32.const 2) (local.get $l) (local.get $iu) (local.get $nldim)) (i32.const 4)))))
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                        (f32.store
                          (i32.add (local.get $wc_u)
                            (i32.mul (call $idx3c (local.get $k) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4)))
                          (f32.add
                            (f32.load (i32.add (local.get $wc_u)
                              (i32.mul (call $idx3c (local.get $k) (local.get $i) (local.get $iu) (local.get $ncdim)) (i32.const 4))))
                            (local.get $contrib)
                          )
                        )
                        (local.set $k (i32.add (local.get $k) (i32.const 1)))
                        (br $kloop2)
                      )
                    )
                    (local.set $iu (i32.add (local.get $iu) (i32.const 1)))
                    (br $iuloop2)
                  )
                )

                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $icloops)
              )
            )

            (local.set $ilseg (i32.add (local.get $ilseg) (i32.const 1)))
            (br $lsegloop)
          )
        )

        (local.set $ib (i32.add (local.get $ib) (i32.const 1)))
        (br $iloop)
      )
    )
  )

  (func (export "VVOR")
    (param $betm f32) (param $iysym i32) (param $ysym f32) (param $izsym i32) (param $zsym f32)
    (param $vrcorec f32) (param $vrcorew f32)
    (param $nv i32) (param $rv1 i32) (param $rv2 i32) (param $ncompv i32) (param $chordv i32)
    (param $nc i32) (param $rc i32) (param $ncompc i32) (param $lvtest i32)
    (param $wc_gam i32) (param $ncdim i32)
    (local $fysym f32) (local $fzsym f32)
    (local $i i32) (local $j i32)
    (local $x f32) (local $y f32) (local $z f32)
    (local $dsY f32) (local $dsZ f32) (local $dsyz f32) (local $rcore f32)
    (local $u f32) (local $v f32) (local $w f32)
    (local $ui f32) (local $vi f32) (local $wi f32)
    (local $yoff f32) (local $zoff f32)
    (local $lbound i32)
    (local $xave f32) (local $yave f32) (local $zave f32)

    (local.set $fysym (f32.convert_i32_s (local.get $iysym)))
    (local.set $fzsym (f32.convert_i32_s (local.get $izsym)))

    (local.set $i (i32.const 0))
    (block $ibreak
      (loop $iloop
        (br_if $ibreak (i32.ge_u (local.get $i) (local.get $nc)))

        (local.set $x (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 0) (local.get $i)) (i32.const 4)))))
        (local.set $y (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 1) (local.get $i)) (i32.const 4)))))
        (local.set $z (f32.load (i32.add (local.get $rc) (i32.mul (call $idx3 (i32.const 2) (local.get $i)) (i32.const 4)))))

        (local.set $j (i32.const 0))
        (block $jbreak
          (loop $jloop
            (br_if $jbreak (i32.ge_u (local.get $j) (local.get $nv)))

            (local.set $dsY
              (f32.sub
                (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4))))
                (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4))))
              )
            )
            (local.set $dsZ
              (f32.sub
                (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4))))
                (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4))))
              )
            )
            (local.set $dsyz (f32.sqrt (f32.add (f32.mul (local.get $dsY) (local.get $dsY))
                                                (f32.mul (local.get $dsZ) (local.get $dsZ)))))
            (local.set $rcore (f32.mul (f32.const 0.0001) (local.get $dsyz)))

            (if (i32.eq (local.get $nc) (local.get $nv))
              (then
                (if (i32.ne
                  (i32.load (i32.add (local.get $ncompc) (i32.mul (local.get $i) (i32.const 4))))
                  (i32.load (i32.add (local.get $ncompv) (i32.mul (local.get $j) (i32.const 4))))
                )
                  (then
                    (local.set $rcore
                      (f32.max
                        (f32.mul (local.get $vrcorec)
                          (f32.load (i32.add (local.get $chordv) (i32.mul (local.get $j) (i32.const 4)))))
                        (f32.mul (local.get $vrcorew) (local.get $dsyz))
                      )
                    )
                  )
                )
              )
            )

            (local.set $u (f32.const 0))
            (local.set $v (f32.const 0))
            (local.set $w (f32.const 0))
            (local.set $ui (f32.const 0))
            (local.set $vi (f32.const 0))
            (local.set $wi (f32.const 0))

            (local.set $yoff (f32.mul (f32.const 2) (local.get $ysym)))
            (local.set $zoff (f32.mul (f32.const 2) (local.get $zsym)))

            (local.set $lbound
              (i32.eqz
                (i32.and (local.get $lvtest) (i32.eq (local.get $i) (local.get $j)))
              )
            )

            (call $VORVELC
              (local.get $x) (local.get $y) (local.get $z) (local.get $lbound)
              (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
              (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4))))
              (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4))))
              (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
              (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4))))
              (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4))))
              (local.get $betm) (local.get $rcore)
              (i32.const 1080)
            )
            (local.set $u (f32.load (i32.const 1080)))
            (local.set $v (f32.load (i32.add (i32.const 1080) (i32.const 4))))
            (local.set $w (f32.load (i32.add (i32.const 1080) (i32.const 8))))

            (if (i32.ne (local.get $iysym) (i32.const 0))
              (then
                (local.set $lbound (i32.const 1))
                (if (i32.eq (local.get $iysym) (i32.const 1))
                  (then
                    (local.set $xave (f32.mul (f32.const 0.5)
                      (f32.add
                        (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
                        (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
                      )))
                    (local.set $yave (f32.sub (local.get $yoff)
                      (f32.mul (f32.const 0.5)
                        (f32.add
                          (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4))))
                          (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4))))
                        ))))
                    (local.set $zave (f32.mul (f32.const 0.5)
                      (f32.add
                        (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4))))
                        (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4))))
                      )))
                    (if (i32.and
                      (f32.eq (local.get $x) (local.get $xave))
                      (i32.and (f32.eq (local.get $y) (local.get $yave)) (f32.eq (local.get $z) (local.get $zave)))
                    )
                      (then (local.set $lbound (i32.const 0)))
                    )
                  )
                )

                (call $VORVELC
                  (local.get $x) (local.get $y) (local.get $z) (local.get $lbound)
                  (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
                  (f32.sub (local.get $yoff)
                    (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4)))))
                  (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
                  (f32.sub (local.get $yoff)
                    (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4)))))
                  (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4))))
                  (local.get $betm) (local.get $rcore)
                  (i32.const 1096)
                )
                (local.set $ui (f32.mul (f32.load (i32.const 1096)) (local.get $fysym)))
                (local.set $vi (f32.mul (f32.load (i32.add (i32.const 1096) (i32.const 4))) (local.get $fysym)))
                (local.set $wi (f32.mul (f32.load (i32.add (i32.const 1096) (i32.const 8))) (local.get $fysym)))
              )
            )

            (if (i32.ne (local.get $izsym) (i32.const 0))
              (then
                (call $VORVELC
                  (local.get $x) (local.get $y) (local.get $z) (i32.const 1)
                  (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4))))
                  (f32.sub (local.get $zoff)
                    (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4)))))
                  (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
                  (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4))))
                  (f32.sub (local.get $zoff)
                    (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4)))))
                  (local.get $betm) (local.get $rcore)
                  (i32.const 1112)
                )
                (local.set $u (f32.add (local.get $u) (f32.mul (f32.load (i32.const 1112)) (local.get $fzsym))))
                (local.set $v (f32.add (local.get $v) (f32.mul (f32.load (i32.add (i32.const 1112) (i32.const 4))) (local.get $fzsym))))
                (local.set $w (f32.add (local.get $w) (f32.mul (f32.load (i32.add (i32.const 1112) (i32.const 8))) (local.get $fzsym))))

                (if (i32.ne (local.get $iysym) (i32.const 0))
                  (then
                    (call $VORVELC
                      (local.get $x) (local.get $y) (local.get $z) (i32.const 1)
                      (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
                      (f32.sub (local.get $yoff)
                        (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4)))))
                      (f32.sub (local.get $zoff)
                        (f32.load (i32.add (local.get $rv1) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4)))))
                      (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 0) (local.get $j)) (i32.const 4))))
                      (f32.sub (local.get $yoff)
                        (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 1) (local.get $j)) (i32.const 4)))))
                      (f32.sub (local.get $zoff)
                        (f32.load (i32.add (local.get $rv2) (i32.mul (call $idx3 (i32.const 2) (local.get $j)) (i32.const 4)))))
                      (local.get $betm) (local.get $rcore)
                      (i32.const 1128)
                    )
                    (local.set $ui (f32.add (local.get $ui)
                      (f32.mul (f32.load (i32.const 1128)) (f32.mul (local.get $fysym) (local.get $fzsym)))))
                    (local.set $vi (f32.add (local.get $vi)
                      (f32.mul (f32.load (i32.add (i32.const 1128) (i32.const 4))) (f32.mul (local.get $fysym) (local.get $fzsym)))))
                    (local.set $wi (f32.add (local.get $wi)
                      (f32.mul (f32.load (i32.add (i32.const 1128) (i32.const 8))) (f32.mul (local.get $fysym) (local.get $fzsym)))))
                  )
                )
              )
            )

            (f32.store (i32.add (local.get $wc_gam)
              (i32.mul (call $idx3c (i32.const 0) (local.get $i) (local.get $j) (local.get $ncdim)) (i32.const 4)))
              (f32.add (local.get $u) (local.get $ui)))
            (f32.store (i32.add (local.get $wc_gam)
              (i32.mul (call $idx3c (i32.const 1) (local.get $i) (local.get $j) (local.get $ncdim)) (i32.const 4)))
              (f32.add (local.get $v) (local.get $vi)))
            (f32.store (i32.add (local.get $wc_gam)
              (i32.mul (call $idx3c (i32.const 2) (local.get $i) (local.get $j) (local.get $ncdim)) (i32.const 4)))
              (f32.add (local.get $w) (local.get $wi)))

            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (br $jloop)
          )
        )

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $iloop)
      )
    )
  )
)
