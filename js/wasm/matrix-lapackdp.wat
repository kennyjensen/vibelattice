(module
  (memory (export "memory") 1)

  (func (export "LUDCMP") (param $nsiz i32) (param $n i32) (param $a i32) (param $indx i32) (param $work i32)
    (local $i i32) (local $j i32) (local $k i32)
    (local $imax i32)
    (local $aamax f64)
    (local $sum f64)
    (local $dum f64)
    (local $idx i32)
    (local $idx2 i32)
    (local $addr_ij i32)
    (local $addr_ik i32)
    (local $addr_kj i32)
    (local $one f64)

    (local.set $one (f64.const 1))

    (local.set $i (i32.const 0))
    (block $i_done0
      (loop $i_loop0
        (local.set $aamax (f64.const 0))
        (local.set $j (i32.const 0))
        (block $j_done0
          (loop $j_loop0
            (local.set $idx
              (i32.add (local.get $a)
                       (i32.mul (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $j)) (i32.const 8))))
            (local.set $sum (f64.abs (f64.load (local.get $idx))))
            (if (f64.gt (local.get $sum) (local.get $aamax))
              (then (local.set $aamax (local.get $sum)))
            )
            (local.set $j (i32.add (local.get $j) (i32.const 1)))
            (br_if $j_loop0 (i32.lt_s (local.get $j) (local.get $n)))
          )
        )

        (local.set $idx (i32.add (local.get $work) (i32.mul (local.get $i) (i32.const 8))))
        (f64.store (local.get $idx) (f64.div (local.get $one) (local.get $aamax)))

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br_if $i_loop0 (i32.lt_s (local.get $i) (local.get $n)))
      )
    )

    (local.set $j (i32.const 0))
    (block $j_done1
      (loop $j_loop1
        (local.set $i (i32.const 0))
        (block $i_done1
          (loop $i_loop1
            (br_if $i_done1 (i32.ge_s (local.get $i) (local.get $j)))
            (local.set $addr_ij
              (i32.add (local.get $a)
                       (i32.mul (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $j)) (i32.const 8))))
            (local.set $sum (f64.load (local.get $addr_ij)))
            (local.set $k (i32.const 0))
            (block $k_done1
              (loop $k_loop1
                (br_if $k_done1 (i32.ge_s (local.get $k) (local.get $i)))
                (local.set $addr_ik
                  (i32.add (local.get $a)
                           (i32.mul (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $k)) (i32.const 8))))
                (local.set $addr_kj
                  (i32.add (local.get $a)
                           (i32.mul (i32.add (i32.mul (local.get $k) (local.get $n)) (local.get $j)) (i32.const 8))))
                (local.set $sum
                  (f64.sub (local.get $sum)
                           (f64.mul (f64.load (local.get $addr_ik)) (f64.load (local.get $addr_kj)))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k_loop1)
              )
            )
            (f64.store (local.get $addr_ij) (local.get $sum))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i_loop1)
          )
        )

        (local.set $aamax (f64.const 0))
        (local.set $imax (local.get $j))
        (local.set $i (local.get $j))
        (block $i_done2
          (loop $i_loop2
            (br_if $i_done2 (i32.ge_s (local.get $i) (local.get $n)))
            (local.set $addr_ij
              (i32.add (local.get $a)
                       (i32.mul (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $j)) (i32.const 8))))
            (local.set $sum (f64.load (local.get $addr_ij)))
            (local.set $k (i32.const 0))
            (block $k_done2
              (loop $k_loop2
                (br_if $k_done2 (i32.ge_s (local.get $k) (local.get $j)))
                (local.set $addr_ik
                  (i32.add (local.get $a)
                           (i32.mul (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $k)) (i32.const 8))))
                (local.set $addr_kj
                  (i32.add (local.get $a)
                           (i32.mul (i32.add (i32.mul (local.get $k) (local.get $n)) (local.get $j)) (i32.const 8))))
                (local.set $sum
                  (f64.sub (local.get $sum)
                           (f64.mul (f64.load (local.get $addr_ik)) (f64.load (local.get $addr_kj)))))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k_loop2)
              )
            )
            (f64.store (local.get $addr_ij) (local.get $sum))

            (local.set $idx
              (i32.add (local.get $work) (i32.mul (local.get $i) (i32.const 8))))
            (local.set $dum (f64.mul (f64.load (local.get $idx)) (f64.abs (local.get $sum))))
            (if (f64.ge (local.get $dum) (local.get $aamax))
              (then
                (local.set $aamax (local.get $dum))
                (local.set $imax (local.get $i))
              )
            )

            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $i_loop2)
          )
        )

        (if (i32.ne (local.get $j) (local.get $imax))
          (then
            (local.set $k (i32.const 0))
            (block $k_done3
              (loop $k_loop3
                (br_if $k_done3 (i32.ge_s (local.get $k) (local.get $n)))
                (local.set $idx
                  (i32.add (local.get $a)
                           (i32.mul (i32.add (i32.mul (local.get $imax) (local.get $n)) (local.get $k)) (i32.const 8))))
                (local.set $idx2
                  (i32.add (local.get $a)
                           (i32.mul (i32.add (i32.mul (local.get $j) (local.get $n)) (local.get $k)) (i32.const 8))))
                (local.set $dum (f64.load (local.get $idx)))
                (f64.store (local.get $idx) (f64.load (local.get $idx2)))
                (f64.store (local.get $idx2) (local.get $dum))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
                (br $k_loop3)
              )
            )
            (local.set $idx
              (i32.add (local.get $work) (i32.mul (local.get $imax) (i32.const 8))))
            (local.set $idx2
              (i32.add (local.get $work) (i32.mul (local.get $j) (i32.const 8))))
            (local.set $dum (f64.load (local.get $idx)))
            (f64.store (local.get $idx) (f64.load (local.get $idx2)))
            (f64.store (local.get $idx2) (local.get $dum))
          )
        )

        (i32.store (i32.add (local.get $indx) (i32.mul (local.get $j) (i32.const 4))) (local.get $imax))

        (if (i32.ne (local.get $j) (i32.sub (local.get $n) (i32.const 1)))
          (then
            (local.set $idx
              (i32.add (local.get $a)
                       (i32.mul (i32.add (i32.mul (local.get $j) (local.get $n)) (local.get $j)) (i32.const 8))))
            (local.set $dum (f64.div (local.get $one) (f64.load (local.get $idx))))
            (local.set $i (i32.add (local.get $j) (i32.const 1)))
            (block $i_done3
              (loop $i_loop3
                (br_if $i_done3 (i32.ge_s (local.get $i) (local.get $n)))
                (local.set $idx
                  (i32.add (local.get $a)
                           (i32.mul (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $j)) (i32.const 8))))
                (f64.store (local.get $idx) (f64.mul (f64.load (local.get $idx)) (local.get $dum)))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $i_loop3)
              )
            )
          )
        )

        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br_if $j_loop1 (i32.lt_s (local.get $j) (local.get $n)))
      )
    )
  )

  (func (export "BAKSUB") (param $nsiz i32) (param $n i32) (param $a i32) (param $indx i32) (param $b i32)
    (local $i i32) (local $j i32) (local $ii i32) (local $ll i32)
    (local $sum f64) (local $idx i32) (local $idx2 i32) (local $addr_bi i32)

    (local.set $ii (i32.const -1))

    (local.set $i (i32.const 0))
    (block $i_done
      (loop $i_loop
        (br_if $i_done (i32.ge_s (local.get $i) (local.get $n)))
        (local.set $ll (i32.load (i32.add (local.get $indx) (i32.mul (local.get $i) (i32.const 4)))))
        (local.set $idx (i32.add (local.get $b) (i32.mul (local.get $ll) (i32.const 8))))
        (local.set $sum (f64.load (local.get $idx)))
        (local.set $addr_bi (i32.add (local.get $b) (i32.mul (local.get $i) (i32.const 8))))
        (f64.store (local.get $idx) (f64.load (local.get $addr_bi)))

        (if (i32.ne (local.get $ii) (i32.const -1))
          (then
            (local.set $j (local.get $ii))
            (block $j_done
              (loop $j_loop
                (br_if $j_done (i32.gt_s (local.get $j) (i32.sub (local.get $i) (i32.const 1))))
                (local.set $idx
                  (i32.add (local.get $a)
                           (i32.mul (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $j)) (i32.const 8))))
                (local.set $idx2
                  (i32.add (local.get $b) (i32.mul (local.get $j) (i32.const 8))))
                (local.set $sum
                  (f64.sub (local.get $sum)
                           (f64.mul (f64.load (local.get $idx)) (f64.load (local.get $idx2)))))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop)
              )
            )
          )
          (else
            (if (f64.ne (local.get $sum) (f64.const 0))
              (then (local.set $ii (local.get $i)))
            )
          )
        )

        (f64.store (local.get $addr_bi) (local.get $sum))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $i_loop)
      )
    )

    (local.set $i (i32.sub (local.get $n) (i32.const 1)))
    (block $i_done2
      (loop $i_loop2
        (br_if $i_done2 (i32.lt_s (local.get $i) (i32.const 0)))
        (local.set $addr_bi (i32.add (local.get $b) (i32.mul (local.get $i) (i32.const 8))))
        (local.set $sum (f64.load (local.get $addr_bi)))

        (if (i32.lt_s (local.get $i) (i32.sub (local.get $n) (i32.const 1)))
          (then
            (local.set $j (i32.add (local.get $i) (i32.const 1)))
            (block $j_done2
              (loop $j_loop2
                (br_if $j_done2 (i32.ge_s (local.get $j) (local.get $n)))
                (local.set $idx
                  (i32.add (local.get $a)
                           (i32.mul (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $j)) (i32.const 8))))
                (local.set $idx2
                  (i32.add (local.get $b) (i32.mul (local.get $j) (i32.const 8))))
                (local.set $sum
                  (f64.sub (local.get $sum)
                           (f64.mul (f64.load (local.get $idx)) (f64.load (local.get $idx2)))))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $j_loop2)
              )
            )
          )
        )

        (local.set $idx
          (i32.add (local.get $a)
                   (i32.mul (i32.add (i32.mul (local.get $i) (local.get $n)) (local.get $i)) (i32.const 8))))
        (f64.store (local.get $addr_bi) (f64.div (local.get $sum) (f64.load (local.get $idx))))
        (local.set $i (i32.sub (local.get $i) (i32.const 1)))
        (br $i_loop2)
      )
    )
  )
)
