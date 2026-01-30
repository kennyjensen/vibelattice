(module
  (import "env" "now" (func $now (result f64)))
  (func (export "SECONDS") (result f64)
    (call $now)
  )
)
