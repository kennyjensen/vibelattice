# AVL JS/WASM Port (autil.f)

This folder contains a numerically faithful JS/WASM port of AVL's `autil.f` (`M3INV`, `RATEKI3`, `ROTENS3`), plus Fortran reference harnesses to compare outputs.

## Quick start

1) Build the Fortran references (requires `gfortran`):

```
make -C ../third_party/avl/ref
```

2) Install JS dev deps and build wasm:

```
npm install
npm run build:wasm
```

3) Run tests comparing JS/WASM to Fortran:

```
npm test
```

If the Fortran binaries or wasm build are missing, the corresponding tests will be skipped with a message.
