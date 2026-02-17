# VibeLattice Overview

VibeLattice is a browser-based workflow for working with AVL models, running cases, and visualizing results.
It is built as a numerically faithful JS/WASM port of AVL logic, with UI tooling around the original AVL file formats.

## AVL Credit

This project is based on and ports core functionality from **AVL (Athena Vortex Lattice)** by Mark Drela and Harold Youngren.

- Original AVL page: https://web.mit.edu/drela/Public/web/avl/
- See `LICENSE` in this repo for licensing details.

## Main Functionality

- Load, edit, and save `.avl` files in an in-browser AVL editor.
- Syntax-highlight AVL text and keep formatting suitable for direct AVL use.
- Load and save `.run` run-case files.
- Load and save `.mass` mass-property files.
- Auto-load companion `plane.run` and `plane.mass` when default `plane.avl` is loaded.
- Display and edit flight conditions, constraints, run cases, and mass/inertia values.
- Compute forces/derivatives using the JS/WASM solver pipeline.
- View total forces, stability derivatives, body-axis derivatives, surface forces, and hinge moments.
- Plot Trefftz-plane outputs.
- Plot eigenmodes and select modes for 3D animation.
- 3D aircraft view with toggles for:
  - wireframe and surface rendering modes
  - pressure visualization
  - panel spacing overlays
  - bound and leg vortices
  - flow-field visualization modes

## Template Parameters in AVL Files

VibeLattice supports template literals directly inside AVL text and creates sliders for them in the Aircraft panel.

### Supported Forms

- Basic variable: `${name}`
- Variable with default: `${name:1.5}` or `${name??1.5}`
- Expression support: simple math in template expressions (`+`, `-`, `*`, `/`, parentheses), for example:
  - `${span_base * scale}`
  - `${x0 + dx}`
  - `${(c_root + c_tip) / 2}`

### Slider Behavior

- Sliders are generated per template variable name.
- If no explicit default is given, default value is `1`.
- Default slider range is scaled around the default value (currently up to `10x` magnitude).
- Parameters in positive-only AVL contexts are enforced as strictly positive.
- Double-click or long-press a parameter name to rescale the slider around the current value for finer tuning.
- Changing sliders rewrites/resolves template values and refreshes geometry and solution outputs.

## Notes on Fidelity

- The codebase includes parity tests against Fortran reference executables for key solver paths.
- Some UI outputs use AVL sign conventions for display; internal force vectors may use solver-native orientation and be converted for presentation.
