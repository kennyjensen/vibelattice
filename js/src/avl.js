/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL avl.f with a minimal, non-interactive startup path.

const f32 = Math.fround;

export function DEFINI(state) {
  state.NDEFINI = (state.NDEFINI ?? 0) + 1;
  state.NRUN = 0;
  state.LGEO = false;
}

export function MASINI(state) {
  state.NMASINI = (state.NMASINI ?? 0) + 1;
  state.RMASS0 = f32(1.0);
}

export function AVLHEAP_INIT(state) {
  state.NHEAP = (state.NHEAP ?? 0) + 1;
}

export function PLINIT(state) {
  state.NPLINIT = (state.NPLINIT ?? 0) + 1;
}

export function PLCLOSE(state) {
  state.NPLCLOSE = (state.NPLCLOSE ?? 0) + 1;
  return state;
}

export function AVL_MAIN(state, io = {}) {
  const askc = io.askc ?? (() => ({ COMAND: 'Q   ', COMARG: ' ' }));
  const getarg0 = io.getarg0 ?? (() => ' ');

  state.VERSION = f32(3.52);
  state.PI = f32(4.0 * Math.atan(1.0));
  state.DTR = f32(state.PI / 180.0);

  state.LUINP = 4;
  state.LURUN = 7;
  state.LUMAS = 8;
  state.LUPRM = 9;
  state.LUOUT = 19;
  state.LUSTD = 20;
  state.LUSYS = 22;

  let linpfile = false;

  DEFINI(state);
  MASINI(state);
  AVLHEAP_INIT(state);
  PLINIT(state);

  const fildef = getarg0(1);
  if (fildef && fildef.trim() !== '') {
    // Not implemented in JS port: INPUT/PARSET/ENCALC/VARINI
    linpfile = true;
  }

  if (linpfile && typeof io.plpars === 'function') {
    io.plpars(state);
  }

  const cmd = askc(' AVL^');
  const comand = (cmd.COMAND ?? '    ').padEnd(4, ' ').slice(0, 4);

  if (comand === 'QUIT' || comand === 'Q   ') {
    PLCLOSE(state);
    if (typeof io.avlheap_clean === 'function') {
      io.avlheap_clean(state);
    }
    state.STOP = true;
  }

  return state;
}

export function avlSnapshot(state) {
  return [
    state.VERSION,
    state.PI,
    state.DTR,
    state.LUINP,
    state.LURUN,
    state.LUOUT,
    state.LUSTD,
    state.LUSYS,
    state.RMASS0,
    state.NDEFINI ?? 0,
    state.NMASINI ?? 0,
    state.NHEAP ?? 0,
    state.NPLINIT ?? 0,
    state.NPLCLOSE ?? 0,
  ];
}
