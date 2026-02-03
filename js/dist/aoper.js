/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL aoper.f (EXEC) with float32 math for numerical fidelity.

import { SETUP, GUCALC, GDCALC, GAMSUM, VELSUM, preloadAicWasm, preloadAsetpLuWasm } from './asetup.js';
import { AERO, VINFAB } from './aero.js';
import { loadAoperLinSolveWasm } from './aoper_linsolve_wasm.js';
import { loadAsetupWasm } from './asetup_wasm.js';
import { loadAeroWasm } from './aero_wasm.js';

const f32 = Math.fround;
let wasmLinSolve = null;
let wasmAsetup = null;
let wasmAero = null;

export async function preloadAoperLinSolveWasm() {
  if (wasmLinSolve) return wasmLinSolve;
  wasmLinSolve = await loadAoperLinSolveWasm();
  return wasmLinSolve;
}

export async function preloadAsetupWasm() {
  if (wasmAsetup) return wasmAsetup;
  wasmAsetup = await loadAsetupWasm();
  return wasmAsetup;
}

export async function preloadAeroWasm() {
  if (wasmAero) return wasmAero;
  wasmAero = await loadAeroWasm();
  return wasmAero;
}

export async function preloadAicWasmBridge() {
  return preloadAicWasm();
}

export async function preloadAsetpLuWasmBridge() {
  return preloadAsetpLuWasm();
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function idx2_0(i, j, dim1) {
  return i + dim1 * j;
}

function idxA(i, j, dim1) {
  return i + dim1 * j;
}

function LUDCMP_COL(dim, n, A, INDX, WORK) {
  for (let i = 1; i <= n; i += 1) {
    let aamax = f32(0.0);
    for (let j = 1; j <= n; j += 1) {
      const val = Math.abs(A[idxA(i, j, dim)]);
      if (val > aamax) aamax = f32(val);
    }
    WORK[i] = aamax === 0.0 ? 0.0 : f32(1.0 / aamax);
  }

  for (let j = 1; j <= n; j += 1) {
    for (let i = 1; i < j; i += 1) {
      let sum = f32(A[idxA(i, j, dim)]);
      for (let k = 1; k < i; k += 1) {
        sum = f32(sum - f32(A[idxA(i, k, dim)] * A[idxA(k, j, dim)]));
      }
      A[idxA(i, j, dim)] = sum;
    }

    let aamax = f32(0.0);
    let imax = j;
    for (let i = j; i <= n; i += 1) {
      let sum = f32(A[idxA(i, j, dim)]);
      for (let k = 1; k < j; k += 1) {
        sum = f32(sum - f32(A[idxA(i, k, dim)] * A[idxA(k, j, dim)]));
      }
      A[idxA(i, j, dim)] = sum;
      const dum = f32(WORK[i] * Math.abs(sum));
      if (dum >= aamax) {
        aamax = dum;
        imax = i;
      }
    }

    if (j !== imax) {
      for (let k = 1; k <= n; k += 1) {
        const dum = A[idxA(imax, k, dim)];
        A[idxA(imax, k, dim)] = A[idxA(j, k, dim)];
        A[idxA(j, k, dim)] = dum;
      }
      WORK[imax] = WORK[j];
    }

    INDX[j] = imax;

    if (j !== n) {
      const piv = A[idxA(j, j, dim)] === 0.0 ? 1.0e-12 : A[idxA(j, j, dim)];
      const dum = f32(1.0 / piv);
      for (let i = j + 1; i <= n; i += 1) {
        A[idxA(i, j, dim)] = f32(A[idxA(i, j, dim)] * dum);
      }
    }
  }
}

function BAKSUB_COL(dim, n, A, INDX, B) {
  let ii = 0;

  for (let i = 1; i <= n; i += 1) {
    const ll = INDX[i];
    let sum = f32(B[ll]);
    B[ll] = B[i];
    if (ii !== 0) {
      for (let j = ii; j <= i - 1; j += 1) {
        sum = f32(sum - f32(A[idxA(i, j, dim)] * B[j]));
      }
    } else if (sum !== 0.0) {
      ii = i;
    }
    B[i] = sum;
  }

  for (let i = n; i >= 1; i -= 1) {
    let sum = f32(B[i]);
    if (i < n) {
      for (let j = i + 1; j <= n; j += 1) {
        sum = f32(sum - f32(A[idxA(i, j, dim)] * B[j]));
      }
    }
    const piv = A[idxA(i, i, dim)] === 0.0 ? 1.0e-12 : A[idxA(i, i, dim)];
    B[i] = f32(sum / piv);
  }
}

function LUDCMP_COL64(dim, n, A, INDX, WORK) {
  for (let i = 1; i <= n; i += 1) {
    let aamax = 0.0;
    for (let j = 1; j <= n; j += 1) {
      const val = Math.abs(A[idxA(i, j, dim)]);
      if (val > aamax) aamax = val;
    }
    WORK[i] = aamax === 0.0 ? 0.0 : 1.0 / aamax;
  }

  for (let j = 1; j <= n; j += 1) {
    for (let i = 1; i < j; i += 1) {
      let sum = A[idxA(i, j, dim)];
      for (let k = 1; k < i; k += 1) {
        sum -= A[idxA(i, k, dim)] * A[idxA(k, j, dim)];
      }
      A[idxA(i, j, dim)] = sum;
    }

    let aamax = 0.0;
    let imax = j;
    for (let i = j; i <= n; i += 1) {
      let sum = A[idxA(i, j, dim)];
      for (let k = 1; k < j; k += 1) {
        sum -= A[idxA(i, k, dim)] * A[idxA(k, j, dim)];
      }
      A[idxA(i, j, dim)] = sum;
      const dum = WORK[i] * Math.abs(sum);
      if (dum >= aamax) {
        aamax = dum;
        imax = i;
      }
    }

    if (j !== imax) {
      for (let k = 1; k <= n; k += 1) {
        const dum = A[idxA(imax, k, dim)];
        A[idxA(imax, k, dim)] = A[idxA(j, k, dim)];
        A[idxA(j, k, dim)] = dum;
      }
      WORK[imax] = WORK[j];
    }

    INDX[j] = imax;

    if (j !== n) {
      const dum = 1.0 / A[idxA(j, j, dim)];
      for (let i = j + 1; i <= n; i += 1) {
        A[idxA(i, j, dim)] = A[idxA(i, j, dim)] * dum;
      }
    }
  }
}

function BAKSUB_COL64(dim, n, A, INDX, B) {
  let ii = 0;

  for (let i = 1; i <= n; i += 1) {
    const ll = INDX[i];
    let sum = B[ll];
    B[ll] = B[i];
    if (ii !== 0) {
      for (let j = ii; j <= i - 1; j += 1) {
        sum -= A[idxA(i, j, dim)] * B[j];
      }
    } else if (sum !== 0.0) {
      ii = i;
    }
    B[i] = sum;
  }

  for (let i = n; i >= 1; i -= 1) {
    let sum = B[i];
    if (i < n) {
      for (let j = i + 1; j <= n; j += 1) {
        sum -= A[idxA(i, j, dim)] * B[j];
      }
    }
    B[i] = sum / A[idxA(i, i, dim)];
  }
}

// ASETUP/GAMSUM expect 1-based VINF/WROT; mirror 0-based arrays into 1-based buffers.
function syncVinfWrot1(state, vinf1, wrot1) {
  vinf1[1] = f32(state.VINF[0]);
  vinf1[2] = f32(state.VINF[1]);
  vinf1[3] = f32(state.VINF[2]);
  wrot1[1] = f32(state.WROT[0]);
  wrot1[2] = f32(state.WROT[1]);
  wrot1[3] = f32(state.WROT[2]);
}

function withAsetpVinfWrot(state, vinf1, wrot1, fn) {
  const prevV = state.VINF;
  const prevW = state.WROT;
  state.VINF = vinf1;
  state.WROT = wrot1;
  const res = fn();
  state.VINF = prevV;
  state.WROT = prevW;
  return res;
}

function runAsetupSums(state, vinf1, wrot1) {
  if (state.USE_WASM_GAM && wasmAsetup) {
    const prevV = state.VINF;
    const prevW = state.WROT;
    state.VINF = vinf1;
    state.WROT = wrot1;
    wasmAsetup.GAMSUM_wasm(state);
    wasmAsetup.VELSUM_wasm(state);
    state.VINF = prevV;
    state.WROT = prevW;
    return;
  }
  withAsetpVinfWrot(state, vinf1, wrot1, () => {
    GAMSUM(state);
    VELSUM(state);
  });
}

export function EXEC(state, NITER, INFO, IR) {
  const niter = NITER ?? 0;
  const info = INFO ?? 0;
  const ir = IR ?? 1;
  const dir = state.LNASA_SA ? -1.0 : 1.0;

  const vinf1 = new Float32Array(4);
  const wrot1 = new Float32Array(4);

  state.LSOL = false;

  state.XYZREF[0] = f32(state.PARVAL[idx2(state.IPXCG, ir, state.IPTOT)]);
  state.XYZREF[1] = f32(state.PARVAL[idx2(state.IPYCG, ir, state.IPTOT)]);
  state.XYZREF[2] = f32(state.PARVAL[idx2(state.IPZCG, ir, state.IPTOT)]);

  state.CDREF = f32(state.PARVAL[idx2(state.IPCD0, ir, state.IPTOT)]);

  state.MACH = f32(state.PARVAL[idx2(state.IPMACH, ir, state.IPTOT)]);

  if (state.MACH !== state.AMACH) {
    state.LAIC = false;
    state.LSRD = false;
    state.LVEL = false;
    state.LSOL = false;
    state.LSEN = false;
    state.LOBAIC = false;
    state.LOBVEL = false;
  }

  SETUP(state);

  if (niter > 0) {
    if (state.ICON[idx2(state.IVALFA, ir, state.IVMAX)] === state.ICALFA) {
      state.ALFA = f32(state.CONVAL[idx2(state.ICALFA, ir, state.ICMAX)] * state.DTR);
    }
    if (state.ICON[idx2(state.IVBETA, ir, state.IVMAX)] === state.ICBETA) {
      state.BETA = f32(state.CONVAL[idx2(state.ICBETA, ir, state.ICMAX)] * state.DTR);
    }
    if (state.ICON[idx2(state.IVROTX, ir, state.IVMAX)] === state.ICROTX) {
      state.WROT[0] = f32(state.CONVAL[idx2(state.ICROTX, ir, state.ICMAX)] * 2.0 / state.BREF);
    }
    if (state.ICON[idx2(state.IVROTY, ir, state.IVMAX)] === state.ICROTY) {
      state.WROT[1] = f32(state.CONVAL[idx2(state.ICROTY, ir, state.ICMAX)] * 2.0 / state.CREF);
    }
    if (state.ICON[idx2(state.IVROTZ, ir, state.IVMAX)] === state.ICROTZ) {
      state.WROT[2] = f32(state.CONVAL[idx2(state.ICROTZ, ir, state.ICMAX)] * 2.0 / state.BREF);
    }
  }

  GUCALC(state);

  if (state.USE_WASM_AERO && wasmAero) {
    try {
      const vinf = wasmAero.VINFAB(state);
      if (vinf?.VINF) state.VINF = Float32Array.from(vinf.VINF);
      if (vinf?.VINF_A) state.VINF_A = Float32Array.from(vinf.VINF_A);
      if (vinf?.VINF_B) state.VINF_B = Float32Array.from(vinf.VINF_B);
    } catch {
      state.USE_WASM_AERO = false;
      VINFAB(state);
    }
  } else {
    VINFAB(state);
  }
  syncVinfWrot1(state, vinf1, wrot1);

  runAsetupSums(state, vinf1, wrot1);

  if (state.USE_WASM_AERO && wasmAero) {
    try {
      wasmAero.AERO(state);
    } catch {
      state.USE_WASM_AERO = false;
      AERO(state);
    }
  } else {
    AERO(state);
  }

  if (niter > 0) {
    const ivmax = state.IVMAX;
    const vsys = new Float64Array((ivmax + 1) * (ivmax + 1));
    const vres = new Float64Array(ivmax + 1);
    const ddc = new Float64Array(state.NDMAX + 1);
    const work = new Float64Array(ivmax + 1);
    const ivsys = new Int32Array(ivmax + 1);

    for (let iter = 1; iter <= niter; iter += 1) {
      let ca;
      let sa;
      let caA;
      let saA;
      if (state.LSA_RATES) {
        ca = f32(Math.cos(state.ALFA));
        sa = f32(Math.sin(state.ALFA));
        caA = f32(-sa);
        saA = f32(ca);
      } else {
        ca = 1.0;
        sa = 0.0;
        caA = 0.0;
        saA = 0.0;
      }

      for (let k = 1; k <= ivmax; k += 1) {
        for (let l = 1; l <= ivmax; l += 1) {
          vsys[idxA(k, l, ivmax + 1)] = 0.0;
        }
      }

      for (let iv = 1; iv <= state.NVTOT; iv += 1) {
        const ic = state.ICON[idx2(iv, ir, state.IVMAX)];
        if (ic === 0) {
          vres[iv] = 0.0;
          vsys[idxA(iv, iv, ivmax + 1)] = 1.0;
        } else if (ic === state.ICALFA) {
          vres[iv] = f32(state.ALFA - f32(state.CONVAL[idx2(ic, ir, state.ICMAX)] * state.DTR));
          vsys[idxA(iv, state.IVALFA, ivmax + 1)] = 1.0;
        } else if (ic === state.ICBETA) {
          vres[iv] = f32(state.BETA - f32(state.CONVAL[idx2(ic, ir, state.ICMAX)] * state.DTR));
          vsys[idxA(iv, state.IVBETA, ivmax + 1)] = 1.0;
        } else if (ic === state.ICROTX) {
          vres[iv] = f32((state.WROT[0] * ca + state.WROT[2] * sa) * dir
            - f32(state.CONVAL[idx2(ic, ir, state.ICMAX)] * 2.0 / state.BREF));
          vsys[idxA(iv, state.IVROTX, ivmax + 1)] = f32(ca * dir);
          vsys[idxA(iv, state.IVROTZ, ivmax + 1)] = f32(sa * dir);
          vsys[idxA(iv, state.IVALFA, ivmax + 1)] = f32((state.WROT[0] * caA + state.WROT[2] * saA) * dir);
        } else if (ic === state.ICROTY) {
          vres[iv] = f32(state.WROT[1]
            - f32(state.CONVAL[idx2(ic, ir, state.ICMAX)] * 2.0 / state.CREF));
          vsys[idxA(iv, state.IVROTY, ivmax + 1)] = 1.0;
        } else if (ic === state.ICROTZ) {
          vres[iv] = f32((state.WROT[2] * ca - state.WROT[0] * sa) * dir
            - f32(state.CONVAL[idx2(ic, ir, state.ICMAX)] * 2.0 / state.BREF));
          vsys[idxA(iv, state.IVROTX, ivmax + 1)] = f32(-sa * dir);
          vsys[idxA(iv, state.IVROTZ, ivmax + 1)] = f32(ca * dir);
          vsys[idxA(iv, state.IVALFA, ivmax + 1)] = f32((state.WROT[2] * caA - state.WROT[0] * saA) * dir);
        } else if (ic === state.ICCL) {
          vres[iv] = f32(state.CLTOT - state.CONVAL[idx2(ic, ir, state.ICMAX)]);
          vsys[idxA(iv, state.IVALFA, ivmax + 1)] = f32(
            state.CLTOT_U[0] * state.VINF_A[0]
              + state.CLTOT_U[1] * state.VINF_A[1]
              + state.CLTOT_U[2] * state.VINF_A[2]
              + state.CLTOT_A,
          );
          vsys[idxA(iv, state.IVBETA, ivmax + 1)] = f32(
            state.CLTOT_U[0] * state.VINF_B[0]
              + state.CLTOT_U[1] * state.VINF_B[1]
              + state.CLTOT_U[2] * state.VINF_B[2],
          );
          vsys[idxA(iv, state.IVROTX, ivmax + 1)] = f32(state.CLTOT_U[3]);
          vsys[idxA(iv, state.IVROTY, ivmax + 1)] = f32(state.CLTOT_U[4]);
          vsys[idxA(iv, state.IVROTZ, ivmax + 1)] = f32(state.CLTOT_U[5]);
          for (let n = 1; n <= state.NCONTROL; n += 1) {
            const nv = state.IVTOT + n;
            vsys[idxA(iv, nv, ivmax + 1)] = f32(state.CLTOT_D[n - 1]);
          }
        } else if (ic === state.ICCY) {
          vres[iv] = f32(state.CYTOT - state.CONVAL[idx2(ic, ir, state.ICMAX)]);
          vsys[idxA(iv, state.IVALFA, ivmax + 1)] = f32(
            state.CYTOT_U[0] * state.VINF_A[0]
              + state.CYTOT_U[1] * state.VINF_A[1]
              + state.CYTOT_U[2] * state.VINF_A[2],
          );
          vsys[idxA(iv, state.IVBETA, ivmax + 1)] = f32(
            state.CYTOT_U[0] * state.VINF_B[0]
              + state.CYTOT_U[1] * state.VINF_B[1]
              + state.CYTOT_U[2] * state.VINF_B[2],
          );
          vsys[idxA(iv, state.IVROTX, ivmax + 1)] = f32(state.CYTOT_U[3]);
          vsys[idxA(iv, state.IVROTY, ivmax + 1)] = f32(state.CYTOT_U[4]);
          vsys[idxA(iv, state.IVROTZ, ivmax + 1)] = f32(state.CYTOT_U[5]);
          for (let n = 1; n <= state.NCONTROL; n += 1) {
            const nv = state.IVTOT + n;
            vsys[idxA(iv, nv, ivmax + 1)] = f32(state.CYTOT_D[n - 1]);
          }
        } else if (ic === state.ICMOMX) {
          const cmt1A = f32(
            state.CMTOT_U[idx2_0(0, 0, 3)] * state.VINF_A[0]
              + state.CMTOT_U[idx2_0(0, 1, 3)] * state.VINF_A[1]
              + state.CMTOT_U[idx2_0(0, 2, 3)] * state.VINF_A[2],
          );
          const cmt3A = f32(
            state.CMTOT_U[idx2_0(2, 0, 3)] * state.VINF_A[0]
              + state.CMTOT_U[idx2_0(2, 1, 3)] * state.VINF_A[1]
              + state.CMTOT_U[idx2_0(2, 2, 3)] * state.VINF_A[2],
          );
          const cmt1B = f32(
            state.CMTOT_U[idx2_0(0, 0, 3)] * state.VINF_B[0]
              + state.CMTOT_U[idx2_0(0, 1, 3)] * state.VINF_B[1]
              + state.CMTOT_U[idx2_0(0, 2, 3)] * state.VINF_B[2],
          );
          const cmt3B = f32(
            state.CMTOT_U[idx2_0(2, 0, 3)] * state.VINF_B[0]
              + state.CMTOT_U[idx2_0(2, 1, 3)] * state.VINF_B[1]
              + state.CMTOT_U[idx2_0(2, 2, 3)] * state.VINF_B[2],
          );
          vres[iv] = f32((state.CMTOT[0] * ca + state.CMTOT[2] * sa) * dir
            - state.CONVAL[idx2(ic, ir, state.ICMAX)]);
          vsys[idxA(iv, state.IVALFA, ivmax + 1)] = f32(
            f32(cmt1A * ca + cmt3A * sa) * dir
              + f32(state.CMTOT[0] * caA + state.CMTOT[2] * saA) * dir,
          );
          vsys[idxA(iv, state.IVBETA, ivmax + 1)] = f32(f32(cmt1B * ca + cmt3B * sa) * dir);
          vsys[idxA(iv, state.IVROTX, ivmax + 1)] = f32(
            f32(state.CMTOT_U[idx2_0(0, 3, 3)] * ca
              + state.CMTOT_U[idx2_0(2, 3, 3)] * sa) * dir,
          );
          vsys[idxA(iv, state.IVROTY, ivmax + 1)] = f32(
            f32(state.CMTOT_U[idx2_0(0, 4, 3)] * ca
              + state.CMTOT_U[idx2_0(2, 4, 3)] * sa) * dir,
          );
          vsys[idxA(iv, state.IVROTZ, ivmax + 1)] = f32(
            f32(state.CMTOT_U[idx2_0(0, 5, 3)] * ca
              + state.CMTOT_U[idx2_0(2, 5, 3)] * sa) * dir,
          );
          for (let n = 1; n <= state.NCONTROL; n += 1) {
            const nv = state.IVTOT + n;
            vsys[idxA(iv, nv, ivmax + 1)] = f32(
              f32(state.CMTOT_D[idx2_0(0, n - 1, 3)] * ca
                + state.CMTOT_D[idx2_0(2, n - 1, 3)] * sa) * dir,
            );
          }
        } else if (ic === state.ICMOMY) {
          vres[iv] = f32(state.CMTOT[1] - state.CONVAL[idx2(ic, ir, state.ICMAX)]);
          vsys[idxA(iv, state.IVALFA, ivmax + 1)] = f32(
            state.CMTOT_U[idx2_0(1, 0, 3)] * state.VINF_A[0]
              + state.CMTOT_U[idx2_0(1, 1, 3)] * state.VINF_A[1]
              + state.CMTOT_U[idx2_0(1, 2, 3)] * state.VINF_A[2],
          );
          vsys[idxA(iv, state.IVBETA, ivmax + 1)] = f32(
            state.CMTOT_U[idx2_0(1, 0, 3)] * state.VINF_B[0]
              + state.CMTOT_U[idx2_0(1, 1, 3)] * state.VINF_B[1]
              + state.CMTOT_U[idx2_0(1, 2, 3)] * state.VINF_B[2],
          );
          vsys[idxA(iv, state.IVROTX, ivmax + 1)] = f32(state.CMTOT_U[idx2_0(1, 3, 3)]);
          vsys[idxA(iv, state.IVROTY, ivmax + 1)] = f32(state.CMTOT_U[idx2_0(1, 4, 3)]);
          vsys[idxA(iv, state.IVROTZ, ivmax + 1)] = f32(state.CMTOT_U[idx2_0(1, 5, 3)]);
          for (let n = 1; n <= state.NCONTROL; n += 1) {
            const nv = state.IVTOT + n;
            vsys[idxA(iv, nv, ivmax + 1)] = f32(state.CMTOT_D[idx2_0(1, n - 1, 3)]);
          }
        } else if (ic === state.ICMOMZ) {
          const cmt3A = f32(
            state.CMTOT_U[idx2_0(2, 0, 3)] * state.VINF_A[0]
              + state.CMTOT_U[idx2_0(2, 1, 3)] * state.VINF_A[1]
              + state.CMTOT_U[idx2_0(2, 2, 3)] * state.VINF_A[2],
          );
          const cmt1A = f32(
            state.CMTOT_U[idx2_0(0, 0, 3)] * state.VINF_A[0]
              + state.CMTOT_U[idx2_0(0, 1, 3)] * state.VINF_A[1]
              + state.CMTOT_U[idx2_0(0, 2, 3)] * state.VINF_A[2],
          );
          const cmt3B = f32(
            state.CMTOT_U[idx2_0(2, 0, 3)] * state.VINF_B[0]
              + state.CMTOT_U[idx2_0(2, 1, 3)] * state.VINF_B[1]
              + state.CMTOT_U[idx2_0(2, 2, 3)] * state.VINF_B[2],
          );
          const cmt1B = f32(
            state.CMTOT_U[idx2_0(0, 0, 3)] * state.VINF_B[0]
              + state.CMTOT_U[idx2_0(0, 1, 3)] * state.VINF_B[1]
              + state.CMTOT_U[idx2_0(0, 2, 3)] * state.VINF_B[2],
          );
          vres[iv] = f32((state.CMTOT[2] * ca - state.CMTOT[0] * sa) * dir
            - state.CONVAL[idx2(ic, ir, state.ICMAX)]);
          vsys[idxA(iv, state.IVALFA, ivmax + 1)] = f32(
            f32(cmt3A * ca - cmt1A * sa) * dir
              + f32(state.CMTOT[2] * caA - state.CMTOT[0] * saA) * dir,
          );
          vsys[idxA(iv, state.IVBETA, ivmax + 1)] = f32(f32(cmt3B * ca - cmt1B * sa) * dir);
          vsys[idxA(iv, state.IVROTX, ivmax + 1)] = f32(
            f32(state.CMTOT_U[idx2_0(2, 3, 3)] * ca
              - state.CMTOT_U[idx2_0(0, 3, 3)] * sa) * dir,
          );
          vsys[idxA(iv, state.IVROTY, ivmax + 1)] = f32(
            f32(state.CMTOT_U[idx2_0(2, 4, 3)] * ca
              - state.CMTOT_U[idx2_0(0, 4, 3)] * sa) * dir,
          );
          vsys[idxA(iv, state.IVROTZ, ivmax + 1)] = f32(
            f32(state.CMTOT_U[idx2_0(2, 5, 3)] * ca
              - state.CMTOT_U[idx2_0(0, 5, 3)] * sa) * dir,
          );
          for (let n = 1; n <= state.NCONTROL; n += 1) {
            const nv = state.IVTOT + n;
            vsys[idxA(iv, nv, ivmax + 1)] = f32(
              f32(state.CMTOT_D[idx2_0(2, n - 1, 3)] * ca
                - state.CMTOT_D[idx2_0(0, n - 1, 3)] * sa) * dir,
            );
          }
        } else {
          let matched = false;
          for (let n = 1; n <= state.NCONTROL; n += 1) {
            const iccon = state.ICTOT + n;
            const ivcon = state.IVTOT + n;
            if (ic === iccon) {
              vres[iv] = f32(state.DELCON[n] - state.CONVAL[idx2(iccon, ir, state.ICMAX)]);
              vsys[idxA(iv, ivcon, ivmax + 1)] = 1.0;
              matched = true;
              break;
            }
          }
          if (!matched) {
            throw new Error(`Illegal constraint index ${ic}`);
          }
        }
      }

      if (state.USE_WASM_SOLVE && wasmLinSolve) {
        const solved = wasmLinSolve.solveLinearSystem(vsys, vres, ivmax + 1, state.NVTOT);
        vres.set(solved);
      } else {
        LUDCMP_COL64(ivmax + 1, state.NVTOT, vsys, ivsys, work);
        BAKSUB_COL64(ivmax + 1, state.NVTOT, vsys, ivsys, vres);
      }

      let badSolve = false;
      for (let iv = 1; iv <= state.NVTOT; iv += 1) {
        if (!Number.isFinite(vres[iv])) {
          badSolve = true;
          break;
        }
      }
      if (badSolve) {
        return state;
      }

      const dal = -vres[state.IVALFA];
      const dbe = -vres[state.IVBETA];
      const dwx = -vres[state.IVROTX];
      const dwy = -vres[state.IVROTY];
      const dwz = -vres[state.IVROTZ];
      for (let n = 1; n <= state.NCONTROL; n += 1) {
        const iv = state.IVTOT + n;
        ddc[n] = -vres[iv];
      }

      const dmax = 1.5708;
      const dmaxa = dmax;
      const dmaxr = 5.0 * dmax / state.BREF;
      if (Math.abs(state.ALFA + dal) > dmaxa) {
        return state;
      }
      if (Math.abs(state.BETA + dbe) > dmaxa) {
        return state;
      }
      if (Math.abs(state.WROT[0] + dwx) > dmaxr) {
        return state;
      }
      if (Math.abs(state.WROT[1] + dwy) > dmaxr) {
        return state;
      }
      if (Math.abs(state.WROT[2] + dwz) > dmaxr) {
        return state;
      }

      state.ALFA = f32(state.ALFA + dal);
      state.BETA = f32(state.BETA + dbe);
      state.WROT[0] = f32(state.WROT[0] + dwx);
      state.WROT[1] = f32(state.WROT[1] + dwy);
      state.WROT[2] = f32(state.WROT[2] + dwz);
      for (let k = 1; k <= state.NCONTROL; k += 1) {
        state.DELCON[k] = f32(state.DELCON[k] + ddc[k]);
      }

      VINFAB(state);
      syncVinfWrot1(state, vinf1, wrot1);

      if (state.NCONTROL > 0) {
        withAsetpVinfWrot(state, vinf1, wrot1, () => {
          GDCALC(state, state.NCONTROL, state.LCONDEF, state.ENC_D, state.GAM_D);
        });
      }
      if (state.NDESIGN > 0) {
        withAsetpVinfWrot(state, vinf1, wrot1, () => {
          GDCALC(state, state.NDESIGN, state.LDESDEF, state.ENC_G, state.GAM_G);
        });
      }

      runAsetupSums(state, vinf1, wrot1);

      if (state.USE_WASM_AERO && wasmAero) {
        try {
          wasmAero.AERO(state);
        } catch {
          state.USE_WASM_AERO = false;
          AERO(state);
        }
      } else {
        AERO(state);
      }

      let delmax = Math.max(
        Math.abs(dal),
        Math.abs(dbe),
        Math.abs(dwx * state.BREF / 2.0),
        Math.abs(dwy * state.CREF / 2.0),
        Math.abs(dwz * state.BREF / 2.0),
      );
      for (let k = 1; k <= state.NCONTROL; k += 1) {
        delmax = Math.max(delmax, Math.abs(ddc[k]));
      }

      if (delmax < 0.00002) {
        state.LSOL = true;
        state.LOBVEL = false;
        if (state.ITRIM) {
          state.ITRIM[ir] = Math.abs(state.ITRIM[ir]);
        }
        break;
      }
    }

    if (!state.LSOL) {
      state.LOBVEL = false;
      return state;
    }
  }

  state.PARVAL[idx2(state.IPALFA, ir, state.IPTOT)] = f32(state.ALFA / state.DTR);
  state.PARVAL[idx2(state.IPBETA, ir, state.IPTOT)] = f32(state.BETA / state.DTR);
  state.PARVAL[idx2(state.IPROTX, ir, state.IPTOT)] = f32(state.WROT[0] * 0.5 * state.BREF);
  state.PARVAL[idx2(state.IPROTY, ir, state.IPTOT)] = f32(state.WROT[1] * 0.5 * state.CREF);
  state.PARVAL[idx2(state.IPROTZ, ir, state.IPTOT)] = f32(state.WROT[2] * 0.5 * state.BREF);
  state.PARVAL[idx2(state.IPCL, ir, state.IPTOT)] = f32(state.CLTOT);

  state.LSEN = true;
  return state;
}
