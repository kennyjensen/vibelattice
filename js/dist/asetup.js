// Port of AVL asetup.f (SETUP, GUCALC, GDCALC, GAMSUM, VELSUM) with float32 math for numerical fidelity.

const f32 = Math.fround;

import {
  VVOR,
  VSRD,
  SRDSET,
  DOT,
  CROSS,
} from './aic.js';

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function idx3(i, j, k, dim1, dim2) {
  return i + dim1 * (j + dim2 * k);
}

// uComp expects 1-based VINF/WROT (Fortran-style); aoper wraps them for ASETUP.
function uComp(state, iu) {
  if (iu <= 3) {
    return state.VINF[iu];
  }
  return state.WROT[iu - 3];
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

function MUNGEA(state) {
  for (let j = 1; j <= state.NSTRIP; j += 1) {
    if (!state.LSTRIPOFF[j]) continue;
    const i1 = state.IJFRST[j];
    for (let k = 1; k <= state.NVSTRP[j]; k += 1) {
      const ii = i1 + k - 1;
      for (let i = 1; i <= state.NVOR; i += 1) {
        state.AICN[idxA(ii, i, state.NVMAX + 1)] = 0.0;
      }
      state.AICN[idxA(ii, ii, state.NVMAX + 1)] = 1.0;
    }
  }
}

export function SETUP(state) {
  state.AMACH = state.MACH;
  state.BETM = f32(Math.sqrt(f32(1.0 - f32(state.AMACH * state.AMACH))));

  if (!state.LAIC) {
    VVOR(state.BETM, state.IYSYM, state.YSYM, state.IZSYM, state.ZSYM,
      state.VRCOREC, state.VRCOREW,
      state.NVOR, state.RV1, state.RV2, state.LVCOMP, state.CHORDV,
      state.NVOR, state.RC, state.LVCOMP, false,
      state.WC_GAM, state.NVMAX);

    for (let i = 1; i <= state.NVOR; i += 1) {
      for (let j = 1; j <= state.NVOR; j += 1) {
        state.AICN[idxA(i, j, state.NVMAX + 1)] = f32(
          f32(state.WC_GAM[idx3(1, i, j, 4, state.NVOR + 1)] * state.ENC[idx2(1, i, 4)])
          + f32(state.WC_GAM[idx3(2, i, j, 4, state.NVOR + 1)] * state.ENC[idx2(2, i, 4)])
          + f32(state.WC_GAM[idx3(3, i, j, 4, state.NVOR + 1)] * state.ENC[idx2(3, i, 4)])
        );
        state.LVNC[i] = true;
      }
    }

    for (let n = 1; n <= state.NSURF; n += 1) {
      if (state.LFWAKE[n]) continue;
      const j1 = state.JFRST[n];
      const jn = j1 + state.NJ[n] - 1;
      for (let j = j1; j <= jn; j += 1) {
        const i1 = state.IJFRST[j];
        const iv = state.IJFRST[j] + state.NVSTRP[j] - 1;
        for (let jv = 1; jv <= state.NVOR; jv += 1) {
          state.AICN[idxA(iv, jv, state.NVMAX + 1)] = 0.0;
        }
        state.LVNC[iv] = false;
        for (let jv = i1; jv <= iv; jv += 1) {
          state.AICN[idxA(iv, jv, state.NVMAX + 1)] = 1.0;
        }
      }
    }

    MUNGEA(state);
    LUDCMP_COL(state.NVMAX + 1, state.NVOR, state.AICN, state.IAPIV, state.WORK);
    state.LAIC = true;
  }

  if (!state.LSRD) {
    SRDSET(state.BETM, state.XYZREF, state.IYSYM,
      state.NBODY, state.LFRST, state.NLMAX,
      state.NL, state.RL, state.RADL,
      state.SRC_U, state.DBL_U);

    VSRD(state.BETM, state.IYSYM, state.YSYM, state.IZSYM, state.ZSYM, state.SRCORE,
      state.NBODY, state.LFRST, state.NLMAX,
      state.NL, state.RL, state.RADL,
      6, state.SRC_U, state.DBL_U,
      state.NVOR, state.RC,
      state.WCSRD_U, state.NVMAX);
    state.LSRD = true;
  }

  if (!state.LVEL) {
    VVOR(state.BETM, state.IYSYM, state.YSYM, state.IZSYM, state.ZSYM,
      state.VRCOREC, state.VRCOREW,
      state.NVOR, state.RV1, state.RV2, state.LVCOMP, state.CHORDV,
      state.NVOR, state.RV, state.LVCOMP, true,
      state.WV_GAM, state.NVMAX);

    VSRD(state.BETM, state.IYSYM, state.YSYM, state.IZSYM, state.ZSYM, state.SRCORE,
      state.NBODY, state.LFRST, state.NLMAX,
      state.NL, state.RL, state.RADL,
      6, state.SRC_U, state.DBL_U,
      state.NVOR, state.RV,
      state.WVSRD_U, state.NVMAX);
    state.LVEL = true;
  }

  return state;
}

export function GUCALC(state) {
  const RROT = new Float32Array(3);
  const VUNIT = new Float32Array(3);
  const WUNIT = new Float32Array(3);

  for (let iu = 1; iu <= 3; iu += 1) {
    for (let i = 1; i <= state.NVOR; i += 1) {
      if (state.LVNC[i]) {
        VUNIT[0] = 0.0;
        VUNIT[1] = 0.0;
        VUNIT[2] = 0.0;
        if (state.LVALBE[i]) {
          VUNIT[iu - 1] = f32(VUNIT[iu - 1] + 1.0);
        }
        VUNIT[0] = f32(VUNIT[0] + state.WCSRD_U[idx3(1, i, iu, 4, state.NVOR + 1)]);
        VUNIT[1] = f32(VUNIT[1] + state.WCSRD_U[idx3(2, i, iu, 4, state.NVOR + 1)]);
        VUNIT[2] = f32(VUNIT[2] + state.WCSRD_U[idx3(3, i, iu, 4, state.NVOR + 1)]);

        state.GAM_U_0[idx2(i, iu, state.NVOR + 1)] = f32(-DOT(state.ENC.subarray(idx2(1, i, 4), idx2(1, i, 4) + 3), VUNIT));
        for (let n = 1; n <= state.NCONTROL; n += 1) {
          state.GAM_U_D[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = f32(
            -DOT(state.ENC_D.subarray(idx3(1, i, n, 4, state.NVOR + 1), idx3(1, i, n, 4, state.NVOR + 1) + 3), VUNIT)
          );
        }
        for (let n = 1; n <= state.NDESIGN; n += 1) {
          state.GAM_U_G[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = f32(
            -DOT(state.ENC_G.subarray(idx3(1, i, n, 4, state.NVOR + 1), idx3(1, i, n, 4, state.NVOR + 1) + 3), VUNIT)
          );
        }
      } else {
        state.GAM_U_0[idx2(i, iu, state.NVOR + 1)] = 0.0;
        for (let n = 1; n <= state.NCONTROL; n += 1) {
          state.GAM_U_D[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = 0.0;
        }
        for (let n = 1; n <= state.NDESIGN; n += 1) {
          state.GAM_U_G[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = 0.0;
        }
      }
    }

    {
      const col = new Float32Array(state.NVOR + 1);
      for (let i = 1; i <= state.NVOR; i += 1) {
        col[i] = state.GAM_U_0[idx2(i, iu, state.NVOR + 1)];
      }
      BAKSUB_COL(state.NVMAX + 1, state.NVOR, state.AICN, state.IAPIV, col);
      for (let i = 1; i <= state.NVOR; i += 1) {
        state.GAM_U_0[idx2(i, iu, state.NVOR + 1)] = col[i];
      }
    }
    for (let n = 1; n <= state.NCONTROL; n += 1) {
      const col = new Float32Array(state.NVOR + 1);
      for (let i = 1; i <= state.NVOR; i += 1) {
        col[i] = state.GAM_U_D[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)];
      }
      BAKSUB_COL(state.NVMAX + 1, state.NVOR, state.AICN, state.IAPIV, col);
      for (let i = 1; i <= state.NVOR; i += 1) {
        state.GAM_U_D[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = col[i];
      }
    }
    for (let n = 1; n <= state.NDESIGN; n += 1) {
      const col = new Float32Array(state.NVOR + 1);
      for (let i = 1; i <= state.NVOR; i += 1) {
        col[i] = state.GAM_U_G[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)];
      }
      BAKSUB_COL(state.NVMAX + 1, state.NVOR, state.AICN, state.IAPIV, col);
      for (let i = 1; i <= state.NVOR; i += 1) {
        state.GAM_U_G[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = col[i];
      }
    }
  }

  for (let iu = 4; iu <= 6; iu += 1) {
    for (let i = 1; i <= state.NVOR; i += 1) {
      if (state.LVNC[i]) {
        WUNIT[0] = 0.0;
        WUNIT[1] = 0.0;
        WUNIT[2] = 0.0;
        if (state.LVALBE[i]) {
          WUNIT[iu - 4] = f32(WUNIT[iu - 4] + 1.0);
        }
        RROT[0] = f32(state.RC[idx2(1, i, 4)] - state.XYZREF[0]);
        RROT[1] = f32(state.RC[idx2(2, i, 4)] - state.XYZREF[1]);
        RROT[2] = f32(state.RC[idx2(3, i, 4)] - state.XYZREF[2]);
        const vunit = CROSS(RROT, WUNIT, VUNIT);
        vunit[0] = f32(vunit[0] + state.WCSRD_U[idx3(1, i, iu, 4, state.NVOR + 1)]);
        vunit[1] = f32(vunit[1] + state.WCSRD_U[idx3(2, i, iu, 4, state.NVOR + 1)]);
        vunit[2] = f32(vunit[2] + state.WCSRD_U[idx3(3, i, iu, 4, state.NVOR + 1)]);

        state.GAM_U_0[idx2(i, iu, state.NVOR + 1)] = f32(-DOT(state.ENC.subarray(idx2(1, i, 4), idx2(1, i, 4) + 3), vunit));
        for (let n = 1; n <= state.NCONTROL; n += 1) {
          state.GAM_U_D[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = f32(
            -DOT(state.ENC_D.subarray(idx3(1, i, n, 4, state.NVOR + 1), idx3(1, i, n, 4, state.NVOR + 1) + 3), vunit)
          );
        }
        for (let n = 1; n <= state.NDESIGN; n += 1) {
          state.GAM_U_G[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = f32(
            -DOT(state.ENC_G.subarray(idx3(1, i, n, 4, state.NVOR + 1), idx3(1, i, n, 4, state.NVOR + 1) + 3), vunit)
          );
        }
      } else {
        state.GAM_U_0[idx2(i, iu, state.NVOR + 1)] = 0.0;
        for (let n = 1; n <= state.NCONTROL; n += 1) {
          state.GAM_U_D[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = 0.0;
        }
        for (let n = 1; n <= state.NDESIGN; n += 1) {
          state.GAM_U_G[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = 0.0;
        }
      }
    }
    {
      const col = new Float32Array(state.NVOR + 1);
      for (let i = 1; i <= state.NVOR; i += 1) {
        col[i] = state.GAM_U_0[idx2(i, iu, state.NVOR + 1)];
      }
      BAKSUB_COL(state.NVMAX + 1, state.NVOR, state.AICN, state.IAPIV, col);
      for (let i = 1; i <= state.NVOR; i += 1) {
        state.GAM_U_0[idx2(i, iu, state.NVOR + 1)] = col[i];
      }
    }
    for (let n = 1; n <= state.NCONTROL; n += 1) {
      const col = new Float32Array(state.NVOR + 1);
      for (let i = 1; i <= state.NVOR; i += 1) {
        col[i] = state.GAM_U_D[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)];
      }
      BAKSUB_COL(state.NVMAX + 1, state.NVOR, state.AICN, state.IAPIV, col);
      for (let i = 1; i <= state.NVOR; i += 1) {
        state.GAM_U_D[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = col[i];
      }
    }
    for (let n = 1; n <= state.NDESIGN; n += 1) {
      const col = new Float32Array(state.NVOR + 1);
      for (let i = 1; i <= state.NVOR; i += 1) {
        col[i] = state.GAM_U_G[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)];
      }
      BAKSUB_COL(state.NVMAX + 1, state.NVOR, state.AICN, state.IAPIV, col);
      for (let i = 1; i <= state.NVOR; i += 1) {
        state.GAM_U_G[idx3(i, iu, n, state.NVOR + 1, state.NUMAX + 1)] = col[i];
      }
    }
  }

  return state;
}

export function GDCALC(state, NQDEF, LQDEF, ENC_Q, GAM_Q) {
  if (NQDEF === 0) return state;
  const RROT = new Float32Array(3);
  const VROT = new Float32Array(3);
  const VQ = new Float32Array(3);

  for (let iq = 1; iq <= NQDEF; iq += 1) {
    if (!LQDEF[iq]) continue;
    for (let i = 1; i <= state.NVOR; i += 1) {
      if (state.LVNC[i]) {
        if (state.LVALBE[i]) {
          RROT[0] = f32(state.RC[idx2(1, i, 4)] - state.XYZREF[0]);
          RROT[1] = f32(state.RC[idx2(2, i, 4)] - state.XYZREF[1]);
          RROT[2] = f32(state.RC[idx2(3, i, 4)] - state.XYZREF[2]);
          const vrot = CROSS(RROT, state.WROT, VROT);
          VQ[0] = f32(state.VINF[1] + vrot[0]);
          VQ[1] = f32(state.VINF[2] + vrot[1]);
          VQ[2] = f32(state.VINF[3] + vrot[2]);
        } else {
          VQ[0] = 0.0;
          VQ[1] = 0.0;
          VQ[2] = 0.0;
        }
        for (let k = 1; k <= 3; k += 1) {
          VQ[k - 1] = f32(VQ[k - 1]
            + f32(state.WCSRD_U[idx3(k, i, 1, 4, state.NVOR + 1)] * state.VINF[1])
            + f32(state.WCSRD_U[idx3(k, i, 2, 4, state.NVOR + 1)] * state.VINF[2])
            + f32(state.WCSRD_U[idx3(k, i, 3, 4, state.NVOR + 1)] * state.VINF[3])
            + f32(state.WCSRD_U[idx3(k, i, 4, 4, state.NVOR + 1)] * state.WROT[1])
            + f32(state.WCSRD_U[idx3(k, i, 5, 4, state.NVOR + 1)] * state.WROT[2])
            + f32(state.WCSRD_U[idx3(k, i, 6, 4, state.NVOR + 1)] * state.WROT[3]));
        }
        GAM_Q[idx2(i, iq, state.NVOR + 1)] = f32(-DOT(ENC_Q.subarray(idx3(1, i, iq, 4, state.NVOR + 1), idx3(1, i, iq, 4, state.NVOR + 1) + 3), VQ));
      } else {
        GAM_Q[idx2(i, iq, state.NVOR + 1)] = 0.0;
      }
    }
    BAKSUB_COL(state.NVMAX + 1, state.NVOR, state.AICN, state.IAPIV, GAM_Q.subarray(idx2(1, iq, state.NVOR + 1)));
  }
  return state;
}

export function GAMSUM(state) {
  const { NVOR, NCONTROL, NDESIGN, NLNODE, NUMAX } = state;
  const dimN = state.DIM_N;
  const dimU = state.DIM_U;
  const dimC = state.DIM_C;
  const dimG = state.DIM_G;
  const dimL = state.DIM_L;
  const dimK = 4;

  for (let i = 1; i <= NVOR; i += 1) {
    for (let iu = 1; iu <= NUMAX; iu += 1) {
      let sum = state.GAM_U_0[idx2(i, iu, dimN)];
      for (let n = 1; n <= NCONTROL; n += 1) {
        sum = f32(sum + f32(state.GAM_U_D[idx3(i, iu, n, dimN, dimU)] * state.DELCON[n]));
      }
      for (let n = 1; n <= NDESIGN; n += 1) {
        sum = f32(sum + f32(state.GAM_U_G[idx3(i, iu, n, dimN, dimU)] * state.DELDES[n]));
      }
      state.GAM_U[idx2(i, iu, dimN)] = f32(sum);
    }

    for (let n = 1; n <= NCONTROL; n += 1) {
      let sum = f32(0.0);
      for (let iu = 1; iu <= NUMAX; iu += 1) {
        sum = f32(sum + f32(state.GAM_U_D[idx3(i, iu, n, dimN, dimU)] * uComp(state, iu)));
      }
      state.GAM_D[idx2(i, n, dimN)] = f32(sum);
    }

    for (let n = 1; n <= NDESIGN; n += 1) {
      let sum = f32(0.0);
      for (let iu = 1; iu <= NUMAX; iu += 1) {
        sum = f32(sum + f32(state.GAM_U_G[idx3(i, iu, n, dimN, dimU)] * uComp(state, iu)));
      }
      state.GAM_G[idx2(i, n, dimN)] = f32(sum);
    }

    let gsum = f32(0.0);
    for (let iu = 1; iu <= NUMAX; iu += 1) {
      gsum = f32(gsum + f32(state.GAM_U[idx2(i, iu, dimN)] * uComp(state, iu)));
    }
    state.GAM[i] = f32(gsum);
  }

  for (let l = 1; l <= NLNODE; l += 1) {
    let ssum = f32(0.0);
    for (let iu = 1; iu <= NUMAX; iu += 1) {
      ssum = f32(ssum + f32(state.SRC_U[idx2(l, iu, dimL)] * uComp(state, iu)));
    }
    state.SRC[l] = f32(ssum);

    for (let k = 1; k <= 3; k += 1) {
      let dsum = f32(0.0);
      for (let iu = 1; iu <= NUMAX; iu += 1) {
        dsum = f32(dsum + f32(state.DBL_U[idx3(k, l, iu, dimK, dimL)] * uComp(state, iu)));
      }
      state.DBL[idx2(k, l, dimK)] = f32(dsum);
    }
  }

  return state;
}

export function VELSUM(state) {
  const { NVOR, NCONTROL, NDESIGN, NUMAX, NDMAX, NGMAX } = state;
  const dimN = state.DIM_N;
  const dimU = state.DIM_U;
  const dimC = state.DIM_C;
  const dimG = state.DIM_G;
  const dimK = 4;

  for (let i = 1; i <= NVOR; i += 1) {
    for (let k = 1; k <= 3; k += 1) {
      let vc = f32(0.0);
      let vv = f32(0.0);
      for (let j = 1; j <= NVOR; j += 1) {
        vc = f32(vc + f32(state.WC_GAM[idx3(k, i, j, dimK, dimN)] * state.GAM[j]));
        vv = f32(vv + f32(state.WV_GAM[idx3(k, i, j, dimK, dimN)] * state.GAM[j]));
      }
      state.VC[idx2(k, i, dimK)] = f32(vc);
      state.VV[idx2(k, i, dimK)] = f32(vv);

      for (let n = 1; n <= NUMAX; n += 1) {
        let vcU = f32(0.0);
        let vvU = f32(0.0);
        for (let j = 1; j <= NVOR; j += 1) {
          vcU = f32(vcU + f32(state.WC_GAM[idx3(k, i, j, dimK, dimN)] * state.GAM_U[idx2(j, n, dimN)]));
          vvU = f32(vvU + f32(state.WV_GAM[idx3(k, i, j, dimK, dimN)] * state.GAM_U[idx2(j, n, dimN)]));
        }
        state.VC_U[idx3(k, i, n, dimK, dimN)] = f32(vcU);
        state.VV_U[idx3(k, i, n, dimK, dimN)] = f32(vvU);
      }

      for (let n = 1; n <= NCONTROL; n += 1) {
        let vcD = f32(0.0);
        let vvD = f32(0.0);
        for (let j = 1; j <= NVOR; j += 1) {
          vcD = f32(vcD + f32(state.WC_GAM[idx3(k, i, j, dimK, dimN)] * state.GAM_D[idx2(j, n, dimN)]));
          vvD = f32(vvD + f32(state.WV_GAM[idx3(k, i, j, dimK, dimN)] * state.GAM_D[idx2(j, n, dimN)]));
        }
        state.VC_D[idx3(k, i, n, dimK, dimN)] = f32(vcD);
        state.VV_D[idx3(k, i, n, dimK, dimN)] = f32(vvD);
      }

      for (let n = 1; n <= NDESIGN; n += 1) {
        let vcG = f32(0.0);
        let vvG = f32(0.0);
        for (let j = 1; j <= NVOR; j += 1) {
          vcG = f32(vcG + f32(state.WC_GAM[idx3(k, i, j, dimK, dimN)] * state.GAM_G[idx2(j, n, dimN)]));
          vvG = f32(vvG + f32(state.WV_GAM[idx3(k, i, j, dimK, dimN)] * state.GAM_G[idx2(j, n, dimN)]));
        }
        state.VC_G[idx3(k, i, n, dimK, dimN)] = f32(vcG);
        state.VV_G[idx3(k, i, n, dimK, dimN)] = f32(vvG);
      }

      let wcsrd = f32(0.0);
      let wvsrd = f32(0.0);
      for (let n = 1; n <= NUMAX; n += 1) {
        wcsrd = f32(wcsrd + f32(state.WCSRD_U[idx3(k, i, n, dimK, dimN)] * uComp(state, n)));
        wvsrd = f32(wvsrd + f32(state.WVSRD_U[idx3(k, i, n, dimK, dimN)] * uComp(state, n)));
      }
      state.WCSRD[idx2(k, i, dimK)] = f32(wcsrd);
      state.WVSRD[idx2(k, i, dimK)] = f32(wvsrd);

      state.WC[idx2(k, i, dimK)] = f32(vc + wcsrd);
      state.WV[idx2(k, i, dimK)] = f32(vv + wvsrd);

      for (let n = 1; n <= NUMAX; n += 1) {
        state.WC_U[idx3(k, i, n, dimK, dimN)] = f32(state.VC_U[idx3(k, i, n, dimK, dimN)] + state.WCSRD_U[idx3(k, i, n, dimK, dimN)]);
        state.WV_U[idx3(k, i, n, dimK, dimN)] = f32(state.VV_U[idx3(k, i, n, dimK, dimN)] + state.WVSRD_U[idx3(k, i, n, dimK, dimN)]);
      }

      for (let n = 1; n <= NDMAX; n += 1) {
        state.WC_D[idx3(k, i, n, dimK, dimN)] = state.VC_D[idx3(k, i, n, dimK, dimN)];
        state.WV_D[idx3(k, i, n, dimK, dimN)] = state.VV_D[idx3(k, i, n, dimK, dimN)];
      }

      for (let n = 1; n <= NGMAX; n += 1) {
        state.WC_G[idx3(k, i, n, dimK, dimN)] = state.VC_G[idx3(k, i, n, dimK, dimN)];
        state.WV_G[idx3(k, i, n, dimK, dimN)] = state.VV_G[idx3(k, i, n, dimK, dimN)];
      }
    }
  }

  return state;
}
