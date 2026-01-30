// Port of AVL aero.f (AERO, SFFORC, BDFORC, VINFAB) with float32 math.

import { CROSS, DOT } from './aic.js';
import { CDCL } from './cdcl.js';
import { TPFORC } from './atpforc.js';

const f32 = Math.fround;

// All solver geometry arrays are Fortran 1-based with stride 4 (x/y/z).
// Use idx3 with 0-based component (0..2) and 1-based element index.
function idx3(r, c) {
  return (r + 1) + 4 * c;
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

// 3D vector array with Fortran-style stride-4 components and 1-based i,n indices.
function idx3u(r, i, n, dimN) {
  return (r + 1) + 4 * (i + dimN * n);
}

export function VINFAB(state) {
  const sina = f32(Math.sin(f32(state.ALFA)));
  const cosa = f32(Math.cos(f32(state.ALFA)));
  const sinb = f32(Math.sin(f32(state.BETA)));
  const cosb = f32(Math.cos(f32(state.BETA)));

  state.VINF[0] = f32(cosa * cosb);
  state.VINF[1] = f32(-sinb);
  state.VINF[2] = f32(sina * cosb);

  state.VINF_A[0] = f32(-sina * cosb);
  state.VINF_A[1] = 0.0;
  state.VINF_A[2] = f32(cosa * cosb);

  state.VINF_B[0] = f32(-cosa * sinb);
  state.VINF_B[1] = f32(-cosb);
  state.VINF_B[2] = f32(-sina * sinb);

  return state;
}

export function AERO(state) {
  state.CDTOT = 0.0;
  state.CYTOT = 0.0;
  state.CLTOT = 0.0;
  for (let l = 0; l < 3; l += 1) {
    state.CFTOT[l] = 0.0;
    state.CMTOT[l] = 0.0;
  }
  state.CDVTOT = 0.0;

  state.CDTOT_A = 0.0;
  state.CLTOT_A = 0.0;

  for (let l = 0; l < state.NCONTROL; l += 1) {
    state.CHINGE[l] = 0.0;
  }

  for (let n = 0; n < state.NUMAX; n += 1) {
    state.CDTOT_U[n] = 0.0;
    state.CYTOT_U[n] = 0.0;
    state.CLTOT_U[n] = 0.0;
    for (let l = 0; l < 3; l += 1) {
      state.CFTOT_U[idx2(l, n, 3)] = 0.0;
      state.CMTOT_U[idx2(l, n, 3)] = 0.0;
    }
    for (let l = 0; l < state.NCONTROL; l += 1) {
      state.CHINGE_U[idx2(l, n, state.NCONTROL)] = 0.0;
    }
  }

  for (let n = 0; n < state.NCONTROL; n += 1) {
    state.CDTOT_D[n] = 0.0;
    state.CYTOT_D[n] = 0.0;
    state.CLTOT_D[n] = 0.0;
    for (let l = 0; l < 3; l += 1) {
      state.CFTOT_D[idx2(l, n, 3)] = 0.0;
      state.CMTOT_D[idx2(l, n, 3)] = 0.0;
    }
    for (let l = 0; l < state.NCONTROL; l += 1) {
      state.CHINGE_D[idx2(l, n, state.NCONTROL)] = 0.0;
    }
  }

  for (let n = 0; n < state.NDESIGN; n += 1) {
    state.CDTOT_G[n] = 0.0;
    state.CYTOT_G[n] = 0.0;
    state.CLTOT_G[n] = 0.0;
    for (let l = 0; l < 3; l += 1) {
      state.CFTOT_G[idx2(l, n, 3)] = 0.0;
      state.CMTOT_G[idx2(l, n, 3)] = 0.0;
    }
    for (let l = 0; l < state.NCONTROL; l += 1) {
      state.CHINGE_G[idx2(l, n, state.NCONTROL)] = 0.0;
    }
  }

  SFFORC(state);
  BDFORC(state);
  TPFORC(state);

  if (state.IYSYM === 1) {
    state.CDTOT = f32(2.0 * state.CDTOT);
    state.CYTOT = 0.0;
    state.CLTOT = f32(2.0 * state.CLTOT);
    state.CFTOT[0] = f32(2.0 * state.CFTOT[0]);
    state.CFTOT[1] = 0.0;
    state.CFTOT[2] = f32(2.0 * state.CFTOT[2]);
    state.CMTOT[0] = 0.0;
    state.CMTOT[1] = f32(2.0 * state.CMTOT[1]);
    state.CMTOT[2] = 0.0;
    state.CDVTOT = f32(2.0 * state.CDVTOT);

    state.CDTOT_A = f32(2.0 * state.CDTOT_A);
    state.CLTOT_A = f32(2.0 * state.CLTOT_A);

    for (let n = 0; n < state.NUMAX; n += 1) {
      state.CDTOT_U[n] = f32(2.0 * state.CDTOT_U[n]);
      state.CYTOT_U[n] = 0.0;
      state.CLTOT_U[n] = f32(2.0 * state.CLTOT_U[n]);
      state.CFTOT_U[idx2(0, n, 3)] = f32(2.0 * state.CFTOT_U[idx2(0, n, 3)]);
      state.CFTOT_U[idx2(1, n, 3)] = 0.0;
      state.CFTOT_U[idx2(2, n, 3)] = f32(2.0 * state.CFTOT_U[idx2(2, n, 3)]);
      state.CMTOT_U[idx2(0, n, 3)] = 0.0;
      state.CMTOT_U[idx2(1, n, 3)] = f32(2.0 * state.CMTOT_U[idx2(1, n, 3)]);
      state.CMTOT_U[idx2(2, n, 3)] = 0.0;
    }

    for (let n = 0; n < state.NCONTROL; n += 1) {
      state.CDTOT_D[n] = f32(2.0 * state.CDTOT_D[n]);
      state.CYTOT_D[n] = 0.0;
      state.CLTOT_D[n] = f32(2.0 * state.CLTOT_D[n]);
      state.CFTOT_D[idx2(0, n, 3)] = f32(2.0 * state.CFTOT_D[idx2(0, n, 3)]);
      state.CFTOT_D[idx2(1, n, 3)] = 0.0;
      state.CFTOT_D[idx2(2, n, 3)] = f32(2.0 * state.CFTOT_D[idx2(2, n, 3)]);
      state.CMTOT_D[idx2(0, n, 3)] = 0.0;
      state.CMTOT_D[idx2(1, n, 3)] = f32(2.0 * state.CMTOT_D[idx2(1, n, 3)]);
      state.CMTOT_D[idx2(2, n, 3)] = 0.0;
    }

    for (let n = 0; n < state.NDESIGN; n += 1) {
      state.CDTOT_G[n] = f32(2.0 * state.CDTOT_G[n]);
      state.CYTOT_G[n] = 0.0;
      state.CLTOT_G[n] = f32(2.0 * state.CLTOT_G[n]);
      state.CFTOT_G[idx2(0, n, 3)] = f32(2.0 * state.CFTOT_G[idx2(0, n, 3)]);
      state.CFTOT_G[idx2(1, n, 3)] = 0.0;
      state.CFTOT_G[idx2(2, n, 3)] = f32(2.0 * state.CFTOT_G[idx2(2, n, 3)]);
      state.CMTOT_G[idx2(0, n, 3)] = 0.0;
      state.CMTOT_G[idx2(1, n, 3)] = f32(2.0 * state.CMTOT_G[idx2(1, n, 3)]);
      state.CMTOT_G[idx2(2, n, 3)] = 0.0;
    }
  }

  const vsq = f32(f32(state.VINF[0] * state.VINF[0])
    + f32(state.VINF[1] * state.VINF[1])
    + f32(state.VINF[2] * state.VINF[2]));
  const vmag = f32(Math.sqrt(vsq));

  state.CDVTOT = f32(state.CDVTOT + f32(state.CDREF * vsq));
  state.CDTOT = f32(state.CDTOT + f32(state.CDREF * vsq));
  state.CYTOT = f32(state.CYTOT + f32(state.CDREF * f32(state.VINF[1] * vmag)));
  for (let l = 0; l < 3; l += 1) {
    state.CFTOT[l] = f32(state.CFTOT[l] + f32(state.CDREF * f32(state.VINF[l] * vmag)));
    state.CFTOT_U[idx2(l, l, 3)] = f32(state.CFTOT_U[idx2(l, l, 3)] + f32(state.CDREF * vmag));
  }

  for (let iu = 0; iu < 3; iu += 1) {
    state.CDTOT_U[iu] = f32(state.CDTOT_U[iu] + f32(state.CDREF * f32(2.0 * state.VINF[iu])));
    for (let l = 0; l < 3; l += 1) {
      state.CFTOT_U[idx2(l, iu, 3)] = f32(state.CFTOT_U[idx2(l, iu, 3)]
        + f32(state.CDREF * f32(state.VINF[l] * state.VINF[iu]) / vmag));
    }
  }

  return state;
}

export function SFFORC(state) {
  const ICRS = [1, 2, 0];
  const JCRS = [2, 0, 1];
  const dimStrip = state.NSTRIP + 1;
  const dimSurf = state.NSURF + 1;
  const dimN = state.DIM_N;

  const sina = f32(Math.sin(f32(state.ALFA)));
  const cosa = f32(Math.cos(f32(state.ALFA)));

  for (let j = 1; j <= state.NSTRIP; j += 1) {
    const i1 = state.IJFRST[j];
    const nvc = state.NVSTRP[j];

    if (state.LSTRIPOFF[j] || state.WSTRIP[j] === 0.0) {
      state.CNC[j] = 0.0;
      state.CDSTRP[j] = 0.0;
      state.CYSTRP[j] = 0.0;
      state.CLSTRP[j] = 0.0;
      state.CDST_A[j] = 0.0;
      state.CYST_A[j] = 0.0;
      state.CLST_A[j] = 0.0;
      state.CDV_LSTRP[j] = 0.0;
      state.CL_LSTRP[j] = 0.0;
      state.CD_LSTRP[j] = 0.0;
      state.CMC4_LSTRP[j] = 0.0;
      state.CA_LSTRP[j] = 0.0;
      state.CN_LSTRP[j] = 0.0;
      state.CLT_LSTRP[j] = 0.0;
      state.CLA_LSTRP[j] = 0.0;
      state.CMLE_LSTRP[j] = 0.0;

      for (let l = 0; l < 3; l += 1) {
        state.CF_LSTRP[idx2(l, j, 3)] = 0.0;
        state.CM_LSTRP[idx2(l, j, 3)] = 0.0;
        state.CFSTRP[idx2(l, j, 3)] = 0.0;
        state.CMSTRP[idx2(l, j, 3)] = 0.0;
      }

      for (let n = 0; n < state.NUMAX; n += 1) {
        state.CNC_U[idx2(j, n, dimStrip)] = 0.0;
        state.CDST_U[idx2(j, n, dimStrip)] = 0.0;
        state.CYST_U[idx2(j, n, dimStrip)] = 0.0;
        state.CLST_U[idx2(j, n, dimStrip)] = 0.0;
        for (let l = 0; l < 3; l += 1) {
          state.CFST_U[idx2(l, idx2(j, n, dimStrip), 3)] = 0.0;
          state.CMST_U[idx2(l, idx2(j, n, dimStrip), 3)] = 0.0;
        }
      }

      for (let n = 0; n < state.NCONTROL; n += 1) {
        state.CNC_D[idx2(j, n, dimStrip)] = 0.0;
        state.CDST_D[idx2(j, n, dimStrip)] = 0.0;
        state.CYST_D[idx2(j, n, dimStrip)] = 0.0;
        state.CLST_D[idx2(j, n, dimStrip)] = 0.0;
        for (let l = 0; l < 3; l += 1) {
          state.CFST_D[idx2(l, idx2(j, n, dimStrip), 3)] = 0.0;
          state.CMST_D[idx2(l, idx2(j, n, dimStrip), 3)] = 0.0;
        }
      }

      for (let n = 0; n < state.NDESIGN; n += 1) {
        state.CNC_G[idx2(j, n, dimStrip)] = 0.0;
        state.CDST_G[idx2(j, n, dimStrip)] = 0.0;
        state.CYST_G[idx2(j, n, dimStrip)] = 0.0;
        state.CLST_G[idx2(j, n, dimStrip)] = 0.0;
        for (let l = 0; l < 3; l += 1) {
          state.CFST_G[idx2(l, idx2(j, n, dimStrip), 3)] = 0.0;
          state.CMST_G[idx2(l, idx2(j, n, dimStrip), 3)] = 0.0;
        }
      }

      for (let ii = 0; ii < nvc; ii += 1) {
        const i = i1 + ii;
        state.DCP[i] = 0.0;
        for (let n = 0; n < state.NUMAX; n += 1) {
          const nu = n + 1;
          state.DCP_U[idx2(i, nu, dimN)] = 0.0;
        }
        for (let n = 0; n < state.NCONTROL; n += 1) {
          const nd = n + 1;
          state.DCP_D[idx2(i, nd, dimN)] = 0.0;
        }
        for (let n = 0; n < state.NDESIGN; n += 1) {
          const ng = n + 1;
          state.DCP_G[idx2(i, ng, dimN)] = 0.0;
        }
      }

      continue;
    }

    const cr = f32(state.CHORD[j]);
    const sr = f32(state.CHORD[j] * state.WSTRIP[j]);

    const xte1 = f32(state.RLE1[idx3(0, j)] + state.CHORD1[j]);
    const xte2 = f32(state.RLE2[idx3(0, j)] + state.CHORD2[j]);

    const spn = new Float32Array(3);
    spn[0] = 0.0;
    spn[1] = f32(state.ENSZ[j]);
    spn[2] = f32(-state.ENSY[j]);

    const udrag = new Float32Array(3);
    const udrag_u = new Float32Array(3 * state.NUMAX);
    for (let k = 0; k < 3; k += 1) {
      udrag[k] = f32(state.VINF[k]);
      for (let n = 0; n < state.NUMAX; n += 1) {
        udrag_u[idx2(k, n, 3)] = 0.0;
      }
      udrag_u[idx2(k, k, 3)] = 1.0;
    }

    const ulift = new Float32Array(3);
    CROSS(udrag, spn, ulift);
    let ulmag = f32(Math.sqrt(f32(DOT(ulift, ulift))));

    const ulift_u = new Float32Array(3 * state.NUMAX);
    const ulift_d = new Float32Array(3 * state.NCONTROL);
    const ulift_g = new Float32Array(3 * state.NDESIGN);
    const ulmag_u = new Float32Array(state.NUMAX);

    if (ulmag === 0.0) {
      ulift[0] = 0.0; ulift[1] = 0.0; ulift[2] = 1.0;
      for (let n = 0; n < state.NUMAX; n += 1) {
        ulift_u[idx2(0, n, 3)] = 0.0;
        ulift_u[idx2(1, n, 3)] = 0.0;
        ulift_u[idx2(2, n, 3)] = 0.0;
      }
    } else {
      for (let k = 0; k < 3; k += 1) {
        const ic = ICRS[k];
        const jc = JCRS[k];
        ulift[k] = f32(udrag[ic] * spn[jc] - udrag[jc] * spn[ic]);
        for (let n = 0; n < state.NUMAX; n += 1) {
          ulift_u[idx2(k, n, 3)] = f32(udrag_u[idx2(ic, n, 3)] * spn[jc]
            - udrag_u[idx2(jc, n, 3)] * spn[ic]);
        }
      }
      ulmag = f32(Math.sqrt(f32(DOT(ulift, ulift))));
      for (let n = 0; n < state.NUMAX; n += 1) {
        ulmag_u[n] = f32((ulift[0] * ulift_u[idx2(0, n, 3)]
          + ulift[1] * ulift_u[idx2(1, n, 3)]
          + ulift[2] * ulift_u[idx2(2, n, 3)]) / ulmag);
      }
      for (let k = 0; k < 3; k += 1) {
        ulift[k] = f32(ulift[k] / ulmag);
        for (let n = 0; n < state.NUMAX; n += 1) {
          ulift_u[idx2(k, n, 3)] = f32((ulift_u[idx2(k, n, 3)]
            - f32(ulift[k] * ulmag_u[n])) / ulmag);
        }
      }
    }

    const rc4 = new Float32Array(3);
    rc4[0] = f32(state.RLE[idx3(0, j)] + f32(0.25 * cr));
    rc4[1] = f32(state.RLE[idx3(1, j)]);
    rc4[2] = f32(state.RLE[idx3(2, j)]);

    let cfx = 0.0; let cfy = 0.0; let cfz = 0.0;
    let cmx = 0.0; let cmy = 0.0; let cmz = 0.0;
    state.CNC[j] = 0.0;

    const cfx_u = new Float32Array(state.NUMAX);
    const cfy_u = new Float32Array(state.NUMAX);
    const cfz_u = new Float32Array(state.NUMAX);
    const cmx_u = new Float32Array(state.NUMAX);
    const cmy_u = new Float32Array(state.NUMAX);
    const cmz_u = new Float32Array(state.NUMAX);

    const cfx_d = new Float32Array(state.NCONTROL);
    const cfy_d = new Float32Array(state.NCONTROL);
    const cfz_d = new Float32Array(state.NCONTROL);
    const cmx_d = new Float32Array(state.NCONTROL);
    const cmy_d = new Float32Array(state.NCONTROL);
    const cmz_d = new Float32Array(state.NCONTROL);

    const cfx_g = new Float32Array(state.NDESIGN);
    const cfy_g = new Float32Array(state.NDESIGN);
    const cfz_g = new Float32Array(state.NDESIGN);
    const cmx_g = new Float32Array(state.NDESIGN);
    const cmy_g = new Float32Array(state.NDESIGN);
    const cmz_g = new Float32Array(state.NDESIGN);

    for (let n = 0; n < state.NUMAX; n += 1) {
      cfx_u[n] = 0.0; cfy_u[n] = 0.0; cfz_u[n] = 0.0;
      cmx_u[n] = 0.0; cmy_u[n] = 0.0; cmz_u[n] = 0.0;
      state.CNC_U[idx2(j, n, dimStrip)] = 0.0;
    }
    for (let n = 0; n < state.NCONTROL; n += 1) {
      cfx_d[n] = 0.0; cfy_d[n] = 0.0; cfz_d[n] = 0.0;
      cmx_d[n] = 0.0; cmy_d[n] = 0.0; cmz_d[n] = 0.0;
      state.CNC_D[idx2(j, n, dimStrip)] = 0.0;
    }
    for (let n = 0; n < state.NDESIGN; n += 1) {
      cfx_g[n] = 0.0; cfy_g[n] = 0.0; cfz_g[n] = 0.0;
      cmx_g[n] = 0.0; cmy_g[n] = 0.0; cmz_g[n] = 0.0;
      state.CNC_G[idx2(j, n, dimStrip)] = 0.0;
    }

    for (let ii = 0; ii < nvc; ii += 1) {
      const i = i1 + ii;

      const r = new Float32Array(3);
      r[0] = f32(state.RV[idx3(0, i)] - rc4[0]);
      r[1] = f32(state.RV[idx3(1, i)] - rc4[1]);
      r[2] = f32(state.RV[idx3(2, i)] - rc4[2]);

      const rrot = new Float32Array(3);
      rrot[0] = f32(state.RV[idx3(0, i)] - state.XYZREF[0]);
      rrot[1] = f32(state.RV[idx3(1, i)] - state.XYZREF[1]);
      rrot[2] = f32(state.RV[idx3(2, i)] - state.XYZREF[2]);

      const vrot = new Float32Array(3);
      CROSS(rrot, state.WROT, vrot);

      const veff = new Float32Array(3);
      const veff_u = new Float32Array(3 * state.NUMAX);
      const veff_d = new Float32Array(3 * state.NCONTROL);
      const veff_g = new Float32Array(3 * state.NDESIGN);

      if (state.LNFLD_WV) {
        veff[0] = f32(state.VINF[0] + vrot[0] + state.WV[idx3(0, i)]);
        veff[1] = f32(state.VINF[1] + vrot[1] + state.WV[idx3(1, i)]);
        veff[2] = f32(state.VINF[2] + vrot[2] + state.WV[idx3(2, i)]);

        for (let k = 0; k < 3; k += 1) {
          const nu = k + 1;
          veff_u[idx2(0, k, 3)] = f32(state.WV_U[idx3u(0, i, nu, dimN)]);
          veff_u[idx2(1, k, 3)] = f32(state.WV_U[idx3u(1, i, nu, dimN)]);
          veff_u[idx2(2, k, 3)] = f32(state.WV_U[idx3u(2, i, nu, dimN)]);
          veff_u[idx2(k, k, 3)] = f32(1.0 + veff_u[idx2(k, k, 3)]);
        }
        for (let k = 3; k < 6; k += 1) {
          const wrot_u = new Float32Array(3);
          wrot_u[0] = 0.0; wrot_u[1] = 0.0; wrot_u[2] = 0.0;
          wrot_u[k - 3] = 1.0;
          const vrot_u = new Float32Array(3);
          CROSS(rrot, wrot_u, vrot_u);
          const nu = k + 1;
          veff_u[idx2(0, k, 3)] = f32(vrot_u[0] + state.WV_U[idx3u(0, i, nu, dimN)]);
          veff_u[idx2(1, k, 3)] = f32(vrot_u[1] + state.WV_U[idx3u(1, i, nu, dimN)]);
          veff_u[idx2(2, k, 3)] = f32(vrot_u[2] + state.WV_U[idx3u(2, i, nu, dimN)]);
        }
        for (let n = 0; n < state.NCONTROL; n += 1) {
          const nd = n + 1;
          veff_d[idx2(0, n, 3)] = f32(state.WV_D[idx3u(0, i, nd, dimN)]);
          veff_d[idx2(1, n, 3)] = f32(state.WV_D[idx3u(1, i, nd, dimN)]);
          veff_d[idx2(2, n, 3)] = f32(state.WV_D[idx3u(2, i, nd, dimN)]);
        }
        for (let n = 0; n < state.NDESIGN; n += 1) {
          const ng = n + 1;
          veff_g[idx2(0, n, 3)] = f32(state.WV_G[idx3u(0, i, ng, dimN)]);
          veff_g[idx2(1, n, 3)] = f32(state.WV_G[idx3u(1, i, ng, dimN)]);
          veff_g[idx2(2, n, 3)] = f32(state.WV_G[idx3u(2, i, ng, dimN)]);
        }
      } else {
        veff[0] = f32(state.VINF[0] + vrot[0] + state.VV[idx3(0, i)]);
        veff[1] = f32(state.VINF[1] + vrot[1] + state.VV[idx3(1, i)]);
        veff[2] = f32(state.VINF[2] + vrot[2] + state.VV[idx3(2, i)]);

        for (let k = 0; k < 3; k += 1) {
          const nu = k + 1;
          veff_u[idx2(0, k, 3)] = f32(state.VV_U[idx3u(0, i, nu, dimN)]);
          veff_u[idx2(1, k, 3)] = f32(state.VV_U[idx3u(1, i, nu, dimN)]);
          veff_u[idx2(2, k, 3)] = f32(state.VV_U[idx3u(2, i, nu, dimN)]);
          veff_u[idx2(k, k, 3)] = f32(1.0 + veff_u[idx2(k, k, 3)]);
        }
        for (let k = 3; k < 6; k += 1) {
          const wrot_u = new Float32Array(3);
          wrot_u[0] = 0.0; wrot_u[1] = 0.0; wrot_u[2] = 0.0;
          wrot_u[k - 3] = 1.0;
          const vrot_u = new Float32Array(3);
          CROSS(rrot, wrot_u, vrot_u);
          const nu = k + 1;
          veff_u[idx2(0, k, 3)] = f32(vrot_u[0] + state.VV_U[idx3u(0, i, nu, dimN)]);
          veff_u[idx2(1, k, 3)] = f32(vrot_u[1] + state.VV_U[idx3u(1, i, nu, dimN)]);
          veff_u[idx2(2, k, 3)] = f32(vrot_u[2] + state.VV_U[idx3u(2, i, nu, dimN)]);
        }
        for (let n = 0; n < state.NCONTROL; n += 1) {
          const nd = n + 1;
          veff_d[idx2(0, n, 3)] = f32(state.VV_D[idx3u(0, i, nd, dimN)]);
          veff_d[idx2(1, n, 3)] = f32(state.VV_D[idx3u(1, i, nd, dimN)]);
          veff_d[idx2(2, n, 3)] = f32(state.VV_D[idx3u(2, i, nd, dimN)]);
        }
        for (let n = 0; n < state.NDESIGN; n += 1) {
          const ng = n + 1;
          veff_g[idx2(0, n, 3)] = f32(state.VV_G[idx3u(0, i, ng, dimN)]);
          veff_g[idx2(1, n, 3)] = f32(state.VV_G[idx3u(1, i, ng, dimN)]);
          veff_g[idx2(2, n, 3)] = f32(state.VV_G[idx3u(2, i, ng, dimN)]);
        }
      }

      const g = new Float32Array(3);
      g[0] = f32(state.RV2[idx3(0, i)] - state.RV1[idx3(0, i)]);
      g[1] = f32(state.RV2[idx3(1, i)] - state.RV1[idx3(1, i)]);
      g[2] = f32(state.RV2[idx3(2, i)] - state.RV1[idx3(2, i)]);

      const f = new Float32Array(3);
      CROSS(veff, g, f);

      const f_u = new Float32Array(3 * state.NUMAX);
      const f_d = new Float32Array(3 * state.NCONTROL);
      const f_g = new Float32Array(3 * state.NDESIGN);

      for (let n = 0; n < state.NUMAX; n += 1) {
        const tmp = new Float32Array(3);
        tmp[0] = veff_u[idx2(0, n, 3)];
        tmp[1] = veff_u[idx2(1, n, 3)];
        tmp[2] = veff_u[idx2(2, n, 3)];
        const out = new Float32Array(3);
        CROSS(tmp, g, out);
        f_u[idx2(0, n, 3)] = out[0];
        f_u[idx2(1, n, 3)] = out[1];
        f_u[idx2(2, n, 3)] = out[2];
      }
      for (let n = 0; n < state.NCONTROL; n += 1) {
        const tmp = new Float32Array(3);
        tmp[0] = veff_d[idx2(0, n, 3)];
        tmp[1] = veff_d[idx2(1, n, 3)];
        tmp[2] = veff_d[idx2(2, n, 3)];
        const out = new Float32Array(3);
        CROSS(tmp, g, out);
        f_d[idx2(0, n, 3)] = out[0];
        f_d[idx2(1, n, 3)] = out[1];
        f_d[idx2(2, n, 3)] = out[2];
      }
      for (let n = 0; n < state.NDESIGN; n += 1) {
        const tmp = new Float32Array(3);
        tmp[0] = veff_g[idx2(0, n, 3)];
        tmp[1] = veff_g[idx2(1, n, 3)];
        tmp[2] = veff_g[idx2(2, n, 3)];
        const out = new Float32Array(3);
        CROSS(tmp, g, out);
        f_g[idx2(0, n, 3)] = out[0];
        f_g[idx2(1, n, 3)] = out[1];
        f_g[idx2(2, n, 3)] = out[2];
      }

      const fgam = new Float32Array(3);
      fgam[0] = f32(2.0 * state.GAM[i] * f[0]);
      fgam[1] = f32(2.0 * state.GAM[i] * f[1]);
      fgam[2] = f32(2.0 * state.GAM[i] * f[2]);

      const fgam_u = new Float32Array(3 * state.NUMAX);
      const fgam_d = new Float32Array(3 * state.NCONTROL);
      const fgam_g = new Float32Array(3 * state.NDESIGN);

      for (let n = 0; n < state.NUMAX; n += 1) {
        const nu = n + 1;
        fgam_u[idx2(0, n, 3)] = f32(2.0 * state.GAM_U[idx2(i, nu, dimN)] * f[0]
          + 2.0 * state.GAM[i] * f_u[idx2(0, n, 3)]);
        fgam_u[idx2(1, n, 3)] = f32(2.0 * state.GAM_U[idx2(i, nu, dimN)] * f[1]
          + 2.0 * state.GAM[i] * f_u[idx2(1, n, 3)]);
        fgam_u[idx2(2, n, 3)] = f32(2.0 * state.GAM_U[idx2(i, nu, dimN)] * f[2]
          + 2.0 * state.GAM[i] * f_u[idx2(2, n, 3)]);
      }
      for (let n = 0; n < state.NCONTROL; n += 1) {
        const nd = n + 1;
        fgam_d[idx2(0, n, 3)] = f32(2.0 * state.GAM_D[idx2(i, nd, dimN)] * f[0]
          + 2.0 * state.GAM[i] * f_d[idx2(0, n, 3)]);
        fgam_d[idx2(1, n, 3)] = f32(2.0 * state.GAM_D[idx2(i, nd, dimN)] * f[1]
          + 2.0 * state.GAM[i] * f_d[idx2(1, n, 3)]);
        fgam_d[idx2(2, n, 3)] = f32(2.0 * state.GAM_D[idx2(i, nd, dimN)] * f[2]
          + 2.0 * state.GAM[i] * f_d[idx2(2, n, 3)]);
      }
      for (let n = 0; n < state.NDESIGN; n += 1) {
        const ng = n + 1;
        fgam_g[idx2(0, n, 3)] = f32(2.0 * state.GAM_G[idx2(i, ng, dimN)] * f[0]
          + 2.0 * state.GAM[i] * f_g[idx2(0, n, 3)]);
        fgam_g[idx2(1, n, 3)] = f32(2.0 * state.GAM_G[idx2(i, ng, dimN)] * f[1]
          + 2.0 * state.GAM[i] * f_g[idx2(1, n, 3)]);
        fgam_g[idx2(2, n, 3)] = f32(2.0 * state.GAM_G[idx2(i, ng, dimN)] * f[2]
          + 2.0 * state.GAM[i] * f_g[idx2(2, n, 3)]);
      }

      const env = new Float32Array(3);
      env[0] = state.ENV[idx3(0, i)];
      env[1] = state.ENV[idx3(1, i)];
      env[2] = state.ENV[idx3(2, i)];
      const fnv = f32(DOT(env, fgam));
      state.DCP[i] = f32(fnv / f32(state.DXV[i] * state.WSTRIP[j]));

      for (let n = 0; n < state.NUMAX; n += 1) {
        const ftmp = new Float32Array(3);
        ftmp[0] = fgam_u[idx2(0, n, 3)];
        ftmp[1] = fgam_u[idx2(1, n, 3)];
        ftmp[2] = fgam_u[idx2(2, n, 3)];
        const fnv_u = f32(DOT(env, ftmp));
        const nu = n + 1;
        state.DCP_U[idx2(i, nu, dimN)] = f32(fnv_u / f32(state.DXV[i] * state.WSTRIP[j]));
      }

      for (let n = 0; n < state.NCONTROL; n += 1) {
        const ftmp = new Float32Array(3);
        ftmp[0] = fgam_d[idx2(0, n, 3)];
        ftmp[1] = fgam_d[idx2(1, n, 3)];
        ftmp[2] = fgam_d[idx2(2, n, 3)];
        const envd = new Float32Array(3);
        const nd = n + 1;
        envd[0] = state.ENV_D[idx3u(0, i, nd, dimN)];
        envd[1] = state.ENV_D[idx3u(1, i, nd, dimN)];
        envd[2] = state.ENV_D[idx3u(2, i, nd, dimN)];
        const fnv_d = f32(DOT(env, ftmp) + DOT(envd, fgam));
        state.DCP_D[idx2(i, nd, dimN)] = f32(fnv_d / f32(state.DXV[i] * state.WSTRIP[j]));
      }

      for (let n = 0; n < state.NDESIGN; n += 1) {
        const ftmp = new Float32Array(3);
        ftmp[0] = fgam_g[idx2(0, n, 3)];
        ftmp[1] = fgam_g[idx2(1, n, 3)];
        ftmp[2] = fgam_g[idx2(2, n, 3)];
        const envg = new Float32Array(3);
        const ng = n + 1;
        envg[0] = state.ENV_G[idx3u(0, i, ng, dimN)];
        envg[1] = state.ENV_G[idx3u(1, i, ng, dimN)];
        envg[2] = state.ENV_G[idx3u(2, i, ng, dimN)];
        const fnv_g = f32(DOT(env, ftmp) + DOT(envg, fgam));
        state.DCP_G[idx2(i, ng, dimN)] = f32(fnv_g / f32(state.DXV[i] * state.WSTRIP[j]));
      }

      const dcfx = f32(fgam[0] / sr);
      const dcfy = f32(fgam[1] / sr);
      const dcfz = f32(fgam[2] / sr);

      cfx = f32(cfx + dcfx);
      cfy = f32(cfy + dcfy);
      cfz = f32(cfz + dcfz);

      cmx = f32(cmx + f32(f32(dcfz * r[1]) - f32(dcfy * r[2])) / cr);
      cmy = f32(cmy + f32(f32(dcfx * r[2]) - f32(dcfz * r[0])) / cr);
      cmz = f32(cmz + f32(f32(dcfy * r[0]) - f32(dcfx * r[1])) / cr);

      state.CNC[j] = f32(state.CNC[j] + f32(cr * f32(state.ENSY[j] * dcfy + state.ENSZ[j] * dcfz)));

      for (let n = 0; n < state.NUMAX; n += 1) {
        const dcfx_u = f32(fgam_u[idx2(0, n, 3)] / sr);
        const dcfy_u = f32(fgam_u[idx2(1, n, 3)] / sr);
        const dcfz_u = f32(fgam_u[idx2(2, n, 3)] / sr);

        cfx_u[n] = f32(cfx_u[n] + dcfx_u);
        cfy_u[n] = f32(cfy_u[n] + dcfy_u);
        cfz_u[n] = f32(cfz_u[n] + dcfz_u);
        cmx_u[n] = f32(cmx_u[n] + f32(f32(dcfz_u * r[1]) - f32(dcfy_u * r[2])) / cr);
        cmy_u[n] = f32(cmy_u[n] + f32(f32(dcfx_u * r[2]) - f32(dcfz_u * r[0])) / cr);
        cmz_u[n] = f32(cmz_u[n] + f32(f32(dcfy_u * r[0]) - f32(dcfx_u * r[1])) / cr);

        state.CNC_U[idx2(j, n, dimStrip)] = f32(state.CNC_U[idx2(j, n, dimStrip)]
          + f32(cr * f32(state.ENSY[j] * dcfy_u + state.ENSZ[j] * dcfz_u)));
      }

      for (let n = 0; n < state.NCONTROL; n += 1) {
        const dcfx_d = f32(fgam_d[idx2(0, n, 3)] / sr);
        const dcfy_d = f32(fgam_d[idx2(1, n, 3)] / sr);
        const dcfz_d = f32(fgam_d[idx2(2, n, 3)] / sr);

        cfx_d[n] = f32(cfx_d[n] + dcfx_d);
        cfy_d[n] = f32(cfy_d[n] + dcfy_d);
        cfz_d[n] = f32(cfz_d[n] + dcfz_d);
        cmx_d[n] = f32(cmx_d[n] + f32(f32(dcfz_d * r[1]) - f32(dcfy_d * r[2])) / cr);
        cmy_d[n] = f32(cmy_d[n] + f32(f32(dcfx_d * r[2]) - f32(dcfz_d * r[0])) / cr);
        cmz_d[n] = f32(cmz_d[n] + f32(f32(dcfy_d * r[0]) - f32(dcfx_d * r[1])) / cr);

        state.CNC_D[idx2(j, n, dimStrip)] = f32(state.CNC_D[idx2(j, n, dimStrip)]
          + f32(cr * f32(state.ENSY[j] * dcfy_d + state.ENSZ[j] * dcfz_d)));
      }

      for (let n = 0; n < state.NDESIGN; n += 1) {
        const dcfx_g = f32(fgam_g[idx2(0, n, 3)] / sr);
        const dcfy_g = f32(fgam_g[idx2(1, n, 3)] / sr);
        const dcfz_g = f32(fgam_g[idx2(2, n, 3)] / sr);

        cfx_g[n] = f32(cfx_g[n] + dcfx_g);
        cfy_g[n] = f32(cfy_g[n] + dcfy_g);
        cfz_g[n] = f32(cfz_g[n] + dcfz_g);
        cmx_g[n] = f32(cmx_g[n] + f32(f32(dcfz_g * r[1]) - f32(dcfy_g * r[2])) / cr);
        cmy_g[n] = f32(cmy_g[n] + f32(f32(dcfx_g * r[2]) - f32(dcfz_g * r[0])) / cr);
        cmz_g[n] = f32(cmz_g[n] + f32(f32(dcfy_g * r[0]) - f32(dcfx_g * r[1])) / cr);

        state.CNC_G[idx2(j, n, dimStrip)] = f32(state.CNC_G[idx2(j, n, dimStrip)]
          + f32(cr * f32(state.ENSY[j] * dcfy_g + state.ENSZ[j] * dcfz_g)));
      }

      for (let l = 0; l < state.NCONTROL; l += 1) {
        const rh = new Float32Array(3);
        rh[0] = f32(state.RV[idx3(0, i)] - state.PHINGE[idx2(0, idx2(j, l, dimStrip), 3)]);
        rh[1] = f32(state.RV[idx3(1, i)] - state.PHINGE[idx2(1, idx2(j, l, dimStrip), 3)]);
        rh[2] = f32(state.RV[idx3(2, i)] - state.PHINGE[idx2(2, idx2(j, l, dimStrip), 3)]);

        const ld = l + 1;
        const dfac = f32(state.DCONTROL[idx2(i, ld, dimN)] / f32(state.SREF * state.CREF));

        const mh = new Float32Array(3);
        CROSS(rh, fgam, mh);
        const vhinge = new Float32Array(3);
        vhinge[0] = state.VHINGE[idx2(0, idx2(j, l, dimStrip), 3)];
        vhinge[1] = state.VHINGE[idx2(1, idx2(j, l, dimStrip), 3)];
        vhinge[2] = state.VHINGE[idx2(2, idx2(j, l, dimStrip), 3)];
        state.CHINGE[l] = f32(state.CHINGE[l] + f32(DOT(mh, vhinge) * dfac));

        for (let n = 0; n < state.NUMAX; n += 1) {
          const mh_u = new Float32Array(3);
          const tmp = new Float32Array(3);
          tmp[0] = fgam_u[idx2(0, n, 3)];
          tmp[1] = fgam_u[idx2(1, n, 3)];
          tmp[2] = fgam_u[idx2(2, n, 3)];
          CROSS(rh, tmp, mh_u);
          state.CHINGE_U[idx2(l, n, state.NCONTROL)] = f32(state.CHINGE_U[idx2(l, n, state.NCONTROL)]
            + f32(DOT(mh_u, vhinge) * dfac));
        }
        for (let n = 0; n < state.NCONTROL; n += 1) {
          const mh_d = new Float32Array(3);
          const tmp = new Float32Array(3);
          tmp[0] = fgam_d[idx2(0, n, 3)];
          tmp[1] = fgam_d[idx2(1, n, 3)];
          tmp[2] = fgam_d[idx2(2, n, 3)];
          CROSS(rh, tmp, mh_d);
          state.CHINGE_D[idx2(l, n, state.NCONTROL)] = f32(state.CHINGE_D[idx2(l, n, state.NCONTROL)]
            + f32(DOT(mh_d, vhinge) * dfac));
        }
        for (let n = 0; n < state.NDESIGN; n += 1) {
          const mh_g = new Float32Array(3);
          const tmp = new Float32Array(3);
          tmp[0] = fgam_g[idx2(0, n, 3)];
          tmp[1] = fgam_g[idx2(1, n, 3)];
          tmp[2] = fgam_g[idx2(2, n, 3)];
          CROSS(rh, tmp, mh_g);
          state.CHINGE_G[idx2(l, n, state.NCONTROL)] = f32(state.CHINGE_G[idx2(l, n, state.NCONTROL)]
            + f32(DOT(mh_g, vhinge) * dfac));
        }
      }
    }

    if (state.LTRFORCE) {
      for (let ii = 0; ii < nvc; ii += 1) {
        const i = i1 + ii;
        for (let ileg = 0; ileg < 2; ileg += 1) {
          const r = new Float32Array(3);
          const rrot = new Float32Array(3);
          const g = new Float32Array(3);
          if (ileg === 0) {
            r[0] = f32(0.5 * (state.RV1[idx3(0, i)] + xte1) - rc4[0]);
            r[1] = f32(state.RV1[idx3(1, i)] - rc4[1]);
            r[2] = f32(state.RV1[idx3(2, i)] - rc4[2]);

            rrot[0] = f32(0.5 * (state.RV1[idx3(0, i)] + xte1) - state.XYZREF[0]);
            rrot[1] = f32(state.RV1[idx3(1, i)] - state.XYZREF[1]);
            rrot[2] = f32(state.RV1[idx3(2, i)] - state.XYZREF[2]);

            g[0] = f32(state.RV1[idx3(0, i)] - xte1);
            g[1] = 0.0;
            g[2] = 0.0;
          } else {
            r[0] = f32(0.5 * (state.RV2[idx3(0, i)] + xte2) - rc4[0]);
            r[1] = f32(state.RV2[idx3(1, i)] - rc4[1]);
            r[2] = f32(state.RV2[idx3(2, i)] - rc4[2]);

            rrot[0] = f32(0.5 * (state.RV2[idx3(0, i)] + xte2) - state.XYZREF[0]);
            rrot[1] = f32(state.RV2[idx3(1, i)] - state.XYZREF[1]);
            rrot[2] = f32(state.RV2[idx3(2, i)] - state.XYZREF[2]);

            g[0] = f32(xte2 - state.RV2[idx3(0, i)]);
            g[1] = 0.0;
            g[2] = 0.0;
          }

          const vrot = new Float32Array(3);
          CROSS(rrot, state.WROT, vrot);
          const veff = new Float32Array(3);
          veff[0] = f32(state.VINF[0] + vrot[0]);
          veff[1] = f32(state.VINF[1] + vrot[1]);
          veff[2] = f32(state.VINF[2] + vrot[2]);

          const veff_u = new Float32Array(3 * state.NUMAX);
          for (let k = 0; k < 3; k += 1) {
            veff_u[idx2(0, k, 3)] = 0.0;
            veff_u[idx2(1, k, 3)] = 0.0;
            veff_u[idx2(2, k, 3)] = 0.0;
            veff_u[idx2(k, k, 3)] = 1.0;
          }
          for (let k = 3; k < 6; k += 1) {
            const wrot_u = new Float32Array(3);
            wrot_u[0] = 0.0; wrot_u[1] = 0.0; wrot_u[2] = 0.0;
            wrot_u[k - 3] = 1.0;
            const vrot_u = new Float32Array(3);
            CROSS(rrot, wrot_u, vrot_u);
            veff_u[idx2(0, k, 3)] = vrot_u[0];
            veff_u[idx2(1, k, 3)] = vrot_u[1];
            veff_u[idx2(2, k, 3)] = vrot_u[2];
          }

          const f = new Float32Array(3);
          CROSS(veff, g, f);
          const f_u = new Float32Array(3 * state.NUMAX);
          for (let n = 0; n < state.NUMAX; n += 1) {
            const tmp = new Float32Array(3);
            tmp[0] = veff_u[idx2(0, n, 3)];
            tmp[1] = veff_u[idx2(1, n, 3)];
            tmp[2] = veff_u[idx2(2, n, 3)];
            const out = new Float32Array(3);
            CROSS(tmp, g, out);
            f_u[idx2(0, n, 3)] = out[0];
            f_u[idx2(1, n, 3)] = out[1];
            f_u[idx2(2, n, 3)] = out[2];
          }

          const fgam = new Float32Array(3);
          fgam[0] = f32(2.0 * state.GAM[i] * f[0]);
          fgam[1] = f32(2.0 * state.GAM[i] * f[1]);
          fgam[2] = f32(2.0 * state.GAM[i] * f[2]);

          const fgam_u = new Float32Array(3 * state.NUMAX);
          for (let n = 0; n < state.NUMAX; n += 1) {
            const nu = n + 1;
            fgam_u[idx2(0, n, 3)] = f32(2.0 * state.GAM_U[idx2(i, nu, dimN)] * f[0]
              + 2.0 * state.GAM[i] * f_u[idx2(0, n, 3)]);
            fgam_u[idx2(1, n, 3)] = f32(2.0 * state.GAM_U[idx2(i, nu, dimN)] * f[1]
              + 2.0 * state.GAM[i] * f_u[idx2(1, n, 3)]);
            fgam_u[idx2(2, n, 3)] = f32(2.0 * state.GAM_U[idx2(i, nu, dimN)] * f[2]
              + 2.0 * state.GAM[i] * f_u[idx2(2, n, 3)]);
          }

          const dcfx = f32(fgam[0] / sr);
          const dcfy = f32(fgam[1] / sr);
          const dcfz = f32(fgam[2] / sr);

          cfx = f32(cfx + dcfx);
          cfy = f32(cfy + dcfy);
          cfz = f32(cfz + dcfz);

          cmx = f32(cmx + f32(f32(dcfz * r[1]) - f32(dcfy * r[2])) / cr);
          cmy = f32(cmy + f32(f32(dcfx * r[2]) - f32(dcfz * r[0])) / cr);
          cmz = f32(cmz + f32(f32(dcfy * r[0]) - f32(dcfx * r[1])) / cr);

          state.CNC[j] = f32(state.CNC[j] + f32(cr * f32(state.ENSY[j] * dcfy + state.ENSZ[j] * dcfz)));

          for (let n = 0; n < state.NUMAX; n += 1) {
            const dcfx_u = f32(fgam_u[idx2(0, n, 3)] / sr);
            const dcfy_u = f32(fgam_u[idx2(1, n, 3)] / sr);
            const dcfz_u = f32(fgam_u[idx2(2, n, 3)] / sr);

            cfx_u[n] = f32(cfx_u[n] + dcfx_u);
            cfy_u[n] = f32(cfy_u[n] + dcfy_u);
            cfz_u[n] = f32(cfz_u[n] + dcfz_u);
            cmx_u[n] = f32(cmx_u[n] + f32(f32(dcfz_u * r[1]) - f32(dcfy_u * r[2])) / cr);
            cmy_u[n] = f32(cmy_u[n] + f32(f32(dcfx_u * r[2]) - f32(dcfz_u * r[0])) / cr);
            cmz_u[n] = f32(cmz_u[n] + f32(f32(dcfy_u * r[0]) - f32(dcfx_u * r[1])) / cr);

            state.CNC_U[idx2(j, n, dimStrip)] = f32(state.CNC_U[idx2(j, n, dimStrip)]
              + f32(cr * f32(state.ENSY[j] * dcfy_u + state.ENSZ[j] * dcfz_u)));
          }
        }
      }
    }

    state.CDV_LSTRP[j] = 0.0;

    if (state.LVISC && state.LVISCSTRP[j]) {
      const rrot = new Float32Array(3);
      rrot[0] = f32(rc4[0] - state.XYZREF[0]);
      rrot[1] = f32(rc4[1] - state.XYZREF[1]);
      rrot[2] = f32(rc4[2] - state.XYZREF[2]);

      const vrot = new Float32Array(3);
      CROSS(rrot, state.WROT, vrot);
      const veff = new Float32Array(3);
      veff[0] = f32(state.VINF[0] + vrot[0]);
      veff[1] = f32(state.VINF[1] + vrot[1]);
      veff[2] = f32(state.VINF[2] + vrot[2]);
      const veffmag = f32(Math.sqrt(f32(f32(veff[0] * veff[0]) + f32(veff[1] * veff[1]) + f32(veff[2] * veff[2]))));

      const veff_u = new Float32Array(3 * state.NUMAX);
      for (let k = 0; k < 3; k += 1) {
        veff_u[idx2(0, k, 3)] = 0.0;
        veff_u[idx2(1, k, 3)] = 0.0;
        veff_u[idx2(2, k, 3)] = 0.0;
      }
      veff_u[idx2(0, 0, 3)] = 1.0;
      veff_u[idx2(1, 1, 3)] = 1.0;
      veff_u[idx2(2, 2, 3)] = 1.0;
      for (let k = 3; k < 6; k += 1) {
        const wrot_u = new Float32Array(3);
        wrot_u[0] = 0.0; wrot_u[1] = 0.0; wrot_u[2] = 0.0;
        wrot_u[k - 3] = 1.0;
        const vrot_u = new Float32Array(3);
        CROSS(rrot, wrot_u, vrot_u);
        veff_u[idx2(0, k, 3)] = vrot_u[0];
        veff_u[idx2(1, k, 3)] = vrot_u[1];
        veff_u[idx2(2, k, 3)] = vrot_u[2];
      }

      const veffmag_u = new Float32Array(state.NUMAX);
      for (let n = 0; n < state.NUMAX; n += 1) {
        veffmag_u[n] = f32((veff[0] * veff_u[idx2(0, n, 3)]
          + veff[1] * veff_u[idx2(1, n, 3)]
          + veff[2] * veff_u[idx2(2, n, 3)]) / veffmag);
      }

      const clv = f32(ulift[0] * cfx + ulift[1] * cfy + ulift[2] * cfz);
      const clv_u = new Float32Array(state.NUMAX);
      const clv_d = new Float32Array(state.NCONTROL);
      const clv_g = new Float32Array(state.NDESIGN);
      for (let n = 0; n < state.NUMAX; n += 1) {
        clv_u[n] = f32(ulift[0] * cfx_u[n] + ulift_u[idx2(0, n, 3)] * cfx
          + ulift[1] * cfy_u[n] + ulift_u[idx2(1, n, 3)] * cfy
          + ulift[2] * cfz_u[n] + ulift_u[idx2(2, n, 3)] * cfz);
      }
      for (let n = 0; n < state.NCONTROL; n += 1) {
        clv_d[n] = f32(ulift[0] * cfx_d[n] + ulift_d[idx2(0, n, 3)] * cfx
          + ulift[1] * cfy_d[n] + ulift_d[idx2(1, n, 3)] * cfy
          + ulift[2] * cfz_d[n] + ulift_d[idx2(2, n, 3)] * cfz);
      }
      for (let n = 0; n < state.NDESIGN; n += 1) {
        clv_g[n] = f32(ulift[0] * cfx_g[n] + ulift_g[idx2(0, n, 3)] * cfx
          + ulift[1] * cfy_g[n] + ulift_g[idx2(1, n, 3)] * cfy
          + ulift[2] * cfz_g[n] + ulift_g[idx2(2, n, 3)] * cfz);
      }

      const cdcl = CDCL(state.CLCD.subarray(j * 6, j * 6 + 6), clv);
      const cdv = cdcl.cd;
      const cdv_clv = cdcl.cd_cl;

      let dcvfx = f32(veff[0] * veffmag * cdv);
      let dcvfy = f32(veff[1] * veffmag * cdv);
      let dcvfz = f32(veff[2] * veffmag * cdv);

      cfx = f32(cfx + dcvfx);
      cfy = f32(cfy + dcvfy);
      cfz = f32(cfz + dcvfz);

      state.CDV_LSTRP[j] = f32(udrag[0] * dcvfx + udrag[1] * dcvfy + udrag[2] * dcvfz);

      for (let n = 0; n < state.NUMAX; n += 1) {
        const dcvfx_u = f32((veff_u[idx2(0, n, 3)] * veffmag + veff[0] * veffmag_u[n]) * cdv
          + veff[0] * veffmag * cdv_clv * clv_u[n]);
        const dcvfy_u = f32((veff_u[idx2(1, n, 3)] * veffmag + veff[1] * veffmag_u[n]) * cdv
          + veff[1] * veffmag * cdv_clv * clv_u[n]);
        const dcvfz_u = f32((veff_u[idx2(2, n, 3)] * veffmag + veff[2] * veffmag_u[n]) * cdv
          + veff[2] * veffmag * cdv_clv * clv_u[n]);

        cfx_u[n] = f32(cfx_u[n] + dcvfx_u);
        cfy_u[n] = f32(cfy_u[n] + dcvfy_u);
        cfz_u[n] = f32(cfz_u[n] + dcvfz_u);
        state.CNC_U[idx2(j, n, dimStrip)] = f32(state.CNC_U[idx2(j, n, dimStrip)]
          + f32(cr * f32(state.ENSY[j] * dcvfy_u + state.ENSZ[j] * dcvfz_u)));
      }

      for (let n = 0; n < state.NCONTROL; n += 1) {
        const dcvfx_d = f32(veff[0] * veffmag * cdv_clv * clv_d[n]);
        const dcvfy_d = f32(veff[1] * veffmag * cdv_clv * clv_d[n]);
        const dcvfz_d = f32(veff[2] * veffmag * cdv_clv * clv_d[n]);

        cfx_d[n] = f32(cfx_d[n] + dcvfx_d);
        cfy_d[n] = f32(cfy_d[n] + dcvfy_d);
        cfz_d[n] = f32(cfz_d[n] + dcvfz_d);
        state.CNC_D[idx2(j, n, dimStrip)] = f32(state.CNC_D[idx2(j, n, dimStrip)]
          + f32(cr * f32(state.ENSY[j] * dcvfy_d + state.ENSZ[j] * dcvfz_d)));
      }

      for (let n = 0; n < state.NDESIGN; n += 1) {
        const dcvfx_g = f32(veff[0] * veffmag * cdv_clv * clv_g[n]);
        const dcvfy_g = f32(veff[1] * veffmag * cdv_clv * clv_g[n]);
        const dcvfz_g = f32(veff[2] * veffmag * cdv_clv * clv_g[n]);

        cfx_g[n] = f32(cfx_g[n] + dcvfx_g);
        cfy_g[n] = f32(cfy_g[n] + dcvfy_g);
        cfz_g[n] = f32(cfz_g[n] + dcvfz_g);
        state.CNC_G[idx2(j, n, dimStrip)] = f32(state.CNC_G[idx2(j, n, dimStrip)]
          + f32(cr * f32(state.ENSY[j] * dcvfy_g + state.ENSZ[j] * dcvfz_g)));
      }
    }

    state.CF_LSTRP[idx2(0, j, 3)] = cfx;
    state.CF_LSTRP[idx2(1, j, 3)] = cfy;
    state.CF_LSTRP[idx2(2, j, 3)] = cfz;
    state.CM_LSTRP[idx2(0, j, 3)] = cmx;
    state.CM_LSTRP[idx2(1, j, 3)] = cmy;
    state.CM_LSTRP[idx2(2, j, 3)] = cmz;

    state.CFSTRP[idx2(0, j, 3)] = cfx;
    state.CFSTRP[idx2(1, j, 3)] = cfy;
    state.CFSTRP[idx2(2, j, 3)] = cfz;

    state.CDSTRP[j] = f32(cfx * cosa + cfz * sina);
    state.CYSTRP[j] = cfy;
    state.CLSTRP[j] = f32(-cfx * sina + cfz * cosa);

    state.CDST_A[j] = f32(-cfx * sina + cfz * cosa);
    state.CYST_A[j] = 0.0;
    state.CLST_A[j] = f32(-cfx * cosa - cfz * sina);

    for (let n = 0; n < state.NUMAX; n += 1) {
      state.CDST_U[idx2(j, n, dimStrip)] = f32(cfx_u[n] * cosa + cfz_u[n] * sina);
      state.CYST_U[idx2(j, n, dimStrip)] = cfy_u[n];
      state.CLST_U[idx2(j, n, dimStrip)] = f32(-cfx_u[n] * sina + cfz_u[n] * cosa);
      state.CFST_U[idx2(0, idx2(j, n, dimStrip), 3)] = cfx_u[n];
      state.CFST_U[idx2(1, idx2(j, n, dimStrip), 3)] = cfy_u[n];
      state.CFST_U[idx2(2, idx2(j, n, dimStrip), 3)] = cfz_u[n];
    }

    for (let n = 0; n < state.NCONTROL; n += 1) {
      state.CDST_D[idx2(j, n, dimStrip)] = f32(cfx_d[n] * cosa + cfz_d[n] * sina);
      state.CYST_D[idx2(j, n, dimStrip)] = cfy_d[n];
      state.CLST_D[idx2(j, n, dimStrip)] = f32(-cfx_d[n] * sina + cfz_d[n] * cosa);
      state.CFST_D[idx2(0, idx2(j, n, dimStrip), 3)] = cfx_d[n];
      state.CFST_D[idx2(1, idx2(j, n, dimStrip), 3)] = cfy_d[n];
      state.CFST_D[idx2(2, idx2(j, n, dimStrip), 3)] = cfz_d[n];
    }

    for (let n = 0; n < state.NDESIGN; n += 1) {
      state.CDST_G[idx2(j, n, dimStrip)] = f32(cfx_g[n] * cosa + cfz_g[n] * sina);
      state.CYST_G[idx2(j, n, dimStrip)] = cfy_g[n];
      state.CLST_G[idx2(j, n, dimStrip)] = f32(-cfx_g[n] * sina + cfz_g[n] * cosa);
      state.CFST_G[idx2(0, idx2(j, n, dimStrip), 3)] = cfx_g[n];
      state.CFST_G[idx2(1, idx2(j, n, dimStrip), 3)] = cfy_g[n];
      state.CFST_G[idx2(2, idx2(j, n, dimStrip), 3)] = cfz_g[n];
    }

    const r = new Float32Array(3);
    r[0] = f32(rc4[0] - state.XYZREF[0]);
    r[1] = f32(rc4[1] - state.XYZREF[1]);
    r[2] = f32(rc4[2] - state.XYZREF[2]);

    state.CMSTRP[idx2(0, j, 3)] = f32(cmx + f32(f32(cfz * r[1]) - f32(cfy * r[2])) / cr);
    state.CMSTRP[idx2(1, j, 3)] = f32(cmy + f32(f32(cfx * r[2]) - f32(cfz * r[0])) / cr);
    state.CMSTRP[idx2(2, j, 3)] = f32(cmz + f32(f32(cfy * r[0]) - f32(cfx * r[1])) / cr);

    for (let n = 0; n < state.NUMAX; n += 1) {
      state.CMST_U[idx2(0, idx2(j, n, dimStrip), 3)] = f32(cmx_u[n] + f32(f32(cfz_u[n] * r[1]) - f32(cfy_u[n] * r[2])) / cr);
      state.CMST_U[idx2(1, idx2(j, n, dimStrip), 3)] = f32(cmy_u[n] + f32(f32(cfx_u[n] * r[2]) - f32(cfz_u[n] * r[0])) / cr);
      state.CMST_U[idx2(2, idx2(j, n, dimStrip), 3)] = f32(cmz_u[n] + f32(f32(cfy_u[n] * r[0]) - f32(cfx_u[n] * r[1])) / cr);
    }

    for (let n = 0; n < state.NCONTROL; n += 1) {
      state.CMST_D[idx2(0, idx2(j, n, dimStrip), 3)] = f32(cmx_d[n] + f32(f32(cfz_d[n] * r[1]) - f32(cfy_d[n] * r[2])) / cr);
      state.CMST_D[idx2(1, idx2(j, n, dimStrip), 3)] = f32(cmy_d[n] + f32(f32(cfx_d[n] * r[2]) - f32(cfz_d[n] * r[0])) / cr);
      state.CMST_D[idx2(2, idx2(j, n, dimStrip), 3)] = f32(cmz_d[n] + f32(f32(cfy_d[n] * r[0]) - f32(cfx_d[n] * r[1])) / cr);
    }

    for (let n = 0; n < state.NDESIGN; n += 1) {
      state.CMST_G[idx2(0, idx2(j, n, dimStrip), 3)] = f32(cmx_g[n] + f32(f32(cfz_g[n] * r[1]) - f32(cfy_g[n] * r[2])) / cr);
      state.CMST_G[idx2(1, idx2(j, n, dimStrip), 3)] = f32(cmy_g[n] + f32(f32(cfx_g[n] * r[2]) - f32(cfz_g[n] * r[0])) / cr);
      state.CMST_G[idx2(2, idx2(j, n, dimStrip), 3)] = f32(cmz_g[n] + f32(f32(cfy_g[n] * r[0]) - f32(cfx_g[n] * r[1])) / cr);
    }

    state.CL_LSTRP[j] = f32(ulift[0] * cfx + ulift[1] * cfy + ulift[2] * cfz);
    state.CD_LSTRP[j] = f32(udrag[0] * cfx + udrag[1] * cfy + udrag[2] * cfz);
    state.CMC4_LSTRP[j] = f32(state.ENSZ[j] * cmy - state.ENSY[j] * cmz);

    const caxl0 = cfx;
    const cnrm0 = f32(state.ENSY[j] * cfy + state.ENSZ[j] * cfz);
    const sinainc = f32(Math.sin(f32(state.AINC[j])));
    const cosainc = f32(Math.cos(f32(state.AINC[j])));
    state.CA_LSTRP[j] = f32(caxl0 * cosainc - cnrm0 * sinainc);
    state.CN_LSTRP[j] = f32(cnrm0 * cosainc + caxl0 * sinainc);

    const rrot = new Float32Array(3);
    rrot[0] = f32(state.XSREF[j] - state.XYZREF[0]);
    rrot[1] = f32(state.YSREF[j] - state.XYZREF[1]);
    rrot[2] = f32(state.ZSREF[j] - state.XYZREF[2]);

    const vrot = new Float32Array(3);
    CROSS(rrot, state.WROT, vrot);
    const veff = new Float32Array(3);
    veff[0] = f32(state.VINF[0] + vrot[0]);
    veff[1] = f32(state.VINF[1] + vrot[1]);
    veff[2] = f32(state.VINF[2] + vrot[2]);

    const vsq = f32(f32(veff[0] * veff[0]) + f32(veff[1] * veff[1]) + f32(veff[2] * veff[2]));
    const vsqi = vsq === 0.0 ? 1.0 : f32(1.0 / vsq);

    const vspan = f32(veff[0] * state.ESS[idx3(0, j)]
      + veff[1] * state.ESS[idx3(1, j)]
      + veff[2] * state.ESS[idx3(2, j)]);

    const vperp = new Float32Array(3);
    vperp[0] = f32(veff[0] - state.ESS[idx3(0, j)] * vspan);
    vperp[1] = f32(veff[1] - state.ESS[idx3(1, j)] * vspan);
    vperp[2] = f32(veff[2] - state.ESS[idx3(2, j)] * vspan);

    const vpsq = f32(f32(vperp[0] * vperp[0]) + f32(vperp[1] * vperp[1]) + f32(vperp[2] * vperp[2]));
    const vpsqi = vpsq === 0.0 ? 1.0 : f32(1.0 / vpsq);

    state.CLT_LSTRP[j] = f32(state.CL_LSTRP[j] * vpsqi);
    state.CLA_LSTRP[j] = f32(state.CL_LSTRP[j] * vsqi);

    const rle = new Float32Array(3);
    rle[0] = f32(rc4[0] - state.RLE[idx3(0, j)]);
    rle[1] = f32(rc4[1] - state.RLE[idx3(1, j)]);
    rle[2] = f32(rc4[2] - state.RLE[idx3(2, j)]);

    let delx = f32(state.RLE2[idx3(0, j)] - state.RLE1[idx3(0, j)]);
    let dely = f32(state.RLE2[idx3(1, j)] - state.RLE1[idx3(1, j)]);
    let delz = f32(state.RLE2[idx3(2, j)] - state.RLE1[idx3(2, j)]);

    if (state.IMAGS[state.LSSURF[j]] < 0) {
      delx = f32(-delx); dely = f32(-dely); delz = f32(-delz);
    }
    const dmag = f32(Math.sqrt(f32(f32(delx * delx) + f32(dely * dely) + f32(delz * delz))));
    state.CMLE_LSTRP[j] = 0.0;
    if (dmag !== 0.0) {
      state.CMLE_LSTRP[j] = f32(delx / dmag * f32(cmx + f32(f32(cfz * rle[1]) - f32(cfy * rle[2])) / cr)
        + dely / dmag * f32(cmy + f32(f32(cfx * rle[2]) - f32(cfz * rle[0])) / cr)
        + delz / dmag * f32(cmz + f32(f32(cfy * rle[0]) - f32(cfx * rle[1])) / cr));
    }
  }

  for (let is = 1; is <= state.NSURF; is += 1) {
    state.CDSURF[is] = 0.0;
    state.CYSURF[is] = 0.0;
    state.CLSURF[is] = 0.0;
    for (let l = 0; l < 3; l += 1) {
      state.CFSURF[idx2(l, is, 3)] = 0.0;
      state.CMSURF[idx2(l, is, 3)] = 0.0;
    }
    state.CDVSURF[is] = 0.0;

    state.CDS_A[is] = 0.0;
    state.CYS_A[is] = 0.0;
    state.CLS_A[is] = 0.0;
    for (let n = 0; n < state.NUMAX; n += 1) {
      state.CDS_U[idx2(is, n, dimSurf)] = 0.0;
      state.CYS_U[idx2(is, n, dimSurf)] = 0.0;
      state.CLS_U[idx2(is, n, dimSurf)] = 0.0;
      for (let l = 0; l < 3; l += 1) {
        state.CFS_U[idx2(l, idx2(is, n, dimSurf), 3)] = 0.0;
        state.CMS_U[idx2(l, idx2(is, n, dimSurf), 3)] = 0.0;
      }
    }
    for (let n = 0; n < state.NCONTROL; n += 1) {
      state.CDS_D[idx2(is, n, dimSurf)] = 0.0;
      state.CYS_D[idx2(is, n, dimSurf)] = 0.0;
      state.CLS_D[idx2(is, n, dimSurf)] = 0.0;
      for (let l = 0; l < 3; l += 1) {
        state.CFS_D[idx2(l, idx2(is, n, dimSurf), 3)] = 0.0;
        state.CMS_D[idx2(l, idx2(is, n, dimSurf), 3)] = 0.0;
      }
    }
    for (let n = 0; n < state.NDESIGN; n += 1) {
      state.CDS_G[idx2(is, n, dimSurf)] = 0.0;
      state.CYS_G[idx2(is, n, dimSurf)] = 0.0;
      state.CLS_G[idx2(is, n, dimSurf)] = 0.0;
      for (let l = 0; l < 3; l += 1) {
        state.CFS_G[idx2(l, idx2(is, n, dimSurf), 3)] = 0.0;
        state.CMS_G[idx2(l, idx2(is, n, dimSurf), 3)] = 0.0;
      }
    }

    const enave = new Float32Array(3);
    for (let l = 0; l < 3; l += 1) {
      state.CF_LSRF[idx2(l, is, 3)] = 0.0;
      state.CM_LSRF[idx2(l, is, 3)] = 0.0;
      enave[l] = 0.0;
    }

    const nstrps = state.NJ[is];
    for (let jj = 0; jj < nstrps; jj += 1) {
      const j = state.JFRST[is] + jj;
      const sr = f32(state.CHORD[j] * state.WSTRIP[j]);
      const cr = f32(state.CHORD[j]);
      const rc4 = new Float32Array(3);
      rc4[0] = f32(state.RLE[idx3(0, j)] + f32(0.25 * state.CHORD[j]));
      rc4[1] = state.RLE[idx3(1, j)];
      rc4[2] = state.RLE[idx3(2, j)];

      enave[0] = 0.0;
      enave[1] = f32(enave[1] + sr * state.ENSY[j]);
      enave[2] = f32(enave[2] + sr * state.ENSZ[j]);

      state.CDSURF[is] = f32(state.CDSURF[is] + f32(state.CDSTRP[j] * sr / state.SREF));
      state.CYSURF[is] = f32(state.CYSURF[is] + f32(state.CYSTRP[j] * sr / state.SREF));
      state.CLSURF[is] = f32(state.CLSURF[is] + f32(state.CLSTRP[j] * sr / state.SREF));

      state.CFSURF[idx2(0, is, 3)] = f32(state.CFSURF[idx2(0, is, 3)] + f32(state.CFSTRP[idx2(0, j, 3)] * sr / state.SREF));
      state.CFSURF[idx2(1, is, 3)] = f32(state.CFSURF[idx2(1, is, 3)] + f32(state.CFSTRP[idx2(1, j, 3)] * sr / state.SREF));
      state.CFSURF[idx2(2, is, 3)] = f32(state.CFSURF[idx2(2, is, 3)] + f32(state.CFSTRP[idx2(2, j, 3)] * sr / state.SREF));

      state.CMSURF[idx2(0, is, 3)] = f32(state.CMSURF[idx2(0, is, 3)]
        + f32(state.CMSTRP[idx2(0, j, 3)] * (sr / state.SREF) * (cr / state.BREF)));
      state.CMSURF[idx2(1, is, 3)] = f32(state.CMSURF[idx2(1, is, 3)]
        + f32(state.CMSTRP[idx2(1, j, 3)] * (sr / state.SREF) * (cr / state.CREF)));
      state.CMSURF[idx2(2, is, 3)] = f32(state.CMSURF[idx2(2, is, 3)]
        + f32(state.CMSTRP[idx2(2, j, 3)] * (sr / state.SREF) * (cr / state.BREF)));

      state.CDVSURF[is] = f32(state.CDVSURF[is] + f32(state.CDV_LSTRP[j] * (sr / state.SREF)));

      state.CDS_A[is] = f32(state.CDS_A[is] + f32(state.CDST_A[j] * sr / state.SREF));
      state.CYS_A[is] = f32(state.CYS_A[is] + f32(state.CYST_A[j] * sr / state.SREF));
      state.CLS_A[is] = f32(state.CLS_A[is] + f32(state.CLST_A[j] * sr / state.SREF));

      for (let n = 0; n < state.NUMAX; n += 1) {
        state.CDS_U[idx2(is, n, dimSurf)] = f32(state.CDS_U[idx2(is, n, dimSurf)] + f32(state.CDST_U[idx2(j, n, dimStrip)] * sr / state.SREF));
        state.CYS_U[idx2(is, n, dimSurf)] = f32(state.CYS_U[idx2(is, n, dimSurf)] + f32(state.CYST_U[idx2(j, n, dimStrip)] * sr / state.SREF));
        state.CLS_U[idx2(is, n, dimSurf)] = f32(state.CLS_U[idx2(is, n, dimSurf)] + f32(state.CLST_U[idx2(j, n, dimStrip)] * sr / state.SREF));

        for (let l = 0; l < 3; l += 1) {
          state.CFS_U[idx2(l, idx2(is, n, dimSurf), 3)] = f32(state.CFS_U[idx2(l, idx2(is, n, dimSurf), 3)] + f32(state.CFST_U[idx2(l, idx2(j, n, dimStrip), 3)] * sr / state.SREF));
        }
        state.CMS_U[idx2(0, idx2(is, n, dimSurf), 3)] = f32(state.CMS_U[idx2(0, idx2(is, n, dimSurf), 3)]
          + f32(state.CMST_U[idx2(0, idx2(j, n, dimStrip), 3)] * (sr / state.SREF) * (cr / state.BREF)));
        state.CMS_U[idx2(1, idx2(is, n, dimSurf), 3)] = f32(state.CMS_U[idx2(1, idx2(is, n, dimSurf), 3)]
          + f32(state.CMST_U[idx2(1, idx2(j, n, dimStrip), 3)] * (sr / state.SREF) * (cr / state.CREF)));
        state.CMS_U[idx2(2, idx2(is, n, dimSurf), 3)] = f32(state.CMS_U[idx2(2, idx2(is, n, dimSurf), 3)]
          + f32(state.CMST_U[idx2(2, idx2(j, n, dimStrip), 3)] * (sr / state.SREF) * (cr / state.BREF)));
      }

      for (let n = 0; n < state.NCONTROL; n += 1) {
        state.CDS_D[idx2(is, n, dimSurf)] = f32(state.CDS_D[idx2(is, n, dimSurf)] + f32(state.CDST_D[idx2(j, n, dimStrip)] * sr / state.SREF));
        state.CYS_D[idx2(is, n, dimSurf)] = f32(state.CYS_D[idx2(is, n, dimSurf)] + f32(state.CYST_D[idx2(j, n, dimStrip)] * sr / state.SREF));
        state.CLS_D[idx2(is, n, dimSurf)] = f32(state.CLS_D[idx2(is, n, dimSurf)] + f32(state.CLST_D[idx2(j, n, dimStrip)] * sr / state.SREF));

        for (let l = 0; l < 3; l += 1) {
          state.CFS_D[idx2(l, idx2(is, n, dimSurf), 3)] = f32(state.CFS_D[idx2(l, idx2(is, n, dimSurf), 3)] + f32(state.CFST_D[idx2(l, idx2(j, n, dimStrip), 3)] * sr / state.SREF));
        }
        state.CMS_D[idx2(0, idx2(is, n, dimSurf), 3)] = f32(state.CMS_D[idx2(0, idx2(is, n, dimSurf), 3)]
          + f32(state.CMST_D[idx2(0, idx2(j, n, dimStrip), 3)] * (sr / state.SREF) * (cr / state.BREF)));
        state.CMS_D[idx2(1, idx2(is, n, dimSurf), 3)] = f32(state.CMS_D[idx2(1, idx2(is, n, dimSurf), 3)]
          + f32(state.CMST_D[idx2(1, idx2(j, n, dimStrip), 3)] * (sr / state.SREF) * (cr / state.CREF)));
        state.CMS_D[idx2(2, idx2(is, n, dimSurf), 3)] = f32(state.CMS_D[idx2(2, idx2(is, n, dimSurf), 3)]
          + f32(state.CMST_D[idx2(2, idx2(j, n, dimStrip), 3)] * (sr / state.SREF) * (cr / state.BREF)));
      }

      for (let n = 0; n < state.NDESIGN; n += 1) {
        state.CDS_G[idx2(is, n, dimSurf)] = f32(state.CDS_G[idx2(is, n, dimSurf)] + f32(state.CDST_G[idx2(j, n, dimStrip)] * sr / state.SREF));
        state.CYS_G[idx2(is, n, dimSurf)] = f32(state.CYS_G[idx2(is, n, dimSurf)] + f32(state.CYST_G[idx2(j, n, dimStrip)] * sr / state.SREF));
        state.CLS_G[idx2(is, n, dimSurf)] = f32(state.CLS_G[idx2(is, n, dimSurf)] + f32(state.CLST_G[idx2(j, n, dimStrip)] * sr / state.SREF));

        for (let l = 0; l < 3; l += 1) {
          state.CFS_G[idx2(l, idx2(is, n, dimSurf), 3)] = f32(state.CFS_G[idx2(l, idx2(is, n, dimSurf), 3)] + f32(state.CFST_G[idx2(l, idx2(j, n, dimStrip), 3)] * sr / state.SREF));
        }
        state.CMS_G[idx2(0, idx2(is, n, dimSurf), 3)] = f32(state.CMS_G[idx2(0, idx2(is, n, dimSurf), 3)]
          + f32(state.CMST_G[idx2(0, idx2(j, n, dimStrip), 3)] * (sr / state.SREF) * (cr / state.BREF)));
        state.CMS_G[idx2(1, idx2(is, n, dimSurf), 3)] = f32(state.CMS_G[idx2(1, idx2(is, n, dimSurf), 3)]
          + f32(state.CMST_G[idx2(1, idx2(j, n, dimStrip), 3)] * (sr / state.SREF) * (cr / state.CREF)));
        state.CMS_G[idx2(2, idx2(is, n, dimSurf), 3)] = f32(state.CMS_G[idx2(2, idx2(is, n, dimSurf), 3)]
          + f32(state.CMST_G[idx2(2, idx2(j, n, dimStrip), 3)] * (sr / state.SREF) * (cr / state.BREF)));
      }

      const r = new Float32Array(3);
      if (state.IMAGS[is] >= 0) {
        r[0] = f32(rc4[0] - state.RLE1[idx3(0, state.JFRST[is])]);
        r[1] = f32(rc4[1] - state.RLE1[idx3(1, state.JFRST[is])]);
        r[2] = f32(rc4[2] - state.RLE1[idx3(2, state.JFRST[is])]);
      } else {
        r[0] = f32(rc4[0] - state.RLE2[idx3(0, state.JFRST[is])]);
        r[1] = f32(rc4[1] - state.RLE2[idx3(1, state.JFRST[is])]);
        r[2] = f32(rc4[2] - state.RLE2[idx3(2, state.JFRST[is])]);
      }

      for (let k = 0; k < 3; k += 1) {
        const ic = ICRS[k];
        const jc = JCRS[k];

        state.CF_LSRF[idx2(k, is, 3)] = f32(state.CF_LSRF[idx2(k, is, 3)]
          + f32(state.CF_LSTRP[idx2(k, j, 3)] * sr / state.SSURF[is]));

        const dcm = f32((sr / state.SSURF[is]) * (cr / state.CAVESURF[is])
          * (state.CM_LSTRP[idx2(k, j, 3)]
            + state.CF_LSTRP[idx2(jc, j, 3)] * r[ic]
            - state.CF_LSTRP[idx2(ic, j, 3)] * r[jc]) / cr);

        state.CM_LSRF[idx2(k, is, 3)] = f32(state.CM_LSRF[idx2(k, is, 3)] + dcm);
      }
    }

    enave[0] = f32(enave[0] / state.SSURF[is]);
    enave[1] = f32(enave[1] / state.SSURF[is]);
    enave[2] = f32(enave[2] / state.SSURF[is]);
    let enmag = f32(Math.sqrt(f32(DOT(enave, enave))));
    if (enmag === 0.0) {
      enave[2] = 1.0;
    } else {
      enave[0] = f32(enave[0] / enmag);
      enave[1] = f32(enave[1] / enmag);
      enave[2] = f32(enave[2] / enmag);
    }

    const spn = new Float32Array(3);
    spn[0] = 0.0;
    spn[1] = enave[2];
    spn[2] = f32(-enave[1]);

    const udrag = new Float32Array(3);
    udrag[0] = state.VINF[0];
    udrag[1] = state.VINF[1];
    udrag[2] = state.VINF[2];

    const ulift = new Float32Array(3);
    CROSS(udrag, spn, ulift);
    let ulmag = f32(Math.sqrt(f32(DOT(ulift, ulift))));
    if (ulmag === 0.0) {
      ulift[2] = 1.0;
    } else {
      ulift[0] = f32(ulift[0] / ulmag);
      ulift[1] = f32(ulift[1] / ulmag);
      ulift[2] = f32(ulift[2] / ulmag);
    }
    state.CL_LSRF[is] = f32(DOT(ulift, state.CF_LSRF.subarray(is * 3, is * 3 + 3)));
    state.CD_LSRF[is] = f32(DOT(udrag, state.CF_LSRF.subarray(is * 3, is * 3 + 3)));

    if (state.LFLOAD[is]) {
      state.CFTOT[0] = f32(state.CFTOT[0] + state.CFSURF[idx2(0, is, 3)]);
      state.CFTOT[1] = f32(state.CFTOT[1] + state.CFSURF[idx2(1, is, 3)]);
      state.CFTOT[2] = f32(state.CFTOT[2] + state.CFSURF[idx2(2, is, 3)]);
      state.CDTOT = f32(state.CDTOT + state.CDSURF[is]);
      state.CYTOT = f32(state.CYTOT + state.CYSURF[is]);
      state.CLTOT = f32(state.CLTOT + state.CLSURF[is]);
      state.CDVTOT = f32(state.CDVTOT + state.CDVSURF[is]);

      state.CMTOT[0] = f32(state.CMTOT[0] + state.CMSURF[idx2(0, is, 3)]);
      state.CMTOT[1] = f32(state.CMTOT[1] + state.CMSURF[idx2(1, is, 3)]);
      state.CMTOT[2] = f32(state.CMTOT[2] + state.CMSURF[idx2(2, is, 3)]);

      state.CDTOT_A = f32(state.CDTOT_A + state.CDS_A[is]);
      state.CYTOT_A = f32(state.CYTOT_A + state.CYS_A[is]);
      state.CLTOT_A = f32(state.CLTOT_A + state.CLS_A[is]);

      for (let n = 0; n < state.NUMAX; n += 1) {
        state.CDTOT_U[n] = f32(state.CDTOT_U[n] + state.CDS_U[idx2(is, n, dimSurf)]);
        state.CYTOT_U[n] = f32(state.CYTOT_U[n] + state.CYS_U[idx2(is, n, dimSurf)]);
        state.CLTOT_U[n] = f32(state.CLTOT_U[n] + state.CLS_U[idx2(is, n, dimSurf)]);
        for (let l = 0; l < 3; l += 1) {
          state.CFTOT_U[idx2(l, n, 3)] = f32(state.CFTOT_U[idx2(l, n, 3)] + state.CFS_U[idx2(l, idx2(is, n, dimSurf), 3)]);
          state.CMTOT_U[idx2(l, n, 3)] = f32(state.CMTOT_U[idx2(l, n, 3)] + state.CMS_U[idx2(l, idx2(is, n, dimSurf), 3)]);
        }
      }

      for (let n = 0; n < state.NCONTROL; n += 1) {
        state.CDTOT_D[n] = f32(state.CDTOT_D[n] + state.CDS_D[idx2(is, n, dimSurf)]);
        state.CYTOT_D[n] = f32(state.CYTOT_D[n] + state.CYS_D[idx2(is, n, dimSurf)]);
        state.CLTOT_D[n] = f32(state.CLTOT_D[n] + state.CLS_D[idx2(is, n, dimSurf)]);
        for (let l = 0; l < 3; l += 1) {
          state.CFTOT_D[idx2(l, n, 3)] = f32(state.CFTOT_D[idx2(l, n, 3)] + state.CFS_D[idx2(l, idx2(is, n, dimSurf), 3)]);
          state.CMTOT_D[idx2(l, n, 3)] = f32(state.CMTOT_D[idx2(l, n, 3)] + state.CMS_D[idx2(l, idx2(is, n, dimSurf), 3)]);
        }
      }

      for (let n = 0; n < state.NDESIGN; n += 1) {
        state.CDTOT_G[n] = f32(state.CDTOT_G[n] + state.CDS_G[idx2(is, n, dimSurf)]);
        state.CYTOT_G[n] = f32(state.CYTOT_G[n] + state.CYS_G[idx2(is, n, dimSurf)]);
        state.CLTOT_G[n] = f32(state.CLTOT_G[n] + state.CLS_G[idx2(is, n, dimSurf)]);
        for (let l = 0; l < 3; l += 1) {
          state.CFTOT_G[idx2(l, n, 3)] = f32(state.CFTOT_G[idx2(l, n, 3)] + state.CFS_G[idx2(l, idx2(is, n, dimSurf), 3)]);
          state.CMTOT_G[idx2(l, n, 3)] = f32(state.CMTOT_G[idx2(l, n, 3)] + state.CMS_G[idx2(l, idx2(is, n, dimSurf), 3)]);
        }
      }
    }
  }

  return state;
}

export function BDFORC(state) {
  const betm = f32(Math.sqrt(f32(1.0 - f32(state.MACH * state.MACH))));
  const sina = f32(Math.sin(f32(state.ALFA)));
  const cosa = f32(Math.cos(f32(state.ALFA)));

  for (let ib = 0; ib < state.NBODY; ib += 1) {
    state.CDBDY[ib] = 0.0;
    state.CYBDY[ib] = 0.0;
    state.CLBDY[ib] = 0.0;
    for (let l = 0; l < 3; l += 1) {
      state.CFBDY[idx2(l, ib, 3)] = 0.0;
      state.CMBDY[idx2(l, ib, 3)] = 0.0;
    }

    const cdbdy_u = new Float32Array(6);
    const cybdy_u = new Float32Array(6);
    const clbdy_u = new Float32Array(6);
    const cfbd_y_u = new Float32Array(3 * state.NUMAX);
    const cmbdy_u = new Float32Array(3 * state.NUMAX);

    for (let iu = 0; iu < 6; iu += 1) {
      cdbdy_u[iu] = 0.0;
      cybdy_u[iu] = 0.0;
      clbdy_u[iu] = 0.0;
      for (let l = 0; l < 3; l += 1) {
        cfbd_y_u[idx2(l, iu, 3)] = 0.0;
        cmbdy_u[idx2(l, iu, 3)] = 0.0;
      }
    }

    const nln = state.NL[ib];
    for (let ilseg = 0; ilseg < nln - 1; ilseg += 1) {
      const l1 = state.LFRST[ib] + ilseg;
      const l2 = state.LFRST[ib] + ilseg + 1;

      const drl = new Float32Array(3);
      drl[0] = f32((state.RL[idx3(0, l2)] - state.RL[idx3(0, l1)]) / betm);
      drl[1] = f32(state.RL[idx3(1, l2)] - state.RL[idx3(1, l1)]);
      drl[2] = f32(state.RL[idx3(2, l2)] - state.RL[idx3(2, l1)]);
      const drlmag = f32(Math.sqrt(f32(f32(drl[0] * drl[0]) + f32(drl[1] * drl[1]) + f32(drl[2] * drl[2]))));
      const drlmi = drlmag === 0.0 ? 0.0 : f32(1.0 / drlmag);

      const dia = f32(state.RADL[l1] + state.RADL[l2]);
      const dinv = dia <= 0.0 ? 0.0 : f32(1.0 / dia);

      const esl = new Float32Array(3);
      esl[0] = f32(drl[0] * drlmi);
      esl[1] = f32(drl[1] * drlmi);
      esl[2] = f32(drl[2] * drlmi);

      const rrot = new Float32Array(3);
      rrot[0] = f32(0.5 * (state.RL[idx3(0, l2)] + state.RL[idx3(0, l1)]) - state.XYZREF[0]);
      rrot[1] = f32(0.5 * (state.RL[idx3(1, l2)] + state.RL[idx3(1, l1)]) - state.XYZREF[1]);
      rrot[2] = f32(0.5 * (state.RL[idx3(2, l2)] + state.RL[idx3(2, l1)]) - state.XYZREF[2]);

      const vrot = new Float32Array(3);
      CROSS(rrot, state.WROT, vrot);

      const veff = new Float32Array(3);
      veff[0] = f32((state.VINF[0] + vrot[0]) / betm);
      veff[1] = f32(state.VINF[1] + vrot[1]);
      veff[2] = f32(state.VINF[2] + vrot[2]);

      const veff_u = new Float32Array(3 * 6);
      for (let k = 0; k < 3; k += 1) {
        veff_u[idx2(0, k, 3)] = 0.0;
        veff_u[idx2(1, k, 3)] = 0.0;
        veff_u[idx2(2, k, 3)] = 0.0;
        veff_u[idx2(k, k, 3)] = 1.0;
      }
      for (let k = 3; k < 6; k += 1) {
        const wrot_u = new Float32Array(3);
        wrot_u[0] = 0.0; wrot_u[1] = 0.0; wrot_u[2] = 0.0;
        wrot_u[k - 3] = 1.0;
        const vrot_u = new Float32Array(3);
        CROSS(rrot, wrot_u, vrot_u);
        veff_u[idx2(0, k, 3)] = vrot_u[0];
        veff_u[idx2(1, k, 3)] = vrot_u[1];
        veff_u[idx2(2, k, 3)] = vrot_u[2];
      }

      const us = f32(veff[0] * esl[0] + veff[1] * esl[1] + veff[2] * esl[2]);

      const fb = new Float32Array(3);
      const fb_u = new Float32Array(3 * 6);
      for (let k = 0; k < 3; k += 1) {
        const un = f32(veff[k] - f32(us * esl[k]));
        fb[k] = f32(un * state.SRC[l1]);
        for (let iu = 0; iu < 6; iu += 1) {
          const un_u = f32(veff_u[idx2(k, iu, 3)]
            - f32((veff_u[idx2(0, iu, 3)] * esl[0]
              + veff_u[idx2(1, iu, 3)] * esl[1]
              + veff_u[idx2(2, iu, 3)] * esl[2]) * esl[k]));
          fb_u[idx2(k, iu, 3)] = f32(un * state.SRC_U[idx2(l1, iu, state.NLNODE)] + un_u * state.SRC[l1]);
        }
        state.DCPB[idx2(k, l1, 3)] = f32(fb[k] * f32(2.0 * dinv * drlmi));
      }

      const mb = new Float32Array(3);
      CROSS(rrot, fb, mb);
      const mb_u = new Float32Array(3 * 6);
      for (let iu = 0; iu < 6; iu += 1) {
        const tmp = new Float32Array(3);
        tmp[0] = fb_u[idx2(0, iu, 3)];
        tmp[1] = fb_u[idx2(1, iu, 3)];
        tmp[2] = fb_u[idx2(2, iu, 3)];
        const out = new Float32Array(3);
        CROSS(rrot, tmp, out);
        mb_u[idx2(0, iu, 3)] = out[0];
        mb_u[idx2(1, iu, 3)] = out[1];
        mb_u[idx2(2, iu, 3)] = out[2];
      }

      state.CDBDY[ib] = f32(state.CDBDY[ib] + f32((fb[0] * cosa + fb[2] * sina) * 2.0 / state.SREF));
      state.CYBDY[ib] = f32(state.CYBDY[ib] + f32(fb[1] * 2.0 / state.SREF));
      state.CLBDY[ib] = f32(state.CLBDY[ib] + f32((-fb[0] * sina + fb[2] * cosa) * 2.0 / state.SREF));
      for (let l = 0; l < 3; l += 1) {
        state.CFBDY[idx2(l, ib, 3)] = f32(state.CFBDY[idx2(l, ib, 3)] + f32(fb[l] * 2.0 / state.SREF));
      }
      state.CMBDY[idx2(0, ib, 3)] = f32(state.CMBDY[idx2(0, ib, 3)] + f32(mb[0] * 2.0 / state.SREF / state.BREF));
      state.CMBDY[idx2(1, ib, 3)] = f32(state.CMBDY[idx2(1, ib, 3)] + f32(mb[1] * 2.0 / state.SREF / state.CREF));
      state.CMBDY[idx2(2, ib, 3)] = f32(state.CMBDY[idx2(2, ib, 3)] + f32(mb[2] * 2.0 / state.SREF / state.BREF));

      for (let iu = 0; iu < 6; iu += 1) {
        cdbdy_u[iu] = f32(cdbdy_u[iu] + f32((fb_u[idx2(0, iu, 3)] * cosa + fb_u[idx2(2, iu, 3)] * sina) * 2.0 / state.SREF));
        cybdy_u[iu] = f32(cybdy_u[iu] + f32(fb_u[idx2(1, iu, 3)] * 2.0 / state.SREF));
        clbdy_u[iu] = f32(clbdy_u[iu] + f32((-fb_u[idx2(0, iu, 3)] * sina + fb_u[idx2(2, iu, 3)] * cosa) * 2.0 / state.SREF));
        for (let l = 0; l < 3; l += 1) {
          cfbd_y_u[idx2(l, iu, 3)] = f32(cfbd_y_u[idx2(l, iu, 3)] + f32(fb_u[idx2(l, iu, 3)] * 2.0 / state.SREF));
        }
        cmbdy_u[idx2(0, iu, 3)] = f32(cmbdy_u[idx2(0, iu, 3)] + f32(mb_u[idx2(0, iu, 3)] * 2.0 / state.SREF / state.BREF));
        cmbdy_u[idx2(1, iu, 3)] = f32(cmbdy_u[idx2(1, iu, 3)] + f32(mb_u[idx2(1, iu, 3)] * 2.0 / state.SREF / state.CREF));
        cmbdy_u[idx2(2, iu, 3)] = f32(cmbdy_u[idx2(2, iu, 3)] + f32(mb_u[idx2(2, iu, 3)] * 2.0 / state.SREF / state.BREF));
      }
    }

    state.CDTOT = f32(state.CDTOT + state.CDBDY[ib]);
    state.CYTOT = f32(state.CYTOT + state.CYBDY[ib]);
    state.CLTOT = f32(state.CLTOT + state.CLBDY[ib]);

    for (let l = 0; l < 3; l += 1) {
      state.CFTOT[l] = f32(state.CFTOT[l] + state.CFBDY[idx2(l, ib, 3)]);
      state.CMTOT[l] = f32(state.CMTOT[l] + state.CMBDY[idx2(l, ib, 3)]);
    }

    for (let iu = 0; iu < 6; iu += 1) {
      state.CDTOT_U[iu] = f32(state.CDTOT_U[iu] + cdbdy_u[iu]);
      state.CYTOT_U[iu] = f32(state.CYTOT_U[iu] + cybdy_u[iu]);
      state.CLTOT_U[iu] = f32(state.CLTOT_U[iu] + clbdy_u[iu]);
      for (let l = 0; l < 3; l += 1) {
        state.CFTOT_U[idx2(l, iu, 3)] = f32(state.CFTOT_U[idx2(l, iu, 3)] + cfbd_y_u[idx2(l, iu, 3)]);
        state.CMTOT_U[idx2(l, iu, 3)] = f32(state.CMTOT_U[idx2(l, iu, 3)] + cmbdy_u[idx2(l, iu, 3)]);
      }
    }
  }

  return state;
}
