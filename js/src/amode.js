// Port of AVL amode.f (selected non-interactive routines) with float32 math.

import { M3INV, ROTENS3, RATEKI3 } from './autil.js';

const f32 = Math.fround;

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function idx2r(i, j, dim2) {
  return i * dim2 + j;
}

function idx3(i, j, k, dim1, dim2) {
  return i * dim2 + j + dim1 * dim2 * k;
}

function getVinf(state, k) {
  return state.VINF[k - 1] ?? 0.0;
}

function getWrot(state, k) {
  return state.WROT[k - 1] ?? 0.0;
}

function getCFTOTU(state, k, iu) {
  return state.CFTOT_U[(k - 1) + 3 * (iu - 1)] ?? 0.0;
}

function getCFTOTD(state, k, n) {
  return state.CFTOT_D[(k - 1) + 3 * (n - 1)] ?? 0.0;
}

function getCMTOTU(state, k, iu) {
  return state.CMTOT_U[(k - 1) + 3 * (iu - 1)] ?? 0.0;
}

function getCMTOTD(state, k, n) {
  return state.CMTOT_D[(k - 1) + 3 * (n - 1)] ?? 0.0;
}

export function RUNCHK(state, JRUN) {
  let ok = true;
  for (let iv = 1; iv <= state.NVTOT; iv += 1) {
    for (let jv = 1; jv <= state.NVTOT; jv += 1) {
      if (iv !== jv) {
        const ic1 = state.ICON[idx2(iv, JRUN, state.IVMAX)];
        const ic2 = state.ICON[idx2(jv, JRUN, state.IVMAX)];
        if (ic1 === ic2) {
          ok = false;
        }
      }
    }
  }
  return ok;
}

export function SYSMAT(state, IR, ASYS, BSYS, RSYS) {
  const ICRS = [0, 2, 3, 1];
  const JCRS = [0, 3, 1, 2];

  const GEE = state.PARVAL[idx2(state.IPGEE, IR, state.IPTOT)];
  const RHO = state.PARVAL[idx2(state.IPRHO, IR, state.IPTOT)];
  const VEE = state.PARVAL[idx2(state.IPVEE, IR, state.IPTOT)];
  const PHI = state.PARVAL[idx2(state.IPPHI, IR, state.IPTOT)];
  const THE = state.PARVAL[idx2(state.IPTHE, IR, state.IPTOT)];
  const PSI = state.PARVAL[idx2(state.IPPSI, IR, state.IPTOT)];
  const XCG = state.PARVAL[idx2(state.IPXCG, IR, state.IPTOT)];
  const YCG = state.PARVAL[idx2(state.IPYCG, IR, state.IPTOT)];
  const ZCG = state.PARVAL[idx2(state.IPZCG, IR, state.IPTOT)];
  const RMASS = state.PARVAL[idx2(state.IPMASS, IR, state.IPTOT)];

  const RINER = new Float32Array(9);
  RINER[0] = state.PARVAL[idx2(state.IPIXX, IR, state.IPTOT)];
  RINER[4] = state.PARVAL[idx2(state.IPIYY, IR, state.IPTOT)];
  RINER[8] = state.PARVAL[idx2(state.IPIZZ, IR, state.IPTOT)];
  RINER[1] = state.PARVAL[idx2(state.IPIXY, IR, state.IPTOT)];
  RINER[5] = state.PARVAL[idx2(state.IPIYZ, IR, state.IPTOT)];
  RINER[6] = state.PARVAL[idx2(state.IPIZX, IR, state.IPTOT)];
  RINER[3] = RINER[1];
  RINER[7] = RINER[5];
  RINER[2] = RINER[6];

  const DCL_U = state.PARVAL[idx2(state.IPCLU, IR, state.IPTOT)];
  const DCM_U = state.PARVAL[idx2(state.IPCMU, IR, state.IPTOT)];
  const DCL_A = state.PARVAL[idx2(state.IPCLA, IR, state.IPTOT)];
  const DCM_A = state.PARVAL[idx2(state.IPCMA, IR, state.IPTOT)];

  if (VEE <= 0.0 || RMASS <= 0.0 || RINER[0] <= 0.0 || RINER[4] <= 0.0 || RINER[8] <= 0.0) {
    return { NSYS: 0, LTERR: true };
  }

  const SREFD = f32(state.SREF * state.UNITL * state.UNITL);
  const BREFD = f32(state.BREF * state.UNITL);
  const CREFD = f32(state.CREF * state.UNITL);

  state.XYZREF[0] = XCG;
  state.XYZREF[1] = YCG;
  state.XYZREF[2] = ZCG;

  const QS = f32(0.5 * RHO * VEE * VEE * SREFD);
  const ROT = f32(VEE / state.UNITL);

  const MAMAT = new Float32Array(9);
  const RIMAT = new Float32Array(9);
  for (let k = 0; k < 3; k += 1) {
    MAMAT[idx2(k, 0, 3)] = f32(state.AMASS[idx2(k, 0, 3)] * RHO);
    MAMAT[idx2(k, 1, 3)] = f32(state.AMASS[idx2(k, 1, 3)] * RHO);
    MAMAT[idx2(k, 2, 3)] = f32(state.AMASS[idx2(k, 2, 3)] * RHO);
    MAMAT[idx2(k, k, 3)] = f32(MAMAT[idx2(k, k, 3)] + RMASS);

    RIMAT[idx2(k, 0, 3)] = f32(RINER[idx2(k, 0, 3)] + state.AINER[idx2(k, 0, 3)] * RHO);
    RIMAT[idx2(k, 1, 3)] = f32(RINER[idx2(k, 1, 3)] + state.AINER[idx2(k, 1, 3)] * RHO);
    RIMAT[idx2(k, 2, 3)] = f32(RINER[idx2(k, 2, 3)] + state.AINER[idx2(k, 2, 3)] * RHO);
  }

  const MAINV = new Float32Array(9);
  const RIINV = new Float32Array(9);
  M3INV(MAMAT, MAINV);
  M3INV(RIMAT, RIINV);

  const P = new Float32Array(4);
  const H = new Float32Array(4);
  const P_U = new Float32Array((4) * (state.NUMAX + 1));
  const H_U = new Float32Array((4) * (state.NUMAX + 1));

  for (let k = 1; k <= 3; k += 1) {
    const pk = f32(-(MAMAT[idx2(k - 1, 0, 3)] * getVinf(state, 1)
      + MAMAT[idx2(k - 1, 1, 3)] * getVinf(state, 2)
      + MAMAT[idx2(k - 1, 2, 3)] * getVinf(state, 3)) * VEE);
    P[k] = pk;
    P_U[idx2(k, 1, 4)] = f32(-MAMAT[idx2(k - 1, 0, 3)] * VEE);
    P_U[idx2(k, 2, 4)] = f32(-MAMAT[idx2(k - 1, 1, 3)] * VEE);
    P_U[idx2(k, 3, 4)] = f32(-MAMAT[idx2(k - 1, 2, 3)] * VEE);

    const hk = f32((RIMAT[idx2(k - 1, 0, 3)] * getWrot(state, 1)
      + RIMAT[idx2(k - 1, 1, 3)] * getWrot(state, 2)
      + RIMAT[idx2(k - 1, 2, 3)] * getWrot(state, 3)) * ROT);
    H[k] = hk;
    H_U[idx2(k, 4, 4)] = f32(RIMAT[idx2(k - 1, 0, 3)] * ROT);
    H_U[idx2(k, 5, 4)] = f32(RIMAT[idx2(k - 1, 1, 3)] * ROT);
    H_U[idx2(k, 6, 4)] = f32(RIMAT[idx2(k - 1, 2, 3)] * ROT);
  }

  const WXP = new Float32Array(4);
  const WXH = new Float32Array(4);
  const WXP_U = new Float32Array((4) * (state.NUMAX + 1));
  const WXH_U = new Float32Array((4) * (state.NUMAX + 1));
  for (let k = 1; k <= 3; k += 1) {
    const i = ICRS[k];
    const j = JCRS[k];
    WXP[k] = f32((getWrot(state, i) * P[j] - getWrot(state, j) * P[i]) * ROT);
    WXH[k] = f32((getWrot(state, i) * H[j] - getWrot(state, j) * H[i]) * ROT);

    for (let iu = 1; iu <= 6; iu += 1) {
      WXP_U[idx2(k, iu, 4)] = f32((getWrot(state, i) * P_U[idx2(j, iu, 4)]
        - getWrot(state, j) * P_U[idx2(i, iu, 4)]) * ROT);
      WXH_U[idx2(k, iu, 4)] = f32((getWrot(state, i) * H_U[idx2(j, iu, 4)]
        - getWrot(state, j) * H_U[idx2(i, iu, 4)]) * ROT);
    }
    WXP_U[idx2(k, i + 3, 4)] = f32(WXP_U[idx2(k, i + 3, 4)] + P[j] * ROT);
    WXP_U[idx2(k, j + 3, 4)] = f32(WXP_U[idx2(k, j + 3, 4)] - P[i] * ROT);

    WXH_U[idx2(k, i + 3, 4)] = f32(WXH_U[idx2(k, i + 3, 4)] + H[j] * ROT);
    WXH_U[idx2(k, j + 3, 4)] = f32(WXH_U[idx2(k, j + 3, 4)] - H[i] * ROT);
  }

  const MIF = new Float32Array(4);
  const RIM = new Float32Array(4);
  const PRF = new Float32Array(4);
  const PRM = new Float32Array(4);
  const MIF_U = new Float32Array((4) * (state.NUMAX + 1));
  const RIM_U = new Float32Array((4) * (state.NUMAX + 1));
  const PRF_U = new Float32Array((4) * (state.NUMAX + 1));
  const PRM_U = new Float32Array((4) * (state.NUMAX + 1));
  const MIF_D = new Float32Array((4) * (state.NDMAX + 1));
  const RIM_D = new Float32Array((4) * (state.NDMAX + 1));

  for (let k = 1; k <= 3; k += 1) {
    MIF[k] = f32(MAINV[idx2(k - 1, 0, 3)] * state.CFTOT[0] * QS
      + MAINV[idx2(k - 1, 1, 3)] * state.CFTOT[1] * QS
      + MAINV[idx2(k - 1, 2, 3)] * state.CFTOT[2] * QS);
    RIM[k] = f32(RIINV[idx2(k - 1, 0, 3)] * state.CMTOT[0] * QS * BREFD
      + RIINV[idx2(k - 1, 1, 3)] * state.CMTOT[1] * QS * CREFD
      + RIINV[idx2(k - 1, 2, 3)] * state.CMTOT[2] * QS * BREFD);
    PRF[k] = f32(MAINV[idx2(k - 1, 0, 3)] * WXP[1]
      + MAINV[idx2(k - 1, 1, 3)] * WXP[2]
      + MAINV[idx2(k - 1, 2, 3)] * WXP[3]);
    PRM[k] = f32(RIINV[idx2(k - 1, 0, 3)] * WXH[1]
      + RIINV[idx2(k - 1, 1, 3)] * WXH[2]
      + RIINV[idx2(k - 1, 2, 3)] * WXH[3]);

    for (let iu = 1; iu <= 6; iu += 1) {
      MIF_U[idx2(k, iu, 4)] = f32(
        MAINV[idx2(k - 1, 0, 3)] * getCFTOTU(state, 1, iu) * QS
        + MAINV[idx2(k - 1, 1, 3)] * getCFTOTU(state, 2, iu) * QS
        + MAINV[idx2(k - 1, 2, 3)] * getCFTOTU(state, 3, iu) * QS,
      );
      RIM_U[idx2(k, iu, 4)] = f32(
        RIINV[idx2(k - 1, 0, 3)] * getCMTOTU(state, 1, iu) * QS * BREFD
        + RIINV[idx2(k - 1, 1, 3)] * getCMTOTU(state, 2, iu) * QS * CREFD
        + RIINV[idx2(k - 1, 2, 3)] * getCMTOTU(state, 3, iu) * QS * BREFD,
      );
      PRF_U[idx2(k, iu, 4)] = f32(
        MAINV[idx2(k - 1, 0, 3)] * WXP_U[idx2(1, iu, 4)]
        + MAINV[idx2(k - 1, 1, 3)] * WXP_U[idx2(2, iu, 4)]
        + MAINV[idx2(k - 1, 2, 3)] * WXP_U[idx2(3, iu, 4)],
      );
      PRM_U[idx2(k, iu, 4)] = f32(
        RIINV[idx2(k - 1, 0, 3)] * WXH_U[idx2(1, iu, 4)]
        + RIINV[idx2(k - 1, 1, 3)] * WXH_U[idx2(2, iu, 4)]
        + RIINV[idx2(k - 1, 2, 3)] * WXH_U[idx2(3, iu, 4)],
      );
    }

    for (let n = 1; n <= state.NCONTROL; n += 1) {
      MIF_D[idx2(k, n, 4)] = f32(
        MAINV[idx2(k - 1, 0, 3)] * getCFTOTD(state, 1, n) * QS
        + MAINV[idx2(k - 1, 1, 3)] * getCFTOTD(state, 2, n) * QS
        + MAINV[idx2(k - 1, 2, 3)] * getCFTOTD(state, 3, n) * QS,
      );
      RIM_D[idx2(k, n, 4)] = f32(
        RIINV[idx2(k - 1, 0, 3)] * getCMTOTD(state, 1, n) * QS * BREFD
        + RIINV[idx2(k - 1, 1, 3)] * getCMTOTD(state, 2, n) * QS * CREFD
        + RIINV[idx2(k - 1, 2, 3)] * getCMTOTD(state, 3, n) * QS * BREFD,
      );
    }

    let iu = 1;
    MIF_U[idx2(k, iu, 4)] = f32(MIF_U[idx2(k, iu, 4)] - MAINV[idx2(k - 1, 2, 3)] * DCL_U * QS);
    RIM_U[idx2(k, iu, 4)] = f32(RIM_U[idx2(k, iu, 4)] - RIINV[idx2(k - 1, 1, 3)] * DCM_U * QS * CREFD);
    iu = 3;
    MIF_U[idx2(k, iu, 4)] = f32(MIF_U[idx2(k, iu, 4)] + MAINV[idx2(k - 1, 2, 3)] * DCL_A * QS);
    RIM_U[idx2(k, iu, 4)] = f32(RIM_U[idx2(k, iu, 4)] + RIINV[idx2(k - 1, 1, 3)] * DCM_A * QS * CREFD);
  }

  const ANG = new Float32Array([f32(PHI * state.DTR), f32(THE * state.DTR), f32(PSI * state.DTR)]);
  const TT = new Float32Array(9);
  const TT_ANG = new Float32Array(27);
  const RT = new Float32Array(9);
  const RT_ANG = new Float32Array(27);
  ROTENS3(ANG, TT, TT_ANG);
  RATEKI3(ANG, RT, RT_ANG);

  const NSYS = 12;
  for (let ieq = 1; ieq <= NSYS; ieq += 1) {
    for (let je = 1; je <= NSYS; je += 1) {
      ASYS[idx2(ieq, je, state.JEMAX + 1)] = 0.0;
    }
    for (let n = 1; n <= state.NCONTROL; n += 1) {
      BSYS[idx2(ieq, n, state.JEMAX + 1)] = 0.0;
    }
  }

  let IEQ = state.JEU;
  let K = 1;
  RSYS[IEQ] = f32(MIF[K] - PRF[K] - GEE * TT[idx2r(2, K - 1, 3)]);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = f32(-(MIF_U[idx2(K, 1, 4)] - PRF_U[idx2(K, 1, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = f32(-(MIF_U[idx2(K, 2, 4)] - PRF_U[idx2(K, 2, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = f32(-(MIF_U[idx2(K, 3, 4)] - PRF_U[idx2(K, 3, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = f32((MIF_U[idx2(K, 4, 4)] - PRF_U[idx2(K, 4, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = f32((MIF_U[idx2(K, 5, 4)] - PRF_U[idx2(K, 5, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = f32((MIF_U[idx2(K, 6, 4)] - PRF_U[idx2(K, 6, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(-GEE * TT_ANG[idx3(2, K - 1, 0, 3, 3)]);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(-GEE * TT_ANG[idx3(2, K - 1, 1, 3, 3)]);
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(-GEE * TT_ANG[idx3(2, K - 1, 2, 3, 3)]);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(MIF_D[idx2(K, n, 4)]);
  }

  IEQ = state.JEV;
  K = 2;
  RSYS[IEQ] = f32(MIF[K] - PRF[K] - GEE * TT[idx2r(2, K - 1, 3)]);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = f32(-(MIF_U[idx2(K, 1, 4)] - PRF_U[idx2(K, 1, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = f32(-(MIF_U[idx2(K, 2, 4)] - PRF_U[idx2(K, 2, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = f32(-(MIF_U[idx2(K, 3, 4)] - PRF_U[idx2(K, 3, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = f32((MIF_U[idx2(K, 4, 4)] - PRF_U[idx2(K, 4, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = f32((MIF_U[idx2(K, 5, 4)] - PRF_U[idx2(K, 5, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = f32((MIF_U[idx2(K, 6, 4)] - PRF_U[idx2(K, 6, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(-GEE * TT_ANG[idx3(2, K - 1, 0, 3, 3)]);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(-GEE * TT_ANG[idx3(2, K - 1, 1, 3, 3)]);
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(-GEE * TT_ANG[idx3(2, K - 1, 2, 3, 3)]);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(MIF_D[idx2(K, n, 4)]);
  }

  IEQ = state.JEW;
  K = 3;
  RSYS[IEQ] = f32(MIF[K] - PRF[K] - GEE * TT[idx2r(2, K - 1, 3)]);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = f32(-(MIF_U[idx2(K, 1, 4)] - PRF_U[idx2(K, 1, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = f32(-(MIF_U[idx2(K, 2, 4)] - PRF_U[idx2(K, 2, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = f32(-(MIF_U[idx2(K, 3, 4)] - PRF_U[idx2(K, 3, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = f32((MIF_U[idx2(K, 4, 4)] - PRF_U[idx2(K, 4, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = f32((MIF_U[idx2(K, 5, 4)] - PRF_U[idx2(K, 5, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = f32((MIF_U[idx2(K, 6, 4)] - PRF_U[idx2(K, 6, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(-GEE * TT_ANG[idx3(2, K - 1, 0, 3, 3)]);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(-GEE * TT_ANG[idx3(2, K - 1, 1, 3, 3)]);
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(-GEE * TT_ANG[idx3(2, K - 1, 2, 3, 3)]);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(MIF_D[idx2(K, n, 4)]);
  }

  IEQ = state.JEP;
  K = 1;
  RSYS[IEQ] = f32(RIM[K] - PRM[K]);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = f32(-(RIM_U[idx2(K, 1, 4)] - PRM_U[idx2(K, 1, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = f32(-(RIM_U[idx2(K, 2, 4)] - PRM_U[idx2(K, 2, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = f32(-(RIM_U[idx2(K, 3, 4)] - PRM_U[idx2(K, 3, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = f32((RIM_U[idx2(K, 4, 4)] - PRM_U[idx2(K, 4, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = f32((RIM_U[idx2(K, 5, 4)] - PRM_U[idx2(K, 5, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = f32((RIM_U[idx2(K, 6, 4)] - PRM_U[idx2(K, 6, 4)]) / ROT);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(RIM_D[idx2(K, n, 4)]);
  }

  IEQ = state.JEQ;
  K = 2;
  RSYS[IEQ] = f32(RIM[K] - PRM[K]);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = f32(-(RIM_U[idx2(K, 1, 4)] - PRM_U[idx2(K, 1, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = f32(-(RIM_U[idx2(K, 2, 4)] - PRM_U[idx2(K, 2, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = f32(-(RIM_U[idx2(K, 3, 4)] - PRM_U[idx2(K, 3, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = f32((RIM_U[idx2(K, 4, 4)] - PRM_U[idx2(K, 4, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = f32((RIM_U[idx2(K, 5, 4)] - PRM_U[idx2(K, 5, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = f32((RIM_U[idx2(K, 6, 4)] - PRM_U[idx2(K, 6, 4)]) / ROT);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(RIM_D[idx2(K, n, 4)]);
  }

  IEQ = state.JER;
  K = 3;
  RSYS[IEQ] = f32(RIM[K] - PRM[K]);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = f32(-(RIM_U[idx2(K, 1, 4)] - PRM_U[idx2(K, 1, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = f32(-(RIM_U[idx2(K, 2, 4)] - PRM_U[idx2(K, 2, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = f32(-(RIM_U[idx2(K, 3, 4)] - PRM_U[idx2(K, 3, 4)]) / VEE);
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = f32((RIM_U[idx2(K, 4, 4)] - PRM_U[idx2(K, 4, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = f32((RIM_U[idx2(K, 5, 4)] - PRM_U[idx2(K, 5, 4)]) / ROT);
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = f32((RIM_U[idx2(K, 6, 4)] - PRM_U[idx2(K, 6, 4)]) / ROT);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(RIM_D[idx2(K, n, 4)]);
  }

  IEQ = state.JEPH;
  K = 1;
  RSYS[IEQ] = f32(ROT * (RT[idx2r(K - 1, 0, 3)] * getWrot(state, 1)
    + RT[idx2r(K - 1, 1, 3)] * getWrot(state, 2)
    + RT[idx2r(K - 1, 2, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = RT[idx2r(K - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = RT[idx2r(K - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = RT[idx2r(K - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 0, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 0, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 0, 3, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 1, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 1, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 1, 3, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 2, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 2, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 2, 3, 3)] * getWrot(state, 3)));

  IEQ = state.JETH;
  K = 2;
  RSYS[IEQ] = f32(ROT * (RT[idx2r(K - 1, 0, 3)] * getWrot(state, 1)
    + RT[idx2r(K - 1, 1, 3)] * getWrot(state, 2)
    + RT[idx2r(K - 1, 2, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = RT[idx2r(K - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = RT[idx2r(K - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = RT[idx2r(K - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 0, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 0, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 0, 3, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 1, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 1, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 1, 3, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 2, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 2, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 2, 3, 3)] * getWrot(state, 3)));

  IEQ = state.JEPS;
  K = 3;
  RSYS[IEQ] = f32(ROT * (RT[idx2r(K - 1, 0, 3)] * getWrot(state, 1)
    + RT[idx2r(K - 1, 1, 3)] * getWrot(state, 2)
    + RT[idx2r(K - 1, 2, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = RT[idx2r(K - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = RT[idx2r(K - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = RT[idx2r(K - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 0, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 0, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 0, 3, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 1, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 1, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 1, 3, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 2, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 2, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 2, 3, 3)] * getWrot(state, 3)));

  IEQ = state.JEX;
  K = 1;
  RSYS[IEQ] = f32(-(TT[idx2r(K - 1, 0, 3)] * getVinf(state, 1)
    + TT[idx2r(K - 1, 1, 3)] * getVinf(state, 2)
    + TT[idx2r(K - 1, 2, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = TT[idx2r(K - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = TT[idx2r(K - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = TT[idx2r(K - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(K - 1, 0, 0, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(K - 1, 1, 0, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(K - 1, 2, 0, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(K - 1, 0, 1, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(K - 1, 1, 1, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(K - 1, 2, 1, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(K - 1, 0, 2, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(K - 1, 1, 2, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(K - 1, 2, 2, 3, 3)] * getVinf(state, 3)) * VEE);

  IEQ = state.JEY;
  K = 2;
  RSYS[IEQ] = f32(-(TT[idx2r(K - 1, 0, 3)] * getVinf(state, 1)
    + TT[idx2r(K - 1, 1, 3)] * getVinf(state, 2)
    + TT[idx2r(K - 1, 2, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = TT[idx2r(K - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = TT[idx2r(K - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = TT[idx2r(K - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(K - 1, 0, 0, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(K - 1, 1, 0, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(K - 1, 2, 0, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(K - 1, 0, 1, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(K - 1, 1, 1, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(K - 1, 2, 1, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(K - 1, 0, 2, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(K - 1, 1, 2, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(K - 1, 2, 2, 3, 3)] * getVinf(state, 3)) * VEE);

  IEQ = state.JEZ;
  K = 3;
  RSYS[IEQ] = f32(-(TT[idx2r(K - 1, 0, 3)] * getVinf(state, 1)
    + TT[idx2r(K - 1, 1, 3)] * getVinf(state, 2)
    + TT[idx2r(K - 1, 2, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = TT[idx2r(K - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = TT[idx2r(K - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = TT[idx2r(K - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(K - 1, 0, 0, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(K - 1, 1, 0, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(K - 1, 2, 0, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(K - 1, 0, 1, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(K - 1, 1, 1, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(K - 1, 2, 1, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(K - 1, 0, 2, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(K - 1, 1, 2, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(K - 1, 2, 2, 3, 3)] * getVinf(state, 3)) * VEE);

  return { NSYS, LTERR: false };
}

export function APPMAT(state, IR, ASYS, BSYS, RSYS) {
  const GEE = state.PARVAL[idx2(state.IPGEE, IR, state.IPTOT)];
  const RHO = state.PARVAL[idx2(state.IPRHO, IR, state.IPTOT)];
  const VEE = state.PARVAL[idx2(state.IPVEE, IR, state.IPTOT)];
  const PHI = state.PARVAL[idx2(state.IPPHI, IR, state.IPTOT)];
  const THE = state.PARVAL[idx2(state.IPTHE, IR, state.IPTOT)];
  const PSI = state.PARVAL[idx2(state.IPPSI, IR, state.IPTOT)];
  const XCG = state.PARVAL[idx2(state.IPXCG, IR, state.IPTOT)];
  const YCG = state.PARVAL[idx2(state.IPYCG, IR, state.IPTOT)];
  const ZCG = state.PARVAL[idx2(state.IPZCG, IR, state.IPTOT)];
  const RMASS = state.PARVAL[idx2(state.IPMASS, IR, state.IPTOT)];

  const RINXX = state.PARVAL[idx2(state.IPIXX, IR, state.IPTOT)];
  const RINYY = state.PARVAL[idx2(state.IPIYY, IR, state.IPTOT)];
  const RINZZ = state.PARVAL[idx2(state.IPIZZ, IR, state.IPTOT)];

  if (VEE <= 0.0 || RMASS <= 0.0 || RINXX <= 0.0 || RINYY <= 0.0 || RINZZ <= 0.0) {
    return { NSYS: 0, LTERR: true };
  }

  const SREFD = f32(state.SREF * state.UNITL * state.UNITL);
  const BREFD = f32(state.BREF * state.UNITL);
  const CREFD = f32(state.CREF * state.UNITL);

  state.XYZREF[0] = XCG;
  state.XYZREF[1] = YCG;
  state.XYZREF[2] = ZCG;

  const QS = f32(0.5 * RHO * VEE * VEE * SREFD);
  const QSC = f32(QS * CREFD);
  const QSB = f32(QS * BREFD);
  const ROT = f32(VEE / state.UNITL);

  const ANG = new Float32Array([f32(PHI * state.DTR), f32(THE * state.DTR), f32(PSI * state.DTR)]);
  const TT = new Float32Array(9);
  const TT_ANG = new Float32Array(27);
  const RT = new Float32Array(9);
  const RT_ANG = new Float32Array(27);
  ROTENS3(ANG, TT, TT_ANG);
  RATEKI3(ANG, RT, RT_ANG);

  const NSYS = 12;
  for (let ieq = 1; ieq <= NSYS; ieq += 1) {
    for (let je = 1; je <= NSYS; je += 1) {
      ASYS[idx2(ieq, je, state.JEMAX + 1)] = 0.0;
    }
    for (let n = 1; n <= state.NCONTROL; n += 1) {
      BSYS[idx2(ieq, n, state.JEMAX + 1)] = 0.0;
    }
  }

  let IEQ = state.JEU;
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = f32(-getCFTOTU(state, 1, 1) * QS / RMASS / VEE);
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = f32(-getCFTOTU(state, 1, 3) * QS / RMASS / VEE);
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = f32(getCFTOTU(state, 1, 5) * QS / RMASS / ROT + getVinf(state, 3) * VEE);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(GEE);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(getCFTOTD(state, 1, n) * QS / RMASS);
  }

  IEQ = state.JEW;
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = f32(-getCFTOTU(state, 3, 1) * QS / RMASS / VEE);
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = f32(-getCFTOTU(state, 3, 3) * QS / RMASS / VEE);
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = f32(getCFTOTU(state, 3, 5) * QS / RMASS / ROT - getVinf(state, 1) * VEE);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(getCFTOTD(state, 3, n) * QS / RMASS);
  }

  IEQ = state.JEQ;
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = f32(-getCMTOTU(state, 2, 1) * QSC / RINYY / VEE);
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = f32(-getCMTOTU(state, 2, 3) * QSC / RINYY / VEE);
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = f32(getCMTOTU(state, 2, 5) * QSC / RINYY / ROT);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(getCMTOTD(state, 2, n) * QSC / RINYY);
  }

  IEQ = state.JETH;
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = 1.0;

  IEQ = state.JEV;
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = f32(-getCFTOTU(state, 2, 2) * QS / RMASS / VEE);
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = f32(getCFTOTU(state, 2, 4) * QS / RMASS / ROT - getVinf(state, 3) * VEE);
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = f32(getCFTOTU(state, 2, 6) * QS / RMASS / ROT + getVinf(state, 1) * VEE);
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(GEE);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(getCFTOTD(state, 2, n) * QS / RMASS);
  }

  IEQ = state.JEP;
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = f32(-getCMTOTU(state, 1, 2) * QSB / RINXX / VEE);
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = f32(getCMTOTU(state, 1, 4) * QSB / RINXX / ROT);
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = f32(getCMTOTU(state, 1, 6) * QSB / RINXX / ROT);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(getCMTOTD(state, 1, n) * QSB / RINXX);
  }

  IEQ = state.JER;
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = f32(-getCMTOTU(state, 3, 2) * QSB / RINZZ / VEE);
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = f32(getCMTOTU(state, 3, 4) * QSB / RINZZ / ROT);
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = f32(getCMTOTU(state, 3, 6) * QSB / RINZZ / ROT);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    BSYS[idx2(IEQ, n, state.JEMAX + 1)] = f32(getCMTOTD(state, 3, n) * QSB / RINZZ);
  }

  IEQ = state.JEPH;
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = -1.0;

  IEQ = state.JEPS;
  const K = 3;
  RSYS[IEQ] = f32(ROT * (RT[idx2r(K - 1, 0, 3)] * getWrot(state, 1)
    + RT[idx2r(K - 1, 1, 3)] * getWrot(state, 2)
    + RT[idx2r(K - 1, 2, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JEP, state.JEMAX + 1)] = RT[idx2r(K - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEQ, state.JEMAX + 1)] = RT[idx2r(K - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JER, state.JEMAX + 1)] = RT[idx2r(K - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 0, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 0, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 0, 3, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 1, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 1, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 1, 3, 3)] * getWrot(state, 3)));
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(ROT * (RT_ANG[idx3(K - 1, 0, 2, 3, 3)] * getWrot(state, 1)
    + RT_ANG[idx3(K - 1, 1, 2, 3, 3)] * getWrot(state, 2)
    + RT_ANG[idx3(K - 1, 2, 2, 3, 3)] * getWrot(state, 3)));

  IEQ = state.JEX;
  let k = 1;
  RSYS[IEQ] = f32(-(TT[idx2r(k - 1, 0, 3)] * getVinf(state, 1)
    + TT[idx2r(k - 1, 1, 3)] * getVinf(state, 2)
    + TT[idx2r(k - 1, 2, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = TT[idx2r(k - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = TT[idx2r(k - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = TT[idx2r(k - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(k - 1, 0, 0, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(k - 1, 1, 0, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(k - 1, 2, 0, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(k - 1, 0, 1, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(k - 1, 1, 1, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(k - 1, 2, 1, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(k - 1, 0, 2, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(k - 1, 1, 2, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(k - 1, 2, 2, 3, 3)] * getVinf(state, 3)) * VEE);

  IEQ = state.JEY;
  k = 2;
  RSYS[IEQ] = f32(-(TT[idx2r(k - 1, 0, 3)] * getVinf(state, 1)
    + TT[idx2r(k - 1, 1, 3)] * getVinf(state, 2)
    + TT[idx2r(k - 1, 2, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = TT[idx2r(k - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = TT[idx2r(k - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = TT[idx2r(k - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(k - 1, 0, 0, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(k - 1, 1, 0, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(k - 1, 2, 0, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(k - 1, 0, 1, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(k - 1, 1, 1, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(k - 1, 2, 1, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(k - 1, 0, 2, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(k - 1, 1, 2, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(k - 1, 2, 2, 3, 3)] * getVinf(state, 3)) * VEE);

  IEQ = state.JEZ;
  k = 3;
  RSYS[IEQ] = f32(-(TT[idx2r(k - 1, 0, 3)] * getVinf(state, 1)
    + TT[idx2r(k - 1, 1, 3)] * getVinf(state, 2)
    + TT[idx2r(k - 1, 2, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEU, state.JEMAX + 1)] = TT[idx2r(k - 1, 0, 3)];
  ASYS[idx2(IEQ, state.JEV, state.JEMAX + 1)] = TT[idx2r(k - 1, 1, 3)];
  ASYS[idx2(IEQ, state.JEW, state.JEMAX + 1)] = TT[idx2r(k - 1, 2, 3)];
  ASYS[idx2(IEQ, state.JEPH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(k - 1, 0, 0, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(k - 1, 1, 0, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(k - 1, 2, 0, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JETH, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(k - 1, 0, 1, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(k - 1, 1, 1, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(k - 1, 2, 1, 3, 3)] * getVinf(state, 3)) * VEE);
  ASYS[idx2(IEQ, state.JEPS, state.JEMAX + 1)] = f32(-(TT_ANG[idx3(k - 1, 0, 2, 3, 3)] * getVinf(state, 1)
    + TT_ANG[idx3(k - 1, 1, 2, 3, 3)] * getVinf(state, 2)
    + TT_ANG[idx3(k - 1, 2, 2, 3, 3)] * getVinf(state, 3)) * VEE);

  return { NSYS, LTERR: false };
}

export function SYSSHO(state, ASYS, BSYS, RSYS, NSYS) {
  const usgn = new Float32Array(NSYS + 1);
  for (let i = 1; i <= NSYS; i += 1) usgn[i] = 1.0;
  usgn[state.JEU] = -1.0;
  usgn[state.JEW] = -1.0;
  usgn[state.JEP] = -1.0;
  usgn[state.JER] = -1.0;
  usgn[state.JEX] = -1.0;
  usgn[state.JEZ] = -1.0;

  const lines = [];
  lines.push('');
  const names = [];
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    names.push(state.DNAME?.[n] ?? state.DNAME?.[n - 1] ?? '');
  }
  lines.push(`     u         w         q        the        v         p         r        phi        x         y         z        psi   |  ${names.join(' ')}`);

  for (let i = 1; i <= NSYS; i += 1) {
    const row = [];
    for (let j = 1; j <= NSYS; j += 1) {
      row.push(f32(ASYS[idx2(i, j, state.JEMAX + 1)] * usgn[i] * usgn[j]));
    }
    for (let n = 1; n <= state.NCONTROL; n += 1) {
      row.push(f32(BSYS[idx2(i, n, state.JEMAX + 1)] * usgn[i]));
    }
    lines.push(row.map((v) => v.toExponential(4)).join(' '));
  }
  return lines.join('\n');
}

export function EIGSOL() {
  return { KEIG: 0 };
}

export function MODE() {
  return { LMATCH: false };
}

export function EIGLST() {}
export function EIGOUT() {}
export function EIGINP() {}
export function RUNLST() {}
export function PARMOD() {}
