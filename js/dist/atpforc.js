// Port of AVL atpforc.f (TPFORC, PGMAT) with float32 math for numerical fidelity.

const f32 = Math.fround;

// 3-component arrays use 1-based strip index with stride 3.
function idx3(r, c) {
  return r + 3 * c;
}

// Geometry arrays are Fortran 1-based with stride 4 (x/y/z).
function idx4(r, c) {
  return (r + 1) + 4 * c;
}

export function PGMAT(MACH, ALFA, BETA, P = new Float32Array(9), P_M = new Float32Array(9), P_A = new Float32Array(9), P_B = new Float32Array(9)) {
  const binv = f32(1.0 / Math.sqrt(f32(1.0 - f32(MACH * MACH))));
  const bi_m = f32(f32(MACH) * f32(binv * binv * binv));

  const sina = f32(Math.sin(f32(ALFA)));
  const cosa = f32(Math.cos(f32(ALFA)));
  const sinb = f32(Math.sin(f32(BETA)));
  const cosb = f32(Math.cos(f32(BETA)));

  P[idx3(0, 0)] = f32(cosa * cosb * binv);
  P[idx3(0, 1)] = f32(-sinb * binv);
  P[idx3(0, 2)] = f32(sina * cosb * binv);

  P[idx3(1, 0)] = f32(cosa * sinb);
  P[idx3(1, 1)] = cosb;
  P[idx3(1, 2)] = f32(sina * sinb);

  P[idx3(2, 0)] = f32(-sina);
  P[idx3(2, 1)] = 0.0;
  P[idx3(2, 2)] = cosa;

  P_M[idx3(0, 0)] = f32(cosa * cosb * bi_m);
  P_M[idx3(0, 1)] = f32(-sinb * bi_m);
  P_M[idx3(0, 2)] = f32(sina * cosb * bi_m);
  P_M[idx3(1, 0)] = 0.0;
  P_M[idx3(1, 1)] = 0.0;
  P_M[idx3(1, 2)] = 0.0;
  P_M[idx3(2, 0)] = 0.0;
  P_M[idx3(2, 1)] = 0.0;
  P_M[idx3(2, 2)] = 0.0;

  P_A[idx3(0, 0)] = f32(-sina * cosb * binv);
  P_A[idx3(0, 1)] = 0.0;
  P_A[idx3(0, 2)] = f32(cosa * cosb * binv);
  P_A[idx3(1, 0)] = f32(-sina * sinb);
  P_A[idx3(1, 1)] = 0.0;
  P_A[idx3(1, 2)] = f32(cosa * sinb);
  P_A[idx3(2, 0)] = f32(-cosa);
  P_A[idx3(2, 1)] = 0.0;
  P_A[idx3(2, 2)] = f32(-sina);

  P_B[idx3(0, 0)] = f32(-cosa * sinb * binv);
  P_B[idx3(0, 1)] = f32(-cosb * binv);
  P_B[idx3(0, 2)] = f32(-sina * sinb * binv);
  P_B[idx3(1, 0)] = f32(cosa * cosb);
  P_B[idx3(1, 1)] = f32(-sinb);
  P_B[idx3(1, 2)] = f32(sina * cosb);
  P_B[idx3(2, 0)] = 0.0;
  P_B[idx3(2, 1)] = 0.0;
  P_B[idx3(2, 2)] = 0.0;

  return { P, P_M, P_A, P_B };
}

export function TPFORC(state) {
  const {
    PI,
    AMACH,
    YSYM,
    ZSYM,
    IYSYM,
    IZSYM,
    VRCOREC,
    VRCOREW,
    NSTRIP,
    NUMAX,
    NCONTROL,
    NDESIGN,
    SREF,
    BREF,
    IJFRST,
    NVSTRP,
    GAM,
    GAM_U,
    GAM_D,
    GAM_G,
    RV1,
    RV2,
    RC,
    CHORD,
    LSSURF,
    LNCOMP,
    LFLOAD,
    LSTRIPOFF,
  } = state;

  const HPI = f32(1.0 / (2.0 * PI));

  const P = new Float32Array(9);
  const P_M = new Float32Array(9);
  const P_A = new Float32Array(9);
  const P_B = new Float32Array(9);
  PGMAT(AMACH, 0.0, 0.0, P, P_M, P_A, P_B);

  const YOFF = f32(2.0 * YSYM);
  const ZOFF = f32(2.0 * ZSYM);

  const GAMS = new Float32Array(NSTRIP + 1);
  const GAMS_U = new Float32Array((NSTRIP + 1) * NUMAX);
  const GAMS_D = new Float32Array((NSTRIP + 1) * Math.max(1, NCONTROL));
  const GAMS_G = new Float32Array((NSTRIP + 1) * Math.max(1, NDESIGN));

  const RT1 = new Float32Array(3 * (NSTRIP + 1));
  const RT2 = new Float32Array(3 * (NSTRIP + 1));
  const RTC = new Float32Array(3 * (NSTRIP + 1));

  for (let jc = 1; jc <= NSTRIP; jc += 1) {
    GAMS[jc] = 0.0;
    for (let n = 0; n < NUMAX; n += 1) {
      GAMS_U[jc * NUMAX + n] = 0.0;
    }
    for (let n = 0; n < NCONTROL; n += 1) {
      GAMS_D[jc * NCONTROL + n] = 0.0;
    }
    for (let n = 0; n < NDESIGN; n += 1) {
      GAMS_G[jc * NDESIGN + n] = 0.0;
    }

    const i1 = IJFRST[jc];
    const nvc = NVSTRP[jc];
    for (let ii = 0; ii < nvc; ii += 1) {
      const i = i1 + ii;
      GAMS[jc] = f32(GAMS[jc] + GAM[i]);
      for (let n = 0; n < NUMAX; n += 1) {
        GAMS_U[jc * NUMAX + n] = f32(GAMS_U[jc * NUMAX + n] + GAM_U[i * NUMAX + n]);
      }
      for (let n = 0; n < NCONTROL; n += 1) {
        GAMS_D[jc * NCONTROL + n] = f32(GAMS_D[jc * NCONTROL + n] + GAM_D[i * NCONTROL + n]);
      }
      for (let n = 0; n < NDESIGN; n += 1) {
        GAMS_G[jc * NDESIGN + n] = f32(GAMS_G[jc * NDESIGN + n] + GAM_G[i * NDESIGN + n]);
      }
    }
  }

  for (let jc = 1; jc <= NSTRIP; jc += 1) {
    const ic = IJFRST[jc] + NVSTRP[jc] - 1;
    for (let k = 0; k < 3; k += 1) {
      RT1[idx3(k, jc)] = f32(
        P[idx3(k, 0)] * RV1[idx4(0, ic)]
        + P[idx3(k, 1)] * RV1[idx4(1, ic)]
        + P[idx3(k, 2)] * RV1[idx4(2, ic)]
      );
      RT2[idx3(k, jc)] = f32(
        P[idx3(k, 0)] * RV2[idx4(0, ic)]
        + P[idx3(k, 1)] * RV2[idx4(1, ic)]
        + P[idx3(k, 2)] * RV2[idx4(2, ic)]
      );
      RTC[idx3(k, jc)] = f32(
        P[idx3(k, 0)] * RC[idx4(0, ic)]
        + P[idx3(k, 1)] * RC[idx4(1, ic)]
        + P[idx3(k, 2)] * RC[idx4(2, ic)]
      );
    }
  }

  let CLFF = 0.0;
  let CYFF = 0.0;
  let CDFF = 0.0;
  const CLFF_U = new Float32Array(NUMAX);
  const CYFF_U = new Float32Array(NUMAX);
  const CDFF_U = new Float32Array(NUMAX);
  const CLFF_D = new Float32Array(NCONTROL);
  const CYFF_D = new Float32Array(NCONTROL);
  const CDFF_D = new Float32Array(NCONTROL);
  const CLFF_G = new Float32Array(NDESIGN);
  const CYFF_G = new Float32Array(NDESIGN);
  const CDFF_G = new Float32Array(NDESIGN);
  const DWWAKE = new Float32Array(NSTRIP + 1);

  for (let jc = 1; jc <= NSTRIP; jc += 1) {
    if (LSTRIPOFF[jc]) {
      DWWAKE[jc] = 0.0;
      continue;
    }
    const dxt = f32(RT2[idx3(0, jc)] - RT1[idx3(0, jc)]);
    const dyt = f32(RT2[idx3(1, jc)] - RT1[idx3(1, jc)]);
    const dzt = f32(RT2[idx3(2, jc)] - RT1[idx3(2, jc)]);
    const dst = f32(Math.sqrt(f32(f32(dyt * dyt) + f32(dzt * dzt))));

    const ny = f32(-dzt / dst);
    const nz = f32(dyt / dst);
    const ycntr = RTC[idx3(1, jc)];
    const zcntr = RTC[idx3(2, jc)];

    let vy = 0.0;
    let vz = 0.0;
    const vy_u = new Float32Array(NUMAX);
    const vz_u = new Float32Array(NUMAX);
    const vy_d = new Float32Array(NCONTROL);
    const vz_d = new Float32Array(NCONTROL);
    const vy_g = new Float32Array(NDESIGN);
    const vz_g = new Float32Array(NDESIGN);

    for (let jv = 1; jv <= NSTRIP; jv += 1) {
      if (LSTRIPOFF[jv]) continue;
      const dsy = f32(RT2[idx3(1, jv)] - RT1[idx3(1, jv)]);
      const dsz = f32(RT2[idx3(2, jv)] - RT1[idx3(2, jv)]);
      const dsyz = f32(Math.sqrt(f32(f32(dsy * dsy) + f32(dsz * dsz))));

      let rcore = 0.0;
      if (LNCOMP[LSSURF[jc]] === LNCOMP[LSSURF[jv]]) {
        rcore = 0.0;
      } else {
        rcore = f32(Math.max(f32(VRCOREC * CHORD[jv]), f32(VRCOREW * dsyz)));
      }

      const dy1 = f32(ycntr - RT1[idx3(1, jv)]);
      const dy2 = f32(ycntr - RT2[idx3(1, jv)]);
      const dz1 = f32(zcntr - RT1[idx3(2, jv)]);
      const dz2 = f32(zcntr - RT2[idx3(2, jv)]);

      const rsq1 = f32(Math.sqrt(f32(f32(f32(dy1 * dy1) + f32(dz1 * dz1)) ** 2 + f32(rcore * rcore * rcore * rcore))));
      const rsq2 = f32(Math.sqrt(f32(f32(f32(dy2 * dy2) + f32(dz2 * dz2)) ** 2 + f32(rcore * rcore * rcore * rcore))));

      vy = f32(vy + f32(HPI * GAMS[jv] * f32(f32(dz1 / rsq1) - f32(dz2 / rsq2))));
      vz = f32(vz + f32(HPI * GAMS[jv] * f32(f32(-dy1 / rsq1) + f32(dy2 / rsq2))));
      for (let n = 0; n < NUMAX; n += 1) {
        vy_u[n] = f32(vy_u[n] + f32(HPI * GAMS_U[jv * NUMAX + n] * f32(f32(dz1 / rsq1) - f32(dz2 / rsq2))));
        vz_u[n] = f32(vz_u[n] + f32(HPI * GAMS_U[jv * NUMAX + n] * f32(f32(-dy1 / rsq1) + f32(dy2 / rsq2))));
      }
      for (let n = 0; n < NCONTROL; n += 1) {
        vy_d[n] = f32(vy_d[n] + f32(HPI * GAMS_D[jv * NCONTROL + n] * f32(f32(dz1 / rsq1) - f32(dz2 / rsq2))));
        vz_d[n] = f32(vz_d[n] + f32(HPI * GAMS_D[jv * NCONTROL + n] * f32(f32(-dy1 / rsq1) + f32(dy2 / rsq2))));
      }
      for (let n = 0; n < NDESIGN; n += 1) {
        vy_g[n] = f32(vy_g[n] + f32(HPI * GAMS_G[jv * NDESIGN + n] * f32(f32(dz1 / rsq1) - f32(dz2 / rsq2))));
        vz_g[n] = f32(vz_g[n] + f32(HPI * GAMS_G[jv * NDESIGN + n] * f32(f32(-dy1 / rsq1) + f32(dy2 / rsq2))));
      }

      if (IZSYM !== 0) {
        const dy1z = f32(ycntr - RT1[idx3(1, jv)]);
        const dy2z = f32(ycntr - RT2[idx3(1, jv)]);
        const dz1z = f32(zcntr - f32(ZOFF - RT1[idx3(2, jv)]));
        const dz2z = f32(zcntr - f32(ZOFF - RT2[idx3(2, jv)]));
        const rsq1z = f32(f32(dy1z * dy1z) + f32(dz1z * dz1z));
        const rsq2z = f32(f32(dy2z * dy2z) + f32(dz2z * dz2z));
        vy = f32(vy - f32(HPI * GAMS[jv] * f32(f32(dz1z / rsq1z) - f32(dz2z / rsq2z)) * IZSYM));
        vz = f32(vz - f32(HPI * GAMS[jv] * f32(f32(-dy1z / rsq1z) + f32(dy2z / rsq2z)) * IZSYM));
        for (let n = 0; n < NUMAX; n += 1) {
          vy_u[n] = f32(vy_u[n] - f32(HPI * GAMS_U[jv * NUMAX + n] * f32(f32(dz1z / rsq1z) - f32(dz2z / rsq2z)) * IZSYM));
          vz_u[n] = f32(vz_u[n] - f32(HPI * GAMS_U[jv * NUMAX + n] * f32(f32(-dy1z / rsq1z) + f32(dy2z / rsq2z)) * IZSYM));
        }
        for (let n = 0; n < NCONTROL; n += 1) {
          vy_d[n] = f32(vy_d[n] - f32(HPI * GAMS_D[jv * NCONTROL + n] * f32(f32(dz1z / rsq1z) - f32(dz2z / rsq2z)) * IZSYM));
          vz_d[n] = f32(vz_d[n] - f32(HPI * GAMS_D[jv * NCONTROL + n] * f32(f32(-dy1z / rsq1z) + f32(dy2z / rsq2z)) * IZSYM));
        }
        for (let n = 0; n < NDESIGN; n += 1) {
          vy_g[n] = f32(vy_g[n] - f32(HPI * GAMS_G[jv * NDESIGN + n] * f32(f32(dz1z / rsq1z) - f32(dz2z / rsq2z)) * IZSYM));
          vz_g[n] = f32(vz_g[n] - f32(HPI * GAMS_G[jv * NDESIGN + n] * f32(f32(-dy1z / rsq1z) + f32(dy2z / rsq2z)) * IZSYM));
        }
      }

      if (IYSYM !== 0) {
        const dy1y = f32(ycntr - f32(YOFF - RT1[idx3(1, jv)]));
        const dy2y = f32(ycntr - f32(YOFF - RT2[idx3(1, jv)]));
        const dz1y = f32(zcntr - RT1[idx3(2, jv)]);
        const dz2y = f32(zcntr - RT2[idx3(2, jv)]);
        const rsq1y = f32(f32(dy1y * dy1y) + f32(dz1y * dz1y));
        const rsq2y = f32(f32(dy2y * dy2y) + f32(dz2y * dz2y));
        vy = f32(vy - f32(HPI * GAMS[jv] * f32(f32(dz1y / rsq1y) - f32(dz2y / rsq2y)) * IYSYM));
        vz = f32(vz - f32(HPI * GAMS[jv] * f32(f32(-dy1y / rsq1y) + f32(dy2y / rsq2y)) * IYSYM));
        for (let n = 0; n < NUMAX; n += 1) {
          vy_u[n] = f32(vy_u[n] - f32(HPI * GAMS_U[jv * NUMAX + n] * f32(f32(dz1y / rsq1y) - f32(dz2y / rsq2y)) * IYSYM));
          vz_u[n] = f32(vz_u[n] - f32(HPI * GAMS_U[jv * NUMAX + n] * f32(f32(-dy1y / rsq1y) + f32(dy2y / rsq2y)) * IYSYM));
        }
        for (let n = 0; n < NCONTROL; n += 1) {
          vy_d[n] = f32(vy_d[n] - f32(HPI * GAMS_D[jv * NCONTROL + n] * f32(f32(dz1y / rsq1y) - f32(dz2y / rsq2y)) * IYSYM));
          vz_d[n] = f32(vz_d[n] - f32(HPI * GAMS_D[jv * NCONTROL + n] * f32(f32(-dy1y / rsq1y) + f32(dy2y / rsq2y)) * IYSYM));
        }
        for (let n = 0; n < NDESIGN; n += 1) {
          vy_g[n] = f32(vy_g[n] - f32(HPI * GAMS_G[jv * NDESIGN + n] * f32(f32(dz1y / rsq1y) - f32(dz2y / rsq2y)) * IYSYM));
          vz_g[n] = f32(vz_g[n] - f32(HPI * GAMS_G[jv * NDESIGN + n] * f32(f32(-dy1y / rsq1y) + f32(dy2y / rsq2y)) * IYSYM));
        }

        if (IZSYM !== 0) {
          const dy1yz = f32(ycntr - f32(YOFF - RT1[idx3(1, jv)]));
          const dy2yz = f32(ycntr - f32(YOFF - RT2[idx3(1, jv)]));
          const dz1yz = f32(zcntr - f32(ZOFF - RT1[idx3(2, jv)]));
          const dz2yz = f32(zcntr - f32(ZOFF - RT2[idx3(2, jv)]));
          const rsq1yz = f32(f32(dy1yz * dy1yz) + f32(dz1yz * dz1yz));
          const rsq2yz = f32(f32(dy2yz * dy2yz) + f32(dz2yz * dz2yz));
          vy = f32(vy + f32(HPI * GAMS[jv] * f32(f32(dz1yz / rsq1yz) - f32(dz2yz / rsq2yz)) * IYSYM * IZSYM));
          vz = f32(vz + f32(HPI * GAMS[jv] * f32(f32(-dy1yz / rsq1yz) + f32(dy2yz / rsq2yz)) * IYSYM * IZSYM));
          for (let n = 0; n < NUMAX; n += 1) {
            vy_u[n] = f32(vy_u[n] - f32(HPI * GAMS_U[jv * NUMAX + n] * f32(f32(dz1yz / rsq1yz) - f32(dz2yz / rsq2yz)) * IYSYM * IZSYM));
            vz_u[n] = f32(vz_u[n] - f32(HPI * GAMS_U[jv * NUMAX + n] * f32(f32(-dy1yz / rsq1yz) + f32(dy2yz / rsq2yz)) * IYSYM * IZSYM));
          }
          for (let n = 0; n < NCONTROL; n += 1) {
            vy_d[n] = f32(vy_d[n] - f32(HPI * GAMS_D[jv * NCONTROL + n] * f32(f32(dz1yz / rsq1yz) - f32(dz2yz / rsq2yz)) * IYSYM * IZSYM));
            vz_d[n] = f32(vz_d[n] - f32(HPI * GAMS_D[jv * NCONTROL + n] * f32(f32(-dy1yz / rsq1yz) + f32(dy2yz / rsq2yz)) * IYSYM * IZSYM));
          }
          for (let n = 0; n < NDESIGN; n += 1) {
            vy_g[n] = f32(vy_g[n] - f32(HPI * GAMS_G[jv * NDESIGN + n] * f32(f32(dz1yz / rsq1yz) - f32(dz2yz / rsq2yz)) * IYSYM * IZSYM));
            vz_g[n] = f32(vz_g[n] - f32(HPI * GAMS_G[jv * NDESIGN + n] * f32(f32(-dy1yz / rsq1yz) + f32(dy2yz / rsq2yz)) * IYSYM * IZSYM));
          }
        }
      }
    }

    DWWAKE[jc] = f32(-f32(ny * vy + nz * vz));

    const isurf = LSSURF[jc];
    if (LFLOAD[isurf]) {
      CLFF = f32(CLFF + f32(2.0 * GAMS[jc] * dyt / SREF));
      CYFF = f32(CYFF - f32(2.0 * GAMS[jc] * dzt / SREF));
      CDFF = f32(CDFF + f32(GAMS[jc] * f32(f32(dzt * vy) - f32(dyt * vz)) / SREF));
      for (let n = 0; n < NUMAX; n += 1) {
        CLFF_U[n] = f32(CLFF_U[n] + f32(2.0 * GAMS_U[jc * NUMAX + n] * dyt / SREF));
        CYFF_U[n] = f32(CYFF_U[n] - f32(2.0 * GAMS_U[jc * NUMAX + n] * dzt / SREF));
        CDFF_U[n] = f32(CDFF_U[n] + f32(
          (f32(GAMS_U[jc * NUMAX + n] * f32(f32(dzt * vy) - f32(dyt * vz)))
            + f32(GAMS[jc] * f32(f32(dzt * vy_u[n]) - f32(dyt * vz_u[n])))) / SREF
        ));
      }
      for (let n = 0; n < NCONTROL; n += 1) {
        CLFF_D[n] = f32(CLFF_D[n] + f32(2.0 * GAMS_D[jc * NCONTROL + n] * dyt / SREF));
        CYFF_D[n] = f32(CYFF_D[n] - f32(2.0 * GAMS_D[jc * NCONTROL + n] * dzt / SREF));
        CDFF_D[n] = f32(CDFF_D[n] + f32(
          (f32(GAMS_D[jc * NCONTROL + n] * f32(f32(dzt * vy) - f32(dyt * vz)))
            + f32(GAMS[jc] * f32(f32(dzt * vy_d[n]) - f32(dyt * vz_d[n])))) / SREF
        ));
      }
      for (let n = 0; n < NDESIGN; n += 1) {
        CLFF_G[n] = f32(CLFF_G[n] + f32(2.0 * GAMS_G[jc * NDESIGN + n] * dyt / SREF));
        CYFF_G[n] = f32(CYFF_G[n] - f32(2.0 * GAMS_G[jc * NDESIGN + n] * dzt / SREF));
        CDFF_G[n] = f32(CDFF_G[n] + f32(
          (f32(GAMS_G[jc * NDESIGN + n] * f32(f32(dzt * vy) - f32(dyt * vz)))
            + f32(GAMS[jc] * f32(f32(dzt * vy_g[n]) - f32(dyt * vz_g[n])))) / SREF
        ));
      }
    }
  }

  if (IYSYM === 1) {
    CLFF = f32(2.0 * CLFF);
    CYFF = 0.0;
    CDFF = f32(2.0 * CDFF);
    for (let n = 0; n < NUMAX; n += 1) {
      CLFF_U[n] = f32(2.0 * CLFF_U[n]);
      CYFF_U[n] = 0.0;
      CDFF_U[n] = f32(2.0 * CDFF_U[n]);
    }
    for (let n = 0; n < NCONTROL; n += 1) {
      CLFF_D[n] = f32(2.0 * CLFF_D[n]);
      CYFF_D[n] = 0.0;
      CDFF_D[n] = f32(2.0 * CDFF_D[n]);
    }
    for (let n = 0; n < NDESIGN; n += 1) {
      CLFF_G[n] = f32(2.0 * CLFF_G[n]);
      CYFF_G[n] = 0.0;
      CDFF_G[n] = f32(2.0 * CDFF_G[n]);
    }
  }

  const AR = f32(BREF * BREF / SREF);
  let SPANEF = 0.0;
  let SPANEF_A = 0.0;
  const SPANEF_U = new Float32Array(NUMAX);
  const SPANEF_D = new Float32Array(NCONTROL);
  const SPANEF_G = new Float32Array(NDESIGN);

  if (CDFF === 0.0) {
    SPANEF = 0.0;
    SPANEF_A = 0.0;
    for (let n = 0; n < NUMAX; n += 1) {
      SPANEF_U[n] = 0.0;
    }
    for (let n = 0; n < NCONTROL; n += 1) {
      SPANEF_D[n] = 0.0;
    }
    for (let n = 0; n < NDESIGN; n += 1) {
      SPANEF_G[n] = 0.0;
    }
  } else {
    SPANEF = f32(f32(CLFF * CLFF + CYFF * CYFF) / f32(PI * AR * CDFF));
    const SPANEF_CL = f32(f32(2.0 * CLFF) / f32(PI * AR * CDFF));
    const SPANEF_CY = f32(f32(2.0 * CYFF) / f32(PI * AR * CDFF));
    const SPANEF_CD = f32(-SPANEF / CDFF);
    SPANEF_A = 0.0;
    for (let n = 0; n < NUMAX; n += 1) {
      SPANEF_U[n] = f32(SPANEF_CL * CLFF_U[n] + SPANEF_CY * CYFF_U[n] + SPANEF_CD * CDFF_U[n]);
    }
    for (let n = 0; n < NCONTROL; n += 1) {
      SPANEF_D[n] = f32(SPANEF_CL * CLFF_D[n] + SPANEF_CY * CYFF_D[n] + SPANEF_CD * CDFF_D[n]);
    }
    for (let n = 0; n < NDESIGN; n += 1) {
      SPANEF_G[n] = f32(SPANEF_CL * CLFF_G[n] + SPANEF_CY * CYFF_G[n] + SPANEF_CD * CDFF_G[n]);
    }
  }

  state.CLFF = CLFF;
  state.CYFF = CYFF;
  state.CDFF = CDFF;
  state.SPANEF = SPANEF;
  state.SPANEF_A = SPANEF_A;
  state.DWWAKE = DWWAKE;
  state.CLFF_U = CLFF_U;
  state.CYFF_U = CYFF_U;
  state.CDFF_U = CDFF_U;
  state.SPANEF_U = SPANEF_U;
  state.CLFF_D = CLFF_D;
  state.CYFF_D = CYFF_D;
  state.CDFF_D = CDFF_D;
  state.SPANEF_D = SPANEF_D;
  state.CLFF_G = CLFF_G;
  state.CYFF_G = CYFF_G;
  state.CDFF_G = CDFF_G;
  state.SPANEF_G = SPANEF_G;

  return state;
}
