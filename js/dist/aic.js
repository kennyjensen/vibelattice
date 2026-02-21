/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL aic.f with float32 math for numerical fidelity.

const f32 = Math.fround;

function idx3(r, c) {
  return r + 3 * c;
}

function idx3c(r, i, j, dim1) {
  return r + 3 * (i + dim1 * j);
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function idx2f(i, j, dim1) {
  return i + dim1 * j;
}

function idx3f(i, j, k, dim1, dim2) {
  return i + dim1 * (j + dim2 * k);
}

export function CROSS(U, V, W = new Float32Array(3)) {
  const u1 = f32(U[0]);
  const u2 = f32(U[1]);
  const u3 = f32(U[2]);
  const v1 = f32(V[0]);
  const v2 = f32(V[1]);
  const v3 = f32(V[2]);
  W[0] = f32(f32(u2 * v3) - f32(u3 * v2));
  W[1] = f32(f32(u3 * v1) - f32(u1 * v3));
  W[2] = f32(f32(u1 * v2) - f32(u2 * v1));
  return W;
}

export function DOT(U, V) {
  return f32(f32(U[0] * V[0]) + f32(U[1] * V[1]) + f32(U[2] * V[2]));
}

export function VORVELC(X, Y, Z, LBOUND, X1, Y1, Z1, X2, Y2, Z2, BETA, RCORE) {
  const PI4INV = 0.079577472;
  const beta = f32(BETA);

  const A1 = f32(f32(X1 - X) / beta);
  const A2 = f32(Y1 - Y);
  const A3 = f32(Z1 - Z);

  const B1 = f32(f32(X2 - X) / beta);
  const B2 = f32(Y2 - Y);
  const B3 = f32(Z2 - Z);

  const ASQ = f32(f32(A1 * A1) + f32(A2 * A2) + f32(A3 * A3));
  const BSQ = f32(f32(B1 * B1) + f32(B2 * B2) + f32(B3 * B3));

  const AMAG = f32(Math.sqrt(ASQ));
  const BMAG = f32(Math.sqrt(BSQ));

  const RCORE2 = f32(RCORE * RCORE);
  const RCORE4 = f32(RCORE2 * RCORE2);

  let U = 0.0;
  let V = 0.0;
  let W = 0.0;

  if (LBOUND && f32(AMAG * BMAG) !== 0.0) {
    const AXB1 = f32(f32(A2 * B3) - f32(A3 * B2));
    const AXB2 = f32(f32(A3 * B1) - f32(A1 * B3));
    const AXB3 = f32(f32(A1 * B2) - f32(A2 * B1));
    const AXBSQ = f32(f32(AXB1 * AXB1) + f32(AXB2 * AXB2) + f32(AXB3 * AXB3));

    if (AXBSQ !== 0.0) {
      const ADB = f32(f32(A1 * B1) + f32(A2 * B2) + f32(A3 * B3));
      const ALSQ = f32(f32(ASQ + BSQ) - f32(2.0 * ADB));
      const ABMAG = f32(AMAG * BMAG);

      const tNum = f32(
        f32((BSQ - ADB) / Math.sqrt(Math.sqrt(f32(BSQ * BSQ + RCORE4))))
        + f32((ASQ - ADB) / Math.sqrt(Math.sqrt(f32(ASQ * ASQ + RCORE4))))
      );
      const tDen = f32(Math.sqrt(f32(f32(AXBSQ * AXBSQ) + f32(ALSQ * ALSQ * RCORE4))));
      const T = f32(tNum / tDen);

      U = f32(AXB1 * T);
      V = f32(AXB2 * T);
      W = f32(AXB3 * T);
    }
  }

  if (AMAG !== 0.0) {
    const AXISQ = f32(f32(A3 * A3) + f32(A2 * A2));
    const ADX = A1;
    const RSQ = AXISQ;
    const T = f32(-f32(1.0 - f32(ADX / AMAG)) / Math.sqrt(f32(f32(RSQ * RSQ) + RCORE4)));
    V = f32(V + f32(A3 * T));
    W = f32(W - f32(A2 * T));
  }

  if (BMAG !== 0.0) {
    const BXISQ = f32(f32(B3 * B3) + f32(B2 * B2));
    const BDX = B1;
    const RSQ = BXISQ;
    const T = f32(f32(1.0 - f32(BDX / BMAG)) / Math.sqrt(f32(f32(RSQ * RSQ) + RCORE4)));
    V = f32(V + f32(B3 * T));
    W = f32(W - f32(B2 * T));
  }

  U = f32(f32(U * PI4INV) / beta);
  V = f32(V * PI4INV);
  W = f32(W * PI4INV);

  return { U, V, W };
}

export function SRDVELC(X, Y, Z, X1, Y1, Z1, X2, Y2, Z2, BETA, RCORE, UVWS = new Float32Array(3), UVWD = new Float32Array(9)) {
  const PI4INV = 0.079577472;
  const beta = f32(BETA);

  const R1 = new Float32Array(3);
  const R2 = new Float32Array(3);

  R1[0] = f32(f32(X1 - X) / beta);
  R1[1] = f32(Y1 - Y);
  R1[2] = f32(Z1 - Z);

  R2[0] = f32(f32(X2 - X) / beta);
  R2[1] = f32(Y2 - Y);
  R2[2] = f32(Z2 - Z);

  const RCSQ = f32(RCORE * RCORE);

  const R1SQ = f32(f32(R1[0] * R1[0]) + f32(R1[1] * R1[1]) + f32(R1[2] * R1[2]));
  const R2SQ = f32(f32(R2[0] * R2[0]) + f32(R2[1] * R2[1]) + f32(R2[2] * R2[2]));

  const R1SQEPS = f32(R1SQ + RCSQ);
  const R2SQEPS = f32(R2SQ + RCSQ);

  const R1EPS = f32(Math.sqrt(R1SQEPS));
  const R2EPS = f32(Math.sqrt(R2SQEPS));

  const RDR = f32(f32(R1[0] * R2[0]) + f32(R1[1] * R2[1]) + f32(R1[2] * R2[2]));
  const RXR1 = f32(f32(R1[1] * R2[2]) - f32(R1[2] * R2[1]));
  const RXR2 = f32(f32(R1[2] * R2[0]) - f32(R1[0] * R2[2]));
  const RXR3 = f32(f32(R1[0] * R2[1]) - f32(R1[1] * R2[0]));

  const XDX = f32(f32(RXR1 * RXR1) + f32(RXR2 * RXR2) + f32(RXR3 * RXR3));
  const ALL = f32(f32(R1SQ + R2SQ) - f32(2.0 * RDR));
  const DEN = f32(f32(RCSQ * ALL) + XDX);

  const AI1 = f32(f32((RDR + RCSQ) / R1EPS - R2EPS) / DEN);
  const AI2 = f32(f32((RDR + RCSQ) / R2EPS - R1EPS) / DEN);

  for (let k = 0; k < 3; k += 1) {
    const R1K = R1[k];
    const R2K = R2[k];

    UVWS[k] = f32(f32(R1K * AI1) + f32(R2K * AI2));

    const RR1 = f32(
      f32(f32(R1K + R2K) / R1EPS)
      - f32(f32(R1K * (RDR + RCSQ)) / f32(R1EPS * R1EPS * R1EPS))
      - f32(R2K / R2EPS)
    );

    const RR2 = f32(
      f32(f32(R1K + R2K) / R2EPS)
      - f32(f32(R2K * (RDR + RCSQ)) / f32(R2EPS * R2EPS * R2EPS))
      - f32(R1K / R1EPS)
    );

    const RRT = f32(
      f32(2.0 * R1K * (R2SQ - RDR))
      + f32(2.0 * R2K * (R1SQ - RDR))
    );

    const AJ1 = f32(f32(RR1 - f32(AI1 * RRT)) / DEN);
    const AJ2 = f32(f32(RR2 - f32(AI2 * RRT)) / DEN);

    for (let j = 0; j < 3; j += 1) {
      UVWD[idx3(k, j)] = f32(-f32(AJ1 * R1[j]) - f32(AJ2 * R2[j]));
    }

    UVWD[idx3(k, k)] = f32(f32(UVWD[idx3(k, k)] - AI1) - AI2);
  }

  UVWS[0] = f32(f32(UVWS[0] * PI4INV) / beta);
  UVWS[1] = f32(UVWS[1] * PI4INV);
  UVWS[2] = f32(UVWS[2] * PI4INV);

  for (let l = 0; l < 3; l += 1) {
    UVWD[idx3(0, l)] = f32(f32(UVWD[idx3(0, l)] * PI4INV) / beta);
    UVWD[idx3(1, l)] = f32(UVWD[idx3(1, l)] * PI4INV);
    UVWD[idx3(2, l)] = f32(UVWD[idx3(2, l)] * PI4INV);
  }

  return { UVWS, UVWD };
}

export function VVOR(BETM, IYSYM, YSYM, IZSYM, ZSYM,
  VRCOREC, VRCOREW,
  NV, RV1, RV2, NCOMPV, CHORDV,
  NC, RC, NCOMPC, LVTEST,
  WC_GAM = new Float32Array(4 * Math.max(1, NV) * Math.max(1, NC)), NCDIM = NC) {
  const FYSYM = f32(IYSYM);
  const FZSYM = f32(IZSYM);
  const dim2 = NCDIM + 1;

  for (let i = 1; i <= NC; i += 1) {
    const X = f32(RC[idx2f(1, i, 4)]);
    const Y = f32(RC[idx2f(2, i, 4)]);
    const Z = f32(RC[idx2f(3, i, 4)]);

    for (let j = 1; j <= NV; j += 1) {
      const dsY = f32(RV2[idx2f(2, j, 4)] - RV1[idx2f(2, j, 4)]);
      const dsZ = f32(RV2[idx2f(3, j, 4)] - RV1[idx2f(3, j, 4)]);
      const DSYZ = f32(Math.sqrt(f32(f32(dsY * dsY) + f32(dsZ * dsZ))));
      if (!Number.isFinite(DSYZ) || DSYZ === 0.0) {
        WC_GAM[idx3f(1, i, j, 4, dim2)] = 0.0;
        WC_GAM[idx3f(2, i, j, 4, dim2)] = 0.0;
        WC_GAM[idx3f(3, i, j, 4, dim2)] = 0.0;
        continue;
      }
      let RCORE = f32(0.0001 * DSYZ);

      if (NC === NV) {
        if (NCOMPC[i] !== NCOMPV[j]) {
          const rc1 = f32(VRCOREC * CHORDV[j]);
          const rc2 = f32(VRCOREW * DSYZ);
          RCORE = f32(Math.max(rc1, rc2));
        }
      }

      let U = 0.0;
      let V = 0.0;
      let W = 0.0;

      let UI = 0.0;
      let VI = 0.0;
      let WI = 0.0;

      const YOFF = f32(2.0 * YSYM);
      const ZOFF = f32(2.0 * ZSYM);

      const lboundReal = !(LVTEST && i === j);
      {
        const res = VORVELC(
          X, Y, Z, lboundReal,
          RV1[idx2f(1, j, 4)], RV1[idx2f(2, j, 4)], RV1[idx2f(3, j, 4)],
          RV2[idx2f(1, j, 4)], RV2[idx2f(2, j, 4)], RV2[idx2f(3, j, 4)],
          BETM, RCORE,
        );
        U = res.U;
        V = res.V;
        W = res.W;
      }

      if (IYSYM !== 0) {
        let lboundImg = true;
        if (IYSYM === 1) {
          const XAVE = f32(0.5 * f32(RV1[idx2f(1, j, 4)] + RV2[idx2f(1, j, 4)]));
          const YAVE = f32(YOFF - 0.5 * f32(RV1[idx2f(2, j, 4)] + RV2[idx2f(2, j, 4)]));
          const ZAVE = f32(0.5 * f32(RV1[idx2f(3, j, 4)] + RV2[idx2f(3, j, 4)]));
          if (X === XAVE && Y === YAVE && Z === ZAVE) {
            lboundImg = false;
          }
        }
        const res = VORVELC(
          X, Y, Z, lboundImg,
          RV2[idx2f(1, j, 4)], f32(YOFF - RV2[idx2f(2, j, 4)]), RV2[idx2f(3, j, 4)],
          RV1[idx2f(1, j, 4)], f32(YOFF - RV1[idx2f(2, j, 4)]), RV1[idx2f(3, j, 4)],
          BETM, RCORE,
        );
        UI = f32(res.U * FYSYM);
        VI = f32(res.V * FYSYM);
        WI = f32(res.W * FYSYM);
      }

      if (IZSYM !== 0) {
        const resZ = VORVELC(
          X, Y, Z, true,
          RV2[idx2f(1, j, 4)], RV2[idx2f(2, j, 4)], f32(ZOFF - RV2[idx2f(3, j, 4)]),
          RV1[idx2f(1, j, 4)], RV1[idx2f(2, j, 4)], f32(ZOFF - RV1[idx2f(3, j, 4)]),
          BETM, RCORE,
        );
        U = f32(U + f32(resZ.U * FZSYM));
        V = f32(V + f32(resZ.V * FZSYM));
        W = f32(W + f32(resZ.W * FZSYM));

        if (IYSYM !== 0) {
          const resYZ = VORVELC(
            X, Y, Z, true,
            RV1[idx2f(1, j, 4)], f32(YOFF - RV1[idx2f(2, j, 4)]), f32(ZOFF - RV1[idx2f(3, j, 4)]),
            RV2[idx2f(1, j, 4)], f32(YOFF - RV2[idx2f(2, j, 4)]), f32(ZOFF - RV2[idx2f(3, j, 4)]),
            BETM, RCORE,
          );
          UI = f32(UI + f32(resYZ.U * FYSYM * FZSYM));
          VI = f32(VI + f32(resYZ.V * FYSYM * FZSYM));
          WI = f32(WI + f32(resYZ.W * FYSYM * FZSYM));
        }
      }

      const US = f32(U + UI);
      const VS = f32(V + VI);
      const WS = f32(W + WI);

      WC_GAM[idx3f(1, i, j, 4, dim2)] = Number.isFinite(US) ? US : 0.0;
      WC_GAM[idx3f(2, i, j, 4, dim2)] = Number.isFinite(VS) ? VS : 0.0;
      WC_GAM[idx3f(3, i, j, 4, dim2)] = Number.isFinite(WS) ? WS : 0.0;
    }
  }

  return WC_GAM;
}

export function VSRD(BETM, IYSYM, YSYM, IZSYM, ZSYM, SRCORE,
  NBODY, LFRST, NLDIM,
  NL, RL, RADL,
  NU, SRC_U, DBL_U,
  NC, RC,
  WC_U = new Float32Array(3 * Math.max(1, NC) * Math.max(1, NU)), NCDIM = NC) {
  const FYSYM = f32(IYSYM);
  const FZSYM = f32(IZSYM);
  const YOFF = f32(2.0 * YSYM);
  const ZOFF = f32(2.0 * ZSYM);
  const stateLayout = RL.length >= (4 * NLDIM);
  const rcStateLayout = RC.length >= (4 * NCDIM);
  const srcStateLayout = SRC_U.length >= (NLDIM * (NU + 1));
  const dblStateLayout = DBL_U.length >= (4 * NLDIM * (NU + 1));
  const wcStateLayout = WC_U.length >= (4 * NCDIM * (NU + 1));
  const rlv = (r, l) => (stateLayout ? RL[(r + 1) + 4 * l] : RL[idx3(r, l)]);
  const rcv = (r, i0) => (rcStateLayout ? RC[(r + 1) + 4 * (i0 + 1)] : RC[idx3(r, i0)]);
  const rad = (l) => (stateLayout ? RADL[l] : RADL[l]);
  const srcu = (l, iu0) => (srcStateLayout ? SRC_U[idx2(l, iu0 + 1, NLDIM)] : SRC_U[idx2(l, iu0, NLDIM)]);
  const dbu = (k0, l, iu0) => (
    dblStateLayout
      ? DBL_U[idx3f(k0 + 1, l, iu0 + 1, 4, NLDIM)]
      : DBL_U[idx3c(k0, l, iu0, NLDIM)]
  );
  const wcGet = (k0, i0, iu0) => (
    wcStateLayout
      ? WC_U[idx3f(k0 + 1, i0 + 1, iu0 + 1, 4, NCDIM)]
      : WC_U[idx3c(k0, i0, iu0, NCDIM)]
  );
  const wcSet = (k0, i0, iu0, val) => {
    if (wcStateLayout) {
      WC_U[idx3f(k0 + 1, i0 + 1, iu0 + 1, 4, NCDIM)] = val;
    } else {
      WC_U[idx3c(k0, i0, iu0, NCDIM)] = val;
    }
  };

  for (let i = 0; i < NC; i += 1) {
    for (let iu = 0; iu < NU; iu += 1) {
      wcSet(0, i, iu, 0.0);
      wcSet(1, i, iu, 0.0);
      wcSet(2, i, iu, 0.0);
    }
  }

  for (let ib = 0; ib < NBODY; ib += 1) {
    for (let ilseg = 0; ilseg < NL[ib] - 1; ilseg += 1) {
      const L1 = stateLayout ? (LFRST[ib] + ilseg) : (LFRST[ib] - 1 + ilseg);
      const L2 = L1 + 1;
      const L = L1;

      const RAVG = f32(Math.sqrt(0.5 * (f32(rad(L2) * rad(L2)) + f32(rad(L1) * rad(L1)))));
      const dx = f32(rlv(0, L2) - rlv(0, L1));
      const dy = f32(rlv(1, L2) - rlv(1, L1));
      const dz = f32(rlv(2, L2) - rlv(2, L1));
      const RLAVG = f32(Math.sqrt(f32(f32(dx * dx) + f32(dy * dy) + f32(dz * dz))));

      let RCORE = 0.0;
      if (SRCORE > 0) {
        RCORE = f32(SRCORE * RAVG);
      } else {
        RCORE = f32(SRCORE * RLAVG);
      }

      for (let i = 0; i < NC; i += 1) {
        const res = SRDVELC(
          rcv(0, i), rcv(1, i), rcv(2, i),
          rlv(0, L1), rlv(1, L1), rlv(2, L1),
          rlv(0, L2), rlv(1, L2), rlv(2, L2),
          BETM, RCORE,
        );
        const VSRC = res.UVWS;
        const VDBL = res.UVWD;

        for (let iu = 0; iu < NU; iu += 1) {
          for (let k = 0; k < 3; k += 1) {
            const contrib = f32(
              f32(VSRC[k] * srcu(L, iu))
              + f32(VDBL[idx3(k, 0)] * dbu(0, L, iu))
              + f32(VDBL[idx3(k, 1)] * dbu(1, L, iu))
              + f32(VDBL[idx3(k, 2)] * dbu(2, L, iu))
            );
            wcSet(k, i, iu, f32(wcGet(k, i, iu) + contrib));
          }
        }

        if (IYSYM !== 0) {
          const resY = SRDVELC(
            rcv(0, i), rcv(1, i), rcv(2, i),
            rlv(0, L1), f32(YOFF - rlv(1, L1)), rlv(2, L1),
            rlv(0, L2), f32(YOFF - rlv(1, L2)), rlv(2, L2),
            BETM, RCORE,
          );
          const VSRC = resY.UVWS;
          const VDBL = resY.UVWD;
          for (let iu = 0; iu < NU; iu += 1) {
            for (let k = 0; k < 3; k += 1) {
                const contrib = f32(
                f32(VSRC[k] * srcu(L, iu))
                + f32(VDBL[idx3(k, 0)] * dbu(0, L, iu))
                - f32(VDBL[idx3(k, 1)] * dbu(1, L, iu))
                + f32(VDBL[idx3(k, 2)] * dbu(2, L, iu))
              );
              wcSet(k, i, iu, f32(wcGet(k, i, iu) + f32(contrib * FYSYM)));
            }
          }
        }

        if (IZSYM !== 0) {
          const resZ = SRDVELC(
            rcv(0, i), rcv(1, i), rcv(2, i),
            rlv(0, L1), rlv(1, L1), f32(ZOFF - rlv(2, L1)),
            rlv(0, L2), rlv(1, L2), f32(ZOFF - rlv(2, L2)),
            BETM, RCORE,
          );
          const VSRC = resZ.UVWS;
          const VDBL = resZ.UVWD;
          for (let iu = 0; iu < NU; iu += 1) {
            for (let k = 0; k < 3; k += 1) {
              const contrib = f32(
                f32(VSRC[k] * srcu(L, iu))
                + f32(VDBL[idx3(k, 0)] * dbu(0, L, iu))
                + f32(VDBL[idx3(k, 1)] * dbu(1, L, iu))
                - f32(VDBL[idx3(k, 2)] * dbu(2, L, iu))
              );
              wcSet(k, i, iu, f32(wcGet(k, i, iu) + f32(contrib * FZSYM)));
            }
          }

          if (IYSYM !== 0) {
            const resYZ = SRDVELC(
              rcv(0, i), rcv(1, i), rcv(2, i),
              rlv(0, L1), f32(YOFF - rlv(1, L1)), f32(ZOFF - rlv(2, L1)),
              rlv(0, L2), f32(YOFF - rlv(1, L2)), f32(ZOFF - rlv(2, L2)),
              BETM, RCORE,
            );
            const VSRC = resYZ.UVWS;
            const VDBL = resYZ.UVWD;
            for (let iu = 0; iu < NU; iu += 1) {
              for (let k = 0; k < 3; k += 1) {
                const contrib = f32(
                  f32(VSRC[k] * srcu(L, iu))
                  + f32(VDBL[idx3(k, 0)] * dbu(0, L, iu))
                  - f32(VDBL[idx3(k, 1)] * dbu(1, L, iu))
                  - f32(VDBL[idx3(k, 2)] * dbu(2, L, iu))
                );
                wcSet(k, i, iu, f32(wcGet(k, i, iu) + f32(contrib * FYSYM * FZSYM)));
              }
            }
          }
        }
      }
    }
  }

  return WC_U;
}

export function SRDSET(BETM, XYZREF, IYSYM,
  NBODY, LFRST, NLDIM,
  NL, RL, RADL,
  SRC_U = new Float32Array(NLDIM * 6), DBL_U = new Float32Array(3 * NLDIM * 6)) {
  const PI = 3.14159265;
  const beta = f32(BETM);
  const stateLayout = RL.length >= (4 * NLDIM);
  const srcStateLayout = SRC_U.length >= (NLDIM * (6 + 1));
  const dblStateLayout = DBL_U.length >= (4 * NLDIM * (6 + 1));
  const rlv = (r, l) => (stateLayout ? RL[(r + 1) + 4 * l] : RL[idx3(r, l)]);
  const rad = (l) => (stateLayout ? RADL[l] : RADL[l]);
  const setSrcU = (l, iu0, val) => {
    if (srcStateLayout) {
      SRC_U[idx2(l, iu0 + 1, NLDIM)] = val;
    } else {
      SRC_U[idx2(l, iu0, NLDIM)] = val;
    }
  };
  const setDblU = (k0, l, iu0, val) => {
    if (dblStateLayout) {
      DBL_U[idx3f(k0 + 1, l, iu0 + 1, 4, NLDIM)] = val;
    } else {
      DBL_U[idx3c(k0, l, iu0, NLDIM)] = val;
    }
  };

  const DRL = new Float32Array(3);
  const ESL = new Float32Array(3);
  const WROT = new Float32Array(3);
  const UREL = new Float32Array(3);
  const RLREF = new Float32Array(3);

  for (let ib = 0; ib < NBODY; ib += 1) {
    const L1b = stateLayout ? LFRST[ib] : (LFRST[ib] - 1);
    const L2b = L1b + NL[ib] - 1;
    const BLEN = f32(Math.abs(f32(rlv(0, L2b) - rlv(0, L1b))));
    let SDFAC = 1.0;
    if (IYSYM === 1 && f32(rlv(1, L1b)) <= f32(0.001 * BLEN)) {
      SDFAC = 0.5;
    }

    for (let ilseg = 0; ilseg < NL[ib] - 1; ilseg += 1) {
      const L1 = stateLayout ? (LFRST[ib] + ilseg) : (LFRST[ib] - 1 + ilseg);
      const L2 = L1 + 1;
      const L = L1;

      DRL[0] = f32(f32(rlv(0, L2) - rlv(0, L1)) / beta);
      DRL[1] = f32(rlv(1, L2) - rlv(1, L1));
      DRL[2] = f32(rlv(2, L2) - rlv(2, L1));

      const DRLMAG = f32(Math.sqrt(f32(f32(DRL[0] * DRL[0]) + f32(DRL[1] * DRL[1]) + f32(DRL[2] * DRL[2]))));
      const DRLMI = DRLMAG === 0.0 ? 0.0 : f32(1.0 / DRLMAG);

      ESL[0] = f32(DRL[0] * DRLMI);
      ESL[1] = f32(DRL[1] * DRLMI);
      ESL[2] = f32(DRL[2] * DRLMI);

      const ADEL = f32(f32(PI * (f32(rad(L2) * rad(L2)) - f32(rad(L1) * rad(L1)))) * SDFAC);
      const AAVG = f32(f32(PI * 0.5 * (f32(rad(L2) * rad(L2)) + f32(rad(L1) * rad(L1)))) * SDFAC);

      RLREF[0] = f32(0.5 * f32(rlv(0, L2) + rlv(0, L1)) - XYZREF[0]);
      RLREF[1] = f32(0.5 * f32(rlv(1, L2) + rlv(1, L1)) - XYZREF[1]);
      RLREF[2] = f32(0.5 * f32(rlv(2, L2) + rlv(2, L1)) - XYZREF[2]);

      for (let iu = 0; iu < 6; iu += 1) {
        UREL[0] = 0.0;
        UREL[1] = 0.0;
        UREL[2] = 0.0;
        WROT[0] = 0.0;
        WROT[1] = 0.0;
        WROT[2] = 0.0;

        if (iu < 3) {
          UREL[iu] = 1.0;
        } else {
          WROT[iu - 3] = 1.0;
          CROSS(RLREF, WROT, UREL);
        }
        UREL[0] = f32(UREL[0] / beta);

        const US = DOT(UREL, ESL);

        const UN1 = f32(UREL[0] - f32(US * ESL[0]));
        const UN2 = f32(UREL[1] - f32(US * ESL[1]));
        const UN3 = f32(UREL[2] - f32(US * ESL[2]));

        setSrcU(L, iu, f32(ADEL * US));
        setDblU(0, L, iu, f32(f32(AAVG * UN1) * f32(DRLMAG * 2.0)));
        setDblU(1, L, iu, f32(f32(AAVG * UN2) * f32(DRLMAG * 2.0)));
        setDblU(2, L, iu, f32(f32(AAVG * UN3) * f32(DRLMAG * 2.0)));
      }
    }
  }

  return { SRC_U, DBL_U };
}
