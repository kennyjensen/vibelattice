// Port of AVL limits2.f with float32 math for numerical fidelity.

import { ROTENS3 } from './autil.js';

const f32 = Math.fround;

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function ifloor(x) {
  const i = Math.trunc(x);
  return x - i < 0 ? i - 1 : i;
}

function iceiling(x) {
  const i = Math.trunc(x);
  return x - i > 0 ? i + 1 : i;
}

export function AXISADJ(XMIN, XMAX, XSPAN, DELTAX, NTICS) {
  let xmin = f32(XMIN);
  let xmax = f32(XMAX);

  let xspan1 = f32(xmax - xmin);
  if (xspan1 === 0.0) {
    xspan1 = 1.0;
  }

  const xpon = Math.trunc(Math.log10(xspan1));
  let xspan = f32(xspan1 / f32(10.0 ** xpon));

  const xinctbl = [0.1, 0.2, 0.25, 0.5, 1.0];
  let xinc = xinctbl[0];
  let ntics = 1;
  for (let i = 0; i < xinctbl.length; i += 1) {
    xinc = xinctbl[i];
    ntics = 1 + Math.trunc(xspan / xinc + 0.1);
    if (ntics <= 6) {
      break;
    }
  }

  const deltax = f32(xinc * (10.0 ** xpon));
  xmin = f32(deltax * ifloor(xmin / deltax));
  xmax = f32(deltax * iceiling(xmax / deltax));
  xspan = f32(xmax - xmin);
  ntics = 1 + Math.trunc(xspan / deltax + 0.1);

  XSPAN[0] = xspan;
  DELTAX[0] = deltax;
  NTICS[0] = ntics;

  return { xmin, xmax, xspan, deltax, ntics };
}

export function VIEWPROJ(state, XYZ, N, XYZPROJ) {
  const view = state.VIEW;
  const rinv = f32(view.RINV);
  const xihat = f32(view.XIHAT);
  const yihat = f32(view.YIHAT);
  const zihat = f32(view.ZIHAT);
  const xjhat = f32(view.XJHAT);
  const yjhat = f32(view.YJHAT);
  const zjhat = f32(view.ZJHAT);
  const xkhat = f32(view.XKHAT);
  const ykhat = f32(view.YKHAT);
  const zkhat = f32(view.ZKHAT);

  for (let i = 0; i < N; i += 1) {
    const x = f32(XYZ[idx2(0, i, 3)]);
    const y = f32(XYZ[idx2(1, i, 3)]);
    const z = f32(XYZ[idx2(2, i, 3)]);

    const rdotI = f32(f32(x * xihat) + f32(y * yihat) + f32(z * zihat));
    const rdotJ = f32(f32(x * xjhat) + f32(y * yjhat) + f32(z * zjhat));
    const rdotK = f32(f32(x * xkhat) + f32(y * ykhat) + f32(z * zkhat));

    const rkx = f32(rdotK * xkhat);
    const rky = f32(rdotK * ykhat);
    const rkz = f32(rdotK * zkhat);

    const vscal = f32(1.0 / Math.sqrt(
      f32(f32(xkhat - f32(rinv * rkx)) ** 2 + f32(ykhat - f32(rinv * rky)) ** 2 + f32(zkhat - f32(rinv * rkz)) ** 2)
    ));

    XYZPROJ[idx2(0, i, 3)] = f32(vscal * rdotI);
    XYZPROJ[idx2(1, i, 3)] = f32(vscal * rdotJ);
    XYZPROJ[idx2(2, i, 3)] = f32(vscal * rdotK);
  }

  return XYZPROJ;
}

export function TETRAN(R, TT, RREF, DR) {
  const rb1 = f32(R[0] - RREF[0]);
  const rb2 = f32(R[1] - RREF[1]);
  const rb3 = f32(R[2] - RREF[2]);

  const r1 = f32(f32(TT[idx2(0, 0, 3)] * rb1)
    + f32(TT[idx2(0, 1, 3)] * rb2)
    + f32(TT[idx2(0, 2, 3)] * rb3)
    + RREF[0] + DR[0]);
  const r2 = f32(f32(TT[idx2(1, 0, 3)] * rb1)
    + f32(TT[idx2(1, 1, 3)] * rb2)
    + f32(TT[idx2(1, 2, 3)] * rb3)
    + RREF[1] + DR[1]);
  const r3 = f32(f32(TT[idx2(2, 0, 3)] * rb1)
    + f32(TT[idx2(2, 1, 3)] * rb2)
    + f32(TT[idx2(2, 2, 3)] * rb3)
    + RREF[2] + DR[2]);

  R[0] = r1;
  R[1] = r2;
  R[2] = r3;
  return R;
}

export function TRIINIT(ID, NROWS, NCOLS, PTS, state) {
  let it = state.NTRI;
  let idx = ID - 1;

  const nrowsp = NROWS + 1;
  for (let j = 1; j <= NCOLS; j += 1) {
    for (let k = 1; k <= NROWS; k += 1) {
      idx += 1;
      const ip1 = nrowsp * (j - 1) + k;
      const ip2 = ip1 + 1;
      const ip3 = ip1 + nrowsp;
      const ip4 = ip3 + 1;

      const p1 = ip1 - 1;
      const p2 = ip2 - 1;
      const p3 = ip3 - 1;
      const p4 = ip4 - 1;

      it += 1;
      const t1 = it - 1;

      state.TRI[idx2(0, t1, 16)] = PTS[idx2(0, p1, 3)];
      state.TRI[idx2(1, t1, 16)] = PTS[idx2(1, p1, 3)];
      state.TRI[idx2(2, t1, 16)] = PTS[idx2(2, p1, 3)];
      state.TRI[idx2(3, t1, 16)] = PTS[idx2(0, p2, 3)];
      state.TRI[idx2(4, t1, 16)] = PTS[idx2(1, p2, 3)];
      state.TRI[idx2(5, t1, 16)] = PTS[idx2(2, p2, 3)];
      state.TRI[idx2(6, t1, 16)] = PTS[idx2(0, p3, 3)];
      state.TRI[idx2(7, t1, 16)] = PTS[idx2(1, p3, 3)];
      state.TRI[idx2(8, t1, 16)] = PTS[idx2(2, p3, 3)];

      state.TRI[idx2(9, t1, 16)] = Math.min(
        state.TRI[idx2(0, t1, 16)],
        state.TRI[idx2(3, t1, 16)],
        state.TRI[idx2(6, t1, 16)]
      );
      state.TRI[idx2(10, t1, 16)] = Math.min(
        state.TRI[idx2(1, t1, 16)],
        state.TRI[idx2(4, t1, 16)],
        state.TRI[idx2(7, t1, 16)]
      );
      state.TRI[idx2(11, t1, 16)] = Math.min(
        state.TRI[idx2(2, t1, 16)],
        state.TRI[idx2(5, t1, 16)],
        state.TRI[idx2(8, t1, 16)]
      );
      state.TRI[idx2(12, t1, 16)] = Math.max(
        state.TRI[idx2(0, t1, 16)],
        state.TRI[idx2(3, t1, 16)],
        state.TRI[idx2(6, t1, 16)]
      );
      state.TRI[idx2(13, t1, 16)] = Math.max(
        state.TRI[idx2(1, t1, 16)],
        state.TRI[idx2(4, t1, 16)],
        state.TRI[idx2(7, t1, 16)]
      );
      state.TRI[idx2(14, t1, 16)] = Math.max(
        state.TRI[idx2(2, t1, 16)],
        state.TRI[idx2(5, t1, 16)],
        state.TRI[idx2(8, t1, 16)]
      );
      state.TRI[idx2(15, t1, 16)] = f32(idx);

      it += 1;
      const t2 = it - 1;

      state.TRI[idx2(0, t2, 16)] = PTS[idx2(0, p3, 3)];
      state.TRI[idx2(1, t2, 16)] = PTS[idx2(1, p3, 3)];
      state.TRI[idx2(2, t2, 16)] = PTS[idx2(2, p3, 3)];
      state.TRI[idx2(3, t2, 16)] = PTS[idx2(0, p2, 3)];
      state.TRI[idx2(4, t2, 16)] = PTS[idx2(1, p2, 3)];
      state.TRI[idx2(5, t2, 16)] = PTS[idx2(2, p2, 3)];
      state.TRI[idx2(6, t2, 16)] = PTS[idx2(0, p4, 3)];
      state.TRI[idx2(7, t2, 16)] = PTS[idx2(1, p4, 3)];
      state.TRI[idx2(8, t2, 16)] = PTS[idx2(2, p4, 3)];

      state.TRI[idx2(9, t2, 16)] = Math.min(
        state.TRI[idx2(0, t2, 16)],
        state.TRI[idx2(3, t2, 16)],
        state.TRI[idx2(6, t2, 16)]
      );
      state.TRI[idx2(10, t2, 16)] = Math.min(
        state.TRI[idx2(1, t2, 16)],
        state.TRI[idx2(4, t2, 16)],
        state.TRI[idx2(7, t2, 16)]
      );
      state.TRI[idx2(11, t2, 16)] = Math.min(
        state.TRI[idx2(2, t2, 16)],
        state.TRI[idx2(5, t2, 16)],
        state.TRI[idx2(8, t2, 16)]
      );
      state.TRI[idx2(12, t2, 16)] = Math.max(
        state.TRI[idx2(0, t2, 16)],
        state.TRI[idx2(3, t2, 16)],
        state.TRI[idx2(6, t2, 16)]
      );
      state.TRI[idx2(13, t2, 16)] = Math.max(
        state.TRI[idx2(1, t2, 16)],
        state.TRI[idx2(4, t2, 16)],
        state.TRI[idx2(7, t2, 16)]
      );
      state.TRI[idx2(14, t2, 16)] = Math.max(
        state.TRI[idx2(2, t2, 16)],
        state.TRI[idx2(5, t2, 16)],
        state.TRI[idx2(8, t2, 16)]
      );
      state.TRI[idx2(15, t2, 16)] = f32(idx);
    }
  }

  state.NTRI = it;
  return { ID: idx, NTRI: it, TRI: state.TRI };
}

export function GLIMS(state, XYZMIN = new Float32Array(3), XYZMAX = new Float32Array(3), LPROJ = false) {
  XYZMIN[0] = 1.0e20;
  XYZMIN[1] = 1.0e20;
  XYZMIN[2] = 1.0e20;
  XYZMAX[0] = -1.0e20;
  XYZMAX[1] = -1.0e20;
  XYZMAX[2] = -1.0e20;

  const pts = new Float32Array(6);
  let l1 = 0;
  let ln = 0;

  for (let n = 0; n < state.NSURF; n += 1) {
    if (state.LPLTSURF[n]) {
      const j1 = state.JFRST[n];
      const jn = state.JFRST[n] + state.NJ[n] - 1;
      const jdel = 1;

      for (let j = j1; j <= jn; j += jdel) {
        pts[idx2(0, 0, 3)] = state.RLE1[idx2(0, j, 3)];
        pts[idx2(1, 0, 3)] = state.RLE1[idx2(1, j, 3)];
        pts[idx2(2, 0, 3)] = state.RLE1[idx2(2, j, 3)];
        pts[idx2(0, 1, 3)] = f32(state.RLE1[idx2(0, j, 3)] + state.CHORD1[j]);
        pts[idx2(1, 1, 3)] = state.RLE1[idx2(1, j, 3)];
        pts[idx2(2, 1, 3)] = state.RLE1[idx2(2, j, 3)];
        if (LPROJ) {
          VIEWPROJ(state, pts, 2, pts);
        }

        for (let k = 0; k < 3; k += 1) {
          const v1 = pts[idx2(k, 0, 3)];
          const v2 = pts[idx2(k, 1, 3)];
          XYZMIN[k] = Math.min(XYZMIN[k], v1, v2);
          XYZMAX[k] = Math.max(XYZMAX[k], v1, v2);
        }
      }
    }
  }

  for (let n = 0; n < state.NBODY; n += 1) {
    if (state.LPLTBODY[n]) {
      l1 = state.LFRST[n];
      ln = state.LFRST[n] + state.NL[n] - 1;
      pts[idx2(0, 0, 3)] = state.RL[idx2(0, l1, 3)];
      pts[idx2(1, 0, 3)] = state.RL[idx2(1, l1, 3)];
      pts[idx2(2, 0, 3)] = state.RL[idx2(2, l1, 3)];
      pts[idx2(0, 1, 3)] = state.RL[idx2(0, ln, 3)];
      pts[idx2(1, 1, 3)] = state.RL[idx2(1, ln, 3)];
      pts[idx2(2, 1, 3)] = state.RL[idx2(2, ln, 3)];
      if (LPROJ) {
        VIEWPROJ(state, pts, 2, pts);
      }

      for (let k = 0; k < 3; k += 1) {
        const v1 = pts[idx2(k, 0, 3)];
        const v2 = pts[idx2(k, 1, 3)];
        XYZMIN[k] = Math.min(XYZMIN[k], v1, v2);
        XYZMAX[k] = Math.max(XYZMAX[k], v1, v2);
      }
    }
  }

  if (state.LOBPLT) {
    for (let n = 0; n < state.NOB; n += 1) {
      pts[idx2(0, 0, 3)] = state.ROB[idx2(0, l1, 3)];
      pts[idx2(1, 0, 3)] = state.ROB[idx2(1, l1, 3)];
      pts[idx2(2, 0, 3)] = state.ROB[idx2(2, l1, 3)];
      pts[idx2(0, 1, 3)] = state.ROB[idx2(0, ln, 3)];
      pts[idx2(1, 1, 3)] = state.ROB[idx2(1, ln, 3)];
      pts[idx2(2, 1, 3)] = state.ROB[idx2(2, ln, 3)];
      if (LPROJ) {
        VIEWPROJ(state, pts, 2, pts);
      }
      for (let k = 0; k < 3; k += 1) {
        const v1 = pts[idx2(k, 0, 3)];
        const v2 = pts[idx2(k, 1, 3)];
        XYZMIN[k] = Math.min(XYZMIN[k], v1, v2);
        XYZMAX[k] = Math.max(XYZMAX[k], v1, v2);
      }
    }
  }

  return { XYZMIN, XYZMAX };
}

export function GRLIMS(state, XYZMIN = new Float32Array(3), XYZMAX = new Float32Array(3), LPROJ, TT, XYZR, DXYZ) {
  XYZMIN[0] = 1.0e20;
  XYZMIN[1] = 1.0e20;
  XYZMIN[2] = 1.0e20;
  XYZMAX[0] = -1.0e20;
  XYZMAX[1] = -1.0e20;
  XYZMAX[2] = -1.0e20;

  const pts = new Float32Array(6);

  for (let n = 0; n < state.NSURF; n += 1) {
    if (state.LPLTSURF[n]) {
      const j1 = state.JFRST[n];
      const jn = state.JFRST[n] + state.NJ[n] - 1;

      pts[idx2(0, 0, 3)] = state.RLE1[idx2(0, j1, 3)];
      pts[idx2(1, 0, 3)] = state.RLE1[idx2(1, j1, 3)];
      pts[idx2(2, 0, 3)] = state.RLE1[idx2(2, j1, 3)];
      pts[idx2(0, 1, 3)] = f32(state.RLE1[idx2(0, j1, 3)] + state.CHORD1[j1]);
      pts[idx2(1, 1, 3)] = state.RLE1[idx2(1, j1, 3)];
      pts[idx2(2, 1, 3)] = state.RLE1[idx2(2, j1, 3)];
      TETRAN(pts.subarray(0, 3), TT, XYZR, DXYZ);
      TETRAN(pts.subarray(3, 6), TT, XYZR, DXYZ);
      if (LPROJ) {
        VIEWPROJ(state, pts, 2, pts);
      }
      for (let k = 0; k < 3; k += 1) {
        XYZMIN[k] = Math.min(XYZMIN[k], pts[idx2(k, 0, 3)], pts[idx2(k, 1, 3)]);
        XYZMAX[k] = Math.max(XYZMAX[k], pts[idx2(k, 0, 3)], pts[idx2(k, 1, 3)]);
      }

      pts[idx2(0, 0, 3)] = state.RLE2[idx2(0, jn, 3)];
      pts[idx2(1, 0, 3)] = state.RLE2[idx2(1, jn, 3)];
      pts[idx2(2, 0, 3)] = state.RLE2[idx2(2, jn, 3)];
      pts[idx2(0, 1, 3)] = f32(state.RLE2[idx2(0, jn, 3)] + state.CHORD2[jn]);
      pts[idx2(1, 1, 3)] = state.RLE2[idx2(1, jn, 3)];
      pts[idx2(2, 1, 3)] = state.RLE2[idx2(2, jn, 3)];
      TETRAN(pts.subarray(0, 3), TT, XYZR, DXYZ);
      TETRAN(pts.subarray(3, 6), TT, XYZR, DXYZ);
      if (LPROJ) {
        VIEWPROJ(state, pts, 2, pts);
      }
      for (let k = 0; k < 3; k += 1) {
        XYZMIN[k] = Math.min(XYZMIN[k], pts[idx2(k, 0, 3)], pts[idx2(k, 1, 3)]);
        XYZMAX[k] = Math.max(XYZMAX[k], pts[idx2(k, 0, 3)], pts[idx2(k, 1, 3)]);
      }
    }
  }

  for (let n = 0; n < state.NBODY; n += 1) {
    if (state.LPLTBODY[n]) {
      const l1 = state.LFRST[n];
      const ln = state.LFRST[n] + state.NL[n] - 1;
      pts[idx2(0, 0, 3)] = state.RL[idx2(0, l1, 3)];
      pts[idx2(1, 0, 3)] = state.RL[idx2(1, l1, 3)];
      pts[idx2(2, 0, 3)] = state.RL[idx2(2, l1, 3)];
      pts[idx2(0, 1, 3)] = state.RL[idx2(0, ln, 3)];
      pts[idx2(1, 1, 3)] = state.RL[idx2(1, ln, 3)];
      pts[idx2(2, 1, 3)] = state.RL[idx2(2, ln, 3)];
      TETRAN(pts.subarray(0, 3), TT, XYZR, DXYZ);
      TETRAN(pts.subarray(3, 6), TT, XYZR, DXYZ);
      if (LPROJ) {
        VIEWPROJ(state, pts, 2, pts);
      }
      for (let k = 0; k < 3; k += 1) {
        XYZMIN[k] = Math.min(XYZMIN[k], pts[idx2(k, 0, 3)], pts[idx2(k, 1, 3)]);
        XYZMAX[k] = Math.max(XYZMAX[k], pts[idx2(k, 0, 3)], pts[idx2(k, 1, 3)]);
      }
    }
  }

  return { XYZMIN, XYZMAX };
}

export function AXLIMS(state) {
  for (let k = 0; k < 3; k += 1) {
    state.AXMIN[k] = Math.min(state.GMIN[k], 0.0);
    state.AXMAX[k] = Math.max(state.GMAX[k], 0.0);
  }

  const axdmax = Math.max(
    state.AXMAX[0] - state.AXMIN[0],
    state.AXMAX[1] - state.AXMIN[1],
    state.AXMAX[2] - state.AXMIN[2]
  );

  for (let k = 0; k < 3; k += 1) {
    if (state.AXMAX[k] - state.AXMIN[k] < 0.25 * axdmax) {
      state.AXMIN[k] = Math.min(state.AXMIN[k], -0.125 * axdmax);
      state.AXMAX[k] = Math.max(state.AXMAX[k], 0.125 * axdmax);
    }
    const xspan = new Float32Array(1);
    const del = new Float32Array(1);
    const nt = new Int32Array(1);
    const out = AXISADJ(state.AXMIN[k], state.AXMAX[k], xspan, del, nt);
    state.AXMIN[k] = out.xmin;
    state.AXMAX[k] = out.xmax;
    state.AXSPAN[k] = xspan[0];
    state.AXDEL[k] = del[0];
    state.NAXANN[k] = nt[0];
  }

  return state;
}

export function HIDINIT(state, LRESET) {
  if (LRESET) {
    state.NTRI = 0;
  }

  const pts = new Float32Array(12);

  for (let n = 0; n < state.NSURF; n += 1) {
    if (state.LPLTSURF[n]) {
      const j1 = state.JFRST[n];
      const jn = state.JFRST[n] + state.NJ[n] - 1;
      const jinc = 1;

      for (let j = j1; j <= jn; j += jinc) {
        const id = j + 1;
        pts[idx2(0, 0, 3)] = state.RLE1[idx2(0, j, 3)];
        pts[idx2(1, 0, 3)] = state.RLE1[idx2(1, j, 3)];
        pts[idx2(2, 0, 3)] = state.RLE1[idx2(2, j, 3)];
        pts[idx2(0, 1, 3)] = f32(state.RLE1[idx2(0, j, 3)] + state.CHORD1[j]);
        pts[idx2(1, 1, 3)] = state.RLE1[idx2(1, j, 3)];
        pts[idx2(2, 1, 3)] = state.RLE1[idx2(2, j, 3)];
        pts[idx2(0, 2, 3)] = state.RLE2[idx2(0, j, 3)];
        pts[idx2(1, 2, 3)] = state.RLE2[idx2(1, j, 3)];
        pts[idx2(2, 2, 3)] = state.RLE2[idx2(2, j, 3)];
        pts[idx2(0, 3, 3)] = f32(state.RLE2[idx2(0, j, 3)] + state.CHORD2[j]);
        pts[idx2(1, 3, 3)] = state.RLE2[idx2(1, j, 3)];
        pts[idx2(2, 3, 3)] = state.RLE2[idx2(2, j, 3)];

        VIEWPROJ(state, pts, 4, pts);
        TRIINIT(id, 1, 1, pts, state);
      }
    }
  }

  return state;
}

export function HIDINITE(state, LRESET, ANG, POS, XYZR) {
  if (LRESET) {
    state.NTRI = 0;
  }

  const tt = new Float32Array(9);
  const ttAng = new Float32Array(27);
  ROTENS3(ANG, tt, ttAng);

  const pts = new Float32Array(12);

  for (let n = 0; n < state.NSURF; n += 1) {
    if (state.LPLTSURF[n]) {
      const j1 = state.JFRST[n];
      const jn = state.JFRST[n] + state.NJ[n] - 1;
      const jinc = 1;

      for (let j = j1; j <= jn; j += jinc) {
        const id = j + 1;
        pts[idx2(0, 0, 3)] = state.RLE1[idx2(0, j, 3)];
        pts[idx2(1, 0, 3)] = state.RLE1[idx2(1, j, 3)];
        pts[idx2(2, 0, 3)] = state.RLE1[idx2(2, j, 3)];
        pts[idx2(0, 1, 3)] = f32(state.RLE1[idx2(0, j, 3)] + state.CHORD1[j]);
        pts[idx2(1, 1, 3)] = state.RLE1[idx2(1, j, 3)];
        pts[idx2(2, 1, 3)] = state.RLE1[idx2(2, j, 3)];
        pts[idx2(0, 2, 3)] = state.RLE2[idx2(0, j, 3)];
        pts[idx2(1, 2, 3)] = state.RLE2[idx2(1, j, 3)];
        pts[idx2(2, 2, 3)] = state.RLE2[idx2(2, j, 3)];
        pts[idx2(0, 3, 3)] = f32(state.RLE2[idx2(0, j, 3)] + state.CHORD2[j]);
        pts[idx2(1, 3, 3)] = state.RLE2[idx2(1, j, 3)];
        pts[idx2(2, 3, 3)] = state.RLE2[idx2(2, j, 3)];

        TETRAN(pts.subarray(0, 3), tt, XYZR, POS);
        TETRAN(pts.subarray(3, 6), tt, XYZR, POS);
        TETRAN(pts.subarray(6, 9), tt, XYZR, POS);
        TETRAN(pts.subarray(9, 12), tt, XYZR, POS);

        VIEWPROJ(state, pts, 4, pts);
        TRIINIT(id, 1, 1, pts, state);
      }
    }
  }

  return state;
}
