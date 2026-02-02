/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL amake.f (surface geometry + ENCALC) with float32 math.
// Surface-only subset (no BODY), sufficient for b737.avl.

const f32 = Math.fround;

import { SPACER, CSPACER, AKIMA } from './sgutil.js';
import { CROSS } from './aic.js';

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function idx3(i, j, k, dim1, dim2) {
  return i + dim1 * (j + dim2 * k);
}

function zero3() {
  return new Float32Array([0.0, 0.0, 0.0]);
}

function dot3(a, b) {
  return f32(a[0] * b[0] + a[1] * b[1] + a[2] * b[2]);
}

export function MAKESURF(state, isurf, surf) {
  const debug = state.__debug;
  const NSEC = surf.sections.length;
  if (NSEC < 2) return;

  const NVC1 = surf.nChord || 1;
  const CSPACE = surf.cSpace;
  const NVS1 = surf.nSpan || 0;
  const SSPACE = surf.sSpace;
  const XYZSCAL = surf.scale;
  const XYZTRAN = surf.translate;
  const ADDINC = f32((surf.angleDeg ?? surf.angle ?? 0.0) * state.DTR);

  const NCONTROL = state.NCONTROL;
  const NDESIGN = 0;

  const XYZLES = surf.sections.map((s) => [s.xle, s.yle, s.zle]);
  const CHORDS = surf.sections.map((s) => s.chord);
  const AINCS = surf.sections.map((s) => f32(s.aincDeg * state.DTR));
  const SSPACES = surf.sections.map((s) => s.sSpace ?? SSPACE);
  const NSPANS = surf.sections.map((s) => s.nSpan ?? 0);

  const NASEC = surf.sections.map((s) => s.airfoilCamber?.x?.length ?? 0);
  const XASEC = surf.sections.map((s) => s.airfoilCamber?.x ?? []);
  const SASEC = surf.sections.map((s) => s.airfoilCamber?.s ?? []);
  const TASEC = surf.sections.map((s) => s.airfoilCamber?.t ?? []);

  const CLAF = surf.sections.map((s) => s.claf ?? 1.0);

  // Per-section control declarations
  const NSCON = surf.sections.map((s) => s.controls.length);
  const ICONTD = surf.sections.map((s) => s.controls.map((c) => c.index));
  const GAIND = surf.sections.map((s) => s.controls.map((c) => c.gain));
  const XHINGED = surf.sections.map((s) => s.controls.map((c) => c.xhinge));
  const VHINGED = surf.sections.map((s) => s.controls.map((c) => c.vhinge));
  const REFLD = surf.sections.map((s) => s.controls.map((c) => c.sgnDup));

  const IMAGS = state.IMAGS;
  const IFRST = state.IFRST;
  const JFRST = state.JFRST;
  const NK = state.NK;

  const NJ = state.NJ;
  const IJFRST = state.IJFRST;
  const NVSTRP = state.NVSTRP;
  const LSSURF = state.LSSURF;

  const RLE1 = state.RLE1;
  const RLE2 = state.RLE2;
  const RLE = state.RLE;
  const CHORD1 = state.CHORD1;
  const CHORD2 = state.CHORD2;
  const CHORD = state.CHORD;
  const WSTRIP = state.WSTRIP;
  const TANLE = state.TANLE;
  const TANTE = state.TANTE;
  const AINC = state.AINC;
  const AINC_G = state.AINC_G;

  const CLCD = state.CLCD;
  const LVISCSTRP = state.LVISCSTRP;

  const RV1 = state.RV1;
  const RV2 = state.RV2;
  const RV = state.RV;
  const RC = state.RC;
  const RS = state.RS;
  const DXV = state.DXV;
  const CHORDV = state.CHORDV;
  const SLOPEC = state.SLOPEC;
  const SLOPEV = state.SLOPEV;
  const LVCOMP = state.LVCOMP;
  const LVALBE = state.LVALBE;
  const DCONTROL = state.DCONTROL;
  const VHINGE = state.VHINGE;
  const VREFL = state.VREFL;
  const PHINGE = state.PHINGE;

  const imags = (surf.imags == null || surf.imags === 0) ? 1 : surf.imags;
  IMAGS[isurf] = imags;
  const compIndex = (surf.component == null || surf.component === 0) ? isurf : surf.component;
  IFRST[isurf] = state.NVOR + 1;
  JFRST[isurf] = state.NSTRIP + 1;
  NK[isurf] = NVC1;

  const YZLEN = new Float32Array(NSEC + 1);
  YZLEN[1] = 0.0;
  for (let isec = 2; isec <= NSEC; isec += 1) {
    const dy = XYZLES[isec - 1][1] - XYZLES[isec - 2][1];
    const dz = XYZLES[isec - 1][2] - XYZLES[isec - 2][2];
    YZLEN[isec] = f32(YZLEN[isec - 1] + Math.sqrt(f32(dy * dy + dz * dz)));
  }

  let NVS = NVS1;
  const YPT = new Float32Array(500 + 2);
  const YCP = new Float32Array(500 + 2);
  const IPTLOC = new Int32Array(NSEC + 2);
  if (NVS1 === 0) {
    NVS = 0;
    for (let isec = 1; isec <= NSEC - 1; isec += 1) NVS += NSPANS[isec - 1] ?? 0;
    YPT[1] = YZLEN[1];
    IPTLOC[1] = 1;
    let nvs = 0;
    for (let isec = 1; isec <= NSEC - 1; isec += 1) {
      const dyzlen = f32(YZLEN[isec + 1] - YZLEN[isec]);
      const nvint = NSPANS[isec - 1] ?? 0;
      const nspace = 2 * nvint + 1;
      const fspace = new Float32Array(nspace + 1);
      SPACER(nspace, SSPACES[isec - 1] ?? 1.0, fspace);
      for (let n = 1; n <= nvint; n += 1) {
        const ivs = nvs + n;
        YCP[ivs] = f32(YPT[nvs + 1] + f32(dyzlen * fspace[2 * n]));
        YPT[ivs + 1] = f32(YPT[nvs + 1] + f32(dyzlen * fspace[2 * n + 1]));
      }
      IPTLOC[isec + 1] = nvs + nvint + 1;
      nvs += nvint;
    }
    NVS = nvs;
  } else {
    const nspace = 2 * NVS + 1;
    const fspace = new Float32Array(nspace + 1);
    SPACER(nspace, SSPACE, fspace);
    YPT[1] = YZLEN[1];
    for (let ivs = 1; ivs <= NVS; ivs += 1) {
      YCP[ivs] = f32(YZLEN[1] + f32((YZLEN[NSEC] - YZLEN[1]) * fspace[2 * ivs]));
      YPT[ivs + 1] = f32(YZLEN[1] + f32((YZLEN[NSEC] - YZLEN[1]) * fspace[2 * ivs + 1]));
    }
    const npt = NVS + 1;
    for (let isec = 2; isec <= NSEC - 1; isec += 1) {
      let best = 1e9;
      let bestIdx = 1;
      for (let ipt = 1; ipt <= npt; ipt += 1) {
        const d = Math.abs(f32(YZLEN[isec] - YPT[ipt]));
        if (d < best) {
          best = d;
          bestIdx = ipt;
        }
      }
      IPTLOC[isec] = bestIdx;
    }
    IPTLOC[1] = 1;
    IPTLOC[NSEC] = npt;
  }

  NJ[isurf] = 0;
  if (debug) {
    debug({
      step: 'MAKESURF span setup',
      isurf,
      NSEC,
      NVS,
      NVC1,
      NSTRIP: state.NSTRIP,
      NVOR: state.NVOR,
    });
  }

  let loopGuard = 0;
  for (let isec = 1; isec <= NSEC - 1; isec += 1) {
    const xyzleL = [
      f32(XYZSCAL[0] * XYZLES[isec - 1][0] + XYZTRAN[0]),
      f32(XYZSCAL[1] * XYZLES[isec - 1][1] + XYZTRAN[1]),
      f32(XYZSCAL[2] * XYZLES[isec - 1][2] + XYZTRAN[2]),
    ];
    const xyzleR = [
      f32(XYZSCAL[0] * XYZLES[isec][0] + XYZTRAN[0]),
      f32(XYZSCAL[1] * XYZLES[isec][1] + XYZTRAN[1]),
      f32(XYZSCAL[2] * XYZLES[isec][2] + XYZTRAN[2]),
    ];
    const width = f32(Math.sqrt(f32((xyzleR[1] - xyzleL[1]) ** 2 + (xyzleR[2] - xyzleL[2]) ** 2)));
    if (!Number.isFinite(width) || width === 0.0) {
      continue;
    }

    const chordL = f32(XYZSCAL[0] * CHORDS[isec - 1]);
    const chordR = f32(XYZSCAL[0] * CHORDS[isec]);

    const clafL = CLAF[isec - 1] ?? 1.0;
    const clafR = CLAF[isec] ?? 1.0;

    const aincL = f32(AINCS[isec - 1] + ADDINC);
    const aincR = f32(AINCS[isec] + ADDINC);

    const chsinL = f32(chordL * Math.sin(aincL));
    const chsinR = f32(chordR * Math.sin(aincR));
    const chcosL = f32(chordL * Math.cos(aincL));
    const chcosR = f32(chordR * Math.cos(aincR));

    const isconL = new Int32Array(NCONTROL + 1);
    const isconR = new Int32Array(NCONTROL + 1);
    for (let n = 1; n <= NCONTROL; n += 1) {
      isconL[n] = 0;
      isconR[n] = 0;
      for (let iscon = 1; iscon <= (NSCON[isec - 1] ?? 0); iscon += 1) {
        if (ICONTD[isec - 1][iscon - 1] === n) isconL[n] = iscon;
      }
      for (let iscon = 1; iscon <= (NSCON[isec] ?? 0); iscon += 1) {
        if (ICONTD[isec][iscon - 1] === n) isconR[n] = iscon;
      }
    }

    const iptL = IPTLOC[isec];
    const iptR = IPTLOC[isec + 1];
    const nspan = iptR - iptL;
    if (nspan <= 0) continue;
    if (debug) {
      debug({
        step: 'MAKESURF span segment',
        isec,
        iptL,
        iptR,
        nspan,
        YPT_L: YPT[iptL],
        YPT_R: YPT[iptR],
      });
    }

    for (let ispan = 1; ispan <= nspan; ispan += 1) {
      loopGuard += 1;
      if (loopGuard > 1e6) {
        throw new Error('MAKESURF loop guard tripped');
      }
      const ipt1 = iptL + ispan - 1;
      const ipt2 = iptL + ispan;
      const ivs = iptL + ispan - 1;
      const f1 = f32((YPT[ipt1] - YPT[iptL]) / (YPT[iptR] - YPT[iptL]));
      const f2 = f32((YPT[ipt2] - YPT[iptL]) / (YPT[iptR] - YPT[iptL]));
      const fc = f32((YCP[ivs] - YPT[iptL]) / (YPT[iptR] - YPT[iptL]));

      state.NSTRIP += 1;
      NJ[isurf] += 1;
      const j = state.NSTRIP;

      const rle1 = [
        f32((1.0 - f1) * xyzleL[0] + f1 * xyzleR[0]),
        f32((1.0 - f1) * xyzleL[1] + f1 * xyzleR[1]),
        f32((1.0 - f1) * xyzleL[2] + f1 * xyzleR[2]),
      ];
      const rle2 = [
        f32((1.0 - f2) * xyzleL[0] + f2 * xyzleR[0]),
        f32((1.0 - f2) * xyzleL[1] + f2 * xyzleR[1]),
        f32((1.0 - f2) * xyzleL[2] + f2 * xyzleR[2]),
      ];
      const rle = [
        f32((1.0 - fc) * xyzleL[0] + fc * xyzleR[0]),
        f32((1.0 - fc) * xyzleL[1] + fc * xyzleR[1]),
        f32((1.0 - fc) * xyzleL[2] + fc * xyzleR[2]),
      ];

      let rle1x = rle1[0];
      let rle1y = rle1[1];
      let rle1z = rle1[2];
      let rle2x = rle2[0];
      let rle2y = rle2[1];
      let rle2z = rle2[2];
      if (imags < 0) {
        const tx = rle1x; const ty = rle1y; const tz = rle1z;
        rle1x = rle2x; rle1y = rle2y; rle1z = rle2z;
        rle2x = tx; rle2y = ty; rle2z = tz;
      }
      RLE1[idx2(1, j, 4)] = rle1x;
      RLE1[idx2(2, j, 4)] = rle1y;
      RLE1[idx2(3, j, 4)] = rle1z;
      RLE2[idx2(1, j, 4)] = rle2x;
      RLE2[idx2(2, j, 4)] = rle2y;
      RLE2[idx2(3, j, 4)] = rle2z;
      RLE[idx2(1, j, 4)] = rle[0];
      RLE[idx2(2, j, 4)] = rle[1];
      RLE[idx2(3, j, 4)] = rle[2];

      let chord1 = f32((1.0 - f1) * chordL + f1 * chordR);
      let chord2 = f32((1.0 - f2) * chordL + f2 * chordR);
      if (imags < 0) {
        const tmp = chord1;
        chord1 = chord2;
        chord2 = tmp;
      }
      CHORD1[j] = chord1;
      CHORD2[j] = chord2;
      CHORD[j] = f32((1.0 - fc) * chordL + fc * chordR);

      WSTRIP[j] = f32(Math.abs(f2 - f1) * width);
      TANLE[j] = f32((xyzleR[0] - xyzleL[0]) / width);
      if (imags < 0) TANLE[j] = f32(-TANLE[j]);
      TANTE[j] = f32((xyzleR[0] + chordR - xyzleL[0] - chordL) / width);

      const chsin = f32(chsinL + fc * (chsinR - chsinL));
      const chcos = f32(chcosL + fc * (chcosR - chcosL));
      AINC[j] = f32(Math.atan2(chsin, chcos));

      IJFRST[j] = state.NVOR + 1;
      NVSTRP[j] = NVC1;
      LSSURF[j] = isurf;

      const chordC = CHORD[j];
      const clafC = f32((1.0 - fc) * (chordL / chordC) * clafL + fc * (chordR / chordC) * clafR);
      const XPT = new Float32Array(NVC1 + 2);
      const XVR = new Float32Array(NVC1 + 2);
      const XSR = new Float32Array(NVC1 + 2);
      const XCP = new Float32Array(NVC1 + 2);
      CSPACER(NVC1, CSPACE, clafC, XPT, XVR, XSR, XCP);

      for (let ivc = 1; ivc <= NVC1; ivc += 1) {
        state.NVOR += 1;
        const i = state.NVOR;
        RV1[idx2(1, i, 4)] = f32(RLE1[idx2(1, j, 4)] + XVR[ivc] * CHORD1[j]);
        RV1[idx2(2, i, 4)] = RLE1[idx2(2, j, 4)];
        RV1[idx2(3, i, 4)] = RLE1[idx2(3, j, 4)];
        RV2[idx2(1, i, 4)] = f32(RLE2[idx2(1, j, 4)] + XVR[ivc] * CHORD2[j]);
        RV2[idx2(2, i, 4)] = RLE2[idx2(2, j, 4)];
        RV2[idx2(3, i, 4)] = RLE2[idx2(3, j, 4)];

        RV[idx2(1, i, 4)] = f32(RLE[idx2(1, j, 4)] + XVR[ivc] * chordC);
        RV[idx2(2, i, 4)] = RLE[idx2(2, j, 4)];
        RV[idx2(3, i, 4)] = RLE[idx2(3, j, 4)];

        RC[idx2(1, i, 4)] = f32(RLE[idx2(1, j, 4)] + XCP[ivc] * chordC);
        RC[idx2(2, i, 4)] = RLE[idx2(2, j, 4)];
        RC[idx2(3, i, 4)] = RLE[idx2(3, j, 4)];

        RS[idx2(1, i, 4)] = f32(RLE[idx2(1, j, 4)] + XSR[ivc] * chordC);
        RS[idx2(2, i, 4)] = RLE[idx2(2, j, 4)];
        RS[idx2(3, i, 4)] = RLE[idx2(3, j, 4)];

        let sL = 0.0;
        let sR = 0.0;
        if (NASEC[isec - 1] > 1) {
          const xarr = XASEC[isec - 1];
          const sarr = SASEC[isec - 1];
          const res = AKIMA(xarr, sarr, NASEC[isec - 1], XCP[ivc]);
          sL = res.YY;
        }
        if (NASEC[isec] > 1) {
          const xarr = XASEC[isec];
          const sarr = SASEC[isec];
          const res = AKIMA(xarr, sarr, NASEC[isec], XCP[ivc]);
          sR = res.YY;
        }
        SLOPEC[i] = f32((1.0 - fc) * (chordL / chordC) * sL + fc * (chordR / chordC) * sR);

        let svL = 0.0;
        let svR = 0.0;
        if (NASEC[isec - 1] > 1) {
          const res = AKIMA(XASEC[isec - 1], SASEC[isec - 1], NASEC[isec - 1], XVR[ivc]);
          svL = res.YY;
        }
        if (NASEC[isec] > 1) {
          const res = AKIMA(XASEC[isec], SASEC[isec], NASEC[isec], XVR[ivc]);
          svR = res.YY;
        }
        SLOPEV[i] = f32((1.0 - fc) * (chordL / chordC) * svL + fc * (chordR / chordC) * svR);

        const dxoc = f32(XPT[ivc + 1] - XPT[ivc]);
        DXV[i] = f32(dxoc * chordC);
        CHORDV[i] = chordC;
        LVCOMP[i] = compIndex;
        LVALBE[i] = surf.lvalbe ?? true;

        for (let n = 1; n <= NCONTROL; n += 1) {
          const icl = isconL[n];
          const icr = isconR[n];
          let gainda = 0.0;
          let xled = 0.0;
          let xted = 0.0;
          if (icl !== 0 && icr !== 0) {
            gainda = f32(GAIND[isec - 1][icl - 1] * (1.0 - fc) + GAIND[isec][icr - 1] * fc);
            const xhd = f32(chordL * XHINGED[isec - 1][icl - 1] * (1.0 - fc)
              + chordR * XHINGED[isec][icr - 1] * fc);
            if (xhd >= 0.0) {
              xled = xhd;
              xted = chordC;
            } else {
              xled = 0.0;
              xted = -xhd;
            }
            let vhx = VHINGED[isec - 1][icl - 1][0] * XYZSCAL[0];
            let vhy = VHINGED[isec - 1][icl - 1][1] * XYZSCAL[1];
            let vhz = VHINGED[isec - 1][icl - 1][2] * XYZSCAL[2];
            let vsq = f32(vhx * vhx + vhy * vhy + vhz * vhz);
            if (vsq === 0.0) {
              vhx = f32((XYZLES[isec][0] + Math.abs(chordR * XHINGED[isec][icr - 1]))
                - (XYZLES[isec - 1][0] + Math.abs(chordL * XHINGED[isec - 1][icl - 1])));
              vhy = f32(XYZLES[isec][1] - XYZLES[isec - 1][1]);
              vhz = f32(XYZLES[isec][2] - XYZLES[isec - 1][2]);
              vhx = f32(vhx * XYZSCAL[0]);
              vhy = f32(vhy * XYZSCAL[1]);
              vhz = f32(vhz * XYZSCAL[2]);
              vsq = f32(vhx * vhx + vhy * vhy + vhz * vhz);
            }
            const vmod = f32(Math.sqrt(vsq));
            VHINGE[idx3(1, i, n, 4, state.NVMAX + 1)] = f32(vhx / vmod);
            VHINGE[idx3(2, i, n, 4, state.NVMAX + 1)] = f32(vhy / vmod);
            VHINGE[idx3(3, i, n, 4, state.NVMAX + 1)] = f32(vhz / vmod);
            VREFL[idx2(i, n, state.NVMAX + 1)] = f32(REFLD[isec - 1][icl - 1]);
            if (xhd >= 0.0) {
              PHINGE[idx3(1, i, n, 4, state.NVMAX + 1)] = f32(RLE[idx2(1, j, 4)] + xhd);
              PHINGE[idx3(2, i, n, 4, state.NVMAX + 1)] = RLE[idx2(2, j, 4)];
              PHINGE[idx3(3, i, n, 4, state.NVMAX + 1)] = RLE[idx2(3, j, 4)];
            } else {
              PHINGE[idx3(1, i, n, 4, state.NVMAX + 1)] = f32(RLE[idx2(1, j, 4)] - xhd);
              PHINGE[idx3(2, i, n, 4, state.NVMAX + 1)] = RLE[idx2(2, j, 4)];
              PHINGE[idx3(3, i, n, 4, state.NVMAX + 1)] = RLE[idx2(3, j, 4)];
            }
          }
          const fracle = f32((xled / chordC - XPT[ivc]) / dxoc);
          const fracte = f32((xted / chordC - XPT[ivc]) / dxoc);
          const fracl = Math.min(1.0, Math.max(0.0, fracle));
          const fract = Math.min(1.0, Math.max(0.0, fracte));
          DCONTROL[idx2(i, n, state.NVMAX + 1)] = f32(gainda * (fract - fracl));
        }

        LVISCSTRP[j] = false;
      }
    }
  }

  // Surface area
  let sum = 0.0;
  let wtot = 0.0;
  for (let jj = 1; jj <= NJ[isurf]; jj += 1) {
    const j = JFRST[isurf] + jj - 1;
    const astrp = f32(WSTRIP[j] * CHORD[j]);
    sum = f32(sum + astrp);
    wtot = f32(wtot + WSTRIP[j]);
  }
  state.SSURF[isurf] = sum;
  state.CAVESURF[isurf] = wtot === 0.0 ? 0.0 : f32(sum / wtot);
}

export function SDUPL(state, baseSurf, ydup, newSurf) {
  const yoff = f32(2.0 * ydup);

  state.LNCOMP[newSurf] = state.LNCOMP[baseSurf];
  if (state.LFWAKE) state.LFWAKE[newSurf] = state.LFWAKE[baseSurf];
  if (state.LFLOAD) state.LFLOAD[newSurf] = state.LFLOAD[baseSurf];
  state.SSURF[newSurf] = state.SSURF[baseSurf];
  state.CAVESURF[newSurf] = state.CAVESURF[baseSurf];
  state.IMAGS[newSurf] = -state.IMAGS[baseSurf];

  state.IFRST[newSurf] = state.NVOR + 1;
  state.JFRST[newSurf] = state.NSTRIP + 1;
  state.NJ[newSurf] = state.NJ[baseSurf];
  state.NK[newSurf] = state.NK[baseSurf];

  const nvs = state.NJ[newSurf];
  const nvc = state.NK[newSurf];

  for (let ivs = 1; ivs <= nvs; ivs += 1) {
    state.NSTRIP += 1;
    const jji = state.JFRST[newSurf] + ivs - 1;
    const jj = state.JFRST[baseSurf] + ivs - 1;

    state.RLE1[idx2(1, jji, 4)] = state.RLE2[idx2(1, jj, 4)];
    state.RLE1[idx2(2, jji, 4)] = f32(-state.RLE2[idx2(2, jj, 4)] + yoff);
    state.RLE1[idx2(3, jji, 4)] = state.RLE2[idx2(3, jj, 4)];
    state.CHORD1[jji] = state.CHORD2[jj];

    state.RLE2[idx2(1, jji, 4)] = state.RLE1[idx2(1, jj, 4)];
    state.RLE2[idx2(2, jji, 4)] = f32(-state.RLE1[idx2(2, jj, 4)] + yoff);
    state.RLE2[idx2(3, jji, 4)] = state.RLE1[idx2(3, jj, 4)];
    state.CHORD2[jji] = state.CHORD1[jj];

    state.RLE[idx2(1, jji, 4)] = state.RLE[idx2(1, jj, 4)];
    state.RLE[idx2(2, jji, 4)] = f32(-state.RLE[idx2(2, jj, 4)] + yoff);
    state.RLE[idx2(3, jji, 4)] = state.RLE[idx2(3, jj, 4)];
    state.CHORD[jji] = state.CHORD[jj];
    state.WSTRIP[jji] = state.WSTRIP[jj];
    state.TANLE[jji] = f32(-state.TANLE[jj]);
    state.AINC[jji] = state.AINC[jj];
    state.LSSURF[jji] = newSurf;

    if (state.AINC_G && state.NDESIGN) {
      for (let n = 1; n <= state.NDESIGN; n += 1) {
        state.AINC_G[idx2(jji, n, state.NSTRMAX + 1)] = state.AINC_G[idx2(jj, n, state.NSTRMAX + 1)];
      }
    }

    for (let l = 1; l <= 6; l += 1) {
      state.CLCD[idx2(l, jji, 6)] = state.CLCD[idx2(l, jj, 6)];
    }
    state.LVISCSTRP[jji] = state.LVISCSTRP[jj];

    state.IJFRST[jji] = state.NVOR + 1;
    state.NVSTRP[jji] = nvc;

    for (let ivc = 1; ivc <= nvc; ivc += 1) {
      state.NVOR += 1;
      const iii = state.IJFRST[jji] + ivc - 1;
      const ii = state.IJFRST[jj] + ivc - 1;

      state.RV1[idx2(1, iii, 4)] = state.RV2[idx2(1, ii, 4)];
      state.RV1[idx2(2, iii, 4)] = f32(-state.RV2[idx2(2, ii, 4)] + yoff);
      state.RV1[idx2(3, iii, 4)] = state.RV2[idx2(3, ii, 4)];

      state.RV2[idx2(1, iii, 4)] = state.RV1[idx2(1, ii, 4)];
      state.RV2[idx2(2, iii, 4)] = f32(-state.RV1[idx2(2, ii, 4)] + yoff);
      state.RV2[idx2(3, iii, 4)] = state.RV1[idx2(3, ii, 4)];

      state.RV[idx2(1, iii, 4)] = state.RV[idx2(1, ii, 4)];
      state.RV[idx2(2, iii, 4)] = f32(-state.RV[idx2(2, ii, 4)] + yoff);
      state.RV[idx2(3, iii, 4)] = state.RV[idx2(3, ii, 4)];

      state.RC[idx2(1, iii, 4)] = state.RC[idx2(1, ii, 4)];
      state.RC[idx2(2, iii, 4)] = f32(-state.RC[idx2(2, ii, 4)] + yoff);
      state.RC[idx2(3, iii, 4)] = state.RC[idx2(3, ii, 4)];

      state.RS[idx2(1, iii, 4)] = state.RS[idx2(1, ii, 4)];
      state.RS[idx2(2, iii, 4)] = f32(-state.RS[idx2(2, ii, 4)] + yoff);
      state.RS[idx2(3, iii, 4)] = state.RS[idx2(3, ii, 4)];

      state.SLOPEC[iii] = state.SLOPEC[ii];
      state.SLOPEV[iii] = state.SLOPEV[ii];
      state.DXV[iii] = state.DXV[ii];
      state.CHORDV[iii] = state.CHORDV[ii];
      state.LVCOMP[iii] = state.LNCOMP[newSurf];
      state.LVALBE[iii] = state.LVALBE[ii];
      state.LVNC[iii] = state.LVNC[ii];

      for (let n = 1; n <= state.NCONTROL; n += 1) {
        const rsgn = state.VREFL[idx2(ii, n, state.NVMAX + 1)];
        state.DCONTROL[idx2(iii, n, state.NVMAX + 1)] = f32(
          -state.DCONTROL[idx2(ii, n, state.NVMAX + 1)] * rsgn,
        );
        state.VREFL[idx2(iii, n, state.NVMAX + 1)] = state.VREFL[idx2(ii, n, state.NVMAX + 1)];
        state.VHINGE[idx3(1, iii, n, 4, state.NVMAX + 1)] = state.VHINGE[idx3(1, ii, n, 4, state.NVMAX + 1)];
        state.VHINGE[idx3(2, iii, n, 4, state.NVMAX + 1)] = f32(
          -state.VHINGE[idx3(2, ii, n, 4, state.NVMAX + 1)],
        );
        state.VHINGE[idx3(3, iii, n, 4, state.NVMAX + 1)] = state.VHINGE[idx3(3, ii, n, 4, state.NVMAX + 1)];
        state.PHINGE[idx3(1, iii, n, 4, state.NVMAX + 1)] = state.PHINGE[idx3(1, ii, n, 4, state.NVMAX + 1)];
        state.PHINGE[idx3(2, iii, n, 4, state.NVMAX + 1)] = f32(
          -state.PHINGE[idx3(2, ii, n, 4, state.NVMAX + 1)] + yoff,
        );
        state.PHINGE[idx3(3, iii, n, 4, state.NVMAX + 1)] = state.PHINGE[idx3(3, ii, n, 4, state.NVMAX + 1)];
      }
    }
  }
}

export function ENCALC(state) {
  const debug = state.__debug;
  const { NSTRIP, NCONTROL, NDESIGN } = state;
  const dimN = state.NVMAX + 1;
  const SAXFR = state.SAXFR ?? 0.0;
  const RV1 = state.RV1;
  const RV2 = state.RV2;
  const RV = state.RV;
  const AINC = state.AINC;
  const SLOPEC = state.SLOPEC;
  const SLOPEV = state.SLOPEV;
  const IJFRST = state.IJFRST;
  const NVSTRP = state.NVSTRP;
  const ENC = state.ENC;
  const ENV = state.ENV;
  const ESS = state.ESS;
  const ENSY = state.ENSY;
  const ENSZ = state.ENSZ;
  const XSREF = state.XSREF;
  const YSREF = state.YSREF;
  const ZSREF = state.ZSREF;
  const LSTRIPOFF = state.LSTRIPOFF;
  const WSTRIP = state.WSTRIP;
  const DCONTROL = state.DCONTROL;
  const VHINGE = state.VHINGE;
  const VREFL = state.VREFL;
  const PHINGE = state.PHINGE;
  const LVNC = state.LVNC;
  const ENC_D = state.ENC_D;
  const ENV_D = state.ENV_D;

  if (debug) debug({ step: 'ENCALC start', NSTRIP });
  for (let j = 1; j <= NSTRIP; j += 1) {
    let i = IJFRST[j];
    const dxle = f32(RV2[idx2(1, i, 4)] - RV1[idx2(1, i, 4)]);
    const dyle = f32(RV2[idx2(2, i, 4)] - RV1[idx2(2, i, 4)]);
    const dzle = f32(RV2[idx2(3, i, 4)] - RV1[idx2(3, i, 4)]);
    const axle = RV[idx2(1, i, 4)];
    const ayle = RV[idx2(2, i, 4)];
    const azle = RV[idx2(3, i, 4)];

    i = IJFRST[j] + (NVSTRP[j] - 1);
    const dxte = f32(RV2[idx2(1, i, 4)] - RV1[idx2(1, i, 4)]);
    const dyte = f32(RV2[idx2(2, i, 4)] - RV1[idx2(2, i, 4)]);
    const dzte = f32(RV2[idx2(3, i, 4)] - RV1[idx2(3, i, 4)]);
    const axte = RV[idx2(1, i, 4)];
    const ayte = RV[idx2(2, i, 4)];
    const azte = RV[idx2(3, i, 4)];

    const dxt = f32((1.0 - SAXFR) * dxle + SAXFR * dxte);
    const dyt = f32((1.0 - SAXFR) * dyle + SAXFR * dyte);
    const dzt = f32((1.0 - SAXFR) * dzle + SAXFR * dzte);
    const dmag = f32(Math.sqrt(f32(dxt * dxt + dyt * dyt + dzt * dzt)));
    const yzmag = f32(Math.sqrt(f32(dyt * dyt + dzt * dzt)));
    if (!Number.isFinite(dmag) || dmag === 0.0 || !Number.isFinite(yzmag) || yzmag === 0.0 || WSTRIP[j] === 0.0) {
      LSTRIPOFF[j] = true;
      ESS[idx2(1, j, 4)] = 0.0;
      ESS[idx2(2, j, 4)] = 0.0;
      ESS[idx2(3, j, 4)] = 0.0;
      ENSY[j] = 0.0;
      ENSZ[j] = 0.0;

      const nvOff = NVSTRP[j];
      for (let ii = 1; ii <= nvOff; ii += 1) {
        const iv = IJFRST[j] + (ii - 1);
        ENC[idx2(1, iv, 4)] = 0.0;
        ENC[idx2(2, iv, 4)] = 0.0;
        ENC[idx2(3, iv, 4)] = 0.0;
        ENV[idx2(1, iv, 4)] = 0.0;
        ENV[idx2(2, iv, 4)] = 0.0;
        ENV[idx2(3, iv, 4)] = 0.0;
        LVNC[iv] = false;
      }
      continue;
    }

    ESS[idx2(1, j, 4)] = f32(dxt / dmag);
    ESS[idx2(2, j, 4)] = f32(dyt / dmag);
    ESS[idx2(3, j, 4)] = f32(dzt / dmag);

    ENSY[j] = f32(-dzt / yzmag);
    ENSZ[j] = f32(dyt / yzmag);

    XSREF[j] = f32((1.0 - SAXFR) * axle + SAXFR * axte);
    YSREF[j] = f32((1.0 - SAXFR) * ayle + SAXFR * ayte);
    ZSREF[j] = f32((1.0 - SAXFR) * azle + SAXFR * azte);

    const ES = [0.0, ENSY[j], ENSZ[j]];
    LSTRIPOFF[j] = false;

    const nv = NVSTRP[j];
    for (let ii = 1; ii <= nv; ii += 1) {
      const iv = IJFRST[j] + (ii - 1);
      for (let n = 1; n <= NCONTROL; n += 1) {
        ENV_D[idx3(1, iv, n, 4, dimN)] = 0.0;
        ENV_D[idx3(2, iv, n, 4, dimN)] = 0.0;
        ENV_D[idx3(3, iv, n, 4, dimN)] = 0.0;
        ENC_D[idx3(1, iv, n, 4, dimN)] = 0.0;
        ENC_D[idx3(2, iv, n, 4, dimN)] = 0.0;
        ENC_D[idx3(3, iv, n, 4, dimN)] = 0.0;
      }

      const dxb = f32(RV2[idx2(1, iv, 4)] - RV1[idx2(1, iv, 4)]);
      const dyb = f32(RV2[idx2(2, iv, 4)] - RV1[idx2(2, iv, 4)]);
      const dzb = f32(RV2[idx2(3, iv, 4)] - RV1[idx2(3, iv, 4)]);
      const emag = f32(Math.sqrt(f32(dxb * dxb + dyb * dyb + dzb * dzb)));
      const eb = [f32(dxb / emag), f32(dyb / emag), f32(dzb / emag)];

      let ang = f32(AINC[j] - Math.atan(SLOPEC[iv]));
      let sinc = f32(Math.sin(ang));
      let cosc = f32(Math.cos(ang));
      const ec = [
        cosc,
        f32(-sinc * ES[1]),
        f32(-sinc * ES[2]),
      ];

      const ecxb = zero3();
      CROSS(ec, eb, ecxb);
      let em = f32(Math.sqrt(f32(ecxb[0] * ecxb[0] + ecxb[1] * ecxb[1] + ecxb[2] * ecxb[2])));
      if (em !== 0.0) {
        ENC[idx2(1, iv, 4)] = f32(ecxb[0] / em);
        ENC[idx2(2, iv, 4)] = f32(ecxb[1] / em);
        ENC[idx2(3, iv, 4)] = f32(ecxb[2] / em);
      } else {
        ENC[idx2(1, iv, 4)] = ES[0];
        ENC[idx2(2, iv, 4)] = ES[1];
        ENC[idx2(3, iv, 4)] = ES[2];
      }

      ang = f32(AINC[j] - Math.atan(SLOPEV[iv]));
      sinc = f32(Math.sin(ang));
      cosc = f32(Math.cos(ang));
      const ecv = [
        cosc,
        f32(-sinc * ES[1]),
        f32(-sinc * ES[2]),
      ];
      const ecxbv = zero3();
      CROSS(ecv, eb, ecxbv);
      em = f32(Math.sqrt(f32(ecxbv[0] * ecxbv[0] + ecxbv[1] * ecxbv[1] + ecxbv[2] * ecxbv[2])));
      if (em !== 0.0) {
        ENV[idx2(1, iv, 4)] = f32(ecxbv[0] / em);
        ENV[idx2(2, iv, 4)] = f32(ecxbv[1] / em);
        ENV[idx2(3, iv, 4)] = f32(ecxbv[2] / em);
      } else {
        ENV[idx2(1, iv, 4)] = ES[0];
        ENV[idx2(2, iv, 4)] = ES[1];
        ENV[idx2(3, iv, 4)] = ES[2];
      }

      for (let n = 1; n <= NCONTROL; n += 1) {
        const idx = idx2(iv, n, dimN);
        if (DCONTROL[idx] === 0.0) continue;
        const angd = f32(state.DTR * DCONTROL[idx] * state.DELCON[n]);
        const angddc = f32(state.DTR * DCONTROL[idx]);
        const cosd = f32(Math.cos(angd));
        const sind = f32(Math.sin(angd));

        const vhinge = [
          VHINGE[idx3(1, iv, n, 4, dimN)],
          VHINGE[idx3(2, iv, n, 4, dimN)],
          VHINGE[idx3(3, iv, n, 4, dimN)],
        ];

        let end = dot3([ENC[idx2(1, iv, 4)], ENC[idx2(2, iv, 4)], ENC[idx2(3, iv, 4)]], vhinge);
        const ep = [
          f32(ENC[idx2(1, iv, 4)] - end * vhinge[0]),
          f32(ENC[idx2(2, iv, 4)] - end * vhinge[1]),
          f32(ENC[idx2(3, iv, 4)] - end * vhinge[2]),
        ];
        const eq = zero3();
        CROSS(vhinge, ep, eq);
        if (cosd || sind) {
          ENC_D[idx3(1, iv, n, 4, dimN)] = f32(ENC_D[idx3(1, iv, n, 4, dimN)] + eq[0] * angddc);
          ENC_D[idx3(2, iv, n, 4, dimN)] = f32(ENC_D[idx3(2, iv, n, 4, dimN)] + eq[1] * angddc);
          ENC_D[idx3(3, iv, n, 4, dimN)] = f32(ENC_D[idx3(3, iv, n, 4, dimN)] + eq[2] * angddc);
        }

        end = dot3([ENV[idx2(1, iv, 4)], ENV[idx2(2, iv, 4)], ENV[idx2(3, iv, 4)]], vhinge);
        const epv = [
          f32(ENV[idx2(1, iv, 4)] - end * vhinge[0]),
          f32(ENV[idx2(2, iv, 4)] - end * vhinge[1]),
          f32(ENV[idx2(3, iv, 4)] - end * vhinge[2]),
        ];
        const eqv = zero3();
        CROSS(vhinge, epv, eqv);
        if (cosd || sind) {
          ENV_D[idx3(1, iv, n, 4, dimN)] = f32(ENV_D[idx3(1, iv, n, 4, dimN)] + eqv[0] * angddc);
          ENV_D[idx3(2, iv, n, 4, dimN)] = f32(ENV_D[idx3(2, iv, n, 4, dimN)] + eqv[1] * angddc);
          ENV_D[idx3(3, iv, n, 4, dimN)] = f32(ENV_D[idx3(3, iv, n, 4, dimN)] + eqv[2] * angddc);
        }
      }

      LVNC[iv] = true;
    }
  }
}
