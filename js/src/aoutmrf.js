/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL aoutmrf.f (MRF* routines) with float32 math for numerical fidelity.

const f32 = Math.fround;

function fmtES(value) {
  if (!Number.isFinite(value)) {
    return String(value);
  }
  return Number(value).toExponential(15);
}

function fmtI(value) {
  return `${Math.trunc(value)}`;
}

function stripString(str) {
  if (str == null) return { text: '', len: 0 };
  const trimmed = String(str).trim();
  return { text: trimmed, len: trimmed.length };
}

function GETSA(LSA) {
  if (LSA) {
    return { SATYPE: 'Standard axis orientation,  X fwd, Z down', DIR: -1.0 };
  }
  return { SATYPE: 'Geometric axis orientation,  X aft, Z up  ', DIR: 1.0 };
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function idx3(i, j, k, dim1, dim2) {
  return i + dim1 * (j + dim2 * k);
}

export function MRFTOT(state, LUN = 6, FILEID = '') {
  if (LUN === 0) return '';
  const { SATYPE, DIR } = GETSA(state.LNASA_SA);

  const CA = f32(Math.cos(state.ALFA));
  const SA = f32(Math.sin(state.ALFA));

  const RX_S = f32((state.WROT[0] * CA + state.WROT[2] * SA) * state.BREF / 2.0);
  const RY_S = f32(state.WROT[1] * state.CREF / 2.0);
  const RZ_S = f32((state.WROT[2] * CA - state.WROT[0] * SA) * state.BREF / 2.0);
  const RX_B = f32(state.WROT[0] * state.BREF / 2.0);
  const RY_B = f32(state.WROT[1] * state.CREF / 2.0);
  const RZ_B = f32(state.WROT[2] * state.BREF / 2.0);

  const CRSAX = f32(state.CMTOT[0] * CA + state.CMTOT[2] * SA);
  const CMSAX = f32(state.CMTOT[1]);
  const CNSAX = f32(state.CMTOT[2] * CA - state.CMTOT[0] * SA);

  const lines = [];
  lines.push(`mrftot: fileid = ${FILEID}`);
  lines.push(`${FILEID}`);
  lines.push('VERSION 1.0');
  lines.push('Vortex Lattice Output -- Total Forces');
  lines.push(String(state.TITLE ?? '').slice(0, 60));
  lines.push(`${fmtI(state.NSURF)}      | # surfaces`);
  lines.push(`${fmtI(state.NSTRIP)}      | # strips`);
  lines.push(`${fmtI(state.NVOR)}      | # vortices`);
  if (state.IYSYM > 0) lines.push(`${fmtES(state.YSYM)}      | Y Symmetry: Wall plane`);
  if (state.IYSYM < 0) lines.push(`${fmtES(state.YSYM)}      | Y Symmetry: Free surface`);
  if (state.IZSYM > 0) lines.push(`${fmtES(state.ZSYM)}      | Z Symmetry: Ground plane`);
  if (state.IZSYM < 0) lines.push(`${fmtES(state.ZSYM)}      | Z Symmetry: Free surface`);

  lines.push(`${fmtES(state.SREF)} ${fmtES(state.CREF)} ${fmtES(state.BREF)}      | Sref, Cref, Bref`);
  lines.push(`${fmtES(state.XYZREF[0])} ${fmtES(state.XYZREF[1])} ${fmtES(state.XYZREF[2])}      | Xref, Yref, Zref`);
  lines.push(SATYPE);
  lines.push(String(state.RTITLE?.[state.IRUN] ?? state.RTITLE?.[0] ?? '').trim());

  lines.push(`${fmtES(state.ALFA / state.DTR)} ${fmtES(DIR * RX_B)} ${fmtES(DIR * RX_S)}      | Alpha, pb/2V, p'b/2V`);
  lines.push(`${fmtES(state.BETA / state.DTR)} ${fmtES(RY_B)}      | Beta, qc/2V`);
  lines.push(`${fmtES(state.AMACH)} ${fmtES(DIR * RZ_B)} ${fmtES(DIR * RZ_S)}      | Mach, rb/2V, r'b/2V`);

  const CDITOT = f32(state.CDTOT - state.CDVTOT);
  lines.push(`${fmtES(DIR * state.CFTOT[0])} ${fmtES(DIR * state.CMTOT[0])} ${fmtES(DIR * CRSAX)}      | CXtot, Cltot, Cl'tot`);
  lines.push(`${fmtES(state.CFTOT[1])} ${fmtES(CMSAX)}      | CYtot, Cmtot`);
  lines.push(`${fmtES(DIR * state.CFTOT[2])} ${fmtES(DIR * state.CMTOT[2])} ${fmtES(DIR * CNSAX)}      | CZtot, Cntot, Cn'tot`);
  lines.push(`${fmtES(state.CLTOT)}      | CLtot`);
  lines.push(`${fmtES(state.CDTOT)}      | CDtot`);
  lines.push(`${fmtES(state.CDVTOT)} ${fmtES(CDITOT)}      | CDvis, CDind`);
  lines.push(`${fmtES(state.CLFF)} ${fmtES(state.CDFF)} ${fmtES(state.CYFF)} ${fmtES(state.SPANEF)}      | Trefftz Plane: CLff, CDff, CYff, e`);

  lines.push('CONTROL');
  lines.push(fmtI(state.NCONTROL));
  for (let k = 1; k <= state.NCONTROL; k += 1) {
    const name = state.DNAME?.[k] ?? state.DNAME?.[k - 1] ?? '';
    lines.push(`${fmtES(state.DELCON[k])}  ${name}`);
  }

  lines.push('DESIGN');
  lines.push(fmtI(state.NDESIGN));
  for (let k = 1; k <= state.NDESIGN; k += 1) {
    const name = state.GNAME?.[k] ?? state.GNAME?.[k - 1] ?? '';
    lines.push(`${fmtES(state.DELDES[k])}  ${name}`);
  }

  return lines.join('\n');
}

export function MRFSURF(state, LUN = 6) {
  if (LUN === 0) return '';
  const { SATYPE, DIR } = GETSA(state.LNASA_SA);
  const lines = [];
  lines.push('SURF');
  lines.push('VERSION 1.0');
  lines.push(SATYPE);
  lines.push(`${fmtES(state.SREF)} ${fmtES(state.CREF)} ${fmtES(state.BREF)}      | Sref, Cref, Bref`);
  lines.push(`${fmtES(state.XYZREF[0])} ${fmtES(state.XYZREF[1])} ${fmtES(state.XYZREF[2])}      | Xref, Yref, Zref`);
  lines.push(`${fmtI(state.NSURF)}      | # surfaces`);
  for (let n = 1; n <= state.NSURF; n += 1) {
    const title = state.STITLE?.[n] ?? state.STITLE?.[n - 1] ?? '';
    const { text } = stripString(title);
    lines.push('SURFACE');
    lines.push(text);
    lines.push(
      `${fmtI(n)} ${fmtES(state.SSURF[n])} ${fmtES(state.CLSURF[n])} ${fmtES(state.CDSURF[n])} ${fmtES(state.CMSURF[idx2(1, n, 3)])} `
      + `${fmtES(state.CYSURF[n])} ${fmtES(DIR * state.CMSURF[idx2(2, n, 3)])} ${fmtES(DIR * state.CMSURF[idx2(0, n, 3)])} `
      + `${fmtES(state.CDSURF[n] - state.CDVSURF[n])} ${fmtES(state.CDVSURF[n])} `
      + '| Surface Forces (referred to Sref,Cref,Bref, moments in body axes about Xref,Yref,Zref) : n Area CL CD Cm CY Cn Cl CDi CDv',
    );
    lines.push(
      `${fmtI(n)} ${fmtES(state.SSURF[n])} ${fmtES(state.CAVESURF[n])} ${fmtES(state.CL_LSRF[n])} ${fmtES(state.CD_LSRF[n])} `
      + `${fmtES(state.CDVSURF[n] * state.SREF / state.SSURF[n])} `
      + '| Surface Forces (referred to Ssurf, Cave about root LE on hinge axis) : n Ssurf Cave cl cd cdv',
    );
  }
  return lines.join('\n');
}

export function MRFBODY(state, LUN = 6) {
  if (LUN === 0) return '';
  const { SATYPE, DIR } = GETSA(state.LNASA_SA);
  const lines = [];
  lines.push('BODY');
  lines.push('VERSION 1.0');
  lines.push(SATYPE);
  lines.push(`${fmtES(state.SREF)} ${fmtES(state.CREF)} ${fmtES(state.BREF)}      | Sref, Cref, Bref`);
  lines.push(`${fmtES(state.XYZREF[0])} ${fmtES(state.XYZREF[1])} ${fmtES(state.XYZREF[2])}      | Xref, Yref, Zref`);
  lines.push(`${fmtI(state.NBODY)} | # bodies`);
  for (let ib = 1; ib <= state.NBODY; ib += 1) {
    const title = state.BTITLE?.[ib] ?? state.BTITLE?.[ib - 1] ?? '';
    const { text } = stripString(title);
    lines.push('BODY');
    lines.push(text);
    const ELBD = state.ELBDY[ib];
    const SBDY = state.SRFBDY[ib];
    const VBDY = state.VOLBDY[ib];
    lines.push(
      `${fmtI(ib)} ${fmtES(ELBD)} ${fmtES(SBDY)} ${fmtES(VBDY)} `
      + `${fmtES(state.CLBDY[ib])} ${fmtES(state.CDBDY[ib])} ${fmtES(state.CMBDY[idx2(1, ib, 3)])} `
      + `${fmtES(state.CFBDY[idx2(1, ib, 3)])} ${fmtES(DIR * state.CMBDY[idx2(2, ib, 3)])} ${fmtES(DIR * state.CMBDY[idx2(0, ib, 3)])} `
      + '| Body Forces (referred to Sref,Cref,Bref about Xref,Yref,Zref) : Ibdy Length Asurf Vol CL CD Cm CY Cn Cl',
    );
  }
  return lines.join('\n');
}

export function MRFSTRP(state, LUN = 6) {
  if (LUN === 0) return '';
  const { SATYPE, DIR } = GETSA(state.LNASA_SA);
  const lines = [];
  lines.push('STRP');
  lines.push('VERSION 1.0');
  lines.push(SATYPE);
  lines.push(`${fmtES(state.SREF)} ${fmtES(state.CREF)} ${fmtES(state.BREF)}      | Sref, Cref, Bref`);
  lines.push(`${fmtES(state.XYZREF[0])} ${fmtES(state.XYZREF[1])} ${fmtES(state.XYZREF[2])}      | Xref, Yref, Zref`);
  lines.push('Surface and Strip Forces by surface (referred to Sref,Cref,Bref about Xref,Yref,Zref)');
  lines.push(`${fmtI(state.NSURF)}      | surfaces`);
  for (let n = 1; n <= state.NSURF; n += 1) {
    const NS = state.NJ[n];
    const NV = state.NK[n];
    const J1 = state.JFRST[n];
    lines.push('SURFACE');
    lines.push(String(state.STITLE?.[n] ?? state.STITLE?.[n - 1] ?? ''));
    lines.push(`${fmtI(n)} ${fmtI(NV)} ${fmtI(NS)} ${fmtI(J1)}   | Surface #, # Chordwise, # Spanwise, First strip`);
    lines.push(`${fmtES(state.SSURF[n])} ${fmtES(state.CAVESURF[n])}   | Surface area Ssurf, Ave. chord Cave`);

    const CDISURF = f32(state.CDSURF[n] - state.CDVSURF[n]);
    lines.push(
      `${fmtES(state.CLSURF[n])} ${fmtES(DIR * state.CMSURF[idx2(0, n, 3)])} `
      + `${fmtES(state.CYSURF[n])} ${fmtES(state.CMSURF[idx2(1, n, 3)])} `
      + `${fmtES(state.CDSURF[n])} ${fmtES(DIR * state.CMSURF[idx2(2, n, 3)])} `
      + `${fmtES(CDISURF)} ${fmtES(state.CDVSURF[n])} `
      + '| CLsurf, Clsurf, CYsurf, Cmsurf, CDsurf, Cnsurf, CDisurf, CDvsurf; Forces referred to Sref, Cref, Bref about Xref, Yref, Zref',
    );

    lines.push(
      `${fmtES(state.CL_LSRF[n])} ${fmtES(state.CD_LSRF[n])}   | CL_srf CD_srf; Forces referred to Ssurf, Cave`,
    );

    lines.push('Strip Forces referred to Strip Area, Chord');
    lines.push('j, Xle, Yle, Zle, Chord, Area, c_cl, ai, cl_perp, cl, cd, cdv, cm_c/4, cm_LE, C.P.x/c');

    for (let jj = 1; jj <= NS; jj += 1) {
      const j = J1 + jj - 1;
      const astrp = f32(state.WSTRIP[j] * state.CHORD[j]);
      let xcp = 999.0;
      if (state.CL_LSTRP[j] !== 0.0) {
        xcp = f32(0.25 - state.CMC4_LSTRP[j] / state.CL_LSTRP[j]);
      }
      lines.push(
        `${fmtI(j)} ${fmtES(state.RLE[idx2(0, j, 3)])} ${fmtES(state.RLE[idx2(1, j, 3)])} ${fmtES(state.RLE[idx2(2, j, 3)])} `
        + `${fmtES(state.CHORD[j])} ${fmtES(astrp)} ${fmtES(state.CNC[j])} ${fmtES(state.DWWAKE[j])} `
        + `${fmtES(state.CLT_LSTRP[j])} ${fmtES(state.CL_LSTRP[j])} ${fmtES(state.CD_LSTRP[j])} ${fmtES(state.CDV_LSTRP[j])} `
        + `${fmtES(state.CMC4_LSTRP[j])} ${fmtES(state.CMLE_LSTRP[j])} ${fmtES(xcp)}`,
      );
    }
  }
  return lines.join('\n');
}

export function MRFELE(state, LUN = 6) {
  if (LUN === 0) return '';
  const { SATYPE, DIR } = GETSA(state.LNASA_SA);
  const lines = [];
  lines.push('ELE');
  lines.push('VERSION 1.0');
  lines.push(SATYPE);
  lines.push(`${fmtES(state.SREF)} ${fmtES(state.CREF)} ${fmtES(state.BREF)}      | Sref, Cref, Bref`);
  lines.push(`${fmtES(state.XYZREF[0])} ${fmtES(state.XYZREF[1])} ${fmtES(state.XYZREF[2])}      | Xref, Yref, Zref`);
  lines.push('Vortex Strengths (by surface, by strip)');
  lines.push(`${fmtI(state.NSURF)}      | # surfaces`);
  for (let n = 1; n <= state.NSURF; n += 1) {
    const NS = state.NJ[n];
    const NV = state.NK[n];
    const J1 = state.JFRST[n];
    lines.push('SURFACE');
    lines.push(String(state.STITLE?.[n] ?? state.STITLE?.[n - 1] ?? ''));
    lines.push(`${fmtI(n)} ${fmtI(NV)} ${fmtI(NS)} ${fmtI(J1)}   | Surface #, # Chordwise, # Spanwise, First strip`);
    lines.push(`${fmtES(state.SSURF[n])} ${fmtES(state.CAVESURF[n])}   | Surface area, Ave. chord`);

    const CDISURF = f32(state.CDSURF[n] - state.CDVSURF[n]);
    lines.push(
      `${fmtES(state.CLSURF[n])} ${fmtES(DIR * state.CMSURF[idx2(0, n, 3)])} `
      + `${fmtES(state.CYSURF[n])} ${fmtES(state.CMSURF[idx2(1, n, 3)])} `
      + `${fmtES(state.CDSURF[n])} ${fmtES(DIR * state.CMSURF[idx2(2, n, 3)])} `
      + `${fmtES(CDISURF)} ${fmtES(state.CDVSURF[n])} `
      + '| CLsurf, Clsurf, CYsurf, Cmsurf, CDsurf, Cnsurf, CDisurf, CDvsurf',
    );
    lines.push(
      `${fmtES(state.CL_LSRF[n])} ${fmtES(state.CD_LSRF[n])}   | CL_srf CD_srf; Forces referred to Ssurf, Cave about hinge axis thru LE`,
    );

    for (let jj = 1; jj <= NS; jj += 1) {
      const j = J1 + jj - 1;
      const I1 = state.IJFRST[j];
      const astrp = f32(state.WSTRIP[j] * state.CHORD[j]);
      const dihed = f32(-Math.atan2(state.ENSY[j], state.ENSZ[j]) / state.DTR);
      lines.push('STRIP');
      lines.push(`${fmtI(j)} ${fmtI(NV)} ${fmtI(I1)}  | Strip #, # Chordwise, First Vortex`);
      lines.push(
        `${fmtES(state.RLE[idx2(0, j, 3)])} ${fmtES(state.CHORD[j])} ${fmtES(state.AINC[j] / state.DTR)} `
        + `${fmtES(state.RLE[idx2(1, j, 3)])} ${fmtES(state.WSTRIP[j])} ${fmtES(astrp)} `
        + `${fmtES(state.RLE[idx2(2, j, 3)])} ${fmtES(dihed)} `
        + '| Xle, Ave. Chord, Incidence (deg), Yle, Strip Width, Strip Area, Zle, Strip Dihed (deg)',
      );
      lines.push(
        `${fmtES(state.CL_LSTRP[j])} ${fmtES(state.CD_LSTRP[j])} ${fmtES(state.CDV_LSTRP[j])} `
        + `${fmtES(state.CN_LSTRP[j])} ${fmtES(state.CA_LSTRP[j])} `
        + `${fmtES(state.CNC[j])} ${fmtES(state.DWWAKE[j])} `
        + `${fmtES(state.CMLE_LSTRP[j])} ${fmtES(state.CMC4_LSTRP[j])} `
        + '| cl, cd, cdv, cn, ca, cnc, wake dnwsh, cmLE, cm c/4',
      );

      for (let ii = 1; ii <= NV; ii += 1) {
        const i = I1 + ii - 1;
        const xm = f32(0.5 * (state.RV1[idx2(0, i, 3)] + state.RV2[idx2(0, i, 3)]));
        const ym = f32(0.5 * (state.RV1[idx2(1, i, 3)] + state.RV2[idx2(1, i, 3)]));
        const zm = f32(0.5 * (state.RV1[idx2(2, i, 3)] + state.RV2[idx2(2, i, 3)]));
        lines.push(
          `${fmtI(i)} ${fmtES(xm)} ${fmtES(ym)} ${fmtES(zm)} ${fmtES(state.DXV[i])} ${fmtES(state.SLOPEC[i])} ${fmtES(state.DCP[i])} `
          + '| I, X, Y, Z, DX, Slope, dCp',
        );
      }
    }
  }
  return lines.join('\n');
}

export function MRFHINGE(state, LUN = 6) {
  if (LUN === 0) return '';
  const { SATYPE } = GETSA(state.LNASA_SA);
  const lines = [];
  lines.push('HINGE');
  lines.push('VERSION 1.0');
  lines.push(SATYPE);
  lines.push(`${fmtES(state.SREF)} ${fmtES(state.CREF)}      | Sref, Cref`);
  lines.push(`${fmtI(state.NCONTROL)} | # controls`);
  for (let n = 1; n <= state.NCONTROL; n += 1) {
    const name = state.DNAME?.[n] ?? state.DNAME?.[n - 1] ?? '';
    lines.push(
      `${fmtES(state.CHINGE[n])}  ${name}  | Control Hinge Moments (referred to Sref, Cref) : Chinge, Control`,
    );
  }
  return lines.join('\n');
}

export function MRFCNC(state, LUN = 6) {
  if (LUN === 0) return '';
  const lines = [];
  lines.push('CNC');
  lines.push('VERSION 1.0');
  lines.push('Strip Loadings:  XM, YM, ZM, CNCM, CLM, CHM, DYM, ASM');
  lines.push(`${fmtI(state.NSTRIP)}   | # strips`);
  for (let j = 1; j <= state.NSTRIP; j += 1) {
    const i = state.IJFRST[j];
    const xm = f32(0.5 * (state.RV1[idx2(0, i, 3)] + state.RV2[idx2(0, i, 3)]));
    const ym = f32(0.5 * (state.RV1[idx2(1, i, 3)] + state.RV2[idx2(1, i, 3)]));
    const zm = f32(0.5 * (state.RV1[idx2(2, i, 3)] + state.RV2[idx2(2, i, 3)]));
    const cncm = state.CNC[j];
    const clm = state.CL_LSTRP[j];
    const chm = state.CHORD[j];
    const dym = state.WSTRIP[j];
    const asm = f32(dym * chm);
    lines.push(`${fmtES(xm)} ${fmtES(ym)} ${fmtES(zm)} ${fmtES(cncm)} ${fmtES(clm)} ${fmtES(chm)} ${fmtES(dym)} ${fmtES(asm)}`);
  }
  return lines.join('\n');
}

export function MRFVM(state, LUN = 6) {
  if (LUN === 0) return '';
  const lines = [];
  lines.push('VM');
  lines.push('VERSION 1.0');
  lines.push('Shear/q and Bending Moment/q vs Y');
  lines.push(String(state.TITLE ?? '').slice(0, 60));
  lines.push(
    `${fmtES(state.AMACH)} ${fmtES(state.ALFA / state.DTR)} ${fmtES(state.CLTOT)} ${fmtES(state.BETA / state.DTR)} ${fmtES(state.SREF)} ${fmtES(state.BREF)} `
    + '| Mach, alpha, CLtot, beta, Sref, Bref',
  );
  lines.push(`${fmtI(state.NSURF)}      | # surfaces`);

  for (let n = 1; n <= state.NSURF; n += 1) {
    const J1 = state.JFRST[n];
    const JN = J1 + state.NJ[n] - 1;
    lines.push('SURFACE');
    lines.push(String(state.STITLE?.[n] ?? state.STITLE?.[n - 1] ?? ''));
    lines.push(`${fmtI(n)} ${fmtI(state.NJ[n])}   | Surface #, # strips`);

    let ymin = 1.0e10;
    let ymax = -1.0e10;
    for (let j = J1; j <= JN; j += 1) {
      const iv = state.IJFRST[j];
      ymin = Math.min(ymin, state.RV1[idx2(1, iv, 3)], state.RV2[idx2(1, iv, 3)]);
      ymax = Math.max(ymax, state.RV1[idx2(1, iv, 3)], state.RV2[idx2(1, iv, 3)]);
    }

    let cnclst = 0.0;
    let bmlst = 0.0;
    let wlst = 0.0;
    let vlst = 0.0;

    const jf = J1;
    const jl = JN;
    const jinc = 1;
    const V = new Float32Array(state.NJ[n] + 2);
    const BM = new Float32Array(state.NJ[n] + 2);
    const YSTRP = new Float32Array(state.NJ[n] + 2);

    let dy = 0.0;
    for (let j = jl; j >= jf; j -= jinc) {
      const jj = jinc * (j - jf + jinc);
      dy = f32(0.5 * (state.WSTRIP[j] + wlst));
      YSTRP[jj] = state.RLE[idx2(1, j, 3)];
      V[jj] = f32(vlst + 0.5 * (state.CNC[j] + cnclst) * dy);
      BM[jj] = f32(bmlst + 0.5 * (V[jj] + vlst) * dy);
      vlst = V[jj];
      bmlst = BM[jj];
      cnclst = state.CNC[j];
      wlst = state.WSTRIP[j];
    }

    const vroot = f32(vlst + cnclst * 0.5 * dy);
    const bmroot = f32(bmlst + 0.5 * (vroot + vlst) * 0.5 * dy);
    const vtip = 0.0;
    const bmtip = 0.0;
    let yroot;
    let ytip;
    if (state.IMAGS[n] >= 0) {
      yroot = state.RLE1[idx2(1, J1, 3)];
      ytip = state.RLE2[idx2(1, JN, 3)];
    } else {
      yroot = state.RLE2[idx2(1, J1, 3)];
      ytip = state.RLE1[idx2(1, JN, 3)];
    }

    let dir = 1.0;
    if (ymin + ymax < 0.0) dir = -1.0;
    lines.push(`${fmtES(2.0 * ymin / state.BREF)} ${fmtES(2.0 * ymax / state.BREF)}  | 2Ymin/Bref, 2Ymax/Bref`);
    lines.push(
      `${fmtES(2.0 * yroot / state.BREF)} ${fmtES(vroot / state.SREF)} ${fmtES(dir * bmroot / state.SREF / state.BREF)} `
      + '| 2Y/Bref, Vz/(q*Sref), Mx/(q*Bref*Sref) : root',
    );
    for (let j = 1; j <= state.NJ[n]; j += 1) {
      lines.push(
        `${fmtES(2.0 * YSTRP[j] / state.BREF)} ${fmtES(V[j] / state.SREF)} ${fmtES(dir * BM[j] / state.SREF / state.BREF)} `
        + '| 2Y/Bref, Vz/(q*Sref), Mx/(q*Bref*Sref)',
      );
    }
    lines.push(
      `${fmtES(2.0 * ytip / state.BREF)} ${fmtES(vtip / state.SREF)} ${fmtES(dir * bmtip / state.SREF / state.BREF)} `
      + '| 2Y/Bref, Vz/(q*Sref), Mx/(q*Bref*Sref) : tip',
    );
  }
  return lines.join('\n');
}
