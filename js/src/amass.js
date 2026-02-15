/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL amass.f with float32 math for numerical fidelity.

import { CROSS } from './aic.js';

const f32 = Math.fround;

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function strip(str) {
  const trimmed = str.replace(/^\s+|\s+$/g, '');
  return { text: trimmed, len: trimmed.length };
}

function getflt(input, maxCount = 0) {
  const clean = input.split('!')[0];
  const matches = clean.match(/[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?/g) || [];
  const vals = matches.map((v) => Number(v.replace(/d/i, 'e')));
  if (maxCount > 0) {
    return vals.slice(0, maxCount);
  }
  return vals;
}

export function MASINI(state) {
  state.RMASS0 = f32(1.0);

  for (let k = 0; k < 3; k += 1) {
    for (let l = 0; l < 3; l += 1) {
      state.RINER0[idx2(k, l, 3)] = 0.0;
      state.AMASS[idx2(k, l, 3)] = 0.0;
      state.AINER[idx2(k, l, 3)] = 0.0;
    }
    state.RINER0[idx2(k, k, 3)] = f32(1.0);
  }

  state.XYZMASS0[0] = 0.0;
  state.XYZMASS0[1] = 0.0;
  state.XYZMASS0[2] = 0.0;
  state.LMASS = false;
  return state;
}

export function UNITSET(state) {
  state.UNITF = f32(state.UNITM * state.UNITL / f32(state.UNITT * state.UNITT));
  if (state.UNCHM && state.UNCHL && state.UNCHT) {
    state.UNCHF = `${state.UNCHM}-${state.UNCHL}/${state.UNCHT}^2`;
    let out = strip(state.UNCHF).text;
    if (out === 'slug-ft/s^2') out = 'lb';
    if (out === 'kg-m/s^2') out = 'N';
    if (out === 'g-cm/s^2') out = 'dyn';
    state.UNCHF = out;
    state.NUF = out.length;
  }

  state.UNITS = f32(state.UNITL * state.UNITL);
  if (state.UNCHL) {
    state.UNCHS = `${state.UNCHL}^2`;
    const out = strip(state.UNCHS);
    state.UNCHS = out.text;
    state.NUS = out.len;
  }

  state.UNITV = f32(state.UNITL / state.UNITT);
  if (state.UNCHL && state.UNCHT) {
    state.UNCHV = `${state.UNCHL}/${state.UNCHT}`;
    const out = strip(state.UNCHV);
    state.UNCHV = out.text;
    state.NUV = out.len;
  }

  state.UNITA = f32(state.UNITL / f32(state.UNITT * state.UNITT));
  if (state.UNCHL && state.UNCHT) {
    state.UNCHA = `${state.UNCHL}/${state.UNCHT}^2`;
    const out = strip(state.UNCHA);
    state.UNCHA = out.text;
    state.NUA = out.len;
  }

  state.UNITI = f32(state.UNITM * state.UNITL * state.UNITL);
  if (state.UNCHM && state.UNCHL) {
    state.UNCHI = `${state.UNCHM}-${state.UNCHL}^2`;
    const out = strip(state.UNCHI);
    state.UNCHI = out.text;
    state.NUI = out.len;
  }

  state.UNITD = f32(state.UNITM / f32(state.UNITL * state.UNITL * state.UNITL));
  if (state.UNCHM && state.UNCHL) {
    state.UNCHD = `${state.UNCHM}/${state.UNCHL}^3`;
    const out = strip(state.UNCHD);
    state.UNCHD = out.text;
    state.NUD = out.len;
  }

  return state;
}

export function PARNSET(state) {
  for (let ip = 0; ip < state.IPTOT; ip += 1) {
    state.PARUNCH[ip] = '';
  }

  state.PARUNCH[state.IPALFA] = 'deg';
  state.PARUNCH[state.IPBETA] = 'deg';
  state.PARUNCH[state.IPPHI] = 'deg';
  state.PARUNCH[state.IPTHE] = 'deg';
  state.PARUNCH[state.IPPSI] = 'deg';
  state.PARUNCH[state.IPVEE] = state.UNCHV ?? '';
  state.PARUNCH[state.IPRHO] = state.UNCHD ?? '';
  state.PARUNCH[state.IPGEE] = state.UNCHA ?? '';
  state.PARUNCH[state.IPRAD] = state.UNCHL ?? '';

  state.PARUNCH[state.IPXCG] = 'Lunit';
  state.PARUNCH[state.IPYCG] = 'Lunit';
  state.PARUNCH[state.IPZCG] = 'Lunit';

  state.PARUNCH[state.IPMASS] = state.UNCHM ?? '';
  state.PARUNCH[state.IPIXX] = state.UNCHI ?? '';
  state.PARUNCH[state.IPIYY] = state.UNCHI ?? '';
  state.PARUNCH[state.IPIZZ] = state.UNCHI ?? '';
  state.PARUNCH[state.IPIXY] = state.UNCHI ?? '';
  state.PARUNCH[state.IPIYZ] = state.UNCHI ?? '';
  state.PARUNCH[state.IPIZX] = state.UNCHI ?? '';

  return state;
}

export function MASGET(state, text) {
  let error = false;
  const lines = text.split(/\r?\n/);

  let sum_m = 0.0;
  let sum_mx = 0.0;
  let sum_my = 0.0;
  let sum_mz = 0.0;
  let sum_mxx = 0.0;
  let sum_myy = 0.0;
  let sum_mzz = 0.0;
  let sum_mxy = 0.0;
  let sum_mxz = 0.0;
  let sum_myz = 0.0;
  let sum_ixx = 0.0;
  let sum_iyy = 0.0;
  let sum_izz = 0.0;
  let sum_ixy = 0.0;
  let sum_ixz = 0.0;
  let sum_iyz = 0.0;

  const fac = new Float32Array(10);
  const add = new Float32Array(10);
  for (let k = 0; k < 10; k += 1) {
    fac[k] = 1.0;
    add[k] = 0.0;
  }

  state.UNITL = 1.0;
  state.UNITM = 1.0;
  state.UNITT = 1.0;
  state.UNCHL = 'Lunit';
  state.UNCHM = 'Munit';
  state.UNCHT = 'Tunit';
  state.NUL = 5;
  state.NUM = 5;
  state.NUT = 5;

  state.GEE0 = 1.0;
  state.RHO0 = 1.0;
  state.UNCHGEE = 'Lunit/Tunit^2';
  state.UNCHRHO = 'Munit/Lunit^3';
  state.NUGEE = 13;
  state.NURHO = 13;

  for (const rawLine of lines) {
    const line = rawLine;
    if (!line) continue;
    const first = line[0];
    if (first === '#' || first === '!') continue;

    if (first === '*') {
      const vals = getflt(line.slice(1), 10);
      for (let k = 0; k < vals.length; k += 1) {
        fac[k] = f32(vals[k]);
      }
      continue;
    }

    if (first === '+') {
      const vals = getflt(line.slice(1), 10);
      for (let k = 0; k < vals.length; k += 1) {
        add[k] = f32(vals[k]);
      }
      continue;
    }

    const eq = line.indexOf('=');
    if (eq > 0) {
      const key = line.slice(0, eq);
      const rest = strip(line.slice(eq + 1)).text;
      const parts = rest.split(/\s+/);
      const value = Number(parts[0]);
      const unit = rest.slice(parts[0].length).trim();

      if (key.includes('Lunit')) {
        state.UNITL = f32(value);
        state.UNCHL = unit;
        state.NUL = Math.max(1, unit.length);
        continue;
      }
      if (key.includes('Munit')) {
        state.UNITM = f32(value);
        state.UNCHM = unit;
        state.NUM = Math.max(1, unit.length);
        continue;
      }
      if (key.includes('Tunit')) {
        state.UNITT = f32(value);
        state.UNCHT = unit;
        state.NUT = Math.max(1, unit.length);
        continue;
      }
      if (key.includes('g')) {
        state.GEE0 = f32(value);
        state.UNCHGEE = unit;
        state.NUGEE = Math.max(1, unit.length);
        continue;
      }
      if (key.includes('rho')) {
        state.RHO0 = f32(value);
        state.UNCHRHO = unit;
        state.NURHO = Math.max(1, unit.length);
        continue;
      }
    }

    const rinp = getflt(line, 10);
    if (rinp.length === 0) {
      continue;
    }
    while (rinp.length < 10) rinp.push(0.0);

    const mi = f32(f32(fac[0] * rinp[0]) + add[0]);
    const xi = f32(f32(fac[1] * rinp[1]) + add[1]);
    const yi = f32(f32(fac[2] * rinp[2]) + add[2]);
    const zi = f32(f32(fac[3] * rinp[3]) + add[3]);
    const ixxi = f32(f32(fac[4] * rinp[4]) + add[4]);
    const iyyi = f32(f32(fac[5] * rinp[5]) + add[5]);
    const izzi = f32(f32(fac[6] * rinp[6]) + add[6]);
    const ixyi = f32(f32(fac[7] * rinp[7]) + add[7]);
    const ixzi = f32(f32(fac[8] * rinp[8]) + add[8]);
    const iyzi = f32(f32(fac[9] * rinp[9]) + add[9]);

    sum_m = f32(sum_m + mi);
    sum_mx = f32(sum_mx + f32(mi * xi));
    sum_my = f32(sum_my + f32(mi * yi));
    sum_mz = f32(sum_mz + f32(mi * zi));
    sum_mxx = f32(sum_mxx + f32(mi * xi * xi));
    sum_myy = f32(sum_myy + f32(mi * yi * yi));
    sum_mzz = f32(sum_mzz + f32(mi * zi * zi));
    sum_mxy = f32(sum_mxy + f32(mi * xi * yi));
    sum_mxz = f32(sum_mxz + f32(mi * xi * zi));
    sum_myz = f32(sum_myz + f32(mi * yi * zi));

    sum_ixx = f32(sum_ixx + ixxi);
    sum_iyy = f32(sum_iyy + iyyi);
    sum_izz = f32(sum_izz + izzi);
    sum_ixy = f32(sum_ixy + ixyi);
    sum_ixz = f32(sum_ixz + ixzi);
    sum_iyz = f32(sum_iyz + iyzi);
  }

  if (sum_m === 0.0) {
    error = true;
    state.LMASS = false;
    return { ERROR: error };
  }

  const xcg = f32(sum_mx / sum_m);
  const ycg = f32(sum_my / sum_m);
  const zcg = f32(sum_mz / sum_m);

  const ixx = f32(sum_ixx + f32(sum_myy + sum_mzz) - f32(sum_m * f32(ycg * ycg + zcg * zcg)));
  const iyy = f32(sum_iyy + f32(sum_mzz + sum_mxx) - f32(sum_m * f32(zcg * zcg + xcg * xcg)));
  const izz = f32(sum_izz + f32(sum_mxx + sum_myy) - f32(sum_m * f32(xcg * xcg + ycg * ycg)));
  const ixy = f32(sum_ixy + sum_mxy - f32(sum_m * f32(xcg * ycg)));
  const ixz = f32(sum_ixz + sum_mxz - f32(sum_m * f32(xcg * zcg)));
  const iyz = f32(sum_iyz + sum_myz - f32(sum_m * f32(ycg * zcg)));

  state.RMASS0 = f32(sum_m * state.UNITM);

  const inertiaScale = f32(state.UNITM * f32(state.UNITL * state.UNITL));
  state.RINER0[idx2(0, 0, 3)] = f32(ixx * inertiaScale);
  state.RINER0[idx2(0, 1, 3)] = f32(-ixy * inertiaScale);
  state.RINER0[idx2(0, 2, 3)] = f32(-ixz * inertiaScale);
  state.RINER0[idx2(1, 0, 3)] = f32(-ixy * inertiaScale);
  state.RINER0[idx2(1, 1, 3)] = f32(iyy * inertiaScale);
  state.RINER0[idx2(1, 2, 3)] = f32(-iyz * inertiaScale);
  state.RINER0[idx2(2, 0, 3)] = f32(-ixz * inertiaScale);
  state.RINER0[idx2(2, 1, 3)] = f32(-iyz * inertiaScale);
  state.RINER0[idx2(2, 2, 3)] = f32(izz * inertiaScale);

  state.XYZMASS0[0] = f32(xcg * state.UNITL);
  state.XYZMASS0[1] = f32(ycg * state.UNITL);
  state.XYZMASS0[2] = f32(zcg * state.UNITL);

  UNITSET(state);
  PARNSET(state);

  state.LMASS = true;
  return { ERROR: false };
}

export function MASPUT(state, IR1, IR2) {
  for (let ir = IR1; ir <= IR2; ir += 1) {
    state.PARVAL[idx2(state.IPMASS, ir, state.IPTOT)] = state.RMASS0;
    state.PARVAL[idx2(state.IPIXX, ir, state.IPTOT)] = state.RINER0[idx2(0, 0, 3)];
    state.PARVAL[idx2(state.IPIYY, ir, state.IPTOT)] = state.RINER0[idx2(1, 1, 3)];
    state.PARVAL[idx2(state.IPIZZ, ir, state.IPTOT)] = state.RINER0[idx2(2, 2, 3)];
    state.PARVAL[idx2(state.IPIXY, ir, state.IPTOT)] = state.RINER0[idx2(0, 1, 3)];
    state.PARVAL[idx2(state.IPIYZ, ir, state.IPTOT)] = state.RINER0[idx2(1, 2, 3)];
    state.PARVAL[idx2(state.IPIZX, ir, state.IPTOT)] = state.RINER0[idx2(2, 0, 3)];
    state.PARVAL[idx2(state.IPGEE, ir, state.IPTOT)] = state.GEE0;
    state.PARVAL[idx2(state.IPRHO, ir, state.IPTOT)] = state.RHO0;
    state.PARVAL[idx2(state.IPXCG, ir, state.IPTOT)] = f32(state.XYZMASS0[0] / state.UNITL);
    state.PARVAL[idx2(state.IPYCG, ir, state.IPTOT)] = f32(state.XYZMASS0[1] / state.UNITL);
    state.PARVAL[idx2(state.IPZCG, ir, state.IPTOT)] = f32(state.XYZMASS0[2] / state.UNITL);
  }
  return state;
}

export function MASSHO(state) {
  const lines = [];
  lines.push('');
  lines.push(`Mass        = ${state.RMASS0 / state.UNITM} Munit`);
  lines.push(`Mass        = ${state.RMASS0} ${state.UNCHM ?? ''}`);
  lines.push('');
  lines.push(`Ref. x,y,z  = ${state.XYZREF0?.join(' ') ?? ''} Lunit`);
  lines.push(`C.G. x,y,z  = ${(state.XYZMASS0[0] / state.UNITL)} ${(state.XYZMASS0[1] / state.UNITL)} ${(state.XYZMASS0[2] / state.UNITL)} Lunit`);
  lines.push(`C.G. x,y,z  = ${state.XYZMASS0[0]} ${state.XYZMASS0[1]} ${state.XYZMASS0[2]} ${state.UNCHL ?? ''}`);
  lines.push('');
  lines.push(`Ixx -Ixy -Ixz   | ${state.RINER0[idx2(0, 0, 3)]} ${state.RINER0[idx2(0, 1, 3)]} ${state.RINER0[idx2(0, 2, 3)]} |`);
  lines.push(`     Iyy -Iyz = | ${state.RINER0[idx2(1, 1, 3)]} ${state.RINER0[idx2(1, 2, 3)]} | ${state.UNCHI ?? ''}`);
  lines.push(`          Izz   | ${state.RINER0[idx2(2, 2, 3)]} |`);
  return lines;
}

export function APPSHO(state, RHO) {
  const rho = f32(RHO);
  const lines = [];
  lines.push('Apparent mass, inertia');
  lines.push('');
  lines.push(`mxx  mxy  mxz   | ${f32(state.AMASS[idx2(0, 0, 3)] * rho)} ${f32(state.AMASS[idx2(0, 1, 3)] * rho)} ${f32(state.AMASS[idx2(0, 2, 3)] * rho)} |`);
  lines.push(`     myy  myz = | ${f32(state.AMASS[idx2(1, 1, 3)] * rho)} ${f32(state.AMASS[idx2(1, 2, 3)] * rho)} | ${state.UNCHM ?? ''}`);
  lines.push(`          mzz   | ${f32(state.AMASS[idx2(2, 2, 3)] * rho)} |`);
  lines.push('');
  lines.push(`Ixx -Ixy -Ixz   | ${f32(state.AINER[idx2(0, 0, 3)] * rho)} ${f32(state.AINER[idx2(0, 1, 3)] * rho)} ${f32(state.AINER[idx2(0, 2, 3)] * rho)} |`);
  lines.push(`     Iyy -Iyz = | ${f32(state.AINER[idx2(1, 1, 3)] * rho)} ${f32(state.AINER[idx2(1, 2, 3)] * rho)} | ${state.UNCHI ?? ''}`);
  lines.push(`          Izz   | ${f32(state.AINER[idx2(2, 2, 3)] * rho)} |`);
  return lines;
}

export function APPGET(state) {
  for (let k = 0; k < 3; k += 1) {
    for (let l = 0; l < 3; l += 1) {
      state.AMASS[idx2(k, l, 3)] = 0.0;
      state.AINER[idx2(k, l, 3)] = 0.0;
    }
  }

  // Runtime geometry arrays are 1-based (AVL-style) with 4-value point stride.
  // Some unit tests use compact 0-based arrays with 3-value stride.
  const stripBase = (
    (state.CHORD?.length ?? 0) >= state.NSTRIP + 1
    && (state.WSTRIP?.length ?? 0) >= state.NSTRIP + 1
    && (state.ENSY?.length ?? 0) >= state.NSTRIP + 1
    && (state.ENSZ?.length ?? 0) >= state.NSTRIP + 1
  ) ? 1 : 0;
  const vecDim = (
    (state.RLE?.length ?? 0) >= 4 * (state.NSTRIP + stripBase)
    && (state.RLE1?.length ?? 0) >= 4 * (state.NSTRIP + stripBase)
    && (state.RLE2?.length ?? 0) >= 4 * (state.NSTRIP + stripBase)
  ) ? 4 : 3;
  // Runtime geometry built by MAKESURF stores vectors in 4-stride arrays with
  // component indices 1..3. Compact test fixtures store 3-stride vectors at 0..2.
  const coordBase = vecDim === 4 ? 1 : 0;

  const uc = new Float32Array(3);
  const us = new Float32Array(3);
  const un = new Float32Array(3);
  const rm = new Float32Array(3);
  const rxun = new Float32Array(3);

  for (let j = 0; j < state.NSTRIP; j += 1) {
    const jj = j + stripBase;
    const cr = f32(state.CHORD[jj] ?? 0.0);
    const sr = f32(cr * (state.WSTRIP[jj] ?? 0.0));

    un[0] = 0.0;
    un[1] = state.ENSY[jj] ?? 0.0;
    un[2] = state.ENSZ[jj] ?? 0.0;

    us[0] = f32(
      (state.RLE2[idx2(coordBase + 0, jj, vecDim)] ?? 0.0)
      - (state.RLE1[idx2(coordBase + 0, jj, vecDim)] ?? 0.0)
      + f32(0.5 * f32((state.CHORD2[jj] ?? 0.0) - (state.CHORD1[jj] ?? 0.0))),
    );
    us[1] = f32((state.RLE2[idx2(coordBase + 1, jj, vecDim)] ?? 0.0) - (state.RLE1[idx2(coordBase + 1, jj, vecDim)] ?? 0.0));
    us[2] = f32((state.RLE2[idx2(coordBase + 2, jj, vecDim)] ?? 0.0) - (state.RLE1[idx2(coordBase + 2, jj, vecDim)] ?? 0.0));
    const umag = f32(Math.sqrt(f32(us[0] * us[0] + us[1] * us[1] + us[2] * us[2])));
    if (umag > 0.0) {
      us[0] = f32(us[0] / umag);
      us[1] = f32(us[1] / umag);
      us[2] = f32(us[2] / umag);
    }

    CROSS(us, un, uc);

    rm[0] = f32((state.RLE[idx2(coordBase + 0, jj, vecDim)] ?? 0.0) + f32(0.5 * cr));
    rm[1] = state.RLE[idx2(coordBase + 1, jj, vecDim)] ?? 0.0;
    rm[2] = state.RLE[idx2(coordBase + 2, jj, vecDim)] ?? 0.0;

    CROSS(rm, un, rxun);

    const cperp = f32(cr * f32(f32(us[1] * un[2]) - f32(us[2] * un[1])));
    const appm = f32(sr * f32(0.25 * Math.PI) * cperp);
    const appi = f32(sr * f32(0.25 * Math.PI) * f32((cperp * cperp * cperp) / 64.0));

    const massScale = f32(state.UNITL * state.UNITL * state.UNITL);
    const inerScale = f32(state.UNITL * state.UNITL * state.UNITL * state.UNITL * state.UNITL);

    for (let k = 0; k < 3; k += 1) {
      for (let l = 0; l < 3; l += 1) {
        state.AMASS[idx2(k, l, 3)] = f32(
          state.AMASS[idx2(k, l, 3)] + f32(appm * un[k] * un[l] * massScale)
        );
        state.AINER[idx2(k, l, 3)] = f32(
          state.AINER[idx2(k, l, 3)]
          + f32(appm * rxun[k] * rxun[l] * inerScale)
          + f32(appi * us[k] * us[l] * inerScale)
        );
      }
    }
  }

  return state;
}
