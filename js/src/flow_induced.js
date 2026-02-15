/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */

const PI4INV = 0.079577472;

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

export function addInducedVelocityFromVorvelc(
  px, py, pz,
  lbound,
  x1, y1, z1,
  x2, y2, z2,
  beta,
  rcore,
  gammaScale,
  acc,
) {
  const bx = Math.abs(Number(beta) || 0) > 1e-8 ? Number(beta) : 1;
  const ax1 = (x1 - px) / bx;
  const ay1 = y1 - py;
  const az1 = z1 - pz;
  const bx1 = (x2 - px) / bx;
  const by1 = y2 - py;
  const bz1 = z2 - pz;

  const asq = (ax1 * ax1) + (ay1 * ay1) + (az1 * az1);
  const bsq = (bx1 * bx1) + (by1 * by1) + (bz1 * bz1);
  const amag = Math.sqrt(Math.max(0, asq));
  const bmag = Math.sqrt(Math.max(0, bsq));
  const rc = Math.max(1e-10, Number(rcore) || 0);
  const rcore2 = rc * rc;
  const rcore4 = rcore2 * rcore2;

  let u = 0;
  let v = 0;
  let w = 0;

  if (lbound && (amag * bmag) !== 0) {
    const axb1 = (ay1 * bz1) - (az1 * by1);
    const axb2 = (az1 * bx1) - (ax1 * bz1);
    const axb3 = (ax1 * by1) - (ay1 * bx1);
    const axbsq = (axb1 * axb1) + (axb2 * axb2) + (axb3 * axb3);
    if (axbsq !== 0) {
      const adb = (ax1 * bx1) + (ay1 * by1) + (az1 * bz1);
      const alsq = (asq + bsq) - (2 * adb);
      const t1 = bsq - adb;
      const t2 = asq - adb;
      const s1 = Math.sqrt(Math.sqrt((bsq * bsq) + rcore4));
      const s2 = Math.sqrt(Math.sqrt((asq * asq) + rcore4));
      const num = (t1 / (s1 || 1e-12)) + (t2 / (s2 || 1e-12));
      const den = Math.sqrt((axbsq * axbsq) + (alsq * alsq * rcore4));
      if (den > 1e-16) {
        const t = num / den;
        u = axb1 * t;
        v = axb2 * t;
        w = axb3 * t;
      }
    }
  }

  if (amag !== 0) {
    const axisq = (az1 * az1) + (ay1 * ay1);
    const t = -((1 - (ax1 / amag)) / Math.sqrt((axisq * axisq) + rcore4));
    v += az1 * t;
    w -= ay1 * t;
  }
  if (bmag !== 0) {
    const bxisq = (bz1 * bz1) + (by1 * by1);
    const t = (1 - (bx1 / bmag)) / Math.sqrt((bxisq * bxisq) + rcore4);
    v += bz1 * t;
    w -= by1 * t;
  }

  u = (u * PI4INV) / bx;
  v *= PI4INV;
  w *= PI4INV;

  const s = Number(gammaScale) || 0;
  if (!Number.isFinite(s) || s === 0) return;
  acc[0] += u * s;
  acc[1] += v * s;
  acc[2] += w * s;
}

export function addInducedVelocityFromHorseshoe(px, py, pz, horse, solverParams, acc) {
  const gamma = Number(horse?.gamma) || 0;
  if (!Number.isFinite(gamma) || Math.abs(gamma) < 1e-12) return;
  const beta = Math.abs(Number(solverParams?.beta) || 0) > 1e-8 ? Number(solverParams.beta) : 1;
  const core = Math.max(1e-7, Number(horse?.core) || 0);
  const x1 = Number(horse?.x1) || 0;
  const y1 = Number(horse?.y1) || 0;
  const z1 = Number(horse?.z1) || 0;
  const x2 = Number(horse?.x2) || 0;
  const y2 = Number(horse?.y2) || 0;
  const z2 = Number(horse?.z2) || 0;
  addInducedVelocityFromVorvelc(px, py, pz, true, x1, y1, z1, x2, y2, z2, beta, core, gamma, acc);

  const fysym = Number(solverParams?.iysym) || 0;
  const fzsym = Number(solverParams?.izsym) || 0;
  const yoff = 2 * (Number(solverParams?.ysym) || 0);
  const zoff = 2 * (Number(solverParams?.zsym) || 0);
  if (fysym !== 0) {
    addInducedVelocityFromVorvelc(
      px, py, pz, true,
      x2, yoff - y2, z2,
      x1, yoff - y1, z1,
      beta, core, gamma * fysym, acc,
    );
  }
  if (fzsym !== 0) {
    addInducedVelocityFromVorvelc(
      px, py, pz, true,
      x2, y2, zoff - z2,
      x1, y1, zoff - z1,
      beta, core, gamma * fzsym, acc,
    );
    if (fysym !== 0) {
      addInducedVelocityFromVorvelc(
        px, py, pz, true,
        x1, yoff - y1, zoff - z1,
        x2, yoff - y2, zoff - z2,
        beta, core, gamma * fysym * fzsym, acc,
      );
    }
  }
}

export function computeInducedVelocityFromHorseshoes(px, py, pz, horseshoes, solverParams, out = [0, 0, 0]) {
  out[0] = 0;
  out[1] = 0;
  out[2] = 0;
  if (!Array.isArray(horseshoes) || !horseshoes.length) return out;
  for (let i = 0; i < horseshoes.length; i += 1) {
    addInducedVelocityFromHorseshoe(px, py, pz, horseshoes[i], solverParams, out);
  }
  return out;
}

export function buildSolverHorseshoesFromExec(result) {
  const isArrayLike = (v) => Array.isArray(v) || ArrayBuffer.isView(v);
  const gam = result?.GAM;
  const rv1 = result?.RV1;
  const rv2 = result?.RV2;
  const nvor = Math.max(0, Math.floor(Number(result?.NVOR) || 0));
  if (!isArrayLike(gam)
      || !isArrayLike(rv1)
      || !isArrayLike(rv2)
      || nvor <= 0) {
    return {
      horseshoes: [],
      beta: Number(result?.BETM) || 1,
      iysym: Number(result?.IYSYM) || 0,
      izsym: Number(result?.IZSYM) || 0,
      ysym: Number(result?.YSYM) || 0,
      zsym: Number(result?.ZSYM) || 0,
    };
  }

  const dim = 4;
  const maxIv = Math.min(
    nvor,
    Math.floor(Math.min(rv1.length, rv2.length) / dim) - 1,
    gam.length - 1,
  );
  if (maxIv <= 0) {
    return {
      horseshoes: [],
      beta: Number(result?.BETM) || 1,
      iysym: Number(result?.IYSYM) || 0,
      izsym: Number(result?.IZSYM) || 0,
      ysym: Number(result?.YSYM) || 0,
      zsym: Number(result?.ZSYM) || 0,
    };
  }

  const horseshoes = [];
  for (let i = 1; i <= maxIv; i += 1) {
    const gamma = Number(gam[i]) || 0;
    if (!Number.isFinite(gamma) || Math.abs(gamma) < 1e-8) continue;

    const x1 = Number(rv1[idx2(1, i, dim)]);
    const y1 = Number(rv1[idx2(2, i, dim)]);
    const z1 = Number(rv1[idx2(3, i, dim)]);
    const x2 = Number(rv2[idx2(1, i, dim)]);
    const y2 = Number(rv2[idx2(2, i, dim)]);
    const z2 = Number(rv2[idx2(3, i, dim)]);
    if (![x1, y1, z1, x2, y2, z2].every(Number.isFinite)) continue;

    const dsyz = Math.hypot(y2 - y1, z2 - z1);
    const core = Math.max(1e-7, 1e-4 * dsyz);
    horseshoes.push({
      x1, y1, z1, x2, y2, z2, gamma, core,
    });
  }

  return {
    horseshoes,
    beta: Number(result?.BETM) || 1,
    iysym: Number(result?.IYSYM) || 0,
    izsym: Number(result?.IZSYM) || 0,
    ysym: Number(result?.YSYM) || 0,
    zsym: Number(result?.ZSYM) || 0,
  };
}
