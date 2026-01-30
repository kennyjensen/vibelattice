// Port of AVL spline.f with float32 math for numerical fidelity.

const f32 = Math.fround;
const NMAX = 1000;
const A = new Float32Array(NMAX);
const B = new Float32Array(NMAX);
const C = new Float32Array(NMAX);

function findIntervalIndex(ss, s, n) {
  let ilow = 1;
  let i = n;

  while (i - ilow > 1) {
    const imid = Math.floor((i + ilow) / 2);
    if (ss < s[imid - 1]) {
      i = imid;
    } else {
      ilow = imid;
    }
  }

  return i;
}

export function TRISOL(a, b, c, d, kk) {
  for (let k = 1; k < kk; k += 1) {
    const km = k - 1;
    const aKm = a[km];
    c[km] = f32(c[km] / aKm);
    d[km] = f32(d[km] / aKm);
    a[k] = f32(a[k] - f32(b[k] * c[km]));
    d[k] = f32(d[k] - f32(b[k] * d[km]));
  }

  d[kk - 1] = f32(d[kk - 1] / a[kk - 1]);

  for (let k = kk - 2; k >= 0; k -= 1) {
    d[k] = f32(d[k] - f32(c[k] * d[k + 1]));
  }

  return d;
}

export function SPLINE(X, XS, S, N) {
  if (N > NMAX) {
    throw new Error('SPLINE: array overflow, increase NMAX');
  }

  for (let i = 1; i <= N - 2; i += 1) {
    const dsm = f32(S[i] - S[i - 1]);
    const dsp = f32(S[i + 1] - S[i]);
    B[i] = dsp;
    A[i] = f32(2.0 * f32(dsm + dsp));
    C[i] = dsm;
    const term1 = f32(f32(X[i + 1] - X[i]) * f32(dsm / dsp));
    const term2 = f32(f32(X[i] - X[i - 1]) * f32(dsp / dsm));
    XS[i] = f32(3.0 * f32(term1 + term2));
  }

  A[0] = 2.0;
  C[0] = 1.0;
  XS[0] = f32(3.0 * f32(f32(X[1] - X[0]) / f32(S[1] - S[0])));
  B[N - 1] = 1.0;
  A[N - 1] = 2.0;
  XS[N - 1] = f32(3.0 * f32(f32(X[N - 1] - X[N - 2]) / f32(S[N - 1] - S[N - 2])));

  return TRISOL(A, B, C, XS, N);
}

export function SPLIND(X, XS, S, N, XS1, XS2) {
  if (N > NMAX) {
    throw new Error('SPLIND: array overflow, increase NMAX');
  }

  for (let i = 1; i <= N - 2; i += 1) {
    const dsm = f32(S[i] - S[i - 1]);
    const dsp = f32(S[i + 1] - S[i]);
    B[i] = dsp;
    A[i] = f32(2.0 * f32(dsm + dsp));
    C[i] = dsm;
    const term1 = f32(f32(X[i + 1] - X[i]) * f32(dsm / dsp));
    const term2 = f32(f32(X[i] - X[i - 1]) * f32(dsp / dsm));
    XS[i] = f32(3.0 * f32(term1 + term2));
  }

  if (XS1 === 999.0) {
    A[0] = 2.0;
    C[0] = 1.0;
    XS[0] = f32(3.0 * f32(f32(X[1] - X[0]) / f32(S[1] - S[0])));
  } else if (XS1 === -999.0) {
    A[0] = 1.0;
    C[0] = 1.0;
    XS[0] = f32(2.0 * f32(f32(X[1] - X[0]) / f32(S[1] - S[0])));
  } else {
    A[0] = 1.0;
    C[0] = 0.0;
    XS[0] = f32(XS1);
  }

  if (XS2 === 999.0) {
    B[N - 1] = 1.0;
    A[N - 1] = 2.0;
    XS[N - 1] = f32(3.0 * f32(f32(X[N - 1] - X[N - 2]) / f32(S[N - 1] - S[N - 2])));
  } else if (XS2 === -999.0) {
    B[N - 1] = 1.0;
    A[N - 1] = 1.0;
    XS[N - 1] = f32(2.0 * f32(f32(X[N - 1] - X[N - 2]) / f32(S[N - 1] - S[N - 2])));
  } else {
    A[N - 1] = 1.0;
    B[N - 1] = 0.0;
    XS[N - 1] = f32(XS2);
  }

  if (N === 2 && XS1 === -999.0 && XS2 === -999.0) {
    B[N - 1] = 1.0;
    A[N - 1] = 2.0;
    XS[N - 1] = f32(3.0 * f32(f32(X[N - 1] - X[N - 2]) / f32(S[N - 1] - S[N - 2])));
  }

  return TRISOL(A, B, C, XS, N);
}

export function SPLINA(X, XS, S, N) {
  let lend = true;
  let xs1 = 0.0;
  let xs2 = 0.0;

  for (let i = 0; i < N - 1; i += 1) {
    const ds = f32(S[i + 1] - S[i]);
    if (ds === 0.0) {
      XS[i] = f32(xs1);
      lend = true;
    } else {
      const dx = f32(X[i + 1] - X[i]);
      xs2 = f32(dx / ds);
      if (lend) {
        XS[i] = f32(xs2);
        lend = false;
      } else {
        XS[i] = f32(0.5 * f32(xs1 + xs2));
      }
    }
    xs1 = f32(xs2);
  }

  XS[N - 1] = f32(xs1);
  return XS;
}

export function SEVAL(SS, X, XS, S, N) {
  const i = findIntervalIndex(SS, S, N);
  const ds = f32(S[i - 1] - S[i - 2]);
  const t = f32(f32(SS - S[i - 2]) / ds);
  const cx1 = f32(f32(ds * XS[i - 2]) - X[i - 1] + X[i - 2]);
  const cx2 = f32(f32(ds * XS[i - 1]) - X[i - 1] + X[i - 2]);
  const term = f32(f32(t - f32(t * t)) * f32(f32(1.0 - t) * cx1 - f32(t * cx2)));
  const val = f32(f32(t * X[i - 1]) + f32((1.0 - t) * X[i - 2]) + term);
  return val;
}

export function DEVAL(SS, X, XS, S, N) {
  const i = findIntervalIndex(SS, S, N);
  const ds = f32(S[i - 1] - S[i - 2]);
  const t = f32(f32(SS - S[i - 2]) / ds);
  const cx1 = f32(f32(ds * XS[i - 2]) - X[i - 1] + X[i - 2]);
  const cx2 = f32(f32(ds * XS[i - 1]) - X[i - 1] + X[i - 2]);
  let val = f32(f32(X[i - 1] - X[i - 2]) + f32(f32(1.0 - 4.0 * t + 3.0 * f32(t * t)) * cx1)
    + f32(t * f32(3.0 * t - 2.0) * cx2));
  val = f32(val / ds);
  return val;
}

export function D2VAL(SS, X, XS, S, N) {
  const i = findIntervalIndex(SS, S, N);
  const ds = f32(S[i - 1] - S[i - 2]);
  const t = f32(f32(SS - S[i - 2]) / ds);
  const cx1 = f32(f32(ds * XS[i - 2]) - X[i - 1] + X[i - 2]);
  const cx2 = f32(f32(ds * XS[i - 1]) - X[i - 1] + X[i - 2]);
  let val = f32(f32(f32(6.0 * t - 4.0) * cx1) + f32(f32(6.0 * t - 2.0) * cx2));
  val = f32(val / f32(ds * ds));
  return val;
}

export function SEVALL(SS, X, XS, S, N) {
  const i = findIntervalIndex(SS, S, N);
  const ds = f32(S[i - 1] - S[i - 2]);
  const t = f32(f32(SS - S[i - 2]) / ds);

  const f0 = f32(X[i - 2]);
  const f1 = f32(ds * XS[i - 2]);
  const f2 = f32(-ds * f32(2.0 * XS[i - 2] + XS[i - 1]) + f32(3.0 * f32(X[i - 1] - X[i - 2])));
  const f3 = f32(ds * f32(XS[i - 2] + XS[i - 1]) - f32(2.0 * f32(X[i - 1] - X[i - 2])));

  let xx = f32(f0 + f32(t * f32(f1 + f32(t * f32(f2 + f32(t * f3))))));
  let xxs = f32(f1 + f32(t * f32(2.0 * f2 + f32(t * 3.0 * f3))));
  let xxss = f32(f32(2.0 * f2) + f32(t * 6.0 * f3));

  xxs = f32(xxs / ds);
  xxss = f32(xxss / f32(ds * ds));

  return { XX: xx, XXS: xxs, XXSS: xxss };
}

export function CURV(SS, X, XS, Y, YS, S, N) {
  const i = findIntervalIndex(SS, S, N);
  const ds = f32(S[i - 1] - S[i - 2]);
  const t = f32(f32(SS - S[i - 2]) / ds);

  const cx1 = f32(f32(ds * XS[i - 2]) - X[i - 1] + X[i - 2]);
  const cx2 = f32(f32(ds * XS[i - 1]) - X[i - 1] + X[i - 2]);
  const xd = f32(f32(X[i - 1] - X[i - 2]) + f32(f32(1.0 - 4.0 * t + 3.0 * f32(t * t)) * cx1)
    + f32(t * f32(3.0 * t - 2.0) * cx2));
  const xdd = f32(f32(f32(6.0 * t - 4.0) * cx1) + f32(f32(6.0 * t - 2.0) * cx2));

  const cy1 = f32(f32(ds * YS[i - 2]) - Y[i - 1] + Y[i - 2]);
  const cy2 = f32(f32(ds * YS[i - 1]) - Y[i - 1] + Y[i - 2]);
  const yd = f32(f32(Y[i - 1] - Y[i - 2]) + f32(f32(1.0 - 4.0 * t + 3.0 * f32(t * t)) * cy1)
    + f32(t * f32(3.0 * t - 2.0) * cy2));
  const ydd = f32(f32(f32(6.0 * t - 4.0) * cy1) + f32(f32(6.0 * t - 2.0) * cy2));

  const denom = Math.sqrt(f32(f32(xd * xd) + f32(yd * yd)) ** 3);
  return f32(f32(f32(xd * ydd) - f32(yd * xdd)) / f32(denom));
}

export function SINVRT(SI, XI, X, XS, S, N) {
  for (let iter = 0; iter < 10; iter += 1) {
    const res = f32(SEVAL(SI, X, XS, S, N) - XI);
    const resp = f32(DEVAL(SI, X, XS, S, N));
    const ds = f32(-res / resp);
    SI = f32(SI + ds);
    if (Math.abs(ds / (S[N - 1] - S[0])) < 1.0e-5) {
      return SI;
    }
  }
  return SI;
}

export function SCALC(X, Y, S, N) {
  S[0] = 0.0;
  for (let i = 1; i < N; i += 1) {
    const dx = f32(X[i] - X[i - 1]);
    const dy = f32(Y[i] - Y[i - 1]);
    S[i] = f32(S[i - 1] + f32(Math.sqrt(f32(dx * dx + dy * dy))));
  }
  return S;
}

export function SEGSPL(X, XS, S, N) {
  if (S[0] === S[1]) {
    throw new Error('SEGSPL:  First input point duplicated');
  }
  if (S[N - 1] === S[N - 2]) {
    throw new Error('SEGSPL:  Last  input point duplicated');
  }

  let iseg0 = 0;
  for (let iseg = 1; iseg <= N - 3; iseg += 1) {
    if (S[iseg] === S[iseg + 1]) {
      const nseg = iseg - iseg0 + 1;
      const xSeg = X.subarray(iseg0, iseg0 + nseg);
      const xsSeg = XS.subarray(iseg0, iseg0 + nseg);
      const sSeg = S.subarray(iseg0, iseg0 + nseg);
      SPLIND(xSeg, xsSeg, sSeg, nseg, -999.0, -999.0);
      iseg0 = iseg + 1;
    }
  }

  const nseg = N - iseg0;
  const xSeg = X.subarray(iseg0, iseg0 + nseg);
  const xsSeg = XS.subarray(iseg0, iseg0 + nseg);
  const sSeg = S.subarray(iseg0, iseg0 + nseg);
  SPLIND(xSeg, xsSeg, sSeg, nseg, -999.0, -999.0);

  return XS;
}
