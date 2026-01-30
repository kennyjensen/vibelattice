// Port of AVL airutil.f with float32 math for numerical fidelity.

const f32 = Math.fround;
import { SCALC, SEGSPL, SEVAL, DEVAL, D2VAL, SINVRT } from './spline.js';

export function LEFIND(X, XP, Y, YP, S, N) {
  let sle = S[0];
  for (let i = 1; i < N; i += 1) {
    if (X[i] > X[i - 1]) {
      sle = S[i - 1];
      break;
    }
  }

  const sref = f32(S[N - 1] - S[0]);
  for (let iter = 0; iter < 20; iter += 1) {
    const res = f32(DEVAL(sle, X, XP, S, N));
    const resp = f32(D2VAL(sle, X, XP, S, N));
    const dsle = f32(-res / resp);
    sle = f32(sle + dsle);
    if (Math.abs(dsle) / sref < 1.0e-5) {
      return sle;
    }
  }
  return sle;
}

export function NORMIT(SLE, X, XP, Y, YP, S, N) {
  const xle = SEVAL(SLE, X, XP, S, N);
  const xte = f32(0.5 * f32(X[0] + X[N - 1]));
  const dnorm = f32(1.0 / f32(xte - xle));

  for (let i = 0; i < N; i += 1) {
    X[i] = f32(f32(X[i] - xle) * dnorm);
    Y[i] = f32(Y[i] * dnorm);
    S[i] = f32(S[i] * dnorm);
  }

  return f32(SLE * dnorm);
}

export function GETCAM(X, Y, N, XC, YC, TC, NC, LNORM) {
  const s = new Float32Array(N);
  const xp = new Float32Array(N);
  const yp = new Float32Array(N);

  SCALC(X, Y, s, N);
  SEGSPL(X, xp, s, N);
  SEGSPL(Y, yp, s, N);

  let sle = LEFIND(X, xp, Y, yp, s, N);
  if (LNORM) {
    sle = NORMIT(sle, X, xp, Y, yp, s, N);
  }

  const xle = SEVAL(sle, X, xp, s, N);
  const yle = SEVAL(sle, Y, yp, s, N);
  const xte = f32(0.5 * f32(X[0] + X[N - 1]));

  let nc = NC;
  if (nc <= 0) {
    nc = 30;
  }

  let su = f32(sle - 0.01);
  let sl = f32(sle + 0.01);
  const fnc1 = f32(nc - 1);

  XC[0] = xle;
  YC[0] = yle;
  TC[0] = 0.0;

  const pi = f32(4.0 * Math.atan(1.0));
  for (let i = 1; i < nc; i += 1) {
    const xout = f32(xle + f32(xte - xle) * f32(0.5 * f32(1.0 - Math.cos(f32(pi * f32(i) / fnc1)))));
    su = SINVRT(su, xout, X, xp, s, N);
    const yu = SEVAL(su, Y, yp, s, N);
    sl = SINVRT(sl, xout, X, xp, s, N);
    const yl = SEVAL(sl, Y, yp, s, N);
    XC[i] = xout;
    YC[i] = f32(0.5 * f32(yu + yl));
    TC[i] = f32(yu - yl);
  }

  return { XC, YC, TC, NC: nc };
}
