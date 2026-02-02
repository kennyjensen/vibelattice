/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL sgutil.f with float32 math for numerical fidelity.

const f32 = Math.fround;

export function AKIMA(X, Y, N, XX) {
  if (X[0] === X[N - 1]) {
    return { YY: f32(Y[0]), SLP: 0.0 };
  }

  let xordr = 1.0;
  if (X[0] > X[N - 1]) {
    xordr = -1.0;
  }

  let ibot = 1;
  let itop = N;
  const xxo = f32(XX * xordr);

  while (true) {
    const nstep = Math.floor((itop - ibot) / 2);
    const i = ibot + nstep;
    const xo = f32(X[i - 1] * xordr);
    if (xxo >= xo) {
      ibot = i;
    }
    if (xxo < xo) {
      itop = i;
    }
    if (nstep === 0) {
      break;
    }
  }

  const i = ibot;
  const d = new Float32Array(5);
  for (let j = 1; j <= 5; j += 1) {
    const k = i + (j - 2);
    if ((k - 1) >= 1 && k <= N) {
      d[j - 1] = f32(f32(Y[k - 1] - Y[k - 2]) / f32(X[k - 1] - X[k - 2]));
    }
  }

  if (N === 2) {
    d[1] = d[2];
  }
  if (i + 2 > N) {
    d[3] = f32(2.0 * d[2] - d[1]);
  }
  if (i + 3 > N) {
    d[4] = f32(2.0 * d[3] - d[2]);
  }
  if (i - 1 < 1) {
    d[1] = f32(2.0 * d[2] - d[3]);
  }
  if (i - 2 < 1) {
    d[0] = f32(2.0 * d[1] - d[2]);
  }

  const t = new Float32Array(2);
  for (let j = 1; j <= 2; j += 1) {
    let a = f32(Math.abs(d[j + 2] - d[j + 1]));
    let b = f32(Math.abs(d[j] - d[j - 1]));
    if (f32(a + b) === 0.0) {
      a = 1.0;
      b = 1.0;
    }
    t[j - 1] = f32(f32(a * d[j]) + f32(b * d[j + 1])) / f32(a + b);
  }

  if (XX === X[i]) {
    return { YY: f32(Y[i]), SLP: f32(t[1]) };
  }

  const xint = f32(X[i] - X[i - 1]);
  const xdif = f32(XX - X[i - 1]);
  const p0 = f32(Y[i - 1]);
  const p1 = f32(t[0]);
  const p2 = f32(f32(3.0 * d[2]) - f32(2.0 * t[0]) - t[1]) / xint;
  const p3 = f32(f32(t[0] + t[1]) - f32(2.0 * d[2])) / f32(xint * xint);

  const yy = f32(p0 + f32(xdif * f32(p1 + f32(xdif * f32(p2 + f32(xdif * p3))))));
  const slp = f32(p1 + f32(xdif * f32(2.0 * p2 + f32(xdif * f32(3.0 * p3)))));

  return { YY: yy, SLP: slp };
}

export function TRP1(N, X, Y, XTRP) {
  if (N < 1) {
    return 0.0;
  }
  if (N < 2) {
    return f32(Y[0]);
  }

  let i = 0;
  while (true) {
    if (X[i + 1] > XTRP || i + 1 === N - 1) {
      break;
    }
    i += 1;
  }

  return f32(Y[i] + f32(f32(Y[i + 1] - Y[i]) * f32(f32(XTRP - X[i]) / f32(X[i + 1] - X[i]))));
}

export function NRMLIZ(N, X) {
  if (N <= 1) {
    return X;
  }

  let dx = f32(X[N - 1] - X[0]);
  if (dx === 0.0) {
    dx = 1.0;
  }

  const x1 = f32(X[0]);
  for (let i = 0; i < N; i += 1) {
    X[i] = f32(f32(X[i] - x1) / dx);
  }

  return X;
}

export function SPACER(N, PSPACE, X) {
  const pi = f32(3.1415926535);
  const pabs = f32(Math.abs(PSPACE));
  const nabs = Math.trunc(pabs) + 1;
  let pequ = 0.0;
  let pcos = 0.0;
  let psin = 0.0;

  if (nabs === 1) {
    pequ = f32(1.0 - pabs);
    pcos = f32(pabs);
    psin = 0.0;
  } else if (nabs === 2) {
    pequ = 0.0;
    pcos = f32(2.0 - pabs);
    psin = f32(pabs - 1.0);
  } else {
    pequ = f32(pabs - 2.0);
    pcos = 0.0;
    psin = f32(3.0 - pabs);
  }

  for (let k = 1; k <= N; k += 1) {
    const frac = f32((k - 1) / (N - 1));
    const theta = f32(frac * pi);
    const cosTheta = f32(Math.cos(f32(theta)));
    const cosHalf = f32(Math.cos(f32(theta * 0.5)));
    const sinHalf = f32(Math.sin(f32(theta * 0.5)));
    if (PSPACE >= 0.0) {
      X[k] = f32(f32(pequ * frac)
        + f32(pcos * f32(f32(1.0 - cosTheta) * 0.5))
        + f32(psin * f32(1.0 - cosHalf)));
    } else {
      X[k] = f32(f32(pequ * frac)
        + f32(pcos * f32(f32(1.0 - cosTheta) * 0.5))
        + f32(psin * sinHalf));
    }
  }

  return X;
}

export function CSPACER(NVC, CSPACE, CLAF, XPT, XVR, XSR, XCP) {
  const pi = f32(4.0 * Math.atan(1.0));
  const acsp = f32(Math.abs(CSPACE));
  const ncsp = Math.trunc(acsp);
  let f0 = 0.0;
  let f1 = 0.0;
  let f2 = 0.0;

  if (ncsp === 0) {
    f0 = f32(1.0 - acsp);
    f1 = f32(acsp);
    f2 = 0.0;
  } else if (ncsp === 1) {
    f0 = 0.0;
    f1 = f32(2.0 - acsp);
    f2 = f32(acsp - 1.0);
  } else {
    f0 = f32(acsp - 2.0);
    f1 = 0.0;
    f2 = f32(3.0 - acsp);
  }

  const dth1 = f32(pi / f32(4 * NVC + 2));
  const dth2 = f32(0.5 * pi / f32(4 * NVC + 1));
  const dxc0 = f32(1.0 / f32(4 * NVC));

  for (let ivc = 1; ivc <= NVC; ivc += 1) {
    const idx = ivc;
    const xc0 = f32(Math.trunc(4 * ivc - 4) * dxc0);
    const xpt0 = xc0;
    const xvr0 = f32(xc0 + dxc0);
    const xsr0 = f32(xc0 + f32(2.0 * dxc0));
    const xcp0 = f32(xc0 + f32(dxc0 + f32(2.0 * dxc0 * CLAF)));

    const th1 = f32(Math.trunc(4 * ivc - 3) * dth1);
    const xpt1 = f32(0.5 * f32(1.0 - Math.cos(f32(th1))));
    const xvr1 = f32(0.5 * f32(1.0 - Math.cos(f32(th1 + dth1))));
    const xsr1 = f32(0.5 * f32(1.0 - Math.cos(f32(th1 + f32(2.0 * dth1)))));
    const xcp1 = f32(0.5 * f32(1.0 - Math.cos(f32(th1 + dth1 + f32(2.0 * dth1 * CLAF)))));

    let xpt2 = 0.0;
    let xvr2 = 0.0;
    let xsr2 = 0.0;
    let xcp2 = 0.0;
    if (CSPACE > 0.0) {
      const th2 = f32(Math.trunc(4 * ivc - 3) * dth2);
      xpt2 = f32(1.0 - Math.cos(f32(th2)));
      xvr2 = f32(1.0 - Math.cos(f32(th2 + dth2)));
      xsr2 = f32(1.0 - Math.cos(f32(th2 + f32(2.0 * dth2))));
      xcp2 = f32(1.0 - Math.cos(f32(th2 + dth2 + f32(2.0 * dth2 * CLAF))));
    } else {
      const th2 = f32(Math.trunc(4 * ivc - 4) * dth2);
      xpt2 = f32(Math.sin(f32(th2)));
      xvr2 = f32(Math.sin(f32(th2 + dth2)));
      xsr2 = f32(Math.sin(f32(th2 + f32(2.0 * dth2))));
      xcp2 = f32(Math.sin(f32(th2 + dth2 + f32(2.0 * dth2 * CLAF))));
    }

    XPT[idx] = f32(f32(f0 * xpt0) + f32(f1 * xpt1) + f32(f2 * xpt2));
    XVR[idx] = f32(f32(f0 * xvr0) + f32(f1 * xvr1) + f32(f2 * xvr2));
    XSR[idx] = f32(f32(f0 * xsr0) + f32(f1 * xsr1) + f32(f2 * xsr2));
    XCP[idx] = f32(f32(f0 * xcp0) + f32(f1 * xcp1) + f32(f2 * xcp2));
  }

  XPT[1] = 0.0;
  XPT[NVC + 1] = 1.0;
  return { XPT, XVR, XSR, XCP };
}
