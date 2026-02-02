/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL matrix-linpack.f with float32 math for numerical fidelity.

const f32 = Math.fround;

function idx(lda, i, j) {
  return j * lda + i;
}

export function ISAMAX(n, sx, incx, start = 0) {
  if (n < 1 || incx <= 0) return 0;
  if (n === 1) return 1;
  let imax = 1;
  let smax = Math.abs(sx[start]);
  if (incx === 1) {
    for (let i = 1; i < n; i += 1) {
      const v = Math.abs(sx[start + i]);
      if (v > smax) {
        smax = v;
        imax = i + 1;
      }
    }
    return imax;
  }
  let ix = start;
  for (let i = 0; i < n; i += 1) {
    const v = Math.abs(sx[ix]);
    if (v > smax) {
      smax = v;
      imax = i + 1;
    }
    ix += incx;
  }
  return imax;
}

export function SSCAL(n, sa, sx, incx, start = 0) {
  if (n <= 0 || incx <= 0) return;
  if (incx === 1) {
    for (let i = 0; i < n; i += 1) {
      sx[start + i] = f32(sa * sx[start + i]);
    }
    return;
  }
  let ix = start;
  for (let i = 0; i < n; i += 1) {
    sx[ix] = f32(sa * sx[ix]);
    ix += incx;
  }
}

export function SAXPY(n, sa, sx, incx, sy, incy, sxStart = 0, syStart = 0) {
  if (n <= 0) return;
  if (sa === 0.0) return;
  if (incx === 1 && incy === 1) {
    for (let i = 0; i < n; i += 1) {
      sy[syStart + i] = f32(sy[syStart + i] + f32(sa * sx[sxStart + i]));
    }
    return;
  }
  let ix = sxStart;
  let iy = syStart;
  for (let i = 0; i < n; i += 1) {
    sy[iy] = f32(sy[iy] + f32(sa * sx[ix]));
    ix += incx;
    iy += incy;
  }
}

export function SDOT(n, sx, incx, sy, incy, sxStart = 0, syStart = 0) {
  let stemp = f32(0.0);
  if (n <= 0) return stemp;
  if (incx === 1 && incy === 1) {
    for (let i = 0; i < n; i += 1) {
      stemp = f32(stemp + f32(sx[sxStart + i] * sy[syStart + i]));
    }
    return stemp;
  }
  let ix = sxStart;
  let iy = syStart;
  for (let i = 0; i < n; i += 1) {
    stemp = f32(stemp + f32(sx[ix] * sy[iy]));
    ix += incx;
    iy += incy;
  }
  return stemp;
}

export function SGEFA(A, lda, n, ipvt, info = { value: 0 }) {
  info.value = 0;
  const nm1 = n - 1;
  if (nm1 < 1) {
    ipvt[n - 1] = n;
    if (A[idx(lda, n - 1, n - 1)] === 0.0) info.value = n;
    return;
  }

  for (let k = 0; k < nm1; k += 1) {
    const kp1 = k + 1;
    const l = ISAMAX(n - k, A, 1, idx(lda, k, k)) + k; // 1-based
    ipvt[k] = l;
    const l0 = l - 1;
    if (A[idx(lda, l0, k)] === 0.0) {
      info.value = k + 1;
    } else {
      if (l0 !== k) {
        const t = A[idx(lda, l0, k)];
        A[idx(lda, l0, k)] = A[idx(lda, k, k)];
        A[idx(lda, k, k)] = t;
      }
      const t = f32(-1.0 / A[idx(lda, k, k)]);
      SSCAL(n - kp1, t, A, 1, idx(lda, kp1, k));

      for (let j = kp1; j < n; j += 1) {
        let tj = A[idx(lda, l0, j)];
        if (l0 !== k) {
          A[idx(lda, l0, j)] = A[idx(lda, k, j)];
          A[idx(lda, k, j)] = tj;
        }
        SAXPY(n - kp1, tj, A, 1, A, 1, idx(lda, kp1, k), idx(lda, kp1, j));
      }
    }
  }

  ipvt[n - 1] = n;
  if (A[idx(lda, n - 1, n - 1)] === 0.0) info.value = n;
}

export function SGESL(A, lda, n, ipvt, b, job) {
  const nm1 = n - 1;
  if (job === 0) {
    if (nm1 >= 1) {
      for (let k = 0; k < nm1; k += 1) {
        const l = ipvt[k] - 1;
        const t = b[l];
        if (l !== k) {
          b[l] = b[k];
          b[k] = t;
        }
        SAXPY(n - k - 1, t, A, 1, b, 1, idx(lda, k + 1, k), k + 1);
      }
    }

    for (let kb = 1; kb <= n; kb += 1) {
      const k = n - kb;
      b[k] = f32(b[k] / A[idx(lda, k, k)]);
      const t = f32(-b[k]);
      SAXPY(k, t, A, 1, b, 1, idx(lda, 0, k), 0);
    }
    return;
  }

  for (let k = 0; k < n; k += 1) {
    const t = SDOT(k, A, 1, b, 1, idx(lda, 0, k), 0);
    b[k] = f32((b[k] - t) / A[idx(lda, k, k)]);
  }

  if (nm1 >= 1) {
    for (let kb = 1; kb <= nm1; kb += 1) {
      const k = n - kb - 1;
      b[k] = f32(b[k] + SDOT(n - k - 1, A, 1, b, 1, idx(lda, k + 1, k), k + 1));
      const l = ipvt[k] - 1;
      if (l !== k) {
        const t = b[l];
        b[l] = b[k];
        b[k] = t;
      }
    }
  }
}

export function LUDCMP(nsiz, n, A, INDX, WORK) {
  const info = { value: 0 };
  SGEFA(A, nsiz, n, INDX, info);
}

export function BAKSUB(nsiz, n, A, INDX, B) {
  SGESL(A, nsiz, n, INDX, B, 0);
}
