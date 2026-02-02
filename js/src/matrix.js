/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL matrix.f with float32 math for numerical fidelity.

const f32 = Math.fround;

function idx(n, i, j) {
  return i * n + j;
}

export function LUDCMP(nsiz, n, A, INDX, WORK) {
  for (let i = 0; i < n; i += 1) {
    let aamax = f32(0.0);
    for (let j = 0; j < n; j += 1) {
      const val = Math.abs(A[idx(n, i, j)]);
      if (val > aamax) aamax = f32(val);
    }
    WORK[i] = f32(1.0 / aamax);
  }

  for (let j = 0; j < n; j += 1) {
    for (let i = 0; i < j; i += 1) {
      let sum = f32(A[idx(n, i, j)]);
      for (let k = 0; k < i; k += 1) {
        sum = f32(sum - f32(A[idx(n, i, k)] * A[idx(n, k, j)]));
      }
      A[idx(n, i, j)] = sum;
    }

    let aamax = f32(0.0);
    let imax = j;
    for (let i = j; i < n; i += 1) {
      let sum = f32(A[idx(n, i, j)]);
      for (let k = 0; k < j; k += 1) {
        sum = f32(sum - f32(A[idx(n, i, k)] * A[idx(n, k, j)]));
      }
      A[idx(n, i, j)] = sum;
      const dum = f32(WORK[i] * Math.abs(sum));
      if (dum >= aamax) {
        aamax = dum;
        imax = i;
      }
    }

    if (j !== imax) {
      for (let k = 0; k < n; k += 1) {
        const dum = A[idx(n, imax, k)];
        A[idx(n, imax, k)] = A[idx(n, j, k)];
        A[idx(n, j, k)] = dum;
      }
      WORK[imax] = WORK[j];
    }

    INDX[j] = imax;

    if (j !== n - 1) {
      const dum = f32(1.0 / A[idx(n, j, j)]);
      for (let i = j + 1; i < n; i += 1) {
        A[idx(n, i, j)] = f32(A[idx(n, i, j)] * dum);
      }
    }
  }
}

export function BAKSUB(nsiz, n, A, INDX, B) {
  let ii = -1;

  for (let i = 0; i < n; i += 1) {
    const ll = INDX[i];
    let sum = f32(B[ll]);
    B[ll] = B[i];
    if (ii !== -1) {
      for (let j = ii; j <= i - 1; j += 1) {
        sum = f32(sum - f32(A[idx(n, i, j)] * B[j]));
      }
    } else if (sum !== 0.0) {
      ii = i;
    }
    B[i] = sum;
  }

  for (let i = n - 1; i >= 0; i -= 1) {
    let sum = f32(B[i]);
    if (i < n - 1) {
      for (let j = i + 1; j < n; j += 1) {
        sum = f32(sum - f32(A[idx(n, i, j)] * B[j]));
      }
    }
    B[i] = f32(sum / A[idx(n, i, i)]);
  }
}

export function LUSOLVE(A, B, n) {
  const nsiz = n;
  const a = Float32Array.from(A);
  const b = Float32Array.from(B);
  const indx = new Int32Array(n);
  const work = new Float32Array(n);

  LUDCMP(nsiz, n, a, indx, work);
  BAKSUB(nsiz, n, a, indx, b);
  return b;
}
