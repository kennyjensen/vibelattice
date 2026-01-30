// Port of AVL matrix-lapackdp.f using double-precision LU/solve.

function idx(n, i, j) {
  return i * n + j;
}

export function LUDCMP(NSIZ, N, A, INDX, WORK) {
  for (let i = 0; i < N; i += 1) {
    let aamax = 0.0;
    for (let j = 0; j < N; j += 1) {
      const val = Math.abs(A[idx(N, i, j)]);
      if (val > aamax) aamax = val;
    }
    WORK[i] = 1.0 / aamax;
  }

  for (let j = 0; j < N; j += 1) {
    for (let i = 0; i < j; i += 1) {
      let sum = A[idx(N, i, j)];
      for (let k = 0; k < i; k += 1) {
        sum -= A[idx(N, i, k)] * A[idx(N, k, j)];
      }
      A[idx(N, i, j)] = sum;
    }

    let aamax = 0.0;
    let imax = j;
    for (let i = j; i < N; i += 1) {
      let sum = A[idx(N, i, j)];
      for (let k = 0; k < j; k += 1) {
        sum -= A[idx(N, i, k)] * A[idx(N, k, j)];
      }
      A[idx(N, i, j)] = sum;
      const dum = WORK[i] * Math.abs(sum);
      if (dum >= aamax) {
        aamax = dum;
        imax = i;
      }
    }

    if (j !== imax) {
      for (let k = 0; k < N; k += 1) {
        const dum = A[idx(N, imax, k)];
        A[idx(N, imax, k)] = A[idx(N, j, k)];
        A[idx(N, j, k)] = dum;
      }
      WORK[imax] = WORK[j];
    }

    INDX[j] = imax;

    if (j !== N - 1) {
      const dum = 1.0 / A[idx(N, j, j)];
      for (let i = j + 1; i < N; i += 1) {
        A[idx(N, i, j)] *= dum;
      }
    }
  }
}

export function BAKSUB(NSIZ, N, A, INDX, B) {
  let ii = -1;

  for (let i = 0; i < N; i += 1) {
    const ll = INDX[i];
    let sum = B[ll];
    B[ll] = B[i];
    if (ii !== -1) {
      for (let j = ii; j <= i - 1; j += 1) {
        sum -= A[idx(N, i, j)] * B[j];
      }
    } else if (sum !== 0.0) {
      ii = i;
    }
    B[i] = sum;
  }

  for (let i = N - 1; i >= 0; i -= 1) {
    let sum = B[i];
    if (i < N - 1) {
      for (let j = i + 1; j < N; j += 1) {
        sum -= A[idx(N, i, j)] * B[j];
      }
    }
    B[i] = sum / A[idx(N, i, i)];
  }
}

export function LUSOLVE(A, B, N) {
  const a = Float64Array.from(A);
  const b = Float64Array.from(B);
  const indx = new Int32Array(N);
  const work = new Float64Array(N);

  LUDCMP(N, N, a, indx, work);
  BAKSUB(N, N, a, indx, b);
  return b;
}
