// Port of AVL autil.f with float32 math for numerical fidelity.

const f32 = Math.fround;

function idx3(r, c) {
  return r * 3 + c;
}

function idxT(r, c) {
  return r * 6 + c;
}

function idxA3(r, c, k) {
  return k * 9 + r * 3 + c;
}

export function M3INV(A, AINV = new Float32Array(9)) {
  const T = new Float32Array(18);

  for (let k = 0; k < 3; k += 1) {
    T[idxT(k, 0)] = f32(A[idx3(k, 0)]);
    T[idxT(k, 1)] = f32(A[idx3(k, 1)]);
    T[idxT(k, 2)] = f32(A[idx3(k, 2)]);
    T[idxT(k, 3)] = 0.0;
    T[idxT(k, 4)] = 0.0;
    T[idxT(k, 5)] = 0.0;
  }

  T[idxT(0, 3)] = 1.0;
  T[idxT(1, 4)] = 1.0;
  T[idxT(2, 5)] = 1.0;

  for (let n = 0; n < 3; n += 1) {
    const pivot = f32(T[idxT(n, n)]);
    if (pivot === 0.0) {
      for (let l = n + 1; l < 6; l += 1) {
        T[idxT(n, l)] = 0.0;
      }
    } else {
      for (let l = n + 1; l < 6; l += 1) {
        T[idxT(n, l)] = f32(f32(T[idxT(n, l)]) / pivot);
      }
    }

    for (let k = n + 1; k < 3; k += 1) {
      const tel = f32(T[idxT(k, n)]);
      for (let l = n + 1; l < 6; l += 1) {
        T[idxT(k, l)] = f32(f32(T[idxT(k, l)]) - f32(tel * f32(T[idxT(n, l)])));
      }
    }
  }

  for (let n = 2; n >= 1; n -= 1) {
    for (let k = n - 1; k >= 0; k -= 1) {
      const tel = f32(T[idxT(k, n)]);
      for (let l = 3; l < 6; l += 1) {
        T[idxT(k, l)] = f32(f32(T[idxT(k, l)]) - f32(tel * f32(T[idxT(n, l)])));
      }
    }
  }

  for (let k = 0; k < 3; k += 1) {
    AINV[idx3(k, 0)] = f32(T[idxT(k, 3)]);
    AINV[idx3(k, 1)] = f32(T[idxT(k, 4)]);
    AINV[idx3(k, 2)] = f32(T[idxT(k, 5)]);
  }

  return AINV;
}

export function RATEKI3(A, R = new Float32Array(9), R_A = new Float32Array(27)) {
  const c1 = f32(Math.cos(f32(A[0])));
  const c2 = f32(Math.cos(f32(A[1])));
  const s1 = f32(Math.sin(f32(A[0])));
  const t2 = f32(Math.tan(f32(A[1])));

  R[idx3(0, 0)] = -1.0;
  R[idx3(1, 0)] = 0.0;
  R[idx3(2, 0)] = 0.0;

  R[idx3(0, 1)] = f32(s1 * t2);
  R[idx3(1, 1)] = c1;
  R[idx3(2, 1)] = f32(s1 / c2);

  R[idx3(0, 2)] = f32(-c1 * t2);
  R[idx3(1, 2)] = s1;
  R[idx3(2, 2)] = f32(-c1 / c2);

  R_A[idxA3(0, 0, 0)] = 0.0;
  R_A[idxA3(1, 0, 0)] = 0.0;
  R_A[idxA3(2, 0, 0)] = 0.0;

  R_A[idxA3(0, 1, 0)] = f32(c1 * t2);
  R_A[idxA3(1, 1, 0)] = f32(-s1);
  R_A[idxA3(2, 1, 0)] = f32(c1 / c2);

  R_A[idxA3(0, 2, 0)] = f32(s1 * t2);
  R_A[idxA3(1, 2, 0)] = c1;
  R_A[idxA3(2, 2, 0)] = f32(s1 / c2);

  R_A[idxA3(0, 0, 1)] = 0.0;
  R_A[idxA3(1, 0, 1)] = 0.0;
  R_A[idxA3(2, 0, 1)] = 0.0;

  R_A[idxA3(0, 1, 1)] = f32(s1 / f32(c2 * c2));
  R_A[idxA3(1, 1, 1)] = 0.0;
  R_A[idxA3(2, 1, 1)] = f32(s1 * t2 / c2);

  R_A[idxA3(0, 2, 1)] = f32(-c1 / f32(c2 * c2));
  R_A[idxA3(1, 2, 1)] = 0.0;
  R_A[idxA3(2, 2, 1)] = f32(-c1 * t2 / c2);

  R_A[idxA3(0, 0, 2)] = 0.0;
  R_A[idxA3(1, 0, 2)] = 0.0;
  R_A[idxA3(2, 0, 2)] = 0.0;

  R_A[idxA3(0, 1, 2)] = 0.0;
  R_A[idxA3(1, 1, 2)] = 0.0;
  R_A[idxA3(2, 1, 2)] = 0.0;

  R_A[idxA3(0, 2, 2)] = 0.0;
  R_A[idxA3(1, 2, 2)] = 0.0;
  R_A[idxA3(2, 2, 2)] = 0.0;

  return { R, R_A };
}

export function ROTENS3(A, T = new Float32Array(9), T_A = new Float32Array(27)) {
  const c1 = f32(Math.cos(f32(A[0])));
  const c2 = f32(Math.cos(f32(A[1])));
  const c3 = f32(Math.cos(f32(A[2])));

  const s1 = f32(Math.sin(f32(A[0])));
  const s2 = f32(Math.sin(f32(A[1])));
  const s3 = f32(Math.sin(f32(A[2])));

  T[idx3(0, 0)] = f32(c2 * c3);
  T[idx3(1, 0)] = f32(-c2 * s3);
  T[idx3(2, 0)] = f32(-s2);

  T[idx3(0, 1)] = f32(-s1 * s2 * c3 + c1 * s3);
  T[idx3(1, 1)] = f32(s1 * s2 * s3 + c1 * c3);
  T[idx3(2, 1)] = f32(-s1 * c2);

  T[idx3(0, 2)] = f32(c1 * s2 * c3 + s1 * s3);
  T[idx3(1, 2)] = f32(-c1 * s2 * s3 + s1 * c3);
  T[idx3(2, 2)] = f32(c1 * c2);

  T_A[idxA3(0, 0, 0)] = 0.0;
  T_A[idxA3(1, 0, 0)] = 0.0;
  T_A[idxA3(2, 0, 0)] = 0.0;

  T_A[idxA3(0, 1, 0)] = f32(-c1 * s2 * c3 - s1 * s3);
  T_A[idxA3(1, 1, 0)] = f32(c1 * s2 * s3 - s1 * c3);
  T_A[idxA3(2, 1, 0)] = f32(-c1 * c2);

  T_A[idxA3(0, 2, 0)] = f32(-s1 * s2 * c3 + c1 * s3);
  T_A[idxA3(1, 2, 0)] = f32(s1 * s2 * s3 + c1 * c3);
  T_A[idxA3(2, 2, 0)] = f32(-s1 * c2);

  T_A[idxA3(0, 0, 1)] = f32(-s2 * c3);
  T_A[idxA3(1, 0, 1)] = f32(s2 * s3);
  T_A[idxA3(2, 0, 1)] = f32(-c2);

  T_A[idxA3(0, 1, 1)] = f32(-s1 * c2 * c3);
  T_A[idxA3(1, 1, 1)] = f32(s1 * c2 * s3);
  T_A[idxA3(2, 1, 1)] = f32(s1 * s2);

  T_A[idxA3(0, 2, 1)] = f32(c1 * c2 * c3);
  T_A[idxA3(1, 2, 1)] = f32(-c1 * c2 * s3);
  T_A[idxA3(2, 2, 1)] = f32(-c1 * s2);

  T_A[idxA3(0, 0, 2)] = f32(-c2 * s3);
  T_A[idxA3(1, 0, 2)] = f32(-c2 * c3);
  T_A[idxA3(2, 0, 2)] = 0.0;

  T_A[idxA3(0, 1, 2)] = f32(s1 * s2 * s3 + c1 * c3);
  T_A[idxA3(1, 1, 2)] = f32(s1 * s2 * c3 - c1 * s3);
  T_A[idxA3(2, 1, 2)] = 0.0;

  T_A[idxA3(0, 2, 2)] = f32(-c1 * s2 * s3 + s1 * c3);
  T_A[idxA3(1, 2, 2)] = f32(-c1 * s2 * c3 - s1 * s3);
  T_A[idxA3(2, 2, 2)] = 0.0;

  return { T, T_A };
}
