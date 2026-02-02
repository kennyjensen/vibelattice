/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL ba_trans.f with float32 math for numerical fidelity.

const f32 = Math.fround;

function idx3(r, c) {
  return r * 3 + c;
}

export function BA2WA_MAT(alfa, beta, binv, P = new Float32Array(9), P_A = new Float32Array(9), P_B = new Float32Array(9)) {
  const sina = f32(Math.sin(f32(alfa)));
  const cosa = f32(Math.cos(f32(alfa)));
  const sinb = f32(Math.sin(f32(beta)));
  const cosb = f32(Math.cos(f32(beta)));
  const b = f32(binv);

  P[idx3(0, 0)] = f32(cosa * cosb * b);
  P[idx3(0, 1)] = f32(-sinb * b);
  P[idx3(0, 2)] = f32(sina * cosb * b);

  P[idx3(1, 0)] = f32(cosa * sinb);
  P[idx3(1, 1)] = cosb;
  P[idx3(1, 2)] = f32(sina * sinb);

  P[idx3(2, 0)] = f32(-sina);
  P[idx3(2, 1)] = 0.0;
  P[idx3(2, 2)] = cosa;

  P_A[idx3(0, 0)] = f32(-sina * cosb);
  P_A[idx3(0, 1)] = 0.0;
  P_A[idx3(0, 2)] = f32(cosa * cosb);

  P_A[idx3(1, 0)] = f32(-sina * sinb);
  P_A[idx3(1, 1)] = 0.0;
  P_A[idx3(1, 2)] = f32(cosa * sinb);

  P_A[idx3(2, 0)] = f32(-cosa);
  P_A[idx3(2, 1)] = 0.0;
  P_A[idx3(2, 2)] = f32(-sina);

  P_B[idx3(0, 0)] = f32(-cosa * sinb);
  P_B[idx3(0, 1)] = f32(-cosb);
  P_B[idx3(0, 2)] = f32(-sina * sinb);

  P_B[idx3(1, 0)] = f32(cosa * cosb);
  P_B[idx3(1, 1)] = f32(-sinb);
  P_B[idx3(1, 2)] = f32(sina * cosb);

  P_B[idx3(2, 0)] = 0.0;
  P_B[idx3(2, 1)] = 0.0;
  P_B[idx3(2, 2)] = 0.0;

  return { P, P_A, P_B };
}

export function BA2SA_MAT(alfa, P = new Float32Array(9), P_A = new Float32Array(9)) {
  const sina = f32(Math.sin(f32(alfa)));
  const cosa = f32(Math.cos(f32(alfa)));

  P[idx3(0, 0)] = cosa;
  P[idx3(0, 1)] = 0.0;
  P[idx3(0, 2)] = sina;

  P[idx3(1, 0)] = 0.0;
  P[idx3(1, 1)] = 1.0;
  P[idx3(1, 2)] = 0.0;

  P[idx3(2, 0)] = f32(-sina);
  P[idx3(2, 1)] = 0.0;
  P[idx3(2, 2)] = cosa;

  P_A[idx3(0, 0)] = f32(-sina);
  P_A[idx3(0, 1)] = 0.0;
  P_A[idx3(0, 2)] = cosa;

  P_A[idx3(1, 0)] = 0.0;
  P_A[idx3(1, 1)] = 0.0;
  P_A[idx3(1, 2)] = 0.0;

  P_A[idx3(2, 0)] = f32(-cosa);
  P_A[idx3(2, 1)] = 0.0;
  P_A[idx3(2, 2)] = f32(-sina);

  return { P, P_A };
}
