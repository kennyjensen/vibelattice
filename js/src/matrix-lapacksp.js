/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL matrix-lapacksp.f using the same LU algorithm as matrix.f.

import { LUDCMP as LUDCMP_base, BAKSUB as BAKSUB_base } from './matrix.js';

export function LUDCMP(NSIZ, N, A, INDX, WORK) {
  return LUDCMP_base(NSIZ, N, A, INDX, WORK);
}

export function BAKSUB(NSIZ, N, A, INDX, B) {
  return BAKSUB_base(NSIZ, N, A, INDX, B);
}
