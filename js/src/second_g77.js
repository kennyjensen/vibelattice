/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL second_g77.f: SECONDS returns current time in seconds.

import { SECONDS as SECONDS_BASE } from './second.js';

export function SECONDS() {
  return SECONDS_BASE();
}
