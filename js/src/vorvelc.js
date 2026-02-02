/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
// Port of AVL vorvelc.f with float32 math for numerical fidelity.

const f32 = Math.fround;
const PI4INV = f32(0.079577472);

export function VORVELC(
  x, y, z,
  lbound,
  x1, y1, z1,
  x2, y2, z2,
  beta,
  rcore,
) {
  const bx = f32(beta);
  const ax1 = f32((f32(x1) - f32(x)) / bx);
  const ay1 = f32(f32(y1) - f32(y));
  const az1 = f32(f32(z1) - f32(z));

  const bx1 = f32((f32(x2) - f32(x)) / bx);
  const by1 = f32(f32(y2) - f32(y));
  const bz1 = f32(f32(z2) - f32(z));

  const asq = f32(f32(ax1 * ax1) + f32(ay1 * ay1) + f32(az1 * az1));
  const bsq = f32(f32(bx1 * bx1) + f32(by1 * by1) + f32(bz1 * bz1));

  const amag = f32(Math.sqrt(asq));
  const bmag = f32(Math.sqrt(bsq));

  const rcore2 = f32(f32(rcore) * f32(rcore));
  const rcore4 = f32(rcore2 * rcore2);

  let u = f32(0.0);
  let v = f32(0.0);
  let w = f32(0.0);

  if (lbound && f32(amag * bmag) !== 0.0) {
    const axb1 = f32(f32(ay1 * bz1) - f32(az1 * by1));
    const axb2 = f32(f32(az1 * bx1) - f32(ax1 * bz1));
    const axb3 = f32(f32(ax1 * by1) - f32(ay1 * bx1));
    const axbsq = f32(f32(axb1 * axb1) + f32(axb2 * axb2) + f32(axb3 * axb3));

    if (axbsq !== 0.0) {
      const adb = f32(f32(ax1 * bx1) + f32(ay1 * by1) + f32(az1 * bz1));
      const alsq = f32(f32(asq + bsq) - f32(2.0 * adb));

      const t1 = f32(bsq - adb);
      const t2 = f32(asq - adb);
      const s1 = f32(Math.sqrt(Math.sqrt(f32(bsq * bsq + rcore4))));
      const s2 = f32(Math.sqrt(Math.sqrt(f32(asq * asq + rcore4))));
      const num = f32(f32(t1 / s1) + f32(t2 / s2));
      const den = f32(Math.sqrt(f32(axbsq * axbsq) + f32(alsq * alsq * rcore4)));

      const t = f32(num / den);
      u = f32(axb1 * t);
      v = f32(axb2 * t);
      w = f32(axb3 * t);
    }
  }

  if (amag !== 0.0) {
    const axisq = f32(f32(az1 * az1) + f32(ay1 * ay1));
    const adx = ax1;
    const rsq = axisq;
    const t = f32(-f32(f32(1.0 - f32(adx / amag)) / Math.sqrt(f32(rsq * rsq + rcore4))));
    v = f32(v + f32(az1 * t));
    w = f32(w - f32(ay1 * t));
  }

  if (bmag !== 0.0) {
    const bxisq = f32(f32(bz1 * bz1) + f32(by1 * by1));
    const bdx = bx1;
    const rsq = bxisq;
    const t = f32(f32(f32(1.0 - f32(bdx / bmag)) / Math.sqrt(f32(rsq * rsq + rcore4))));
    v = f32(v + f32(bz1 * t));
    w = f32(w - f32(by1 * t));
  }

  u = f32(f32(u * PI4INV) / bx);
  v = f32(v * PI4INV);
  w = f32(w * PI4INV);

  return { u, v, w };
}
