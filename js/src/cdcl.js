// Port of AVL cdcl.f with float32 math for numerical fidelity.

const f32 = Math.fround;

export function CDCL(cdclpol, cl) {
  const clmin = f32(cdclpol[0]);
  const cdmin = f32(cdclpol[1]);
  const cl0 = f32(cdclpol[2]);
  const cd0 = f32(cdclpol[3]);
  const clmax = f32(cdclpol[4]);
  const cdmax = f32(cdclpol[5]);

  if (clmax <= cl0 || cl0 <= clmin) {
    return { cd: NaN, cd_cl: NaN, error: 'CDCL: input CL data out of order' };
  }

  const clinc = f32(0.2);
  const cdinc = f32(0.05);

  const cdx1 = f32(f32(2.0 * (cdmin - cd0) * (clmin - cl0)) / f32((clmin - cl0) * (clmin - cl0)));
  const cdx2 = f32(f32(2.0 * (cdmax - cd0) * (clmax - cl0)) / f32((clmax - cl0) * (clmax - cl0)));
  const clfac = f32(1.0 / clinc);

  const clv = f32(cl);
  let cd = f32(0.0);
  let cd_cl = f32(0.0);

  if (clv < clmin) {
    const dcl = f32(clv - clmin);
    cd = f32(cdmin
      + f32(f32(cdinc * f32(clfac * clfac)) * f32(dcl * dcl))
      + f32(cdx1 * f32(1.0 - f32((clv - cl0) / (clmin - cl0)))));
    cd_cl = f32(f32(cdinc * f32(clfac * clfac)) * f32(dcl * 2.0)
      - f32(cdx1 / (clmin - cl0)));
  } else if (clv < cl0) {
    const dcl = f32(clv - cl0);
    cd = f32(cd0 + f32((cdmin - cd0) * f32(dcl * dcl) / f32((clmin - cl0) * (clmin - cl0))));
    cd_cl = f32((cdmin - cd0) * f32(dcl * 2.0) / f32((clmin - cl0) * (clmin - cl0)));
  } else if (clv < clmax) {
    const dcl = f32(clv - cl0);
    cd = f32(cd0 + f32((cdmax - cd0) * f32(dcl * dcl) / f32((clmax - cl0) * (clmax - cl0))));
    cd_cl = f32((cdmax - cd0) * f32(dcl * 2.0) / f32((clmax - cl0) * (clmax - cl0)));
  } else {
    const dcl = f32(clv - clmax);
    cd = f32(cdmax
      + f32(f32(cdinc * f32(clfac * clfac)) * f32(dcl * dcl))
      - f32(cdx2 * f32(1.0 - f32((clv - cl0) / (clmax - cl0)))));
    cd_cl = f32(f32(cdinc * f32(clfac * clfac)) * f32(dcl * 2.0)
      + f32(cdx2 / (clmax - cl0)));
  }

  return { cd, cd_cl };
}
