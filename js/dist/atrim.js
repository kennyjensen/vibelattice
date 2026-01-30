// Port of AVL atrim.f (TRMSET core) with float32 math for numerical fidelity.

const f32 = Math.fround;

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

export function TRMSET_CORE(state, KTRIM, IR1, IR2, IR) {
  const ktrim = KTRIM;

  if (ktrim === 1 || ktrim === 2) {
    if (state.PARVAL[idx2(state.IPCL, IR, state.IPTOT)] === 0.0) {
      for (let iv = 1; iv <= state.NVTOT; iv += 1) {
        if (state.ICON[idx2(iv, IR, state.IVTOT)] === state.ICCL) {
          state.PARVAL[idx2(state.IPCL, IR, state.IPTOT)] = state.CONVAL[idx2(state.ICCL, IR, state.ICMAX)];
          break;
        }
      }
    }
  }

  state.ITRIM[IR] = -ktrim;

  if (state.PARVAL[idx2(state.IPRHO, IR, state.IPTOT)] <= 0.0) {
    state.PARVAL[idx2(state.IPRHO, IR, state.IPTOT)] = state.RHO0;
  }
  if (state.PARVAL[idx2(state.IPGEE, IR, state.IPTOT)] <= 0.0) {
    state.PARVAL[idx2(state.IPGEE, IR, state.IPTOT)] = state.GEE0;
  }
  if (state.PARVAL[idx2(state.IPMASS, IR, state.IPTOT)] <= 0.0) {
    state.PARVAL[idx2(state.IPMASS, IR, state.IPTOT)] = state.RMASS0;
  }

  const crefD = f32(state.CREF * state.UNITL);
  const brefD = f32(state.BREF * state.UNITL);
  const srefD = f32(state.SREF * f32(state.UNITL * state.UNITL));

  for (let jr = IR1; jr <= IR2; jr += 1) {
    let phi = state.PARVAL[idx2(state.IPPHI, jr, state.IPTOT)];
    let the = state.PARVAL[idx2(state.IPTHE, jr, state.IPTOT)];
    let cl = state.PARVAL[idx2(state.IPCL, jr, state.IPTOT)];
    let vee = state.PARVAL[idx2(state.IPVEE, jr, state.IPTOT)];
    let rad = state.PARVAL[idx2(state.IPRAD, jr, state.IPTOT)];
    const rho = state.PARVAL[idx2(state.IPRHO, jr, state.IPTOT)];
    const gee = state.PARVAL[idx2(state.IPGEE, jr, state.IPTOT)];
    let fac = state.PARVAL[idx2(state.IPFAC, jr, state.IPTOT)];
    const rmass = state.PARVAL[idx2(state.IPMASS, jr, state.IPTOT)];

    const sinp = f32(Math.sin(f32(phi * state.DTR)));
    const cosp = f32(Math.cos(f32(phi * state.DTR)));

    if (ktrim === 1) {
      if (vee <= 0.0 && cl > 0.0) {
        vee = f32(Math.sqrt(f32(f32(2.0 * rmass * gee) / f32(rho * srefD * cl * cosp))));
        state.PARVAL[idx2(state.IPVEE, jr, state.IPTOT)] = vee;
      }
      if (cl <= 0.0 && vee > 0.0) {
        cl = f32(f32(2.0 * rmass * gee) / f32(rho * srefD * vee * vee * cosp));
        state.PARVAL[idx2(state.IPCL, jr, state.IPTOT)] = cl;
      }

      if (sinp === 0.0) {
        rad = 0.0;
      } else {
        rad = f32(f32(vee * vee * cosp) / f32(gee * sinp));
      }
      state.PARVAL[idx2(state.IPRAD, jr, state.IPTOT)] = rad;

      fac = f32(1.0 / cosp);
      state.PARVAL[idx2(state.IPFAC, jr, state.IPTOT)] = fac;

      the = 0.0;
      state.PARVAL[idx2(state.IPTHE, jr, state.IPTOT)] = the;

      let whx = 0.0;
      let why = 0.0;
      let whz = 0.0;
      if (rad > 0.0) {
        why = f32(f32(sinp * crefD) / f32(2.0 * rad));
        whz = f32(f32(cosp * brefD) / f32(2.0 * rad));
      }

      state.CONVAL[idx2(state.ICCL, jr, state.ICMAX)] = cl;
      state.CONVAL[idx2(state.ICROTX, jr, state.ICMAX)] = whx;
      state.CONVAL[idx2(state.ICROTY, jr, state.ICMAX)] = why;
      state.CONVAL[idx2(state.ICROTZ, jr, state.ICMAX)] = whz;

      state.ICON[idx2(state.IVALFA, jr, state.IVTOT)] = state.ICCL;
      state.ICON[idx2(state.IVROTX, jr, state.IVTOT)] = state.ICROTX;
      state.ICON[idx2(state.IVROTY, jr, state.IVTOT)] = state.ICROTY;
      state.ICON[idx2(state.IVROTZ, jr, state.IVTOT)] = state.ICROTZ;
    } else if (ktrim === 2) {
      if (rad === 0.0 && cl > 0.0) {
        rad = f32(rmass / f32(0.5 * rho * srefD * cl));
        state.PARVAL[idx2(state.IPRAD, jr, state.IPTOT)] = rad;
      }
      if (rad > 0.0 && cl === 0.0) {
        cl = f32(rmass / f32(0.5 * rho * srefD * rad));
        state.PARVAL[idx2(state.IPCL, jr, state.IPTOT)] = cl;
      }
      if (fac === 0.0 && cl > 0.0 && vee > 0.0 && gee > 0.0) {
        fac = f32(f32(0.5 * rho * vee * vee) * f32(srefD * cl) / f32(rmass * gee));
        state.PARVAL[idx2(state.IPFAC, jr, state.IPTOT)] = fac;
      }
      if (fac > 0.0 && cl > 0.0 && vee === 0.0 && gee > 0.0) {
        vee = f32(Math.sqrt(f32(f32(fac * rmass * gee) / f32(0.5 * rho * srefD * cl))));
        state.PARVAL[idx2(state.IPVEE, jr, state.IPTOT)] = vee;
      }

      the = 0.0;
      state.PARVAL[idx2(state.IPTHE, jr, state.IPTOT)] = the;

      let whx = 0.0;
      let why = 0.0;
      let whz = 0.0;
      if (rad > 0.0) {
        why = f32(crefD / f32(2.0 * rad));
      }

      state.CONVAL[idx2(state.ICCL, jr, state.ICMAX)] = cl;
      state.CONVAL[idx2(state.ICROTX, jr, state.ICMAX)] = whx;
      state.CONVAL[idx2(state.ICROTY, jr, state.ICMAX)] = why;
      state.CONVAL[idx2(state.ICROTZ, jr, state.ICMAX)] = whz;

      state.ICON[idx2(state.IVALFA, jr, state.IVTOT)] = state.ICCL;
      state.ICON[idx2(state.IVROTX, jr, state.IVTOT)] = state.ICROTX;
      state.ICON[idx2(state.IVROTY, jr, state.IVTOT)] = state.ICROTY;
      state.ICON[idx2(state.IVROTZ, jr, state.IVTOT)] = state.ICROTZ;
    }
  }

  return state;
}

export function TRMSET(state, COMAND, COMARG, IR, options = {}) {
  if (!COMAND || COMAND[0] !== 'C') {
    return { LMATCH: false };
  }

  const cnum = (COMARG ?? '').slice(0, 2);
  let ktrim = 0;
  if (cnum === '1 ') {
    ktrim = 1;
  } else if (cnum === '2 ') {
    ktrim = 2;
  } else {
    return { LMATCH: false };
  }

  TRMSET_CORE(state, ktrim, IR, IR, IR);
  if (options.skipMenu) {
    return { LMATCH: true };
  }

  // Interactive portion is not implemented in the JS port.
  return { LMATCH: true };
}
