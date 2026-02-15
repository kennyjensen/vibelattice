/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 *
 * EISPACK routines used by AVL EIGSOL:
 *   CDIV, BALANC, ELMHES, ELTRAN, HQR2, BALBAK, RG
 */

function idx(i, j, ld) {
  return i + ld * j;
}

function dsign(a, b) {
  return b >= 0 ? Math.abs(a) : -Math.abs(a);
}

function cdiv(ar, ai, br, bi) {
  const s0 = Math.abs(br) + Math.abs(bi);
  if (s0 === 0) {
    return { cr: 0, ci: 0 };
  }
  const ars = ar / s0;
  const ais = ai / s0;
  const brs = br / s0;
  const bis = bi / s0;
  const s = brs * brs + bis * bis;
  return {
    cr: (ars * brs + ais * bis) / s,
    ci: (ais * brs - ars * bis) / s,
  };
}

function balanc(nm, n, a, scale) {
  const radix = 16.0;
  const b2 = radix * radix;
  let k = 1;
  let l = n;

  function exchange(j, m, iexc) {
    scale[m] = j;
    if (j !== m) {
      for (let i = 1; i <= l; i += 1) {
        const f = a[idx(i, j, nm + 1)];
        a[idx(i, j, nm + 1)] = a[idx(i, m, nm + 1)];
        a[idx(i, m, nm + 1)] = f;
      }
      for (let i = k; i <= n; i += 1) {
        const f = a[idx(j, i, nm + 1)];
        a[idx(j, i, nm + 1)] = a[idx(m, i, nm + 1)];
        a[idx(m, i, nm + 1)] = f;
      }
    }
    if (iexc === 1) {
      l -= 1;
    } else {
      k += 1;
    }
  }

  while (true) {
    let moved = false;

    if (l !== 1) {
      for (let jj = 1; jj <= l; jj += 1) {
        const j = l + 1 - jj;
        let isolated = true;
        for (let i = 1; i <= l; i += 1) {
          if (i === j) continue;
          if (a[idx(j, i, nm + 1)] !== 0.0) {
            isolated = false;
            break;
          }
        }
        if (isolated) {
          exchange(j, l, 1);
          moved = true;
          break;
        }
      }
    }
    if (moved) continue;

    for (let j = k; j <= l; j += 1) {
      let isolated = true;
      for (let i = k; i <= l; i += 1) {
        if (i === j) continue;
        if (a[idx(i, j, nm + 1)] !== 0.0) {
          isolated = false;
          break;
        }
      }
      if (isolated) {
        exchange(j, k, 2);
        moved = true;
        break;
      }
    }
    if (moved) continue;

    break;
  }

  for (let i = k; i <= l; i += 1) {
    scale[i] = 1.0;
  }

  let noconv = true;
  while (noconv) {
    noconv = false;
    for (let i = k; i <= l; i += 1) {
      let c = 0.0;
      let r = 0.0;
      for (let j = k; j <= l; j += 1) {
        if (j === i) continue;
        c += Math.abs(a[idx(j, i, nm + 1)]);
        r += Math.abs(a[idx(i, j, nm + 1)]);
      }
      if (c === 0.0 || r === 0.0) continue;

      let g = r / radix;
      let f = 1.0;
      const s = c + r;

      while (c < g) {
        f *= radix;
        c *= b2;
      }
      g = r * radix;
      while (c >= g) {
        f /= radix;
        c /= b2;
      }

      if ((c + r) / f >= 0.95 * s) continue;

      g = 1.0 / f;
      scale[i] *= f;
      noconv = true;

      for (let j = k; j <= n; j += 1) {
        a[idx(i, j, nm + 1)] *= g;
      }
      for (let j = 1; j <= l; j += 1) {
        a[idx(j, i, nm + 1)] *= f;
      }
    }
  }

  return { low: k, igh: l };
}

function elmhes(nm, n, low, igh, a, intv) {
  const la = igh - 1;
  const kp1 = low + 1;
  if (la < kp1) return;

  for (let m = kp1; m <= la; m += 1) {
    const mm1 = m - 1;
    let x = 0.0;
    let i = m;
    for (let j = m; j <= igh; j += 1) {
      const v = Math.abs(a[idx(j, mm1, nm + 1)]);
      if (v > Math.abs(x)) {
        x = a[idx(j, mm1, nm + 1)];
        i = j;
      }
    }

    intv[m] = i;
    if (i !== m) {
      for (let j = mm1; j <= n; j += 1) {
        const y = a[idx(i, j, nm + 1)];
        a[idx(i, j, nm + 1)] = a[idx(m, j, nm + 1)];
        a[idx(m, j, nm + 1)] = y;
      }
      for (let j = 1; j <= igh; j += 1) {
        const y = a[idx(j, i, nm + 1)];
        a[idx(j, i, nm + 1)] = a[idx(j, m, nm + 1)];
        a[idx(j, m, nm + 1)] = y;
      }
    }

    if (x === 0.0) continue;
    const mp1 = m + 1;
    for (i = mp1; i <= igh; i += 1) {
      let y = a[idx(i, mm1, nm + 1)];
      if (y === 0.0) continue;
      y /= x;
      a[idx(i, mm1, nm + 1)] = y;
      for (let j = m; j <= n; j += 1) {
        a[idx(i, j, nm + 1)] -= y * a[idx(m, j, nm + 1)];
      }
      for (let j = 1; j <= igh; j += 1) {
        a[idx(j, m, nm + 1)] += y * a[idx(j, i, nm + 1)];
      }
    }
  }
}

function eltran(nm, n, low, igh, a, intv, z) {
  for (let j = 1; j <= n; j += 1) {
    for (let i = 1; i <= n; i += 1) z[idx(i, j, nm + 1)] = 0.0;
    z[idx(j, j, nm + 1)] = 1.0;
  }

  const kl = igh - low - 1;
  if (kl < 1) return;

  for (let mm = 1; mm <= kl; mm += 1) {
    const mp = igh - mm;
    const mp1 = mp + 1;
    for (let i = mp1; i <= igh; i += 1) {
      z[idx(i, mp, nm + 1)] = a[idx(i, mp - 1, nm + 1)];
    }
    const i = intv[mp];
    if (i === mp) continue;
    for (let j = mp; j <= igh; j += 1) {
      z[idx(mp, j, nm + 1)] = z[idx(i, j, nm + 1)];
      z[idx(i, j, nm + 1)] = 0.0;
    }
    z[idx(i, mp, nm + 1)] = 1.0;
  }
}

function hqr2(nm, n, low, igh, h, wr, wi, z) {
  let ierr = 0;
  let norm = 0.0;
  let k = 1;

  for (let i = 1; i <= n; i += 1) {
    for (let j = k; j <= n; j += 1) {
      norm += Math.abs(h[idx(i, j, nm + 1)]);
    }
    k = i;
    if (i < low || i > igh) {
      wr[i] = h[idx(i, i, nm + 1)];
      wi[i] = 0.0;
    }
  }

  let en = igh;
  let t = 0.0;
  let itn = 30 * n;

  while (en >= low) {
    let its = 0;

    while (true) {
      const na = en - 1;
      const enm2 = na - 1;

      let l;
      for (let ll = low; ll <= en; ll += 1) {
        l = en + low - ll;
        if (l === low) break;
        let s = Math.abs(h[idx(l - 1, l - 1, nm + 1)]) + Math.abs(h[idx(l, l, nm + 1)]);
        if (s === 0.0) s = norm;
        const tst1 = s;
        const tst2 = tst1 + Math.abs(h[idx(l, l - 1, nm + 1)]);
        if (tst2 === tst1) break;
      }

      let x = h[idx(en, en, nm + 1)];
      if (l === en) {
        h[idx(en, en, nm + 1)] = x + t;
        wr[en] = h[idx(en, en, nm + 1)];
        wi[en] = 0.0;
        en = na;
        break;
      }

      let y = h[idx(na, na, nm + 1)];
      let w = h[idx(en, na, nm + 1)] * h[idx(na, en, nm + 1)];

      if (l === na) {
        let p = (y - x) / 2.0;
        let q = p * p + w;
        let zz = Math.sqrt(Math.abs(q));
        h[idx(en, en, nm + 1)] = x + t;
        x = h[idx(en, en, nm + 1)];
        h[idx(na, na, nm + 1)] = y + t;

        if (q >= 0.0) {
          zz = p + dsign(zz, p);
          wr[na] = x + zz;
          wr[en] = wr[na];
          if (zz !== 0.0) wr[en] = x - w / zz;
          wi[na] = 0.0;
          wi[en] = 0.0;

          x = h[idx(en, na, nm + 1)];
          let s = Math.abs(x) + Math.abs(zz);
          p = x / s;
          q = zz / s;
          let r = Math.sqrt(p * p + q * q);
          p /= r;
          q /= r;

          for (let j = na; j <= n; j += 1) {
            zz = h[idx(na, j, nm + 1)];
            h[idx(na, j, nm + 1)] = q * zz + p * h[idx(en, j, nm + 1)];
            h[idx(en, j, nm + 1)] = q * h[idx(en, j, nm + 1)] - p * zz;
          }
          for (let i = 1; i <= en; i += 1) {
            zz = h[idx(i, na, nm + 1)];
            h[idx(i, na, nm + 1)] = q * zz + p * h[idx(i, en, nm + 1)];
            h[idx(i, en, nm + 1)] = q * h[idx(i, en, nm + 1)] - p * zz;
          }
          for (let i = low; i <= igh; i += 1) {
            zz = z[idx(i, na, nm + 1)];
            z[idx(i, na, nm + 1)] = q * zz + p * z[idx(i, en, nm + 1)];
            z[idx(i, en, nm + 1)] = q * z[idx(i, en, nm + 1)] - p * zz;
          }
        } else {
          wr[na] = x + p;
          wr[en] = x + p;
          wi[na] = zz;
          wi[en] = -zz;
        }

        en = enm2;
        break;
      }

      if (itn === 0) {
        ierr = en;
        return ierr;
      }

      if (its === 10 || its === 20) {
        t += x;
        for (let i = low; i <= en; i += 1) {
          h[idx(i, i, nm + 1)] -= x;
        }
        const s = Math.abs(h[idx(en, na, nm + 1)]) + Math.abs(h[idx(na, enm2, nm + 1)]);
        x = 0.75 * s;
        y = x;
        w = -0.4375 * s * s;
      }

      its += 1;
      itn -= 1;

      let m;
      let pM = 0.0;
      let qM = 0.0;
      let rM = 0.0;
      for (let mm = l; mm <= enm2; mm += 1) {
        m = enm2 + l - mm;
        let zz = h[idx(m, m, nm + 1)];
        let r = x - zz;
        let s = y - zz;
        let p = (r * s - w) / h[idx(m + 1, m, nm + 1)] + h[idx(m, m + 1, nm + 1)];
        let q = h[idx(m + 1, m + 1, nm + 1)] - zz - r - s;
        r = h[idx(m + 2, m + 1, nm + 1)];
        s = Math.abs(p) + Math.abs(q) + Math.abs(r);
        p /= s;
        q /= s;
        r /= s;
        pM = p;
        qM = q;
        rM = r;

        if (m === l) break;
        const tst1 = Math.abs(p) * (
          Math.abs(h[idx(m - 1, m - 1, nm + 1)])
          + Math.abs(zz)
          + Math.abs(h[idx(m + 1, m + 1, nm + 1)])
        );
        const tst2 = tst1 + Math.abs(h[idx(m, m - 1, nm + 1)]) * (Math.abs(q) + Math.abs(r));
        if (tst2 === tst1) break;
      }

      const mp2 = m + 2;
      for (let i = mp2; i <= en; i += 1) {
        h[idx(i, i - 2, nm + 1)] = 0.0;
        if (i !== mp2) h[idx(i, i - 3, nm + 1)] = 0.0;
      }

      for (k = m; k <= na; k += 1) {
        const notlas = k !== na;
        let p;
        let q;
        let r;
        if (k === m) {
          p = pM;
          q = qM;
          r = rM;
        } else {
          p = h[idx(k, k - 1, nm + 1)];
          q = h[idx(k + 1, k - 1, nm + 1)];
          r = notlas ? h[idx(k + 2, k - 1, nm + 1)] : 0.0;
          x = Math.abs(p) + Math.abs(q) + Math.abs(r);
          if (x === 0.0) continue;
          p /= x;
          q /= x;
          r /= x;
        }

        let s = dsign(Math.sqrt(p * p + q * q + r * r), p);
        if (k !== m) {
          h[idx(k, k - 1, nm + 1)] = -s * x;
        } else if (l !== m) {
          h[idx(k, k - 1, nm + 1)] = -h[idx(k, k - 1, nm + 1)];
        }

        p += s;
        x = p / s;
        y = q / s;
        let zz = r / s;
        q /= p;
        r /= p;

        if (!notlas) {
          for (let j = k; j <= n; j += 1) {
            p = h[idx(k, j, nm + 1)] + q * h[idx(k + 1, j, nm + 1)];
            h[idx(k, j, nm + 1)] -= p * x;
            h[idx(k + 1, j, nm + 1)] -= p * y;
          }
          let j = Math.min(en, k + 3);
          for (let i = 1; i <= j; i += 1) {
            p = x * h[idx(i, k, nm + 1)] + y * h[idx(i, k + 1, nm + 1)];
            h[idx(i, k, nm + 1)] -= p;
            h[idx(i, k + 1, nm + 1)] -= p * q;
          }
          for (let i = low; i <= igh; i += 1) {
            p = x * z[idx(i, k, nm + 1)] + y * z[idx(i, k + 1, nm + 1)];
            z[idx(i, k, nm + 1)] -= p;
            z[idx(i, k + 1, nm + 1)] -= p * q;
          }
        } else {
          for (let j = k; j <= n; j += 1) {
            p = h[idx(k, j, nm + 1)] + q * h[idx(k + 1, j, nm + 1)] + r * h[idx(k + 2, j, nm + 1)];
            h[idx(k, j, nm + 1)] -= p * x;
            h[idx(k + 1, j, nm + 1)] -= p * y;
            h[idx(k + 2, j, nm + 1)] -= p * zz;
          }
          let j = Math.min(en, k + 3);
          for (let i = 1; i <= j; i += 1) {
            p = x * h[idx(i, k, nm + 1)] + y * h[idx(i, k + 1, nm + 1)] + zz * h[idx(i, k + 2, nm + 1)];
            h[idx(i, k, nm + 1)] -= p;
            h[idx(i, k + 1, nm + 1)] -= p * q;
            h[idx(i, k + 2, nm + 1)] -= p * r;
          }
          for (let i = low; i <= igh; i += 1) {
            p = x * z[idx(i, k, nm + 1)] + y * z[idx(i, k + 1, nm + 1)] + zz * z[idx(i, k + 2, nm + 1)];
            z[idx(i, k, nm + 1)] -= p;
            z[idx(i, k + 1, nm + 1)] -= p * q;
            z[idx(i, k + 2, nm + 1)] -= p * r;
          }
        }
      }
    }
  }

  if (norm === 0.0) {
    return ierr;
  }

  for (let nn = 1; nn <= n; nn += 1) {
    en = n + 1 - nn;
    let p = wr[en];
    let q = wi[en];
    const na = en - 1;

    if (q < 0.0) {
      let m = na;
      if (Math.abs(h[idx(en, na, nm + 1)]) > Math.abs(h[idx(na, en, nm + 1)])) {
        h[idx(na, na, nm + 1)] = q / h[idx(en, na, nm + 1)];
        h[idx(na, en, nm + 1)] = -(h[idx(en, en, nm + 1)] - p) / h[idx(en, na, nm + 1)];
      } else {
        const div = cdiv(0.0, -h[idx(na, en, nm + 1)], h[idx(na, na, nm + 1)] - p, q);
        h[idx(na, na, nm + 1)] = div.cr;
        h[idx(na, en, nm + 1)] = div.ci;
      }
      h[idx(en, na, nm + 1)] = 0.0;
      h[idx(en, en, nm + 1)] = 1.0;

      const enm2 = na - 1;
      for (let ii = 1; ii <= enm2; ii += 1) {
        const i = na - ii;
        const w = h[idx(i, i, nm + 1)] - p;
        let ra = 0.0;
        let sa = 0.0;
        for (let j = m; j <= en; j += 1) {
          ra += h[idx(i, j, nm + 1)] * h[idx(j, na, nm + 1)];
          sa += h[idx(i, j, nm + 1)] * h[idx(j, en, nm + 1)];
        }

        if (wi[i] < 0.0) {
          var zz = w;
          var r = ra;
          var s = sa;
          continue;
        }

        m = i;
        if (wi[i] === 0.0) {
          const div = cdiv(-ra, -sa, w, q);
          h[idx(i, na, nm + 1)] = div.cr;
          h[idx(i, en, nm + 1)] = div.ci;
        } else {
          const x = h[idx(i, i + 1, nm + 1)];
          const y = h[idx(i + 1, i, nm + 1)];
          let vr = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i] - q * q;
          let vi = (wr[i] - p) * 2.0 * q;
          if (vr === 0.0 && vi === 0.0) {
            const tst1 = norm * (Math.abs(w) + Math.abs(q) + Math.abs(x) + Math.abs(y) + Math.abs(zz));
            vr = tst1;
            while (true) {
              vr *= 0.01;
              const tst2 = tst1 + vr;
              if (tst2 > tst1) break;
            }
          }
          const div = cdiv(x * r - zz * ra + q * sa, x * s - zz * sa - q * ra, vr, vi);
          h[idx(i, na, nm + 1)] = div.cr;
          h[idx(i, en, nm + 1)] = div.ci;

          if (Math.abs(x) > Math.abs(zz) + Math.abs(q)) {
            h[idx(i + 1, na, nm + 1)] = (-ra - w * h[idx(i, na, nm + 1)] + q * h[idx(i, en, nm + 1)]) / x;
            h[idx(i + 1, en, nm + 1)] = (-sa - w * h[idx(i, en, nm + 1)] - q * h[idx(i, na, nm + 1)]) / x;
          } else {
            const div2 = cdiv(-r - y * h[idx(i, na, nm + 1)], -s - y * h[idx(i, en, nm + 1)], zz, q);
            h[idx(i + 1, na, nm + 1)] = div2.cr;
            h[idx(i + 1, en, nm + 1)] = div2.ci;
          }
        }

        let t0 = Math.max(Math.abs(h[idx(i, na, nm + 1)]), Math.abs(h[idx(i, en, nm + 1)]));
        if (t0 !== 0.0) {
          const tst1 = t0;
          const tst2 = tst1 + 1.0 / tst1;
          if (!(tst2 > tst1)) {
            for (let j = i; j <= en; j += 1) {
              h[idx(j, na, nm + 1)] /= t0;
              h[idx(j, en, nm + 1)] /= t0;
            }
          }
        }
      }
      continue;
    }

    if (q > 0.0) {
      continue;
    }

    let m = en;
    h[idx(en, en, nm + 1)] = 1.0;
    for (let ii = 1; ii <= na; ii += 1) {
      const i = en - ii;
      const w = h[idx(i, i, nm + 1)] - p;
      let r = 0.0;
      for (let j = m; j <= en; j += 1) {
        r += h[idx(i, j, nm + 1)] * h[idx(j, en, nm + 1)];
      }

      if (wi[i] < 0.0) {
        var zz = w;
        var s = r;
        continue;
      }

      m = i;
      if (wi[i] === 0.0) {
        let t0 = w;
        if (t0 === 0.0) {
          const tst1 = norm;
          t0 = tst1;
          while (true) {
            t0 *= 0.01;
            const tst2 = norm + t0;
            if (tst2 > tst1) break;
          }
        }
        h[idx(i, en, nm + 1)] = -r / t0;
      } else {
        const x = h[idx(i, i + 1, nm + 1)];
        const y = h[idx(i + 1, i, nm + 1)];
        const q0 = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i];
        const t0 = (x * s - zz * r) / q0;
        h[idx(i, en, nm + 1)] = t0;
        if (Math.abs(x) > Math.abs(zz)) {
          h[idx(i + 1, en, nm + 1)] = (-r - w * t0) / x;
        } else {
          h[idx(i + 1, en, nm + 1)] = (-s - y * t0) / zz;
        }
      }

      let t0 = Math.abs(h[idx(i, en, nm + 1)]);
      if (t0 !== 0.0) {
        const tst1 = t0;
        const tst2 = tst1 + 1.0 / tst1;
        if (!(tst2 > tst1)) {
          for (let j = i; j <= en; j += 1) {
            h[idx(j, en, nm + 1)] /= t0;
          }
        }
      }
    }
  }

  for (let i = 1; i <= n; i += 1) {
    if (i < low || i > igh) {
      for (let j = i; j <= n; j += 1) {
        z[idx(i, j, nm + 1)] = h[idx(i, j, nm + 1)];
      }
    }
  }

  for (let jj = low; jj <= n; jj += 1) {
    const j = n + low - jj;
    const m = Math.min(j, igh);
    for (let i = low; i <= igh; i += 1) {
      let zz = 0.0;
      for (let kk = low; kk <= m; kk += 1) {
        zz += z[idx(i, kk, nm + 1)] * h[idx(kk, j, nm + 1)];
      }
      z[idx(i, j, nm + 1)] = zz;
    }
  }

  return ierr;
}

function balbak(nm, n, low, igh, scale, m, z) {
  if (m === 0) return;

  if (igh !== low) {
    for (let i = low; i <= igh; i += 1) {
      const s = scale[i];
      for (let j = 1; j <= m; j += 1) {
        z[idx(i, j, nm + 1)] *= s;
      }
    }
  }

  for (let ii = 1; ii <= n; ii += 1) {
    let i = ii;
    if (i >= low && i <= igh) continue;
    if (i < low) i = low - ii;
    const k = Math.trunc(scale[i]);
    if (k === i) continue;
    for (let j = 1; j <= m; j += 1) {
      const s = z[idx(i, j, nm + 1)];
      z[idx(i, j, nm + 1)] = z[idx(k, j, nm + 1)];
      z[idx(k, j, nm + 1)] = s;
    }
  }
}

export function rg(n, aIn, matz = 1) {
  const nm = n;
  const ld = nm + 1;

  const a = new Float64Array(ld * ld);
  a.set(aIn);

  const wr = new Float64Array(n + 1);
  const wi = new Float64Array(n + 1);
  const z = new Float64Array(ld * ld);
  const scale = new Float64Array(n + 1);
  const iv1 = new Int32Array(n + 1);

  if (n > nm) {
    return { ierr: 10 * n, wr, wi, z };
  }

  const { low, igh } = balanc(nm, n, a, scale);
  elmhes(nm, n, low, igh, a, iv1);

  let ierr = 0;
  if (matz === 0) {
    // Not needed by AVL (always eigenvectors too).
    ierr = hqr2(nm, n, low, igh, a, wr, wi, z);
  } else {
    eltran(nm, n, low, igh, a, iv1, z);
    ierr = hqr2(nm, n, low, igh, a, wr, wi, z);
    if (ierr === 0) {
      balbak(nm, n, low, igh, scale, n, z);
    }
  }

  return { ierr, wr, wi, z };
}

export function eigSolveRgFromAsys(asys, nsys, srcLd) {
  const n = Math.max(0, Math.trunc(nsys));
  const ld = n + 1;
  const a = new Float64Array(ld * ld);
  for (let j = 1; j <= n; j += 1) {
    for (let i = 1; i <= n; i += 1) {
      a[idx(i, j, ld)] = Number(asys[idx(i, j, srcLd)] || 0.0);
    }
  }
  const out = rg(n, a, 1);

  const wr = new Float64Array(n);
  const wi = new Float64Array(n);
  const vr = new Float64Array(n * n);

  for (let j = 1; j <= n; j += 1) {
    wr[j - 1] = out.wr[j];
    wi[j - 1] = out.wi[j];
    for (let i = 1; i <= n; i += 1) {
      vr[(i - 1) * n + (j - 1)] = out.z[idx(i, j, ld)];
    }
  }

  return { ierr: out.ierr, wr, wi, vr };
}
