// Port of AVL matrix-lapacksubs.f (selected LAPACK/BLAS routines).
// Column-major layout with leading dimensions, matching Fortran.

const f32 = Math.fround;

function idx(i, j, lda) {
  return i + j * lda;
}

export function XERBLA(srname, info) {
  // LAPACK error handler; no-op for now.
  return { srname, info };
}

export function SSCAL(n, sa, sx, incx) {
  let ix = 0;
  for (let i = 0; i < n; i += 1) {
    sx[ix] = f32(f32(sa) * f32(sx[ix]));
    ix += incx;
  }
}

export function DSCAL(n, da, dx, incx) {
  let ix = 0;
  for (let i = 0; i < n; i += 1) {
    dx[ix] = da * dx[ix];
    ix += incx;
  }
}

export function SLASWP(n, a, lda, k1, k2, ipiv, incx) {
  let ix = incx > 0 ? k1 - 1 : k2 - 1;
  for (let i = k1; i <= k2; i += 1) {
    const ip = ipiv[ix] - 1;
    if (ip !== i - 1) {
      for (let j = 0; j < n; j += 1) {
        const i1 = idx(i - 1, j, lda);
        const i2 = idx(ip, j, lda);
        const temp = a[i1];
        a[i1] = a[i2];
        a[i2] = temp;
      }
    }
    ix += incx;
  }
}

export function DLASWP(n, a, lda, k1, k2, ipiv, incx) {
  let ix = incx > 0 ? k1 - 1 : k2 - 1;
  for (let i = k1; i <= k2; i += 1) {
    const ip = ipiv[ix] - 1;
    if (ip !== i - 1) {
      for (let j = 0; j < n; j += 1) {
        const i1 = idx(i - 1, j, lda);
        const i2 = idx(ip, j, lda);
        const temp = a[i1];
        a[i1] = a[i2];
        a[i2] = temp;
      }
    }
    ix += incx;
  }
}

export function SGEMM(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc) {
  const ta = transa.toUpperCase();
  const tb = transb.toUpperCase();
  const aN = ta === 'N';
  const bN = tb === 'N';

  for (let j = 0; j < n; j += 1) {
    for (let i = 0; i < m; i += 1) {
      let sum = f32(0.0);
      for (let l = 0; l < k; l += 1) {
        const aval = aN ? a[idx(i, l, lda)] : a[idx(l, i, lda)];
        const bval = bN ? b[idx(l, j, ldb)] : b[idx(j, l, ldb)];
        sum = f32(sum + f32(f32(aval) * f32(bval)));
      }
      const cij = idx(i, j, ldc);
      c[cij] = f32(f32(alpha) * sum + f32(f32(beta) * f32(c[cij])));
    }
  }
}

export function DGEMM(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc) {
  const ta = transa.toUpperCase();
  const tb = transb.toUpperCase();
  const aN = ta === 'N';
  const bN = tb === 'N';

  for (let j = 0; j < n; j += 1) {
    for (let i = 0; i < m; i += 1) {
      let sum = 0.0;
      for (let l = 0; l < k; l += 1) {
        const aval = aN ? a[idx(i, l, lda)] : a[idx(l, i, lda)];
        const bval = bN ? b[idx(l, j, ldb)] : b[idx(j, l, ldb)];
        sum += aval * bval;
      }
      const cij = idx(i, j, ldc);
      c[cij] = alpha * sum + beta * c[cij];
    }
  }
}

export function STRSM(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb) {
  const left = side.toUpperCase() === 'L';
  const upper = uplo.toUpperCase() === 'U';
  const trans = transa.toUpperCase() !== 'N';
  const unit = diag.toUpperCase() === 'U';

  for (let j = 0; j < n; j += 1) {
    for (let i = 0; i < m; i += 1) {
      b[idx(i, j, ldb)] = f32(f32(alpha) * f32(b[idx(i, j, ldb)]));
    }
  }

  if (left) {
    if (!trans) {
      if (upper) {
        for (let j = 0; j < n; j += 1) {
          for (let i = m - 1; i >= 0; i -= 1) {
            let temp = b[idx(i, j, ldb)];
            if (!unit) {
              temp = f32(f32(temp) / f32(a[idx(i, i, lda)]));
              b[idx(i, j, ldb)] = temp;
            }
            for (let k = 0; k < i; k += 1) {
              b[idx(k, j, ldb)] = f32(b[idx(k, j, ldb)] - f32(temp * a[idx(k, i, lda)]));
            }
          }
        }
      } else {
        for (let j = 0; j < n; j += 1) {
          for (let i = 0; i < m; i += 1) {
            let temp = b[idx(i, j, ldb)];
            if (!unit) {
              temp = f32(f32(temp) / f32(a[idx(i, i, lda)]));
              b[idx(i, j, ldb)] = temp;
            }
            for (let k = i + 1; k < m; k += 1) {
              b[idx(k, j, ldb)] = f32(b[idx(k, j, ldb)] - f32(temp * a[idx(k, i, lda)]));
            }
          }
        }
      }
    } else {
      if (upper) {
        for (let j = 0; j < n; j += 1) {
          for (let i = 0; i < m; i += 1) {
            let temp = b[idx(i, j, ldb)];
            for (let k = 0; k < i; k += 1) {
              temp = f32(temp - f32(a[idx(k, i, lda)] * b[idx(k, j, ldb)]));
            }
            if (!unit) temp = f32(f32(temp) / f32(a[idx(i, i, lda)]));
            b[idx(i, j, ldb)] = temp;
          }
        }
      } else {
        for (let j = 0; j < n; j += 1) {
          for (let i = m - 1; i >= 0; i -= 1) {
            let temp = b[idx(i, j, ldb)];
            for (let k = i + 1; k < m; k += 1) {
              temp = f32(temp - f32(a[idx(k, i, lda)] * b[idx(k, j, ldb)]));
            }
            if (!unit) temp = f32(f32(temp) / f32(a[idx(i, i, lda)]));
            b[idx(i, j, ldb)] = temp;
          }
        }
      }
    }
  } else {
    // Right side solve: B * op(A)^{-1}
    if (!trans) {
      if (upper) {
        for (let j = 0; j < n; j += 1) {
          for (let k = 0; k < j; k += 1) {
            if (a[idx(k, j, lda)] !== 0.0) {
              const temp = a[idx(k, j, lda)];
              for (let i = 0; i < m; i += 1) {
                b[idx(i, j, ldb)] = f32(b[idx(i, j, ldb)] - f32(temp * b[idx(i, k, ldb)]));
              }
            }
          }
          if (!unit) {
            const temp = f32(1.0 / f32(a[idx(j, j, lda)]));
            for (let i = 0; i < m; i += 1) {
              b[idx(i, j, ldb)] = f32(b[idx(i, j, ldb)] * temp);
            }
          }
        }
      } else {
        for (let j = n - 1; j >= 0; j -= 1) {
          for (let k = j + 1; k < n; k += 1) {
            if (a[idx(k, j, lda)] !== 0.0) {
              const temp = a[idx(k, j, lda)];
              for (let i = 0; i < m; i += 1) {
                b[idx(i, j, ldb)] = f32(b[idx(i, j, ldb)] - f32(temp * b[idx(i, k, ldb)]));
              }
            }
          }
          if (!unit) {
            const temp = f32(1.0 / f32(a[idx(j, j, lda)]));
            for (let i = 0; i < m; i += 1) {
              b[idx(i, j, ldb)] = f32(b[idx(i, j, ldb)] * temp);
            }
          }
        }
      }
    } else {
      if (upper) {
        for (let j = n - 1; j >= 0; j -= 1) {
          if (!unit) {
            const temp = f32(1.0 / f32(a[idx(j, j, lda)]));
            for (let i = 0; i < m; i += 1) {
              b[idx(i, j, ldb)] = f32(b[idx(i, j, ldb)] * temp);
            }
          }
          for (let k = 0; k < j; k += 1) {
            if (a[idx(k, j, lda)] !== 0.0) {
              const temp = a[idx(k, j, lda)];
              for (let i = 0; i < m; i += 1) {
                b[idx(i, k, ldb)] = f32(b[idx(i, k, ldb)] - f32(temp * b[idx(i, j, ldb)]));
              }
            }
          }
        }
      } else {
        for (let j = 0; j < n; j += 1) {
          if (!unit) {
            const temp = f32(1.0 / f32(a[idx(j, j, lda)]));
            for (let i = 0; i < m; i += 1) {
              b[idx(i, j, ldb)] = f32(b[idx(i, j, ldb)] * temp);
            }
          }
          for (let k = j + 1; k < n; k += 1) {
            if (a[idx(k, j, lda)] !== 0.0) {
              const temp = a[idx(k, j, lda)];
              for (let i = 0; i < m; i += 1) {
                b[idx(i, k, ldb)] = f32(b[idx(i, k, ldb)] - f32(temp * b[idx(i, j, ldb)]));
              }
            }
          }
        }
      }
    }
  }
}

export function DTRSM(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb) {
  const left = side.toUpperCase() === 'L';
  const upper = uplo.toUpperCase() === 'U';
  const trans = transa.toUpperCase() !== 'N';
  const unit = diag.toUpperCase() === 'U';

  for (let j = 0; j < n; j += 1) {
    for (let i = 0; i < m; i += 1) {
      b[idx(i, j, ldb)] = alpha * b[idx(i, j, ldb)];
    }
  }

  if (left) {
    if (!trans) {
      if (upper) {
        for (let j = 0; j < n; j += 1) {
          for (let i = m - 1; i >= 0; i -= 1) {
            let temp = b[idx(i, j, ldb)];
            if (!unit) {
              temp /= a[idx(i, i, lda)];
              b[idx(i, j, ldb)] = temp;
            }
            for (let k = 0; k < i; k += 1) {
              b[idx(k, j, ldb)] -= temp * a[idx(k, i, lda)];
            }
          }
        }
      } else {
        for (let j = 0; j < n; j += 1) {
          for (let i = 0; i < m; i += 1) {
            let temp = b[idx(i, j, ldb)];
            if (!unit) {
              temp /= a[idx(i, i, lda)];
              b[idx(i, j, ldb)] = temp;
            }
            for (let k = i + 1; k < m; k += 1) {
              b[idx(k, j, ldb)] -= temp * a[idx(k, i, lda)];
            }
          }
        }
      }
    } else {
      if (upper) {
        for (let j = 0; j < n; j += 1) {
          for (let i = 0; i < m; i += 1) {
            let temp = b[idx(i, j, ldb)];
            for (let k = 0; k < i; k += 1) {
              temp -= a[idx(k, i, lda)] * b[idx(k, j, ldb)];
            }
            if (!unit) temp /= a[idx(i, i, lda)];
            b[idx(i, j, ldb)] = temp;
          }
        }
      } else {
        for (let j = 0; j < n; j += 1) {
          for (let i = m - 1; i >= 0; i -= 1) {
            let temp = b[idx(i, j, ldb)];
            for (let k = i + 1; k < m; k += 1) {
              temp -= a[idx(k, i, lda)] * b[idx(k, j, ldb)];
            }
            if (!unit) temp /= a[idx(i, i, lda)];
            b[idx(i, j, ldb)] = temp;
          }
        }
      }
    }
  } else {
    if (!trans) {
      if (upper) {
        for (let j = 0; j < n; j += 1) {
          for (let k = 0; k < j; k += 1) {
            if (a[idx(k, j, lda)] !== 0.0) {
              const temp = a[idx(k, j, lda)];
              for (let i = 0; i < m; i += 1) {
                b[idx(i, j, ldb)] -= temp * b[idx(i, k, ldb)];
              }
            }
          }
          if (!unit) {
            const temp = 1.0 / a[idx(j, j, lda)];
            for (let i = 0; i < m; i += 1) {
              b[idx(i, j, ldb)] *= temp;
            }
          }
        }
      } else {
        for (let j = n - 1; j >= 0; j -= 1) {
          for (let k = j + 1; k < n; k += 1) {
            if (a[idx(k, j, lda)] !== 0.0) {
              const temp = a[idx(k, j, lda)];
              for (let i = 0; i < m; i += 1) {
                b[idx(i, j, ldb)] -= temp * b[idx(i, k, ldb)];
              }
            }
          }
          if (!unit) {
            const temp = 1.0 / a[idx(j, j, lda)];
            for (let i = 0; i < m; i += 1) {
              b[idx(i, j, ldb)] *= temp;
            }
          }
        }
      }
    } else {
      if (upper) {
        for (let j = n - 1; j >= 0; j -= 1) {
          if (!unit) {
            const temp = 1.0 / a[idx(j, j, lda)];
            for (let i = 0; i < m; i += 1) {
              b[idx(i, j, ldb)] *= temp;
            }
          }
          for (let k = 0; k < j; k += 1) {
            if (a[idx(k, j, lda)] !== 0.0) {
              const temp = a[idx(k, j, lda)];
              for (let i = 0; i < m; i += 1) {
                b[idx(i, k, ldb)] -= temp * b[idx(i, j, ldb)];
              }
            }
          }
        }
      } else {
        for (let j = 0; j < n; j += 1) {
          if (!unit) {
            const temp = 1.0 / a[idx(j, j, lda)];
            for (let i = 0; i < m; i += 1) {
              b[idx(i, j, ldb)] *= temp;
            }
          }
          for (let k = j + 1; k < n; k += 1) {
            if (a[idx(k, j, lda)] !== 0.0) {
              const temp = a[idx(k, j, lda)];
              for (let i = 0; i < m; i += 1) {
                b[idx(i, k, ldb)] -= temp * b[idx(i, j, ldb)];
              }
            }
          }
        }
      }
    }
  }
}

export function SGETRF(m, n, a, lda, ipiv, info = { value: 0 }) {
  const minmn = Math.min(m, n);
  info.value = 0;

  for (let j = 0; j < minmn; j += 1) {
    let jp = j;
    let max = Math.abs(a[idx(j, j, lda)]);
    for (let i = j + 1; i < m; i += 1) {
      const val = Math.abs(a[idx(i, j, lda)]);
      if (val > max) {
        max = val;
        jp = i;
      }
    }
    ipiv[j] = jp + 1;
    if (a[idx(jp, j, lda)] !== 0.0) {
      if (jp !== j) {
        for (let k = 0; k < n; k += 1) {
          const i1 = idx(j, k, lda);
          const i2 = idx(jp, k, lda);
          const temp = a[i1];
          a[i1] = a[i2];
          a[i2] = temp;
        }
      }
      if (j < m - 1) {
        const ajj = a[idx(j, j, lda)];
        for (let i = j + 1; i < m; i += 1) {
          a[idx(i, j, lda)] = f32(a[idx(i, j, lda)] / ajj);
        }
      }
    } else if (info.value === 0) {
      info.value = j + 1;
    }

    if (j < minmn - 1) {
      for (let i = j + 1; i < m; i += 1) {
        const aij = a[idx(i, j, lda)];
        if (aij !== 0.0) {
          for (let k = j + 1; k < n; k += 1) {
            a[idx(i, k, lda)] = f32(a[idx(i, k, lda)] - f32(aij * a[idx(j, k, lda)]));
          }
        }
      }
    }
  }
}

export function DGETRF(m, n, a, lda, ipiv, info = { value: 0 }) {
  const minmn = Math.min(m, n);
  info.value = 0;

  for (let j = 0; j < minmn; j += 1) {
    let jp = j;
    let max = Math.abs(a[idx(j, j, lda)]);
    for (let i = j + 1; i < m; i += 1) {
      const val = Math.abs(a[idx(i, j, lda)]);
      if (val > max) {
        max = val;
        jp = i;
      }
    }
    ipiv[j] = jp + 1;
    if (a[idx(jp, j, lda)] !== 0.0) {
      if (jp !== j) {
        for (let k = 0; k < n; k += 1) {
          const i1 = idx(j, k, lda);
          const i2 = idx(jp, k, lda);
          const temp = a[i1];
          a[i1] = a[i2];
          a[i2] = temp;
        }
      }
      if (j < m - 1) {
        const ajj = a[idx(j, j, lda)];
        for (let i = j + 1; i < m; i += 1) {
          a[idx(i, j, lda)] = a[idx(i, j, lda)] / ajj;
        }
      }
    } else if (info.value === 0) {
      info.value = j + 1;
    }

    if (j < minmn - 1) {
      for (let i = j + 1; i < m; i += 1) {
        const aij = a[idx(i, j, lda)];
        if (aij !== 0.0) {
          for (let k = j + 1; k < n; k += 1) {
            a[idx(i, k, lda)] -= aij * a[idx(j, k, lda)];
          }
        }
      }
    }
  }
}

export function SGETRS(trans, n, nrhs, a, lda, ipiv, b, ldb, info = { value: 0 }) {
  info.value = 0;
  if (trans.toUpperCase() !== 'N') {
    XERBLA('SGETRS', -1);
    return;
  }

  SLASWP(nrhs, b, ldb, 1, n, ipiv, 1);

  // Solve L*X = B
  for (let k = 0; k < n; k += 1) {
    for (let i = k + 1; i < n; i += 1) {
      const aik = a[idx(i, k, lda)];
      if (aik !== 0.0) {
        for (let j = 0; j < nrhs; j += 1) {
          b[idx(i, j, ldb)] = f32(b[idx(i, j, ldb)] - f32(aik * b[idx(k, j, ldb)]));
        }
      }
    }
  }

  // Solve U*X = B
  for (let k = n - 1; k >= 0; k -= 1) {
    const akk = a[idx(k, k, lda)];
    for (let j = 0; j < nrhs; j += 1) {
      b[idx(k, j, ldb)] = f32(b[idx(k, j, ldb)] / akk);
    }
    for (let i = 0; i < k; i += 1) {
      const aik = a[idx(i, k, lda)];
      if (aik !== 0.0) {
        for (let j = 0; j < nrhs; j += 1) {
          b[idx(i, j, ldb)] = f32(b[idx(i, j, ldb)] - f32(aik * b[idx(k, j, ldb)]));
        }
      }
    }
  }
}

export function DGETRS(trans, n, nrhs, a, lda, ipiv, b, ldb, info = { value: 0 }) {
  info.value = 0;
  if (trans.toUpperCase() !== 'N') {
    XERBLA('DGETRS', -1);
    return;
  }

  DLASWP(nrhs, b, ldb, 1, n, ipiv, 1);

  for (let k = 0; k < n; k += 1) {
    for (let i = k + 1; i < n; i += 1) {
      const aik = a[idx(i, k, lda)];
      if (aik !== 0.0) {
        for (let j = 0; j < nrhs; j += 1) {
          b[idx(i, j, ldb)] -= aik * b[idx(k, j, ldb)];
        }
      }
    }
  }

  for (let k = n - 1; k >= 0; k -= 1) {
    const akk = a[idx(k, k, lda)];
    for (let j = 0; j < nrhs; j += 1) {
      b[idx(k, j, ldb)] /= akk;
    }
    for (let i = 0; i < k; i += 1) {
      const aik = a[idx(i, k, lda)];
      if (aik !== 0.0) {
        for (let j = 0; j < nrhs; j += 1) {
          b[idx(i, j, ldb)] -= aik * b[idx(k, j, ldb)];
        }
      }
    }
  }
}
