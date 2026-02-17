/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';
import { SYSSHO } from './amode.js';

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function requireNativeExports(exports, names) {
  const missing = names.filter((name) => typeof exports[name] !== 'function');
  if (missing.length) {
    throw new Error(`amode.wasm missing native exports: ${missing.join(', ')}`);
  }
}

export async function loadAmodeWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'amode.wasm');
  const wasmBytes = await fs.readFile(wasmPath);

  let memoryForImports = null;
  const imports = {
    env: {
      // f2c list-directed WRITE stubs (messages are intentionally discarded).
      s_wsle: () => 0,
      e_wsle: () => 0,
      do_lio: () => 0,
      sin: (x) => Math.sin(x),
      cos: (x) => Math.cos(x),
      tan: (x) => Math.tan(x),
      d_sign: (aPtr, bPtr) => {
        if (!memoryForImports) return 0;
        const f64 = new Float64Array(memoryForImports.buffer);
        const a = f64[(aPtr | 0) >> 3] || 0;
        const b = f64[(bPtr | 0) >> 3] || 0;
        return b >= 0 ? Math.abs(a) : -Math.abs(a);
      },
      memset: (ptr, value, count) => {
        if (!memoryForImports) return ptr | 0;
        const u8 = new Uint8Array(memoryForImports.buffer);
        u8.fill(value & 0xff, ptr, ptr + count);
        return ptr | 0;
      },
      memmove: (dst, src, count) => {
        if (!memoryForImports) return dst | 0;
        const u8 = new Uint8Array(memoryForImports.buffer);
        u8.copyWithin(dst, src, src + count);
        return dst | 0;
      },
    },
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);
  memoryForImports = instance.exports.memory || null;
  const { exports } = instance;

  requireNativeExports(exports, [
    'AMODE_ptr_case_i_icon',
    'AMODE_ptr_case_i_nvtot',
    'AMODE_ptr_case_i_ncontrol',
    'AMODE_ptr_case_i_neigen',
    'AMODE_ptr_case_r_parval',
    'AMODE_ptr_case_r_vinf',
    'AMODE_ptr_case_r_wrot',
    'AMODE_ptr_case_r_cftot',
    'AMODE_ptr_case_r_cmtot',
    'AMODE_ptr_case_r_cftot_u',
    'AMODE_ptr_case_r_cmtot_u',
    'AMODE_ptr_case_r_cftot_d',
    'AMODE_ptr_case_r_cmtot_d',
    'AMODE_ptr_case_r_sref',
    'AMODE_ptr_case_r_cref',
    'AMODE_ptr_case_r_bref',
    'AMODE_ptr_case_r_dtr',
    'AMODE_ptr_un_r_unitl',
    'AMODE_ptr_mass_r_amass',
    'AMODE_ptr_mass_r_ainer',
    'AMODE_ptr_case_z_eval',
    'AMODE_ptr_case_z_evec',
    'AMODE_runchk',
    'AMODE_sysmat',
    'AMODE_appmat',
    'AMODE_eigsol',
  ]);
  if (!exports.memory) {
    throw new Error('amode.wasm missing memory export');
  }

  const DIMS = {
    iconIv: 35,
    iconRun: 25,
    parIp: 30,
    parRun: 25,
    jemax: 12,
    ndmax: 30,
    neigRun: 25,
  };

  const ptr = {
    icon: exports.AMODE_ptr_case_i_icon(),
    nvtot: exports.AMODE_ptr_case_i_nvtot(),
    ncontrol: exports.AMODE_ptr_case_i_ncontrol(),
    neigen: exports.AMODE_ptr_case_i_neigen(),
    parval: exports.AMODE_ptr_case_r_parval(),
    vinf: exports.AMODE_ptr_case_r_vinf(),
    wrot: exports.AMODE_ptr_case_r_wrot(),
    cftot: exports.AMODE_ptr_case_r_cftot(),
    cmtot: exports.AMODE_ptr_case_r_cmtot(),
    cftotU: exports.AMODE_ptr_case_r_cftot_u(),
    cmtotU: exports.AMODE_ptr_case_r_cmtot_u(),
    cftotD: exports.AMODE_ptr_case_r_cftot_d(),
    cmtotD: exports.AMODE_ptr_case_r_cmtot_d(),
    sref: exports.AMODE_ptr_case_r_sref(),
    cref: exports.AMODE_ptr_case_r_cref(),
    bref: exports.AMODE_ptr_case_r_bref(),
    dtr: exports.AMODE_ptr_case_r_dtr(),
    unitl: exports.AMODE_ptr_un_r_unitl(),
    amass: exports.AMODE_ptr_mass_r_amass(),
    ainer: exports.AMODE_ptr_mass_r_ainer(),
    eval: exports.AMODE_ptr_case_z_eval(),
    evec: exports.AMODE_ptr_case_z_evec(),
  };

  const heapBase = Number(exports.__heap_base?.value ?? exports.__heap_base ?? 65536);
  let arenaTop = (heapBase + 7) & ~7;
  let arena = null;
  let currentState = null;

  function ensureMemoryBytes(requiredBytes) {
    const memory = exports.memory;
    const pageSize = 64 * 1024;
    const have = memory.buffer.byteLength;
    if (have >= requiredBytes) return;
    const neededPages = Math.ceil((requiredBytes - have) / pageSize);
    if (neededPages > 0) memory.grow(neededPages);
  }

  function alloc(bytes, align = 8) {
    const aligned = (arenaTop + (align - 1)) & ~(align - 1);
    const end = aligned + bytes;
    ensureMemoryBytes(end);
    arenaTop = end;
    return aligned;
  }

  function getArena() {
    if (arena) return arena;
    const asysLen = DIMS.jemax * DIMS.jemax;
    const bsysLen = DIMS.jemax * DIMS.ndmax;
    const rsysLen = DIMS.jemax;
    arena = {
      asysLen,
      bsysLen,
      rsysLen,
      asysPtr: alloc(asysLen * Float64Array.BYTES_PER_ELEMENT, 8),
      bsysPtr: alloc(bsysLen * Float64Array.BYTES_PER_ELEMENT, 8),
      rsysPtr: alloc(rsysLen * Float64Array.BYTES_PER_ELEMENT, 8),
    };
    return arena;
  }

  function bind(state) {
    currentState = state;
    if (!currentState.__amode) {
      currentState.__amode = {
        ASYS: new Float32Array((state.JEMAX + 1) * (state.JEMAX + 1)),
        BSYS: new Float32Array((state.JEMAX + 1) * (state.NDMAX + 1)),
        RSYS: new Float32Array(state.JEMAX + 1),
      };
    }
  }

  function setF32Scalar(addr, value) {
    new Float32Array(exports.memory.buffer, addr, 1)[0] = Math.fround(value || 0);
  }

  function setI32Scalar(addr, value) {
    new Int32Array(exports.memory.buffer, addr, 1)[0] = value | 0;
  }

  function copyF32(addr, src, len) {
    const out = new Float32Array(exports.memory.buffer, addr, len);
    out.fill(0);
    if (!src) return;
    const n = Math.min(len, src.length);
    for (let i = 0; i < n; i += 1) out[i] = Math.fround(src[i] || 0);
  }

  function syncNativeState(state) {
    setI32Scalar(ptr.nvtot, state.NVTOT || 0);
    setI32Scalar(ptr.ncontrol, state.NCONTROL || 0);

    {
      const icon = new Int32Array(exports.memory.buffer, ptr.icon, DIMS.iconIv * DIMS.iconRun);
      icon.fill(0);
      const src = state.ICON;
      if (src && src.length) {
        const ivmax = state.IVMAX || DIMS.iconIv;
        const nrun = Math.min(DIMS.iconRun, Math.floor((src.length - 1) / Math.max(1, ivmax)));
        const nvtot = Math.min(DIMS.iconIv, state.NVTOT || 0);
        for (let run = 1; run <= nrun; run += 1) {
          for (let iv = 1; iv <= nvtot; iv += 1) {
            icon[(iv - 1) + DIMS.iconIv * (run - 1)] = src[idx2(iv, run, ivmax)] | 0;
          }
        }
      }
    }

    {
      const par = new Float32Array(exports.memory.buffer, ptr.parval, DIMS.parIp * DIMS.parRun);
      par.fill(0);
      const src = state.PARVAL;
      if (src && src.length) {
        const iptot = state.IPTOT || DIMS.parIp;
        const nrun = Math.min(DIMS.parRun, Math.floor((src.length - 1) / Math.max(1, iptot)));
        const nip = Math.min(DIMS.parIp, iptot);
        for (let run = 1; run <= nrun; run += 1) {
          for (let ip = 1; ip <= nip; ip += 1) {
            par[(ip - 1) + DIMS.parIp * (run - 1)] = Math.fround(src[idx2(ip, run, iptot)] || 0);
          }
        }
      }
    }

    copyF32(ptr.vinf, state.VINF, 3);
    copyF32(ptr.wrot, state.WROT, 3);
    copyF32(ptr.cftot, state.CFTOT, 3);
    copyF32(ptr.cmtot, state.CMTOT, 3);
    copyF32(ptr.cftotU, state.CFTOT_U, 18);
    copyF32(ptr.cmtotU, state.CMTOT_U, 18);
    copyF32(ptr.cftotD, state.CFTOT_D, 90);
    copyF32(ptr.cmtotD, state.CMTOT_D, 90);
    copyF32(ptr.amass, state.AMASS, 9);
    copyF32(ptr.ainer, state.AINER, 9);
    setF32Scalar(ptr.sref, state.SREF);
    setF32Scalar(ptr.cref, state.CREF);
    setF32Scalar(ptr.bref, state.BREF);
    setF32Scalar(ptr.dtr, state.DTR);
    setF32Scalar(ptr.unitl, state.UNITL);
  }

  function writeAsysInput(asysInput, state) {
    const { asysPtr, asysLen } = getArena();
    const out = new Float64Array(exports.memory.buffer, asysPtr, asysLen);
    out.fill(0);
    if (!asysInput) return;
    const dim = (state.JEMAX || DIMS.jemax) + 1;
    for (let j = 1; j <= DIMS.jemax; j += 1) {
      for (let i = 1; i <= DIMS.jemax; i += 1) {
        out[(i - 1) + DIMS.jemax * (j - 1)] = Number(asysInput[idx2(i, j, dim)] || 0);
      }
    }
  }

  function readSystemOutputs(state, nsys) {
    const dim = (state.JEMAX || DIMS.jemax) + 1;
    const { asysPtr, bsysPtr, rsysPtr, asysLen, bsysLen, rsysLen } = getArena();
    const srcA = new Float64Array(exports.memory.buffer, asysPtr, asysLen);
    const srcB = new Float64Array(exports.memory.buffer, bsysPtr, bsysLen);
    const srcR = new Float64Array(exports.memory.buffer, rsysPtr, rsysLen);
    const dstA = state.__amode.ASYS;
    const dstB = state.__amode.BSYS;
    const dstR = state.__amode.RSYS;
    dstA.fill(0);
    dstB.fill(0);
    dstR.fill(0);
    for (let j = 1; j <= DIMS.jemax; j += 1) {
      for (let i = 1; i <= DIMS.jemax; i += 1) {
        dstA[idx2(i, j, dim)] = Math.fround(srcA[(i - 1) + DIMS.jemax * (j - 1)] || 0);
      }
    }
    const nctrl = Math.min(state.NCONTROL || 0, DIMS.ndmax);
    for (let n = 1; n <= nctrl; n += 1) {
      for (let i = 1; i <= DIMS.jemax; i += 1) {
        dstB[idx2(i, n, dim)] = Math.fround(srcB[(i - 1) + DIMS.jemax * (n - 1)] || 0);
      }
    }
    for (let i = 1; i <= DIMS.jemax; i += 1) {
      dstR[i] = Math.fround(srcR[i - 1] || 0);
    }
    return { NSYS: nsys, LTERR: nsys <= 0 };
  }

  function RUNCHK_wasm(state, run) {
    bind(state);
    syncNativeState(state);
    return exports.AMODE_runchk(run | 0) !== 0;
  }

  function SYSMAT_wasm(state, ir) {
    bind(state);
    syncNativeState(state);
    const { asysPtr, bsysPtr, rsysPtr } = getArena();
    const nsys = exports.AMODE_sysmat(ir | 0, asysPtr, bsysPtr, rsysPtr) | 0;
    return readSystemOutputs(state, nsys);
  }

  function APPMAT_wasm(state, ir) {
    bind(state);
    syncNativeState(state);
    const { asysPtr, bsysPtr, rsysPtr } = getArena();
    const nsys = exports.AMODE_appmat(ir | 0, asysPtr, bsysPtr, rsysPtr) | 0;
    return readSystemOutputs(state, nsys);
  }

  function SYSSHO_wasm(state, nsys) {
    bind(state);
    const { ASYS, BSYS, RSYS } = currentState.__amode;
    return SYSSHO(currentState, ASYS, BSYS, RSYS, nsys);
  }

  function EIGSOL_wasm(state, ir, etol, asys, nsys) {
    bind(state);
    if ((nsys | 0) <= 0) {
      return { KEIG: 0, EVAL: [], EVEC: [], IERR: 0 };
    }
    syncNativeState(state);
    writeAsysInput(asys || currentState.__amode?.ASYS, state);
    const { asysPtr } = getArena();
    const ierr = exports.AMODE_eigsol(ir | 0, Math.fround(etol || 0), asysPtr, nsys | 0) | 0;

    const neigen = new Int32Array(exports.memory.buffer, ptr.neigen, DIMS.neigRun);
    const keig = Math.max(0, neigen[(ir | 0) - 1] | 0);
    const evalRaw = new Float32Array(exports.memory.buffer, ptr.eval, DIMS.jemax * DIMS.neigRun * 2);
    const evecRaw = new Float32Array(exports.memory.buffer, ptr.evec, DIMS.jemax * DIMS.jemax * DIMS.neigRun * 2);

    const evals = [];
    const evecs = [];
    for (let k = 1; k <= keig; k += 1) {
      const evalBase = ((k - 1) + DIMS.jemax * ((ir | 0) - 1)) * 2;
      evals.push({
        re: Number(evalRaw[evalBase] || 0),
        im: Number(evalRaw[evalBase + 1] || 0),
      });

      const vr = new Float32Array(nsys);
      const vi = new Float32Array(nsys);
      for (let i = 1; i <= (nsys | 0); i += 1) {
        const evecBase = ((i - 1) + DIMS.jemax * ((k - 1) + DIMS.jemax * ((ir | 0) - 1))) * 2;
        vr[i - 1] = Math.fround(evecRaw[evecBase] || 0);
        vi[i - 1] = Math.fround(evecRaw[evecBase + 1] || 0);
      }
      evecs.push({ re: vr, im: vi });
    }

    if (!state.__amodeEig) state.__amodeEig = {};
    state.__amodeEig[ir] = { evals, evecs, ierr };
    if (!state.NEIGEN) state.NEIGEN = [];
    state.NEIGEN[ir] = keig;

    return { KEIG: keig, EVAL: evals, EVEC: evecs, IERR: ierr };
  }

  return { RUNCHK_wasm, SYSMAT_wasm, APPMAT_wasm, SYSSHO_wasm, EIGSOL_wasm };
}
