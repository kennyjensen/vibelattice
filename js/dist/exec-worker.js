import { EXEC } from './aoper.js';
import { TRMSET_CORE } from './atrim.js';
import { SYSMAT, EIGSOL } from '../src/amode.js';
import { APPGET } from '../src/amass.js';

let execWasm = null;
let execWasmReady = null;
let execWasmState = null;
let amodeWasm = null;
let amodeWasmReady = null;
let amodeWasmMemory = null;
let amodeNativeCtx = null;

async function loadExecWasm() {
  if (execWasm) return execWasm;
  if (execWasmReady) return execWasmReady;
  execWasmReady = (async () => {
    const url = new URL('./aoper.wasm', import.meta.url);
    const bytes = await (await fetch(url)).arrayBuffer();
    const imports = {
      env: {
        exec_js: (niter, info, ir) => {
          if (!execWasmState) {
            throw new Error('EXEC wasm called without bound state');
          }
          EXEC(execWasmState, niter, info, ir);
        },
      },
    };
    const { instance } = await WebAssembly.instantiate(bytes, imports);
    execWasm = instance.exports.EXEC;
    return execWasm;
  })();
  return execWasmReady;
}

function idx2(i, j, dim1) {
  return i + dim1 * j;
}

function ensureAmodeState(state) {
  state.JEMAX = state.JEMAX || 12;
  state.JEU = state.JEU || 1;
  state.JEW = state.JEW || 2;
  state.JEQ = state.JEQ || 3;
  state.JETH = state.JETH || 4;
  state.JEV = state.JEV || 5;
  state.JEP = state.JEP || 6;
  state.JER = state.JER || 7;
  state.JEPH = state.JEPH || 8;
  state.JEX = state.JEX || 9;
  state.JEY = state.JEY || 10;
  state.JEZ = state.JEZ || 11;
  state.JEPS = state.JEPS || 12;
  if (!state.__amode) {
    const jemax = state.JEMAX + 1;
    const ndmax = (state.NDMAX || Math.max(1, state.NCONTROL || 1)) + 1;
    state.__amode = {
      ASYS: new Float32Array(jemax * jemax),
      BSYS: new Float32Array(jemax * ndmax),
      RSYS: new Float32Array(jemax),
    };
  }
}

async function loadAmodeWasm() {
  if (amodeWasm) return amodeWasm;
  if (amodeWasmReady) return amodeWasmReady;
  amodeWasmReady = (async () => {
    const url = new URL('./amode.wasm', import.meta.url);
    const bytes = await (await fetch(url)).arrayBuffer();
    const imports = {
      env: {
        s_wsle: () => 0,
        e_wsle: () => 0,
        do_lio: () => 0,
        sin: (x) => Math.sin(x),
        cos: (x) => Math.cos(x),
        tan: (x) => Math.tan(x),
        d_sign: (aPtr, bPtr) => {
          if (!amodeWasmMemory) return 0;
          const f64 = new Float64Array(amodeWasmMemory.buffer);
          const a = f64[(aPtr | 0) >> 3] || 0;
          const b = f64[(bPtr | 0) >> 3] || 0;
          return b >= 0 ? Math.abs(a) : -Math.abs(a);
        },
        memset: (ptr, value, count) => {
          if (!amodeWasmMemory) return ptr | 0;
          const u8 = new Uint8Array(amodeWasmMemory.buffer);
          u8.fill(value & 0xff, ptr, ptr + count);
          return ptr | 0;
        },
        memmove: (dst, src, count) => {
          if (!amodeWasmMemory) return dst | 0;
          const u8 = new Uint8Array(amodeWasmMemory.buffer);
          u8.copyWithin(dst, src, src + count);
          return dst | 0;
        },
      },
    };
    const { instance } = await WebAssembly.instantiate(bytes, imports);
    amodeWasm = instance.exports;
    amodeWasmMemory = instance.exports.memory || null;
    return amodeWasm;
  })();
  return amodeWasmReady;
}

function getAmodeNativeCtx(wasm) {
  if (
    !wasm
    || typeof wasm.AMODE_runchk !== 'function'
    || typeof wasm.AMODE_sysmat !== 'function'
    || typeof wasm.AMODE_eigsol !== 'function'
    || !wasm.memory
  ) {
    return null;
  }
  if (amodeNativeCtx && amodeNativeCtx.wasm === wasm) return amodeNativeCtx;
  if (
    typeof wasm.AMODE_ptr_case_i_icon !== 'function'
    || typeof wasm.AMODE_ptr_case_i_nvtot !== 'function'
    || typeof wasm.AMODE_ptr_case_i_ncontrol !== 'function'
    || typeof wasm.AMODE_ptr_case_i_neigen !== 'function'
    || typeof wasm.AMODE_ptr_case_r_parval !== 'function'
    || typeof wasm.AMODE_ptr_case_r_vinf !== 'function'
    || typeof wasm.AMODE_ptr_case_r_wrot !== 'function'
    || typeof wasm.AMODE_ptr_case_r_cftot !== 'function'
    || typeof wasm.AMODE_ptr_case_r_cmtot !== 'function'
    || typeof wasm.AMODE_ptr_case_r_cftot_u !== 'function'
    || typeof wasm.AMODE_ptr_case_r_cmtot_u !== 'function'
    || typeof wasm.AMODE_ptr_case_r_cftot_d !== 'function'
    || typeof wasm.AMODE_ptr_case_r_cmtot_d !== 'function'
    || typeof wasm.AMODE_ptr_case_r_sref !== 'function'
    || typeof wasm.AMODE_ptr_case_r_cref !== 'function'
    || typeof wasm.AMODE_ptr_case_r_bref !== 'function'
    || typeof wasm.AMODE_ptr_case_r_dtr !== 'function'
    || typeof wasm.AMODE_ptr_un_r_unitl !== 'function'
    || typeof wasm.AMODE_ptr_mass_r_amass !== 'function'
    || typeof wasm.AMODE_ptr_mass_r_ainer !== 'function'
    || typeof wasm.AMODE_ptr_case_z_eval !== 'function'
    || typeof wasm.AMODE_ptr_case_z_evec !== 'function'
  ) {
    return null;
  }
  const dims = {
    iconIv: 35,
    iconRun: 25,
    parIp: 30,
    parRun: 25,
    jemax: 12,
    ndmax: 30,
    neigRun: 25,
  };
  const ptr = {
    icon: wasm.AMODE_ptr_case_i_icon(),
    nvtot: wasm.AMODE_ptr_case_i_nvtot(),
    ncontrol: wasm.AMODE_ptr_case_i_ncontrol(),
    neigen: wasm.AMODE_ptr_case_i_neigen(),
    parval: wasm.AMODE_ptr_case_r_parval(),
    vinf: wasm.AMODE_ptr_case_r_vinf(),
    wrot: wasm.AMODE_ptr_case_r_wrot(),
    cftot: wasm.AMODE_ptr_case_r_cftot(),
    cmtot: wasm.AMODE_ptr_case_r_cmtot(),
    cftotU: wasm.AMODE_ptr_case_r_cftot_u(),
    cmtotU: wasm.AMODE_ptr_case_r_cmtot_u(),
    cftotD: wasm.AMODE_ptr_case_r_cftot_d(),
    cmtotD: wasm.AMODE_ptr_case_r_cmtot_d(),
    sref: wasm.AMODE_ptr_case_r_sref(),
    cref: wasm.AMODE_ptr_case_r_cref(),
    bref: wasm.AMODE_ptr_case_r_bref(),
    dtr: wasm.AMODE_ptr_case_r_dtr(),
    unitl: wasm.AMODE_ptr_un_r_unitl(),
    amass: wasm.AMODE_ptr_mass_r_amass(),
    ainer: wasm.AMODE_ptr_mass_r_ainer(),
    eval: wasm.AMODE_ptr_case_z_eval(),
    evec: wasm.AMODE_ptr_case_z_evec(),
  };
  const heapBase = Number(wasm.__heap_base?.value ?? wasm.__heap_base ?? 65536);
  let arenaTop = (heapBase + 7) & ~7;
  function ensureMemoryBytes(requiredBytes) {
    const pageSize = 64 * 1024;
    const have = wasm.memory.buffer.byteLength;
    if (have >= requiredBytes) return;
    const pages = Math.ceil((requiredBytes - have) / pageSize);
    if (pages > 0) wasm.memory.grow(pages);
  }
  function alloc(bytes, align = 8) {
    const aligned = (arenaTop + (align - 1)) & ~(align - 1);
    const end = aligned + bytes;
    ensureMemoryBytes(end);
    arenaTop = end;
    return aligned;
  }
  const asysLen = dims.jemax * dims.jemax;
  const bsysLen = dims.jemax * dims.ndmax;
  const rsysLen = dims.jemax;
  amodeNativeCtx = {
    wasm,
    dims,
    ptr,
    asysLen,
    bsysLen,
    rsysLen,
    asysPtr: alloc(asysLen * Float64Array.BYTES_PER_ELEMENT, 8),
    bsysPtr: alloc(bsysLen * Float64Array.BYTES_PER_ELEMENT, 8),
    rsysPtr: alloc(rsysLen * Float64Array.BYTES_PER_ELEMENT, 8),
  };
  return amodeNativeCtx;
}

function syncAmodeNativeState(ctx, state) {
  const { wasm, dims, ptr } = ctx;
  const mem = wasm.memory.buffer;
  new Int32Array(mem, ptr.nvtot, 1)[0] = state.NVTOT | 0;
  new Int32Array(mem, ptr.ncontrol, 1)[0] = state.NCONTROL | 0;

  {
    const out = new Int32Array(mem, ptr.icon, dims.iconIv * dims.iconRun);
    out.fill(0);
    const src = state.ICON;
    if (src && src.length) {
      const ivmax = state.IVMAX || dims.iconIv;
      const nrun = Math.min(dims.iconRun, Math.floor((src.length - 1) / Math.max(1, ivmax)));
      const nvtot = Math.min(dims.iconIv, state.NVTOT || 0);
      for (let run = 1; run <= nrun; run += 1) {
        for (let iv = 1; iv <= nvtot; iv += 1) {
          out[(iv - 1) + dims.iconIv * (run - 1)] = src[idx2(iv, run, ivmax)] | 0;
        }
      }
    }
  }

  {
    const out = new Float32Array(mem, ptr.parval, dims.parIp * dims.parRun);
    out.fill(0);
    const src = state.PARVAL;
    if (src && src.length) {
      const iptot = state.IPTOT || dims.parIp;
      const nrun = Math.min(dims.parRun, Math.floor((src.length - 1) / Math.max(1, iptot)));
      const nip = Math.min(dims.parIp, iptot);
      for (let run = 1; run <= nrun; run += 1) {
        for (let ip = 1; ip <= nip; ip += 1) {
          out[(ip - 1) + dims.parIp * (run - 1)] = Math.fround(src[idx2(ip, run, iptot)] || 0);
        }
      }
    }
  }

  const copyF32 = (dstPtr, src, len) => {
    const out = new Float32Array(mem, dstPtr, len);
    out.fill(0);
    if (!src) return;
    const n = Math.min(len, src.length);
    for (let i = 0; i < n; i += 1) out[i] = Math.fround(src[i] || 0);
  };
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
  new Float32Array(mem, ptr.sref, 1)[0] = Math.fround(state.SREF || 0);
  new Float32Array(mem, ptr.cref, 1)[0] = Math.fround(state.CREF || 0);
  new Float32Array(mem, ptr.bref, 1)[0] = Math.fround(state.BREF || 0);
  new Float32Array(mem, ptr.dtr, 1)[0] = Math.fround(state.DTR || 0);
  new Float32Array(mem, ptr.unitl, 1)[0] = Math.fround(state.UNITL || 0);
}

function runAmodeNativeEigen(wasm, state, ir, etol) {
  const ctx = getAmodeNativeCtx(wasm);
  if (!ctx) return null;
  syncAmodeNativeState(ctx, state);
  const ok = wasm.AMODE_runchk(ir | 0) !== 0;
  if (!ok) {
    return {
      sysRes: { NSYS: 0, LTERR: true },
      eigRes: { KEIG: 0, EVAL: [], EVEC: [], IERR: 0 },
    };
  }
  const nsys = wasm.AMODE_sysmat(ir | 0, ctx.asysPtr, ctx.bsysPtr, ctx.rsysPtr) | 0;
  const sysRes = { NSYS: nsys, LTERR: nsys <= 0 };
  if (nsys <= 0) {
    return { sysRes, eigRes: { KEIG: 0, EVAL: [], EVEC: [], IERR: 0 } };
  }
  const ierr = wasm.AMODE_eigsol(ir | 0, Math.fround(etol || 0), ctx.asysPtr, nsys) | 0;
  const mem = wasm.memory.buffer;
  const neigen = new Int32Array(mem, ctx.ptr.neigen, ctx.dims.neigRun);
  const keig = Math.max(0, neigen[(ir | 0) - 1] | 0);
  const evalRaw = new Float32Array(mem, ctx.ptr.eval, ctx.dims.jemax * ctx.dims.neigRun * 2);
  const evecRaw = new Float32Array(mem, ctx.ptr.evec, ctx.dims.jemax * ctx.dims.jemax * ctx.dims.neigRun * 2);
  const EVAL = [];
  const EVEC = [];
  for (let k = 1; k <= keig; k += 1) {
    const evalBase = ((k - 1) + ctx.dims.jemax * ((ir | 0) - 1)) * 2;
    EVAL.push({ re: Number(evalRaw[evalBase] || 0), im: Number(evalRaw[evalBase + 1] || 0) });
    const vr = new Float32Array(nsys);
    const vi = new Float32Array(nsys);
    for (let i = 1; i <= nsys; i += 1) {
      const evecBase = ((i - 1) + ctx.dims.jemax * ((k - 1) + ctx.dims.jemax * ((ir | 0) - 1))) * 2;
      vr[i - 1] = Math.fround(evecRaw[evecBase] || 0);
      vi[i - 1] = Math.fround(evecRaw[evecBase + 1] || 0);
    }
    EVEC.push({ re: vr, im: vi });
  }
  return { sysRes, eigRes: { KEIG: keig, EVAL, EVEC, IERR: ierr } };
}

function normalizeEigenVec(vr, vi, nsys) {
  let mag = 0;
  for (let i = 0; i < nsys; i += 1) {
    mag = Math.max(mag, Math.hypot(vr[i] || 0, vi[i] || 0));
  }
  const s = mag > 1e-8 ? 1 / mag : 1;
  return {
    re: Array.from({ length: nsys }, (_, i) => (vr[i] || 0) * s),
    im: Array.from({ length: nsys }, (_, i) => (vi[i] || 0) * s),
  };
}

async function computeEigenmodes(state, useWasm) {
  ensureAmodeState(state);
  const IR = 1;
  const ETOL = 1e-5;
  if (state.LMASS) {
    try {
      APPGET(state);
    } catch (err) {
      log(`APPGET failed: ${err?.message ?? err}`);
    }
  }
  let sysRes = null;
  let eigRes = null;
  let usedNative = false;
  if (useWasm) {
    try {
      const wasm = await loadAmodeWasm();
      const nativeEigen = runAmodeNativeEigen(wasm, state, IR, ETOL);
      if (nativeEigen) {
        const nativeNsys = nativeEigen?.sysRes?.NSYS || 0;
        if (!(nativeEigen?.sysRes?.LTERR) && nativeNsys > 0) {
          sysRes = nativeEigen.sysRes;
          eigRes = nativeEigen.eigRes;
          usedNative = true;
        } else {
          log(`RUNCHK/SYSMAT failed for run ${IR}; falling back to JS eigenmode path`);
        }
      } else {
        log('Native AMODE exports missing; falling back to JS eigenmode path');
      }
    } catch (err) {
      log(`AMODE wasm load failed: ${err?.message ?? err}; falling back to JS eigenmode path`);
    }
  } else {
    // no-op, use JS path below
  }

  if (!usedNative) {
    const { ASYS, BSYS, RSYS } = state.__amode;
    sysRes = SYSMAT(state, IR, ASYS, BSYS, RSYS);
    eigRes = EIGSOL(state, IR, ETOL, ASYS, sysRes?.NSYS || 0);
  }
  const nsys = sysRes?.NSYS || 0;
  const evals = eigRes?.EVAL || [];
  const evecs = eigRes?.EVEC || [];
  const names = ['u', 'w', 'q', 'theta', 'v', 'p', 'r', 'phi', 'x', 'y', 'z', 'psi'];
  const modes = evals.map((ev, idx) => {
    const vec = evecs[idx] ? normalizeEigenVec(evecs[idx].re, evecs[idx].im, nsys) : { re: [], im: [] };
    const v = vec.re;
    const roll = (v[5] || 0) + 0.8 * (v[7] || 0);
    const pitch = (v[2] || 0) + 0.8 * (v[3] || 0);
    const yaw = (v[6] || 0) + 0.8 * (v[11] || 0);
    const tx = (v[0] || 0) + 0.6 * (v[8] || 0);
    const ty = (v[4] || 0) + 0.6 * (v[9] || 0);
    const tz = (v[1] || 0) + 0.6 * (v[10] || 0);
    return {
      name: `Mode ${idx + 1}`,
      re: ev.re,
      im: ev.im,
      stateOrder: names.slice(0, nsys),
      eigenvector: vec,
      vec: {
        rx: roll,
        ry: pitch,
        rz: yaw,
        tx,
        ty,
        tz,
      },
    };
  });
  return { nsys, modes };
}

function log(message) {
  postMessage({ type: 'log', message });
}

onmessage = async (evt) => {
  const { state, type, requestId, useWasm } = evt.data || {};
  if (!state) {
    postMessage({ type: 'error', message: 'Missing state.' });
    return;
  }
  // Keep body-derivative sign convention aligned with Fortran AVL defaults.
  state.LNASA_SA = true;
  if (type === 'trim') {
    try {
      const IR = 1;
      TRMSET_CORE(state, 1, IR, IR, IR);
      postMessage({ type: 'trimResult', state, requestId });
    } catch (err) {
      postMessage({ type: 'error', message: err?.message ?? String(err) });
    }
    return;
  }
  try {
    const t0 = Date.now();
    log('Worker EXEC start');
    try {
      const IR = 1;
      const idx2 = (i, j, dim1) => i + dim1 * j;
      const ipcl = state.IPCL ?? 6;
      const ipvee = state.IPVEE ?? 12;
      const ipphi = state.IPPHI ?? 8;
      const iprho = state.IPRHO ?? 13;
      const ipgee = state.IPGEE ?? 14;
      const ipmass = state.IPMASS ?? 20;
      const par = state.PARVAL || [];
      log(`EXEC inputs: CL=${par[idx2(ipcl, IR, state.IPTOT || 30)]} V=${par[idx2(ipvee, IR, state.IPTOT || 30)]} PHI=${par[idx2(ipphi, IR, state.IPTOT || 30)]} RHO=${par[idx2(iprho, IR, state.IPTOT || 30)]} GEE=${par[idx2(ipgee, IR, state.IPTOT || 30)]} MASS=${par[idx2(ipmass, IR, state.IPTOT || 30)]}`);
      log(`EXEC refs: SREF=${state.SREF} CREF=${state.CREF} BREF=${state.BREF}`);
      const ivmax = state.IVMAX || 0;
      const icmax = state.ICMAX || 0;
      if (state.ICON && state.CONVAL && ivmax && icmax) {
        const iValfa = state.IVALFA || 1;
        const iVbeta = state.IVBETA || 2;
        const iVrotx = state.IVROTX || 3;
        const iVroty = state.IVROTY || 4;
        const iVrotz = state.IVROTZ || 5;
        const icMap = [
          ['IVALFA', iValfa],
          ['IVBETA', iVbeta],
          ['IVROTX', iVrotx],
          ['IVROTY', iVroty],
          ['IVROTZ', iVrotz],
        ];
        const iconVals = icMap.map(([label, iv]) => {
          const ic = state.ICON[idx2(iv, IR, ivmax)];
          const cv = state.CONVAL[idx2(ic, IR, icmax)];
          return `${label}=>IC${ic} CON=${cv}`;
        });
        log(`EXEC constraints: ${iconVals.join(' | ')}`);
      }
    } catch (err) {
      log(`EXEC precheck failed: ${err?.message ?? err}`);
    }
    // Honor the UI toggle for all wasm-enabled EXEC kernels.
    state.USE_WASM_SOLVE = Boolean(useWasm);
    state.USE_WASM_GAM = Boolean(useWasm);
    state.USE_WASM_AERO = false;
    state.USE_WASM_AIC = false;
    state.USE_WASM_LU = false;
    // Use the JS EXEC entrypoint and let it dispatch to wasm kernels via the
    // USE_WASM_* flags above. The legacy aoper.wasm wrapper has diverged from
    // current state layout and can emit non-finite totals in default runs.
    EXEC(state, 20, 0, 1);
    const dt = Date.now() - t0;
    log(`Worker EXEC done (${dt} ms)`);
    log(`EXEC state: ALFA=${state.ALFA} BETA=${state.BETA} MACH=${state.MACH}`);
    if (state.VINF && state.WROT) {
      log(`EXEC VINF=[${state.VINF[0]}, ${state.VINF[1]}, ${state.VINF[2]}] WROT=[${state.WROT[0]}, ${state.WROT[1]}, ${state.WROT[2]}]`);
    }
    const trefftz = {
      axis: 'Y',
      cref: state.CREF,
      strips: [],
      surfaces: [],
    };
    let badCnc = 0;
    let badCl = 0;
    let badClt = 0;
    let badDw = 0;
    const toFinite = (v, kind) => {
      if (Number.isFinite(v)) return v;
      if (kind === 'cnc') badCnc += 1;
      if (kind === 'cl') badCl += 1;
      if (kind === 'clt') badClt += 1;
      if (kind === 'dw') badDw += 1;
      return 0.0;
    };
    if (state.DWWAKE && state.RLE && state.CNC && state.CLA_LSTRP && state.CLT_LSTRP && state.JFRST && state.NJ) {
      for (let n = 1; n <= state.NSURF; n += 1) {
        const j1 = state.JFRST[n];
        const nj = state.NJ[n];
        if (!j1 || !nj) continue;
        const start = trefftz.strips.length;
        for (let jj = 0; jj < nj; jj += 1) {
          const j = j1 + jj;
          const y = state.RLE[idx2(2, j, 4)];
          const z = state.RLE[idx2(3, j, 4)];
          const cnc = toFinite(state.CNC[j], 'cnc');
          const cl = toFinite(state.CLA_LSTRP[j], 'cl');
          const clPerp = toFinite(state.CLT_LSTRP[j], 'clt');
          const dw = toFinite(state.DWWAKE[j], 'dw');
          trefftz.strips.push([y, z, cnc, cl, clPerp, dw, n]);
        }
        trefftz.surfaces.push({ id: n, start, count: nj });
      }
    }
    if (badCnc || badCl || badClt || badDw) {
      log(`Trefftz non-finite: cnc=${badCnc} cl=${badCl} clt=${badClt} dw=${badDw}`);
    }
    const surfVec = (arr, is) => (arr ? [
      arr[idx2(0, is, 3)],
      arr[idx2(1, is, 3)],
      arr[idx2(2, is, 3)],
    ] : null);
    const rowU = (arr, row) => {
      if (!arr) return null;
      const out = [];
      for (let n = 0; n < 6; n += 1) out.push(arr[idx2(row, n, 3)]);
      return out;
    };
    const rowD = (arr, row) => {
      if (!arr) return null;
      const out = [];
      for (let n = 0; n <= state.NDMAX; n += 1) out.push(arr[idx2(row, n, 3)]);
      return out;
    };
    const hinge = state.CHINGE ? (() => {
      const out = new Array(state.NCONTROL + 1).fill(0);
      for (let i = 1; i <= state.NCONTROL; i += 1) out[i] = state.CHINGE[i - 1] ?? 0.0;
      return out;
    })() : null;
    const eigen = await computeEigenmodes(state, Boolean(useWasm));
    postMessage({
      type: 'result',
      requestId,
      SREF: state.SREF,
      CREF: state.CREF,
      BREF: state.BREF,
      ALFA: state.ALFA,
      BETA: state.BETA,
      CLTOT: state.CLTOT,
      CDTOT: state.CDTOT,
      CYTOT: state.CYTOT,
      CDVTOT: state.CDVTOT,
      CFTOT: state.CFTOT ? Array.from(state.CFTOT) : null,
      CMTOT: state.CMTOT ? Array.from(state.CMTOT) : null,
      CFTOT_U: state.CFTOT_U ? [rowU(state.CFTOT_U, 0), rowU(state.CFTOT_U, 1), rowU(state.CFTOT_U, 2)] : null,
      CMTOT_U: state.CMTOT_U ? [rowU(state.CMTOT_U, 0), rowU(state.CMTOT_U, 1), rowU(state.CMTOT_U, 2)] : null,
      CFTOT_D: state.CFTOT_D ? [rowD(state.CFTOT_D, 0), rowD(state.CFTOT_D, 1), rowD(state.CFTOT_D, 2)] : null,
      CLTOT_U: state.CLTOT_U ? Array.from(state.CLTOT_U.slice(0, 6)) : null,
      CDTOT_U: state.CDTOT_U ? Array.from(state.CDTOT_U.slice(0, 6)) : null,
      CYTOT_U: state.CYTOT_U ? Array.from(state.CYTOT_U.slice(0, 6)) : null,
      CLTOT_D: state.CLTOT_D ? Array.from(state.CLTOT_D) : null,
      CDTOT_D: state.CDTOT_D ? Array.from(state.CDTOT_D) : null,
      CYTOT_D: state.CYTOT_D ? Array.from(state.CYTOT_D) : null,
      CMTOT_D: state.CMTOT_D ? [rowD(state.CMTOT_D, 0), rowD(state.CMTOT_D, 1), rowD(state.CMTOT_D, 2)] : null,
      CLTOT_A: state.CLTOT_A ?? 0.0,
      CDTOT_A: state.CDTOT_A ?? 0.0,
      VINF_A: state.VINF_A ? Array.from(state.VINF_A) : null,
      VINF_B: state.VINF_B ? Array.from(state.VINF_B) : null,
      CDSURF: state.CDSURF ? Array.from(state.CDSURF) : null,
      CYSURF: state.CYSURF ? Array.from(state.CYSURF) : null,
      CLSURF: state.CLSURF ? Array.from(state.CLSURF) : null,
      CDVSURF: state.CDVSURF ? Array.from(state.CDVSURF) : null,
      CFSURF: state.CFSURF ? Array.from({ length: state.NSURF + 1 }, (_, is) => (is === 0 ? null : surfVec(state.CFSURF, is))) : null,
      CMSURF: state.CMSURF ? Array.from({ length: state.NSURF + 1 }, (_, is) => (is === 0 ? null : surfVec(state.CMSURF, is))) : null,
      CDSTRP: state.CDSTRP ? Array.from(state.CDSTRP) : null,
      CYSTRP: state.CYSTRP ? Array.from(state.CYSTRP) : null,
      CLSTRP: state.CLSTRP ? Array.from(state.CLSTRP) : null,
      CNC: state.CNC ? Array.from(state.CNC) : null,
      CLA_LSTRP: state.CLA_LSTRP ? Array.from(state.CLA_LSTRP) : null,
      CLT_LSTRP: state.CLT_LSTRP ? Array.from(state.CLT_LSTRP) : null,
      DWWAKE: state.DWWAKE ? Array.from(state.DWWAKE) : null,
      RLE: state.RLE ? Array.from(state.RLE) : null,
      DCP: state.DCP ? Array.from(state.DCP) : null,
      RV: state.RV ? Array.from(state.RV) : null,
      ENSY: state.ENSY ? Array.from(state.ENSY) : null,
      ENSZ: state.ENSZ ? Array.from(state.ENSZ) : null,
      IJFRST: state.IJFRST ? Array.from(state.IJFRST) : null,
      NVSTRP: state.NVSTRP ? Array.from(state.NVSTRP) : null,
      JFRST: state.JFRST ? Array.from(state.JFRST) : null,
      NJ: state.NJ ? Array.from(state.NJ) : null,
      LFLOAD: state.LFLOAD ? Array.from(state.LFLOAD) : null,
      CDBDY: state.CDBDY ? Array.from(state.CDBDY) : null,
      CYBDY: state.CYBDY ? Array.from(state.CYBDY) : null,
      CLBDY: state.CLBDY ? Array.from(state.CLBDY) : null,
      CFBDY: state.CFBDY ? Array.from({ length: state.NBODY }, (_, ib) => surfVec(state.CFBDY, ib)) : null,
      CMBDY: state.CMBDY ? Array.from({ length: state.NBODY }, (_, ib) => surfVec(state.CMBDY, ib)) : null,
      CHINGE: hinge,
      JFRST: state.JFRST ? Array.from(state.JFRST) : null,
      NJ: state.NJ ? Array.from(state.NJ) : null,
      BETM: state.BETM,
      IYSYM: state.IYSYM,
      IZSYM: state.IZSYM,
      YSYM: state.YSYM,
      ZSYM: state.ZSYM,
      NVOR: state.NVOR,
      GAM: state.GAM ? Array.from(state.GAM.slice(0, state.NVOR + 1)) : null,
      RV1: state.RV1 ? Array.from(state.RV1.slice(0, 4 * (state.NVOR + 1))) : null,
      RV2: state.RV2 ? Array.from(state.RV2.slice(0, 4 * (state.NVOR + 1))) : null,
      PARVAL: state.PARVAL,
      WROT: state.WROT,
      DELCON: state.DELCON,
      SPANEF: state.SPANEF,
      LNASA_SA: state.LNASA_SA,
      USE_WASM_SOLVE: Boolean(state.USE_WASM_SOLVE),
      USE_WASM_GAM: Boolean(state.USE_WASM_GAM),
      USE_WASM_AERO: Boolean(state.USE_WASM_AERO),
      USE_WASM_AIC: Boolean(state.USE_WASM_AIC),
      USE_WASM_LU: Boolean(state.USE_WASM_LU),
      TREFFTZ: trefftz,
      EIGEN: eigen,
    });
  } catch (err) {
    postMessage({ type: 'error', message: err?.message ?? String(err) });
  }
};
