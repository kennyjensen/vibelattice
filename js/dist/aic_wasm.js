/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */

function writeF32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function writeI32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function readF32(view, offset, length) {
  return Float32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

function packVec3Stride4Into(src, count, out) {
  for (let i = 1; i <= count; i += 1) {
    const baseSrc = 4 * i;
    const baseOut = 3 * (i - 1);
    out[baseOut] = src[baseSrc + 1];
    out[baseOut + 1] = src[baseSrc + 2];
    out[baseOut + 2] = src[baseSrc + 3];
  }
  return out;
}

function packVec1Into(src, count, out) {
  for (let i = 1; i <= count; i += 1) out[i - 1] = src[i] ?? 0;
  return out;
}

function packF32Into(src, count, out) {
  for (let i = 1; i <= count; i += 1) out[i - 1] = src[i] ?? 0;
  return out;
}

function expandVec3ToStride4(packed, n1, n2, dim1) {
  const out = new Float32Array(4 * (dim1 + 1) * (n2 + 1));
  for (let j = 1; j <= n2; j += 1) {
    for (let i = 1; i <= n1; i += 1) {
      const packedBase = 3 * ((i - 1) + dim1 * (j - 1));
      const outBase = 4 * (i + (dim1 + 1) * j);
      out[outBase + 1] = packed[packedBase];
      out[outBase + 2] = packed[packedBase + 1];
      out[outBase + 3] = packed[packedBase + 2];
    }
  }
  return out;
}

function makeAllocator(start = 0, ensure) {
  let offset = start;
  const alloc = (bytes, align = 4) => {
    const aligned = (offset + (align - 1)) & ~(align - 1);
    const next = aligned + bytes;
    if (ensure) ensure(next);
    offset = next;
    return aligned;
  };
  const reset = () => {
    offset = start;
  };
  return { alloc, reset };
}

export async function loadAicWasm() {
  const url = new URL('./aic.wasm', import.meta.url);
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Failed to load wasm: ${res.status}`);
  const wasmBytes = await res.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, VORVELC, SRDVELC, SRDSET, VSRD, VVOR } = instance.exports;
  let f32 = new Float32Array(memory.buffer);
  let i32 = new Int32Array(memory.buffer);
  const refreshViews = () => {
    f32 = new Float32Array(memory.buffer);
    i32 = new Int32Array(memory.buffer);
  };
  const ensureMemory = (bytes) => {
    const minBytes = 32 * 1024 * 1024;
    const target = Math.max(bytes, minBytes);
    const needed = target - memory.buffer.byteLength;
    if (needed <= 0) return;
    const pages = Math.ceil(needed / 65536);
    memory.grow(pages);
    refreshViews();
  };
  const allocator = makeAllocator(4096, ensureMemory);
  const cacheByKey = new Map();

  function cacheKeyVSRD(NBODY, NLDIM, NU, NC, NCDIM) {
    return `vsrd:${NBODY}:${NLDIM}:${NU}:${NC}:${NCDIM}`;
  }

  function cacheKeyVVOR(NV, NC, NCDIM) {
    return `vvor:${NV}:${NC}:${NCDIM}`;
  }

  function getVSRDCache(NBODY, NLDIM, NU, NC, NCDIM) {
    const key = cacheKeyVSRD(NBODY, NLDIM, NU, NC, NCDIM);
    const cached = cacheByKey.get(key);
    if (cached) return cached;
    allocator.reset();
    const lfrstPtr = allocator.alloc(NBODY * 4, 4);
    const nlPtr = allocator.alloc(NBODY * 4, 4);
    const rlPtr = allocator.alloc(3 * NLDIM * 4, 4);
    const radlPtr = allocator.alloc(NLDIM * 4, 4);
    const srcUPtr = allocator.alloc(NLDIM * NU * 4, 4);
    const dblUPtr = allocator.alloc(3 * NLDIM * NU * 4, 4);
    const rcPtr = allocator.alloc(3 * NC * 4, 4);
    const wcLen = 3 * NCDIM * NU;
    const wcPtr = allocator.alloc(wcLen * 4, 4);
    const cache = {
      lfrstPtr,
      nlPtr,
      rlPtr,
      radlPtr,
      srcUPtr,
      dblUPtr,
      rcPtr,
      wcPtr,
      wcLen,
      lfrstPacked: new Int32Array(NBODY),
      nlPacked: new Int32Array(NBODY),
      rlPacked: new Float32Array(3 * NLDIM),
      radlPacked: new Float32Array(NLDIM),
      srcPacked: new Float32Array(NLDIM * NU),
      dblPacked: new Float32Array(3 * NLDIM * NU),
      rcPacked: new Float32Array(3 * NC),
    };
    cacheByKey.set(key, cache);
    return cache;
  }

  function getVVORCache(NV, NC, NCDIM) {
    const key = cacheKeyVVOR(NV, NC, NCDIM);
    const cached = cacheByKey.get(key);
    if (cached) return cached;
    allocator.reset();
    const rv1Ptr = allocator.alloc(3 * NV * 4, 4);
    const rv2Ptr = allocator.alloc(3 * NV * 4, 4);
    const ncompPtr = allocator.alloc(NV * 4, 4);
    const chordPtr = allocator.alloc(NV * 4, 4);
    const rcPtr = allocator.alloc(3 * NC * 4, 4);
    const ncompcPtr = allocator.alloc(NC * 4, 4);
    const wcLen = 3 * NCDIM * NV;
    const wcPtr = allocator.alloc(wcLen * 4, 4);
    const cache = {
      rv1Ptr,
      rv2Ptr,
      ncompPtr,
      chordPtr,
      rcPtr,
      ncompcPtr,
      wcPtr,
      wcLen,
      rv1Packed: new Float32Array(3 * NV),
      rv2Packed: new Float32Array(3 * NV),
      ncompPacked: new Int32Array(NV),
      chordPacked: new Float32Array(NV),
      rcPacked: new Float32Array(3 * NC),
      ncompcPacked: new Int32Array(NC),
    };
    cacheByKey.set(key, cache);
    return cache;
  }

  function VORVELC_wasm(X, Y, Z, LBOUND, X1, Y1, Z1, X2, Y2, Z2, BETA, RCORE) {
    allocator.reset();
    const outPtr = allocator.alloc(3 * 4, 4);
    VORVELC(Math.fround(X), Math.fround(Y), Math.fround(Z), LBOUND ? 1 : 0,
      Math.fround(X1), Math.fround(Y1), Math.fround(Z1),
      Math.fround(X2), Math.fround(Y2), Math.fround(Z2),
      Math.fround(BETA), Math.fround(RCORE),
      outPtr);
    return readF32(f32, outPtr, 3);
  }

  function SRDVELC_wasm(X, Y, Z, X1, Y1, Z1, X2, Y2, Z2, BETA, RCORE) {
    allocator.reset();
    const uvwsPtr = allocator.alloc(3 * 4, 4);
    const uvwdPtr = allocator.alloc(9 * 4, 4);
    SRDVELC(Math.fround(X), Math.fround(Y), Math.fround(Z),
      Math.fround(X1), Math.fround(Y1), Math.fround(Z1),
      Math.fround(X2), Math.fround(Y2), Math.fround(Z2),
      Math.fround(BETA), Math.fround(RCORE),
      uvwsPtr, uvwdPtr);
    return {
      UVWS: readF32(f32, uvwsPtr, 3),
      UVWD: readF32(f32, uvwdPtr, 9),
    };
  }

  function SRDSET_wasm(BETM, XYZREF, IYSYM, NBODY, LFRST, NLDIM, NL, RL, RADL) {
    allocator.reset();
    const xyzPtr = allocator.alloc(3 * 4, 4);
    const lfrstPtr = allocator.alloc(LFRST.length * 4, 4);
    const nlPtr = allocator.alloc(NL.length * 4, 4);
    const rlPtr = allocator.alloc(RL.length * 4, 4);
    const radlPtr = allocator.alloc(RADL.length * 4, 4);
    const srcUPtr = allocator.alloc(NLDIM * 6 * 4, 4);
    const dblUPtr = allocator.alloc(3 * NLDIM * 6 * 4, 4);

    writeF32(f32, xyzPtr, XYZREF);
    writeI32(i32, lfrstPtr, LFRST);
    writeI32(i32, nlPtr, NL);
    writeF32(f32, rlPtr, RL);
    writeF32(f32, radlPtr, RADL);

    SRDSET(Math.fround(BETM), xyzPtr, IYSYM, NBODY, lfrstPtr, NLDIM,
      nlPtr, rlPtr, radlPtr, srcUPtr, dblUPtr);

    return {
      SRC_U: readF32(f32, srcUPtr, NLDIM * 6),
      DBL_U: readF32(f32, dblUPtr, 3 * NLDIM * 6),
    };
  }

  function VSRD_wasm(BETM, IYSYM, YSYM, IZSYM, ZSYM, SRCORE,
    NBODY, LFRST, NLDIM, NL, RL, RADL,
    NU, SRC_U, DBL_U,
    NC, RC, NCDIM) {
    const cache = getVSRDCache(NBODY, NLDIM, NU, NC, NCDIM);
    packVec1Into(LFRST, NBODY, cache.lfrstPacked);
    packVec1Into(NL, NBODY, cache.nlPacked);
    packVec3Stride4Into(RL, NLDIM, cache.rlPacked);
    packF32Into(RADL, NLDIM, cache.radlPacked);
    if (SRC_U.length === NLDIM * NU) {
      cache.srcPacked.set(SRC_U);
    } else {
      packF32Into(SRC_U, NLDIM * NU, cache.srcPacked);
    }
    if (DBL_U.length === 3 * NLDIM * NU) {
      cache.dblPacked.set(DBL_U);
    } else {
      packF32Into(DBL_U, 3 * NLDIM * NU, cache.dblPacked);
    }
    packVec3Stride4Into(RC, NC, cache.rcPacked);

    writeI32(i32, cache.lfrstPtr, cache.lfrstPacked);
    writeI32(i32, cache.nlPtr, cache.nlPacked);
    writeF32(f32, cache.rlPtr, cache.rlPacked);
    writeF32(f32, cache.radlPtr, cache.radlPacked);
    writeF32(f32, cache.srcUPtr, cache.srcPacked);
    writeF32(f32, cache.dblUPtr, cache.dblPacked);
    writeF32(f32, cache.rcPtr, cache.rcPacked);

    VSRD(Math.fround(BETM), IYSYM, Math.fround(YSYM), IZSYM, Math.fround(ZSYM), Math.fround(SRCORE),
      NBODY, cache.lfrstPtr, NLDIM, cache.nlPtr, cache.rlPtr, cache.radlPtr,
      NU, cache.srcUPtr, cache.dblUPtr,
      NC, cache.rcPtr, cache.wcPtr, NCDIM);

    const packed = readF32(f32, cache.wcPtr, cache.wcLen);
    return expandVec3ToStride4(packed, NC, NU, NCDIM);
  }

  function VVOR_wasm(BETM, IYSYM, YSYM, IZSYM, ZSYM, VRCOREC, VRCOREW,
    NV, RV1, RV2, NCOMPV, CHORDV,
    NC, RC, NCOMPC, LVTEST, NCDIM) {
    const cache = getVVORCache(NV, NC, NCDIM);
    packVec3Stride4Into(RV1, NV, cache.rv1Packed);
    packVec3Stride4Into(RV2, NV, cache.rv2Packed);
    packVec1Into(NCOMPV, NV, cache.ncompPacked);
    packF32Into(CHORDV, NV, cache.chordPacked);
    packVec3Stride4Into(RC, NC, cache.rcPacked);
    packVec1Into(NCOMPC, NC, cache.ncompcPacked);

    writeF32(f32, cache.rv1Ptr, cache.rv1Packed);
    writeF32(f32, cache.rv2Ptr, cache.rv2Packed);
    writeI32(i32, cache.ncompPtr, cache.ncompPacked);
    writeF32(f32, cache.chordPtr, cache.chordPacked);
    writeF32(f32, cache.rcPtr, cache.rcPacked);
    writeI32(i32, cache.ncompcPtr, cache.ncompcPacked);

    VVOR(Math.fround(BETM), IYSYM, Math.fround(YSYM), IZSYM, Math.fround(ZSYM),
      Math.fround(VRCOREC), Math.fround(VRCOREW),
      NV, cache.rv1Ptr, cache.rv2Ptr, cache.ncompPtr, cache.chordPtr,
      NC, cache.rcPtr, cache.ncompcPtr, LVTEST ? 1 : 0,
      cache.wcPtr, NCDIM);

    const packed = readF32(f32, cache.wcPtr, cache.wcLen);
    return expandVec3ToStride4(packed, NC, NV, NCDIM);
  }

  return { VORVELC: VORVELC_wasm, SRDVELC: SRDVELC_wasm, SRDSET: SRDSET_wasm, VSRD: VSRD_wasm, VVOR: VVOR_wasm, memory };
}
