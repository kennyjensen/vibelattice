import fs from 'node:fs/promises';
import path from 'node:path';

function makeAllocator(start = 0) {
  let offset = start;
  const alloc = (bytes, align = 4) => {
    const aligned = (offset + (align - 1)) & ~(align - 1);
    offset = aligned + bytes;
    return aligned;
  };
  const reset = () => {
    offset = start;
  };
  return { alloc, reset };
}

function writeF32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function readF32(view, offset, length) {
  return Float32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

export async function loadAsetupWasm(options = {}) {
  const wasmPath = options.wasmPath
    ?? path.resolve(path.dirname(new URL(import.meta.url).pathname), '..', 'dist', 'asetup.wasm');
  const wasmBytes = await fs.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});

  const { memory, GAMSUM, VELSUM } = instance.exports;
  const f32 = new Float32Array(memory.buffer);
  const allocator = makeAllocator(0);

  function allocF32(len) {
    return allocator.alloc(len * 4);
  }

  function GAMSUM_wasm(state) {
    allocator.reset();
    const vinfPtr = allocF32(state.VINF.length);
    const wrotPtr = allocF32(state.WROT.length);
    const delconPtr = allocF32(state.DELCON.length);
    const deldesPtr = allocF32(state.DELDES.length);
    const gamU0Ptr = allocF32(state.GAM_U_0.length);
    const gamUdPtr = allocF32(state.GAM_U_D.length);
    const gamUgPtr = allocF32(state.GAM_U_G.length);
    const gamUPtr = allocF32(state.GAM_U.length);
    const gamDPtr = allocF32(state.GAM_D.length);
    const gamGPtr = allocF32(state.GAM_G.length);
    const gamPtr = allocF32(state.GAM.length);
    const srcUPtr = allocF32(state.SRC_U.length);
    const dblUPtr = allocF32(state.DBL_U.length);
    const srcPtr = allocF32(state.SRC.length);
    const dblPtr = allocF32(state.DBL.length);

    writeF32(f32, vinfPtr, state.VINF);
    writeF32(f32, wrotPtr, state.WROT);
    writeF32(f32, delconPtr, state.DELCON);
    writeF32(f32, deldesPtr, state.DELDES);
    writeF32(f32, gamU0Ptr, state.GAM_U_0);
    writeF32(f32, gamUdPtr, state.GAM_U_D);
    writeF32(f32, gamUgPtr, state.GAM_U_G);
    writeF32(f32, gamUPtr, state.GAM_U);
    writeF32(f32, gamDPtr, state.GAM_D);
    writeF32(f32, gamGPtr, state.GAM_G);
    writeF32(f32, gamPtr, state.GAM);
    writeF32(f32, srcUPtr, state.SRC_U);
    writeF32(f32, dblUPtr, state.DBL_U);
    writeF32(f32, srcPtr, state.SRC);
    writeF32(f32, dblPtr, state.DBL);

    GAMSUM(
      state.NVOR, state.NCONTROL, state.NDESIGN, state.NLNODE, state.NUMAX,
      state.DIM_N, state.DIM_U, state.DIM_C, state.DIM_G, state.DIM_L,
      vinfPtr, wrotPtr, delconPtr, deldesPtr,
      gamU0Ptr, gamUdPtr, gamUgPtr,
      gamUPtr, gamDPtr, gamGPtr, gamPtr,
      srcUPtr, dblUPtr, srcPtr, dblPtr,
    );

    state.GAM_U = readF32(f32, gamUPtr, state.GAM_U.length);
    state.GAM_D = readF32(f32, gamDPtr, state.GAM_D.length);
    state.GAM_G = readF32(f32, gamGPtr, state.GAM_G.length);
    state.GAM = readF32(f32, gamPtr, state.GAM.length);
    state.SRC = readF32(f32, srcPtr, state.SRC.length);
    state.DBL = readF32(f32, dblPtr, state.DBL.length);
    return state;
  }

  function VELSUM_wasm(state) {
    allocator.reset();
    const vinfPtr = allocF32(state.VINF.length);
    const wrotPtr = allocF32(state.WROT.length);
    const wcGamPtr = allocF32(state.WC_GAM.length);
    const wvGamPtr = allocF32(state.WV_GAM.length);
    const gamPtr = allocF32(state.GAM.length);
    const gamUPtr = allocF32(state.GAM_U.length);
    const gamDPtr = allocF32(state.GAM_D.length);
    const gamGPtr = allocF32(state.GAM_G.length);

    const vcPtr = allocF32(state.VC.length);
    const vvPtr = allocF32(state.VV.length);
    const vcUPtr = allocF32(state.VC_U.length);
    const vvUPtr = allocF32(state.VV_U.length);
    const vcDPtr = allocF32(state.VC_D.length);
    const vvDPtr = allocF32(state.VV_D.length);
    const vcGPtr = allocF32(state.VC_G.length);
    const vvGPtr = allocF32(state.VV_G.length);

    const wcsrdUPtr = allocF32(state.WCSRD_U.length);
    const wvsrdUPtr = allocF32(state.WVSRD_U.length);
    const wcsrdPtr = allocF32(state.WCSRD.length);
    const wvsrdPtr = allocF32(state.WVSRD.length);

    const wcPtr = allocF32(state.WC.length);
    const wvPtr = allocF32(state.WV.length);
    const wcUPtr = allocF32(state.WC_U.length);
    const wvUPtr = allocF32(state.WV_U.length);
    const wcDPtr = allocF32(state.WC_D.length);
    const wvDPtr = allocF32(state.WV_D.length);
    const wcGPtr = allocF32(state.WC_G.length);
    const wvGPtr = allocF32(state.WV_G.length);

    writeF32(f32, vinfPtr, state.VINF);
    writeF32(f32, wrotPtr, state.WROT);
    writeF32(f32, wcGamPtr, state.WC_GAM);
    writeF32(f32, wvGamPtr, state.WV_GAM);
    writeF32(f32, gamPtr, state.GAM);
    writeF32(f32, gamUPtr, state.GAM_U);
    writeF32(f32, gamDPtr, state.GAM_D);
    writeF32(f32, gamGPtr, state.GAM_G);

    writeF32(f32, vcPtr, state.VC);
    writeF32(f32, vvPtr, state.VV);
    writeF32(f32, vcUPtr, state.VC_U);
    writeF32(f32, vvUPtr, state.VV_U);
    writeF32(f32, vcDPtr, state.VC_D);
    writeF32(f32, vvDPtr, state.VV_D);
    writeF32(f32, vcGPtr, state.VC_G);
    writeF32(f32, vvGPtr, state.VV_G);

    writeF32(f32, wcsrdUPtr, state.WCSRD_U);
    writeF32(f32, wvsrdUPtr, state.WVSRD_U);
    writeF32(f32, wcsrdPtr, state.WCSRD);
    writeF32(f32, wvsrdPtr, state.WVSRD);

    writeF32(f32, wcPtr, state.WC);
    writeF32(f32, wvPtr, state.WV);
    writeF32(f32, wcUPtr, state.WC_U);
    writeF32(f32, wvUPtr, state.WV_U);
    writeF32(f32, wcDPtr, state.WC_D);
    writeF32(f32, wvDPtr, state.WV_D);
    writeF32(f32, wcGPtr, state.WC_G);
    writeF32(f32, wvGPtr, state.WV_G);

    VELSUM(
      state.NVOR, state.NCONTROL, state.NDESIGN, state.NUMAX, state.NDMAX, state.NGMAX,
      state.DIM_N, state.DIM_U, state.DIM_C, state.DIM_G,
      vinfPtr, wrotPtr,
      wcGamPtr, wvGamPtr,
      gamPtr, gamUPtr, gamDPtr, gamGPtr,
      vcPtr, vvPtr, vcUPtr, vvUPtr,
      vcDPtr, vvDPtr, vcGPtr, vvGPtr,
      wcsrdUPtr, wvsrdUPtr,
      wcsrdPtr, wvsrdPtr,
      wcPtr, wvPtr, wcUPtr, wvUPtr,
      wcDPtr, wvDPtr, wcGPtr, wvGPtr,
    );

    state.VC = readF32(f32, vcPtr, state.VC.length);
    state.VV = readF32(f32, vvPtr, state.VV.length);
    state.WC = readF32(f32, wcPtr, state.WC.length);
    state.WV = readF32(f32, wvPtr, state.WV.length);
    state.WCSRD = readF32(f32, wcsrdPtr, state.WCSRD.length);
    state.WVSRD = readF32(f32, wvsrdPtr, state.WVSRD.length);
    state.VC_U = readF32(f32, vcUPtr, state.VC_U.length);
    state.VV_U = readF32(f32, vvUPtr, state.VV_U.length);
    state.WC_U = readF32(f32, wcUPtr, state.WC_U.length);
    state.WV_U = readF32(f32, wvUPtr, state.WV_U.length);
    state.VC_D = readF32(f32, vcDPtr, state.VC_D.length);
    state.VV_D = readF32(f32, vvDPtr, state.VV_D.length);
    state.VC_G = readF32(f32, vcGPtr, state.VC_G.length);
    state.VV_G = readF32(f32, vvGPtr, state.VV_G.length);
    state.WC_D = readF32(f32, wcDPtr, state.WC_D.length);
    state.WV_D = readF32(f32, wvDPtr, state.WV_D.length);
    state.WC_G = readF32(f32, wcGPtr, state.WC_G.length);
    state.WV_G = readF32(f32, wvGPtr, state.WV_G.length);

    return state;
  }

  return { GAMSUM_wasm, VELSUM_wasm };
}
