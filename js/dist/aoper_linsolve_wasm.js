/*
 * WASM-backed linear solve for EXEC trim system (f64).
 * Browser runtime loader.
 */

function makeAllocator(start = 0) {
  let offset = start;
  const alloc = (bytes, align = 8) => {
    const aligned = (offset + (align - 1)) & ~(align - 1);
    offset = aligned + bytes;
    return aligned;
  };
  const reset = () => {
    offset = start;
  };
  return { alloc, reset };
}

export async function loadAoperLinSolveWasm() {
  const url = new URL('./aoper_exec_port.wasm', import.meta.url);
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Failed to load wasm: ${res.status}`);
  const wasmBytes = await res.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const { memory, LUDCMP_COL_f64, BAKSUB_COL_f64 } = instance.exports;
  let f64 = new Float64Array(memory.buffer);
  const refreshViews = () => {
    f64 = new Float64Array(memory.buffer);
  };
  const ensureMemory = (bytes) => {
    const minBytes = 64 * 1024 * 1024;
    const target = Math.max(bytes, minBytes);
    const needed = target - memory.buffer.byteLength;
    if (needed <= 0) return;
    const pages = Math.ceil(needed / 65536);
    memory.grow(pages);
    refreshViews();
  };
  const allocator = makeAllocator(0);

  function solveLinearSystem(vsys, vres, dim, n) {
    allocator.reset();
    const aPtr = allocator.alloc(vsys.length * 8, 8);
    ensureMemory(aPtr + vsys.length * 8);
    const bPtr = allocator.alloc(vres.length * 8, 8);
    ensureMemory(bPtr + vres.length * 8);
    const indxPtr = allocator.alloc((n + 1) * 4, 4);
    const workPtr = allocator.alloc((n + 1) * 8, 8);

    f64.set(vsys, aPtr / 8);
    f64.set(vres, bPtr / 8);

    LUDCMP_COL_f64(dim, n, aPtr, indxPtr, workPtr);
    BAKSUB_COL_f64(dim, n, aPtr, indxPtr, bPtr);

    return Float64Array.from(f64.subarray(bPtr / 8, bPtr / 8 + vres.length));
  }

  return { solveLinearSystem };
}
