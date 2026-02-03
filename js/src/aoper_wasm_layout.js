/*
 * Layout helper for the native EXEC WASM port.
 * Defines a packed memory layout for the EXEC state so JS can marshal
 * the state into a single WASM linear memory buffer.
 */

function makeAllocator(start = 0) {
  let offset = start;
  return function alloc(bytes, align = 4) {
    const aligned = (offset + (align - 1)) & ~(align - 1);
    offset = aligned + bytes;
    return aligned;
  };
}

const F32 = 'f32';
const I32 = 'i32';

const FIELD_DEFS = [
  { name: 'CONVAL', kind: F32 },
  { name: 'PARVAL', kind: F32 },
  { name: 'DELCON', kind: F32 },
  { name: 'XYZREF', kind: F32 },
  { name: 'VINF', kind: F32 },
  { name: 'WROT', kind: F32 },
  { name: 'VINF_A', kind: F32 },
  { name: 'VINF_B', kind: F32 },
  { name: 'GAM_G', kind: F32 },
  { name: 'GAM_D', kind: F32 },
  { name: 'ENC_G', kind: F32 },
  { name: 'ENC_D', kind: F32 },
  { name: 'CLTOT_U', kind: F32 },
  { name: 'CDTOT_U', kind: F32 },
  { name: 'CYTOT_U', kind: F32 },
  { name: 'CLTOT_D', kind: F32 },
  { name: 'CMTOT_D', kind: F32 },
  { name: 'CYTOT_D', kind: F32 },
  { name: 'CMTOT_U', kind: F32 },
  { name: 'ICON', kind: I32 },
  { name: 'LDESDEF', kind: I32 },
  { name: 'LCONDEF', kind: I32 },
];

function fieldLength(state, field) {
  const value = state[field];
  if (value == null) return 0;
  if (typeof value.length === 'number') return value.length;
  return 1;
}

export function buildAoperWasmLayout(state, start = 0) {
  const alloc = makeAllocator(start);
  const layout = { start, fields: {}, bytes: 0 };

  FIELD_DEFS.forEach((def) => {
    const length = fieldLength(state, def.name);
    const bytes = length * 4;
    const offset = alloc(bytes, 4);
    layout.fields[def.name] = { offset, length, kind: def.kind };
  });

  layout.bytes = alloc(0, 4) - start;
  return layout;
}

export function listAoperWasmFields() {
  return FIELD_DEFS.map((f) => ({ ...f }));
}
