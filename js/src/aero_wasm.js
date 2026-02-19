/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import { CROSS, DOT } from './aic.js';
import { CDCL } from './cdcl.js';
import { TPFORC } from './atpforc.js';
import { SFFORC, BDFORC } from './aero.js';

function isNodeRuntime() {
  return typeof process !== 'undefined' && Boolean(process.versions?.node);
}

function isHttpLike(spec) {
  return /^https?:/i.test(spec);
}

function toBufferSource(bytes) {
  if (bytes instanceof ArrayBuffer) return bytes;
  if (ArrayBuffer.isView(bytes)) {
    const view = bytes;
    return view.buffer.slice(view.byteOffset, view.byteOffset + view.byteLength);
  }
  return bytes;
}

async function loadWasmBytes(options = {}) {
  if (options.wasmBytes != null) return toBufferSource(options.wasmBytes);

  if (typeof options.wasmPath === 'string' && options.wasmPath.trim()) {
    const spec = options.wasmPath.trim();
    if (typeof fetch === 'function' && (isHttpLike(spec) || spec.startsWith('/') || spec.startsWith('./') || spec.startsWith('../'))) {
      const res = await fetch(new URL(spec, import.meta.url));
      if (!res.ok) throw new Error(`Failed to load wasm: ${res.status}`);
      return res.arrayBuffer();
    }
    if (isNodeRuntime()) {
      const fsMod = await import('node:fs/promises');
      return fsMod.readFile(spec);
    }
  }

  const wasmCandidates = [
    new URL('./aero.wasm', import.meta.url),
    new URL('../dist/aero.wasm', import.meta.url),
  ];
  for (const wasmUrl of wasmCandidates) {
    if (typeof fetch === 'function' && (wasmUrl.protocol === 'http:' || wasmUrl.protocol === 'https:')) {
      try {
        const res = await fetch(wasmUrl);
        if (res.ok) return res.arrayBuffer();
      } catch (err) {
        if (!isNodeRuntime()) throw err;
      }
    }
    if (isNodeRuntime()) {
      try {
        const fsMod = await import('node:fs/promises');
        return await fsMod.readFile(wasmUrl);
      } catch {
        // try next candidate
      }
    }
  }
  throw new Error('Failed to load aero.wasm');
}

function writeF32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function writeI32(view, offset, arr) {
  view.set(arr, offset / 4);
}

function readF32(view, offset, length) {
  return Float32Array.from(view.subarray(offset / 4, offset / 4 + length));
}

function makeAllocator(start = 4096, ensure) {
  let offset = start;
  return function alloc(bytes, align = 4) {
    const aligned = (offset + (align - 1)) & ~(align - 1);
    const next = aligned + bytes;
    if (ensure) ensure(next);
    offset = next;
    return aligned;
  };
}

// TPFORC_WAT uses a fixed scratch region starting at byte 256.
// Keep JS-managed state allocation above that workspace to avoid overlap.
function aeroScratchFloorBytes(state) {
  const nstrip = Math.max(0, Number(state?.NSTRIP || 0));
  const numax = Math.max(0, Number(state?.NUMAX || 0));
  // bytes = 256 + 4 * ( nstrip*(numax+11) + 6*numax + 4 )
  const tpforcBytes = 256 + 4 * (nstrip * (numax + 11) + 6 * numax + 4);
  // Add guard pad and align up for clean pointer arithmetic.
  const padded = tpforcBytes + 4096;
  return (padded + 15) & ~15;
}

const SCALAR_F32_FIELDS = [
  'PI',
  'ALFA',
  'BETA',
  'MACH',
  'AMACH',
  'YSYM',
  'ZSYM',
  'VRCOREC',
  'VRCOREW',
  'SREF',
  'CREF',
  'BREF',
  'CDREF',
  'CDTOT',
  'CYTOT',
  'CLTOT',
  'CDVTOT',
  'CDTOT_A',
  'CLTOT_A',
  'CLFF',
  'CYFF',
  'CDFF',
  'SPANEF',
];

const SCALAR_I32_FIELDS = [
  'IYSYM',
  'IZSYM',
  'NSTRIP',
  'NVOR',
  'NSURF',
  'NBODY',
  'NCONTROL',
  'NDESIGN',
  'NUMAX',
  'NL',
  'NLNODE',
  'OUTPUT_PTR',
];

const SCALAR_BOOL_FIELDS = [
  'LTRFORCE',
  'LNFLD_WV',
  'LVISC',
];

const ARRAY_I32_FIELDS = [
  'IJFRST',
  'NVSTRP',
  'JFRST',
  'NJ',
  'LSSURF',
  'IMAGS',
  'LNCOMP',
  'LFRST',
  'NLNODE',
];

const ARRAY_BOOL_FIELDS = [
  'LFLOAD',
  'LVISCSTRP',
];

const ARRAY_F32_FIELDS = [
  'XYZREF',
  'VINF',
  'VINF_A',
  'VINF_B',
  'WROT',
  'CFTOT',
  'CMTOT',
  'CHORD',
  'WSTRIP',
  'CHORD1',
  'CHORD2',
  'RLE1',
  'RLE2',
  'RLE',
  'ENSY',
  'ENSZ',
  'ESS',
  'AINC',
  'XSREF',
  'YSREF',
  'ZSREF',
  'SSURF',
  'CAVESURF',
  'RV1',
  'RV2',
  'RV',
  'RC',
  'DXV',
  'ENV',
  'ENV_D',
  'ENV_G',
  'VV',
  'VV_U',
  'VV_D',
  'VV_G',
  'WV',
  'WV_U',
  'WV_D',
  'WV_G',
  'GAM',
  'GAM_U',
  'GAM_D',
  'GAM_G',
  'DCP',
  'DCP_U',
  'DCP_D',
  'DCP_G',
  'DCPB',
  'CNC',
  'CNC_U',
  'CNC_D',
  'CNC_G',
  'PHINGE',
  'VHINGE',
  'DCONTROL',
  'CF_LSTRP',
  'CM_LSTRP',
  'CFSTRP',
  'CMSTRP',
  'CDSTRP',
  'CYSTRP',
  'CLSTRP',
  'CDST_A',
  'CYST_A',
  'CLST_A',
  'CDST_U',
  'CYST_U',
  'CLST_U',
  'CFST_U',
  'CMST_U',
  'CDST_D',
  'CYST_D',
  'CLST_D',
  'CFST_D',
  'CMST_D',
  'CDST_G',
  'CYST_G',
  'CLST_G',
  'CFST_G',
  'CMST_G',
  'CL_LSTRP',
  'CD_LSTRP',
  'CMC4_LSTRP',
  'CA_LSTRP',
  'CN_LSTRP',
  'CLT_LSTRP',
  'CLA_LSTRP',
  'CMLE_LSTRP',
  'CDV_LSTRP',
  'CF_LSRF',
  'CM_LSRF',
  'CDSURF',
  'CYSURF',
  'CLSURF',
  'CFSURF',
  'CMSURF',
  'CDVSURF',
  'CDS_A',
  'CYS_A',
  'CLS_A',
  'CDS_U',
  'CYS_U',
  'CLS_U',
  'CFS_U',
  'CMS_U',
  'CDS_D',
  'CYS_D',
  'CLS_D',
  'CFS_D',
  'CMS_D',
  'CDS_G',
  'CYS_G',
  'CLS_G',
  'CFS_G',
  'CMS_G',
  'CL_LSRF',
  'CD_LSRF',
  'CLCD',
  'CHINGE',
  'CHINGE_U',
  'CHINGE_D',
  'CHINGE_G',
  'CDTOT_U',
  'CYTOT_U',
  'CLTOT_U',
  'CDTOT_D',
  'CYTOT_D',
  'CLTOT_D',
  'CDTOT_G',
  'CYTOT_G',
  'CLTOT_G',
  'CFTOT_U',
  'CMTOT_U',
  'CFTOT_D',
  'CMTOT_D',
  'CFTOT_G',
  'CMTOT_G',
  'CDBDY',
  'CYBDY',
  'CLBDY',
  'CFBDY',
  'CMBDY',
  'RADL',
  'RL',
  'SRC',
  'SRC_U',
];

const PACK_LAYOUTS = Object.create(null);

function registerPackLayout(names, layout) {
  for (const name of names) {
    PACK_LAYOUTS[name] = layout;
  }
}

const dimStrip = (state) => Math.max(0, Number(state?.NSTRIP || 0));
const dimSurf = (state) => Math.max(0, Number(state?.NSURF || 0));
const dimVort = (state) => Math.max(0, Number(state?.NVOR || 0));
const dimCtrl = (state) => Math.max(0, Number(state?.NCONTROL || 0));
const dimDesign = (state) => Math.max(0, Number(state?.NDESIGN || 0));
const dimU = (state) => Math.max(0, Number(state?.NUMAX || 0));

registerPackLayout(
  ['CHORD', 'WSTRIP', 'CHORD1', 'CHORD2', 'ENSY', 'ENSZ', 'AINC', 'XSREF', 'YSREF', 'ZSREF',
    'CNC', 'CDSTRP', 'CYSTRP', 'CLSTRP', 'CDST_A', 'CYST_A', 'CLST_A',
    'CL_LSTRP', 'CD_LSTRP', 'CMC4_LSTRP', 'CA_LSTRP', 'CN_LSTRP', 'CLT_LSTRP',
    'CLA_LSTRP', 'CMLE_LSTRP', 'CDV_LSTRP'],
  { kind: 'scalar1d', count: dimStrip, offset: 1 },
);

registerPackLayout(
  ['SSURF', 'CAVESURF', 'CDSURF', 'CYSURF', 'CLSURF', 'CDVSURF',
    'CDS_A', 'CYS_A', 'CLS_A', 'CL_LSRF', 'CD_LSRF'],
  { kind: 'scalar1d', count: dimSurf, offset: 1 },
);

registerPackLayout(
  ['DXV', 'GAM', 'DCP'],
  { kind: 'scalar1d', count: dimVort, offset: 1 },
);

registerPackLayout(
  ['RLE1', 'RLE2', 'RLE', 'ESS'],
  { kind: 'vec3_1d_stride4', count: dimStrip, offset: 1 },
);

registerPackLayout(
  ['RV1', 'RV2', 'RV', 'RC', 'ENV', 'VV', 'WV'],
  { kind: 'vec3_1d_stride4', count: dimVort, offset: 1 },
);

registerPackLayout(
  ['CFSTRP', 'CMSTRP', 'CF_LSTRP', 'CM_LSTRP'],
  { kind: 'vec3_1d_stride3', count: dimStrip, offset: 1 },
);

registerPackLayout(
  ['CFSURF', 'CMSURF', 'CF_LSRF', 'CM_LSRF'],
  { kind: 'vec3_1d_stride3', count: dimSurf, offset: 1 },
);

registerPackLayout(
  ['CNC_U', 'CDST_U', 'CYST_U', 'CLST_U'],
  {
    kind: 'scalar2d',
    rows: dimStrip,
    cols: dimU,
    rowStride: (state) => dimStrip(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CNC_D', 'CDST_D', 'CYST_D', 'CLST_D'],
  {
    kind: 'scalar2d',
    rows: dimStrip,
    cols: dimCtrl,
    rowStride: (state) => dimStrip(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CNC_G', 'CDST_G', 'CYST_G', 'CLST_G'],
  {
    kind: 'scalar2d',
    rows: dimStrip,
    cols: dimDesign,
    rowStride: (state) => dimStrip(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CDS_U', 'CYS_U', 'CLS_U'],
  {
    kind: 'scalar2d',
    rows: dimSurf,
    cols: dimU,
    rowStride: (state) => dimSurf(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CDS_D', 'CYS_D', 'CLS_D'],
  {
    kind: 'scalar2d',
    rows: dimSurf,
    cols: dimCtrl,
    rowStride: (state) => dimSurf(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CDS_G', 'CYS_G', 'CLS_G'],
  {
    kind: 'scalar2d',
    rows: dimSurf,
    cols: dimDesign,
    rowStride: (state) => dimSurf(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['DCP_U', 'GAM_U'],
  {
    kind: 'scalar2d',
    rows: dimVort,
    cols: dimU,
    rowStride: (state) => Number(state?.DIM_N || (dimVort(state) + 1)),
    rowOffset: 1,
    colOffset: 1,
  },
);

registerPackLayout(
  ['DCP_D', 'GAM_D', 'DCONTROL'],
  {
    kind: 'scalar2d',
    rows: dimVort,
    cols: dimCtrl,
    rowStride: (state) => Number(state?.DIM_N || (dimVort(state) + 1)),
    rowOffset: 1,
    colOffset: 1,
  },
);

registerPackLayout(
  ['DCP_G', 'GAM_G'],
  {
    kind: 'scalar2d',
    rows: dimVort,
    cols: dimDesign,
    rowStride: (state) => Number(state?.DIM_N || (dimVort(state) + 1)),
    rowOffset: 1,
    colOffset: 1,
  },
);

registerPackLayout(
  ['CFST_U', 'CMST_U'],
  {
    kind: 'vec3_2d_stride3',
    rows: dimStrip,
    cols: dimU,
    rowStride: (state) => dimStrip(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CFST_D', 'CMST_D'],
  {
    kind: 'vec3_2d_stride3',
    rows: dimStrip,
    cols: dimCtrl,
    rowStride: (state) => dimStrip(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CFST_G', 'CMST_G'],
  {
    kind: 'vec3_2d_stride3',
    rows: dimStrip,
    cols: dimDesign,
    rowStride: (state) => dimStrip(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CFS_U', 'CMS_U'],
  {
    kind: 'vec3_2d_stride3',
    rows: dimSurf,
    cols: dimU,
    rowStride: (state) => dimSurf(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CFS_D', 'CMS_D'],
  {
    kind: 'vec3_2d_stride3',
    rows: dimSurf,
    cols: dimCtrl,
    rowStride: (state) => dimSurf(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['CFS_G', 'CMS_G'],
  {
    kind: 'vec3_2d_stride3',
    rows: dimSurf,
    cols: dimDesign,
    rowStride: (state) => dimSurf(state) + 1,
    rowOffset: 1,
    colOffset: 0,
  },
);

registerPackLayout(
  ['VV_U', 'WV_U'],
  {
    kind: 'vec3_2d_stride4',
    rows: dimVort,
    cols: dimU,
    rowStride: (state) => Number(state?.DIM_N || (dimVort(state) + 1)),
    rowOffset: 1,
    colOffset: 1,
  },
);

registerPackLayout(
  ['VV_D', 'WV_D', 'ENV_D'],
  {
    kind: 'vec3_2d_stride4',
    rows: dimVort,
    cols: dimCtrl,
    rowStride: (state) => Number(state?.DIM_N || (dimVort(state) + 1)),
    rowOffset: 1,
    colOffset: 1,
  },
);

registerPackLayout(
  ['VV_G', 'WV_G', 'ENV_G'],
  {
    kind: 'vec3_2d_stride4',
    rows: dimVort,
    cols: dimDesign,
    rowStride: (state) => Number(state?.DIM_N || (dimVort(state) + 1)),
    rowOffset: 1,
    colOffset: 1,
  },
);

registerPackLayout(
  ['PHINGE', 'VHINGE'],
  {
    kind: 'vec3_2d_stride4',
    rows: dimStrip,
    cols: dimCtrl,
    rowStride: (state) => Number(state?.NSTRMAX || dimStrip(state)) + 1,
    rowOffset: 1,
    colOffset: 1,
  },
);

registerPackLayout(
  ['NVSTRP'],
  { kind: 'scalar1d', count: dimStrip, offset: 1 },
);

registerPackLayout(
  ['NJ', 'IMAGS', 'LNCOMP'],
  { kind: 'scalar1d', count: dimSurf, offset: 1 },
);

registerPackLayout(
  ['IJFRST', 'LSSURF'],
  { kind: 'scalar1d', count: dimStrip, offset: 1, valueOffset: -1 },
);

registerPackLayout(
  ['JFRST'],
  { kind: 'scalar1d', count: dimSurf, offset: 1, valueOffset: -1 },
);

registerPackLayout(
  ['LFLOAD'],
  { kind: 'scalar1d', count: dimSurf, offset: 1 },
);

registerPackLayout(
  ['LVISCSTRP'],
  { kind: 'scalar1d', count: dimStrip, offset: 1 },
);

function packArrayInto(dst, src, state, name) {
  const layout = PACK_LAYOUTS[name];
  if (!layout) return false;
  const maxLen = Math.min(dst.length, src?.length || 0);
  dst.fill(0);
  if (maxLen === 0) return true;
  switch (layout.kind) {
    case 'scalar1d': {
      const count = Math.max(0, Math.min(layout.count(state), dst.length));
      const offset = layout.offset || 0;
      const valueOffset = layout.valueOffset || 0;
      for (let i = 0; i < count; i += 1) {
        dst[i] = (src[i + offset] ?? 0) + valueOffset;
      }
      return true;
    }
    case 'vec3_1d_stride4': {
      const count = Math.max(0, Math.min(layout.count(state), Math.floor(dst.length / 3)));
      const offset = layout.offset || 0;
      for (let i = 0; i < count; i += 1) {
        const srcBase = 4 * (i + offset) + 1;
        const dstBase = 3 * i;
        dst[dstBase] = src[srcBase] ?? 0;
        dst[dstBase + 1] = src[srcBase + 1] ?? 0;
        dst[dstBase + 2] = src[srcBase + 2] ?? 0;
      }
      return true;
    }
    case 'vec3_1d_stride3': {
      const count = Math.max(0, Math.min(layout.count(state), Math.floor(dst.length / 3)));
      const offset = layout.offset || 0;
      for (let i = 0; i < count; i += 1) {
        const srcBase = 3 * (i + offset);
        const dstBase = 3 * i;
        dst[dstBase] = src[srcBase] ?? 0;
        dst[dstBase + 1] = src[srcBase + 1] ?? 0;
        dst[dstBase + 2] = src[srcBase + 2] ?? 0;
      }
      return true;
    }
    case 'scalar2d': {
      const rows = Math.max(0, layout.rows(state));
      const cols = Math.max(0, layout.cols(state));
      const rowStride = Math.max(1, layout.rowStride(state));
      const rowOffset = layout.rowOffset || 0;
      const colOffset = layout.colOffset || 0;
      for (let j = 0; j < cols; j += 1) {
        for (let i = 0; i < rows; i += 1) {
          const dstIndex = i + rows * j;
          if (dstIndex >= dst.length) break;
          const srcIndex = (i + rowOffset) + rowStride * (j + colOffset);
          dst[dstIndex] = src[srcIndex] ?? 0;
        }
      }
      return true;
    }
    case 'vec3_2d_stride3': {
      const rows = Math.max(0, layout.rows(state));
      const cols = Math.max(0, layout.cols(state));
      const rowStride = Math.max(1, layout.rowStride(state));
      const rowOffset = layout.rowOffset || 0;
      const colOffset = layout.colOffset || 0;
      for (let j = 0; j < cols; j += 1) {
        for (let i = 0; i < rows; i += 1) {
          const dstBase = 3 * (i + rows * j);
          if (dstBase + 2 >= dst.length) break;
          const slot = (i + rowOffset) + rowStride * (j + colOffset);
          const srcBase = 3 * slot;
          dst[dstBase] = src[srcBase] ?? 0;
          dst[dstBase + 1] = src[srcBase + 1] ?? 0;
          dst[dstBase + 2] = src[srcBase + 2] ?? 0;
        }
      }
      return true;
    }
    case 'vec3_2d_stride4': {
      const rows = Math.max(0, layout.rows(state));
      const cols = Math.max(0, layout.cols(state));
      const rowStride = Math.max(1, layout.rowStride(state));
      const rowOffset = layout.rowOffset || 0;
      const colOffset = layout.colOffset || 0;
      for (let j = 0; j < cols; j += 1) {
        for (let i = 0; i < rows; i += 1) {
          const dstBase = 3 * (i + rows * j);
          if (dstBase + 2 >= dst.length) break;
          const slot = (i + rowOffset) + rowStride * (j + colOffset);
          const srcBase = 4 * slot + 1;
          dst[dstBase] = src[srcBase] ?? 0;
          dst[dstBase + 1] = src[srcBase + 1] ?? 0;
          dst[dstBase + 2] = src[srcBase + 2] ?? 0;
        }
      }
      return true;
    }
    default:
      return false;
  }
}

function unpackArrayInto(dst, src, state, name) {
  const layout = PACK_LAYOUTS[name];
  if (!layout) return false;
  switch (layout.kind) {
    case 'scalar1d': {
      const count = Math.max(0, layout.count(state));
      const offset = layout.offset || 0;
      const valueOffset = layout.valueOffset || 0;
      for (let i = 0; i < count; i += 1) {
        dst[i + offset] = (src[i] ?? 0) - valueOffset;
      }
      return true;
    }
    case 'vec3_1d_stride4': {
      const count = Math.max(0, layout.count(state));
      const offset = layout.offset || 0;
      for (let i = 0; i < count; i += 1) {
        const dstBase = 4 * (i + offset) + 1;
        const srcBase = 3 * i;
        dst[dstBase] = src[srcBase] ?? 0;
        dst[dstBase + 1] = src[srcBase + 1] ?? 0;
        dst[dstBase + 2] = src[srcBase + 2] ?? 0;
      }
      return true;
    }
    case 'vec3_1d_stride3': {
      const count = Math.max(0, layout.count(state));
      const offset = layout.offset || 0;
      for (let i = 0; i < count; i += 1) {
        const dstBase = 3 * (i + offset);
        const srcBase = 3 * i;
        dst[dstBase] = src[srcBase] ?? 0;
        dst[dstBase + 1] = src[srcBase + 1] ?? 0;
        dst[dstBase + 2] = src[srcBase + 2] ?? 0;
      }
      return true;
    }
    case 'scalar2d': {
      const rows = Math.max(0, layout.rows(state));
      const cols = Math.max(0, layout.cols(state));
      const rowStride = Math.max(1, layout.rowStride(state));
      const rowOffset = layout.rowOffset || 0;
      const colOffset = layout.colOffset || 0;
      for (let j = 0; j < cols; j += 1) {
        for (let i = 0; i < rows; i += 1) {
          const srcIndex = i + rows * j;
          const dstIndex = (i + rowOffset) + rowStride * (j + colOffset);
          dst[dstIndex] = src[srcIndex] ?? 0;
        }
      }
      return true;
    }
    case 'vec3_2d_stride3': {
      const rows = Math.max(0, layout.rows(state));
      const cols = Math.max(0, layout.cols(state));
      const rowStride = Math.max(1, layout.rowStride(state));
      const rowOffset = layout.rowOffset || 0;
      const colOffset = layout.colOffset || 0;
      for (let j = 0; j < cols; j += 1) {
        for (let i = 0; i < rows; i += 1) {
          const srcBase = 3 * (i + rows * j);
          const slot = (i + rowOffset) + rowStride * (j + colOffset);
          const dstBase = 3 * slot;
          dst[dstBase] = src[srcBase] ?? 0;
          dst[dstBase + 1] = src[srcBase + 1] ?? 0;
          dst[dstBase + 2] = src[srcBase + 2] ?? 0;
        }
      }
      return true;
    }
    case 'vec3_2d_stride4': {
      const rows = Math.max(0, layout.rows(state));
      const cols = Math.max(0, layout.cols(state));
      const rowStride = Math.max(1, layout.rowStride(state));
      const rowOffset = layout.rowOffset || 0;
      const colOffset = layout.colOffset || 0;
      for (let j = 0; j < cols; j += 1) {
        for (let i = 0; i < rows; i += 1) {
          const srcBase = 3 * (i + rows * j);
          const slot = (i + rowOffset) + rowStride * (j + colOffset);
          const dstBase = 4 * slot + 1;
          dst[dstBase] = src[srcBase] ?? 0;
          dst[dstBase + 1] = src[srcBase + 1] ?? 0;
          dst[dstBase + 2] = src[srcBase + 2] ?? 0;
        }
      }
      return true;
    }
    default:
      return false;
  }
}

const LAYOUT_FIELDS = [
  ...SCALAR_F32_FIELDS,
  ...SCALAR_I32_FIELDS,
  ...SCALAR_BOOL_FIELDS,
  ...ARRAY_I32_FIELDS,
  ...ARRAY_BOOL_FIELDS,
  ...ARRAY_F32_FIELDS,
];

export async function loadAeroWasm(options = {}) {
  const debugSync = options.debugSync ?? (isNodeRuntime() && process.env?.AERO_WASM_DEBUG === '1');
  const wasmBytes = await loadWasmBytes(options);

  const imports = {
    env: {
      sin_f32: (x) => Math.fround(Math.sin(x)),
      cos_f32: (x) => Math.fround(Math.cos(x)),
      sqrt_f32: (x) => Math.fround(Math.sqrt(x)),
      cross: (uPtr, vPtr, wPtr) => {
        const mem = imports.env.__mem;
        const f32 = mem.f32;
        const u = Float32Array.from(f32.subarray(uPtr / 4, uPtr / 4 + 3));
        const v = Float32Array.from(f32.subarray(vPtr / 4, vPtr / 4 + 3));
        const out = CROSS(u, v);
        f32[wPtr / 4] = out[0];
        f32[wPtr / 4 + 1] = out[1];
        f32[wPtr / 4 + 2] = out[2];
      },
      dot: (uPtr, vPtr) => {
        const mem = imports.env.__mem;
        const f32 = mem.f32;
        const u = Float32Array.from(f32.subarray(uPtr / 4, uPtr / 4 + 3));
        const v = Float32Array.from(f32.subarray(vPtr / 4, vPtr / 4 + 3));
        return DOT(u, v);
      },
      cdcl: (clcdPtr, clv, outPtr) => {
        const mem = imports.env.__mem;
        const f32 = mem.f32;
        const clcd = Float32Array.from(f32.subarray(clcdPtr / 4, clcdPtr / 4 + 6));
        const res = CDCL(clcd, clv);
        f32[outPtr / 4] = res.cd;
        f32[outPtr / 4 + 1] = res.cd_cl;
      },
      tpforc_js: () => {
        const mem = imports.env.__mem;
        if (typeof mem.syncStateFromMemory === 'function') {
          mem.syncStateFromMemory();
        }
        const state = mem.state;
        const res = TPFORC(state);
        state.CLFF = res.CLFF;
        state.CYFF = res.CYFF;
        state.CDFF = res.CDFF;
        state.SPANEF = res.SPANEF;
        state.DWWAKE.set(res.DWWAKE);
        state.CLFF_U.set(res.CLFF_U);
        state.CYFF_U.set(res.CYFF_U);
        state.CDFF_U.set(res.CDFF_U);
        state.SPANEF_U.set(res.SPANEF_U);
        mem.syncState();
      },
      sfforc_js: () => {
        const mem = imports.env.__mem;
        const state = mem.state;
        if (debugSync) {
          const cftotPtr = mem.i32[(mem.statePtr + mem.offsets.CFTOT) / 4];
          const pre = Float32Array.from(mem.f32.subarray((cftotPtr / 4) - 1, (cftotPtr / 4) + 3));
          const memCftot = Float32Array.from(mem.f32.subarray(cftotPtr / 4, cftotPtr / 4 + 3));
          console.log('[sfforc_js] pre state.CFTOT', Array.from(state.CFTOT));
          console.log('[sfforc_js] pre mem CFTOT', Array.from(memCftot));
          console.log('[sfforc_js] pre mem around CFTOT', Array.from(pre));
        }
        SFFORC(state);
        if (debugSync) {
          console.log('[sfforc_js] post state.CFTOT', Array.from(state.CFTOT));
        }
        mem.syncState();
        if (debugSync) {
          const cftotPtr = mem.i32[(mem.statePtr + mem.offsets.CFTOT) / 4];
          const memCftot = Float32Array.from(mem.f32.subarray(cftotPtr / 4, cftotPtr / 4 + 3));
          console.log('[sfforc_js] post mem CFTOT', Array.from(memCftot));
        }
      },
      bdforc_js: () => {
        const mem = imports.env.__mem;
        const state = mem.state;
        BDFORC(state);
        mem.syncState();
      },
    },
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);
  const { memory, AERO, VINFAB, BDFORC, set_sfforc_js } = instance.exports;
  let f32 = new Float32Array(memory.buffer);
  let i32 = new Int32Array(memory.buffer);
  const refreshViews = () => {
    f32 = new Float32Array(memory.buffer);
    i32 = new Int32Array(memory.buffer);
    imports.env.__mem.f32 = f32;
    imports.env.__mem.i32 = i32;
    syncStateToMemory.__mem = imports.env.__mem;
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
  imports.env.__mem = { f32, i32 };
  syncStateToMemory.__mem = imports.env.__mem;

  const cacheByState = new WeakMap();

  function buildSignature(state) {
    const parts = [];
    for (const name of ARRAY_I32_FIELDS.concat(ARRAY_BOOL_FIELDS, ARRAY_F32_FIELDS)) {
      const arr = state[name];
      const len = arr && typeof arr.length === 'number' ? arr.length : 0;
      parts.push(`${name}:${len}`);
    }
    const nl = state.NL && typeof state.NL.length === 'number' ? state.NL.length : 0;
    parts.push(`NL:${nl}`);
    return parts.join('|');
  }

  function allocStateCached(state) {
    const alloc = makeAllocator(aeroScratchFloorBytes(state), ensureMemory);
    const offsets = {};
    const ptrs = {};

    offsets.LAYOUT = 0;
    let structOffset = 4;

    function addOffset(name) {
      offsets[name] = structOffset;
      structOffset += 4;
    }

    SCALAR_F32_FIELDS.filter((name) => !Object.hasOwn(offsets, name)).forEach(addOffset);
    SCALAR_I32_FIELDS.filter((name) => !Object.hasOwn(offsets, name)).forEach(addOffset);
    SCALAR_BOOL_FIELDS.forEach(addOffset);
    ARRAY_I32_FIELDS.filter((name) => !Object.hasOwn(offsets, name)).forEach(addOffset);
    ARRAY_BOOL_FIELDS.forEach(addOffset);
    ARRAY_F32_FIELDS.forEach(addOffset);

    const base = alloc(structOffset);

    function allocScalarI32(name) {
      if (name === 'NL' && state[name] && typeof state[name].length === 'number') {
        const arr = Int32Array.from(state[name]);
        const ptr = alloc(arr.length * 4);
        writeI32(i32, base + offsets[name], [ptr]);
        writeI32(i32, ptr, arr);
        ptrs[name] = ptr;
        return;
      }
      const val = name in state ? (state[name] | 0) : 0;
      writeI32(i32, base + offsets[name], [val]);
    }

    SCALAR_I32_FIELDS.forEach(allocScalarI32);

    function allocScalarBool(name) {
      const val = name in state ? (state[name] ? 1 : 0) : 0;
      writeI32(i32, base + offsets[name], [val]);
    }

    SCALAR_BOOL_FIELDS.forEach(allocScalarBool);

    function allocArrayField(name, kind) {
      const arr = state[name];
      if (name === 'NLNODE' && typeof arr === 'number') {
        return;
      }
      if (!arr || typeof arr.length !== 'number' || arr.length === 0) {
        writeI32(i32, base + offsets[name], [0]);
        return;
      }
      const ptr = alloc(arr.length * 4);
      writeI32(i32, base + offsets[name], [ptr]);
      ptrs[name] = ptr;
      const memSlice = kind === 'f32'
        ? f32.subarray(ptr / 4, ptr / 4 + arr.length)
        : i32.subarray(ptr / 4, ptr / 4 + arr.length);
      if (kind === 'f32') {
        if (!packArrayInto(memSlice, arr, state, name)) {
          memSlice.set(arr);
        }
      } else if (kind === 'i32') {
        if (!packArrayInto(memSlice, arr, state, name)) {
          memSlice.set(arr);
        }
      } else if (kind === 'bool') {
        const tmp = Int32Array.from(arr, (v) => (v ? 1 : 0));
        if (!packArrayInto(memSlice, tmp, state, name)) {
          memSlice.set(tmp);
        }
      }
    }

    ARRAY_I32_FIELDS.forEach((name) => allocArrayField(name, 'i32'));
    ARRAY_BOOL_FIELDS.forEach((name) => allocArrayField(name, 'bool'));
    ARRAY_F32_FIELDS.forEach((name) => allocArrayField(name, 'f32'));

    const layoutPtr = alloc(LAYOUT_FIELDS.length * 4);
    for (let i = 0; i < LAYOUT_FIELDS.length; i += 1) {
      const name = LAYOUT_FIELDS[i];
      writeI32(i32, layoutPtr + i * 4, [offsets[name] ?? 0]);
    }
    writeI32(i32, base + offsets.LAYOUT, [layoutPtr]);

    const outputBase = alloc(256);
    writeI32(i32, base + offsets.OUTPUT_PTR, [outputBase]);
    const outputOffsets = {
      CDTOT: outputBase + 0,
      CYTOT: outputBase + 4,
      CLTOT: outputBase + 8,
      CFTOT: outputBase + 12,
      CMTOT: outputBase + 24,
      CDVTOT: outputBase + 36,
      CLFF: outputBase + 40,
      CYFF: outputBase + 44,
      CDFF: outputBase + 48,
      SPANEF: outputBase + 52,
      DCP: outputBase + 56,
      CNC: outputBase + 68,
      CFSTRP: outputBase + 80,
      CMSTRP: outputBase + 92,
      CDSTRP: outputBase + 104,
      CYSTRP: outputBase + 108,
      CLSTRP: outputBase + 112,
      CDVSURF: outputBase + 116,
      CDSURF: outputBase + 120,
      CYSURF: outputBase + 124,
      CLSURF: outputBase + 128,
    };

    return { base, offsets, outputOffsets, ptrs, signature: buildSignature(state) };
  }

  function getStateCache(state) {
    const signature = buildSignature(state);
    const cached = cacheByState.get(state);
    if (cached && cached.signature === signature) {
      return cached;
    }
    const next = allocStateCached(state);
    cacheByState.set(state, next);
    return next;
  }

  function VINFAB_wasm(state) {
    const mem = getStateCache(state);
    imports.env.__mem.state = state;
    imports.env.__mem.outputOffsets = mem.outputOffsets;
    imports.env.__mem.statePtr = mem.base;
    imports.env.__mem.offsets = mem.offsets;
    imports.env.__mem.syncState = () => syncStateToMemory(state, mem.base, mem.offsets);
    imports.env.__mem.syncStateFromMemory = () => syncStateFromMemory(state, mem.base, mem.offsets);
    imports.env.__mem.syncState();
    VINFAB(mem.base);
    const vinfPtr = i32[(mem.base + mem.offsets.VINF) / 4];
    const vinfAPtr = i32[(mem.base + mem.offsets.VINF_A) / 4];
    const vinfBPtr = i32[(mem.base + mem.offsets.VINF_B) / 4];
    return {
      VINF: readF32(f32, vinfPtr, 3),
      VINF_A: readF32(f32, vinfAPtr, 3),
      VINF_B: readF32(f32, vinfBPtr, 3),
    };
  }

  if (typeof set_sfforc_js === 'function') {
    set_sfforc_js(0);
  }

  function AERO_wasm(state) {
    const mem = getStateCache(state);
    imports.env.__mem.state = state;
    imports.env.__mem.outputOffsets = mem.outputOffsets;
    imports.env.__mem.statePtr = mem.base;
    imports.env.__mem.offsets = mem.offsets;
    imports.env.__mem.syncState = () => syncStateToMemory(state, mem.base, mem.offsets);
    imports.env.__mem.syncStateFromMemory = () => syncStateFromMemory(state, mem.base, mem.offsets);
    imports.env.__mem.syncState();
    AERO(mem.base);
    if (debugSync) {
      const cftotPtr = imports.env.__mem.i32[(mem.base + mem.offsets.CFTOT) / 4];
      const memCftot = Float32Array.from(imports.env.__mem.f32.subarray(cftotPtr / 4, cftotPtr / 4 + 3));
      console.log('[AERO_wasm] mem CFTOT', Array.from(memCftot));
    }
    imports.env.__mem.syncStateFromMemory();
    if (debugSync) {
      console.log('[AERO_wasm] state CFTOT', Array.from(state.CFTOT));
    }
    return {
      CDTOT: state.CDTOT,
      CYTOT: state.CYTOT,
      CLTOT: state.CLTOT,
      CFTOT: Float32Array.from(state.CFTOT),
      CMTOT: Float32Array.from(state.CMTOT),
      CDVTOT: state.CDVTOT,
      CLFF: state.CLFF,
      CYFF: state.CYFF,
      CDFF: state.CDFF,
      SPANEF: state.SPANEF,
      DCP: Float32Array.from(state.DCP),
      CNC: Float32Array.from(state.CNC),
      CFSTRP: Float32Array.from(state.CFSTRP),
      CMSTRP: Float32Array.from(state.CMSTRP),
      CDSTRP: Float32Array.from(state.CDSTRP),
      CYSTRP: Float32Array.from(state.CYSTRP),
      CLSTRP: Float32Array.from(state.CLSTRP),
      CDVSURF: Float32Array.from(state.CDVSURF),
      CDSURF: Float32Array.from(state.CDSURF),
      CYSURF: Float32Array.from(state.CYSURF),
      CLSURF: Float32Array.from(state.CLSURF),
    };
  }

  function BDFORC_wasm(state) {
    const mem = getStateCache(state);
    imports.env.__mem.state = state;
    imports.env.__mem.outputOffsets = mem.outputOffsets;
    imports.env.__mem.statePtr = mem.base;
    imports.env.__mem.offsets = mem.offsets;
    imports.env.__mem.syncState = () => syncStateToMemory(state, mem.base, mem.offsets);
    imports.env.__mem.syncStateFromMemory = () => syncStateFromMemory(state, mem.base, mem.offsets);
    imports.env.__mem.syncState();
    BDFORC(mem.base);
    imports.env.__mem.syncStateFromMemory();
    return {
      CDBDY: Float32Array.from(state.CDBDY),
      CYBDY: Float32Array.from(state.CYBDY),
      CLBDY: Float32Array.from(state.CLBDY),
      CFBDY: Float32Array.from(state.CFBDY),
      CMBDY: Float32Array.from(state.CMBDY),
      DCPB: Float32Array.from(state.DCPB),
      CDTOT: state.CDTOT,
      CYTOT: state.CYTOT,
      CLTOT: state.CLTOT,
      CFTOT: Float32Array.from(state.CFTOT),
      CMTOT: Float32Array.from(state.CMTOT),
      CDTOT_U: Float32Array.from(state.CDTOT_U),
      CYTOT_U: Float32Array.from(state.CYTOT_U),
      CLTOT_U: Float32Array.from(state.CLTOT_U),
      CFTOT_U: Float32Array.from(state.CFTOT_U),
      CMTOT_U: Float32Array.from(state.CMTOT_U),
    };
  }

  return { VINFAB: VINFAB_wasm, AERO: AERO_wasm, BDFORC: BDFORC_wasm, memory };
}

function syncStateToMemory(state, statePtr, offsets) {
  const mem = syncStateToMemory.__mem;
  const { f32, i32 } = mem;
  const syncScalars = SCALAR_F32_FIELDS;
  const syncBools = SCALAR_BOOL_FIELDS;
  const syncArrays = ARRAY_F32_FIELDS;

  for (const name of syncScalars) {
    if (!(name in offsets) || !(name in state)) continue;
    f32[(statePtr + offsets[name]) / 4] = state[name];
  }

  for (const name of syncBools) {
    if (!(name in offsets) || !(name in state)) continue;
    i32[(statePtr + offsets[name]) / 4] = state[name] ? 1 : 0;
  }

  for (const name of syncArrays) {
    if (!(name in offsets) || !(name in state)) continue;
    const arr = state[name];
    const ptr = i32[(statePtr + offsets[name]) / 4];
    if (!ptr || !arr) continue;
    const memSlice = f32.subarray(ptr / 4, ptr / 4 + arr.length);
    if (!packArrayInto(memSlice, arr, state, name)) {
      memSlice.set(arr);
    }
  }
}

function syncStateFromMemory(state, statePtr, offsets) {
  const mem = syncStateToMemory.__mem;
  const { f32, i32 } = mem;
  const syncScalars = [
    'CDTOT',
    'CYTOT',
    'CLTOT',
    'CDVTOT',
    'CDTOT_A',
    'CLTOT_A',
    'CLFF',
    'CYFF',
    'CDFF',
    'SPANEF',
  ];
  const syncArrays = [
    'CFTOT',
    'CMTOT',
    'CDBDY',
    'CYBDY',
    'CLBDY',
    'CFBDY',
    'CMBDY',
    'DCPB',
    'CHINGE',
    'CHINGE_U',
    'CHINGE_D',
    'CHINGE_G',
    'CDTOT_U',
    'CYTOT_U',
    'CLTOT_U',
    'CFTOT_U',
    'CMTOT_U',
    'CDTOT_D',
    'CYTOT_D',
    'CLTOT_D',
    'CFTOT_D',
    'CMTOT_D',
    'CDTOT_G',
    'CYTOT_G',
    'CLTOT_G',
    'CFTOT_G',
    'CMTOT_G',
    'DCP',
    'CNC',
    'CFSTRP',
    'CMSTRP',
    'CDSTRP',
    'CYSTRP',
    'CLSTRP',
    'CDVSURF',
    'CDSURF',
    'CYSURF',
    'CLSURF',
  ];

  for (const name of syncScalars) {
    if (!(name in offsets) || !(name in state)) continue;
    state[name] = f32[(statePtr + offsets[name]) / 4];
  }

  for (const name of syncArrays) {
    if (!(name in offsets) || !(name in state)) continue;
    const arr = state[name];
    const ptr = i32[(statePtr + offsets[name]) / 4];
    if (!ptr || !arr) continue;
    const memSlice = f32.subarray(ptr / 4, ptr / 4 + arr.length);
    if (!unpackArrayInto(arr, memSlice, state, name)) {
      arr.set(memSlice);
    }
  }
}
