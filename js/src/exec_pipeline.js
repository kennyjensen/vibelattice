/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { EXEC } from './aoper.js';
import { MAKESURF, ENCALC, SDUPL } from './amake.js';
import { GETCAM } from './airutil.js';
import { AKIMA, NRMLIZ } from './sgutil.js';

function stripInlineComment(line) {
  const text = String(line || '');
  const hashIdx = text.indexOf('#');
  const bangIdx = text.indexOf('!');
  let cutIdx = -1;
  if (hashIdx >= 0) cutIdx = hashIdx;
  if (bangIdx >= 0 && (cutIdx < 0 || bangIdx < cutIdx)) cutIdx = bangIdx;
  return cutIdx >= 0 ? text.slice(0, cutIdx) : text;
}

function parseNumbers(line) {
  const clean = stripInlineComment(line).trim();
  if (!clean) return [];
  return clean
    .split(/\s+/)
    .map((v) => Number(v))
    .filter((v) => Number.isFinite(v));
}

export function parseAVL(text) {
  const lines = text.split(/\r?\n/);
  const surfaces = [];
  let i = 0;
  const inlineAirfoils = [];

  const header = {
    title: '',
    mach: 0.0,
    iysym: 0,
    izsym: 0,
    zsym: 0.0,
    sref: 1.0,
    cref: 1.0,
    bref: 1.0,
    xref: 0.0,
    yref: 0.0,
    zref: 0.0,
  };

  const isComment = (line) => {
    const t = line.trim();
    return !t || t.startsWith('#') || t.startsWith('!') || t.startsWith('%');
  };

  const nextLine = () => {
    while (i < lines.length) {
      const line = lines[i++];
      if (!isComment(line)) return line.trim();
    }
    return null;
  };

  header.title = nextLine() || '';
  const machLine = nextLine();
  if (machLine) header.mach = Number(machLine.trim()) || 0.0;
  const symLine = nextLine();
  if (symLine) {
    const vals = parseNumbers(symLine);
    if (vals.length >= 3) {
      header.iysym = vals[0];
      header.izsym = vals[1];
      header.zsym = vals[2];
    }
  }
  const refLine = nextLine();
  if (refLine) {
    const vals = parseNumbers(refLine);
    if (vals.length >= 3) {
      header.sref = vals[0];
      header.cref = vals[1];
      header.bref = vals[2];
    }
  }
  const cgLine = nextLine();
  if (cgLine) {
    const vals = parseNumbers(cgLine);
    if (vals.length >= 3) {
      header.xref = vals[0];
      header.yref = vals[1];
      header.zref = vals[2];
    }
  }

  const readValueLine = () => {
    const line = nextLine();
    return line ? parseNumbers(line) : [];
  };

  const readAirfoilBlock = () => {
    const coords = [];
    while (i < lines.length) {
      const line = lines[i];
      if (!line.trim()) {
        i += 1;
        break;
      }
      if (/^[A-Za-z]/.test(line.trim())) break;
      const nums = parseNumbers(line);
      if (nums.length >= 2) coords.push([nums[0], nums[1]]);
      i += 1;
    }
    return coords;
  };

  while (i < lines.length) {
    const line = nextLine();
    if (!line) break;
    if (!line.toUpperCase().startsWith('SURFACE')) continue;
    const nameLine = nextLine();
    const surface = {
      name: (nameLine || '').trim(),
      nChord: 0,
      cSpace: 1.0,
      nSpan: 0,
      sSpace: 1.0,
      component: 0,
      yduplicate: null,
      angle: 0.0,
      scale: [1.0, 1.0, 1.0],
      translate: [0.0, 0.0, 0.0],
      sections: [],
    };
    const surfVals = readValueLine();
    if (surfVals.length >= 2) {
      surface.nChord = surfVals[0];
      surface.cSpace = surfVals[1];
      surface.nSpan = surfVals[2] ?? 0;
      surface.sSpace = surfVals[3] ?? 1.0;
    }

    let currentSection = null;
    while (i < lines.length) {
      const raw = lines[i];
      if (raw == null) break;
      const trimmed = raw.trim();
      if (!trimmed || trimmed.startsWith('#') || trimmed.startsWith('!') || trimmed.startsWith('%')) {
        i += 1;
        continue;
      }
      const key = trimmed.split(/\s+/)[0].toUpperCase();
      if (key === 'SURFACE') break;
      i += 1;

      const subkey = key.slice(0, 4);
      if (subkey === 'COMP') {
        const vals = parseNumbers(trimmed.slice(4));
        surface.component = vals[0] ?? parseNumbers(nextLine() || '')[0] ?? surface.component;
      } else if (subkey === 'YDUP') {
        const vals = parseNumbers(trimmed.slice(4));
        surface.yduplicate = vals[0] ?? parseNumbers(nextLine() || '')[0] ?? surface.yduplicate;
      } else if (subkey === 'ANGL') {
        const vals = parseNumbers(trimmed.slice(4));
        surface.angle = vals[0] ?? parseNumbers(nextLine() || '')[0] ?? surface.angle;
      } else if (subkey === 'SCAL') {
        const vals = parseNumbers(trimmed.slice(4));
        const val2 = vals.length >= 3 ? vals : readValueLine();
        if (val2.length >= 3) surface.scale = [val2[0], val2[1], val2[2]];
      } else if (subkey === 'TRAN') {
        const vals = parseNumbers(trimmed.slice(4));
        const val2 = vals.length >= 3 ? vals : readValueLine();
        if (val2.length >= 3) surface.translate = [val2[0], val2[1], val2[2]];
      } else if (subkey === 'NOWA') {
        surface.nowake = true;
      } else if (subkey === 'NOLO') {
        surface.noload = true;
      } else if (subkey === 'SECT') {
        const data = parseNumbers(trimmed.slice(4));
        const vals = data.length >= 5 ? data : readValueLine();
        if (vals.length >= 5) {
          currentSection = {
            xle: vals[0],
            yle: vals[1],
            zle: vals[2],
            chord: vals[3],
            ainc: vals[4],
            aincDeg: vals[4],
            nSpan: vals[5],
            sSpace: vals[6],
            controls: [],
            naca: null,
            airfoilFile: null,
            airfoilCoords: null,
          };
          surface.sections.push(currentSection);
        }
      } else if (subkey === 'NACA') {
        if (currentSection) {
          const code = trimmed.slice(4).trim() || (nextLine() || '');
          const match = code.match(/(\d{4})/);
          currentSection.naca = match ? match[1] : null;
        }
      } else if (subkey === 'AFIL') {
        if (currentSection) {
          const parts = trimmed.split(/\s+/);
          let apath = parts.length > 1 ? parts.slice(1).join(' ') : '';
          if (!apath) apath = nextLine() || '';
          currentSection.airfoilFile = apath || null;
          if (apath) inlineAirfoils.push(apath);
        }
      } else if (subkey === 'AIRF') {
        if (currentSection) {
          currentSection.airfoilCoords = readAirfoilBlock();
        }
    } else if (subkey === 'CONT') {
      if (currentSection) {
        let parts = trimmed.split(/\s+/).slice(1);
        if (parts.length < 2) {
          const next = nextLine() || '';
          parts = next.trim().split(/\s+/);
        }
        const name = parts.shift() || 'CTRL';
        const nums = parts.map((v) => Number(v)).filter((v) => Number.isFinite(v));
        const gain = nums[0] ?? 1.0;
        const xhinge = nums[1] ?? 0.75;
        const vhinge = [nums[2] ?? 0.0, nums[3] ?? 0.0, nums[4] ?? 0.0];
        const sgnDup = nums[5] ?? 1.0;
        currentSection.controls.push({
            name,
            gain,
            xhinge,
            vhinge,
            sgnDup,
          });
        }
      }
    }
    surfaces.push(surface);
  }
  return { header, surfaces, airfoilFiles: inlineAirfoils };
}

export function parseAirfoilText(text) {
  const lines = text.split(/\r?\n/);
  const coords = [];
  for (const raw of lines) {
    const trimmed = raw.trim();
    if (!trimmed) continue;
    if (trimmed.startsWith('#') || trimmed.startsWith('!') || trimmed.startsWith('%')) continue;
    const nums = trimmed.split(/\s+/).map((v) => Number(v)).filter((v) => Number.isFinite(v));
    if (nums.length >= 2) coords.push([nums[0], nums[1]]);
  }
  return coords;
}

function splitAirfoilSurfaces(coords) {
  if (!coords.length) return { upper: [], lower: [] };
  let leIndex = 0;
  let minX = coords[0][0];
  for (let i = 1; i < coords.length; i += 1) {
    if (coords[i][0] < minX) {
      minX = coords[i][0];
      leIndex = i;
    }
  }
  const upper = coords.slice(0, leIndex + 1);
  const lower = coords.slice(leIndex);
  return { upper, lower };
}

function interpY(curve, x) {
  for (let i = 0; i < curve.length - 1; i += 1) {
    const [x0, y0] = curve[i];
    const [x1, y1] = curve[i + 1];
    if ((x >= x0 && x <= x1) || (x >= x1 && x <= x0)) {
      const t = (x - x0) / (x1 - x0 || 1e-6);
      return y0 + t * (y1 - y0);
    }
  }
  return curve[curve.length - 1]?.[1] ?? 0.0;
}

export function buildCamberSlope(coords, samples = 50) {
  if (!coords?.length) return null;
  const n = coords.length;
  const X = new Float32Array(n);
  const Y = new Float32Array(n);
  for (let i = 0; i < n; i += 1) {
    X[i] = coords[i][0];
    Y[i] = coords[i][1];
  }

  const nIn = Math.min(samples, n);
  const XC = new Float32Array(nIn);
  const YC = new Float32Array(nIn);
  const TC = new Float32Array(nIn);
  GETCAM(X, Y, n, XC, YC, TC, nIn, true);

  const xs = new Float32Array(nIn);
  const slope = new Float32Array(nIn);
  const thick = new Float32Array(nIn);
  const x0 = XC[0];
  const x1 = XC[nIn - 1];
  const den = nIn > 1 ? (nIn - 1) : 1;
  for (let i = 0; i < nIn; i += 1) {
    const xf = i / den;
    xs[i] = x0 + (x1 - x0) * xf;
    slope[i] = AKIMA(XC, YC, nIn, xs[i]).SLP;
    thick[i] = AKIMA(XC, TC, nIn, xs[i]).YY;
  }
  NRMLIZ(nIn, xs);
  return { x: Array.from(xs), s: Array.from(slope), t: Array.from(thick) };
}

export function buildNacaSlope(code, samples = 60) {
  const digits = String(code || '').padStart(4, '0');
  const m = Number(digits[0]) / 100;
  const p = Number(digits[1]) / 10;
  const xs = [];
  const slope = [];
  for (let i = 0; i <= samples; i += 1) {
    const x = i / samples;
    let dy = 0.0;
    if (m !== 0 && p !== 0) {
      if (x < p) {
        dy = (2 * m / (p * p)) * (p - x);
      } else {
        dy = (2 * m / ((1 - p) * (1 - p))) * (p - x);
      }
    }
    xs.push(x);
    slope.push(dy);
  }
  return { x: xs, s: slope, t: slope.map(() => 0.0) };
}

export async function buildSolverModel(text, options = {}) {
  const { airfoilResolver, baseDir } = options;
  const model = parseAVL(text);
  const controlMap = new Map();
  const surfaces = [];
  const resolveAirfoil = airfoilResolver || (async (name) => {
    if (!baseDir) return null;
    const full = path.isAbsolute(name) ? name : path.join(baseDir, name);
    try {
      return await fs.readFile(full, 'utf8');
    } catch {
      return null;
    }
  });

  for (const surf of model.surfaces) {
    const baseIndex = surfaces.length + 1;
    if (surf.component == null || surf.component === 0) {
      surf.component = baseIndex;
    }
    if (surf.imags == null) {
      surf.imags = 1;
    }
    surfaces.push(surf);
    for (const sec of surf.sections) {
      for (const ctrl of sec.controls) {
        if (!controlMap.has(ctrl.name)) {
          controlMap.set(ctrl.name, controlMap.size + 1);
        }
        ctrl.index = controlMap.get(ctrl.name);
      }

      let coords = sec.airfoilCoords;
      if (!coords && sec.airfoilFile) {
        const textData = await resolveAirfoil(sec.airfoilFile);
        if (textData) coords = parseAirfoilText(textData);
      }
      if (coords && coords.length) {
        sec.airfoilCamber = buildCamberSlope(coords);
      } else if (sec.naca) {
        sec.airfoilCamber = buildNacaSlope(sec.naca);
      } else {
        sec.airfoilCamber = buildNacaSlope('0000');
      }
      sec.claf = 1.0;
    }
  }
  model.surfaces = surfaces;
  model.controlMap = controlMap;
  return model;
}

export function applyZSymmetry(model) {
  if (!model || !model.header || model.header.izsym !== 1) return model;
  const zsym = Number(model.header.zsym ?? 0);
  const mirrored = model.surfaces.map((surf) => {
    const copy = JSON.parse(JSON.stringify(surf));
    copy.name = `${surf.name}-zsym`;
    copy.imags = (copy.imags ?? 1) * -1;
    copy.sections.forEach((sec) => {
      sec.zle = 2 * zsym - sec.zle;
      if (sec.controls) {
        sec.controls.forEach((ctrl) => {
          if (ctrl.vhinge) ctrl.vhinge[2] *= -1;
        });
      }
    });
    return copy;
  });
  model.surfaces = model.surfaces.concat(mirrored);
  return model;
}

export function applyYSymmetry(model) {
  if (!model || !model.header || model.header.iysym !== 1) return model;
  const mirrored = model.surfaces.map((surf) => {
    const copy = JSON.parse(JSON.stringify(surf));
    copy.name = `${surf.name}-ysym`;
    copy.imags = (copy.imags ?? 1) * -1;
    copy.sections.forEach((sec) => {
      sec.yle = -sec.yle;
      if (sec.controls) {
        sec.controls.forEach((ctrl) => {
          if (ctrl.vhinge) ctrl.vhinge[1] *= -1;
          if (ctrl.sgnDup) ctrl.sgnDup *= -1;
        });
      }
    });
    return copy;
  });
  model.surfaces = model.surfaces.concat(mirrored);
  return model;
}

export function applyYDuplicate(model) {
  if (!model || !Array.isArray(model.surfaces)) return model;
  const next = [];
  model.surfaces.forEach((surf) => {
    const ydup = surf.yduplicate;
    const base = { ...surf, yduplicate: null };
    next.push(base);
    if (typeof ydup !== 'number') return;
    const copy = JSON.parse(JSON.stringify(base));
    copy.name = `${surf.name}-ydup`;
    if (Array.isArray(copy.translate) && copy.translate.length >= 2) {
      copy.translate[1] = 2 * ydup - copy.translate[1];
    }
    copy.sections.forEach((sec) => {
      sec.yle = -sec.yle;
      if (sec.controls) {
        sec.controls.forEach((ctrl) => {
          if (ctrl.vhinge) ctrl.vhinge[1] *= -1;
          if (ctrl.sgnDup) ctrl.sgnDup *= -1;
        });
      }
    });
    next.push(copy);
  });
  model.surfaces = next;
  return model;
}

export function buildExecState(model, options = {}) {
  const defaults = {
    vel: 30,
    rho: 1.225,
    gee: 9.81,
    cl: 0.6,
    bank: 0,
    alpha: 2.0,
    beta: 0.0,
    cmx: 0.0,
    cmy: 0.0,
    cmz: 0.0,
    cd0: 0.0,
    xcg: null,
    ycg: null,
    zcg: null,
  };
  const opts = { ...defaults, ...options };

  const IVALFA = 1;
  const IVBETA = 2;
  const IVROTX = 3;
  const IVROTY = 4;
  const IVROTZ = 5;
  const IVTOT = 5;

  const ICALFA = 1;
  const ICBETA = 2;
  const ICROTX = 3;
  const ICROTY = 4;
  const ICROTZ = 5;
  const ICCL = 6;
  const ICCY = 7;
  const ICMOMX = 8;
  const ICMOMY = 9;
  const ICMOMZ = 10;
  const ICTOT = 10;

  const IPALFA = 1;
  const IPBETA = 2;
  const IPROTX = 3;
  const IPROTY = 4;
  const IPROTZ = 5;
  const IPCL = 6;
  const IPCD0 = 7;
  const IPPHI = 8;
  const IPTHE = 9;
  const IPPSI = 10;
  const IPMACH = 11;
  const IPVEE = 12;
  const IPRHO = 13;
  const IPGEE = 14;
  const IPRAD = 15;
  const IPFAC = 16;
  const IPXCG = 17;
  const IPYCG = 18;
  const IPZCG = 19;
  const IPMASS = 20;
  const IPIXX = 21;
  const IPIYY = 22;
  const IPIZZ = 23;
  const IPIXY = 24;
  const IPIYZ = 25;
  const IPIZX = 26;
  const IPCLA = 27;
  const IPCLU = 28;
  const IPCMA = 29;
  const IPCMU = 30;
  const IPTOT = 30;

  const dupCount = model.surfaces.reduce((sum, surf) => (
    sum + (typeof surf.yduplicate === 'number' ? 1 : 0)
  ), 0);
  const NSURF = model.surfaces.length + dupCount;
  const NCONTROL = model.controlMap?.size ?? 0;
  const NDESIGN = 0;
  const NUMAX = 6;
  const NDMAX = Math.max(1, NCONTROL);
  const NGMAX = Math.max(1, NDESIGN);

  let NSTRIP = 0;
  let NVOR = 0;
  model.surfaces.forEach((surf) => {
    const nvc = surf.nChord || 1;
    let nvs = surf.nSpan || 0;
    if (nvs === 0) {
      nvs = surf.sections.slice(0, -1).reduce((sum, sec) => sum + (sec.nSpan ?? 0), 0);
    }
    const copies = typeof surf.yduplicate === 'number' ? 2 : 1;
    NSTRIP += nvs * copies;
    NVOR += nvs * nvc * copies;
  });

  const NVMAX = Math.max(1, NVOR);
  const NSTRMAX = Math.max(1, NSTRIP);
  const NRMAX = 1;
  const NLMAX = 1;
  const IVMAX = IVTOT + NDMAX;
  const ICMAX = ICTOT + NDMAX;

  const DIM_N = NVMAX + 1;
  const DIM_U = NUMAX + 1;
  const DIM_C = NDMAX + 1;
  const DIM_G = NGMAX + 1;
  const DIM_L = NLMAX + 1;

  const state = {
    __modelRef: model,
    IVALFA, IVBETA, IVROTX, IVROTY, IVROTZ, IVTOT,
    ICALFA, ICBETA, ICROTX, ICROTY, ICROTZ, ICCL, ICCY, ICMOMX, ICMOMY, ICMOMZ, ICTOT,
    IPALFA, IPBETA, IPROTX, IPROTY, IPROTZ, IPCL, IPCD0, IPPHI, IPTHE, IPPSI,
    IPMACH, IPVEE, IPRHO, IPGEE, IPRAD, IPFAC, IPXCG, IPYCG, IPZCG, IPMASS,
    IPIXX, IPIYY, IPIZZ, IPIXY, IPIYZ, IPIZX, IPCLA, IPCLU, IPCMA, IPCMU, IPTOT,
    NVOR,
    NVMAX,
    NSTRIP,
    NSTRMAX,
    NSURF,
    NCONTROL,
    NDESIGN,
    NUMAX,
    NDMAX,
    NGMAX,
    IVMAX,
    ICMAX,
    NRMAX,
    NVTOT: IVTOT + NCONTROL,
    NBODY: 0,
    NLNODE: 0,
    NLMAX: 1,
    DIM_N,
    DIM_U,
    DIM_C,
    DIM_G,
    DIM_L,

    PI: Math.fround(Math.PI),
    DTR: Math.fround(Math.PI / 180.0),
    UNITL: Math.fround(1.0),
    IYSYM: model.header.iysym ?? 0,
    IZSYM: model.header.izsym ?? 0,
    YSYM: 0.0,
    ZSYM: model.header.zsym ?? 0.0,
    VRCOREC: 0.0,
    VRCOREW: Math.fround(2.0),
    SRCORE: Math.fround(1.0),
    SAXFR: Math.fround(0.25),

    LNASA_SA: true,
    LSA_RATES: false,
    LAIC: false,
    LSRD: false,
    LVEL: false,
    LSOL: false,
    LSEN: false,
    LOBAIC: false,
    LOBVEL: false,
    LVISC: false,
    LBFORCE: false,
    LTRFORCE: false,
    LNFLD_WV: false,
    LMASS: Boolean(opts.massLoaded),
    LFLOAD: new Uint8Array(NSURF + 1),

    ALFA: 0.0,
    BETA: 0.0,
    MACH: Math.fround(model.header.mach ?? 0.0),
    AMACH: Math.fround(model.header.mach ?? 0.0),
    BETM: 0.0,
    VINF: new Float32Array(3),
    VINF_A: new Float32Array(3),
    VINF_B: new Float32Array(3),
    WROT: new Float32Array(3),
    AMASS: new Float32Array(9),
    AINER: new Float32Array(9),
    XYZREF: new Float32Array(3),
    SREF: Math.fround(model.header.sref ?? 1.0),
    CREF: Math.fround(model.header.cref ?? 1.0),
    BREF: Math.fround(model.header.bref ?? 1.0),
    CDREF: 0.0,

    PARVAL: new Float32Array((IPTOT + 1) * (NRMAX + 1)),
    CONVAL: new Float32Array((ICMAX + 1) * (NRMAX + 1)),
    ICON: new Int32Array((IVMAX + 1) * (NRMAX + 1)),
    ITRIM: new Int32Array(NRMAX + 1),

    DELCON: new Float32Array(NDMAX + 1),
    DELDES: new Float32Array(NGMAX + 1),

    IMAGS: new Int32Array(NSURF + 1),
    IFRST: new Int32Array(NSURF + 1),
    JFRST: new Int32Array(NSURF + 1),
    NK: new Int32Array(NSURF + 1),
    NJ: new Int32Array(NSURF + 1),
    LFWAKE: new Uint8Array(NSURF + 1),
    LNCOMP: new Int32Array(NSURF + 1),
    SSURF: new Float32Array(NSURF + 1),
    CAVESURF: new Float32Array(NSURF + 1),

    LFRST: new Int32Array(NLMAX + 1),
    NL: new Int32Array(NLMAX + 1),
    RL: new Float32Array(4 * (NLMAX + 1)),
    RADL: new Float32Array(NLMAX + 1),

    IJFRST: new Int32Array(NSTRMAX + 1),
    NVSTRP: new Int32Array(NSTRMAX + 1),
    LSSURF: new Int32Array(NSTRMAX + 1),

    RLE1: new Float32Array(4 * (NSTRMAX + 1)),
    RLE2: new Float32Array(4 * (NSTRMAX + 1)),
    RLE: new Float32Array(4 * (NSTRMAX + 1)),
    CHORD1: new Float32Array(NSTRMAX + 1),
    CHORD2: new Float32Array(NSTRMAX + 1),
    CHORD: new Float32Array(NSTRMAX + 1),
    WSTRIP: new Float32Array(NSTRMAX + 1),
    TANLE: new Float32Array(NSTRMAX + 1),
    TANTE: new Float32Array(NSTRMAX + 1),
    AINC: new Float32Array(NSTRMAX + 1),
    AINC_G: new Float32Array((NSTRMAX + 1) * (NGMAX + 1)),
    CLCD: new Float32Array(6 * (NSTRMAX + 1)),
    LVISCSTRP: new Uint8Array(NSTRMAX + 1),

    RV1: new Float32Array(4 * (NVMAX + 1)),
    RV2: new Float32Array(4 * (NVMAX + 1)),
    RV: new Float32Array(4 * (NVMAX + 1)),
    RC: new Float32Array(4 * (NVMAX + 1)),
    RS: new Float32Array(4 * (NVMAX + 1)),
    DXV: new Float32Array(NVMAX + 1),
    CHORDV: new Float32Array(NVMAX + 1),
    SLOPEC: new Float32Array(NVMAX + 1),
    SLOPEV: new Float32Array(NVMAX + 1),
    LVCOMP: new Int32Array(NVMAX + 1),
    LVNC: new Uint8Array(NVMAX + 1),
    LVALBE: new Uint8Array(NVMAX + 1),

    DCONTROL: new Float32Array((NVMAX + 1) * (NDMAX + 1)),
    VHINGE: new Float32Array(4 * (NSTRMAX + 1) * (NDMAX + 1)),
    VREFL: new Float32Array((NSTRMAX + 1) * (NDMAX + 1)),
    PHINGE: new Float32Array(4 * (NSTRMAX + 1) * (NDMAX + 1)),

    ENC: new Float32Array(4 * (NVMAX + 1)),
    ENV: new Float32Array(4 * (NVMAX + 1)),
    ENC_D: new Float32Array(4 * (NVMAX + 1) * (NDMAX + 1)),
    ENV_D: new Float32Array(4 * (NVMAX + 1) * (NDMAX + 1)),
    ENC_G: new Float32Array(4 * (NVMAX + 1) * (NGMAX + 1)),
    ENV_G: new Float32Array(4 * (NVMAX + 1) * (NGMAX + 1)),
    LCONDEF: new Uint8Array(NDMAX + 1),
    LDESDEF: new Uint8Array(NGMAX + 1),

    ESS: new Float32Array(4 * (NSTRMAX + 1)),
    ENSY: new Float32Array(NSTRMAX + 1),
    ENSZ: new Float32Array(NSTRMAX + 1),
    XSREF: new Float32Array(NSTRMAX + 1),
    YSREF: new Float32Array(NSTRMAX + 1),
    ZSREF: new Float32Array(NSTRMAX + 1),
    LSTRIPOFF: new Uint8Array(NSTRMAX + 1),

    AICN: new Float32Array((NVMAX + 1) * (NVMAX + 1)),
    IAPIV: new Int32Array(NVMAX + 1),
    WORK: new Float32Array(NVMAX + 1),

    WC_GAM: new Float32Array(4 * (NVMAX + 1) * (NVMAX + 1)),
    WV_GAM: new Float32Array(4 * (NVMAX + 1) * (NVMAX + 1)),

    WV: new Float32Array(4 * (NVMAX + 1) * (NVMAX + 1)),
    WV_U: new Float32Array(4 * (NVMAX + 1) * (NUMAX + 1)),
    WV_D: new Float32Array(4 * (NVMAX + 1) * (NDMAX + 1)),
    WV_G: new Float32Array(4 * (NVMAX + 1) * (NGMAX + 1)),

    VV: new Float32Array(4 * (NVMAX + 1) * (NVMAX + 1)),
    VV_U: new Float32Array(4 * (NVMAX + 1) * (NUMAX + 1)),
    VV_D: new Float32Array(4 * (NVMAX + 1) * (NDMAX + 1)),
    VV_G: new Float32Array(4 * (NVMAX + 1) * (NGMAX + 1)),

    WCSRD: new Float32Array(4 * (NVMAX + 1) * (NVMAX + 1)),
    WCSRD_U: new Float32Array(4 * (NVMAX + 1) * (NUMAX + 1)),
    WVSRD: new Float32Array(4 * (NVMAX + 1) * (NVMAX + 1)),
    WVSRD_U: new Float32Array(4 * (NVMAX + 1) * (NUMAX + 1)),

    WC: new Float32Array(4 * (NVMAX + 1)),
    WC_U: new Float32Array(4 * (NVMAX + 1) * (NUMAX + 1)),
    WC_D: new Float32Array(4 * (NVMAX + 1) * (NDMAX + 1)),
    WC_G: new Float32Array(4 * (NVMAX + 1) * (NGMAX + 1)),

    SRC: new Float32Array(4 * (NVMAX + 1) * (NVMAX + 1)),
    SRC_U: new Float32Array(4 * (NVMAX + 1) * (NUMAX + 1)),
    DBL: new Float32Array(4 * (NVMAX + 1) * (NVMAX + 1)),
    DBL_U: new Float32Array(4 * (NVMAX + 1) * (NUMAX + 1)),

    GAM: new Float32Array((NVMAX + 1) * (NVMAX + 1)),
    GAM_U: new Float32Array((NVMAX + 1) * (NUMAX + 1)),
    GAM_D: new Float32Array((NVMAX + 1) * (NDMAX + 1)),
    GAM_G: new Float32Array((NVMAX + 1) * (NGMAX + 1)),
    GAM_U_0: new Float32Array((NVMAX + 1) * (NUMAX + 1)),
    GAM_U_D: new Float32Array((NVMAX + 1) * (NUMAX + 1) * (NDMAX + 1)),
    GAM_U_G: new Float32Array((NVMAX + 1) * (NUMAX + 1) * (NGMAX + 1)),

    DCP: new Float32Array((NVMAX + 1) * (NVMAX + 1)),
    DCPB: new Float32Array((NVMAX + 1) * (NVMAX + 1)),
    DCP_U: new Float32Array((NVMAX + 1) * (NUMAX + 1)),
    DCP_D: new Float32Array((NVMAX + 1) * (NDMAX + 1)),
    DCP_G: new Float32Array((NVMAX + 1) * (NGMAX + 1)),

    CFSTRP: new Float32Array(3 * (NSTRMAX + 1)),
    CMSTRP: new Float32Array(3 * (NSTRMAX + 1)),
    CFSURF: new Float32Array(3 * (NSURF + 1)),
    CMSURF: new Float32Array(3 * (NSURF + 1)),
    CFTOT: new Float32Array(3),
    CFTOT_U: new Float32Array(3 * (NUMAX + 1)),
    CFTOT_D: new Float32Array(3 * (NDMAX + 1)),
    CFTOT_G: new Float32Array(3 * (NGMAX + 1)),
    CMTOT: new Float32Array(3),
    CMTOT_U: new Float32Array(3 * (NUMAX + 1)),
    CMTOT_D: new Float32Array(3 * (NDMAX + 1)),
    CMTOT_G: new Float32Array(3 * (NGMAX + 1)),
    CLSTRP: new Float32Array(NSTRMAX + 1),
    CDSTRP: new Float32Array(NSTRMAX + 1),
    CYSTRP: new Float32Array(NSTRMAX + 1),
    CYST_A: new Float32Array(NSTRMAX + 1),
    CYST_U: new Float32Array((NSTRMAX + 1) * (NUMAX + 1)),
    CYST_D: new Float32Array((NSTRMAX + 1) * (NDMAX + 1)),
    CYST_G: new Float32Array((NSTRMAX + 1) * (NGMAX + 1)),
    CLST_A: new Float32Array(NSTRMAX + 1),
    CLST_U: new Float32Array((NSTRMAX + 1) * (NUMAX + 1)),
    CLST_D: new Float32Array((NSTRMAX + 1) * (NDMAX + 1)),
    CLST_G: new Float32Array((NSTRMAX + 1) * (NGMAX + 1)),
    CDST_A: new Float32Array(NSTRMAX + 1),
    CDST_U: new Float32Array((NSTRMAX + 1) * (NUMAX + 1)),
    CDST_D: new Float32Array((NSTRMAX + 1) * (NDMAX + 1)),
    CDST_G: new Float32Array((NSTRMAX + 1) * (NGMAX + 1)),
    CMST_U: new Float32Array(3 * (NSTRMAX + 1) * (NUMAX + 1)),
    CMST_D: new Float32Array(3 * (NSTRMAX + 1) * (NDMAX + 1)),
    CMST_G: new Float32Array(3 * (NSTRMAX + 1) * (NGMAX + 1)),
    CFST_U: new Float32Array(3 * (NSTRMAX + 1) * (NUMAX + 1)),
    CFST_D: new Float32Array(3 * (NSTRMAX + 1) * (NDMAX + 1)),
    CFST_G: new Float32Array(3 * (NSTRMAX + 1) * (NGMAX + 1)),

    CLSURF: new Float32Array(NSURF + 1),
    CDSURF: new Float32Array(NSURF + 1),
    CYSURF: new Float32Array(NSURF + 1),
    CLS_A: new Float32Array(NSURF + 1),
    CDS_A: new Float32Array(NSURF + 1),
    CYS_A: new Float32Array(NSURF + 1),
    CLS_U: new Float32Array((NSURF + 1) * (NUMAX + 1)),
    CLS_D: new Float32Array((NSURF + 1) * (NDMAX + 1)),
    CLS_G: new Float32Array((NSURF + 1) * (NGMAX + 1)),
    CDS_U: new Float32Array((NSURF + 1) * (NUMAX + 1)),
    CDS_D: new Float32Array((NSURF + 1) * (NDMAX + 1)),
    CDS_G: new Float32Array((NSURF + 1) * (NGMAX + 1)),
    CYS_U: new Float32Array((NSURF + 1) * (NUMAX + 1)),
    CYS_D: new Float32Array((NSURF + 1) * (NDMAX + 1)),
    CYS_G: new Float32Array((NSURF + 1) * (NGMAX + 1)),
    CFS_U: new Float32Array(3 * (NSURF + 1) * (NUMAX + 1)),
    CFS_D: new Float32Array(3 * (NSURF + 1) * (NDMAX + 1)),
    CFS_G: new Float32Array(3 * (NSURF + 1) * (NGMAX + 1)),
    CMS_U: new Float32Array(3 * (NSURF + 1) * (NUMAX + 1)),
    CMS_D: new Float32Array(3 * (NSURF + 1) * (NDMAX + 1)),
    CMS_G: new Float32Array(3 * (NSURF + 1) * (NGMAX + 1)),

    CLTOT: 0.0,
    CDTOT: 0.0,
    CYTOT: 0.0,
    CLTOT_A: 0.0,
    CDTOT_A: 0.0,
    CYTOT_A: 0.0,
    CLTOT_U: new Float32Array((NUMAX + 1)),
    CLTOT_D: new Float32Array((NDMAX + 1)),
    CLTOT_G: new Float32Array((NGMAX + 1)),
    CDTOT_U: new Float32Array((NUMAX + 1)),
    CDTOT_D: new Float32Array((NDMAX + 1)),
    CDTOT_G: new Float32Array((NGMAX + 1)),
    CYTOT_U: new Float32Array((NUMAX + 1)),
    CYTOT_D: new Float32Array((NDMAX + 1)),
    CYTOT_G: new Float32Array((NGMAX + 1)),

    CMLE_LSTRP: new Float32Array(NSTRMAX + 1),
    CMC4_LSTRP: new Float32Array(NSTRMAX + 1),
    CN_LSTRP: new Float32Array(NSTRMAX + 1),
    CLT_LSTRP: new Float32Array(NSTRMAX + 1),
    CLA_LSTRP: new Float32Array(NSTRMAX + 1),
    CD_LSTRP: new Float32Array(NSTRMAX + 1),
    CL_LSTRP: new Float32Array(NSTRMAX + 1),
    CD_LSRF: new Float32Array(NSURF + 1),
    CL_LSRF: new Float32Array(NSURF + 1),
    CA_LSTRP: new Float32Array(NSTRMAX + 1),
    CDV_LSTRP: new Float32Array(NSTRMAX + 1),
    CDVSURF: new Float32Array(NSURF + 1),
    CDVTOT: 0.0,

    CM_LSTRP: new Float32Array(3 * (NSTRMAX + 1)),
    CF_LSTRP: new Float32Array(3 * (NSTRMAX + 1)),
    CM_LSRF: new Float32Array(3 * (NSURF + 1)),
    CF_LSRF: new Float32Array(3 * (NSURF + 1)),

    CHINGE: new Float32Array(NDMAX + 1),
    CHINGE_U: new Float32Array((NDMAX + 1) * (NUMAX + 1)),
    CHINGE_D: new Float32Array((NDMAX + 1) * (NDMAX + 1)),
    CHINGE_G: new Float32Array((NDMAX + 1) * (NGMAX + 1)),

    VC: new Float32Array(4 * (NVMAX + 1)),
    VC_U: new Float32Array(4 * (NVMAX + 1) * (NUMAX + 1)),
    VC_D: new Float32Array(4 * (NVMAX + 1) * (NDMAX + 1)),
    VC_G: new Float32Array(4 * (NVMAX + 1) * (NGMAX + 1)),
    CNC: new Float32Array(NSTRMAX + 1),
    CNC_U: new Float32Array((NSTRMAX + 1) * (NUMAX + 1)),
    CNC_D: new Float32Array((NSTRMAX + 1) * (NDMAX + 1)),
    CNC_G: new Float32Array((NSTRMAX + 1) * (NGMAX + 1)),
  };

  if (opts.debug) {
    state.__debug = opts.debug;
  }

  const IR = 1;
  const idx2 = (i, j, dim1) => i + dim1 * j;
  state.PARVAL[idx2(IPMACH, IR, IPTOT)] = state.MACH;
  state.PARVAL[idx2(IPVEE, IR, IPTOT)] = Math.fround(opts.vel);
  state.PARVAL[idx2(IPRHO, IR, IPTOT)] = Math.fround(opts.rho);
  state.PARVAL[idx2(IPGEE, IR, IPTOT)] = Math.fround(opts.gee);
  state.PARVAL[idx2(IPCL, IR, IPTOT)] = Math.fround(opts.cl);
  state.PARVAL[idx2(IPPHI, IR, IPTOT)] = Math.fround(opts.bank);
  const xcg = opts.xcg ?? model.header.xref ?? 0.0;
  const ycg = opts.ycg ?? model.header.yref ?? 0.0;
  const zcg = opts.zcg ?? model.header.zref ?? 0.0;
  state.PARVAL[idx2(IPXCG, IR, IPTOT)] = Math.fround(xcg);
  state.PARVAL[idx2(IPYCG, IR, IPTOT)] = Math.fround(ycg);
  state.PARVAL[idx2(IPZCG, IR, IPTOT)] = Math.fround(zcg);
  state.PARVAL[idx2(IPCD0, IR, IPTOT)] = Math.fround(opts.cd0 ?? 0.0);
  state.ALFA = Math.fround(opts.alpha * state.DTR);
  state.BETA = Math.fround(opts.beta * state.DTR);

  state.CONVAL[idx2(ICCL, IR, ICMAX)] = Math.fround(opts.cl);
  state.CONVAL[idx2(ICMOMX, IR, ICMAX)] = Math.fround(opts.cmx);
  state.CONVAL[idx2(ICMOMY, IR, ICMAX)] = Math.fround(opts.cmy);
  state.CONVAL[idx2(ICMOMZ, IR, ICMAX)] = Math.fround(opts.cmz);
  state.CONVAL[idx2(ICBETA, IR, ICMAX)] = Math.fround(opts.beta);

  state.ICON[idx2(IVALFA, IR, IVMAX)] = ICCL;
  state.ICON[idx2(IVBETA, IR, IVMAX)] = ICBETA;
  state.ICON[idx2(IVROTX, IR, IVMAX)] = ICMOMX;
  state.ICON[idx2(IVROTY, IR, IVMAX)] = ICMOMY;
  state.ICON[idx2(IVROTZ, IR, IVMAX)] = ICMOMZ;
  for (let n = 1; n <= NDMAX; n += 1) {
    const iv = IVTOT + n;
    const ic = ICTOT + n;
    state.ICON[idx2(iv, IR, IVMAX)] = ic;
    state.CONVAL[idx2(ic, IR, ICMAX)] = 0.0;
  }

  for (let n = 1; n <= NCONTROL; n += 1) {
    state.LCONDEF[n] = 1;
  }
  for (let n = 1; n <= NDESIGN; n += 1) {
    state.LDESDEF[n] = 1;
  }

  return state;
}

export function buildGeometry(state, model) {
  state.NVOR = 0;
  state.NSTRIP = 0;
  let surfIndex = 0;
  model.surfaces.forEach((surf) => {
    surfIndex += 1;
    const isurf = surfIndex;
    const comp = (surf.component == null || surf.component === 0) ? isurf : surf.component;
    state.LNCOMP[isurf] = comp;
    state.LFWAKE[isurf] = surf.nowake ? 0 : 1;
    state.LFLOAD[isurf] = surf.noload ? 0 : 1;
    MAKESURF(state, isurf, surf);
    if (typeof surf.yduplicate === 'number') {
      surfIndex += 1;
      SDUPL(state, isurf, surf.yduplicate, surfIndex);
    }
  });
  state.NSURF = surfIndex;
  ENCALC(state);
}

export async function runExec(text, options = {}) {
  const model = await buildSolverModel(text, options);
  const state = buildExecState(model, options);
  buildGeometry(state, model);
  EXEC(state, 20, 0, 1);
  return { state, model };
}

export function repoRootDir() {
  const filePath = fileURLToPath(import.meta.url);
  return path.resolve(path.dirname(filePath), '..', '..');
}
