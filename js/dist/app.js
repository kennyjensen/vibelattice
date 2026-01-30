import { TRMSET_CORE } from './atrim.js';
import { EXEC } from './aoper.js';
import { MAKESURF, ENCALC } from './amake.js';

let THREE = null;
let OrbitControls = null;
let threeReady = false;

const els = {
  fileInput: document.getElementById('fileInput'),
  saveBtn: document.getElementById('saveBtn'),
  fileMeta: document.getElementById('fileMeta'),
  fileText: document.getElementById('fileText'),
  bank: document.getElementById('bank'),
  cl: document.getElementById('cl'),
  vel: document.getElementById('vel'),
  mass: document.getElementById('mass'),
  rho: document.getElementById('rho'),
  gee: document.getElementById('gee'),
  alpha: document.getElementById('alpha'),
  beta: document.getElementById('beta'),
  p: document.getElementById('p'),
  q: document.getElementById('q'),
  r: document.getElementById('r'),
  de: document.getElementById('de'),
  da: document.getElementById('da'),
  dr: document.getElementById('dr'),
  clc: document.getElementById('clc'),
  cmx: document.getElementById('cmx'),
  cmy: document.getElementById('cmy'),
  cmz: document.getElementById('cmz'),
  trimBtn: document.getElementById('trimBtn'),
  execToggle: document.getElementById('execToggle'),
  viewer: document.getElementById('viewer'),
  trefftz: document.getElementById('trefftz'),
  outAlpha: document.getElementById('outAlpha'),
  outBeta: document.getElementById('outBeta'),
  outBank: document.getElementById('outBank'),
  outCL: document.getElementById('outCL'),
  outCD: document.getElementById('outCD'),
  outCY: document.getElementById('outCY'),
  outV: document.getElementById('outV'),
  outRad: document.getElementById('outRad'),
  outFac: document.getElementById('outFac'),
  outThe: document.getElementById('outThe'),
  outCM: document.getElementById('outCM'),
  outRates: document.getElementById('outRates'),
  outDef: document.getElementById('outDef'),
  outStability: document.getElementById('outStability'),
  outBodyDeriv: document.getElementById('outBodyDeriv'),
  outForcesTotal: document.getElementById('outForcesTotal'),
  outForcesSurface: document.getElementById('outForcesSurface'),
  outForcesStrip: document.getElementById('outForcesStrip'),
  outForcesElement: document.getElementById('outForcesElement'),
  outForcesBody: document.getElementById('outForcesBody'),
  outHinge: document.getElementById('outHinge'),
  debugLog: document.getElementById('debugLog'),
  clearDebug: document.getElementById('clearDebug'),
  viewerPan: document.getElementById('viewerPan'),
  viewerZoomIn: document.getElementById('viewerZoomIn'),
  viewerZoomOut: document.getElementById('viewerZoomOut'),
  viewerHome: document.getElementById('viewerHome'),
  viewerView: document.getElementById('viewerView'),
  viewerGrid: document.getElementById('viewerGrid'),
};

const uiState = {
  filename: null,
  text: '',
  surfaceColors: [0xf59e0b, 0x7dd3fc, 0xf97316, 0xa78bfa, 0x22d3ee],
  trefftzData: null,
};

const viewerState = {
  mode: 'rotate',
  viewModes: ['left', 'right', 'top'],
  viewIndex: 0,
  gridModes: ['xy', 'yz', 'xz', 'none'],
  gridIndex: 0,
  bounds: null,
  fitDistance: 12,
};

let execInProgress = false;
let execWorker = null;
let execTimeoutId = null;

function applyYSymmetry(model) {
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

function applyZSymmetry(model) {
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

function applyYDuplicate(model) {
  if (!model || !Array.isArray(model.surfaces)) return model;
  const next = [];
  model.surfaces.forEach((surf) => {
    const ydup = surf.yduplicate;
    const base = { ...surf, yduplicate: null };
    next.push(base);
    if (typeof ydup !== 'number') return;
    const copy = JSON.parse(JSON.stringify(base));
    copy.name = `${surf.name}-ydup`;
    copy.sections.forEach((sec) => {
      sec.yle = 2 * ydup - sec.yle;
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

function ensureExecWorker() {
  if (execWorker) return execWorker;
  if (typeof Worker === 'undefined') return null;
  execWorker = new Worker(new URL('./exec-worker.js', import.meta.url), { type: 'module' });
  execWorker.onmessage = (evt) => {
    const msg = evt.data || {};
    if (msg.type === 'log') {
      logDebug(msg.message);
    } else if (msg.type === 'error') {
      logDebug(`EXEC worker error: ${msg.message}`);
      execInProgress = false;
      if (execTimeoutId) {
        clearTimeout(execTimeoutId);
        execTimeoutId = null;
      }
    } else if (msg.type === 'result') {
      execInProgress = false;
      if (execTimeoutId) {
        clearTimeout(execTimeoutId);
        execTimeoutId = null;
      }
      applyExecResults(msg);
    }
  };
  execWorker.onerror = (evt) => {
    logDebug(`EXEC worker failed: ${evt.message}`);
    execInProgress = false;
    if (execTimeoutId) {
      clearTimeout(execTimeoutId);
      execTimeoutId = null;
    }
  };
  return execWorker;
}

const airfoilCache = new Map();
const airfoilPending = new Map();
const airfoilFailed = new Set();

function fmt(value, digits = 3) {
  if (!Number.isFinite(value)) return '-';
  return Number(value).toFixed(digits);
}

function logDebug(message) {
  const line = `[${new Date().toLocaleTimeString()}] ${message}`;
  if (typeof window.__debugLog === 'function') {
    window.__debugLog(message);
    return;
  }
  if (els.debugLog) {
    els.debugLog.textContent += `${line}\n`;
    els.debugLog.scrollTop = els.debugLog.scrollHeight;
  }
  console.log(line);
}

window.addEventListener('error', (evt) => {
  logDebug(`Error: ${evt.message}`);
});
window.addEventListener('unhandledrejection', (evt) => {
  logDebug(`Promise rejection: ${evt.reason?.message ?? evt.reason}`);
});

async function ensureThree() {
  if (threeReady) return true;
  try {
    const mod = await import('three');
    THREE = mod;
    const controlsMod = await import('three/addons/controls/OrbitControls.js');
    OrbitControls = controlsMod.OrbitControls;
    threeReady = true;
    return true;
  } catch (err) {
    logDebug(`Three.js load failed: ${err?.message ?? err}`);
    threeReady = false;
    return false;
  }
}

function handleFileLoad(file) {
  const reader = new FileReader();
  reader.onload = () => {
    uiState.text = String(reader.result || '');
    uiState.filename = file.name;
    els.fileText.value = uiState.text;
    els.fileMeta.textContent = `Loaded: ${file.name} (${file.size} bytes)`;
    loadGeometryFromText(uiState.text, true);
    logDebug(`Loaded file: ${file.name}`);
  };
  reader.readAsText(file);
}

els.fileInput.addEventListener('change', (evt) => {
  const file = evt.target.files?.[0];
  if (file) handleFileLoad(file);
});

els.saveBtn.addEventListener('click', () => {
  const text = els.fileText.value;
  const blob = new Blob([text], { type: 'text/plain' });
  const a = document.createElement('a');
  const name = uiState.filename || 'model.avl';
  a.href = URL.createObjectURL(blob);
  a.download = name;
  a.click();
  URL.revokeObjectURL(a.href);
});

els.clearDebug?.addEventListener('click', () => {
  if (els.debugLog) els.debugLog.textContent = '';
});

els.viewerPan?.addEventListener('click', () => {
  setControlMode('pan');
});
els.viewerZoomIn?.addEventListener('click', () => {
  if (!controls) return;
  if (typeof controls.dollyIn === 'function') {
    controls.dollyIn(1.2);
  } else if (camera) {
    camera.position.multiplyScalar(0.9);
  }
  controls.update();
});
els.viewerZoomOut?.addEventListener('click', () => {
  if (!controls) return;
  if (typeof controls.dollyOut === 'function') {
    controls.dollyOut(1.2);
  } else if (camera) {
    camera.position.multiplyScalar(1.1);
  }
  controls.update();
});
els.viewerHome?.addEventListener('click', () => {
  fitCameraToObject(aircraft);
});
els.viewerView?.addEventListener('click', () => {
  viewerState.viewIndex = (viewerState.viewIndex + 1) % viewerState.viewModes.length;
  const mode = viewerState.viewModes[viewerState.viewIndex];
  applyViewPreset(mode);
  updateViewerButtons();
});
els.viewerGrid?.addEventListener('click', () => {
  viewerState.gridIndex = (viewerState.gridIndex + 1) % viewerState.gridModes.length;
  applyGridMode();
  updateViewerButtons();
});

let fileUpdateTimer = null;
els.fileText.addEventListener('input', () => {
  clearTimeout(fileUpdateTimer);
  fileUpdateTimer = setTimeout(() => {
    uiState.text = els.fileText.value;
    loadGeometryFromText(uiState.text, true);
  }, 250);
});

function makeTrimState() {
  const state = {
    IPTOT: 30,
    IVTOT: 5,
    ICMAX: 10,
    NVTOT: 5,
    NRMAX: 1,
    IVALFA: 1,
    IVROTX: 3,
    IVROTY: 4,
    IVROTZ: 5,
    ICCL: 6,
    ICROTX: 3,
    ICROTY: 4,
    ICROTZ: 5,
    IPCL: 6,
    IPPHI: 8,
    IPTHE: 9,
    IPVEE: 12,
    IPRHO: 13,
    IPGEE: 14,
    IPRAD: 15,
    IPFAC: 16,
    IPMASS: 20,
    UNITL: 1.0,
    SREF: 1.5,
    CREF: 0.75,
    BREF: 2.0,
    DTR: Math.PI / 180.0,
    RHO0: Number(els.rho.value),
    GEE0: Number(els.gee.value),
    RMASS0: Number(els.mass.value),
    ITRIM: new Int32Array(2),
    PARVAL: new Float32Array((30 + 1) * (2 + 1)),
    ICON: new Int32Array((5 + 1) * (2 + 1)),
    CONVAL: new Float32Array((10 + 1) * (2 + 1)),
  };

  const { IVALFA, IVROTX, IVROTY, IVROTZ, ICCL, ICROTX, ICROTY, ICROTZ } = state;
  const { IPPHI, IPTHE, IPVEE, IPRHO, IPGEE, IPRAD, IPFAC, IPMASS, IPCL } = state;

  const IR = 1;
  const idx2 = (i, j, dim1) => i + dim1 * j;

  state.PARVAL[idx2(IPPHI, IR, state.IPTOT)] = Number(els.bank.value);
  state.PARVAL[idx2(IPTHE, IR, state.IPTOT)] = 0.0;
  state.PARVAL[idx2(IPVEE, IR, state.IPTOT)] = Number(els.vel.value);
  state.PARVAL[idx2(IPRHO, IR, state.IPTOT)] = Number(els.rho.value);
  state.PARVAL[idx2(IPGEE, IR, state.IPTOT)] = Number(els.gee.value);
  state.PARVAL[idx2(IPMASS, IR, state.IPTOT)] = Number(els.mass.value);
  state.PARVAL[idx2(IPCL, IR, state.IPTOT)] = Number(els.cl.value);
  state.PARVAL[idx2(IPRAD, IR, state.IPTOT)] = 0.0;
  state.PARVAL[idx2(IPFAC, IR, state.IPTOT)] = 0.0;

  state.CONVAL[idx2(ICCL, IR, state.ICMAX)] = Number(els.clc.value);
  state.CONVAL[idx2(ICROTX, IR, state.ICMAX)] = Number(els.p.value);
  state.CONVAL[idx2(ICROTY, IR, state.ICMAX)] = Number(els.q.value);
  state.CONVAL[idx2(ICROTZ, IR, state.ICMAX)] = Number(els.r.value);

  state.ICON[idx2(IVALFA, IR, state.IVTOT)] = ICCL;
  state.ICON[idx2(IVROTX, IR, state.IVTOT)] = ICROTX;
  state.ICON[idx2(IVROTY, IR, state.IVTOT)] = ICROTY;
  state.ICON[idx2(IVROTZ, IR, state.IVTOT)] = ICROTZ;

  return state;
}

function applyTrim() {
  logDebug('Trim requested.');
  let state;
  const IR = 1;
  try {
    state = makeTrimState();
    TRMSET_CORE(state, 1, IR, IR, IR);
  } catch (err) {
    logDebug(`Trim setup failed: ${err?.message ?? err}`);
    return;
  }

  const idx2 = (i, j, dim1) => i + dim1 * j;
  const IPPHI = 8;
  const IPTHE = 9;
  const IPVEE = 12;
  const IPRAD = 15;
  const IPFAC = 16;
  const IPCL = 6;

  const phi = state.PARVAL[idx2(IPPHI, IR, state.IPTOT)];
  const the = state.PARVAL[idx2(IPTHE, IR, state.IPTOT)];
  const vee = state.PARVAL[idx2(IPVEE, IR, state.IPTOT)];
  const rad = state.PARVAL[idx2(IPRAD, IR, state.IPTOT)];
  const fac = state.PARVAL[idx2(IPFAC, IR, state.IPTOT)];
  const cl = state.PARVAL[idx2(IPCL, IR, state.IPTOT)];

  els.outAlpha.textContent = `${fmt(Number(els.alpha.value), 2)} deg`;
  els.outBeta.textContent = `${fmt(Number(els.beta.value), 2)} deg`;
  els.outBank.textContent = `${fmt(phi, 2)} deg`;
  els.outCL.textContent = fmt(cl, 3);
  els.outV.textContent = `${fmt(vee, 2)} m/s`;
  els.outRad.textContent = rad > 0 ? `${fmt(rad, 2)} m` : 'level';
  els.outFac.textContent = fmt(fac, 3);
  els.outThe.textContent = `${fmt(the, 2)} deg`;
  els.outRates.textContent = `${fmt(Number(els.p.value), 2)}, ${fmt(Number(els.q.value), 2)}, ${fmt(Number(els.r.value), 2)}`;
  els.outDef.textContent = `de ${fmt(Number(els.de.value), 1)} / da ${fmt(Number(els.da.value), 1)} / dr ${fmt(Number(els.dr.value), 1)}`;

  try {
    updateTrefftz(cl);
    updateBank(phi);
  } catch (err) {
    logDebug(`Trim render failed: ${err?.message ?? err}`);
  }

  logDebug('Trim base outputs updated.');

  // Full EXEC solve based on loaded AVL file
  if (uiState.text.trim() && els.execToggle?.checked) {
    setTimeout(() => {
      if (execInProgress) {
        logDebug('EXEC skipped: already running.');
        return;
      }
      execInProgress = true;
      logDebug('EXEC scheduled.');
      try {
        runExecFromText(uiState.text);
      } catch (err) {
        logDebug(`EXEC failed: ${err?.message ?? err}`);
        execInProgress = false;
      }
    }, 0);
  }
}

els.trimBtn.addEventListener('click', applyTrim);

function updateTrefftz(cl) {
  const canvas = els.trefftz;
  const ctx = canvas.getContext('2d');
  const w = canvas.width;
  const h = canvas.height;
  ctx.clearRect(0, 0, w, h);

  ctx.fillStyle = '#0b0f17';
  ctx.fillRect(0, 0, w, h);

  ctx.strokeStyle = 'rgba(255,255,255,0.1)';
  ctx.lineWidth = 1;
  for (let i = 1; i < 6; i += 1) {
    const y = (h / 6) * i;
    ctx.beginPath();
    ctx.moveTo(0, y);
    ctx.lineTo(w, y);
    ctx.stroke();
  }

  if (uiState.trefftzData && uiState.trefftzData.strips?.length) {
    const { strips, surfaces, cref } = uiState.trefftzData;
    let ymin = Infinity;
    let ymax = -Infinity;
    let fmin = 0.0;
    let fmax = 0.0;
    let cmin = 0.0;
    let cmax = 0.0;
    let wmin = 0.0;
    let wmax = 0.0;
    let wmin8 = 0.0;
    let wmax8 = 0.0;

    strips.forEach(([y, _z, cnc, cl, clPerp, dw]) => {
      if (y < ymin) ymin = y;
      if (y > ymax) ymax = y;
      fmin = Math.min(fmin, cnc, 0.0);
      fmax = Math.max(fmax, cnc, 0.0);
      cmin = Math.min(cmin, clPerp, cl, 0.0);
      cmax = Math.max(cmax, clPerp, cl, 0.0);
      wmin8 += Math.pow(Math.min(-dw, 0.0), 8);
      wmax8 += Math.pow(Math.max(-dw, 0.0), 8);
    });

    if (wmin8 !== 0) wmin = -Math.pow(Math.abs(wmin8) / strips.length, 0.125);
    if (wmax8 !== 0) wmax = Math.pow(Math.abs(wmax8) / strips.length, 0.125);
    cmin = Math.min(fmin / (cref || 1.0), cmin);
    cmax = Math.max(fmax / (cref || 1.0), cmax);

    if (!Number.isFinite(ymin) || !Number.isFinite(ymax) || ymin === ymax) {
      ymin = -1;
      ymax = 1;
    }
    if (!Number.isFinite(cmin) || !Number.isFinite(cmax) || Math.abs(cmax - cmin) < 1e-5) {
      cmin = 0.0;
      cmax = 0.1;
    }
    if (!Number.isFinite(wmin) || !Number.isFinite(wmax) || Math.abs(wmax - wmin) < 1e-5) {
      wmin = 0.0;
      wmax = 0.1;
    }

    const pad = 0.12;
    const plotW = w * (1 - 2 * pad);
    const plotH = h * (1 - 2 * pad);
    const x0 = w * pad;
    const y0 = h * pad;

    const xFor = (y) => x0 + ((y - ymin) / (ymax - ymin)) * plotW;
    const yForC = (val) => y0 + (1 - (val - cmin) / (cmax - cmin)) * plotH;
    const yForW = (val) => y0 + (1 - (val - wmin) / (wmax - wmin)) * plotH;

    ctx.strokeStyle = 'rgba(255,255,255,0.1)';
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(x0, y0);
    ctx.lineTo(x0, y0 + plotH);
    ctx.lineTo(x0 + plotW, y0 + plotH);
    ctx.stroke();

    ctx.strokeStyle = 'rgba(255,255,255,0.08)';
    ctx.beginPath();
    ctx.moveTo(x0 + plotW, y0);
    ctx.lineTo(x0 + plotW, y0 + plotH);
    ctx.stroke();

    const drawSeries = (color, idxValue, scaleFn) => {
      ctx.strokeStyle = color;
      ctx.lineWidth = 2;
      surfaces.forEach((surf) => {
        ctx.beginPath();
        for (let i = 0; i < surf.count; i += 1) {
          const entry = strips[surf.start + i];
          const x = xFor(entry[0]);
          const y = scaleFn(entry[idxValue]);
          if (i === 0) ctx.moveTo(x, y);
          else ctx.lineTo(x, y);
        }
        ctx.stroke();
      });
    };

    drawSeries('#22c55e', 2, (v) => yForC(v / (cref || 1.0))); // CNC/Cref
    drawSeries('#fb923c', 3, yForC); // CL (freestream)
    drawSeries('#ef4444', 4, yForC); // CL_perp
    drawSeries('#3b82f6', 5, (v) => yForW(-v)); // downwash (alpha_i)

    ctx.fillStyle = '#9aa8b7';
    ctx.font = '12px JetBrains Mono, monospace';
    ctx.fillText('Trefftz Plane', 16, 24);
    ctx.fillText('Y (span)', x0 + plotW - 60, y0 + plotH + 18);
    ctx.fillText('CNC/Cref, CL', 16, y0 + 12);
    ctx.fillText('alpha_i', x0 + plotW - 48, y0 + 12);
  } else {
    const span = w * 0.8;
    const center = w * 0.1;
    const base = h * 0.75;
    const scale = Math.max(0.2, Math.min(1.2, cl));

    ctx.beginPath();
    for (let i = 0; i <= 120; i += 1) {
      const t = (i / 120) * 2 - 1;
      const y = base - (1 - t * t) * (h * 0.45) * scale;
      const x = center + (t + 1) * (span / 2);
      if (i === 0) ctx.moveTo(x, y);
      else ctx.lineTo(x, y);
    }
    ctx.strokeStyle = '#2dd4bf';
    ctx.lineWidth = 3;
    ctx.shadowColor = 'rgba(45,212,191,0.45)';
    ctx.shadowBlur = 10;
    ctx.stroke();
    ctx.shadowBlur = 0;

    ctx.fillStyle = '#9aa8b7';
    ctx.font = '12px JetBrains Mono, monospace';
    ctx.fillText(`CL ${fmt(cl, 2)}`, 16, 24);
  }
}

function parseAVL(text) {
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
    while (i < lines.length && isComment(lines[i])) i += 1;
    if (i >= lines.length) return null;
    return lines[i++].trim();
  };

  const parseNumbers = (line) => {
    if (!line) return [];
    return line.split(/\s+/).map((v) => Number(v)).filter((v) => Number.isFinite(v));
  };

  const readValueLine = () => parseNumbers(nextLine());

  const isKeywordLine = (line) => {
    const t = line.trim().toUpperCase();
    if (!t) return false;
    const key = t.slice(0, 4);
    return ['SURF', 'SECT', 'YDUP', 'SCAL', 'TRAN', 'ANGL', 'NACA', 'AFIL', 'AIRF', 'CONT', 'BODY'].includes(key);
  };

  const readAirfoilBlock = () => {
    const coords = [];
    while (i < lines.length) {
      const raw = lines[i];
      if (!raw) {
        i += 1;
        continue;
      }
      const trimmed = raw.trim();
      if (!trimmed) {
        i += 1;
        if (coords.length) break;
        continue;
      }
      if (isKeywordLine(trimmed)) break;
      i += 1;
      if (isComment(trimmed)) continue;
      const nums = parseNumbers(trimmed);
      if (nums.length >= 2) coords.push([nums[0], nums[1]]);
    }
    return coords;
  };

  const readHeaderValueLine = () => {
    const line = nextLine();
    if (!line) return [];
    return parseNumbers(line);
  };

  const parseHeader = () => {
    const titleLine = nextLine();
    if (titleLine) header.title = titleLine.trim();
    const machLine = readHeaderValueLine();
    if (machLine.length) header.mach = machLine[0];
    const symLine = readHeaderValueLine();
    if (symLine.length >= 3) {
      header.iysym = symLine[0];
      header.izsym = symLine[1];
      header.zsym = symLine[2];
    }
    const refLine = readHeaderValueLine();
    if (refLine.length >= 3) {
      header.sref = refLine[0];
      header.cref = refLine[1];
      header.bref = refLine[2];
    }
    const xyzLine = readHeaderValueLine();
    if (xyzLine.length >= 3) {
      header.xref = xyzLine[0];
      header.yref = xyzLine[1];
      header.zref = xyzLine[2];
    }
  };

  parseHeader();

  while (i < lines.length) {
    const line = nextLine();
    if (!line) break;
    const key = line.slice(0, 4).toUpperCase();
    if (key !== 'SURF') continue;

    const surfaceName = nextLine() || 'Surface';
    const spacing = readValueLine();
    const surface = {
      name: surfaceName,
      nChord: spacing[0] ?? 0,
      cSpace: spacing[1] ?? 1,
      nSpan: spacing[2] ?? 0,
      sSpace: spacing[3] ?? 1,
      sections: [],
      yduplicate: null,
      scale: [1, 1, 1],
      translate: [0, 0, 0],
      angle: [0, 0, 0],
      angleDeg: 0.0,
      component: 0,
      lvalbe: true,
    };

    let currentSection = null;
    while (i < lines.length) {
      const mark = lines[i] ?? '';
      const trimmed = mark.trim();
      if (!trimmed || isComment(trimmed)) {
        i += 1;
        continue;
      }
      const subkey = trimmed.slice(0, 4).toUpperCase();
      if (subkey === 'SURF' || subkey === 'BODY') break;
      i += 1;

      if (subkey === 'COMP') {
        const nums = parseNumbers(trimmed.slice(4));
        const vals = nums.length ? nums : readValueLine();
        surface.component = vals[0] ?? surface.component;
      } else if (subkey === 'YDUP') {
        const nums = parseNumbers(trimmed.slice(4));
        const vals = nums.length ? nums : readValueLine();
        surface.yduplicate = vals[0] ?? surface.yduplicate;
      } else if (subkey === 'SCAL') {
        const nums = parseNumbers(trimmed.slice(4));
        const vals = nums.length ? nums : readValueLine();
        if (vals.length >= 3) surface.scale = vals.slice(0, 3);
      } else if (subkey === 'TRAN') {
        const nums = parseNumbers(trimmed.slice(4));
        const vals = nums.length ? nums : readValueLine();
        if (vals.length >= 3) surface.translate = vals.slice(0, 3);
      } else if (subkey === 'ANGL') {
        const nums = parseNumbers(trimmed.slice(4));
        const vals = nums.length ? nums : readValueLine();
        if (vals.length >= 1) surface.angleDeg = vals[0];
        if (vals.length >= 3) surface.angle = vals.slice(0, 3);
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
          const path = trimmed.slice(4).trim() || (nextLine() || '');
          currentSection.airfoilFile = path || null;
          if (path) inlineAirfoils.push(path);
        }
      } else if (subkey === 'AIRF') {
        if (currentSection) {
          currentSection.airfoilCoords = readAirfoilBlock();
        }
      } else if (subkey === 'CONT') {
        if (currentSection) {
          const parts = trimmed.split(/\s+/).slice(1);
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

function parseAirfoilText(text) {
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

function buildCamberSlope(coords, samples = 60) {
  const { upper, lower } = splitAirfoilSurfaces(coords);
  if (!upper.length || !lower.length) return null;
  const xs = [];
  const camber = [];
  for (let i = 0; i <= samples; i += 1) {
    const x = i / samples;
    const zu = interpY(upper, x);
    const zl = interpY(lower, x);
    xs.push(x);
    camber.push(0.5 * (zu + zl));
  }
  const slope = [];
  for (let i = 0; i < xs.length; i += 1) {
    if (i === 0) {
      slope.push((camber[1] - camber[0]) / (xs[1] - xs[0] || 1e-6));
    } else if (i === xs.length - 1) {
      slope.push((camber[i] - camber[i - 1]) / (xs[i] - xs[i - 1] || 1e-6));
    } else {
      slope.push((camber[i + 1] - camber[i - 1]) / (xs[i + 1] - xs[i - 1] || 1e-6));
    }
  }
  return { x: xs, s: slope, t: camber.map((_, i) => 0.0) };
}

function buildNacaSlope(code, samples = 60) {
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

function fetchAirfoil(path) {
  if (!path || airfoilCache.has(path) || airfoilPending.has(path) || airfoilFailed.has(path)) return;
  const candidates = [
    new URL(path, window.location.href).toString(),
    new URL(`../third_party/avl/runs/${path}`, window.location.href).toString(),
    new URL(`../../third_party/avl/runs/${path}`, window.location.href).toString(),
  ];
  const promise = (async () => {
    for (const url of candidates) {
      try {
        const res = await fetch(url);
        if (!res.ok) continue;
        const text = await res.text();
        const coords = parseAirfoilText(text);
        if (coords.length) {
          airfoilCache.set(path, coords);
          return;
        }
      } catch {
        // try next
      }
    }
    airfoilFailed.add(path);
  })()
    .finally(() => {
      airfoilPending.delete(path);
      loadGeometryFromText(uiState.text, false);
    });
  airfoilPending.set(path, promise);
}

function nacaCamber(naca, x) {
  const digits = String(naca || '').padStart(4, '0');
  const m = Number(digits[0]) / 100;
  const p = Number(digits[1]) / 10;
  if (!m || !p) return 0;
  if (x < p) {
    return (m / (p * p)) * (2 * p * x - x * x);
  }
  return (m / ((1 - p) * (1 - p))) * ((1 - 2 * p) + 2 * p * x - x * x);
}

function buildSolverModel(text) {
  const model = parseAVL(text);
  const controlMap = new Map();
  const surfaces = [];
  model.surfaces.forEach((surf) => {
    const baseIndex = surfaces.length + 1;
    if (surf.component == null || surf.component === 0) {
      surf.component = baseIndex;
    }
    if (surf.imags == null) {
      surf.imags = 1;
    }
    surfaces.push(surf);
    surf.sections.forEach((sec) => {
      sec.controls.forEach((ctrl) => {
        if (!controlMap.has(ctrl.name)) {
          controlMap.set(ctrl.name, controlMap.size + 1);
        }
        ctrl.index = controlMap.get(ctrl.name);
      });
      let coords = sec.airfoilCoords;
      if (!coords && sec.airfoilFile) coords = airfoilCache.get(sec.airfoilFile) || null;
      if (coords && coords.length) {
        sec.airfoilCamber = buildCamberSlope(coords);
      } else if (sec.naca) {
        sec.airfoilCamber = buildNacaSlope(sec.naca);
      } else {
        sec.airfoilCamber = buildNacaSlope('0000');
      }
      sec.claf = 1.0;
    });
    if (typeof surf.yduplicate === 'number') {
      const mirror = JSON.parse(JSON.stringify(surf));
      mirror.name = `${surf.name}-dup`;
      mirror.component = surf.component;
      mirror.imags = -1;
      mirror.sections.forEach((sec) => {
        sec.yle = 2 * surf.yduplicate - sec.yle;
        sec.controls.forEach((ctrl) => {
          ctrl.gain *= ctrl.sgnDup ?? 1.0;
          if (ctrl.vhinge) ctrl.vhinge[1] *= -1;
        });
      });
      surfaces.push(mirror);
    }
  });
  model.surfaces = surfaces;
  model.controlMap = controlMap;
  return model;
}

function buildSurfaceMesh(surface, color) {
  const group = new THREE.Group();
  if (surface.sections.length < 2) return group;

  const verts = [];
  const controlVerts = new Map();
  const camberLines = [];
  const airfoilOutlines = [];
  const pushQuad = (le1, le2, te2, te1) => {
    verts.push(...le1, ...le2, ...te2, ...le1, ...te2, ...te1);
  };

  const applyTransforms = (p) => {
    const v = new THREE.Vector3(p[0], p[1], p[2]);
    v.multiply(new THREE.Vector3(...surface.scale));
    v.add(new THREE.Vector3(...surface.translate));
    return [v.x, v.y, v.z];
  };

  const rotateSectionPoint = (section, point) => {
    if (!section.ainc) return point;
    const le = new THREE.Vector3(section.xle, section.yle, section.zle);
    const v = new THREE.Vector3(point[0], point[1], point[2]).sub(le);
    v.applyAxisAngle(new THREE.Vector3(0, 1, 0), THREE.MathUtils.degToRad(section.ainc));
    v.add(le);
    return [v.x, v.y, v.z];
  };

  const addControlQuad = (name, le1, le2, te2, te1) => {
    if (!controlVerts.has(name)) controlVerts.set(name, []);
    const list = controlVerts.get(name);
    list.push(...le1, ...le2, ...te2, ...le1, ...te2, ...te1);
  };

  const addCamberLine = (section, ydup) => {
    const points = [];
    const steps = 32;
    for (let i = 0; i <= steps; i += 1) {
      const x = i / steps;
      const z = nacaCamber(section.naca, x);
      const xr = x * section.chord;
      const zr = z * section.chord;
      const raw = [section.xle + xr, section.yle, section.zle + zr];
      const rotated = rotateSectionPoint(section, raw);
      const final = applyTransforms(rotated);
      points.push(final);
    }
    if (ydup !== null && ydup !== undefined) {
      const mirrored = points.map((p) => [p[0], 2 * ydup - p[1], p[2]]);
      camberLines.push(mirrored);
    }
    camberLines.push(points);
  };

  const addAirfoilOutline = (section, coords, ydup) => {
    if (!coords || !coords.length) return;
    const points = coords.map(([x, z]) => {
      const xr = x * section.chord;
      const zr = z * section.chord;
      const raw = [section.xle + xr, section.yle, section.zle + zr];
      const rotated = rotateSectionPoint(section, raw);
      return applyTransforms(rotated);
    });
    if (ydup !== null && ydup !== undefined) {
      const mirrored = points.map((p) => [p[0], 2 * ydup - p[1], p[2]]);
      airfoilOutlines.push(mirrored);
    }
    airfoilOutlines.push(points);
  };

  for (let s = 0; s < surface.sections.length - 1; s += 1) {
    const a = surface.sections[s];
    const b = surface.sections[s + 1];
    const le1 = applyTransforms([a.xle, a.yle, a.zle]);
    const le2 = applyTransforms([b.xle, b.yle, b.zle]);
    const te1 = applyTransforms(rotateSectionPoint(a, [a.xle + a.chord, a.yle, a.zle]));
    const te2 = applyTransforms(rotateSectionPoint(b, [b.xle + b.chord, b.yle, b.zle]));
    pushQuad(le1, le2, te2, te1);

    if (typeof surface.yduplicate === 'number') {
      const mirror = (p) => [p[0], 2 * surface.yduplicate - p[1], p[2]];
      pushQuad(mirror(le1), mirror(le2), mirror(te2), mirror(te1));
    }

    const controlsA = a.controls || [];
    const controlsB = b.controls || [];
    controlsA.forEach((ctrl) => {
      const mate = controlsB.find((c) => c.name === ctrl.name) || ctrl;
      const hx1 = a.xle + a.chord * (ctrl.xhinge ?? 0.75);
      const hx2 = b.xle + b.chord * (mate.xhinge ?? 0.75);
      const h1 = applyTransforms(rotateSectionPoint(a, [hx1, a.yle, a.zle]));
      const h2 = applyTransforms(rotateSectionPoint(b, [hx2, b.yle, b.zle]));
      addControlQuad(ctrl.name, h1, h2, te2, te1);
      if (typeof surface.yduplicate === 'number') {
        const mirror = (p) => [p[0], 2 * surface.yduplicate - p[1], p[2]];
        addControlQuad(ctrl.name, mirror(h1), mirror(h2), mirror(te2), mirror(te1));
      }
    });
  }

  surface.sections.forEach((section) => {
    let coords = section.airfoilCoords;
    if (!coords && section.airfoilFile) {
      coords = airfoilCache.get(section.airfoilFile) || null;
      if (!coords) fetchAirfoil(section.airfoilFile);
    }
    if (coords && coords.length) {
      addAirfoilOutline(section, coords, surface.yduplicate);
    } else {
      addCamberLine(section, surface.yduplicate);
    }
  });

  const geometry = new THREE.BufferGeometry();
  geometry.setAttribute('position', new THREE.Float32BufferAttribute(verts, 3));
  geometry.computeVertexNormals();
  const material = new THREE.MeshStandardMaterial({ color, roughness: 0.45, metalness: 0.05, side: THREE.DoubleSide });
  const mesh = new THREE.Mesh(geometry, material);
  const wire = new THREE.LineSegments(new THREE.WireframeGeometry(geometry), new THREE.LineBasicMaterial({ color: 0x1f2937 }));
  group.add(mesh, wire);

  controlVerts.forEach((ctrlVerts, name) => {
    if (!ctrlVerts.length) return;
    const ctrlGeom = new THREE.BufferGeometry();
    ctrlGeom.setAttribute('position', new THREE.Float32BufferAttribute(ctrlVerts, 3));
    ctrlGeom.computeVertexNormals();
    const ctrlMat = new THREE.MeshStandardMaterial({ color: 0xef4444, roughness: 0.35, metalness: 0.1, side: THREE.DoubleSide, opacity: 0.8, transparent: true });
    const ctrlMesh = new THREE.Mesh(ctrlGeom, ctrlMat);
    const ctrlWire = new THREE.LineSegments(new THREE.WireframeGeometry(ctrlGeom), new THREE.LineBasicMaterial({ color: 0x7f1d1d }));
    ctrlMesh.name = `control:${name}`;
    group.add(ctrlMesh, ctrlWire);
  });

  camberLines.forEach((line) => {
    const flat = line.flat();
    const camGeom = new THREE.BufferGeometry();
    camGeom.setAttribute('position', new THREE.Float32BufferAttribute(flat, 3));
    const camLine = new THREE.Line(camGeom, new THREE.LineBasicMaterial({ color: 0x22d3ee }));
    group.add(camLine);
  });

  airfoilOutlines.forEach((line) => {
    const flat = line.flat();
    const foilGeom = new THREE.BufferGeometry();
    foilGeom.setAttribute('position', new THREE.Float32BufferAttribute(flat, 3));
    const foilLine = new THREE.Line(foilGeom, new THREE.LineBasicMaterial({ color: 0x34d399 }));
    group.add(foilLine);
  });

  return group;
}

function buildAircraftFromAVL(model) {
  const group = new THREE.Group();
  if (!model.surfaces.length) return buildPlaceholderAircraft();
  model.surfaces.forEach((surface, idx) => {
    const color = uiState.surfaceColors[idx % uiState.surfaceColors.length];
    const surfGroup = buildSurfaceMesh(surface, color);
    group.add(surfGroup);
  });
  if (model.header?.izsym === 1) {
    const zsym = Number(model.header.zsym ?? 0);
    const mirrored = group.clone(true);
    mirrored.scale.z = -1;
    mirrored.position.z = 2 * zsym;
    group.add(mirrored);
  }
  return group;
}

let scene;
let camera;
let renderer;
let controls;
let aircraft;
let gridHelper;
let axesHelper;

function updateViewerButtons() {
  if (!els.viewerPan || !els.viewerView || !els.viewerGrid) return;
  els.viewerPan.classList.toggle('active', viewerState.mode === 'pan');
  const viewLabel = viewerState.viewModes[viewerState.viewIndex] || 'left';
  const gridLabel = viewerState.gridModes[viewerState.gridIndex] || 'xy';
  els.viewerView.title = `View: ${viewLabel.toUpperCase()}`;
  els.viewerGrid.title = `Grid: ${gridLabel.toUpperCase()}`;
}

function setControlMode(mode) {
  if (!controls || !THREE) return;
  const next = viewerState.mode === mode ? 'rotate' : mode;
  viewerState.mode = next;
  controls.enableRotate = true;
  controls.enablePan = true;
  controls.enableZoom = true;
  const mouse = controls.mouseButtons;
  const touch = controls.touches;
  if (next === 'pan') {
    mouse.LEFT = THREE.MOUSE.PAN;
    touch.ONE = THREE.TOUCH.PAN ?? THREE.TOUCH.DOLLY_PAN ?? THREE.TOUCH.ROTATE;
  } else {
    mouse.LEFT = THREE.MOUSE.ROTATE;
    touch.ONE = THREE.TOUCH.ROTATE ?? THREE.TOUCH.DOLLY_PAN;
  }
  updateViewerButtons();
}

function computeBounds(obj) {
  if (!THREE || !obj) return null;
  const box = new THREE.Box3().setFromObject(obj);
  if (box.isEmpty()) return null;
  const size = new THREE.Vector3();
  const center = new THREE.Vector3();
  box.getSize(size);
  box.getCenter(center);
  const maxDim = Math.max(size.x, size.y, size.z);
  return { box, size, center, maxDim };
}

function axisSizeFromMax(maxDim) {
  if (!Number.isFinite(maxDim) || maxDim <= 0) return 1;
  const target = maxDim / 10;
  const exp = Math.floor(Math.log10(target));
  const base = Math.pow(10, exp);
  const steps = [1, 2, 5];
  let best = base;
  for (const step of steps) {
    const val = step * base;
    if (val <= target) best = val;
  }
  return best;
}

function rebuildGrid(size) {
  if (!scene || !THREE) return;
  if (gridHelper) scene.remove(gridHelper);
  const gridSize = Math.max(2, size * 1.2);
  const divisions = Math.max(8, Math.min(48, Math.round(gridSize)));
  gridHelper = new THREE.GridHelper(gridSize, divisions, 0x1f2a3a, 0x111827);
  scene.add(gridHelper);
  if (axesHelper) scene.remove(axesHelper);
  const axisSize = axisSizeFromMax(size);
  axesHelper = new THREE.AxesHelper(axisSize);
  scene.add(axesHelper);
}

function applyGridMode() {
  if (!gridHelper) return;
  const mode = viewerState.gridModes[viewerState.gridIndex] || 'xy';
  gridHelper.visible = mode !== 'none';
  if (axesHelper) axesHelper.visible = mode !== 'none';
  gridHelper.rotation.set(0, 0, 0);
  gridHelper.position.set(0, 0, 0);
  if (mode === 'xy') {
    gridHelper.rotation.x = Math.PI / 2;
  } else if (mode === 'yz') {
    gridHelper.rotation.z = Math.PI / 2;
  } else if (mode === 'xz') {
    gridHelper.rotation.x = 0;
  }
}

function setCameraUp(vec) {
  if (!camera) return;
  camera.up.set(vec.x, vec.y, vec.z);
}

function applyViewPreset(mode) {
  if (!camera || !controls || !viewerState.bounds) return;
  const dist = viewerState.fitDistance || 10;
  const lift = dist * 0.15;
  if (mode === 'left') {
    setCameraUp(new THREE.Vector3(0, 0, 1));
    camera.position.set(0, -dist, lift);
  } else if (mode === 'right') {
    setCameraUp(new THREE.Vector3(0, 0, 1));
    camera.position.set(0, dist, lift);
  } else if (mode === 'top') {
    setCameraUp(new THREE.Vector3(0, 1, 0));
    camera.position.set(0.0001, 0, dist);
  } else {
    setCameraUp(new THREE.Vector3(0, 0, 1));
    camera.position.set(dist, -dist * 0.6, dist * 0.45);
  }
  controls.target.set(0, 0, 0);
  controls.update();
}

function initScene() {
  if (!THREE || !OrbitControls) return;
  scene = new THREE.Scene();
  scene.background = new THREE.Color('#0b0f17');

  const width = els.viewer.clientWidth;
  const height = els.viewer.clientHeight;
  camera = new THREE.PerspectiveCamera(45, width / height, 0.1, 2000);
  camera.position.set(6, 3, 8);
  camera.up.set(0, 0, 1);

  renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.setSize(width, height);
  renderer.setPixelRatio(window.devicePixelRatio || 1);
  els.viewer.appendChild(renderer.domElement);

  controls = new OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;
  controls.dampingFactor = 0.08;
  controls.rotateSpeed = 0.6;
  controls.zoomSpeed = 0.8;
  controls.enablePan = true;
  controls.enableZoom = true;
  controls.enableRotate = true;
  controls.screenSpacePanning = false;
  setControlMode('rotate');

  const hemi = new THREE.HemisphereLight(0x9fb9ff, 0x0b0f17, 0.9);
  scene.add(hemi);

  const dir = new THREE.DirectionalLight(0xffffff, 1.2);
  dir.position.set(5, 8, 6);
  scene.add(dir);

  aircraft = buildPlaceholderAircraft();
  scene.add(aircraft);
  rebuildGrid(30);
  applyGridMode();
  if (axesHelper) axesHelper.position.set(0, 0, 0);
  updateViewerButtons();

  animate();
}

function buildPlaceholderAircraft() {
  if (!THREE) return null;
  const group = new THREE.Group();

  const bodyMat = new THREE.MeshStandardMaterial({
    color: 0x7dd3fc,
    metalness: 0.2,
    roughness: 0.35,
  });
  const wingMat = new THREE.MeshStandardMaterial({
    color: 0xf59e0b,
    metalness: 0.1,
    roughness: 0.4,
  });

  const fuselage = new THREE.Mesh(new THREE.CylinderGeometry(0.35, 0.45, 5.4, 20), bodyMat);
  fuselage.rotation.z = Math.PI / 2;
  group.add(fuselage);

  const nose = new THREE.Mesh(new THREE.ConeGeometry(0.35, 1.2, 20), bodyMat);
  nose.position.x = 3.2;
  nose.rotation.z = Math.PI / 2;
  group.add(nose);

  const tail = new THREE.Mesh(new THREE.ConeGeometry(0.25, 0.9, 16), bodyMat);
  tail.position.x = -3.0;
  tail.rotation.z = -Math.PI / 2;
  group.add(tail);

  const wing = new THREE.Mesh(new THREE.BoxGeometry(0.3, 6.2, 1.1), wingMat);
  wing.position.set(0.2, 0, 0);
  group.add(wing);

  const tailplane = new THREE.Mesh(new THREE.BoxGeometry(0.2, 2.4, 0.6), wingMat);
  tailplane.position.set(-2.4, 0, 0.2);
  group.add(tailplane);

  const fin = new THREE.Mesh(new THREE.BoxGeometry(0.15, 0.7, 1.4), wingMat);
  fin.position.set(-2.6, 0.3, 0.8);
  group.add(fin);

  return group;
}

function updateBank(phiDeg) {
  if (!aircraft) return;
  aircraft.rotation.x = THREE.MathUtils.degToRad(phiDeg);
}

function loadGeometryFromText(text, shouldFit = true) {
  if (!scene) return;
  const parsed = parseAVL(text || '');
  const withDup = applyYDuplicate(parsed);
  const withY = (typeof applyYSymmetry === 'function') ? applyYSymmetry(withDup) : withDup;
  const model = (typeof applyZSymmetry === 'function') ? applyZSymmetry(withY) : withY;
  const dbg = [];
  dbg.push(`Geometry debug: surfaces=${model.surfaces.length}`);
  model.surfaces.forEach((surf, sIdx) => {
    if (!surf.sections?.length) return;
    const left = surf.sections[0];
    const right = surf.sections[surf.sections.length - 1];
    dbg.push(`S${sIdx + 1} ${surf.name}: sections=${surf.sections.length} ydup=${surf.yduplicate ?? 'none'} angle=${surf.angleDeg ?? 0}`);
    dbg.push(`  root LE: x=${fmt(left.xle, 3)} y=${fmt(left.yle, 3)} z=${fmt(left.zle, 3)} chord=${fmt(left.chord, 3)} ainc=${fmt(left.ainc, 3)}`);
    dbg.push(`  tip  LE: x=${fmt(right.xle, 3)} y=${fmt(right.yle, 3)} z=${fmt(right.zle, 3)} chord=${fmt(right.chord, 3)} ainc=${fmt(right.ainc, 3)}`);
  });
  logDebug(dbg.join('\n'));
  model.airfoilFiles.forEach((path) => fetchAirfoil(path));
  const newAircraft = buildAircraftFromAVL(model);
  if (aircraft) scene.remove(aircraft);
  aircraft = newAircraft;
  scene.add(aircraft);
  updateBank(Number(els.bank.value));
  const bounds = computeBounds(aircraft);
  if (bounds) {
    aircraft.position.sub(bounds.center);
    viewerState.bounds = bounds;
    viewerState.fitDistance = bounds.maxDim * 1.6 + 4.0;
    rebuildGrid(bounds.maxDim);
    applyGridMode();
    if (axesHelper) {
      axesHelper.position.set(0, 0, 0);
    }
    logDebug(`Bounds: min=(${fmt(bounds.box.min.x, 3)},${fmt(bounds.box.min.y, 3)},${fmt(bounds.box.min.z, 3)}) max=(${fmt(bounds.box.max.x, 3)},${fmt(bounds.box.max.y, 3)},${fmt(bounds.box.max.z, 3)})`);
  }
  if (shouldFit) fitCameraToObject(aircraft);
  logDebug(`Geometry rebuilt: surfaces=${model.surfaces.length}`);
}

function buildExecState(model) {
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

  const NSURF = model.surfaces.length;
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
      nvs = surf.sections.reduce((sum, sec) => sum + (sec.nSpan ?? 0), 0);
    }
    NSTRIP += nvs;
    NVOR += nvs * nvc;
  });

  const NVMAX = Math.max(1, NVOR);
  const NSTRMAX = Math.max(1, NSTRIP);
  const NRMAX = 1;
  const IVMAX = IVTOT + NDMAX;
  const ICMAX = ICTOT + NDMAX;
  const DIM_N = NVMAX + 1;
  const DIM_U = NUMAX + 1;
  const DIM_C = NDMAX + 1;
  const DIM_G = NGMAX + 1;
  const DIM_L = 2;

  const state = {
    IVALFA, IVBETA, IVROTX, IVROTY, IVROTZ, IVTOT,
    ICALFA, ICBETA, ICROTX, ICROTY, ICROTZ, ICCL, ICCY, ICMOMX, ICMOMY, ICMOMZ, ICTOT,
    IPALFA, IPBETA, IPROTX, IPROTY, IPROTZ, IPCL, IPCD0, IPPHI, IPTHE, IPPSI,
    IPMACH, IPVEE, IPRHO, IPGEE, IPRAD, IPFAC, IPXCG, IPYCG, IPZCG, IPMASS,
    IPIXX, IPIYY, IPIZZ, IPIXY, IPIYZ, IPIZX, IPCLA, IPCLU, IPCMA, IPCMU, IPTOT,
    NVOR: NVOR,
    NVMAX,
    NSTRIP,
    NSURF,
    NCONTROL,
    NDESIGN,
    NUMAX,
    NDMAX,
    NGMAX,
    IVMAX,
    ICMAX,
    NRMAX,
    NVTOT: IVTOT,
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
    IYSYM: model.header.iysym ?? 0,
    IZSYM: model.header.izsym ?? 0,
    YSYM: 0.0,
    ZSYM: model.header.zsym ?? 0.0,
    VRCOREC: 0.0,
    VRCOREW: 2.0,
    SRCORE: 1.0,
    SAXFR: 0.25,

    LNASA_SA: false,
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
    XYZREF: new Float32Array(3),
    SREF: Math.fround(model.header.sref ?? 1.0),
    CREF: Math.fround(model.header.cref ?? 1.0),
    BREF: Math.fround(model.header.bref ?? 1.0),
    CDREF: 0.0,

    PARVAL: new Float32Array((IPTOT + 1) * (NRMAX + 1)),
    CONVAL: new Float32Array((ICMAX + 1) * (NRMAX + 1)),
    ICON: new Int32Array((IVTOT + 1) * (NRMAX + 1)),
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

    LFRST: new Int32Array(2),
    NL: new Int32Array(2),
    RL: new Float32Array(4 * 2),
    RADL: new Float32Array(2),

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
    VHINGE: new Float32Array(4 * (NVMAX + 1) * (NDMAX + 1)),
    VREFL: new Float32Array((NVMAX + 1) * (NDMAX + 1)),
    PHINGE: new Float32Array(4 * (NVMAX + 1) * (NDMAX + 1)),

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

    CHINGE: new Float32Array((NDMAX + 1) * (NSTRMAX + 1)),
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

  // Run-case defaults from UI
  const IR = 1;
  const idx2 = (i, j, dim1) => i + dim1 * j;
  state.PARVAL[idx2(IPMACH, IR, IPTOT)] = state.MACH;
  state.PARVAL[idx2(IPVEE, IR, IPTOT)] = Math.fround(Number(els.vel.value));
  state.PARVAL[idx2(IPRHO, IR, IPTOT)] = Math.fround(Number(els.rho.value));
  state.PARVAL[idx2(IPGEE, IR, IPTOT)] = Math.fround(Number(els.gee.value));
  state.PARVAL[idx2(IPCL, IR, IPTOT)] = Math.fround(Number(els.cl.value));
  state.PARVAL[idx2(IPPHI, IR, IPTOT)] = Math.fround(Number(els.bank.value));
  state.PARVAL[idx2(IPXCG, IR, IPTOT)] = Math.fround(model.header.xref ?? 0.0);
  state.PARVAL[idx2(IPYCG, IR, IPTOT)] = Math.fround(model.header.yref ?? 0.0);
  state.PARVAL[idx2(IPZCG, IR, IPTOT)] = Math.fround(model.header.zref ?? 0.0);
  state.PARVAL[idx2(IPCD0, IR, IPTOT)] = 0.0;
  state.ALFA = Math.fround(Number(els.alpha.value) * state.DTR);
  state.BETA = Math.fround(Number(els.beta.value) * state.DTR);

  // Constraints
  state.CONVAL[idx2(ICCL, IR, ICMAX)] = Math.fround(Number(els.clc.value));
  state.CONVAL[idx2(ICMOMX, IR, ICMAX)] = Math.fround(Number(els.cmx.value));
  state.CONVAL[idx2(ICMOMY, IR, ICMAX)] = Math.fround(Number(els.cmy.value));
  state.CONVAL[idx2(ICMOMZ, IR, ICMAX)] = Math.fround(Number(els.cmz.value));
  state.CONVAL[idx2(ICBETA, IR, ICMAX)] = Math.fround(Number(els.beta.value));

  // Trim variable selection
  state.ICON[idx2(IVALFA, IR, IVTOT)] = ICCL;
  state.ICON[idx2(IVBETA, IR, IVTOT)] = ICBETA;
  state.ICON[idx2(IVROTX, IR, IVTOT)] = ICMOMX;
  state.ICON[idx2(IVROTY, IR, IVTOT)] = ICMOMY;
  state.ICON[idx2(IVROTZ, IR, IVTOT)] = ICMOMZ;

  for (let n = 1; n <= NCONTROL; n += 1) {
    state.LCONDEF[n] = 1;
  }
  for (let n = 1; n <= NDESIGN; n += 1) {
    state.LDESDEF[n] = 1;
  }

  return state;
}

function buildGeometry(state, model) {
  state.NVOR = 0;
  state.NSTRIP = 0;
  model.surfaces.forEach((surf, idx) => {
    const isurf = idx + 1;
    const comp = (surf.component == null || surf.component === 0) ? isurf : surf.component;
    state.LNCOMP[isurf] = comp;
    state.LFWAKE[isurf] = 1;
    state.LFLOAD[isurf] = 1;
    MAKESURF(state, idx + 1, surf);
  });
  ENCALC(state);
}

function runExecFromText(text) {
  const t0 = performance.now();
  logDebug('EXEC start');
  const model = buildSolverModel(text);
  const state = buildExecState(model);
  buildGeometry(state, model);
  const worker = ensureExecWorker();
  if (!worker) {
    EXEC(state, 8, 0, 1);
    const dt = performance.now() - t0;
    logDebug(`EXEC done (${fmt(dt, 1)} ms)`);
    const idx2 = (i, j, dim1) => i + dim1 * j;
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
    const hinge = state.CHINGE ? (() => {
      const out = new Array(state.NCONTROL + 1).fill(0);
      for (let i = 1; i <= state.NCONTROL; i += 1) out[i] = state.CHINGE[i - 1] ?? 0.0;
      return out;
    })() : null;
    applyExecResults({
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
      CLTOT_U: state.CLTOT_U ? Array.from(state.CLTOT_U.slice(0, 6)) : null,
      CDTOT_U: state.CDTOT_U ? Array.from(state.CDTOT_U.slice(0, 6)) : null,
      CYTOT_U: state.CYTOT_U ? Array.from(state.CYTOT_U.slice(0, 6)) : null,
      CLTOT_D: state.CLTOT_D ? Array.from(state.CLTOT_D) : null,
      CDTOT_D: state.CDTOT_D ? Array.from(state.CDTOT_D) : null,
      CYTOT_D: state.CYTOT_D ? Array.from(state.CYTOT_D) : null,
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
      CDBDY: state.CDBDY ? Array.from(state.CDBDY) : null,
      CYBDY: state.CYBDY ? Array.from(state.CYBDY) : null,
      CLBDY: state.CLBDY ? Array.from(state.CLBDY) : null,
      CFBDY: state.CFBDY ? Array.from({ length: state.NBODY }, (_, ib) => surfVec(state.CFBDY, ib)) : null,
      CMBDY: state.CMBDY ? Array.from({ length: state.NBODY }, (_, ib) => surfVec(state.CMBDY, ib)) : null,
      CHINGE: hinge,
      PARVAL: state.PARVAL,
      WROT: state.WROT,
      DELCON: state.DELCON,
    });
    execInProgress = false;
    return;
  }

  execTimeoutId = setTimeout(() => {
    logDebug('EXEC timeout (70s): terminating worker.');
    execWorker?.terminate();
    execWorker = null;
    execInProgress = false;
  }, 70000);

  worker.postMessage({ state });
  const dt = performance.now() - t0;
  logDebug(`EXEC dispatched (${fmt(dt, 1)} ms)`);
}

function applyExecResults(result) {
  const idx2 = (i, j, dim1) => i + dim1 * j;
  const IR = 1;
  const IPPHI = 8;
  const IPTHE = 9;
  const IPVEE = 12;
  const IPRAD = 15;
  const IPFAC = 16;

  const parval = result.PARVAL;
  const vee = parval ? parval[idx2(IPVEE, IR, 30)] : null;
  const phi = parval ? parval[idx2(IPPHI, IR, 30)] : null;
  const the = parval ? parval[idx2(IPTHE, IR, 30)] : null;
  const rad = parval ? parval[idx2(IPRAD, IR, 30)] : null;
  const fac = parval ? parval[idx2(IPFAC, IR, 30)] : null;

  if (Number.isFinite(result.ALFA)) els.outAlpha.textContent = `${fmt(result.ALFA / (Math.PI / 180), 2)} deg`;
  if (Number.isFinite(result.BETA)) els.outBeta.textContent = `${fmt(result.BETA / (Math.PI / 180), 2)} deg`;
  if (Number.isFinite(result.CLTOT)) els.outCL.textContent = fmt(result.CLTOT, 3);
  if (Number.isFinite(result.CDTOT)) els.outCD.textContent = fmt(result.CDTOT, 4);
  if (Number.isFinite(result.CYTOT)) els.outCY.textContent = fmt(result.CYTOT, 4);
  if (Number.isFinite(vee)) els.outV.textContent = `${fmt(vee, 2)} m/s`;
  if (Number.isFinite(phi)) els.outBank.textContent = `${fmt(phi, 2)} deg`;
  if (Number.isFinite(rad)) els.outRad.textContent = rad > 0 ? `${fmt(rad, 2)} m` : 'level';
  if (Number.isFinite(fac)) els.outFac.textContent = fmt(fac, 3);
  if (Number.isFinite(the)) els.outThe.textContent = `${fmt(the, 2)} deg`;
  if (result.WROT) {
    els.outRates.textContent = `${fmt(result.WROT[0], 3)}, ${fmt(result.WROT[1], 3)}, ${fmt(result.WROT[2], 3)}`;
  }
  if (result.CMTOT) {
    els.outCM.textContent = `${fmt(result.CMTOT[0], 4)}, ${fmt(result.CMTOT[1], 4)}, ${fmt(result.CMTOT[2], 4)}`;
  }
  if (result.DELCON) {
    const deflections = [];
    const model = buildSolverModel(uiState.text || '');
    if (model.controlMap && model.controlMap.size) {
      for (const [name, idx] of model.controlMap.entries()) {
        const val = result.DELCON[idx] ?? 0.0;
        deflections.push(`${name} ${fmt(val, 2)}`);
      }
    }
    if (deflections.length) els.outDef.textContent = deflections.join(' / ');
  }

  const stabilityLines = [];
  if (result.CLTOT_U && result.CDTOT_U && result.CYTOT_U && result.VINF_A && result.VINF_B) {
    const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    const clU = result.CLTOT_U;
    const cdU = result.CDTOT_U;
    const cyU = result.CYTOT_U;
    const vinfA = result.VINF_A;
    const vinfB = result.VINF_B;
    const dCLdA = dot(clU, vinfA) + (result.CLTOT_A ?? 0);
    const dCDdA = dot(cdU, vinfA) + (result.CDTOT_A ?? 0);
    const dCYdA = dot(cyU, vinfA);
    const dCLdB = dot(clU, vinfB);
    const dCDdB = dot(cdU, vinfB);
    const dCYdB = dot(cyU, vinfB);
    stabilityLines.push(`dCL/dalpha: ${fmt(dCLdA, 5)}`);
    stabilityLines.push(`dCD/dalpha: ${fmt(dCDdA, 5)}`);
    stabilityLines.push(`dCY/dalpha: ${fmt(dCYdA, 5)}`);
    stabilityLines.push(`dCL/dbeta : ${fmt(dCLdB, 5)}`);
    stabilityLines.push(`dCD/dbeta : ${fmt(dCDdB, 5)}`);
    stabilityLines.push(`dCY/dbeta : ${fmt(dCYdB, 5)}`);
    stabilityLines.push('');
    stabilityLines.push(`CL_U: ${clU.map((v) => fmt(v, 5)).join(', ')}`);
    stabilityLines.push(`CD_U: ${cdU.map((v) => fmt(v, 5)).join(', ')}`);
    stabilityLines.push(`CY_U: ${cyU.map((v) => fmt(v, 5)).join(', ')}`);
  }
  if (result.CLTOT_D && result.CDTOT_D && result.CYTOT_D) {
    stabilityLines.push('');
    stabilityLines.push(`CL_D: ${result.CLTOT_D.map((v) => fmt(v, 5)).join(', ')}`);
    stabilityLines.push(`CD_D: ${result.CDTOT_D.map((v) => fmt(v, 5)).join(', ')}`);
    stabilityLines.push(`CY_D: ${result.CYTOT_D.map((v) => fmt(v, 5)).join(', ')}`);
  }
  if (els.outStability) {
    els.outStability.textContent = stabilityLines.length ? stabilityLines.join('\n') : '-';
  }

  if (els.outBodyDeriv) {
    const lines = [];
    if (result.CFTOT_U) {
      lines.push('dCF/dU (rows x,y,z):');
      lines.push(`x: ${result.CFTOT_U[0].map((v) => fmt(v, 5)).join(', ')}`);
      lines.push(`y: ${result.CFTOT_U[1].map((v) => fmt(v, 5)).join(', ')}`);
      lines.push(`z: ${result.CFTOT_U[2].map((v) => fmt(v, 5)).join(', ')}`);
    }
    if (result.CMTOT_U) {
      lines.push('');
      lines.push('dCM/dU (rows x,y,z):');
      lines.push(`x: ${result.CMTOT_U[0].map((v) => fmt(v, 5)).join(', ')}`);
      lines.push(`y: ${result.CMTOT_U[1].map((v) => fmt(v, 5)).join(', ')}`);
      lines.push(`z: ${result.CMTOT_U[2].map((v) => fmt(v, 5)).join(', ')}`);
    }
    els.outBodyDeriv.textContent = lines.length ? lines.join('\n') : '-';
  }

  if (els.outForcesTotal) {
    const lines = [];
    if (Number.isFinite(result.CLTOT)) lines.push(`CL: ${fmt(result.CLTOT, 5)}`);
    if (Number.isFinite(result.CDTOT)) lines.push(`CD: ${fmt(result.CDTOT, 5)}`);
    if (Number.isFinite(result.CYTOT)) lines.push(`CY: ${fmt(result.CYTOT, 5)}`);
    if (Number.isFinite(result.CDVTOT)) lines.push(`CDV: ${fmt(result.CDVTOT, 5)}`);
    if (result.CFTOT) lines.push(`CF: ${result.CFTOT.map((v) => fmt(v, 5)).join(', ')}`);
    if (result.CMTOT) lines.push(`CM: ${result.CMTOT.map((v) => fmt(v, 5)).join(', ')}`);
    els.outForcesTotal.textContent = lines.length ? lines.join('\n') : '-';
  }

  if (els.outForcesSurface) {
    const model = buildSolverModel(uiState.text || '');
    const lines = [];
    if (result.CDSURF && result.CLSURF && result.CYSURF) {
      for (let i = 1; i < result.CLSURF.length; i += 1) {
        const name = model.surfaces?.[i - 1]?.name ?? `Surf ${i}`;
        const cdv = result.CDVSURF?.[i] ?? 0;
        const cf = result.CFSURF?.[i] ?? null;
        const cm = result.CMSURF?.[i] ?? null;
        lines.push(`${name}: CL ${fmt(result.CLSURF[i], 5)} CD ${fmt(result.CDSURF[i], 5)} CY ${fmt(result.CYSURF[i], 5)} CDV ${fmt(cdv, 5)}`);
        if (cf) lines.push(`  CF: ${cf.map((v) => fmt(v, 5)).join(', ')}`);
        if (cm) lines.push(`  CM: ${cm.map((v) => fmt(v, 5)).join(', ')}`);
      }
    }
    els.outForcesSurface.textContent = lines.length ? lines.join('\n') : '-';
  }

  if (els.outForcesStrip) {
    const lines = [];
    if (result.CDSTRP && result.CLSTRP && result.CYSTRP && result.RLE) {
      for (let j = 1; j < result.CLSTRP.length; j += 1) {
        const y = result.RLE[idx2(2, j, 4)];
        const z = result.RLE[idx2(3, j, 4)];
        const cnc = result.CNC?.[j] ?? 0;
        const cla = result.CLA_LSTRP?.[j] ?? 0;
        const clt = result.CLT_LSTRP?.[j] ?? 0;
        const dw = result.DWWAKE?.[j] ?? 0;
        lines.push(`${j}: y ${fmt(y, 3)} z ${fmt(z, 3)} CL ${fmt(result.CLSTRP[j], 5)} CD ${fmt(result.CDSTRP[j], 5)} CY ${fmt(result.CYSTRP[j], 5)} CNC ${fmt(cnc, 5)} CLA ${fmt(cla, 5)} CLT ${fmt(clt, 5)} DW ${fmt(dw, 5)}`);
      }
    }
    els.outForcesStrip.textContent = lines.length ? lines.join('\n') : '-';
  }

  if (els.outForcesElement) {
    const lines = [];
    if (result.DCP) {
      for (let i = 1; i < result.DCP.length; i += 1) {
        lines.push(`${i}: DCP ${fmt(result.DCP[i], 6)}`);
      }
    }
    els.outForcesElement.textContent = lines.length ? lines.join('\n') : '-';
  }

  if (els.outForcesBody) {
    const lines = [];
    if (result.CDBDY && result.CYBDY && result.CLBDY) {
      for (let i = 0; i < result.CDBDY.length; i += 1) {
        lines.push(`Body ${i + 1}: CL ${fmt(result.CLBDY[i], 5)} CD ${fmt(result.CDBDY[i], 5)} CY ${fmt(result.CYBDY[i], 5)}`);
        const cf = result.CFBDY?.[i] ?? null;
        const cm = result.CMBDY?.[i] ?? null;
        if (cf) lines.push(`  CF: ${cf.map((v) => fmt(v, 5)).join(', ')}`);
        if (cm) lines.push(`  CM: ${cm.map((v) => fmt(v, 5)).join(', ')}`);
      }
    }
    els.outForcesBody.textContent = lines.length ? lines.join('\n') : 'No bodies';
  }

  if (els.outHinge) {
    const lines = [];
    if (result.CHINGE) {
      const model = buildSolverModel(uiState.text || '');
      const names = model.controlMap ? Array.from(model.controlMap.keys()) : [];
      for (let i = 1; i < result.CHINGE.length; i += 1) {
        const name = names[i - 1] ?? `Ctrl ${i}`;
        lines.push(`${name}: ${fmt(result.CHINGE[i], 6)}`);
      }
    }
    els.outHinge.textContent = lines.length ? lines.join('\n') : '-';
  }

  if (result.TREFFTZ && result.TREFFTZ.strips?.length) {
    uiState.trefftzData = result.TREFFTZ;
    const strips = result.TREFFTZ.strips;
    let ymin = Infinity;
    let ymax = -Infinity;
    let cmin = Infinity;
    let cmax = -Infinity;
    let wmin = Infinity;
    let wmax = -Infinity;
    strips.forEach(([y, _z, cnc, cl, clPerp, dw]) => {
      if (y < ymin) ymin = y;
      if (y > ymax) ymax = y;
      cmin = Math.min(cmin, cnc, cl, clPerp);
      cmax = Math.max(cmax, cnc, cl, clPerp);
      wmin = Math.min(wmin, dw);
      wmax = Math.max(wmax, dw);
    });
    logDebug(`Trefftz: strips=${strips.length} y=[${fmt(ymin, 2)}, ${fmt(ymax, 2)}] c=[${fmt(cmin, 4)}, ${fmt(cmax, 4)}] dw=[${fmt(wmin, 4)}, ${fmt(wmax, 4)}]`);
    const sample = strips.slice(0, 5).map((s) => s.map((v) => (Number.isFinite(v) ? Number(v).toFixed(4) : String(v))));
    logDebug(`Trefftz sample: ${JSON.stringify(sample)}`);
    updateTrefftz(Number(els.cl.value));
  }
}
function fitCameraToObject(obj) {
  if (!camera || !controls || !THREE) return;
  let bounds = viewerState.bounds;
  if (!bounds) {
    bounds = computeBounds(obj);
    if (!bounds) return;
    obj.position.sub(bounds.center);
    viewerState.bounds = bounds;
    viewerState.fitDistance = bounds.maxDim * 1.6 + 4.0;
  }
  const fitDist = viewerState.fitDistance || 10;
  camera.up.set(0, 0, 1);
  camera.position.set(fitDist, -fitDist * 0.6, fitDist * 0.45);
  controls.target.set(0, 0, 0);
  controls.update();
}

function animate() {
  requestAnimationFrame(animate);
  if (controls) controls.update();
  if (renderer && scene && camera) renderer.render(scene, camera);
}

function handleResize() {
  if (!renderer || !camera) return;
  const width = els.viewer.clientWidth;
  const height = els.viewer.clientHeight;
  renderer.setSize(width, height);
  camera.aspect = width / height;
  camera.updateProjectionMatrix();
}

window.addEventListener('resize', handleResize);

async function bootApp() {
  updateTrefftz(Number(els.cl.value));
  applyTrim();
  if (await ensureThree()) {
    initScene();
    loadGeometryFromText(els.fileText.value, true);
    logDebug('App ready with 3D viewer.');
  } else {
    logDebug('App ready (3D viewer disabled).');
  }
}

bootApp().catch((err) => logDebug(`Boot failed: ${err?.message ?? err}`));
