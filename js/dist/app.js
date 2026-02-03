import { TRMSET_CORE } from './atrim.js';
import { EXEC } from './aoper.js';
import { MAKESURF, ENCALC, SDUPL } from './amake.js';
import { GETCAM } from './airutil.js';
import { AKIMA, NRMLIZ } from './sgutil.js';

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
  flightMode: document.getElementById('flightMode'),
  levelInputs: document.getElementById('levelInputs'),
  loopInputs: document.getElementById('loopInputs'),
  clLoop: document.getElementById('clLoop'),
  velLoop: document.getElementById('velLoop'),
  radLoop: document.getElementById('radLoop'),
  facLoop: document.getElementById('facLoop'),
  trimBtn: document.getElementById('trimBtn'),
  useWasmExec: document.getElementById('useWasmExec'),
  appRoot: document.getElementById('appRoot'),
  navSettings: document.getElementById('navSettings'),
  navPlots: document.getElementById('navPlots'),
  navOutputs: document.getElementById('navOutputs'),
  settingsCol: document.getElementById('settingsCol'),
  plotsCol: document.getElementById('plotsCol'),
  outputsCol: document.getElementById('outputsCol'),
  viewer: document.getElementById('viewer'),
  trefftz: document.getElementById('trefftz'),
  trefftzOverlay: document.getElementById('trefftzOverlay'),
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
  outForcesBody: document.getElementById('outForcesBody'),
  outHinge: document.getElementById('outHinge'),
  downloadForcesStrip: document.getElementById('downloadForcesStrip'),
  downloadForcesElement: document.getElementById('downloadForcesElement'),
  debugLog: document.getElementById('debugLog'),
  clearDebug: document.getElementById('clearDebug'),
  viewerPan: document.getElementById('viewerPan'),
  viewerZoomIn: document.getElementById('viewerZoomIn'),
  viewerZoomOut: document.getElementById('viewerZoomOut'),
  viewerHome: document.getElementById('viewerHome'),
  viewerView: document.getElementById('viewerView'),
  viewerGrid: document.getElementById('viewerGrid'),
  viewerCoord: document.getElementById('viewerCoord'),
  viewerLoad: document.getElementById('viewerLoad'),
  constraintRows: [],
  constraintTable: document.getElementById('constraintTable'),
};

const uiState = {
  filename: null,
  text: '',
  surfaceColors: [0xff0000, 0xff8c00, 0xffff00, 0x00ff00, 0x00ffff, 0x0000ff, 0x8a2be2, 0xff00ff],
  trefftzData: null,
  modelHeader: null,
  levelDriver: 'cl',
  loopDriver: 'cl',
  lastExecResult: null,
  showLoading: false,
};

const viewerState = {
  mode: 'rotate',
  viewModes: ['left', 'right', 'top'],
  viewIndex: 0,
  gridModes: ['xy', 'yz', 'xz', 'none'],
  gridIndex: 0,
  bounds: null,
  fitDistance: 12,
  coordMode: 'body',
};

let execInProgress = false;
let trefftzPlot = null;
let execWorker = null;
let execTimeoutId = null;
let trimInProgress = false;
let trimRequestId = 0;
let execRequestId = 0;
let autoTrimTimer = null;
let lastTrimState = null;
let loadingGroup = null;

function updateTrefftzBusy() {
  const stage = els.trefftz?.parentElement;
  if (!stage) return;
  const busy = Boolean(trimInProgress || execInProgress);
  stage.classList.toggle('trefftz-busy', busy);
}

function initTopNav() {
  if (!els.appRoot || !els.navSettings || !els.navPlots || !els.navOutputs) return;
  const navItems = [
    { btn: els.navSettings, col: els.settingsCol },
    { btn: els.navPlots, col: els.plotsCol },
    { btn: els.navOutputs, col: els.outputsCol },
  ];

  const setActive = (btn) => {
    navItems.forEach(({ btn: b }) => b?.classList?.toggle('active', b === btn));
  };

  navItems.forEach(({ btn, col }) => {
    if (!btn || !col) return;
    btn.addEventListener('click', () => {
      const left = col.offsetLeft - els.appRoot.offsetLeft;
      els.appRoot.scrollTo({ left, behavior: 'smooth' });
      setActive(btn);
    });
  });

  let raf = 0;
  const onScroll = () => {
    if (raf) return;
    raf = requestAnimationFrame(() => {
      raf = 0;
      const scrollLeft = els.appRoot.scrollLeft;
      let best = navItems[0];
      let bestDist = Infinity;
      navItems.forEach((item) => {
        if (!item.col) return;
        const dist = Math.abs(item.col.offsetLeft - scrollLeft);
        if (dist < bestDist) {
          bestDist = dist;
          best = item;
        }
      });
      if (best?.btn) setActive(best.btn);
    });
  };

  els.appRoot.addEventListener('scroll', onScroll, { passive: true });
  setActive(els.navSettings);
}

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
      trimInProgress = false;
      updateTrefftzBusy();
      if (execTimeoutId) {
        clearTimeout(execTimeoutId);
        execTimeoutId = null;
      }
    } else if (msg.type === 'trimResult') {
      if (msg.requestId !== trimRequestId) {
        return;
      }
      trimInProgress = false;
      updateTrefftzBusy();
      try {
        applyTrimResults(msg.state);
        lastTrimState = extractTrimSeed(msg.state);
      } catch (err) {
        logDebug(`Trim apply failed: ${err?.message ?? err}`);
      }
    } else if (msg.type === 'result') {
      if (msg.requestId !== execRequestId) {
        return;
      }
      execInProgress = false;
      updateTrefftzBusy();
      if (execTimeoutId) {
        clearTimeout(execTimeoutId);
        execTimeoutId = null;
      }
      try {
        applyExecResults(msg);
      } catch (err) {
        logDebug(`EXEC apply failed: ${err?.message ?? err}`);
      }
    }
  };
  execWorker.onerror = (evt) => {
    logDebug(`EXEC worker failed: ${evt.message}`);
    execInProgress = false;
    trimInProgress = false;
    updateTrefftzBusy();
    if (execTimeoutId) {
      clearTimeout(execTimeoutId);
      execTimeoutId = null;
    }
  };
  return execWorker;
}

function resetTrimSeed() {
  lastTrimState = null;
  uiState.lastConstraintRows = null;
}

function extractTrimSeed(state) {
  return {
    ALFA: state.ALFA,
    BETA: state.BETA,
    DELCON: state.DELCON ? new Float64Array(state.DELCON) : null,
  };
}

const airfoilCache = new Map();
const airfoilPending = new Map();
const airfoilFailed = new Set();

function fmt(value, digits = 3) {
  if (!Number.isFinite(value)) return '-';
  return Number(value).toFixed(digits);
}

function getHeaderRefs() {
  const header = uiState.modelHeader || {};
  return {
    sref: Number(header.sref ?? 1.0),
    cref: Number(header.cref ?? 1.0),
    bref: Number(header.bref ?? 1.0),
    unitl: Number(header.unitl ?? 1.0),
  };
}

function setNumericInput(el, value, digits = 3) {
  if (!el || !Number.isFinite(value)) return;
  el.value = fmt(value, digits);
}

function updateFlightConditions(driverHint = null) {
  const mode = els.flightMode?.value || 'level';
  const { sref, unitl } = getHeaderRefs();
  const srefD = sref * unitl * unitl;
  const rho = Number(els.rho?.value || 0);
  const gee = Number(els.gee?.value || 0);
  const mass = Number(els.mass?.value || 0);

  if (mode === 'looping') {
    if (driverHint) uiState.loopDriver = driverHint;
    const cl = Number(els.clLoop?.value || 0);
    const vee = Number(els.velLoop?.value || 0);
    const rad = Number(els.radLoop?.value || 0);

    if (uiState.loopDriver === 'cl' && cl > 0 && rho > 0 && mass > 0) {
      const radNew = mass / (0.5 * rho * srefD * cl);
      if (Number.isFinite(radNew)) setNumericInput(els.radLoop, radNew, 3);
    } else if (uiState.loopDriver === 'rad' && rad > 0 && rho > 0 && mass > 0) {
      const clNew = mass / (0.5 * rho * srefD * rad);
      if (Number.isFinite(clNew)) setNumericInput(els.clLoop, clNew, 3);
    }

    const clUse = Number(els.clLoop?.value || 0);
    if (clUse > 0 && rho > 0 && mass > 0 && gee > 0 && vee > 0) {
      const fac = (0.5 * rho * vee * vee * srefD * clUse) / (mass * gee);
      if (Number.isFinite(fac)) setNumericInput(els.facLoop, fac, 3);
    }
    return;
  }

  if (driverHint) uiState.levelDriver = driverHint;
  const bank = Number(els.bank?.value || 0);
  const cl = Number(els.cl?.value || 0);
  const vee = Number(els.vel?.value || 0);
  const sinp = Math.sin(bank * (Math.PI / 180.0));
  const cosp = Math.cos(bank * (Math.PI / 180.0));

  if (uiState.levelDriver === 'cl' && cl > 0 && rho > 0 && mass > 0 && gee > 0 && cosp !== 0) {
    const veeNew = Math.sqrt((2.0 * mass * gee) / (rho * srefD * cl * cosp));
    if (Number.isFinite(veeNew)) setNumericInput(els.vel, veeNew, 2);
  } else if (uiState.levelDriver === 'vel' && vee > 0 && rho > 0 && mass > 0 && gee > 0 && cosp !== 0) {
    const clNew = (2.0 * mass * gee) / (rho * srefD * vee * vee * cosp);
    if (Number.isFinite(clNew)) setNumericInput(els.cl, clNew, 3);
  }
}

function getActiveCLValue() {
  const mode = els.flightMode?.value || 'level';
  const value = mode === 'looping'
    ? Number(els.clLoop?.value || els.cl?.value || 0)
    : Number(els.cl?.value || 0);
  return Number.isFinite(value) ? value : 0;
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

function readConstraintRows() {
  const rows = [];
  els.constraintRows.forEach((row) => {
    if (!row.dataset.var) return;
    const variable = row.dataset.var;
    const select = row.querySelector('.constraint-select');
    const value = row.querySelector('.constraint-value');
    rows.push({
      row,
      variable,
      select,
      value,
      constraint: select?.value || 'none',
      numeric: Number(value?.value || 0),
    });
  });
  return rows;
}

function updateConstraintDuplicates() {
  const rows = readConstraintRows();
  const counts = new Map();
  rows.forEach((entry) => {
    const key = entry.constraint;
    if (!key || key === 'none') return;
    counts.set(key, (counts.get(key) || 0) + 1);
  });
  rows.forEach((entry) => {
    const key = entry.constraint;
    const dup = key && counts.get(key) > 1;
    entry.row.classList.toggle('dup', dup);
    entry.select?.classList.toggle('dup', dup);
    entry.value?.classList.toggle('dup', dup);
  });
}

function applyConstraintRowsToState(state, controlMap) {
  const idx2 = (i, j, dim1) => i + dim1 * j;
  const controlNames = controlMap ? Array.from(controlMap.keys()) : [];
  const controlIndex = new Map();
  controlNames.forEach((name, idx) => controlIndex.set(name, idx + 1));

  state.NCONTROL = controlNames.length;
  state.NDMAX = Math.max(1, state.NCONTROL);
  state.IVMAX = state.IVTOT + state.NDMAX;
  state.NVTOT = state.IVTOT + state.NCONTROL;
  state.ICMAX = state.ICTOT + state.NDMAX;

  state.ICON = new Int32Array((state.IVMAX + 1) * (state.NRMAX + 1));
  state.CONVAL = new Float32Array((state.ICMAX + 1) * (state.NRMAX + 1));

  const { ICALFA, ICBETA, ICROTX, ICROTY, ICROTZ, ICCL, ICCY, ICMOMX, ICMOMY, ICMOMZ } = state;
  const { IVALFA, IVBETA, IVROTX, IVROTY, IVROTZ } = state;
  const constraintMap = {
    none: null,
    alpha: ICALFA,
    beta: ICBETA,
    p: ICROTX,
    q: ICROTY,
    r: ICROTZ,
    cl: ICCL,
    cy: ICCY,
    cmx: ICMOMX,
    cmy: ICMOMY,
    cmz: ICMOMZ,
  };
  const variableMap = {
    alpha: IVALFA,
    beta: IVBETA,
    p: IVROTX,
    q: IVROTY,
    r: IVROTZ,
  };

  const rows = readConstraintRows();
  rows.forEach((entry) => {
    const constraintKey = entry.constraint;
    let ic = constraintMap[constraintKey];
    if (!ic && constraintKey?.startsWith('ctrl:')) {
      const name = constraintKey.slice(5);
      const idx = controlIndex.get(name);
      if (idx) ic = state.ICTOT + idx;
    }
    if (!ic) return;
    state.CONVAL[idx2(ic, 1, state.ICMAX)] = entry.numeric;

    let iv = variableMap[entry.variable];
    if (!iv && entry.variable?.startsWith('ctrl:')) {
      const name = entry.variable.slice(5);
      const idx = controlIndex.get(name);
      if (idx) iv = state.IVTOT + idx;
    }
    if (iv) state.ICON[idx2(iv, 1, state.IVMAX)] = ic;
  });
}

function rebuildConstraintUI(model) {
  if (!els.constraintTable) return;
  const controlNames = model?.controlMap ? Array.from(model.controlMap.keys()) : [];
  const controlLabels = controlNames.map((name, idx) => `D${idx + 1} ${name}`);
  const rows = [
    { key: 'alpha', label: 'Alpha', value: 0.0, selected: 'alpha' },
    { key: 'beta', label: 'Beta', value: 0.0, selected: 'beta' },
    { key: 'p', label: 'Roll rate', value: 0.0, selected: 'p' },
    { key: 'q', label: 'Pitch rate', value: 0.0, selected: 'q' },
    { key: 'r', label: 'Yaw rate', value: 0.0, selected: 'r' },
  ];
  controlNames.forEach((name, idx) => {
    rows.push({ key: `ctrl:${name}`, label: controlLabels[idx], value: 0.0, selected: `ctrl:${name}` });
  });

  const constraintOptions = [
    { value: 'alpha', label: 'A  alpha' },
    { value: 'beta', label: 'B  beta' },
    { value: 'p', label: 'R  pb/2V' },
    { value: 'q', label: 'P  qc/2V' },
    { value: 'r', label: 'Y  rb/2V' },
    { value: 'cl', label: 'C  CL' },
    { value: 'cy', label: 'S  CY' },
    { value: 'cmx', label: 'RM Cl roll mom' },
    { value: 'cmy', label: 'PM Cm pitchmom' },
    { value: 'cmz', label: 'YM Cn yaw  mom' },
    { value: 'none', label: 'None' },
  ];
  controlNames.forEach((name, idx) => {
    constraintOptions.push({ value: `ctrl:${name}`, label: controlLabels[idx] });
  });

  els.constraintTable.innerHTML = '';
  const headerRow = document.createElement('div');
  headerRow.className = 'constraint-row';
  ['Variable', 'Constraint', 'Value'].forEach((text) => {
    const div = document.createElement('div');
    div.className = 'constraint-head';
    div.textContent = text;
    headerRow.appendChild(div);
  });
  els.constraintTable.appendChild(headerRow);

  rows.forEach((row) => {
    const rowEl = document.createElement('div');
    rowEl.className = 'constraint-row';
    rowEl.dataset.var = row.key;

    const cellVar = document.createElement('div');
    cellVar.className = 'constraint-cell';
    cellVar.textContent = row.label;

    const cellConstraint = document.createElement('div');
    cellConstraint.className = 'constraint-cell';
    const select = document.createElement('select');
    select.className = 'constraint-select';
    constraintOptions.forEach((opt) => {
      const option = document.createElement('option');
      option.value = opt.value;
      option.textContent = opt.label;
      if (opt.value === row.selected) option.selected = true;
      select.appendChild(option);
    });
    select.addEventListener('change', () => {
      if (select.value === 'cl') {
        updateFlightConditions();
        setNumericInput(input, getActiveCLValue(), 3);
      }
    });
    cellConstraint.appendChild(select);

    const cellValue = document.createElement('div');
    cellValue.className = 'constraint-cell';
    const input = document.createElement('input');
    input.className = 'constraint-value';
    input.type = 'number';
    input.step = '0.01';
    input.value = row.value;
    cellValue.appendChild(input);

    rowEl.appendChild(cellVar);
    rowEl.appendChild(cellConstraint);
    rowEl.appendChild(cellValue);
    els.constraintTable.appendChild(rowEl);
  });

  els.constraintRows = Array.from(document.querySelectorAll('.constraint-row'));
  els.constraintRows.forEach((row) => {
    row.querySelectorAll('select, input').forEach((el) => {
      el.addEventListener('change', updateConstraintDuplicates);
      el.addEventListener('input', updateConstraintDuplicates);
      el.addEventListener('change', scheduleAutoTrim);
      el.addEventListener('input', scheduleAutoTrim);
    });
  });
  updateConstraintDuplicates();
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
    resetTrimSeed();
    applyTrim({ useSeed: false });
    logDebug(`Loaded file: ${file.name}`);
  };
  reader.readAsText(file);
}

async function loadDefaultAVL() {
  const existing = els.fileText.value?.trim();
  if (existing) {
    uiState.text = existing;
    return;
  }
  const candidates = [
    new URL('./third_party/avl/runs/plane.avl', window.location.href).toString(),
    new URL('../third_party/avl/runs/plane.avl', window.location.href).toString(),
    new URL('../../third_party/avl/runs/plane.avl', window.location.href).toString(),
  ];
  for (const url of candidates) {
    try {
      const res = await fetch(url);
      if (!res.ok) continue;
      const text = await res.text();
      if (!text.trim()) continue;
      uiState.text = text;
      uiState.filename = 'plane.avl';
      els.fileText.value = text;
      if (els.fileMeta) els.fileMeta.textContent = 'Loaded: plane.avl (default)';
      logDebug('Loaded default file: plane.avl');
      return;
    } catch {
      // try next
    }
  }
  const embedded = document.getElementById('defaultPlane')?.textContent || '';
  if (embedded.trim()) {
    uiState.text = embedded;
    uiState.filename = 'plane.avl';
    els.fileText.value = embedded;
    if (els.fileMeta) els.fileMeta.textContent = 'Loaded: plane.avl (embedded)';
    logDebug('Loaded embedded default file: plane.avl');
    return;
  }
  logDebug('Default file plane.avl not found.');
}

els.fileInput.addEventListener('change', (evt) => {
  const file = evt.target.files?.[0];
  if (file) handleFileLoad(file);
});

els.bank?.addEventListener('input', scheduleAutoTrim);
els.cl?.addEventListener('input', scheduleAutoTrim);
els.vel?.addEventListener('input', scheduleAutoTrim);
els.mass?.addEventListener('input', scheduleAutoTrim);
els.rho?.addEventListener('input', scheduleAutoTrim);
els.gee?.addEventListener('input', scheduleAutoTrim);
els.flightMode?.addEventListener('change', scheduleAutoTrim);
els.clLoop?.addEventListener('input', scheduleAutoTrim);
els.velLoop?.addEventListener('input', scheduleAutoTrim);
els.radLoop?.addEventListener('input', scheduleAutoTrim);
els.facLoop?.addEventListener('input', scheduleAutoTrim);
els.useWasmExec?.addEventListener('change', scheduleAutoTrim);

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
els.viewerCoord?.addEventListener('click', () => {
  viewerState.coordMode = viewerState.coordMode === 'world' ? 'body' : 'world';
  updateBank(Number(els.bank.value));
  updateViewerButtons();
});
els.viewerLoad?.addEventListener('click', () => {
  uiState.showLoading = !uiState.showLoading;
  els.viewerLoad.classList.toggle('active', uiState.showLoading);
  updateLoadingVisualization();
});

els.flightMode?.addEventListener('change', () => {
  const mode = els.flightMode.value;
  const isLoop = mode === 'looping';
  els.levelInputs?.classList.toggle('hidden', isLoop);
  els.loopInputs?.classList.toggle('hidden', !isLoop);
  updateFlightConditions();
});

if (els.flightMode) {
  const isLoop = els.flightMode.value === 'looping';
  els.levelInputs?.classList.toggle('hidden', isLoop);
  els.loopInputs?.classList.toggle('hidden', !isLoop);
}

els.cl?.addEventListener('input', () => updateFlightConditions('cl'));
els.vel?.addEventListener('input', () => updateFlightConditions('vel'));
els.bank?.addEventListener('input', () => updateFlightConditions());
els.rho?.addEventListener('input', () => updateFlightConditions());
els.gee?.addEventListener('input', () => updateFlightConditions());
els.mass?.addEventListener('input', () => updateFlightConditions());

els.clLoop?.addEventListener('input', () => updateFlightConditions('cl'));
els.velLoop?.addEventListener('input', () => updateFlightConditions('vel'));
els.radLoop?.addEventListener('input', () => updateFlightConditions('rad'));
els.facLoop?.addEventListener('input', () => updateFlightConditions());

let fileUpdateTimer = null;
els.fileText.addEventListener('input', () => {
  clearTimeout(fileUpdateTimer);
  fileUpdateTimer = setTimeout(() => {
    uiState.text = els.fileText.value;
    loadGeometryFromText(uiState.text, true);
  }, 250);
});

function makeTrimState(controlMap = null) {
  const state = {
    IPTOT: 30,
    IVTOT: 5,
    ICMAX: 10,
    NVTOT: 5,
    NRMAX: 1,
    IVALFA: 1,
    IVBETA: 2,
    IVROTX: 3,
    IVROTY: 4,
    IVROTZ: 5,
    ICALFA: 1,
    ICBETA: 2,
    ICROTX: 3,
    ICROTY: 4,
    ICROTZ: 5,
    ICCL: 6,
    ICCY: 7,
    ICMOMX: 8,
    ICMOMY: 9,
    ICMOMZ: 10,
    ICTOT: 10,
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

  const {
    IVALFA, IVBETA, IVROTX, IVROTY, IVROTZ,
    ICALFA, ICBETA, ICCL, ICCY, ICMOMX, ICMOMY, ICMOMZ, ICROTX, ICROTY, ICROTZ,
  } = state;
  const { IPPHI, IPTHE, IPVEE, IPRHO, IPGEE, IPRAD, IPFAC, IPMASS, IPCL } = state;

  const IR = 1;
  const idx2 = (i, j, dim1) => i + dim1 * j;

  const mode = els.flightMode?.value || 'level';
  if (mode === 'looping') {
    state.PARVAL[idx2(IPPHI, IR, state.IPTOT)] = 0.0;
    state.PARVAL[idx2(IPTHE, IR, state.IPTOT)] = 0.0;
    state.PARVAL[idx2(IPVEE, IR, state.IPTOT)] = Number(els.velLoop?.value || els.vel.value);
  } else {
    state.PARVAL[idx2(IPPHI, IR, state.IPTOT)] = Number(els.bank.value);
    state.PARVAL[idx2(IPTHE, IR, state.IPTOT)] = 0.0;
    state.PARVAL[idx2(IPVEE, IR, state.IPTOT)] = Number(els.vel.value);
  }
  state.PARVAL[idx2(IPRHO, IR, state.IPTOT)] = Number(els.rho.value);
  state.PARVAL[idx2(IPGEE, IR, state.IPTOT)] = Number(els.gee.value);
  state.PARVAL[idx2(IPMASS, IR, state.IPTOT)] = Number(els.mass.value);
  state.PARVAL[idx2(IPCL, IR, state.IPTOT)] = mode === 'looping'
    ? Number(els.clLoop?.value || els.cl.value)
    : Number(els.cl.value);
  state.PARVAL[idx2(IPRAD, IR, state.IPTOT)] = mode === 'looping'
    ? Number(els.radLoop?.value || 0)
    : 0.0;
  state.PARVAL[idx2(IPFAC, IR, state.IPTOT)] = mode === 'looping'
    ? Number(els.facLoop?.value || 0)
    : 0.0;

  const map = controlMap || uiState.controlMap;
  if (!map) {
    const model = parseAVL(uiState.text || '');
    applyConstraintRowsToState(state, model.controlMap);
  } else {
    applyConstraintRowsToState(state, map);
  }

  return state;
}

function applyTrim({ useSeed = true } = {}) {
  logDebug('Trim requested.');
  updateFlightConditions();
  let state;
  try {
    state = makeTrimState(uiState.controlMap || null);
  } catch (err) {
    logDebug(`Trim setup failed: ${err?.message ?? err}`);
    return;
  }
  if (execWorker && (trimInProgress || execInProgress)) {
    execWorker.terminate();
    execWorker = null;
    trimInProgress = false;
    execInProgress = false;
    updateTrefftzBusy();
    if (execTimeoutId) {
      clearTimeout(execTimeoutId);
      execTimeoutId = null;
    }
  }
  trimInProgress = true;
  updateTrefftzBusy();
  trimRequestId += 1;
  uiState.lastConstraintRows = readConstraintRows();
  if (useSeed && lastTrimState) {
    state.ALFA = lastTrimState.ALFA ?? state.ALFA;
    state.BETA = lastTrimState.BETA ?? state.BETA;
    if (lastTrimState.DELCON && state.DELCON) {
      state.DELCON.set(lastTrimState.DELCON);
    }
  }
  const worker = ensureExecWorker();
  if (!worker) {
    try {
      const IR = 1;
      TRMSET_CORE(state, 1, IR, IR, IR);
      trimInProgress = false;
      updateTrefftzBusy();
      applyTrimResults(state);
      lastTrimState = extractTrimSeed(state);
    } catch (err) {
      logDebug(`Trim setup failed: ${err?.message ?? err}`);
      trimInProgress = false;
      updateTrefftzBusy();
    }
    return;
  }
  worker.postMessage({ type: 'trim', state, requestId: trimRequestId });
}

function applyTrimResults(state) {
  const constraintRows = uiState.lastConstraintRows || readConstraintRows();
  const findVar = (key, fallback = 0) => {
    const row = constraintRows.find((r) => r.variable === key);
    return row ? row.numeric : fallback;
  };
  const IR = 1;
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

  els.outAlpha.textContent = `${fmt(findVar('alpha'), 2)} deg`;
  els.outBeta.textContent = `${fmt(findVar('beta'), 2)} deg`;
  els.outBank.textContent = `${fmt(phi, 2)} deg`;
  els.outCL.textContent = fmt(cl, 3);
  els.outV.textContent = `${fmt(vee, 2)} m/s`;
  els.outRad.textContent = rad > 0 ? `${fmt(rad, 2)} m` : 'level';
  els.outFac.textContent = fmt(fac, 3);
  els.outThe.textContent = `${fmt(the, 2)} deg`;
  els.outRates.textContent = `${fmt(findVar('p'), 2)}, ${fmt(findVar('q'), 2)}, ${fmt(findVar('r'), 2)}`;
  els.outDef.textContent = '-';

  try {
    updateTrefftz(cl);
    updateBank(phi);
  } catch (err) {
    logDebug(`Trim render failed: ${err?.message ?? err}`);
  }

  logDebug('Trim base outputs updated.');

  if (uiState.text.trim()) {
    setTimeout(() => {
      if (execInProgress) {
        logDebug('EXEC skipped: already running.');
        return;
      }
      execInProgress = true;
      updateTrefftzBusy();
      logDebug('EXEC scheduled.');
      try {
        runExecFromText(uiState.text);
      } catch (err) {
        logDebug(`EXEC failed: ${err?.message ?? err}`);
        execInProgress = false;
        updateTrefftzBusy();
      }
    }, 0);
  }
}

els.trimBtn.addEventListener('click', () => applyTrim());

function scheduleAutoTrim() {
  if (autoTrimTimer) clearTimeout(autoTrimTimer);
  autoTrimTimer = setTimeout(() => {
    autoTrimTimer = null;
    applyTrim();
  }, 200);
}

if (els.downloadForcesStrip) {
  els.downloadForcesStrip.addEventListener('click', () => {
    const result = uiState.lastExecResult;
    if (!result) {
      logDebug('Strip forces download skipped: no EXEC results yet.');
      return;
    }
    const lines = buildForcesStripLines(result);
    if (!lines.length) {
      logDebug('Strip forces download skipped: no strip data available.');
      return;
    }
    downloadText(`${uiState.filename || 'model'}_strip_forces.txt`, lines.join('\n'));
  });
}

if (els.downloadForcesElement) {
  els.downloadForcesElement.addEventListener('click', () => {
    const result = uiState.lastExecResult;
    if (!result) {
      logDebug('Element forces download skipped: no EXEC results yet.');
      return;
    }
    const lines = buildForcesElementLines(result);
    if (!lines.length) {
      logDebug('Element forces download skipped: no element data available.');
      return;
    }
    downloadText(`${uiState.filename || 'model'}_element_forces.txt`, lines.join('\n'));
  });
}

  if (typeof ResizeObserver !== 'undefined') {
    const trefPanel = els.trefftz?.parentElement;
    if (trefPanel) {
      const ro = new ResizeObserver(() => {
        if (uiState.trefftzData && uiState.trefftzData.strips?.length) {
          updateTrefftz(uiState.lastCL ?? 0);
        }
      });
      ro.observe(trefPanel);
    }
  }

function updateTrefftz(cl) {
  uiState.lastCL = cl;
  const canvas = els.trefftz;
  const katexLib = window?.katex;
  const hasTrefftz = uiState.trefftzData && uiState.trefftzData.strips?.length;

  if (!canvas?.getContext) {
    return;
  }
  const ctx = canvas.getContext('2d');
  const dpr = window.devicePixelRatio || 1;
  const width = canvas.clientWidth || canvas.width;
  const height = canvas.clientHeight || canvas.height;
  const w = Math.max(1, Math.floor(width * dpr));
  const h = Math.max(1, Math.floor(height * dpr));
  if (canvas.width !== w || canvas.height !== h) {
    canvas.width = w;
    canvas.height = h;
  }
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, width, height);

  ctx.fillStyle = '#0b0f17';
  ctx.fillRect(0, 0, width, height);

  let legend = canvas.parentElement?.querySelector?.('.trefftz-legend');
  if (!legend) {
    legend = document.createElement?.('div');
    if (legend) {
      legend.className = 'trefftz-legend';
      canvas.parentElement?.appendChild(legend);
    }
  }
  if (legend) {
    legend.innerHTML = `
      <div class="trefftz-legend-item">
        <svg class="swatch line clp" viewBox="0 0 24 8" aria-hidden="true">
          <line x1="1" y1="4" x2="23" y2="4" stroke-dasharray="6 4" />
        </svg>
        <span class="katex-label" data-math="c_{l\\perp}"></span>
      </div>
      <div class="trefftz-legend-item">
        <svg class="swatch line cl" viewBox="0 0 24 8" aria-hidden="true">
          <line x1="1" y1="4" x2="23" y2="4" stroke-dasharray="6 3 1.5 3" />
        </svg>
        <span class="katex-label" data-math="c_l"></span>
      </div>
      <div class="trefftz-legend-item">
        <svg class="swatch line cnc" viewBox="0 0 24 8" aria-hidden="true">
          <line x1="1" y1="4" x2="23" y2="4" />
        </svg>
        <span class="katex-label" data-math="c_l c / c_{ref}"></span>
      </div>
      <div class="trefftz-legend-item">
        <svg class="swatch line dw" viewBox="0 0 24 8" aria-hidden="true">
          <line x1="1" y1="4" x2="23" y2="4" stroke-dasharray="1.5 3" />
        </svg>
        <span class="katex-label" data-math="a_i"></span>
      </div>
    `;
  }
  let axisLabels = canvas.parentElement?.querySelector?.('.trefftz-axis-labels');
  if (!axisLabels) {
    axisLabels = document.createElement?.('div');
    if (axisLabels) {
      axisLabels.className = 'trefftz-axis-labels';
      canvas.parentElement?.appendChild(axisLabels);
    }
  }
  if (axisLabels) {
    axisLabels.innerHTML = `
      <div class="trefftz-axis-label x">Span (m)</div>
      <div class="trefftz-axis-label y-left"><span class="katex-label" data-math="c_l"></span></div>
      <div class="trefftz-axis-label y-right"><span class="katex-label" data-math="a_i"></span></div>
    `;
  }
  if (katexLib) {
    const labels = canvas.parentElement?.querySelectorAll?.('.katex-label') || [];
    labels.forEach((el) => {
      const expr = el.getAttribute('data-math') || '';
      if (!expr) return;
      try {
        katexLib.render(expr, el, { throwOnError: false, strict: false });
      } catch {
        el.textContent = expr;
      }
    });
  } else {
    const labels = canvas.parentElement?.querySelectorAll?.('.katex-label') || [];
    labels.forEach((el) => {
      const expr = el.getAttribute('data-math') || '';
      el.textContent = expr.replace(/\\mathrm\{([^}]+)\}/g, '$1').replace(/\\,/g, ' ');
    });
  }
  if (document.fonts && document.fonts.status !== 'loaded' && !uiState.trefftzFontsPending) {
    uiState.trefftzFontsPending = true;
    document.fonts.ready.then(() => {
      uiState.trefftzFontsPending = false;
      if (canvas.isConnected) updateTrefftz(cl);
    });
  }

  if (hasTrefftz) {
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

    strips.forEach(([y, _z, cnc, clVal, clPerp, dw]) => {
      if (y < ymin) ymin = y;
      if (y > ymax) ymax = y;
      fmin = Math.min(fmin, cnc, 0.0);
      fmax = Math.max(fmax, cnc, 0.0);
      cmin = Math.min(cmin, clPerp, clVal, 0.0);
      cmax = Math.max(cmax, clPerp, clVal, 0.0);
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
    if (cmax - cmin < 0.1) {
      const mid = (cmin + cmax) / 2;
      cmin = mid - 0.05;
      cmax = mid + 0.05;
    }
    if (!Number.isFinite(wmin) || !Number.isFinite(wmax) || Math.abs(wmax - wmin) < 1e-5) {
      wmin = 0.0;
      wmax = 0.1;
    }
    if (wmax - wmin < 0.1) {
      const mid = (wmin + wmax) / 2;
      wmin = mid - 0.05;
      wmax = mid + 0.05;
    }

    const spanC = cmax - cmin || 1e-6;
    const wminRaw = Math.min(wmin, 0);
    const wmaxRaw = Math.max(wmax, 0);
    let wSpan = Math.max(wmaxRaw - wminRaw, 1e-6);
    let wminAdj = wminRaw;
    let wmaxAdj = wmaxRaw;

    const ticks = 5;
    const showGridDom = true;
    const showGridCanvas = false;
    const showZeroLine = true;
    const labelGapSide = 3;
    const labelGapBottom = 1;
    const tickGap = 3;
    const panel = canvas.parentElement;
    const panelRect = panel?.getBoundingClientRect();
    const panelStyle = panel ? getComputedStyle(panel) : null;
    const borderLeft = panelStyle ? Number.parseFloat(panelStyle.borderLeftWidth || '0') : 0;
    const borderRight = panelStyle ? Number.parseFloat(panelStyle.borderRightWidth || '0') : 0;
    const borderTop = panelStyle ? Number.parseFloat(panelStyle.borderTopWidth || '0') : 0;
    const borderBottom = panelStyle ? Number.parseFloat(panelStyle.borderBottomWidth || '0') : 0;
    const xLabel = axisLabels?.querySelector('.trefftz-axis-label.x');
    const yLeft = axisLabels?.querySelector('.trefftz-axis-label.y-left');
    const yRight = axisLabels?.querySelector('.trefftz-axis-label.y-right');

    if (xLabel) {
      xLabel.style.left = `${width / 2}px`;
      xLabel.style.bottom = `${3 - borderBottom}px`;
      xLabel.style.top = '';
      xLabel.style.transform = 'translate(-50%, 0)';
    }
    if (yLeft) {
      yLeft.style.left = `${3 - borderLeft}px`;
      yLeft.style.top = `${height / 2}px`;
      yLeft.style.transform = 'translate(0, -50%)';
    }
    if (yRight) {
      yRight.style.right = `${3 - borderRight}px`;
      yRight.style.left = '';
      yRight.style.top = `${height / 2}px`;
      yRight.style.transform = 'translate(0, -50%)';
    }

    const labelRectLeft = yLeft?.getBoundingClientRect();
    const labelRectRight = yRight?.getBoundingClientRect();
    const labelRectBottom = xLabel?.getBoundingClientRect();

    ctx.font = '10px JetBrains Mono, monospace';
    const leftTickVals = [];
    const leftTickNums = [];
    const rightTickVals = [];
    const bottomTickVals = [];
    for (let i = 0; i <= ticks; i += 1) {
      const t = i / ticks;
      const cVal = cmin + t * (cmax - cmin);
      leftTickNums.push(cVal);
      leftTickVals.push(fmt(cVal, 1));
      bottomTickVals.push(fmt(ymin + t * (ymax - ymin), 1));
    }
    let zeroIdx = 0;
    let zeroDist = Infinity;
    leftTickNums.forEach((val, idx) => {
      const dist = Math.abs(val);
      if (dist < zeroDist) {
        zeroDist = dist;
        zeroIdx = idx;
      }
    });
    leftTickNums[zeroIdx] = 0;
    leftTickVals[zeroIdx] = '0.0';

    const zeroFrac = zeroIdx / ticks;
    if (zeroFrac > 0 && zeroFrac < 1) {
      wSpan = Math.max(
        wSpan,
        -wminRaw / zeroFrac,
        wmaxRaw / (1 - zeroFrac),
      );
    } else {
      wSpan = Math.max(Math.abs(wminRaw), Math.abs(wmaxRaw), wSpan);
    }
    wSpan *= 1.05;
    wminAdj = -zeroFrac * wSpan;
    wmaxAdj = wminAdj + wSpan;
    for (let i = 0; i <= ticks; i += 1) {
      const t = i / ticks;
      const wVal = wminAdj + t * (wmaxAdj - wminAdj);
      rightTickVals.push(i === zeroIdx ? '0.00' : fmt(wVal, 2));
    }
    const maxLeftTickW = Math.max(...leftTickVals.map((v) => ctx.measureText(v).width));
    const maxRightTickW = Math.max(...rightTickVals.map((v) => ctx.measureText(v).width));
    let tickH = 10;
    if (panel) {
      const probe = document.createElement('div');
      probe.className = 'trefftz-tick bottom';
      probe.style.position = 'absolute';
      probe.style.visibility = 'hidden';
      probe.textContent = '0.0';
      panel.appendChild(probe);
      tickH = probe.getBoundingClientRect().height || tickH;
      panel.removeChild(probe);
    }

    const leftLimit = (labelRectLeft && panelRect) ? (labelRectLeft.right - panelRect.left - borderLeft) : 24;
    const rightLimit = (labelRectRight && panelRect) ? (labelRectRight.left - panelRect.left - borderLeft) : (width - 24);
    const labelHeight = labelRectBottom?.height || 12;
    const bottomTickTop = panelRect ? (labelRectBottom.top - panelRect.top - borderTop - labelGapBottom - tickH) : (height - 3 - labelHeight - labelGapBottom - tickH);

    const leftTickLeft = leftLimit + labelGapSide;
    const rightTickLeft = rightLimit - labelGapSide - maxRightTickW;

    let x0 = leftTickLeft + maxLeftTickW + tickGap;
    let x1 = rightTickLeft - tickGap;
    const y0 = 8;
    let y1 = bottomTickTop - tickGap;
    if (y1 - y0 < 40) y1 = y0 + 40;

    let plotW = x1 - x0;
    let plotH = y1 - y0;

    const axisSpan = (min, max) => (max - min) || 1e-6;
    const axisY = (val, min, max) => y0 + (1 - (val - min) / axisSpan(min, max)) * plotH;
    const axisYWithZero = (val, min, max, zeroFrac) => {
      const span = axisSpan(min, max);
      const clampedZero = Number.isFinite(zeroFrac) ? Math.min(1, Math.max(0, zeroFrac)) : 0.5;
      const adjMin = -clampedZero * span;
      const adjMax = adjMin + span;
      return y0 + (1 - (val - adjMin) / (adjMax - adjMin)) * plotH;
    };
    const xFor = (y) => x0 + ((y - ymin) / (ymax - ymin)) * plotW;
    const yForC = (val) => axisY(val, cmin, cmax);
    const yForW = (val) => axisYWithZero(val, wminAdj, wmaxAdj, zeroFrac);

    let ticksLayer = panel?.querySelector?.('.trefftz-ticks');
    if (!ticksLayer) {
      ticksLayer = document.createElement?.('div');
      if (ticksLayer) {
        ticksLayer.className = 'trefftz-ticks';
        panel?.appendChild(ticksLayer);
      }
    }
    if (ticksLayer) {
      const parts = [];
      for (let i = 0; i <= ticks; i += 1) {
        const t = i / ticks;
        const y = y0 + plotH - t * plotH;
        const top = y - tickH / 2;
        parts.push(`<div class="trefftz-tick left" style="left:${leftTickLeft}px; top:${top}px; width:${maxLeftTickW}px; text-align:right;">${leftTickVals[i]}</div>`);
        parts.push(`<div class="trefftz-tick right" style="left:${rightTickLeft}px; top:${top}px; width:${maxRightTickW}px; text-align:left;">${rightTickVals[i]}</div>`);
      }
      for (let i = 0; i <= ticks; i += 1) {
        const t = i / ticks;
        const x = x0 + t * plotW;
        parts.push(`<div class="trefftz-tick bottom" style="left:${x}px; top:${y1 + tickGap}px; transform:translateX(-50%);">${bottomTickVals[i]}</div>`);
      }
      ticksLayer.innerHTML = parts.join('');
    }

    let axisLines = panel?.querySelector?.('.trefftz-axis-lines');
    if (!axisLines) {
      axisLines = document.createElement?.('div');
      if (axisLines) {
        axisLines.className = 'trefftz-axis-lines';
        panel?.appendChild(axisLines);
      }
    }
    if (axisLines) {
      axisLines.innerHTML = `
        <div class="trefftz-axis-line left" style="left:${x0}px; top:${y0}px; height:${plotH}px;"></div>
        <div class="trefftz-axis-line right" style="left:${x0 + plotW - 1}px; top:${y0}px; height:${plotH}px;"></div>
        <div class="trefftz-axis-line bottom" style="left:${x0}px; top:${y0 + plotH - 1}px; width:${plotW}px;"></div>
      `;
    }

    ctx.clearRect(0, 0, width, height);
    ctx.fillStyle = '#0b0f17';
    ctx.fillRect(0, 0, width, height);

    let gridLines = panel?.querySelector?.('.trefftz-grid-lines');
    if (!gridLines) {
      gridLines = document.createElement?.('div');
      if (gridLines) {
        gridLines.className = 'trefftz-grid-lines';
        panel?.appendChild(gridLines);
      }
    }
    const gridYPositions = [];
    if (gridLines) {
      if (showGridDom) {
        const parts = [];
        for (let i = 0; i <= ticks; i += 1) {
          const t = i / ticks;
          const y = y0 + plotH - t * plotH;
          const x = x0 + t * plotW;
          gridYPositions.push(y);
          parts.push(`<div class="trefftz-grid-line h" data-idx="${i}" style="left:${x0}px; top:${y}px; width:${plotW}px;"></div>`);
          parts.push(`<div class="trefftz-grid-line v" data-idx="${i}" style="left:${x}px; top:${y0}px; height:${plotH}px;"></div>`);
        }
        gridLines.innerHTML = parts.join('');
      } else {
        gridLines.innerHTML = '';
      }
    }
    if (showGridCanvas) {
      const zeroLine = yForC(0);
      ctx.strokeStyle = 'rgba(255,255,255,0.08)';
      ctx.lineWidth = 1;
      for (let i = 0; i <= ticks; i += 1) {
        const t = i / ticks;
        const y = y0 + plotH - t * plotH;
        if (Math.abs(y - zeroLine) <= 1) {
          continue;
        }
        ctx.beginPath();
        ctx.moveTo(x0, y);
        ctx.lineTo(x0 + plotW, y);
        ctx.stroke();
      }
      for (let i = 0; i <= ticks; i += 1) {
        const t = i / ticks;
        const x = x0 + t * plotW;
        ctx.beginPath();
        ctx.moveTo(x, y0);
        ctx.lineTo(x, y0 + plotH);
        ctx.stroke();
      }
    }

    // Axes are rendered via DOM overlays to avoid canvas/DOM misalignment on scroll.

    if (axisLines) {
      axisLines.innerHTML = `
        <div class="trefftz-axis-line left" style="left:${x0}px; top:${y0}px; height:${plotH}px;"></div>
        <div class="trefftz-axis-line right" style="left:${x1 - 1}px; top:${y0}px; height:${plotH}px;"></div>
        <div class="trefftz-axis-line bottom" style="left:${x0}px; top:${y1 - 1}px; width:${plotW}px;"></div>
      `;
    }

    const drawSeries = (color, idxValue, scaleFn, dash) => {
      ctx.strokeStyle = color;
      ctx.lineWidth = 1;
      ctx.setLineDash(dash);
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
      ctx.setLineDash([]);
    };

    drawSeries('#ef4444', 4, yForC, [6, 4]); // c_l_perp
    drawSeries('#fb923c', 3, yForC, [6, 3, 1.5, 3]); // c_l
    drawSeries('#22c55e', 2, (v) => yForC(v / (cref || 1.0)), []); // c_l c / c_ref
    drawSeries('#3b82f6', 5, (v) => yForW(-v), [1.5, 3]); // a_i

    const zeroLine = y0 + plotH - (zeroIdx / ticks) * plotH;
    if (showZeroLine && zeroLine >= y0 && zeroLine <= y0 + plotH) {
      ctx.strokeStyle = 'rgba(255,255,255,0.6)';
      ctx.lineWidth = 1;
      ctx.beginPath();
      ctx.moveTo(x0, zeroLine);
      ctx.lineTo(x0 + plotW, zeroLine);
      ctx.stroke();
    }

    if (window.__trefftzTestHook) {
      window.__trefftzTestHook.gridY = gridYPositions;
      window.__trefftzTestHook.zeroLine = zeroLine;
      window.__trefftzTestHook.mapAxis = {
        axisY,
        axisYWithZero,
      };
    }

    if (!uiState.trefftzZeroLogged) {
      uiState.trefftzZeroLogged = true;
      const zeroW = yForW(0);
      logDebug(`Trefftz lines: y0=${fmt(y0, 2)} y1=${fmt(y1, 2)} zeroCL=${fmt(zeroLine, 2)} zeroAI=${fmt(zeroW, 2)}`);
    }

  } else {
    const span = width * 0.8;
    const center = width * 0.1;
    const base = height * 0.75;
    const scale = Math.max(0.2, Math.min(1.2, cl));

    ctx.beginPath();
    for (let i = 0; i <= 120; i += 1) {
      const t = (i / 120) * 2 - 1;
      const y = base - (1 - t * t) * (height * 0.45) * scale;
      const x = center + (t + 1) * (span / 2);
      if (i === 0) ctx.moveTo(x, y);
      else ctx.lineTo(x, y);
    }
    ctx.strokeStyle = '#2dd4bf';
    ctx.lineWidth = 2;
    ctx.shadowColor = 'rgba(45,212,191,0.35)';
    ctx.shadowBlur = 8;
    ctx.stroke();
    ctx.shadowBlur = 0;
  }

  uiState.trefftzLayoutVersion = (uiState.trefftzLayoutVersion || 0) + 1;
  if (window.__trefftzTestHook) {
    window.__trefftzTestHook.layoutVersion = uiState.trefftzLayoutVersion;
    window.__trefftzTestHook.layoutReady = !document.fonts || document.fonts.status === 'loaded';
  }
}

if (typeof window !== 'undefined') {
  window.__trefftzTestHook = {
    setTrefftzData(data) {
      uiState.trefftzData = data;
      updateTrefftz(Number(els.cl?.value || 0));
    },
  };
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
      } else if (subkey === 'NOWA') {
        surface.nowake = true;
      } else if (subkey === 'NOLO') {
        surface.noload = true;
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

function buildCamberSlope(coords, samples = 50) {
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
  const hingeSegments = [];
  const pushQuad = (le1, le2, te2, te1) => {
    verts.push(...le1, ...le2, ...te2, ...le1, ...te2, ...te1);
  };
  const outline = [];
  const pushEdge = (a, b) => {
    outline.push(...a, ...b);
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
    pushEdge(le1, le2);
    pushEdge(le2, te2);
    pushEdge(te2, te1);
    pushEdge(te1, le1);

    if (typeof surface.yduplicate === 'number') {
      const mirror = (p) => [p[0], 2 * surface.yduplicate - p[1], p[2]];
      const mle1 = mirror(le1);
      const mle2 = mirror(le2);
      const mte2 = mirror(te2);
      const mte1 = mirror(te1);
      pushQuad(mle1, mle2, mte2, mte1);
      pushEdge(mle1, mle2);
      pushEdge(mle2, mte2);
      pushEdge(mte2, mte1);
      pushEdge(mte1, mle1);
    }

    const controlsA = a.controls || [];
    const controlsB = b.controls || [];
    controlsA.forEach((ctrl) => {
      const mate = controlsB.find((c) => c.name === ctrl.name) || ctrl;
      const rawXhinge = ctrl.xhinge ?? 0.75;
      const rawMateXhinge = mate.xhinge ?? rawXhinge;
      const dispXhinge = rawXhinge >= 0.99 ? rawXhinge - 0.01 : (rawXhinge <= 0.01 ? rawXhinge + 0.01 : rawXhinge);
      const dispMateXhinge = rawMateXhinge >= 0.99 ? rawMateXhinge - 0.01 : (rawMateXhinge <= 0.01 ? rawMateXhinge + 0.01 : rawMateXhinge);
      const hx1 = a.xle + a.chord * dispXhinge;
      const hx2 = b.xle + b.chord * dispMateXhinge;
      const h1 = applyTransforms(rotateSectionPoint(a, [hx1, a.yle, a.zle]));
      const h2 = applyTransforms(rotateSectionPoint(b, [hx2, b.yle, b.zle]));
      addControlQuad(ctrl.name, h1, h2, te2, te1);
      hingeSegments.push([h1, h2]);
      if (typeof surface.yduplicate === 'number') {
        const mirror = (p) => [p[0], 2 * surface.yduplicate - p[1], p[2]];
        const mh1 = mirror(h1);
        const mh2 = mirror(h2);
        addControlQuad(ctrl.name, mh1, mh2, mirror(te2), mirror(te1));
        hingeSegments.push([mh1, mh2]);
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

  const outlineGeom = new THREE.BufferGeometry();
  outlineGeom.setAttribute('position', new THREE.Float32BufferAttribute(outline, 3));
  const outlineMat = new THREE.LineBasicMaterial({ color, linewidth: 1 });
  const sectionMat = new THREE.LineBasicMaterial({ color, linewidth: 1, transparent: true, opacity: 0.55 });
  const controlMat = new THREE.LineBasicMaterial({ color, linewidth: 1, transparent: true, opacity: 0.7 });
  const hingeMat = new THREE.LineDashedMaterial({
    color,
    linewidth: 1,
    dashSize: 0.15,
    gapSize: 0.12,
    transparent: true,
    opacity: 0.9,
  });
  const outlineLine = new THREE.LineSegments(outlineGeom, outlineMat);
  group.add(outlineLine);

  controlVerts.forEach((ctrlVerts, name) => {
    if (!ctrlVerts.length) return;
    const ctrlEdges = [];
    for (let i = 0; i < ctrlVerts.length; i += 18) {
      const le1 = ctrlVerts.slice(i, i + 3);
      const le2 = ctrlVerts.slice(i + 3, i + 6);
      const te2 = ctrlVerts.slice(i + 6, i + 9);
      const te1 = ctrlVerts.slice(i + 15, i + 18);
      ctrlEdges.push(...le1, ...le2, ...le2, ...te2, ...te2, ...te1, ...te1, ...le1);
    }
    const ctrlGeom = new THREE.BufferGeometry();
    ctrlGeom.setAttribute('position', new THREE.Float32BufferAttribute(ctrlEdges, 3));
    const ctrlLine = new THREE.LineSegments(ctrlGeom, controlMat);
    ctrlLine.name = `control:${name}`;
    group.add(ctrlLine);
  });

  if (hingeSegments.length) {
    const hingeVerts = [];
    hingeSegments.forEach(([a, b]) => {
      hingeVerts.push(...a, ...b);
    });
    const hingeGeom = new THREE.BufferGeometry();
    hingeGeom.setAttribute('position', new THREE.Float32BufferAttribute(hingeVerts, 3));
    const hingeLine = new THREE.LineSegments(hingeGeom, hingeMat);
    hingeLine.computeLineDistances();
    group.add(hingeLine);
  }

  camberLines.forEach((line) => {
    const flat = line.flat();
    const camGeom = new THREE.BufferGeometry();
    camGeom.setAttribute('position', new THREE.Float32BufferAttribute(flat, 3));
    const camLine = new THREE.Line(camGeom, sectionMat);
    group.add(camLine);
  });

  airfoilOutlines.forEach((line) => {
    const flat = line.flat();
    const foilGeom = new THREE.BufferGeometry();
    foilGeom.setAttribute('position', new THREE.Float32BufferAttribute(flat, 3));
    const foilLine = new THREE.Line(foilGeom, sectionMat);
    group.add(foilLine);
  });

  const labelCanvas = document.createElement('canvas');
  const ctx = labelCanvas.getContext('2d');
  labelCanvas.width = 256;
  labelCanvas.height = 128;
  ctx.clearRect(0, 0, labelCanvas.width, labelCanvas.height);
  ctx.fillStyle = '#e2e8f0';
  ctx.font = '28px JetBrains Mono, monospace';
  ctx.textAlign = 'center';
  ctx.textBaseline = 'middle';
  ctx.fillText(surface.name || 'Surface', 128, 64);
  const tex = new THREE.CanvasTexture(labelCanvas);
  const spriteMat = new THREE.SpriteMaterial({ map: tex, transparent: true });
  const sprite = new THREE.Sprite(spriteMat);
  const center = surface.sections.reduce((acc, sec) => {
    acc.x += sec.xle;
    acc.y += sec.yle;
    acc.z += sec.zle;
    return acc;
  }, { x: 0, y: 0, z: 0 });
  const denom = surface.sections.length || 1;
  const labelPos = applyTransforms([center.x / denom, center.y / denom, center.z / denom]);
  sprite.position.set(labelPos[0], labelPos[1], labelPos[2] + 0.2);
  sprite.scale.set(1.6, 0.8, 1);
  group.add(sprite);

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
  if (els.viewerCoord) {
    els.viewerCoord.classList.toggle('active', viewerState.coordMode === 'world');
    els.viewerCoord.title = viewerState.coordMode === 'world' ? 'World frame' : 'Body frame';
  }
  if (els.viewerLoad) {
    els.viewerLoad.classList.toggle('active', uiState.showLoading);
  }
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
  if (viewerState.coordMode === 'world') {
    aircraft.rotation.x = THREE.MathUtils.degToRad(phiDeg);
  } else {
    aircraft.rotation.x = 0;
  }
}

function updateLoadingVisualization() {
  if (!scene || !aircraft || !THREE) return;
  if (loadingGroup) {
    aircraft.remove(loadingGroup);
    loadingGroup = null;
  }
  if (!uiState.showLoading) return;
  const result = uiState.lastExecResult;
  if (!result || !result.RV || !result.DCP || !result.ENSY || !result.ENSZ || !result.IJFRST || !result.NVSTRP) return;

  const idx2 = (i, j, dim1) => i + dim1 * j;
  const rv = result.RV;
  const dcp = result.DCP;
  const ensy = result.ENSY;
  const ensz = result.ENSZ;
  const ijfrst = result.IJFRST;
  const nvstrp = result.NVSTRP;
  const jfrst = result.JFRST || null;
  const nj = result.NJ || null;
  const lfload = result.LFLOAD || null;

  const nstr = Math.min(ijfrst.length - 1, nvstrp.length - 1);
  if (nstr <= 0) return;

  const cref = Number.isFinite(result.CREF) ? result.CREF : 1.0;
  const bref = Number.isFinite(result.BREF) ? result.BREF : 1.0;
  const cpscl = Math.min(0.4 * cref, 0.1 * bref);

  const getSurfaceForStrip = (stripIdx) => {
    if (!jfrst || !nj) return 1;
    const nsurf = Math.min(jfrst.length - 1, nj.length - 1);
    for (let n = 1; n <= nsurf; n += 1) {
      const j1 = jfrst[n];
      const count = nj[n];
      if (!j1 || !count) continue;
      if (stripIdx >= j1 && stripIdx < j1 + count) return n;
    }
    return 1;
  };

  const loadVerts = [];
  const loadLines = [];
  for (let j = 1; j <= nstr; j += 1) {
    const surfId = getSurfaceForStrip(j);
    if (lfload && lfload[surfId] === 0) continue;
    const i1 = ijfrst[j];
    const nv = nvstrp[j];
    if (!i1 || !nv) continue;
    let prev = null;
    for (let ii = 1; ii <= nv; ii += 1) {
      const iv = i1 + ii - 1;
      const xave = rv[idx2(1, iv, 4)];
      const yave = rv[idx2(2, iv, 4)];
      const zave = rv[idx2(3, iv, 4)];
      const delyz = (dcp[iv] ?? 0) * cpscl;
      const xload = xave;
      const yload = yave + delyz * ensy[j];
      const zload = zave + delyz * ensz[j];
      loadVerts.push(xave, yave, zave, xload, yload, zload);
      if (prev) {
        loadLines.push(prev[0], prev[1], prev[2], xload, yload, zload);
      }
      prev = [xload, yload, zload];
    }
  }

  if (!loadVerts.length && !loadLines.length) return;
  loadingGroup = new THREE.Group();
  loadingGroup.name = 'loading';
  loadingGroup.renderOrder = 2;

  if (loadVerts.length) {
    const vecGeom = new THREE.BufferGeometry();
    vecGeom.setAttribute('position', new THREE.Float32BufferAttribute(loadVerts, 3));
    const vecMat = new THREE.LineBasicMaterial({ color: 0x22c55e, linewidth: 1, transparent: true, opacity: 0.85 });
    const vecLines = new THREE.LineSegments(vecGeom, vecMat);
    vecLines.name = 'loading-vectors';
    loadingGroup.add(vecLines);
  }

  if (loadLines.length) {
    const lineGeom = new THREE.BufferGeometry();
    lineGeom.setAttribute('position', new THREE.Float32BufferAttribute(loadLines, 3));
    const lineMat = new THREE.LineBasicMaterial({ color: 0xef4444, linewidth: 1, transparent: true, opacity: 0.85 });
    const lineStrip = new THREE.LineSegments(lineGeom, lineMat);
    lineStrip.name = 'loading-lines';
    loadingGroup.add(lineStrip);
  }

  aircraft.add(loadingGroup);
}

function loadGeometryFromText(text, shouldFit = true) {
  if (!scene) return;
  const parsed = parseAVL(text || '');
  uiState.modelHeader = parsed.header || null;
  const solverModel = buildSolverModel(text || '');
  uiState.controlMap = solverModel.controlMap;
  uiState.modelCache = solverModel;
  rebuildConstraintUI(solverModel);
  const withDup = applyYDuplicate(parsed);
  const withY = (typeof applyYSymmetry === 'function') ? applyYSymmetry(withDup) : withDup;
  const model = (typeof applyZSymmetry === 'function') ? applyZSymmetry(withY) : withY;
  const dbg = [];
  dbg.push(`Geometry debug: surfaces=${model.surfaces.length} controls=${solverModel.controlMap?.size ?? 0}`);
  if (solverModel.controlMap && solverModel.controlMap.size) {
    dbg.push(`Controls: ${Array.from(solverModel.controlMap.keys()).join(', ')}`);
  }
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
  updateFlightConditions();
  logDebug(`Geometry rebuilt: surfaces=${model.surfaces.length}`);
  updateLoadingVisualization();
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
    SREF: Math.fround(Number.isFinite(model.header.sref) ? model.header.sref : 1.0),
    CREF: Math.fround(Number.isFinite(model.header.cref) ? model.header.cref : 1.0),
    BREF: Math.fround(Number.isFinite(model.header.bref) ? model.header.bref : 1.0),
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
  state.PARVAL[idx2(IPVEE, IR, IPTOT)] = Math.fround(Number(els.vel?.value || 0));
  state.PARVAL[idx2(IPRHO, IR, IPTOT)] = Math.fround(Number(els.rho.value));
  state.PARVAL[idx2(IPGEE, IR, IPTOT)] = Math.fround(Number(els.gee.value));
  {
    const massVal = Number(els.mass?.value || 0);
    const massUse = massVal > 0 ? massVal : 1.0;
    state.PARVAL[idx2(IPMASS, IR, IPTOT)] = Math.fround(massUse);
  }
  state.PARVAL[idx2(IPCL, IR, IPTOT)] = Math.fround(Number(els.cl?.value || 0));
  state.PARVAL[idx2(IPPHI, IR, IPTOT)] = Math.fround(Number(els.bank?.value || 0));
  state.PARVAL[idx2(IPXCG, IR, IPTOT)] = Math.fround(Number.isFinite(model.header.xref) ? model.header.xref : 0.0);
  state.PARVAL[idx2(IPYCG, IR, IPTOT)] = Math.fround(Number.isFinite(model.header.yref) ? model.header.yref : 0.0);
  state.PARVAL[idx2(IPZCG, IR, IPTOT)] = Math.fround(Number.isFinite(model.header.zref) ? model.header.zref : 0.0);
  state.PARVAL[idx2(IPCD0, IR, IPTOT)] = 0.0;
  // Seed alpha to a small positive value to avoid singular trim steps.
  state.ALFA = Math.fround(2.0 * state.DTR);
  state.BETA = 0.0;

  // Trim variable selection (defaults; overridden by constraint UI)
  state.ICON[idx2(IVALFA, IR, IVMAX)] = ICCL;
  state.ICON[idx2(IVBETA, IR, IVMAX)] = ICBETA;
  state.ICON[idx2(IVROTX, IR, IVMAX)] = ICMOMX;
  state.ICON[idx2(IVROTY, IR, IVMAX)] = ICMOMY;
  state.ICON[idx2(IVROTZ, IR, IVMAX)] = ICMOMZ;

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

function runExecFromText(text) {
  const t0 = performance.now();
  logDebug('EXEC start');
  const model = buildSolverModel(text);
  const state = buildExecState(model);
  try {
    const rows = readConstraintRows();
    if (rows.length) {
      const line = rows.map((r) => `${r.variable}:${r.constraint}=${r.numeric}`).join(' | ');
      logDebug(`EXEC constraints (UI): ${line}`);
    }
  } catch {
    // ignore logging failures
  }
  applyConstraintRowsToState(state, model.controlMap);
  try {
    const names = model.controlMap ? Array.from(model.controlMap.keys()) : [];
    if (names.length) {
      const idx2 = (i, j, dim1) => i + dim1 * j;
      const entries = names.map((name, i) => {
        const iv = state.IVTOT + i + 1;
        const ic = state.ICON[idx2(iv, 1, state.IVMAX)];
        const cv = state.CONVAL[idx2(ic, 1, state.ICMAX)];
        return `${name}:IC${ic}=${fmt(cv, 4)}`;
      });
      logDebug(`EXEC constraints (controls): ${entries.join(' | ')}`);
    }
  } catch {
    // ignore logging failures
  }
  buildGeometry(state, model);
  const worker = ensureExecWorker();
  if (!worker) {
    EXEC(state, 20, 0, 1);
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
      PARVAL: state.PARVAL,
      WROT: state.WROT,
      DELCON: state.DELCON,
    });
    execInProgress = false;
    updateTrefftzBusy();
    return;
  }

  execTimeoutId = setTimeout(() => {
    logDebug('EXEC timeout (70s): terminating worker.');
    execWorker?.terminate();
    execWorker = null;
    execInProgress = false;
    updateTrefftzBusy();
  }, 70000);

  execRequestId += 1;
  const useWasm = Boolean(els.useWasmExec?.checked);
  if (useWasm) {
    logDebug('EXEC using WASM kernels.');
  }
  worker.postMessage({ state, requestId: execRequestId, useWasm });
  const dt = performance.now() - t0;
  logDebug(`EXEC dispatched (${fmt(dt, 1)} ms)`);
}

function downloadText(filename, text) {
  const blob = new Blob([text], { type: 'text/plain' });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = url;
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  link.remove();
  URL.revokeObjectURL(url);
}

function buildForcesStripLines(result) {
  const lines = [];
  const idx2 = (i, j, dim1) => i + dim1 * j;
  if (result?.CDSTRP && result.CLSTRP && result.CYSTRP && result.RLE) {
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
  return lines;
}

function buildForcesElementLines(result) {
  const lines = [];
  if (result?.DCP) {
    for (let i = 1; i < result.DCP.length; i += 1) {
      lines.push(`${i}: DCP ${fmt(result.DCP[i], 6)}`);
    }
  }
  return lines;
}

function applyExecResults(result) {
  uiState.lastExecResult = result;
  updateLoadingVisualization();
  const schedule = (fn) => {
    if (typeof requestIdleCallback === 'function') {
      requestIdleCallback(fn, { timeout: 200 });
    } else {
      setTimeout(fn, 0);
    }
  };
  const renderLinesChunked = (el, lines, chunk = 200) => {
    if (!el) return;
    if (lines.length <= chunk) {
      el.textContent = lines.length ? lines.join('\n') : '-';
      return;
    }
    el.textContent = lines.slice(0, chunk).join('\n');
    let idx = chunk;
    const pump = () => {
      const next = lines.slice(idx, idx + chunk).join('\n');
      if (next) el.textContent += `\n${next}`;
      idx += chunk;
      if (idx < lines.length) schedule(pump);
    };
    schedule(pump);
  };

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
    const model = uiState.modelCache;
    if (model?.controlMap && model.controlMap.size) {
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
  schedule(() => {
    try {
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
        renderLinesChunked(els.outBodyDeriv, lines, 80);
      }
    } catch (err) {
      logDebug(`EXEC derive render failed: ${err?.message ?? err}`);
    }
  });

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

  schedule(() => {
    try {
      if (els.outForcesSurface) {
        const model = uiState.modelCache;
        const lines = [];
        if (result.CDSURF && result.CLSURF && result.CYSURF) {
          for (let i = 1; i < result.CLSURF.length; i += 1) {
            const name = model?.surfaces?.[i - 1]?.name ?? `Surf ${i}`;
            const cdv = result.CDVSURF?.[i] ?? 0;
            const cf = result.CFSURF?.[i] ?? null;
            const cm = result.CMSURF?.[i] ?? null;
            lines.push(`${name}: CL ${fmt(result.CLSURF[i], 5)} CD ${fmt(result.CDSURF[i], 5)} CY ${fmt(result.CYSURF[i], 5)} CDV ${fmt(cdv, 5)}`);
            if (cf) lines.push(`  CF: ${cf.map((v) => fmt(v, 5)).join(', ')}`);
            if (cm) lines.push(`  CM: ${cm.map((v) => fmt(v, 5)).join(', ')}`);
          }
        }
        renderLinesChunked(els.outForcesSurface, lines, 120);
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
        renderLinesChunked(els.outForcesBody, lines, 120);
      }

      if (els.outHinge) {
        const lines = [];
        if (result.CHINGE) {
          const model = uiState.modelCache;
          const names = model?.controlMap ? Array.from(model.controlMap.keys()) : [];
          for (let i = 1; i < result.CHINGE.length; i += 1) {
            const name = names[i - 1] ?? `Ctrl ${i}`;
            lines.push(`${name}: ${fmt(result.CHINGE[i], 6)}`);
          }
        }
        renderLinesChunked(els.outHinge, lines, 120);
      }
    } catch (err) {
      logDebug(`EXEC detail render failed: ${err?.message ?? err}`);
    }
  });

  if (result.TREFFTZ && result.TREFFTZ.strips?.length) {
    const rawTrefftz = result.TREFFTZ;
    const stripsRaw = rawTrefftz.strips;
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
    const strips = stripsRaw.map((entry) => [
      entry[0],
      entry[1],
      toFinite(entry[2], 'cnc'),
      toFinite(entry[3], 'cl'),
      toFinite(entry[4], 'clt'),
      toFinite(entry[5], 'dw'),
      entry[6],
    ]);
    uiState.trefftzData = { ...rawTrefftz, strips };
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
    if (badCnc || badCl || badClt || badDw) {
      logDebug(`Trefftz non-finite: cnc=${badCnc} cl=${badCl} clt=${badClt} dw=${badDw}`);
    }
    logDebug(`Trefftz: strips=${strips.length} y=[${fmt(ymin, 2)}, ${fmt(ymax, 2)}] c=[${fmt(cmin, 4)}, ${fmt(cmax, 4)}] dw=[${fmt(wmin, 4)}, ${fmt(wmax, 4)}]`);
    const sample = strips.slice(0, 5).map((s) => s.map((v) => (Number.isFinite(v) ? Number(v).toFixed(4) : String(v))));
    logDebug(`Trefftz sample: ${JSON.stringify(sample)}`);
    schedule(() => updateTrefftz(Number(els.cl.value)));
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
  camera.position.set(fitDist * 0.6, -fitDist * 0.4, fitDist * 0.28);
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
  initTopNav();
  await loadDefaultAVL();
  try {
    const model = buildSolverModel(els.fileText.value || '');
    rebuildConstraintUI(model);
  } catch {
    rebuildConstraintUI({ controlMap: new Map() });
  }
  if (els.constraintTable) {
    logDebug(`Constraint table rows: ${els.constraintTable.children.length}`);
  } else {
    logDebug('Constraint table missing in DOM.');
  }
  if (await ensureThree()) {
    initScene();
    loadGeometryFromText(els.fileText.value, true);
    logDebug('App ready with 3D viewer.');
  } else {
    logDebug('App ready (3D viewer disabled).');
  }
  updateTrefftz(Number(els.cl.value));
  resetTrimSeed();
  applyTrim({ useSeed: false });
}

bootApp().catch((err) => logDebug(`Boot failed: ${err?.message ?? err}`));
