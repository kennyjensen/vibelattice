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
  fileSummary: document.getElementById('fileSummary'),
  fileAircraftName: document.getElementById('fileAircraftName'),
  fileNSurf: document.getElementById('fileNSurf'),
  fileNStrip: document.getElementById('fileNStrip'),
  fileNVort: document.getElementById('fileNVort'),
  fileSref: document.getElementById('fileSref'),
  fileCref: document.getElementById('fileCref'),
  fileBref: document.getElementById('fileBref'),
  fileXref: document.getElementById('fileXref'),
  fileYref: document.getElementById('fileYref'),
  fileZref: document.getElementById('fileZref'),
  fileEditor: document.getElementById('fileEditor'),
  fileText: document.getElementById('fileText'),
  fileHighlight: document.getElementById('fileHighlight'),
  airfoilFilesPanel: document.getElementById('airfoilFilesPanel'),
  airfoilFilesList: document.getElementById('airfoilFilesList'),
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
  outMach: document.getElementById('outMach'),
  outPb2v: document.getElementById('outPb2v'),
  outQc2v: document.getElementById('outQc2v'),
  outRb2v: document.getElementById('outRb2v'),
  outBank: document.getElementById('outBank'),
  outCXtot: document.getElementById('outCXtot'),
  outCYtot: document.getElementById('outCYtot'),
  outCZtot: document.getElementById('outCZtot'),
  outCltot: document.getElementById('outCltot'),
  outCmtot: document.getElementById('outCmtot'),
  outCntot: document.getElementById('outCntot'),
  outCL: document.getElementById('outCL'),
  outCD: document.getElementById('outCD'),
  outCDvis: document.getElementById('outCDvis'),
  outCDind: document.getElementById('outCDind'),
  outEff: document.getElementById('outEff'),
  outV: document.getElementById('outV'),
  outRad: document.getElementById('outRad'),
  outFac: document.getElementById('outFac'),
  outThe: document.getElementById('outThe'),
  outCM: document.getElementById('outCM'),
  outRates: document.getElementById('outRates'),
  outDef: document.getElementById('outDef'),
  outControlRows: document.getElementById('outControlRows'),
  outStability: document.getElementById('outStability'),
  outStabilityNeutral: document.getElementById('outStabilityNeutral'),
  outBodyDeriv: document.getElementById('outBodyDeriv'),
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
const TREFFTZ_BUSY_SHOW_DELAY_MS = 120;
const TREFFTZ_BUSY_MIN_VISIBLE_MS = 220;
let trefftzBusyVisible = false;
let trefftzBusyShownAt = 0;
let trefftzBusyShowTimer = null;
let trefftzBusyHideTimer = null;
let trimRequestId = 0;
let execRequestId = 0;
let autoTrimTimer = null;
let lastTrimState = null;
let loadingGroup = null;
let fileSummarySyncing = false;
const AVL_KEYWORDS = new Set([
  'SURFACE', 'SECTION', 'BODY', 'PATCH', 'COMPONENT', 'INDEX', 'YDUPLICATE', 'NOWAKE',
  'NOALBE', 'NOLOAD', 'NOSTALL', 'SCALE', 'TRANSLATE', 'ANGLE', 'NACA', 'AIRFOIL',
  'AFILE', 'BFILE', 'CONTROL', 'DESIGN', 'CLAF', 'CDCL', 'MACH', 'SREF', 'CREF',
  'BREF', 'XREF', 'YREF', 'ZREF',
]);
const FILE_EDITOR_FONT = {
  minPx: 8,
  maxPx: 13,
};
let fileMeasureCtx = null;

function escapeHtml(text) {
  return text
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#39;');
}

function highlightAvlCode(code) {
  if (!code) return '';
  const tokenRe = /"(?:[^"\\]|\\.)*"|[-+]?(?:\d*\.\d+|\d+)(?:[Ee][-+]?\d+)?|[A-Za-z_][A-Za-z0-9_]*|[,:()]/g;
  let html = '';
  let last = 0;
  let match = tokenRe.exec(code);
  while (match) {
    const [token] = match;
    const start = match.index;
    html += escapeHtml(code.slice(last, start));
    if (token.startsWith('"')) {
      html += `<span class="avl-token-string">${escapeHtml(token)}</span>`;
    } else if (/^[-+]?(?:\d*\.\d+|\d+)(?:[Ee][-+]?\d+)?$/.test(token)) {
      html += `<span class="avl-token-number">${token}</span>`;
    } else if (/^[A-Za-z_]/.test(token)) {
      const upper = token.toUpperCase();
      if (AVL_KEYWORDS.has(upper)) {
        html += `<span class="avl-token-keyword">${escapeHtml(token)}</span>`;
      } else {
        html += escapeHtml(token);
      }
    } else {
      html += `<span class="avl-token-operator">${escapeHtml(token)}</span>`;
    }
    last = start + token.length;
    match = tokenRe.exec(code);
  }
  html += escapeHtml(code.slice(last));
  return html;
}

function highlightAvlText(text) {
  if (!text) return ' ';
  const lines = text.split('\n');
  const rendered = lines.map((line) => {
    const bang = line.indexOf('!');
    const hash = line.indexOf('#');
    let commentIndex = -1;
    if (bang >= 0 && hash >= 0) commentIndex = Math.min(bang, hash);
    else commentIndex = Math.max(bang, hash);
    if (commentIndex < 0) return highlightAvlCode(line);
    const code = line.slice(0, commentIndex);
    const comment = line.slice(commentIndex);
    return `${highlightAvlCode(code)}<span class="avl-token-comment">${escapeHtml(comment)}</span>`;
  });
  return rendered.join('\n');
}

function getCssVarColor(name, fallback) {
  try {
    const value = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
    return value || fallback;
  } catch {
    return fallback;
  }
}


function renderFileHighlight() {
  if (!els.fileHighlight || !els.fileText) return;
  els.fileHighlight.innerHTML = highlightAvlText(els.fileText.value);
}

function fitFileEditorFontSize() {
  if (!els.fileEditor || !els.fileText) return;
  if (!fileMeasureCtx) {
    const measureCanvas = document.createElement('canvas');
    if (!measureCanvas || typeof measureCanvas.getContext !== 'function') return;
    fileMeasureCtx = measureCanvas.getContext('2d');
    if (!fileMeasureCtx) return;
  }
  const available = Math.max(0, els.fileText.clientWidth - 20);
  if (!available) return;
  const lines = (els.fileText.value || '').split('\n');
  let longest = '';
  for (const line of lines) {
    if (line.length > longest.length) longest = line;
  }
  const sample = longest.replaceAll('\t', '  ');
  if (!sample) {
    els.fileEditor.style.setProperty('--file-font-size', `${FILE_EDITOR_FONT.maxPx}px`);
    return;
  }
  fileMeasureCtx.font = `${FILE_EDITOR_FONT.maxPx}px Consolas, "Courier New", monospace`;
  const measured = fileMeasureCtx.measureText(sample).width;
  if (!measured) return;
  const fitted = FILE_EDITOR_FONT.maxPx * (available / measured);
  const px = Math.max(FILE_EDITOR_FONT.minPx, Math.min(FILE_EDITOR_FONT.maxPx, fitted));
  els.fileEditor.style.setProperty('--file-font-size', `${px.toFixed(2)}px`);
}

function syncFileEditorScroll() {
  if (!els.fileHighlight || !els.fileText) return;
  els.fileHighlight.scrollTop = els.fileText.scrollTop;
  els.fileHighlight.scrollLeft = els.fileText.scrollLeft;
}

function setFileTextValue(text) {
  if (!els.fileText) return;
  els.fileText.value = text;
  fitFileEditorFontSize();
  renderFileHighlight();
  syncFileEditorScroll();
}

function isAvlCommentLine(line) {
  const t = String(line || '').trim();
  return !t || t.startsWith('#') || t.startsWith('!') || t.startsWith('%');
}

function replaceAvlTitleLine(text, title) {
  const lineBreak = text.includes('\r\n') ? '\r\n' : '\n';
  const lines = String(text || '').split(/\r?\n/);
  for (let i = 0; i < lines.length; i += 1) {
    if (isAvlCommentLine(lines[i])) continue;
    lines[i] = title;
    return lines.join(lineBreak);
  }
  return title;
}

function computeModelCounts(model) {
  if (!model?.surfaces?.length) return { surfaces: 0, strips: 0, vortices: 0 };
  let strips = 0;
  let vortices = 0;
  let surfaces = 0;
  model.surfaces.forEach((surf) => {
    const nvc = Number(surf.nChord || 1);
    let nvs = Number(surf.nSpan || 0);
    if (nvs === 0) {
      nvs = surf.sections.slice(0, -1).reduce((sum, sec) => sum + Number(sec.nSpan ?? 0), 0);
    }
    const copies = typeof surf.yduplicate === 'number' ? 2 : 1;
    strips += nvs * copies;
    vortices += nvs * nvc * copies;
    surfaces += copies;
  });
  return { surfaces, strips, vortices };
}

function renderFileHeaderSummary(header, model = null) {
  if (!els.fileSummary) return;
  const hasHeader = Boolean(header && typeof header === 'object');
  els.fileSummary.classList.toggle('hidden', !hasHeader);
  if (!hasHeader) return;

  const show = (el, value, digits = 4) => {
    if (!el) return;
    el.textContent = Number.isFinite(Number(value)) ? fmt(Number(value), digits) : '-';
  };

  fileSummarySyncing = true;
  if (els.fileAircraftName) {
    els.fileAircraftName.value = String(header.title || '').trim();
  }
  fileSummarySyncing = false;

  const counts = computeModelCounts(model);
  if (els.fileNSurf) els.fileNSurf.textContent = String(counts.surfaces);
  if (els.fileNStrip) els.fileNStrip.textContent = String(counts.strips);
  if (els.fileNVort) els.fileNVort.textContent = String(counts.vortices);
  show(els.fileSref, header.sref, 2);
  show(els.fileCref, header.cref, 2);
  show(els.fileBref, header.bref, 2);
  show(els.fileXref, header.xref, 2);
  show(els.fileYref, header.yref, 2);
  show(els.fileZref, header.zref, 2);
}

function applyAircraftNameRename() {
  if (!els.fileAircraftName || !els.fileText) return;
  if (fileSummarySyncing) return;
  const nextName = String(els.fileAircraftName.value || '').trim();
  if (!nextName) {
    renderFileHeaderSummary(uiState.modelHeader);
    return;
  }
  const updated = replaceAvlTitleLine(els.fileText.value, nextName);
  if (!updated || updated === els.fileText.value) return;
  uiState.text = updated;
  setFileTextValue(updated);
  loadGeometryFromText(uiState.text, false);
  resetTrimSeed();
  applyTrim({ useSeed: false });
}

function updateTrefftzBusy() {
  const stage = els.trefftz?.parentElement;
  if (!stage) return;
  const busy = Boolean(trimInProgress || execInProgress);
  const setBusyVisible = (visible) => {
    if (trefftzBusyVisible === visible) return;
    trefftzBusyVisible = visible;
    if (visible) trefftzBusyShownAt = Date.now();
    stage.classList.toggle('trefftz-busy', visible);
  };

  if (busy) {
    if (trefftzBusyHideTimer) {
      clearTimeout(trefftzBusyHideTimer);
      trefftzBusyHideTimer = null;
    }
    if (trefftzBusyVisible || trefftzBusyShowTimer) return;
    trefftzBusyShowTimer = setTimeout(() => {
      trefftzBusyShowTimer = null;
      if (trimInProgress || execInProgress) setBusyVisible(true);
    }, TREFFTZ_BUSY_SHOW_DELAY_MS);
    return;
  }

  if (trefftzBusyShowTimer) {
    clearTimeout(trefftzBusyShowTimer);
    trefftzBusyShowTimer = null;
  }
  if (!trefftzBusyVisible) return;
  if (trefftzBusyHideTimer) clearTimeout(trefftzBusyHideTimer);
  const elapsed = Date.now() - trefftzBusyShownAt;
  const remaining = Math.max(0, TREFFTZ_BUSY_MIN_VISIBLE_MS - elapsed);
  trefftzBusyHideTimer = setTimeout(() => {
    trefftzBusyHideTimer = null;
    if (!trimInProgress && !execInProgress) setBusyVisible(false);
  }, remaining);
}

function initTopNav() {
  if (!els.appRoot || !els.navSettings || !els.navPlots || !els.navOutputs) return;
  const navItems = [
    { btn: els.navSettings, col: els.settingsCol },
    { btn: els.navPlots, col: els.plotsCol },
    { btn: els.navOutputs, col: els.outputsCol },
  ];
  const setActiveIndex = (index) => {
    const clamped = Math.max(0, Math.min(index, navItems.length - 1));
    navItems.forEach(({ btn }, idx) => btn?.classList?.toggle('active', idx === clamped));
  };

  const updateActiveFromScroll = () => {
    const pageWidth = els.appRoot.clientWidth;
    if (!pageWidth) return;
    const index = Math.round(els.appRoot.scrollLeft / pageWidth);
    setActiveIndex(index);
  };

  navItems.forEach(({ btn }, idx) => {
    if (!btn) return;
    if (btn.dataset) btn.dataset.index = String(idx);
    btn.addEventListener('click', () => {
      const pageWidth = els.appRoot.clientWidth;
      els.appRoot.scrollTo({ left: pageWidth * idx, behavior: 'smooth' });
      setActiveIndex(idx);
    });
  });

  let raf = 0;
  els.appRoot.addEventListener('scroll', () => {
    if (raf) return;
    raf = requestAnimationFrame(() => {
      raf = 0;
      updateActiveFromScroll();
    });
  }, { passive: true });

  requestAnimationFrame(() => {
    const hasMatchMedia = typeof window.matchMedia === 'function';
    if (hasMatchMedia && window.matchMedia('(max-width: 900px)').matches) {
      const defaultIndex = 1;
      const pageWidth = els.appRoot.clientWidth;
      if (pageWidth) {
        els.appRoot.scrollLeft = pageWidth * defaultIndex;
        setActiveIndex(defaultIndex);
        return;
      }
    }
    setActiveIndex(0);
  });
}

function initPanelCollapse() {
  const panels = document.querySelectorAll('.panel');
  panels.forEach((panel, idx) => {
    const title = panel.querySelector('.panel-title');
    if (!title || title.querySelector('.panel-toggle')) return;
    const rawText = title.textContent?.trim() || `Panel ${idx + 1}`;
    title.textContent = '';
    const textEl = document.createElement('span');
    textEl.className = 'panel-title-text';
    textEl.textContent = rawText;
    const toggle = document.createElement('button');
    toggle.type = 'button';
    toggle.className = 'panel-toggle';

    const applyState = (collapsed) => {
      panel.classList.toggle('collapsed', collapsed);
      toggle.textContent = collapsed ? '▾' : '▴';
      toggle.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
      toggle.setAttribute('aria-label', `${collapsed ? 'Expand' : 'Collapse'} ${rawText}`);
      toggle.title = collapsed ? 'Expand panel' : 'Collapse panel';
    };

    toggle.addEventListener('click', () => {
      applyState(!panel.classList.contains('collapsed'));
    });

    title.appendChild(textEl);
    title.appendChild(toggle);
    applyState(false);
  });
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
const airfoilUploadInvalid = new Set();
const providedAirfoilFiles = new Set();
const airfoilDisplayNames = new Map();
let requiredAirfoilFiles = [];

function fmt(value, digits = 3) {
  if (!Number.isFinite(value)) return '-';
  return Number(value).toFixed(digits);
}

function fmtSignedFixed(value, digits = 6) {
  if (!Number.isFinite(value)) return ' -';
  const abs = Math.abs(Number(value)).toFixed(digits);
  return `${value < 0 ? '-' : ' '}${abs}`;
}

function fmtSignedNoPad(value, digits = 6) {
  if (!Number.isFinite(value)) return '-';
  const abs = Math.abs(Number(value)).toFixed(digits);
  return `${value < 0 ? '-' : ''}${abs}`;
}

function formatSurfaceDisplayName(rawName, index = 1) {
  const name = String(rawName || `Surf ${index}`);
  if (name.endsWith('-ydup')) return `${name.slice(0, -5)} (YDUP)`;
  if (name.endsWith('-ysym')) return `${name.slice(0, -5)} (YSYM)`;
  if (name.endsWith('-zsym')) return `${name.slice(0, -5)} (ZSYM)`;
  return name;
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

function renderControlRows(entries) {
  if (!els.outControlRows) return;
  if (!entries?.length) {
    els.outControlRows.innerHTML = '';
    return;
  }
  const maxName = entries.reduce((m, e) => Math.max(m, String(e?.name ?? '').length), 0);
  const html = entries.map((entry, i) => {
    const rawName = String(entry?.name ?? 'control');
    const paddedName = rawName.padStart(maxName, ' ');
    const name = escapeHtml(paddedName);
    const value = escapeHtml(String(entry?.value ?? '-'));
    return `<div class="force-cell control-row" style="grid-row:${4 + i};"><span class="force-control-name">${name}</span> = <strong>${value}</strong></div>`;
  }).join('');
  els.outControlRows.innerHTML = html;
}

function renderStabilityGrid(result) {
  if (!els.outStability) return;
  const clU = result?.CLTOT_U;
  const cyU = result?.CYTOT_U;
  const cmU = result?.CMTOT_U;
  const clD = result?.CLTOT_D;
  const cyD = result?.CYTOT_D;
  const cmD = result?.CMTOT_D;
  const vinfA = result?.VINF_A;
  const vinfB = result?.VINF_B;
  if (!clU || !cyU || !cmU || !vinfA || !vinfB) {
    els.outStability.textContent = '-';
    if (els.outStabilityNeutral) els.outStabilityNeutral.textContent = 'Xnp = -';
    return;
  }

  const dot3 = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
  const dA = (arr, add = 0) => dot3(arr, vinfA) + add;
  const dB = (arr) => dot3(arr, vinfB);
  const clxU = cmU[0] || [];
  const cmyU = cmU[1] || [];
  const cnzU = cmU[2] || [];

  const rows = [];
  rows.push(['', 'CL', 'CY', 'Cl', 'Cm', 'Cn']);
  rows.push(['α', dA(clU, result?.CLTOT_A ?? 0), dA(cyU, 0), dA(clxU, 0), dA(cmyU, 0), dA(cnzU, 0)]);
  rows.push(['β', dB(clU), dB(cyU), dB(clxU), dB(cmyU), dB(cnzU)]);
  rows.push(['p', clU[3] ?? 0, cyU[3] ?? 0, clxU[3] ?? 0, cmyU[3] ?? 0, cnzU[3] ?? 0]);
  rows.push(['q', clU[4] ?? 0, cyU[4] ?? 0, clxU[4] ?? 0, cmyU[4] ?? 0, cnzU[4] ?? 0]);
  rows.push(['r', clU[5] ?? 0, cyU[5] ?? 0, clxU[5] ?? 0, cmyU[5] ?? 0, cnzU[5] ?? 0]);

  const nControl = uiState.modelCache?.controlMap?.size
    || Math.max((clD?.length ?? 1) - 1, (cyD?.length ?? 1) - 1, ((cmD?.[0]?.length ?? 1) - 1));
  const momentX = cmD?.[0] || [];
  const momentY = cmD?.[1] || [];
  const momentZ = cmD?.[2] || [];
  for (let i = 1; i <= nControl; i += 1) {
    rows.push([
      `d${i}`,
      clD?.[i] ?? 0,
      cyD?.[i] ?? 0,
      momentX[i] ?? 0,
      momentY[i] ?? 0,
      momentZ[i] ?? 0,
    ]);
  }

  const derivSuffix = (rowHead) => {
    if (rowHead === 'alpha' || rowHead === 'α') return 'a';
    if (rowHead === 'beta' || rowHead === 'β') return 'b';
    return rowHead;
  };

  const html = rows.map((row, rIdx) => row.map((cell, cIdx) => {
    if (rIdx === 0 || cIdx === 0) {
      const cls = (rIdx === 0 && cIdx > 0) ? 'stability-cell stability-head stability-colhead' : 'stability-cell stability-head';
      return `<div class="${cls}">${escapeHtml(String(cell))}</div>`;
    }
    const rowHead = String(rows[rIdx][0]);
    const colHead = String(rows[0][cIdx]);
    const deriv = `${colHead}${derivSuffix(rowHead)}`;
    return `<div class="stability-cell stability-val" title="${escapeHtml(deriv)}"><strong class="stability-num">${fmtSignedFixed(Number(cell), 6)}</strong></div>`;
  }).join('')).join('');
  els.outStability.innerHTML = html;

  if (els.outStabilityNeutral) {
    const cla = dA(clU, result?.CLTOT_A ?? 0);
    const cma = dA(cmyU, 0);
    const xrefRaw = Number.isFinite(result?.XREF) ? Number(result.XREF) : Number(uiState.modelHeader?.xref);
    const crefRaw = Number.isFinite(result?.CREF) ? Number(result.CREF) : Number(uiState.modelHeader?.cref);
    let xnp = Number.NaN;
    if (Number.isFinite(cla) && Math.abs(cla) > 1e-9 && Number.isFinite(cma)
        && Number.isFinite(xrefRaw) && Number.isFinite(crefRaw) && Math.abs(crefRaw) > 1e-9) {
      xnp = (xrefRaw / crefRaw) - (cma / cla);
    }
    els.outStabilityNeutral.textContent = `Xnp = ${Number.isFinite(xnp) ? fmtSignedNoPad(xnp, 6) : '-'}`;
  }
}

function renderBodyDerivGrid(result) {
  if (!els.outBodyDeriv) return;
  const cfU = result?.CFTOT_U;
  const cmU = result?.CMTOT_U;
  const cfD = result?.CFTOT_D;
  const cmD = result?.CMTOT_D;
  const vinfA = result?.VINF_A;
  const vinfB = result?.VINF_B;
  if (!cfU || !cmU || !vinfA || !vinfB) {
    els.outBodyDeriv.textContent = '-';
    return;
  }

  const dot3 = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
  const dA = (arr) => dot3(arr, vinfA);
  const dB = (arr) => dot3(arr, vinfB);

  const cxU = cfU[0] || [];
  const cyU = cfU[1] || [];
  const czU = cfU[2] || [];
  const clU = cmU[0] || [];
  const cmY = cmU[1] || [];
  const cnU = cmU[2] || [];

  const rows = [];
  rows.push(['', 'CX', 'CY', 'CZ', 'Cl', 'Cm', 'Cn']);
  rows.push(['α', dA(cxU), dA(cyU), dA(czU), dA(clU), dA(cmY), dA(cnU)]);
  rows.push(['β', dB(cxU), dB(cyU), dB(czU), dB(clU), dB(cmY), dB(cnU)]);
  rows.push(['p', cxU[3] ?? 0, cyU[3] ?? 0, czU[3] ?? 0, clU[3] ?? 0, cmY[3] ?? 0, cnU[3] ?? 0]);
  rows.push(['q', cxU[4] ?? 0, cyU[4] ?? 0, czU[4] ?? 0, clU[4] ?? 0, cmY[4] ?? 0, cnU[4] ?? 0]);
  rows.push(['r', cxU[5] ?? 0, cyU[5] ?? 0, czU[5] ?? 0, clU[5] ?? 0, cmY[5] ?? 0, cnU[5] ?? 0]);

  const nControl = uiState.modelCache?.controlMap?.size
    || Math.max((cfD?.[0]?.length ?? 1) - 1, (cmD?.[0]?.length ?? 1) - 1);
  const cxD = cfD?.[0] || [];
  const cyD = cfD?.[1] || [];
  const czD = cfD?.[2] || [];
  const clD = cmD?.[0] || [];
  const cmD1 = cmD?.[1] || [];
  const cnD = cmD?.[2] || [];
  for (let i = 1; i <= nControl; i += 1) {
    rows.push([
      `d${i}`,
      cxD[i] ?? 0,
      cyD[i] ?? 0,
      czD[i] ?? 0,
      clD[i] ?? 0,
      cmD1[i] ?? 0,
      cnD[i] ?? 0,
    ]);
  }

  const derivSuffix = (rowHead) => {
    if (rowHead === 'α') return 'a';
    if (rowHead === 'β') return 'b';
    return rowHead;
  };

  const html = rows.map((row, rIdx) => row.map((cell, cIdx) => {
    if (rIdx === 0 || cIdx === 0) {
      const cls = (rIdx === 0 && cIdx > 0) ? 'stability-cell stability-head stability-colhead' : 'stability-cell stability-head';
      return `<div class="${cls}">${escapeHtml(String(cell))}</div>`;
    }
    const rowHead = String(rows[rIdx][0]);
    const colHead = String(rows[0][cIdx]);
    const deriv = `${colHead}${derivSuffix(rowHead)}`;
    return `<div class="stability-cell stability-val" title="${escapeHtml(deriv)}"><strong class="stability-num">${fmtSignedFixed(Number(cell), 6)}</strong></div>`;
  }).join('')).join('');
  els.outBodyDeriv.innerHTML = html;
}

function renderSurfaceForcesGrid(result) {
  if (!els.outForcesSurface) return;
  const clSurf = result?.CLSURF;
  const cdSurf = result?.CDSURF;
  const cySurf = result?.CYSURF;
  if (!clSurf || !cdSurf || !cySurf || clSurf.length <= 1) {
    els.outForcesSurface.textContent = '-';
    return;
  }
  const model = uiState.modelCache;
  const execNames = Array.isArray(uiState.execSurfaceNames) ? uiState.execSurfaceNames : null;
  const rows = [['', 'CL', 'CD', 'CY', 'Cl', 'Cm', 'Cn']];
  for (let i = 1; i < clSurf.length; i += 1) {
    const raw = execNames?.[i - 1] ?? model?.surfaces?.[i - 1]?.name ?? `Surf ${i}`;
    const name = formatSurfaceDisplayName(raw, i);
    const cm = result?.CMSURF?.[i] || [0, 0, 0];
    rows.push([
      name,
      clSurf[i] ?? 0,
      cdSurf[i] ?? 0,
      cySurf[i] ?? 0,
      cm[0] ?? 0,
      cm[1] ?? 0,
      cm[2] ?? 0,
    ]);
  }

  const html = rows.map((row, rIdx) => row.map((cell, cIdx) => {
    if (rIdx === 0 || cIdx === 0) {
      const cls = (rIdx === 0 && cIdx > 0) ? 'stability-cell stability-head stability-colhead' : 'stability-cell stability-head';
      if (cIdx === 0 && rIdx > 0) {
        const text = escapeHtml(String(cell));
        return `<div class="${cls} surface-name-cell" title="${text}"><span class="surface-name-text">${text}</span></div>`;
      }
      return `<div class="${cls}">${escapeHtml(String(cell))}</div>`;
    }
    return `<div class="stability-cell stability-val"><strong class="stability-num">${fmtSignedFixed(Number(cell), 4)}</strong></div>`;
  }).join('')).join('');
  els.outForcesSurface.innerHTML = html;
}

function buildExecSurfaceNames(model) {
  if (!model?.surfaces?.length) return [];
  const names = [];
  model.surfaces.forEach((surf, idx) => {
    const base = String(surf?.name || `Surf ${idx + 1}`);
    names.push(base);
    if (typeof surf?.yduplicate === 'number') {
      names.push(`${base} (YDUP)`);
    }
  });
  return names;
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
    const cl = Number(els.cl?.value || 0);
    const vee = Number(els.vel?.value || 0);
    const rad = Number(els.radLoop?.value || 0);

    if (uiState.loopDriver === 'cl' && cl > 0 && rho > 0 && mass > 0) {
      const radNew = mass / (0.5 * rho * srefD * cl);
      if (Number.isFinite(radNew)) setNumericInput(els.radLoop, radNew, 3);
    } else if (uiState.loopDriver === 'rad' && rad > 0 && rho > 0 && mass > 0) {
      const clNew = mass / (0.5 * rho * srefD * rad);
      if (Number.isFinite(clNew)) setNumericInput(els.cl, clNew, 3);
    }

    const clUse = Number(els.cl?.value || 0);
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
  const value = Number(els.cl?.value || 0);
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

function focusConstraintCell(rowIndex, column, selectValue = false) {
  if (!Array.isArray(els.constraintRows) || rowIndex < 0 || rowIndex >= els.constraintRows.length) return;
  const row = els.constraintRows[rowIndex];
  if (!row) return;
  const target = column === 'value'
    ? row.querySelector('.constraint-value')
    : row.querySelector('.constraint-select');
  if (!target) return;
  target.focus();
  if (selectValue && target instanceof HTMLInputElement && typeof target.select === 'function') {
    target.select();
  }
}

function focusConstraintValueByVariable(variable) {
  if (!Array.isArray(els.constraintRows) || !variable) return false;
  const idx = els.constraintRows.findIndex((row) => row?.dataset?.var === variable);
  if (idx < 0) return false;
  focusConstraintCell(idx, 'value', true);
  return true;
}

function handleConstraintShortcutKey(evt) {
  if (evt.defaultPrevented || evt.ctrlKey || evt.metaKey || evt.altKey) return;
  if (!evt.key || evt.key.length !== 1) return;

  const active = document.activeElement;
  const inConstraintPanel = Boolean(active?.closest?.('#constraintTable'));
  if (!inConstraintPanel) {
    if (active instanceof HTMLTextAreaElement) return;
    if (active instanceof HTMLInputElement && active.type !== 'number') return;
    if (active instanceof HTMLSelectElement) return;
  }

  const key = evt.key.toLowerCase();
  const byShortcut = {
    a: 'alpha',
    b: 'beta',
    r: 'p',
    p: 'q',
    y: 'r',
  };
  const variable = byShortcut[key];
  if (!variable) return;
  if (!focusConstraintValueByVariable(variable)) return;
  evt.preventDefault();
}

function handleConstraintSelectKeydown(evt) {
  if (!(evt.currentTarget instanceof HTMLElement)) return;
  const rowEl = evt.currentTarget.closest('.constraint-row');
  if (!rowEl) return;
  const rowIndex = els.constraintRows.indexOf(rowEl);
  if (rowIndex < 0) return;

  if (evt.key === 'ArrowUp' || evt.key === 'ArrowDown') {
    evt.preventDefault();
    const delta = evt.key === 'ArrowUp' ? -1 : 1;
    const nextIndex = Math.max(0, Math.min(els.constraintRows.length - 1, rowIndex + delta));
    focusConstraintCell(nextIndex, 'constraint');
    return;
  }

  if (evt.key === 'ArrowRight') {
    evt.preventDefault();
    focusConstraintCell(rowIndex, 'value', true);
  }
}

function handleConstraintValueKeydown(evt) {
  if (!(evt.currentTarget instanceof HTMLElement)) return;
  if (evt.key !== 'ArrowLeft') return;
  const rowEl = evt.currentTarget.closest('.constraint-row');
  if (!rowEl) return;
  const rowIndex = els.constraintRows.indexOf(rowEl);
  if (rowIndex < 0) return;
  evt.preventDefault();
  focusConstraintCell(rowIndex, 'constraint');
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
    select.addEventListener('keydown', handleConstraintSelectKeydown);
    cellConstraint.appendChild(select);

    const cellValue = document.createElement('div');
    cellValue.className = 'constraint-cell';
    const input = document.createElement('input');
    input.className = 'constraint-value';
    input.type = 'number';
    input.step = '0.1';
    input.value = row.value;
    input.addEventListener('keydown', handleConstraintValueKeydown);
    cellValue.appendChild(input);

    rowEl.appendChild(cellVar);
    rowEl.appendChild(cellConstraint);
    rowEl.appendChild(cellValue);
    els.constraintTable.appendChild(rowEl);
  });

  // Defensive cleanup: remove any legacy header row if present.
  Array.from(els.constraintTable.querySelectorAll('.constraint-row')).forEach((row) => {
    if (row.querySelectorAll('.constraint-head').length === 3) {
      row.remove();
    }
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
window.addEventListener('keydown', handleConstraintShortcutKey);

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

function readFileAsText(file) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => resolve(String(reader.result || ''));
    reader.onerror = () => reject(reader.error || new Error(`Failed to read ${file?.name || 'file'}`));
    reader.readAsText(file);
  });
}

function basenamePath(path) {
  return String(path || '').split(/[\\/]/).pop() || String(path || '');
}

function resolveProvidedAirfoilKey(path) {
  const cleanPath = normalizeAirfoilPath(path);
  if (!cleanPath) return null;
  if (providedAirfoilFiles.has(cleanPath) && airfoilCache.has(cleanPath)) return cleanPath;
  const base = basenamePath(cleanPath).toLowerCase();
  for (const key of providedAirfoilFiles) {
    if (!airfoilCache.has(key)) continue;
    if (basenamePath(key).toLowerCase() === base) return key;
  }
  return null;
}

function applyAirfoilTextForKey(path, text) {
  const key = normalizeAirfoilPath(path);
  if (!key) return false;
  const { coords, displayName } = parseAirfoilFileDetails(text);
  if (!coords.length) {
    airfoilUploadInvalid.add(key);
    providedAirfoilFiles.delete(key);
    airfoilCache.delete(key);
    airfoilDisplayNames.delete(key);
    return false;
  }
  airfoilUploadInvalid.delete(key);
  airfoilFailed.delete(key);
  providedAirfoilFiles.add(key);
  airfoilCache.set(key, coords);
  if (displayName) airfoilDisplayNames.set(key, displayName);
  else airfoilDisplayNames.delete(key);
  return true;
}

async function handleFileLoad(file) {
  const text = await readFileAsText(file);
  uiState.text = text;
  uiState.filename = file.name;
  setFileTextValue(uiState.text);
  els.fileMeta.textContent = `Loaded: ${file.name} (${file.size} bytes)`;
  loadGeometryFromText(uiState.text, true);
  resetTrimSeed();
  applyTrim({ useSeed: false });
  logDebug(`Loaded file: ${file.name}`);
}

async function handleFileSelection(files) {
  const all = Array.from(files || []);
  if (!all.length) return;

  const textFiles = [];
  for (const file of all) {
    const lower = String(file.name || '').toLowerCase();
    if (lower.endsWith('.dat')) {
      try {
        const text = await readFileAsText(file);
        const ok = applyAirfoilTextForKey(file.name, text);
        if (ok) logDebug(`Loaded airfoil dependency: ${file.name}`);
        else logDebug(`Failed to parse airfoil dependency: ${file.name}`);
      } catch {
        logDebug(`Failed to read airfoil dependency: ${file.name}`);
      }
      continue;
    }
    if (lower.endsWith('.avl') || lower.endsWith('.run') || lower.endsWith('.txt')) {
      textFiles.push(file);
    }
  }

  const primary = textFiles.find((f) => String(f.name || '').toLowerCase().endsWith('.avl'))
    || textFiles.find((f) => String(f.name || '').toLowerCase().endsWith('.run'))
    || textFiles[0]
    || null;

  if (primary) {
    await handleFileLoad(primary);
  } else {
    renderRequiredAirfoilFiles();
    scheduleAutoTrim();
    els.fileMeta.textContent = `Loaded dependencies: ${all.length} file(s)`;
  }

  // Repaint dependency status after model parse.
  renderRequiredAirfoilFiles();
}

async function loadDefaultAVL() {
  const existing = els.fileText.value?.trim();
  if (existing) {
    uiState.text = existing;
    renderFileHighlight();
    syncFileEditorScroll();
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
      setFileTextValue(text);
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
    setFileTextValue(embedded);
    if (els.fileMeta) els.fileMeta.textContent = 'Loaded: plane.avl (embedded)';
    logDebug('Loaded embedded default file: plane.avl');
    return;
  }
  logDebug('Default file plane.avl not found.');
}

els.fileInput.addEventListener('change', async (evt) => {
  const files = evt.target.files;
  if (files?.length) await handleFileSelection(files);
  evt.target.value = '';
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
  fitFileEditorFontSize();
  renderFileHighlight();
  syncFileEditorScroll();
  clearTimeout(fileUpdateTimer);
  fileUpdateTimer = setTimeout(() => {
    uiState.text = els.fileText.value;
    loadGeometryFromText(uiState.text, true);
  }, 250);
});
els.fileText.addEventListener('scroll', syncFileEditorScroll);
window.addEventListener('resize', fitFileEditorFontSize);
els.fileAircraftName?.addEventListener('change', applyAircraftNameRename);
els.fileAircraftName?.addEventListener('keydown', (evt) => {
  if (evt.key !== 'Enter') return;
  evt.preventDefault();
  applyAircraftNameRename();
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
    state.PARVAL[idx2(IPVEE, IR, state.IPTOT)] = Number(els.vel.value);
  } else {
    state.PARVAL[idx2(IPPHI, IR, state.IPTOT)] = Number(els.bank.value);
    state.PARVAL[idx2(IPTHE, IR, state.IPTOT)] = 0.0;
    state.PARVAL[idx2(IPVEE, IR, state.IPTOT)] = Number(els.vel.value);
  }
  state.PARVAL[idx2(IPRHO, IR, state.IPTOT)] = Number(els.rho.value);
  state.PARVAL[idx2(IPGEE, IR, state.IPTOT)] = Number(els.gee.value);
  state.PARVAL[idx2(IPMASS, IR, state.IPTOT)] = Number(els.mass.value);
  state.PARVAL[idx2(IPCL, IR, state.IPTOT)] = Number(els.cl.value);
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

  if (els.outAlpha) els.outAlpha.textContent = `${fmt(findVar('alpha'), 2)} deg`;
  if (els.outBeta) els.outBeta.textContent = `${fmt(findVar('beta'), 2)} deg`;
  if (els.outBank) els.outBank.textContent = `${fmt(phi, 2)} deg`;
  if (els.outCL) els.outCL.textContent = fmt(cl, 5);
  if (els.outV) els.outV.textContent = `${fmt(vee, 2)} m/s`;
  if (els.outRad) els.outRad.textContent = rad > 0 ? `${fmt(rad, 2)} m` : 'level';
  if (els.outFac) els.outFac.textContent = fmt(fac, 3);
  if (els.outThe) els.outThe.textContent = `${fmt(the, 2)} deg`;
  if (els.outPb2v) els.outPb2v.textContent = fmt(findVar('p'), 5);
  if (els.outQc2v) els.outQc2v.textContent = fmt(findVar('q'), 5);
  if (els.outRb2v) els.outRb2v.textContent = fmt(findVar('r'), 5);
  if (els.outRates) els.outRates.textContent = `${fmt(findVar('p'), 2)}, ${fmt(findVar('q'), 2)}, ${fmt(findVar('r'), 2)}`;
  if (els.outDef) els.outDef.textContent = '-';
  renderControlRows([]);
  if (els.outMach) {
    const mach = Number(uiState.modelHeader?.mach);
    els.outMach.textContent = Number.isFinite(mach) ? fmt(mach, 3) : '-';
  }

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

  const panelBg = getCssVarColor('--panel', '#0f1115');
  ctx.fillStyle = panelBg;
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

    ctx.font = '10px Consolas, "Courier New", monospace';
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
    ctx.fillStyle = panelBg;
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
          const normalizedPath = normalizeAirfoilPath(apath);
          currentSection.airfoilFile = normalizedPath || null;
          if (normalizedPath) inlineAirfoils.push(normalizedPath);
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
  return parseAirfoilFileDetails(text).coords;
}

function normalizeAirfoilCoords(coords) {
  if (!Array.isArray(coords) || !coords.length) return [];
  const filtered = coords
    .map((pair) => [Number(pair?.[0]), Number(pair?.[1])])
    .filter(([x, z]) => Number.isFinite(x) && Number.isFinite(z));
  if (!filtered.length) return [];

  let minX = filtered[0][0];
  let maxX = filtered[0][0];
  for (const [x] of filtered) {
    if (x < minX) minX = x;
    if (x > maxX) maxX = x;
  }
  const spanX = Math.abs(maxX - minX);
  const leTol = Math.max(1e-8, spanX * 1e-5);
  const teTol = Math.max(1e-8, spanX * 1e-5);

  let leXSum = 0;
  let leZSum = 0;
  let leCount = 0;
  let teXSum = 0;
  let teZSum = 0;
  let teCount = 0;
  for (const [x, z] of filtered) {
    if (Math.abs(x - minX) <= leTol) {
      leXSum += x;
      leZSum += z;
      leCount += 1;
    }
    if (Math.abs(x - maxX) <= teTol) {
      teXSum += x;
      teZSum += z;
      teCount += 1;
    }
  }

  const leX = leCount ? (leXSum / leCount) : minX;
  const leZ = leCount ? (leZSum / leCount) : 0;
  const teX = teCount ? (teXSum / teCount) : maxX;
  const teZ = teCount ? (teZSum / teCount) : 0;

  let dx = teX - leX;
  let dz = teZ - leZ;
  let chord = Math.hypot(dx, dz);
  if (chord < 1e-9) {
    dx = 1;
    dz = 0;
    chord = 1;
  }

  // Rotate so the LE->TE chord lies on +x, then scale by chord.
  const cosT = dx / chord;
  const sinT = dz / chord;
  return filtered.map(([x, z]) => {
    const rx = x - leX;
    const rz = z - leZ;
    const xx = (rx * cosT + rz * sinT) / chord;
    const zz = (-rx * sinT + rz * cosT) / chord;
    return [xx, zz];
  });
}

function parseAirfoilFileDetails(text) {
  const lines = text.split(/\r?\n/);
  const coords = [];
  let displayName = null;
  let seenData = false;
  for (const raw of lines) {
    const trimmed = raw.trim();
    if (!trimmed) continue;
    if (trimmed.startsWith('#') || trimmed.startsWith('!') || trimmed.startsWith('%')) continue;
    const nums = trimmed.split(/\s+/).map((v) => Number(v)).filter((v) => Number.isFinite(v));
    if (nums.length >= 2) {
      coords.push([nums[0], nums[1]]);
      seenData = true;
      continue;
    }
    if (!seenData && !displayName) {
      displayName = trimmed;
    }
  }
  return { coords: normalizeAirfoilCoords(coords), displayName };
}

function normalizeAirfoilPath(path) {
  const raw = String(path || '').trim();
  if (!raw) return '';
  return raw.replace(/^["']+|["']+$/g, '').trim();
}

function setRequiredAirfoilFiles(paths) {
  const seen = new Set();
  requiredAirfoilFiles = [];
  (paths || []).forEach((entry) => {
    const p = normalizeAirfoilPath(entry);
    if (!p || seen.has(p)) return;
    seen.add(p);
    requiredAirfoilFiles.push(p);
  });
}

function getAirfoilRequirementStatus(path) {
  if (airfoilUploadInvalid.has(path)) return { state: 'invalid', text: 'Parse error' };
  if (resolveProvidedAirfoilKey(path)) return { state: 'ok', text: 'Ready' };
  if (airfoilPending.has(path)) return { state: 'loading', text: 'Loading…' };
  return { state: 'missing', text: 'Missing' };
}

function handleAirfoilUpload(path, file) {
  if (!file || !path) return;
  const reader = new FileReader();
  reader.onload = () => {
    try {
      const text = String(reader.result || '');
      applyAirfoilTextForKey(path, text);
      renderRequiredAirfoilFiles();
      if (uiState.text) {
        loadGeometryFromText(uiState.text, false);
      }
      scheduleAutoTrim();
    } catch {
      const key = normalizeAirfoilPath(path);
      if (key) {
        airfoilUploadInvalid.add(key);
        providedAirfoilFiles.delete(key);
        airfoilCache.delete(key);
        airfoilDisplayNames.delete(key);
      }
      renderRequiredAirfoilFiles();
    }
  };
  reader.readAsText(file);
}

function renderRequiredAirfoilFiles() {
  if (!els.airfoilFilesPanel || !els.airfoilFilesList) return;
  if (!requiredAirfoilFiles.length) {
    if (els.airfoilFilesPanel.classList?.add) els.airfoilFilesPanel.classList.add('hidden');
    els.airfoilFilesList.innerHTML = '';
    return;
  }
  if (els.airfoilFilesPanel.classList?.remove) els.airfoilFilesPanel.classList.remove('hidden');
  els.airfoilFilesList.innerHTML = '';
  requiredAirfoilFiles.forEach((path) => {
    const status = getAirfoilRequirementStatus(path);
    const row = document.createElement('div');
    row.className = `airfoil-file-row ${status.state}`;

    const name = document.createElement('div');
    name.className = 'airfoil-file-name';
    const displayName = airfoilDisplayNames.get(path);
    name.textContent = displayName ? `${path} — ${displayName}` : path;

    const statusEl = document.createElement('div');
    statusEl.className = 'airfoil-file-status';
    statusEl.textContent = status.text;

    const uploadBtn = document.createElement('button');
    uploadBtn.type = 'button';
    uploadBtn.className = 'btn ghost';
    uploadBtn.textContent = 'Upload';

    const uploadInput = document.createElement('input');
    uploadInput.type = 'file';
    uploadInput.accept = '.dat,.txt,text/plain';
    uploadInput.hidden = true;

    uploadBtn.addEventListener('click', () => uploadInput.click());
    uploadInput.addEventListener('change', () => {
      const file = uploadInput.files?.[0];
      if (file) handleAirfoilUpload(path, file);
      uploadInput.value = '';
    });

    row.appendChild(name);
    row.appendChild(statusEl);
    row.appendChild(uploadBtn);
    row.appendChild(uploadInput);
    els.airfoilFilesList.appendChild(row);
  });
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
  const cleanPath = normalizeAirfoilPath(path);
  if (!cleanPath || airfoilCache.has(cleanPath) || airfoilPending.has(cleanPath) || airfoilFailed.has(cleanPath)) return;
  const candidates = [
    new URL(cleanPath, window.location.href).toString(),
    new URL(`../third_party/avl/runs/${cleanPath}`, window.location.href).toString(),
    new URL(`../../third_party/avl/runs/${cleanPath}`, window.location.href).toString(),
  ];
  const promise = (async () => {
    for (const url of candidates) {
      try {
        const res = await fetch(url);
        if (!res.ok) continue;
        const text = await res.text();
        const { coords, displayName } = parseAirfoilFileDetails(text);
        if (coords.length) {
          providedAirfoilFiles.add(cleanPath);
          airfoilCache.set(cleanPath, coords);
          if (displayName) airfoilDisplayNames.set(cleanPath, displayName);
          airfoilUploadInvalid.delete(cleanPath);
          airfoilFailed.delete(cleanPath);
          return;
        }
      } catch {
        // try next
      }
    }
    airfoilFailed.add(cleanPath);
  })()
    .finally(() => {
      airfoilPending.delete(cleanPath);
      loadGeometryFromText(uiState.text, false);
      renderRequiredAirfoilFiles();
    });
  airfoilPending.set(cleanPath, promise);
  renderRequiredAirfoilFiles();
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
      if (!coords && sec.airfoilFile) {
        const resolved = resolveProvidedAirfoilKey(sec.airfoilFile);
        if (resolved) coords = airfoilCache.get(resolved) || null;
      }
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
  const controlDeflections = new Map();
  const delcon = uiState.lastExecResult?.DELCON;
  const controlMap = uiState.modelCache?.controlMap;
  if (delcon && controlMap?.size) {
    for (const [name, idx] of controlMap.entries()) {
      const val = Number(delcon[idx]);
      controlDeflections.set(name, Number.isFinite(val) ? val : 0);
    }
  }

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
    const sectionAinc = Number(section?.ainc ?? 0);
    const surfaceAinc = Number(surface?.angleDeg ?? 0);
    const totalAinc = sectionAinc + surfaceAinc;
    if (!totalAinc) return point;
    const le = new THREE.Vector3(section.xle, section.yle, section.zle);
    const v = new THREE.Vector3(point[0], point[1], point[2]).sub(le);
    v.applyAxisAngle(new THREE.Vector3(0, 1, 0), THREE.MathUtils.degToRad(totalAinc));
    v.add(le);
    return [v.x, v.y, v.z];
  };

  const applyControlDeflections = (section, point) => {
    if (!section?.controls?.length) return point;
    let v = new THREE.Vector3(point[0], point[1], point[2]);
    const le = new THREE.Vector3(section.xle, section.yle, section.zle);
    for (const ctrl of section.controls) {
      const baseDef = controlDeflections.get(ctrl.name) ?? 0;
      if (!baseDef) continue;
      const gain = Number(ctrl.gain ?? 1);
      const sgnDup = Number.isFinite(Number(ctrl.sgnDup)) ? Number(ctrl.sgnDup) : 1;
      const deflectionDeg = baseDef * gain * sgnDup;
      if (!deflectionDeg) continue;
      const xhinge = Number(ctrl.xhinge ?? 0.75);
      const hingeLocalX = section.chord * xhinge;
      const local = v.clone().sub(le);
      if (local.x < hingeLocalX) continue;
      const hinge = new THREE.Vector3(section.xle + hingeLocalX, section.yle, section.zle);
      let axis = new THREE.Vector3(ctrl.vhinge?.[0] ?? 0, ctrl.vhinge?.[1] ?? 1, ctrl.vhinge?.[2] ?? 0);
      if (axis.lengthSq() < 1e-12) axis = new THREE.Vector3(0, 1, 0);
      axis.normalize();
      v.sub(hinge);
      v.applyAxisAngle(axis, THREE.MathUtils.degToRad(deflectionDeg));
      v.add(hinge);
    }
    return [v.x, v.y, v.z];
  };

  const transformSectionPoint = (section, rawPoint) => {
    const flap = applyControlDeflections(section, rawPoint);
    const rotated = rotateSectionPoint(section, flap);
    return applyTransforms(rotated);
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
      points.push(transformSectionPoint(section, raw));
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
      return transformSectionPoint(section, raw);
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
    const le1 = transformSectionPoint(a, [a.xle, a.yle, a.zle]);
    const le2 = transformSectionPoint(b, [b.xle, b.yle, b.zle]);
    const te1 = transformSectionPoint(a, [a.xle + a.chord, a.yle, a.zle]);
    const te2 = transformSectionPoint(b, [b.xle + b.chord, b.yle, b.zle]);
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
      const h1 = transformSectionPoint(a, [hx1, a.yle, a.zle]);
      const h2 = transformSectionPoint(b, [hx2, b.yle, b.zle]);
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
      const resolved = resolveProvidedAirfoilKey(section.airfoilFile);
      if (resolved) coords = airfoilCache.get(resolved) || null;
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
  ctx.font = '28px Consolas, "Courier New", monospace';
  ctx.textAlign = 'center';
  ctx.textBaseline = 'middle';
  ctx.fillText(formatSurfaceDisplayName(surface.name || 'Surface', 1), 128, 64);
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

function addReferenceMarker(target, header, center, maxDim) {
  if (!THREE || !target || !header) return;
  const xref = Number(header.xref);
  const yref = Number(header.yref);
  const zref = Number(header.zref);
  if (!Number.isFinite(xref) || !Number.isFinite(yref) || !Number.isFinite(zref)) return;
  const refCenter = center || new THREE.Vector3(0, 0, 0);
  const size = Number.isFinite(maxDim) && maxDim > 0 ? maxDim : 1.0;
  const radius = Math.max(0.03, Math.min(0.22, size * 0.012));

  const geom = new THREE.SphereGeometry(radius, 20, 14);
  const mat = new THREE.MeshStandardMaterial({
    color: 0xffffff,
    emissive: 0x93c5fd,
    emissiveIntensity: 0.4,
    metalness: 0.15,
    roughness: 0.45,
  });
  const marker = new THREE.Mesh(geom, mat);
  marker.name = 'reference-marker';
  marker.renderOrder = 4;
  marker.position.set(xref - refCenter.x, yref - refCenter.y, zref - refCenter.z);
  target.add(marker);
}

function rebuildAircraftVisual(shouldFit = false) {
  if (!scene || !uiState.displayModel) return;
  const newAircraft = buildAircraftFromAVL(uiState.displayModel);
  if (aircraft) scene.remove(aircraft);
  aircraft = newAircraft;
  scene.add(aircraft);
  updateBank(Number(els.bank.value));
  const bounds = computeBounds(aircraft);
  if (bounds) {
    aircraft.position.sub(bounds.center);
    viewerState.bounds = bounds;
    viewerState.fitDistance = bounds.maxDim * 1.6 + 4.0;
    addReferenceMarker(aircraft, uiState.modelHeader, bounds.center, bounds.maxDim);
    if (shouldFit) {
      rebuildGrid(bounds.maxDim);
      applyGridMode();
      if (axesHelper) axesHelper.position.set(0, 0, 0);
    }
  } else {
    addReferenceMarker(aircraft, uiState.modelHeader, null, null);
  }
  if (shouldFit) fitCameraToObject(aircraft);
  updateLoadingVisualization();
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
  scene.background = new THREE.Color(getCssVarColor('--panel', '#0f1115'));

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
  renderFileHeaderSummary(uiState.modelHeader, solverModel);
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
  setRequiredAirfoilFiles(model.airfoilFiles);
  renderRequiredAirfoilFiles();
  // AFILE dependencies are user-supplied via the File panel.
  uiState.displayModel = model;
  rebuildAircraftVisual(Boolean(shouldFit));
  const bounds = computeBounds(aircraft);
  if (bounds) {
    logDebug(`Bounds: min=(${fmt(bounds.box.min.x, 3)},${fmt(bounds.box.min.y, 3)},${fmt(bounds.box.min.z, 3)}) max=(${fmt(bounds.box.max.x, 3)},${fmt(bounds.box.max.y, 3)},${fmt(bounds.box.max.z, 3)})`);
  }
  updateFlightConditions();
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
  uiState.execSurfaceNames = buildExecSurfaceNames(model);
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
    applyExecResults({
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
      PARVAL: state.PARVAL,
      WROT: state.WROT,
      DELCON: state.DELCON,
      SPANEF: state.SPANEF,
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
  rebuildAircraftVisual(false);
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

  if (Number.isFinite(result.ALFA) && els.outAlpha) els.outAlpha.textContent = `${fmt(result.ALFA / (Math.PI / 180), 2)} deg`;
  if (Number.isFinite(result.BETA) && els.outBeta) els.outBeta.textContent = `${fmt(result.BETA / (Math.PI / 180), 2)} deg`;
  if (Number.isFinite(result.CLTOT) && els.outCL) els.outCL.textContent = fmt(result.CLTOT, 5);
  if (Number.isFinite(result.CDTOT) && els.outCD) els.outCD.textContent = fmt(result.CDTOT, 5);
  if (Number.isFinite(vee) && els.outV) els.outV.textContent = `${fmt(vee, 2)} m/s`;
  if (Number.isFinite(phi) && els.outBank) els.outBank.textContent = `${fmt(phi, 2)} deg`;
  if (Number.isFinite(rad) && els.outRad) els.outRad.textContent = rad > 0 ? `${fmt(rad, 2)} m` : 'level';
  if (Number.isFinite(fac) && els.outFac) els.outFac.textContent = fmt(fac, 3);
  if (Number.isFinite(the) && els.outThe) els.outThe.textContent = `${fmt(the, 2)} deg`;
  if (els.outMach) {
    const mach = Number(uiState.modelHeader?.mach);
    els.outMach.textContent = Number.isFinite(mach) ? fmt(mach, 3) : '-';
  }
  if (result.WROT) {
    if (els.outPb2v) els.outPb2v.textContent = fmt(result.WROT[0], 5);
    if (els.outQc2v) els.outQc2v.textContent = fmt(result.WROT[1], 5);
    if (els.outRb2v) els.outRb2v.textContent = fmt(result.WROT[2], 5);
  }
  if (result.CFTOT) {
    if (els.outCXtot) els.outCXtot.textContent = fmt(result.CFTOT[0], 5);
    if (els.outCYtot) els.outCYtot.textContent = fmt(result.CFTOT[1], 5);
    if (els.outCZtot) els.outCZtot.textContent = fmt(result.CFTOT[2], 5);
  }
  if (result.CMTOT) {
    if (els.outCltot) els.outCltot.textContent = fmt(result.CMTOT[0], 5);
    if (els.outCmtot) els.outCmtot.textContent = fmt(result.CMTOT[1], 5);
    if (els.outCntot) els.outCntot.textContent = fmt(result.CMTOT[2], 5);
  }
  if (Number.isFinite(result.CDVTOT)) {
    if (els.outCDvis) els.outCDvis.textContent = fmt(result.CDVTOT, 5);
    if (Number.isFinite(result.CDTOT) && els.outCDind) els.outCDind.textContent = fmt(result.CDTOT - result.CDVTOT, 5);
  }
  if (els.outEff) {
    els.outEff.textContent = '-';
    if (Number.isFinite(result.SPANEF)) {
      els.outEff.textContent = fmt(result.SPANEF, 4);
    } else if (Number.isFinite(result.CLTOT) && Number.isFinite(result.CDTOT) && Number.isFinite(result.CDVTOT)) {
      const sref = Number.isFinite(result.SREF) ? result.SREF : Number(uiState.modelHeader?.sref);
      const bref = Number.isFinite(result.BREF) ? result.BREF : Number(uiState.modelHeader?.bref);
      const cdi = result.CDTOT - result.CDVTOT;
      const ar = (Number.isFinite(sref) && Number.isFinite(bref) && sref > 0) ? ((bref * bref) / sref) : null;
      if (ar && cdi > 1e-8) {
        els.outEff.textContent = fmt((result.CLTOT * result.CLTOT) / (Math.PI * ar * cdi), 4);
      }
    }
  }
  if (result.WROT) {
    if (els.outRates) els.outRates.textContent = `${fmt(result.WROT[0], 3)}, ${fmt(result.WROT[1], 3)}, ${fmt(result.WROT[2], 3)}`;
  }
  if (result.CMTOT) {
    if (els.outCM) els.outCM.textContent = `${fmt(result.CMTOT[0], 4)}, ${fmt(result.CMTOT[1], 4)}, ${fmt(result.CMTOT[2], 4)}`;
  }
  if (result.DELCON) {
    const deflections = [];
    const model = uiState.modelCache;
    if (model?.controlMap && model.controlMap.size) {
      for (const [name, idx] of model.controlMap.entries()) {
        const val = result.DELCON[idx] ?? 0.0;
        deflections.push({ name, value: fmt(val, 2) });
      }
    }
    renderControlRows(deflections);
    if (deflections.length && els.outDef) {
      els.outDef.textContent = deflections.map((d) => `${d.name} = ${d.value}`).join('\n');
    }
  }

  renderStabilityGrid(result);
  schedule(() => {
    try {
      renderBodyDerivGrid(result);
    } catch (err) {
      logDebug(`EXEC derive render failed: ${err?.message ?? err}`);
    }
  });

  schedule(() => {
    try {
      renderSurfaceForcesGrid(result);

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
        const rows = [['', 'Chinge']];
        if (result.CHINGE) {
          const model = uiState.modelCache;
          const names = model?.controlMap ? Array.from(model.controlMap.keys()) : [];
          for (let i = 1; i < result.CHINGE.length; i += 1) {
            const name = names[i - 1] ?? `Ctrl ${i}`;
            rows.push([name, result.CHINGE[i] ?? 0]);
          }
        }
        if (rows.length === 1) {
          els.outHinge.textContent = '-';
        } else {
          const html = rows.map((row, rIdx) => row.map((cell, cIdx) => {
            if (rIdx === 0) {
              const cls = cIdx > 0 ? 'stability-cell stability-head stability-colhead' : 'stability-cell stability-head';
              return `<div class="${cls}">${escapeHtml(String(cell))}</div>`;
            }
            if (cIdx === 0) {
              return `<div class="stability-cell stability-head">${escapeHtml(String(cell))}</div>`;
            }
            return `<div class="stability-cell stability-val"><strong class="stability-num">${fmtSignedFixed(Number(cell), 6)}</strong></div>`;
          }).join('')).join('');
          els.outHinge.innerHTML = html;
        }
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
  initPanelCollapse();
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
