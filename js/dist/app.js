import { TRMSET_CORE } from './atrim.js';
import { EXEC } from './aoper.js';
import { MAKESURF, ENCALC, SDUPL } from './amake.js';
import { GETCAM } from './airutil.js';
import { AKIMA, NRMLIZ } from './sgutil.js';
import {
  addInducedVelocityFromVorvelc as addInducedVelocityFromVorvelcKernel,
  addInducedVelocityFromHorseshoe as addInducedVelocityFromHorseshoeKernel,
  buildSolverHorseshoesFromExec,
} from './flow_induced.js';

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
  fileIysym: document.getElementById('fileIysym'),
  fileIzsym: document.getElementById('fileIzsym'),
  fileZsym: document.getElementById('fileZsym'),
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
  navEditor: document.getElementById('navEditor'),
  navSettings: document.getElementById('navSettings'),
  navPlots: document.getElementById('navPlots'),
  navOutputs: document.getElementById('navOutputs'),
  editorCol: document.getElementById('editorCol'),
  editorPanelCol: document.getElementById('editorPanelCol'),
  settingsCol: document.getElementById('settingsCol'),
  plotsCol: document.getElementById('plotsCol'),
  outputsCol: document.getElementById('outputsCol'),
  editorDesktopAnchor: document.getElementById('editorDesktopAnchor'),
  editorPanel: document.getElementById('editorPanel'),
  viewer: document.getElementById('viewer'),
  viewerAircraftName: document.getElementById('viewerAircraftName'),
  trefftz: document.getElementById('trefftz'),
  eigenPlot: document.getElementById('eigenPlot'),
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
  outTotalForces: document.getElementById('trimOutput'),
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
  viewerLoad: document.getElementById('viewerLoad'),
  viewerSurface: document.getElementById('viewerSurface'),
  viewerPressure: document.getElementById('viewerPressure'),
  viewerPanelSpacing: document.getElementById('viewerPanelSpacing'),
  viewerVortices: document.getElementById('viewerVortices'),
  viewerFlow: document.getElementById('viewerFlow'),
  constraintRows: [],
  constraintTable: document.getElementById('constraintTable'),
  runCasesInput: document.getElementById('runCasesInput'),
  runCasesSaveBtn: document.getElementById('runCasesSaveBtn'),
  runCasesMeta: document.getElementById('runCasesMeta'),
  runCaseList: document.getElementById('runCaseList'),
  runCaseAddBtn: document.getElementById('runCaseAddBtn'),
  massPropsInput: document.getElementById('massPropsInput'),
  massPropsSaveBtn: document.getElementById('massPropsSaveBtn'),
  massPropsMeta: document.getElementById('massPropsMeta'),
  massTotal: document.getElementById('massTotal'),
  massXcg: document.getElementById('massXcg'),
  massYcg: document.getElementById('massYcg'),
  massZcg: document.getElementById('massZcg'),
  massIxx: document.getElementById('massIxx'),
  massIyy: document.getElementById('massIyy'),
  massIzz: document.getElementById('massIzz'),
  massIxy: document.getElementById('massIxy'),
  massIxz: document.getElementById('massIxz'),
  massIyz: document.getElementById('massIyz'),
  templateParamsPanel: document.getElementById('templateParamsPanel'),
  templateParamsList: document.getElementById('templateParamsList'),
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
  surfaceRenderMode: 'wireframe',
  showPressure: false,
  showPanelSpacing: false,
  showVortices: false,
  showFlowField: false,
  flowFieldMode: 'induced',
  pressureField: null,
  eigenModes: [],
  eigenModesByRunCase: {},
  selectedEigenMode: -1,
  eigenPoints: [],
  eigenZoom: 1,
  eigenCenterRe: 0,
  eigenCenterIm: 0,
  eigenViewport: null,
  runCases: [],
  runCasesFilename: null,
  needsRunCaseConstraintSync: false,
  selectedRunCaseIndex: -1,
  massProps: null,
  massPropsFilename: null,
  templateParams: [],
  resolvedText: '',
  templateParamSignature: '',
};

const viewerState = {
  mode: 'rotate',
  viewModes: ['top', 'forward', 'side'],
  viewIndex: 2,
  gridModes: ['xy', 'yz', 'xz', 'none'],
  gridIndex: 0,
  bounds: null,
  fitDistance: 12,
};

const FLOW_FIELD_MODES = ['induced', 'induced+rotation', 'full'];
const VIEW_MODE_LABELS = {
  top: 'Top (down)',
  forward: 'Forward (aft)',
  side: 'Side (Y axis)',
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
let outputFontFitRaf = 0;
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
const TEMPLATE_LITERAL_RE = /\$\{([^}]*)\}/g;
const TEMPLATE_FLOAT_DECIMALS_MAX = 6;
const TEMPLATE_EXPLICIT_DEFAULT_RANGE_FACTOR = 10;
const TEMPLATE_RESCALE_TARGET_STEPS = 14;
const TEMPLATE_RESCALE_LONG_PRESS_MS = 420;
const TEMPLATE_POSITIVE_FIELD_INDICES = {
  headerMach: new Set([0]),
  headerRef: new Set([0, 1, 2]),
  surfaceSpacing: new Set([0, 2]),
  sectionData: new Set([3, 5]),
};
let fileMeasureCtx = null;
let templateParamApplyTimer = null;

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

function clampNumber(value, min, max) {
  if (!Number.isFinite(value)) return value;
  let lo = Number(min);
  let hi = Number(max);
  if (!Number.isFinite(lo)) lo = value;
  if (!Number.isFinite(hi)) hi = value;
  if (lo > hi) {
    const tmp = lo;
    lo = hi;
    hi = tmp;
  }
  return Math.min(hi, Math.max(lo, value));
}

function parseTemplateNumberToken(token) {
  const raw = String(token ?? '').trim();
  if (!raw) return null;
  const value = Number(raw);
  if (!Number.isFinite(value)) return null;
  const expIdx = Math.max(raw.indexOf('e'), raw.indexOf('E'));
  const mantissa = expIdx >= 0 ? raw.slice(0, expIdx) : raw;
  const dotIdx = mantissa.indexOf('.');
  const decimals = dotIdx >= 0 ? Math.max(0, mantissa.length - dotIdx - 1) : 0;
  const isIntegerToken = /^[+-]?\d+$/.test(mantissa) && expIdx < 0;
  return {
    value,
    kind: isIntegerToken ? 'int' : 'float',
    decimals,
  };
}

function templateFloatStep(min, max, decimals = null) {
  if (Number.isFinite(decimals) && decimals > 0) {
    return Number((10 ** (-Math.min(TEMPLATE_FLOAT_DECIMALS_MAX, Math.round(decimals)))).toFixed(8));
  }
  const span = Math.abs(Number(max) - Number(min));
  if (!Number.isFinite(span) || span <= 0) return 0.01;
  return Number(Math.max(0.001, span / 400).toFixed(8));
}

function formatTemplateFloat(value, decimals = 3) {
  if (!Number.isFinite(value)) return '0.0';
  const d = Math.max(1, Math.min(TEMPLATE_FLOAT_DECIMALS_MAX, Math.round(decimals)));
  const fixed = Number(value).toFixed(d);
  return fixed.replace(/(\.\d*?[1-9])0+$/, '$1').replace(/\.0+$/, '.0');
}

function formatTemplateParamValue(param, value = param?.value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return param?.kind === 'int' ? '0' : '0.0';
  if (param?.kind === 'int') return String(Math.round(n));
  const decimals = Number.isFinite(Number(param?.decimals)) ? Number(param.decimals) : 3;
  return formatTemplateFloat(n, decimals);
}

function skipTemplateSpaces(expr, start = 0) {
  let i = start;
  while (i < expr.length && /\s/.test(expr[i])) i += 1;
  return i;
}

function parseTemplateExpression(exprRaw) {
  const expr = String(exprRaw || '');
  const refs = [];
  const tokens = [];
  let i = 0;
  const len = expr.length;
  const pushRef = (name, defaultMeta = null) => {
    refs.push({
      name,
      defaultMeta,
      kind: defaultMeta?.kind || null,
      decimals: defaultMeta?.decimals || 0,
    });
  };
  while (i < len) {
    i = skipTemplateSpaces(expr, i);
    if (i >= len) break;
    const ch = expr[i];
    if (ch === '(') {
      tokens.push({ type: 'lparen' });
      i += 1;
      continue;
    }
    if (ch === ')') {
      tokens.push({ type: 'rparen' });
      i += 1;
      continue;
    }
    if (ch === '+' || ch === '-' || ch === '*' || ch === '/') {
      tokens.push({ type: 'op', op: ch });
      i += 1;
      continue;
    }
    if (/[A-Za-z_]/.test(ch)) {
      let j = i + 1;
      while (j < len && /[A-Za-z0-9_]/.test(expr[j])) j += 1;
      const name = expr.slice(i, j);
      i = skipTemplateSpaces(expr, j);
      let defaultMeta = null;
      if (expr.startsWith('??', i) || expr[i] === ':') {
        i += expr.startsWith('??', i) ? 2 : 1;
        i = skipTemplateSpaces(expr, i);
        const startNum = i;
        if (expr[i] === '+' || expr[i] === '-') i += 1;
        while (i < len && /[0-9.]/.test(expr[i])) i += 1;
        if (i < len && (expr[i] === 'e' || expr[i] === 'E')) {
          i += 1;
          if (expr[i] === '+' || expr[i] === '-') i += 1;
          while (i < len && /[0-9]/.test(expr[i])) i += 1;
        }
        const rawNum = expr.slice(startNum, i).trim();
        defaultMeta = parseTemplateNumberToken(rawNum);
        if (!defaultMeta) return null;
      }
      pushRef(name, defaultMeta);
      tokens.push({
        type: 'ident',
        name,
      });
      continue;
    }
    if (/[0-9.]/.test(ch)) {
      const startNum = i;
      while (i < len && /[0-9.]/.test(expr[i])) i += 1;
      if (i < len && (expr[i] === 'e' || expr[i] === 'E')) {
        i += 1;
        if (expr[i] === '+' || expr[i] === '-') i += 1;
        while (i < len && /[0-9]/.test(expr[i])) i += 1;
      }
      const rawNum = expr.slice(startNum, i);
      const meta = parseTemplateNumberToken(rawNum);
      if (!meta) return null;
      tokens.push({
        type: 'number',
        value: meta.value,
        kind: meta.kind,
        decimals: meta.decimals,
      });
      continue;
    }
    return null;
  }
  if (!tokens.length) return null;

  const precedence = {
    '+': 1,
    '-': 1,
    '*': 2,
    '/': 2,
    'u+': 3,
    'u-': 3,
  };
  const rightAssoc = {
    'u+': true,
    'u-': true,
  };
  const output = [];
  const ops = [];
  let prev = 'start';
  for (const token of tokens) {
    if (token.type === 'number' || token.type === 'ident') {
      output.push(token);
      prev = 'value';
      continue;
    }
    if (token.type === 'lparen') {
      ops.push({ type: 'lparen' });
      prev = 'lparen';
      continue;
    }
    if (token.type === 'rparen') {
      let found = false;
      while (ops.length) {
        const top = ops.pop();
        if (top.type === 'lparen') {
          found = true;
          break;
        }
        output.push(top);
      }
      if (!found) return null;
      prev = 'value';
      continue;
    }
    if (token.type === 'op') {
      let op = token.op;
      if ((op === '+' || op === '-') && (prev === 'start' || prev === 'op' || prev === 'lparen')) {
        op = op === '+' ? 'u+' : 'u-';
      }
      const currPrec = precedence[op] ?? -1;
      if (currPrec < 0) return null;
      while (ops.length) {
        const top = ops[ops.length - 1];
        if (top.type !== 'op') break;
        const topPrec = precedence[top.op] ?? -1;
        if (topPrec < 0) break;
        const right = Boolean(rightAssoc[op]);
        if ((!right && topPrec >= currPrec) || (right && topPrec > currPrec)) {
          output.push(ops.pop());
        } else {
          break;
        }
      }
      ops.push({ type: 'op', op });
      prev = 'op';
      continue;
    }
    return null;
  }
  while (ops.length) {
    const top = ops.pop();
    if (top.type === 'lparen') return null;
    output.push(top);
  }
  if (!output.length || prev === 'op' || prev === 'lparen') return null;
  const hasDivision = output.some((token) => token.type === 'op' && token.op === '/');
  const hasFloatLiteral = output.some((token) => token.type === 'number' && token.kind === 'float');
  const literalDecimals = output.reduce((max, token) => {
    if (token.type === 'number' && token.kind === 'float') return Math.max(max, token.decimals || 0);
    return max;
  }, 0);
  return {
    rpn: output,
    refs,
    hasDivision,
    hasFloatLiteral,
    literalDecimals,
  };
}

function mergeTemplateGroup(groups, name, defaultMeta = null) {
  let group = groups.get(name);
  if (!group) {
    group = {
      name,
      order: groups.size,
      hasExplicitDefault: false,
      explicitDefault: 1,
      kind: null,
      decimals: 0,
    };
    groups.set(name, group);
  }
  if (!defaultMeta) return;
  if (!group.hasExplicitDefault) {
    group.hasExplicitDefault = true;
    group.explicitDefault = defaultMeta.value;
  }
  if (defaultMeta.kind === 'float') {
    group.kind = 'float';
    group.decimals = Math.max(group.decimals, defaultMeta.decimals || 0);
  } else if (!group.kind) {
    group.kind = 'int';
  }
}

function tokenizeTemplateAwareLine(line) {
  const src = String(line || '');
  const tokens = [];
  const len = src.length;
  let i = 0;
  while (i < len) {
    while (i < len && /\s/.test(src[i])) i += 1;
    if (i >= len) break;
    const start = i;
    while (i < len) {
      const ch = src[i];
      if (/\s/.test(ch)) break;
      if (ch === '$' && src[i + 1] === '{') {
        i += 2;
        while (i < len && src[i] !== '}') i += 1;
        if (i < len && src[i] === '}') i += 1;
        continue;
      }
      i += 1;
    }
    tokens.push(src.slice(start, i));
  }
  return tokens;
}

function collectTemplateRefsFromToken(tokenText) {
  const refs = [];
  const text = String(tokenText || '');
  if (!text.includes('${')) return refs;
  const re = new RegExp(TEMPLATE_LITERAL_RE.source, 'g');
  let match = re.exec(text);
  while (match) {
    const body = String(match[1] || '').trim();
    if (body) {
      const parsed = parseTemplateExpression(body);
      if (parsed?.refs?.length) {
        parsed.refs.forEach((ref) => {
          if (ref?.name) refs.push(ref.name);
        });
      }
    }
    match = re.exec(text);
  }
  return refs;
}

function markTemplateParamPolicies(policyMap, tokens, positiveFieldIndices = null) {
  if (!Array.isArray(tokens) || !tokens.length) return;
  const positive = positiveFieldIndices instanceof Set ? positiveFieldIndices : new Set();
  tokens.forEach((token, tokenIdx) => {
    const refs = collectTemplateRefsFromToken(token);
    if (!refs.length) return;
    const inPositiveField = positive.has(tokenIdx);
    refs.forEach((name) => {
      const key = String(name || '').trim();
      if (!key) return;
      let row = policyMap.get(key);
      if (!row) {
        row = { positiveHits: 0, otherHits: 0 };
        policyMap.set(key, row);
      }
      if (inPositiveField) row.positiveHits += 1;
      else row.otherHits += 1;
    });
  });
}

function collectTemplateParamLocationPolicies(rawText) {
  const policies = new Map();
  const lines = String(rawText || '').split(/\r?\n/);
  const isComment = (line) => {
    const t = String(line || '').trim();
    return !t || t.startsWith('#') || t.startsWith('!') || t.startsWith('%');
  };
  const contextByKeyword = new Map([
    ['COMP', new Set()],
    ['YDUP', new Set()],
    ['SCAL', new Set()],
    ['TRAN', new Set()],
    ['ANGL', new Set()],
    ['SECT', TEMPLATE_POSITIVE_FIELD_INDICES.sectionData],
    ['CONT', new Set()],
  ]);

  let headerIndex = 0;
  let pendingContext = null;
  let expectSurfaceName = false;
  let expectSurfaceSpacing = false;
  for (let lineIdx = 0; lineIdx < lines.length; lineIdx += 1) {
    const raw = lines[lineIdx];
    if (isComment(raw)) continue;
    const trimmed = String(raw || '').trim();
    const tokens = tokenizeTemplateAwareLine(trimmed);
    if (!tokens.length) continue;
    const firstToken = String(tokens[0] || '').toUpperCase();
    const key4 = firstToken.slice(0, 4);

    if (headerIndex < 5) {
      if (headerIndex === 1) {
        markTemplateParamPolicies(policies, tokens, TEMPLATE_POSITIVE_FIELD_INDICES.headerMach);
      } else if (headerIndex === 3) {
        markTemplateParamPolicies(policies, tokens, TEMPLATE_POSITIVE_FIELD_INDICES.headerRef);
      } else {
        markTemplateParamPolicies(policies, tokens, null);
      }
      headerIndex += 1;
      continue;
    }

    if (expectSurfaceName) {
      markTemplateParamPolicies(policies, tokens, null);
      expectSurfaceName = false;
      expectSurfaceSpacing = true;
      continue;
    }

    if (expectSurfaceSpacing) {
      markTemplateParamPolicies(policies, tokens, TEMPLATE_POSITIVE_FIELD_INDICES.surfaceSpacing);
      expectSurfaceSpacing = false;
      continue;
    }

    if (pendingContext) {
      markTemplateParamPolicies(policies, tokens, pendingContext);
      pendingContext = null;
      continue;
    }

    if (key4 === 'SURF') {
      const valueTokens = tokens.slice(1);
      if (valueTokens.length) markTemplateParamPolicies(policies, valueTokens, null);
      expectSurfaceName = true;
      expectSurfaceSpacing = false;
      pendingContext = null;
      continue;
    }

    if (contextByKeyword.has(key4)) {
      const positiveSet = contextByKeyword.get(key4);
      const valueTokens = tokens.slice(1);
      if (valueTokens.length) markTemplateParamPolicies(policies, valueTokens, positiveSet);
      else pendingContext = positiveSet;
      continue;
    }

    markTemplateParamPolicies(policies, tokens, null);
  }

  const resolved = new Map();
  policies.forEach((row, name) => {
    const positiveHits = Number(row?.positiveHits || 0);
    const otherHits = Number(row?.otherHits || 0);
    resolved.set(name, {
      positiveOnly: positiveHits > 0 && otherHits === 0,
      positiveHits,
      otherHits,
    });
  });
  return resolved;
}

function isStrictlyPositiveTemplateParam(param) {
  return Boolean(param?.positiveOnly);
}

function getTemplateParamMinStep(param) {
  if (param?.kind === 'int') return 1;
  const decimals = Number.isFinite(Number(param?.decimals))
    ? Math.max(1, Math.min(TEMPLATE_FLOAT_DECIMALS_MAX, Math.round(Number(param.decimals))))
    : 3;
  const decimalStep = 10 ** (-decimals);
  const currentStep = Number(param?.step);
  if (Number.isFinite(currentStep) && currentStep > 0) return Math.max(decimalStep, currentStep);
  return decimalStep;
}

function getTemplateParamPositiveFloor(param) {
  if (param?.kind === 'int') return 1;
  const decimals = Number.isFinite(Number(param?.decimals))
    ? Math.max(1, Math.min(TEMPLATE_FLOAT_DECIMALS_MAX, Math.round(Number(param.decimals))))
    : 3;
  const decimalStep = 10 ** (-decimals);
  const step = Number(param?.step);
  if (Number.isFinite(step) && step > 0) return Math.max(decimalStep, step);
  return decimalStep;
}

function buildTemplateParamList(rawText) {
  const text = String(rawText || '');
  const prevMap = new Map();
  for (const param of (uiState.templateParams || [])) {
    if (param?.name) prevMap.set(param.name, param);
  }
  const locationPolicies = collectTemplateParamLocationPolicies(text);
  const groups = new Map();
  const re = new RegExp(TEMPLATE_LITERAL_RE.source, 'g');
  let match = re.exec(text);
  while (match) {
    const body = String(match[1] || '').trim();
    if (!body) {
      match = re.exec(text);
      continue;
    }
    const parsed = parseTemplateExpression(body);
    if (!parsed) {
      match = re.exec(text);
      continue;
    }
    parsed.refs.forEach((ref) => mergeTemplateGroup(groups, ref.name, ref.defaultMeta || null));
    match = re.exec(text);
  }

  const params = [];
  const ordered = Array.from(groups.values()).sort((a, b) => a.order - b.order);
  for (const group of ordered) {
    const locationPolicy = locationPolicies.get(group.name);
    const positiveOnly = Boolean(locationPolicy?.positiveOnly);
    const hasExplicitDefault = Boolean(group.hasExplicitDefault);
    const defaultValue = hasExplicitDefault ? Number(group.explicitDefault) : 1;
    const kind = group.kind || (hasExplicitDefault
      ? (Number.isInteger(defaultValue) ? 'int' : 'float')
      : 'int');
    const decimals = kind === 'float'
      ? Math.max(1, Math.min(TEMPLATE_FLOAT_DECIMALS_MAX, group.decimals || 3))
      : 0;
    let min = -10;
    let max = 10;
    if (hasExplicitDefault) {
      const low = -TEMPLATE_EXPLICIT_DEFAULT_RANGE_FACTOR * defaultValue;
      const high = TEMPLATE_EXPLICIT_DEFAULT_RANGE_FACTOR * defaultValue;
      min = Math.min(low, high);
      max = Math.max(low, high);
    }
    if (min > max) {
      const tmp = min;
      min = max;
      max = tmp;
    }
    let step = kind === 'int' ? 1 : templateFloatStep(min, max, hasExplicitDefault ? decimals : null);
    const prev = prevMap.get(group.name);
    let value = defaultValue;
    let touched = false;
    let rangeCustomized = false;
    if (prev && prev.rangeCustomized && prev.kind === kind && Boolean(prev.positiveOnly) === positiveOnly) {
      const prevDefault = Number(prev.defaultValue);
      if (Number.isFinite(prevDefault) && Math.abs(prevDefault - defaultValue) < 1e-9) {
        const prevMin = Number(prev.min);
        const prevMax = Number(prev.max);
        const prevStep = Number(prev.step);
        if (Number.isFinite(prevMin) && Number.isFinite(prevMax) && Number.isFinite(prevStep) && prevStep > 0 && prevMax > prevMin) {
          min = prevMin;
          max = prevMax;
          step = prevStep;
          rangeCustomized = true;
        }
      }
    }
    if (prev && Number.isFinite(Number(prev.value))) {
      value = Number(prev.value);
      touched = Boolean(prev.touched);
      if (!touched && Number.isFinite(Number(prev.defaultValue)) && Math.abs(Number(prev.defaultValue) - defaultValue) > 1e-9) {
        value = defaultValue;
      }
    }
    if (positiveOnly) {
      const positiveFloor = getTemplateParamPositiveFloor({ kind, decimals, step });
      min = Math.max(positiveFloor, min);
      if (!Number.isFinite(max) || max <= min) {
        max = min + (kind === 'int' ? Math.max(1, Math.round(step || 1)) : Math.max(step, positiveFloor));
      }
    }
    if (kind === 'int') value = Math.round(value);
    value = clampNumber(value, min, max);
    params.push({
      name: group.name,
      kind,
      defaultValue,
      min,
      max,
      step,
      decimals,
      value,
      touched,
      hasExplicitDefault,
      rangeCustomized,
      positiveOnly,
    });
  }
  return params;
}

function buildTemplateParamSignature(params = []) {
  return params.map((param) => [
    param.name,
    param.kind,
    formatTemplateParamValue(param, param.defaultValue),
    formatTemplateParamValue(param, param.min),
    formatTemplateParamValue(param, param.max),
    Number(param.step).toFixed(8),
    String(param.decimals || 0),
    param.rangeCustomized ? 'custom' : 'base',
    param.positiveOnly ? 'strictpos' : 'signed',
  ].join('|')).join('||');
}

function getTemplateParamRow(name) {
  if (!els.templateParamsList || !name) return null;
  return els.templateParamsList.querySelector(`.template-param-row[data-template-param="${name}"]`);
}

function updateTemplateParamControlValues() {
  if (!els.templateParamsList) return;
  const params = Array.isArray(uiState.templateParams) ? uiState.templateParams : [];
  params.forEach((param) => {
    const row = getTemplateParamRow(param.name);
    if (!row) return;
    const slider = row.querySelector('input[type="range"]');
    const valueEl = row.querySelector('.template-param-value');
    if (slider) {
      slider.min = String(param.min);
      slider.max = String(param.max);
      slider.step = String(param.step);
      slider.value = String(param.value);
    }
    if (valueEl) valueEl.textContent = formatTemplateParamValue(param, param.value);
  });
}

function rescaleTemplateParamAroundCurrent(name) {
  const key = String(name || '').trim();
  if (!key) return false;
  const params = Array.isArray(uiState.templateParams) ? uiState.templateParams : [];
  const param = params.find((item) => item?.name === key);
  if (!param) return false;
  const beforeValue = Number(param.value);
  let center = Number.isFinite(beforeValue) ? beforeValue : Number(param.defaultValue);
  if (!Number.isFinite(center)) center = 0;
  if (param.kind === 'int') center = Math.round(center);
  let step = getTemplateParamMinStep(param);
  if (!Number.isFinite(step) || step <= 0) step = param.kind === 'int' ? 1 : 0.01;
  if (param.kind === 'int') step = 1;
  const positiveOnly = isStrictlyPositiveTemplateParam(param);
  const positiveFloor = positiveOnly ? getTemplateParamPositiveFloor({ kind: param.kind, decimals: param.decimals, step }) : 0;
  if (positiveOnly && center < positiveFloor) center = positiveFloor;
  const halfSpan = step * (TEMPLATE_RESCALE_TARGET_STEPS / 2);
  let min = center - halfSpan;
  let max = center + halfSpan;
  if (positiveOnly && min < positiveFloor) {
    min = positiveFloor;
    max = min + (step * TEMPLATE_RESCALE_TARGET_STEPS);
  }
  if (!Number.isFinite(min) || !Number.isFinite(max) || max <= min) {
    min = positiveOnly ? positiveFloor : 0;
    max = step * TEMPLATE_RESCALE_TARGET_STEPS;
    if (positiveOnly && max <= min) max = min + step;
  }
  if (param.kind === 'int') {
    min = Math.round(min);
    max = Math.round(max);
    if (max <= min) max = min + TEMPLATE_RESCALE_TARGET_STEPS;
  }
  param.min = min;
  param.max = max;
  param.step = step;
  param.rangeCustomized = true;
  param.value = clampNumber(Number(param.value), min, max);
  if (param.kind === 'int') param.value = Math.round(param.value);
  updateTemplateParamControlValues();
  const changedValue = Math.abs(Number(param.value) - beforeValue) > (param.kind === 'int' ? 0.5 : 1e-9);
  if (changedValue) queueTemplateParamApply();
  return true;
}

function queueTemplateParamApply() {
  if (templateParamApplyTimer) clearTimeout(templateParamApplyTimer);
  templateParamApplyTimer = setTimeout(() => {
    templateParamApplyTimer = null;
    if (!els.fileText) return;
    uiState.text = els.fileText.value;
    loadGeometryFromText(uiState.text, false);
    resetTrimSeed();
    scheduleAutoTrim();
  }, 80);
}

function renderTemplateParamControls() {
  if (!els.templateParamsPanel || !els.templateParamsList) return;
  const params = Array.isArray(uiState.templateParams) ? uiState.templateParams : [];
  if (!params.length) {
    els.templateParamsPanel.classList.add('hidden');
    els.templateParamsList.innerHTML = '';
    return;
  }

  els.templateParamsPanel.classList.remove('hidden');
  const frag = document.createDocumentFragment();
  params.forEach((param) => {
    const paramName = String(param.name || '');
    const row = document.createElement('div');
    row.className = 'template-param-row';
    row.setAttribute('data-template-param', paramName);

    const nameEl = document.createElement('div');
    nameEl.className = 'template-param-name';
    nameEl.textContent = paramName;
    nameEl.title = 'Double-click or long-press to rescale';
    row.appendChild(nameEl);

    const slider = document.createElement('input');
    slider.className = 'template-param-slider';
    slider.type = 'range';
    slider.min = String(param.min);
    slider.max = String(param.max);
    slider.step = String(param.step);
    slider.value = String(param.value);
    slider.setAttribute('data-template-param-name', paramName);
    row.appendChild(slider);

    const valueEl = document.createElement('div');
    valueEl.className = 'template-param-value';
    valueEl.textContent = formatTemplateParamValue(param, param.value);
    row.appendChild(valueEl);

    const getCurrentParam = () => {
      const list = Array.isArray(uiState.templateParams) ? uiState.templateParams : [];
      return list.find((item) => item?.name === paramName) || null;
    };

    const updateFromSlider = () => {
      const current = getCurrentParam();
      if (!current) return;
      const nextRaw = Number(slider.value);
      if (!Number.isFinite(nextRaw)) return;
      const next = current.kind === 'int'
        ? Math.round(clampNumber(nextRaw, current.min, current.max))
        : clampNumber(nextRaw, current.min, current.max);
      if (Math.abs(next - Number(current.value)) < (current.kind === 'int' ? 0.5 : 1e-9)) {
        valueEl.textContent = formatTemplateParamValue(current, current.value);
        return;
      }
      current.value = next;
      current.touched = true;
      slider.value = String(next);
      valueEl.textContent = formatTemplateParamValue(current, current.value);
      queueTemplateParamApply();
    };

    slider.addEventListener('input', updateFromSlider);
    slider.addEventListener('change', updateFromSlider);

    let longPressTimer = null;
    const clearLongPress = () => {
      if (longPressTimer) {
        clearTimeout(longPressTimer);
        longPressTimer = null;
      }
    };
    const triggerRescale = () => {
      clearLongPress();
      rescaleTemplateParamAroundCurrent(paramName);
    };
    nameEl.addEventListener('dblclick', (event) => {
      event.preventDefault();
      triggerRescale();
    });
    nameEl.addEventListener('pointerdown', () => {
      clearLongPress();
      longPressTimer = setTimeout(() => {
        longPressTimer = null;
        rescaleTemplateParamAroundCurrent(paramName);
      }, TEMPLATE_RESCALE_LONG_PRESS_MS);
    });
    nameEl.addEventListener('pointerup', clearLongPress);
    nameEl.addEventListener('pointerleave', clearLongPress);
    nameEl.addEventListener('pointercancel', clearLongPress);
    nameEl.addEventListener('contextmenu', (event) => {
      event.preventDefault();
      clearLongPress();
    });

    frag.appendChild(row);
  });
  els.templateParamsList.replaceChildren(frag);
}

function syncTemplateParamsFromText(rawText) {
  const params = buildTemplateParamList(rawText);
  const signature = buildTemplateParamSignature(params);
  const shouldRebuild = signature !== uiState.templateParamSignature;
  uiState.templateParams = params;
  uiState.templateParamSignature = signature;
  if (shouldRebuild) renderTemplateParamControls();
  else updateTemplateParamControlValues();
}

function evaluateTemplateExpression(parsed, paramMap) {
  if (!parsed?.rpn?.length) return null;
  const stack = [];
  for (const token of parsed.rpn) {
    if (token.type === 'number') {
      stack.push(Number(token.value));
      continue;
    }
    if (token.type === 'ident') {
      const param = paramMap.get(token.name);
      if (param && Number.isFinite(Number(param.value))) {
        stack.push(Number(param.value));
        continue;
      }
      const fallbackRef = parsed.refs.find((ref) => ref.name === token.name && ref.defaultMeta);
      if (fallbackRef?.defaultMeta && Number.isFinite(Number(fallbackRef.defaultMeta.value))) {
        stack.push(Number(fallbackRef.defaultMeta.value));
      } else {
        stack.push(1);
      }
      continue;
    }
    if (token.type === 'op') {
      if (token.op === 'u+' || token.op === 'u-') {
        const a = Number(stack.pop());
        if (!Number.isFinite(a)) return null;
        stack.push(token.op === 'u-' ? -a : a);
        continue;
      }
      const b = Number(stack.pop());
      const a = Number(stack.pop());
      if (!Number.isFinite(a) || !Number.isFinite(b)) return null;
      if (token.op === '+') stack.push(a + b);
      else if (token.op === '-') stack.push(a - b);
      else if (token.op === '*') stack.push(a * b);
      else if (token.op === '/') stack.push(a / b);
      else return null;
      continue;
    }
    return null;
  }
  if (stack.length !== 1) return null;
  const value = Number(stack[0]);
  if (!Number.isFinite(value)) return null;
  let floatLike = Boolean(parsed.hasDivision || parsed.hasFloatLiteral);
  let maxDecimals = Number(parsed.literalDecimals || 0);
  if (!floatLike) {
    for (const ref of parsed.refs) {
      const param = paramMap.get(ref.name);
      if (param?.kind === 'float') {
        floatLike = true;
        maxDecimals = Math.max(maxDecimals, Number(param.decimals || 0));
      } else if (ref.defaultMeta?.kind === 'float') {
        floatLike = true;
        maxDecimals = Math.max(maxDecimals, Number(ref.defaultMeta.decimals || 0));
      }
    }
  } else {
    for (const ref of parsed.refs) {
      const param = paramMap.get(ref.name);
      if (param?.kind === 'float') maxDecimals = Math.max(maxDecimals, Number(param.decimals || 0));
      else if (ref.defaultMeta?.kind === 'float') maxDecimals = Math.max(maxDecimals, Number(ref.defaultMeta.decimals || 0));
    }
  }
  return {
    value,
    floatLike,
    decimals: Math.max(1, Math.min(TEMPLATE_FLOAT_DECIMALS_MAX, maxDecimals || 3)),
  };
}

function resolveTemplateParamsInText(rawText) {
  const text = String(rawText || '');
  const paramMap = new Map((uiState.templateParams || []).map((param) => [param.name, param]));
  const re = new RegExp(TEMPLATE_LITERAL_RE.source, 'g');
  return text.replace(re, (full, bodyRaw) => {
    const body = String(bodyRaw || '').trim();
    if (!body) return full;
    const parsed = parseTemplateExpression(body);
    if (!parsed) return full;
    const result = evaluateTemplateExpression(parsed, paramMap);
    if (!result) return full;
    if (!result.floatLike) return String(Math.round(result.value));
    return formatTemplateFloat(result.value, result.decimals);
  });
}

function hasHorizontalOverflow(container, itemSelector) {
  if (!container) return false;
  const nodes = container.querySelectorAll(itemSelector);
  for (const node of nodes) {
    if (!(node instanceof HTMLElement)) continue;
    if (node.offsetParent === null) continue;
    if (node.scrollWidth - node.clientWidth > 1) return true;
  }
  return false;
}

function fitGridFontVar({
  grid,
  cssVar,
  maxPx,
  minPx,
  stepPx,
  itemSelector,
  extraOverflowCheck = null,
}) {
  if (!grid) return;
  let size = maxPx;
  grid.style.setProperty(cssVar, `${size.toFixed(2)}px`);
  let overflow = hasHorizontalOverflow(grid, itemSelector) || (typeof extraOverflowCheck === 'function' && extraOverflowCheck());
  while (overflow && size > minPx) {
    size = Math.max(minPx, size - stepPx);
    grid.style.setProperty(cssVar, `${size.toFixed(2)}px`);
    overflow = hasHorizontalOverflow(grid, itemSelector) || (typeof extraOverflowCheck === 'function' && extraOverflowCheck());
  }
}

function fitOutputGridFontsNow() {
  fitGridFontVar({
    grid: els.outTotalForces,
    cssVar: '--forces-font-size',
    maxPx: 16,
    minPx: 8,
    stepPx: 0.25,
    itemSelector: '.force-cell:not(.control-row)',
  });

  fitGridFontVar({
    grid: els.outStability,
    cssVar: '--deriv-font-size',
    maxPx: 14,
    minPx: 7.5,
    stepPx: 0.25,
    itemSelector: '.stability-cell',
    extraOverflowCheck: () => {
      const n = els.outStabilityNeutral;
      if (!(n instanceof HTMLElement) || n.offsetParent === null) return false;
      return n.scrollWidth - n.clientWidth > 1;
    },
  });

  fitGridFontVar({
    grid: els.outBodyDeriv,
    cssVar: '--deriv-font-size',
    maxPx: 14,
    minPx: 7,
    stepPx: 0.25,
    itemSelector: '.stability-cell',
  });

  fitGridFontVar({
    grid: els.outForcesSurface,
    cssVar: '--surface-font-size',
    maxPx: 14,
    minPx: 7,
    stepPx: 0.25,
    itemSelector: '.stability-cell',
  });
}

function updateSurfaceNameColumnWidth() {
  if (!els.outForcesSurface) return;
  const names = [...els.outForcesSurface.querySelectorAll('.surface-name-text')]
    .map((el) => String(el.textContent || '').trim())
    .filter(Boolean);
  if (!names.length) {
    els.outForcesSurface.style.removeProperty('--surface-name-col');
    return;
  }
  const maxChars = names.reduce((m, s) => Math.max(m, s.length), 0);
  const widthCh = Math.max(8, Math.min(32, maxChars + 1));
  els.outForcesSurface.style.setProperty('--surface-name-col', `${widthCh}ch`);
}

function scheduleOutputGridFontFit() {
  if (outputFontFitRaf) return;
  outputFontFitRaf = requestAnimationFrame(() => {
    outputFontFitRaf = 0;
    fitOutputGridFontsNow();
  });
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

function replaceAvlHeaderReferenceLines(text, refs) {
  const formatByToken = (value, token, options = {}) => {
    const { minDecimals = 1, integer = false } = options;
    const v = Number(value);
    if (!Number.isFinite(v)) return integer ? '0' : '0.0';
    if (integer) return String(Math.trunc(v));
    const tokenText = String(token || '');
    const frac = tokenText.match(/\.(\d+)/);
    const decimals = Math.max(minDecimals, frac ? frac[1].length : 0);
    return v.toFixed(decimals);
  };

  const rewriteTriple = (line, values, options = [{}, {}, {}]) => {
    const m = String(line).match(/^(\s*)(\S+)(\s+)(\S+)(\s+)(\S+)(\s*)$/);
    if (!m) {
      return `${formatByToken(values[0], '', options[0])}  ${formatByToken(values[1], '', options[1])}  ${formatByToken(values[2], '', options[2])}`;
    }
    const [, lead, t1, s1, t2, s2, t3, trail] = m;
    const n1 = formatByToken(values[0], t1, options[0]);
    const n2 = formatByToken(values[1], t2, options[1]);
    const n3 = formatByToken(values[2], t3, options[2]);
    return `${lead}${n1}${s1}${n2}${s2}${n3}${trail}`;
  };

  const lineBreak = text.includes('\r\n') ? '\r\n' : '\n';
  const lines = String(text || '').split(/\r?\n/);
  const dataLineIdx = [];
  for (let i = 0; i < lines.length; i += 1) {
    if (!isAvlCommentLine(lines[i])) dataLineIdx.push(i);
    if (dataLineIdx.length >= 5) break;
  }
  if (dataLineIdx.length < 5) return text;
  const symIdx = dataLineIdx[2];
  const srefIdx = dataLineIdx[3];
  const xrefIdx = dataLineIdx[4];
  lines[symIdx] = rewriteTriple(
    lines[symIdx],
    [refs.iysym, refs.izsym, refs.zsym],
    [{ integer: true, minDecimals: 0 }, { integer: true, minDecimals: 0 }, { minDecimals: 1 }],
  );
  lines[srefIdx] = rewriteTriple(lines[srefIdx], [refs.sref, refs.cref, refs.bref]);
  lines[xrefIdx] = rewriteTriple(lines[xrefIdx], [refs.xref, refs.yref, refs.zref]);
  return lines.join(lineBreak);
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
  if (els.viewerAircraftName) {
    const title = hasHeader ? String(header.title || '').trim() : '';
    els.viewerAircraftName.textContent = title;
    els.viewerAircraftName.style.display = title ? '' : 'none';
  }
  if (!hasHeader) return;

  const show = (el, value, digits = 4) => {
    if (!el) return;
    el.value = Number.isFinite(Number(value)) ? fmt(Number(value), digits) : '';
  };
  const showSym = (el, value) => {
    if (!el) return;
    const v = Number(value);
    const next = Number.isFinite(v) ? String(Math.trunc(v)) : '0';
    if ([...el.options].some((opt) => opt.value === next)) {
      el.value = next;
    } else {
      el.value = '0';
    }
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
  showSym(els.fileIysym, header.iysym);
  showSym(els.fileIzsym, header.izsym);
  show(els.fileZsym, header.zsym, 2);
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

function applyAircraftRefRename() {
  if (!els.fileText) return;
  if (fileSummarySyncing) return;
  const refs = {
    iysym: Number(els.fileIysym?.value),
    izsym: Number(els.fileIzsym?.value),
    zsym: Number(els.fileZsym?.value),
    sref: Number(els.fileSref?.value),
    cref: Number(els.fileCref?.value),
    bref: Number(els.fileBref?.value),
    xref: Number(els.fileXref?.value),
    yref: Number(els.fileYref?.value),
    zref: Number(els.fileZref?.value),
  };
  const allFinite = Object.values(refs).every((v) => Number.isFinite(v));
  if (!allFinite) {
    renderFileHeaderSummary(uiState.modelHeader, uiState.modelCache);
    return;
  }
  const updated = replaceAvlHeaderReferenceLines(els.fileText.value, refs);
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

function syncEditorPanelPlacement() {
  if (!els.editorPanel || !els.editorDesktopAnchor || !els.editorPanelCol) return;
  const mobile = typeof window.matchMedia === 'function' && window.matchMedia('(max-width: 900px)').matches;
  if (mobile) {
    if (els.editorPanel.parentElement !== els.editorPanelCol) {
      els.editorPanelCol.appendChild(els.editorPanel);
    }
    return;
  }

  const desktopParent = els.editorDesktopAnchor.parentElement;
  if (!desktopParent) return;
  if (els.editorPanel.parentElement !== desktopParent || els.editorDesktopAnchor.nextElementSibling !== els.editorPanel) {
    els.editorDesktopAnchor.after(els.editorPanel);
  }
}

function initTopNav() {
  if (!els.appRoot || !els.navEditor || !els.navSettings || !els.navPlots || !els.navOutputs) return;
  syncEditorPanelPlacement();
  const navItems = [
    { btn: els.navEditor, col: els.editorCol },
    { btn: els.navSettings, col: els.settingsCol },
    { btn: els.navPlots, col: els.plotsCol },
    { btn: els.navOutputs, col: els.outputsCol },
  ];
  const getTargetLeft = (idx) => {
    const col = navItems[idx]?.col;
    if (col && Number.isFinite(col.offsetLeft)) return Math.max(0, col.offsetLeft);
    const pageWidth = els.appRoot.clientWidth || 0;
    return Math.max(0, pageWidth * idx);
  };
  const setActiveIndex = (index) => {
    const clamped = Math.max(0, Math.min(index, navItems.length - 1));
    navItems.forEach(({ btn }, idx) => btn?.classList?.toggle('active', idx === clamped));
  };

  const updateActiveFromScroll = () => {
    const left = els.appRoot.scrollLeft;
    let index = 0;
    let bestDistance = Number.POSITIVE_INFINITY;
    navItems.forEach((_, idx) => {
      const target = getTargetLeft(idx);
      const distance = Math.abs(left - target);
      if (distance < bestDistance) {
        bestDistance = distance;
        index = idx;
      }
    });
    setActiveIndex(index);
  };

  navItems.forEach(({ btn }, idx) => {
    if (!btn) return;
    if (btn.dataset) btn.dataset.index = String(idx);
    btn.addEventListener('click', () => {
      els.appRoot.scrollTo({ left: getTargetLeft(idx), behavior: 'smooth' });
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
    syncEditorPanelPlacement();
    const hasMatchMedia = typeof window.matchMedia === 'function';
    if (hasMatchMedia && window.matchMedia('(max-width: 900px)').matches) {
      const defaultIndex = 2;
      const targetLeft = getTargetLeft(defaultIndex);
      if (targetLeft > 0 || defaultIndex === 0) {
        els.appRoot.scrollLeft = targetLeft;
        setActiveIndex(defaultIndex);
        return;
      }
    }
    updateActiveFromScroll();
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

function fmtSignedAligned(value, digits = 4, maxDigits = 4) {
  if (!Number.isFinite(value)) return '-';
  const d = Math.max(0, Math.min(digits, maxDigits));
  const abs = Math.abs(Number(value)).toFixed(d);
  return `${value < 0 ? '-' : '\u00a0'}${abs}`;
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
    scheduleOutputGridFontFit();
    return;
  }
  const maxName = entries.reduce((m, e) => Math.max(m, String(e?.name ?? '').length), 0);
  const html = entries.map((entry, i) => {
    const rawName = String(entry?.name ?? 'control');
    const name = escapeHtml(rawName);
    const value = escapeHtml(String(entry?.value ?? '-'));
    return `<div class="force-cell control-row" style="grid-row:${4 + i};"><span class="force-control-name" style="--control-name-width:${maxName}ch">${name}</span>&nbsp;=<strong>${value}</strong>&nbsp;deg</div>`;
  }).join('');
  els.outControlRows.innerHTML = html;
  scheduleOutputGridFontFit();
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
    scheduleOutputGridFontFit();
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
  scheduleOutputGridFontFit();
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
    scheduleOutputGridFontFit();
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
  scheduleOutputGridFontFit();
}

function renderSurfaceForcesGrid(result) {
  if (!els.outForcesSurface) return;
  const clSurf = result?.CLSURF;
  const cdSurf = result?.CDSURF;
  const cySurf = result?.CYSURF;
  if (!clSurf || !cdSurf || !cySurf || clSurf.length <= 1) {
    els.outForcesSurface.textContent = '-';
    updateSurfaceNameColumnWidth();
    scheduleOutputGridFontFit();
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
  updateSurfaceNameColumnWidth();
  scheduleOutputGridFontFit();
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

function caseNameOrFallback(entry, index) {
  const name = String(entry?.name || '').trim();
  if (name) return name;
  return `Case ${index + 1}`;
}

function caseColorOrFallback(entry, index) {
  const palette = ['#ff4a3d', '#ff9f1a', '#ffd166', '#6dd36f', '#2f7bff', '#9b5de5', '#f472b6', '#22d3ee'];
  const raw = String(entry?.color || '').trim();
  if (/^#[0-9a-fA-F]{6}$/.test(raw)) return raw.toLowerCase();
  return palette[index % palette.length];
}

function hexToRgba(hex, alpha = 1) {
  const h = String(hex || '').trim();
  if (!/^#[0-9a-fA-F]{6}$/.test(h)) return `rgba(147,197,253,${alpha})`;
  const r = Number.parseInt(h.slice(1, 3), 16);
  const g = Number.parseInt(h.slice(3, 5), 16);
  const b = Number.parseInt(h.slice(5, 7), 16);
  return `rgba(${r},${g},${b},${alpha})`;
}

function activeRunCaseColor() {
  if (!Array.isArray(uiState.runCases) || !uiState.runCases.length) return null;
  const idx = (Number.isInteger(uiState.selectedRunCaseIndex) && uiState.selectedRunCaseIndex >= 0)
    ? Math.min(uiState.selectedRunCaseIndex, uiState.runCases.length - 1)
    : 0;
  return caseColorOrFallback(uiState.runCases[idx], idx);
}

function activeRunCaseIndex() {
  if (!Array.isArray(uiState.runCases) || !uiState.runCases.length) return -1;
  if (Number.isInteger(uiState.selectedRunCaseIndex) && uiState.selectedRunCaseIndex >= 0) {
    return Math.min(uiState.selectedRunCaseIndex, uiState.runCases.length - 1);
  }
  return 0;
}

function activeRunCaseEntry() {
  const idx = activeRunCaseIndex();
  if (idx < 0) return null;
  if (!Array.isArray(uiState.runCases) || idx >= uiState.runCases.length) return null;
  const entry = uiState.runCases[idx];
  return entry && typeof entry === 'object' ? entry : null;
}

function clearEigenModeRunCaseCache() {
  uiState.eigenModesByRunCase = {};
  uiState.eigenModes = [];
  uiState.selectedEigenMode = -1;
  stopModeAnimation();
  drawEigenPlot();
}

function ensureRunFileExtension(name) {
  const trimmed = String(name || '').trim();
  if (!trimmed) return 'cases.run';
  if (/\.[Rr][Uu][Nn]$/.test(trimmed)) return trimmed;
  return `${trimmed}.run`;
}

function ensureMassFileExtension(name) {
  const trimmed = String(name || '').trim();
  if (!trimmed) return 'model.mass';
  if (/\.[Mm][Aa][Ss][Ss]$/.test(trimmed)) return trimmed;
  return `${trimmed}.mass`;
}

function readMassPropsFromUI() {
  const num = (el, fallback = 0) => {
    const v = Number(el?.value);
    return Number.isFinite(v) ? v : fallback;
  };
  return {
    mass: num(els.massTotal, num(els.mass, 0)),
    xcg: num(els.massXcg, Number(uiState.modelHeader?.xref) || 0),
    ycg: num(els.massYcg, Number(uiState.modelHeader?.yref) || 0),
    zcg: num(els.massZcg, Number(uiState.modelHeader?.zref) || 0),
    ixx: num(els.massIxx, 0),
    iyy: num(els.massIyy, 0),
    izz: num(els.massIzz, 0),
    ixy: num(els.massIxy, 0),
    ixz: num(els.massIxz, 0),
    iyz: num(els.massIyz, 0),
    g: Number.isFinite(Number(els.gee?.value)) ? Number(els.gee.value) : 9.81,
    rho: Number.isFinite(Number(els.rho?.value)) ? Number(els.rho.value) : 1.225,
    lunit: 'm',
    munit: 'kg',
    tunit: 's',
  };
}

function makeDefaultMassProps() {
  return {
    mass: Number(els.mass?.value || 0),
    xcg: Number(uiState.modelHeader?.xref || 0),
    ycg: Number(uiState.modelHeader?.yref || 0),
    zcg: Number(uiState.modelHeader?.zref || 0),
    ixx: 0,
    iyy: 0,
    izz: 0,
    ixy: 0,
    ixz: 0,
    iyz: 0,
    g: Number(els.gee?.value || 9.81),
    rho: Number(els.rho?.value || 1.225),
    lunit: 'm',
    munit: 'kg',
    tunit: 's',
  };
}

function renderMassProps(props = null) {
  const mp = props || uiState.massProps || readMassPropsFromUI();
  uiState.massProps = mp;
  const show = (el, v, d = 4) => {
    if (!el) return;
    el.value = Number.isFinite(Number(v)) ? fmt(Number(v), d) : '';
  };
  show(els.massTotal, mp.mass, 4);
  show(els.massXcg, mp.xcg, 4);
  show(els.massYcg, mp.ycg, 4);
  show(els.massZcg, mp.zcg, 4);
  show(els.massIxx, mp.ixx, 4);
  show(els.massIyy, mp.iyy, 4);
  show(els.massIzz, mp.izz, 4);
  show(els.massIxy, mp.ixy, 4);
  show(els.massIxz, mp.ixz, 4);
  show(els.massIyz, mp.iyz, 4);
  if (els.massPropsMeta) {
    const name = ensureMassFileExtension(uiState.massPropsFilename || 'model.mass');
    els.massPropsMeta.textContent = `Mass=${fmt(mp.mass, 4)} • ${name}`;
  }
}

function parseMassFileText(text) {
  const lines = String(text || '').split(/\r?\n/);
  const vars = { g: null, rho: null, lunit: 'm', munit: 'kg', tunit: 's' };
  const scale = new Array(10).fill(1);
  const add = new Array(10).fill(0);
  const rows = [];

  const stripComments = (line) => String(line || '').replace(/!.*/, '').replace(/#.*/, '').trim();
  const parseNums = (line) => line.trim().split(/\s+/).map((t) => Number(t)).filter((n) => Number.isFinite(n));

  lines.forEach((raw) => {
    const line = stripComments(raw);
    if (!line) return;
    const varMatch = line.match(/^(Lunit|Munit|Tunit|g|rho)\s*=\s*([^\s]+)\s*([A-Za-z\/^0-9._-]+)?/i);
    if (varMatch) {
      const key = varMatch[1].toLowerCase();
      const val = Number(varMatch[2]);
      if (key === 'lunit' || key === 'munit' || key === 'tunit') {
        vars[key] = String(varMatch[3] || varMatch[2] || vars[key]).replace(/[^A-Za-z]/g, '') || vars[key];
      } else if (Number.isFinite(val)) {
        vars[key] = val;
      }
      return;
    }
    if (line.startsWith('*')) {
      const nums = parseNums(line.slice(1));
      nums.forEach((n, i) => { if (i < scale.length) scale[i] = n; });
      return;
    }
    if (line.startsWith('+')) {
      const nums = parseNums(line.slice(1));
      nums.forEach((n, i) => { if (i < add.length) add[i] = n; });
      return;
    }
    const nums = parseNums(line);
    if (nums.length < 7) return;
    const rawVals = new Array(10).fill(0);
    nums.slice(0, 10).forEach((n, i) => { rawVals[i] = n; });
    const vals = rawVals.map((v, i) => add[i] + scale[i] * v);
    rows.push({
      mass: vals[0], x: vals[1], y: vals[2], z: vals[3],
      ixx: vals[4], iyy: vals[5], izz: vals[6], ixy: vals[7], ixz: vals[8], iyz: vals[9],
    });
  });

  if (!rows.length) throw new Error('No mass rows found.');
  let massSum = 0;
  let sx = 0;
  let sy = 0;
  let sz = 0;
  rows.forEach((r) => {
    massSum += r.mass;
    sx += r.mass * r.x;
    sy += r.mass * r.y;
    sz += r.mass * r.z;
  });
  if (!Number.isFinite(massSum) || Math.abs(massSum) < 1e-12) throw new Error('Invalid total mass.');
  const xcg = sx / massSum;
  const ycg = sy / massSum;
  const zcg = sz / massSum;

  let ixx0 = 0;
  let iyy0 = 0;
  let izz0 = 0;
  let ixy0 = 0;
  let ixz0 = 0;
  let iyz0 = 0;
  rows.forEach((r) => {
    ixx0 += r.ixx + r.mass * (r.y * r.y + r.z * r.z);
    iyy0 += r.iyy + r.mass * (r.x * r.x + r.z * r.z);
    izz0 += r.izz + r.mass * (r.x * r.x + r.y * r.y);
    ixy0 += r.ixy + r.mass * (r.x * r.y);
    ixz0 += r.ixz + r.mass * (r.x * r.z);
    iyz0 += r.iyz + r.mass * (r.y * r.z);
  });

  return {
    mass: massSum,
    xcg,
    ycg,
    zcg,
    ixx: ixx0 - massSum * (ycg * ycg + zcg * zcg),
    iyy: iyy0 - massSum * (xcg * xcg + zcg * zcg),
    izz: izz0 - massSum * (xcg * xcg + ycg * ycg),
    ixy: ixy0 - massSum * xcg * ycg,
    ixz: ixz0 - massSum * xcg * zcg,
    iyz: iyz0 - massSum * ycg * zcg,
    g: Number.isFinite(vars.g) ? vars.g : Number(els.gee?.value || 9.81),
    rho: Number.isFinite(vars.rho) ? vars.rho : Number(els.rho?.value || 1.225),
    lunit: vars.lunit,
    munit: vars.munit,
    tunit: vars.tunit,
  };
}

function serializeMassFile(props) {
  const p = props || readMassPropsFromUI();
  return [
    '# VibeLattice Mass Properties',
    '#',
    `Lunit = 1.0 ${p.lunit || 'm'}`,
    `Munit = 1.0 ${p.munit || 'kg'}`,
    `Tunit = 1.0 ${p.tunit || 's'}`,
    '',
    `g   = ${fmt(p.g, 6)}`,
    `rho = ${fmt(p.rho, 6)}`,
    '',
    '# mass    xcg    ycg    zcg     Ixx     Iyy     Izz     Ixy     Ixz     Iyz',
    `${fmt(p.mass, 6)} ${fmt(p.xcg, 6)} ${fmt(p.ycg, 6)} ${fmt(p.zcg, 6)} ${fmt(p.ixx, 6)} ${fmt(p.iyy, 6)} ${fmt(p.izz, 6)} ${fmt(p.ixy, 6)} ${fmt(p.ixz, 6)} ${fmt(p.iyz, 6)}`,
    '',
  ].join('\n');
}

function updateRunCasesMeta(message = '') {
  if (!els.runCasesMeta) return;
  const count = uiState.runCases.length;
  if (message) {
    els.runCasesMeta.textContent = message;
    return;
  }
  if (!count) {
    els.runCasesMeta.textContent = 'No run cases loaded.';
    return;
  }
  const label = ensureRunFileExtension(uiState.runCasesFilename || 'unsaved.run');
  els.runCasesMeta.textContent = `${count} run case(s) • ${label}`;
}

function persistSelectedRunCaseFromUI() {
  const idx = uiState.selectedRunCaseIndex;
  if (idx < 0 || idx >= uiState.runCases.length) return;
  const existing = uiState.runCases[idx] || {};
  const captured = captureRunCaseFromUI(caseNameOrFallback(existing, idx));
  uiState.runCases[idx] = {
    ...existing,
    ...captured,
    color: caseColorOrFallback(existing, idx),
    inputs: {
      ...(existing.inputs && typeof existing.inputs === 'object' ? existing.inputs : {}),
      ...(captured.inputs && typeof captured.inputs === 'object' ? captured.inputs : {}),
    },
  };
}

function selectRunCase(index, { apply = true } = {}) {
  if (!Number.isInteger(index) || index < 0 || index >= uiState.runCases.length) return;
  const previous = uiState.selectedRunCaseIndex;
  if (previous !== index) persistSelectedRunCaseFromUI();
  uiState.selectedRunCaseIndex = index;
  if (apply) applyRunCaseToUI(uiState.runCases[index]);
  renderRunCasesList();
  updateRunCasesMeta();
  drawEigenPlot();
}

function renderRunCasesList() {
  if (!els.runCaseList) return;
  els.runCaseList.innerHTML = '';
  if (!uiState.runCases.length) {
    uiState.selectedRunCaseIndex = -1;
    return;
  }
  if (uiState.selectedRunCaseIndex < 0 || uiState.selectedRunCaseIndex >= uiState.runCases.length) {
    uiState.selectedRunCaseIndex = 0;
  }
  uiState.runCases.forEach((entry, idx) => {
    const row = document.createElement('div');
    row.className = `run-case-item${idx === uiState.selectedRunCaseIndex ? ' active' : ''}`;
    row.dataset.index = String(idx);

    const title = document.createElement('input');
    title.type = 'text';
    title.className = 'run-case-title';
    title.value = caseNameOrFallback(entry, idx);
    title.addEventListener('mousedown', (evt) => {
      if (idx !== uiState.selectedRunCaseIndex) {
        evt.preventDefault();
        selectRunCase(idx, { apply: true });
      }
    });
    title.addEventListener('click', (evt) => {
      evt.stopPropagation();
      if (idx !== uiState.selectedRunCaseIndex) {
        selectRunCase(idx, { apply: true });
      }
    });
    title.addEventListener('keydown', (evt) => evt.stopPropagation());
    title.addEventListener('input', () => {
      entry.name = title.value;
    });
    title.addEventListener('change', () => {
      entry.name = String(title.value || '').trim() || `Case ${idx + 1}`;
      title.value = entry.name;
      updateRunCasesMeta();
    });

    const color = document.createElement('input');
    color.type = 'color';
    color.className = 'run-case-color';
    color.value = caseColorOrFallback(entry, idx);
    color.addEventListener('click', (evt) => evt.stopPropagation());
    color.addEventListener('input', () => {
      entry.color = color.value;
      drawEigenPlot();
    });

    const trash = document.createElement('button');
    trash.type = 'button';
    trash.className = 'btn ghost run-case-delete';
    trash.title = 'Delete case';
    trash.setAttribute('aria-label', 'Delete case');
    trash.innerHTML = `
      <svg class="run-case-delete-icon" viewBox="0 0 24 24" aria-hidden="true">
        <path d="M4 7h16" />
        <path d="M10 11v6" />
        <path d="M14 11v6" />
        <path d="M6 7l1 13h10l1-13" />
        <path d="M9 7V4h6v3" />
      </svg>
    `;
    trash.addEventListener('click', (evt) => {
      evt.stopPropagation();
      const removedSelected = idx === uiState.selectedRunCaseIndex;
      uiState.runCases.splice(idx, 1);
      uiState.eigenModesByRunCase = {};
      uiState.eigenModes = [];
      uiState.selectedEigenMode = -1;
      stopModeAnimation();
      if (!uiState.runCases.length) {
        uiState.selectedRunCaseIndex = -1;
      } else if (removedSelected) {
        uiState.selectedRunCaseIndex = Math.max(0, Math.min(idx, uiState.runCases.length - 1));
        applyRunCaseToUI(uiState.runCases[uiState.selectedRunCaseIndex]);
      } else if (idx < uiState.selectedRunCaseIndex) {
        uiState.selectedRunCaseIndex -= 1;
      }
      renderRunCasesList();
      updateRunCasesMeta();
      drawEigenPlot();
    });

    row.addEventListener('click', () => {
      selectRunCase(idx, { apply: true });
    });

    row.appendChild(title);
    row.appendChild(color);
    row.appendChild(trash);
    els.runCaseList.appendChild(row);
  });
}

function captureRunCaseFromUI(nameHint = '') {
  const massProps = uiState.massProps || readMassPropsFromUI();
  const rows = readConstraintRows().map((row) => ({
    variable: String(row.variable || ''),
    constraint: String(row.constraint || 'none'),
    numeric: Number.isFinite(row.numeric) ? row.numeric : 0,
  }));
  return {
    name: String(nameHint || '').trim(),
    color: caseColorOrFallback(null, uiState.runCases.length),
    inputs: {
      flightMode: String(els.flightMode?.value || 'level'),
      bank: Number(els.bank?.value || 0),
      cl: Number(els.cl?.value || 0),
      vel: Number(els.vel?.value || 0),
      mass: Number(els.mass?.value || 0),
      rho: Number(els.rho?.value || 0),
      gee: Number(els.gee?.value || 0),
      clLoop: Number(els.clLoop?.value || 0),
      velLoop: Number(els.velLoop?.value || 0),
      radLoop: Number(els.radLoop?.value || 0),
      facLoop: Number(els.facLoop?.value || 0),
      xcg: Number(massProps?.xcg || 0),
      ycg: Number(massProps?.ycg || 0),
      zcg: Number(massProps?.zcg || 0),
      ixx: Number(massProps?.ixx || 0),
      iyy: Number(massProps?.iyy || 0),
      izz: Number(massProps?.izz || 0),
      ixy: Number(massProps?.ixy || 0),
      iyz: Number(massProps?.iyz || 0),
      izx: Number(massProps?.ixz || 0),
    },
    constraints: rows,
  };
}

function syncFlightModePanels() {
  const isLoop = (els.flightMode?.value || 'level') === 'looping';
  els.levelInputs?.classList.toggle('hidden', isLoop);
  els.loopInputs?.classList.toggle('hidden', !isLoop);
}

function applyRunCaseToUI(entry) {
  if (!entry || typeof entry !== 'object') return;
  const inputs = entry.inputs || {};
  const setIfFinite = (el, value, digits = 3) => {
    const num = Number(value);
    if (!el || !Number.isFinite(num)) return;
    setNumericInput(el, num, digits);
  };
  if (els.flightMode) {
    let mode = String(inputs.flightMode || '').trim().toLowerCase();
    if (mode !== 'level' && mode !== 'looping') {
      const rad = Number(inputs.radLoop);
      mode = Number.isFinite(rad) && Math.abs(rad) > 1e-9 ? 'looping' : 'level';
    }
    if (mode === 'level' || mode === 'looping') {
      els.flightMode.value = mode;
    }
  }
  syncFlightModePanels();
  setIfFinite(els.bank, inputs.bank, 2);
  setIfFinite(els.cl, inputs.cl, 3);
  setIfFinite(els.vel, inputs.vel, 2);
  setIfFinite(els.mass, inputs.mass, 3);
  setIfFinite(els.rho, inputs.rho, 4);
  setIfFinite(els.gee, inputs.gee, 3);
  setIfFinite(els.clLoop, inputs.clLoop, 3);
  setIfFinite(els.velLoop, inputs.velLoop, 2);
  setIfFinite(els.radLoop, inputs.radLoop, 2);
  setIfFinite(els.facLoop, inputs.facLoop, 3);

  const updateMassProp = (obj, key, value) => {
    const num = Number(value);
    if (!Number.isFinite(num)) return false;
    if (!Number.isFinite(Number(obj[key])) || Math.abs(Number(obj[key]) - num) > 1e-12) {
      obj[key] = num;
      return true;
    }
    return false;
  };
  const nextMassProps = { ...(uiState.massProps || readMassPropsFromUI() || makeDefaultMassProps()) };
  let massChanged = false;
  massChanged = updateMassProp(nextMassProps, 'mass', inputs.mass) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'xcg', inputs.xcg) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'ycg', inputs.ycg) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'zcg', inputs.zcg) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'ixx', inputs.ixx) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'iyy', inputs.iyy) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'izz', inputs.izz) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'ixy', inputs.ixy) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'iyz', inputs.iyz) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'ixz', inputs.ixz) || massChanged;
  massChanged = updateMassProp(nextMassProps, 'ixz', inputs.izx) || massChanged;
  if (massChanged) {
    uiState.massProps = nextMassProps;
    renderMassProps(nextMassProps);
  }

  const byVar = new Map();
  (entry.constraints || []).forEach((row) => {
    const key = String(row?.variable || '');
    if (!key) return;
    byVar.set(key, row);
  });
  readConstraintRows().forEach((row) => {
    const saved = byVar.get(row.variable);
    if (!saved) return;
    if (row.select) {
      const nextConstraint = String(saved.constraint || 'none');
      if ([...row.select.options].some((opt) => opt.value === nextConstraint)) {
        row.select.value = nextConstraint;
      }
    }
    if (row.value) {
      const num = Number(saved.numeric);
      row.value.value = Number.isFinite(num) ? String(num) : row.value.value;
    }
  });

  updateConstraintDuplicates();
  updateFlightConditions();
  scheduleAutoTrim();
}

function normalizeRunCase(raw, index) {
  const name = String(raw?.name || `Case ${index + 1}`).trim() || `Case ${index + 1}`;
  const color = caseColorOrFallback(raw, index);
  const inputs = raw?.inputs && typeof raw.inputs === 'object' ? raw.inputs : {};
  const constraints = Array.isArray(raw?.constraints)
    ? raw.constraints.map((row) => ({
      variable: String(row?.variable || ''),
      constraint: String(row?.constraint || 'none'),
      numeric: Number(row?.numeric || 0),
    })).filter((row) => row.variable)
    : [];
  return { name, color, inputs, constraints };
}

function normalizeRunKey(text) {
  return String(text || '')
    .toLowerCase()
    .replaceAll('.', '')
    .replaceAll('_', '')
    .replaceAll('-', '')
    .replace(/\s+/g, ' ')
    .trim();
}

function parseAvlRunCasesText(text) {
  const lines = String(text || '').split(/\r?\n/);
  const cases = [];
  let current = null;
  const finalizeCase = (entry) => {
    if (!entry || typeof entry !== 'object') return;
    if (!entry.inputs || typeof entry.inputs !== 'object') entry.inputs = {};
    const modeRaw = String(entry.inputs.flightMode || '').trim().toLowerCase();
    if (modeRaw !== 'level' && modeRaw !== 'looping') {
      const rad = Number(entry.inputs.radLoop);
      entry.inputs.flightMode = (Number.isFinite(rad) && Math.abs(rad) > 1e-9) ? 'looping' : 'level';
    }
  };
  const startCase = (nameRaw) => {
    if (current) {
      finalizeCase(current);
      cases.push(current);
    }
    current = {
      name: String(nameRaw || '').trim().replace(/^[-\s]+|[-\s]+$/g, ''),
      inputs: {},
      constraints: [],
    };
  };

  const varMap = {
    alpha: 'alpha',
    beta: 'beta',
    'pb/2v': 'p',
    'qc/2v': 'q',
    'rb/2v': 'r',
  };
  const constraintMap = {
    alpha: 'alpha',
    beta: 'beta',
    'pb/2v': 'p',
    'qc/2v': 'q',
    'rb/2v': 'r',
    cl: 'cl',
    cy: 'cy',
    'cl roll mom': 'cmx',
    'cm pitchmom': 'cmy',
    'cn yaw mom': 'cmz',
  };

  for (const rawLine of lines) {
    const line = String(rawLine || '');
    const caseMatch = line.match(/^\s*Run case\s+\d+\s*:\s*(.*?)\s*$/i);
    if (caseMatch) {
      startCase(caseMatch[1]);
      continue;
    }
    if (!current) continue;

    const arrowMatch = line.match(/^\s*([A-Za-z0-9_./+\- ]+?)\s*->\s*([A-Za-z0-9_./+\- ]+?)\s*=\s*([+\-]?(?:\d*\.\d+|\d+)(?:[Ee][+\-]?\d+)?)\s*$/);
    if (arrowMatch) {
      const leftRaw = arrowMatch[1].trim();
      const rightRaw = arrowMatch[2].trim();
      const num = Number(arrowMatch[3]);
      const leftKeyNorm = normalizeRunKey(leftRaw);
      const rightKeyNorm = normalizeRunKey(rightRaw);
      const variable = varMap[leftKeyNorm] || `ctrl:${leftRaw}`;
      const constraint = constraintMap[rightKeyNorm] || (rightRaw ? `ctrl:${rightRaw}` : 'none');
      if (Number.isFinite(num)) {
        current.constraints.push({ variable, constraint, numeric: num });
      }
      continue;
    }

    const valueMatch = line.match(/^\s*([A-Za-z0-9_./+\- ]+?)\s*=\s*([+\-]?(?:\d*\.\d+|\d+)(?:[Ee][+\-]?\d+)?)\s*(.*?)\s*$/);
    if (!valueMatch) continue;
    const keyRaw = valueMatch[1].trim();
    const value = Number(valueMatch[2]);
    const unitRaw = valueMatch[3] || '';
    const keyNorm = normalizeRunKey(keyRaw);
    if (!Number.isFinite(value)) continue;

    const hasMappedVariable = (variableKey) => current.constraints.some((row) => row.variable === variableKey);

    if (keyNorm === 'bank') current.inputs.bank = value;
    else if (keyNorm === 'velocity') current.inputs.vel = value;
    else if (keyNorm === 'density') current.inputs.rho = value;
    else if (keyNorm === 'gravacc') current.inputs.gee = value;
    else if (keyNorm === 'turnrad') current.inputs.radLoop = value;
    else if (keyNorm === 'loadfac') current.inputs.facLoop = value;
    else if (keyNorm === 'cl') current.inputs.cl = value;
    else if (keyNorm === 'mass') current.inputs.mass = value;
    else if (keyNorm === 'cdo' || keyNorm === 'cd0') current.inputs.cd0 = value;
    else if (keyNorm === 'mach') current.inputs.mach = value;
    else if (keyNorm === 'xcg') current.inputs.xcg = value;
    else if (keyNorm === 'ycg') current.inputs.ycg = value;
    else if (keyNorm === 'zcg') current.inputs.zcg = value;
    else if (keyNorm === 'ixx') current.inputs.ixx = value;
    else if (keyNorm === 'iyy') current.inputs.iyy = value;
    else if (keyNorm === 'izz') current.inputs.izz = value;
    else if (keyNorm === 'ixy') current.inputs.ixy = value;
    else if (keyNorm === 'iyz') current.inputs.iyz = value;
    else if (keyNorm === 'izx') current.inputs.izx = value;
    else if (keyNorm === 'alpha' && !hasMappedVariable('alpha')) current.constraints.push({ variable: 'alpha', constraint: 'alpha', numeric: value });
    else if (keyNorm === 'beta' && !hasMappedVariable('beta')) current.constraints.push({ variable: 'beta', constraint: 'beta', numeric: value });
    else if (keyNorm === 'pb/2v' && !hasMappedVariable('p')) current.constraints.push({ variable: 'p', constraint: 'p', numeric: value });
    else if (keyNorm === 'qc/2v' && !hasMappedVariable('q')) current.constraints.push({ variable: 'q', constraint: 'q', numeric: value });
    else if (keyNorm === 'rb/2v' && !hasMappedVariable('r')) current.constraints.push({ variable: 'r', constraint: 'r', numeric: value });
  }
  if (current) {
    finalizeCase(current);
    cases.push(current);
  }

  const normalized = cases.map((entry, idx) => normalizeRunCase(entry, idx));
  return normalized.filter((entry) => entry && (entry.name || entry.constraints.length || Object.keys(entry.inputs || {}).length));
}

function parseRunsPayload(text) {
  let cases = [];
  let selectedIndex = 0;
  try {
    const parsed = JSON.parse(text);
    if (Array.isArray(parsed)) {
      cases = parsed;
    } else if (parsed && typeof parsed === 'object') {
      if (Array.isArray(parsed.cases)) cases = parsed.cases;
      if (Number.isInteger(parsed.selectedIndex)) selectedIndex = parsed.selectedIndex;
    }
  } catch {
    const parsedCases = parseAvlRunCasesText(text);
    if (parsedCases.length) return { cases: parsedCases, selectedIndex: 0 };
    throw new Error('Unsupported run-case file format.');
  }
  const normalized = cases.map((entry, idx) => normalizeRunCase(entry, idx));
  if (!normalized.length) throw new Error('No run cases found in file.');
  if (selectedIndex < 0 || selectedIndex >= normalized.length) selectedIndex = 0;
  return { cases: normalized, selectedIndex };
}

function applyLoadedRunCases(parsed, filename, source = 'Loaded') {
  uiState.eigenModesByRunCase = {};
  uiState.eigenModes = [];
  uiState.selectedEigenMode = -1;
  stopModeAnimation();
  uiState.runCases = parsed.cases;
  uiState.selectedRunCaseIndex = parsed.selectedIndex;
  uiState.runCasesFilename = filename;
  uiState.needsRunCaseConstraintSync = true;
  renderRunCasesList();
  updateRunCasesMeta(`${source} ${parsed.cases.length} run case(s) from ${filename}`);
  applyRunCaseToUI(uiState.runCases[uiState.selectedRunCaseIndex]);
  drawEigenPlot();
}

function applyLoadedMassProps(props, filename, source = 'Loaded') {
  uiState.massPropsFilename = filename;
  uiState.massProps = props;
  renderMassProps(props);
  if (els.mass) setNumericInput(els.mass, props.mass, 4);
  const hasActiveRunCase = Array.isArray(uiState.runCases) && uiState.runCases.length > 0 && uiState.selectedRunCaseIndex >= 0;
  if (!hasActiveRunCase) {
    if (els.gee) setNumericInput(els.gee, props.g, 4);
    if (els.rho) setNumericInput(els.rho, props.rho, 6);
  }
  scheduleAutoTrim();
  logDebug(`${source} mass file: ${filename}`);
}

function resetAuxPanelsForNewAvl() {
  uiState.runCases = [];
  uiState.selectedRunCaseIndex = -1;
  uiState.runCasesFilename = null;
  uiState.needsRunCaseConstraintSync = false;
  renderRunCasesList();
  updateRunCasesMeta();
  uiState.eigenModesByRunCase = {};
  uiState.eigenModes = [];
  uiState.selectedEigenMode = -1;
  stopModeAnimation();
  drawEigenPlot();

  uiState.massPropsFilename = null;
  uiState.massProps = makeDefaultMassProps();
  renderMassProps(uiState.massProps);
}

function buildRunsPayload() {
  return JSON.stringify({
    format: 'vibelattice-runs-v1',
    selectedIndex: uiState.selectedRunCaseIndex >= 0 ? uiState.selectedRunCaseIndex : 0,
    cases: uiState.runCases.map((entry, idx) => normalizeRunCase(entry, idx)),
  }, null, 2);
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
  const lower = String(file.name || '').toLowerCase();
  const isAvl = lower.endsWith('.avl');
  uiState.text = text;
  uiState.filename = file.name;
  setFileTextValue(uiState.text);
  els.fileMeta.textContent = `Loaded: ${file.name} (${file.size} bytes)`;
  loadGeometryFromText(uiState.text, true);
  if (isAvl) {
    resetAuxPanelsForNewAvl();
  }
  resetTrimSeed();
  applyTrim({ useSeed: false });
  logDebug(`Loaded file: ${file.name}`);
}

async function loadRunCasesFromFile(file, source = 'Loaded') {
  const text = await readFileAsText(file);
  const parsed = parseRunsPayload(text);
  applyLoadedRunCases(parsed, file.name, source);
  logDebug(`${source} run cases: ${file.name}`);
}

async function loadMassPropsFromFile(file, source = 'Loaded') {
  const text = await readFileAsText(file);
  const props = parseMassFileText(text);
  applyLoadedMassProps(props, file.name, source);
}

async function handleFileSelection(files) {
  const all = Array.from(files || []);
  if (!all.length) return;

  const avlFiles = [];
  const runFiles = [];
  const massFiles = [];
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
    if (lower.endsWith('.avl')) avlFiles.push(file);
    else if (lower.endsWith('.run')) runFiles.push(file);
    else if (lower.endsWith('.mass')) massFiles.push(file);
    else if (lower.endsWith('.txt')) textFiles.push(file);
  }

  const primaryAvl = avlFiles[0] || textFiles[0] || null;
  if (primaryAvl) {
    await handleFileLoad(primaryAvl);
  } else if (!runFiles.length && !massFiles.length) {
    renderRequiredAirfoilFiles();
    scheduleAutoTrim();
    els.fileMeta.textContent = `Loaded dependencies: ${all.length} file(s)`;
  }

  if (runFiles[0]) {
    try {
      await loadRunCasesFromFile(runFiles[0], 'Loaded');
    } catch (err) {
      updateRunCasesMeta(`Failed to load ${runFiles[0].name}`);
      logDebug(`Run cases load failed: ${err?.message ?? err}`);
    }
  }
  if (massFiles[0]) {
    try {
      await loadMassPropsFromFile(massFiles[0], 'Loaded');
    } catch (err) {
      if (els.massPropsMeta) els.massPropsMeta.textContent = `Failed to load ${massFiles[0].name}`;
      logDebug(`Mass file load failed: ${err?.message ?? err}`);
    }
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

async function fetchTextFromCandidates(candidates) {
  for (const url of candidates) {
    try {
      const res = await fetch(url);
      if (!res.ok) continue;
      const text = await res.text();
      if (!text.trim()) continue;
      return text;
    } catch {
      // try next
    }
  }
  return null;
}

async function loadDefaultRunAndMass() {
  const runCandidates = [
    new URL('./third_party/avl/runs/plane.run', window.location.href).toString(),
    new URL('../third_party/avl/runs/plane.run', window.location.href).toString(),
    new URL('../../third_party/avl/runs/plane.run', window.location.href).toString(),
  ];
  const runText = await fetchTextFromCandidates(runCandidates);
  if (runText) {
    try {
      const parsed = parseRunsPayload(runText);
      applyLoadedRunCases(parsed, 'plane.run', 'Loaded default');
      logDebug('Loaded default run cases: plane.run');
    } catch (err) {
      logDebug(`Default run-case load failed: ${err?.message ?? err}`);
    }
  }

  const massCandidates = [
    new URL('./third_party/avl/runs/plane.mass', window.location.href).toString(),
    new URL('../third_party/avl/runs/plane.mass', window.location.href).toString(),
    new URL('../../third_party/avl/runs/plane.mass', window.location.href).toString(),
  ];
  const massText = await fetchTextFromCandidates(massCandidates);
  if (massText) {
    try {
      const props = parseMassFileText(massText);
      applyLoadedMassProps(props, 'plane.mass', 'Loaded default');
    } catch (err) {
      logDebug(`Default mass load failed: ${err?.message ?? err}`);
    }
  }
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

els.runCasesInput?.addEventListener('change', async (evt) => {
  const file = evt.target?.files?.[0];
  evt.target.value = '';
  if (!file) return;
  try {
    await loadRunCasesFromFile(file, 'Loaded');
  } catch (err) {
    updateRunCasesMeta(`Failed to load ${file.name}`);
    logDebug(`Run cases load failed: ${err?.message ?? err}`);
  }
});

els.runCasesSaveBtn?.addEventListener('click', () => {
  if (!uiState.runCases.length) {
    const created = captureRunCaseFromUI('Case 1');
    created.color = caseColorOrFallback(created, 0);
    uiState.runCases = [created];
    uiState.selectedRunCaseIndex = 0;
    renderRunCasesList();
  }
  persistSelectedRunCaseFromUI();
  const filename = ensureRunFileExtension(uiState.runCasesFilename || 'cases.run');
  uiState.runCasesFilename = filename;
  downloadText(filename, buildRunsPayload());
  updateRunCasesMeta(`Saved ${uiState.runCases.length} run case(s) to ${filename}`);
});

els.runCaseAddBtn?.addEventListener('click', () => {
  persistSelectedRunCaseFromUI();
  const name = `Case ${uiState.runCases.length + 1}`;
  const created = captureRunCaseFromUI(name);
  created.color = caseColorOrFallback(created, uiState.runCases.length);
  uiState.runCases.push(created);
  uiState.eigenModesByRunCase = {};
  uiState.eigenModes = [];
  uiState.selectedEigenMode = -1;
  stopModeAnimation();
  uiState.selectedRunCaseIndex = uiState.runCases.length - 1;
  applyRunCaseToUI(uiState.runCases[uiState.selectedRunCaseIndex]);
  renderRunCasesList();
  updateRunCasesMeta();
  drawEigenPlot();
});

els.massPropsInput?.addEventListener('change', async (evt) => {
  const file = evt.target?.files?.[0];
  evt.target.value = '';
  if (!file) return;
  try {
    await loadMassPropsFromFile(file, 'Loaded');
  } catch (err) {
    if (els.massPropsMeta) els.massPropsMeta.textContent = `Failed to load ${file.name}`;
    logDebug(`Mass file load failed: ${err?.message ?? err}`);
  }
});

els.massPropsSaveBtn?.addEventListener('click', () => {
  const props = readMassPropsFromUI();
  uiState.massProps = props;
  const filename = ensureMassFileExtension(uiState.massPropsFilename || 'model.mass');
  uiState.massPropsFilename = filename;
  downloadText(filename, serializeMassFile(props));
  renderMassProps(props);
});

[
  els.massTotal, els.massXcg, els.massYcg, els.massZcg,
  els.massIxx, els.massIyy, els.massIzz, els.massIxy, els.massIxz, els.massIyz,
].forEach((el) => {
  if (!el) return;
  el.addEventListener('input', () => {
    uiState.massProps = readMassPropsFromUI();
  });
  el.addEventListener('change', () => {
    uiState.massProps = readMassPropsFromUI();
    renderMassProps(uiState.massProps);
    scheduleAutoTrim();
  });
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
els.viewerLoad?.addEventListener('click', () => {
  uiState.showLoading = !uiState.showLoading;
  els.viewerLoad.classList.toggle('active', uiState.showLoading);
  updateLoadingVisualization();
});
els.viewerSurface?.addEventListener('click', () => {
  const cycle = ['wireframe', 'both', 'surface'];
  const idx = cycle.indexOf(uiState.surfaceRenderMode);
  uiState.surfaceRenderMode = cycle[(idx + 1 + cycle.length) % cycle.length];
  updateViewerButtons();
  applySurfaceRenderMode();
});
els.viewerPressure?.addEventListener('click', () => {
  uiState.showPressure = !uiState.showPressure;
  if (uiState.showPressure && uiState.surfaceRenderMode !== 'surface') {
    uiState.surfaceRenderMode = 'surface';
  }
  updateViewerButtons();
  applySurfaceRenderMode();
});
els.viewerPanelSpacing?.addEventListener('click', () => {
  uiState.showPanelSpacing = !uiState.showPanelSpacing;
  applyAuxOverlayVisibility();
  updateViewerButtons();
});
els.viewerVortices?.addEventListener('click', () => {
  uiState.showVortices = !uiState.showVortices;
  applyAuxOverlayVisibility();
  updateViewerButtons();
});
els.viewerFlow?.addEventListener('click', () => {
  if (!uiState.showFlowField) {
    uiState.showFlowField = true;
  } else {
    const idx = FLOW_FIELD_MODES.indexOf(uiState.flowFieldMode);
    const safeIdx = idx >= 0 ? idx : 0;
    if (safeIdx >= FLOW_FIELD_MODES.length - 1) {
      uiState.showFlowField = false;
      uiState.flowFieldMode = FLOW_FIELD_MODES[0];
    } else {
      uiState.flowFieldMode = FLOW_FIELD_MODES[safeIdx + 1];
    }
  }
  if (aircraft) {
    const bounds = computeBounds(aircraft);
    rebuildAuxOverlays(bounds || null);
  } else {
    applyAuxOverlayVisibility();
  }
  updateViewerButtons();
});
els.eigenPlot?.addEventListener('click', handleEigenCanvasClick);
els.eigenPlot?.addEventListener('wheel', handleEigenCanvasWheel, { passive: false });
els.eigenPlot?.addEventListener('touchstart', handleEigenTouchStart, { passive: false });
els.eigenPlot?.addEventListener('touchmove', handleEigenTouchMove, { passive: false });
els.eigenPlot?.addEventListener('touchend', handleEigenTouchEnd, { passive: false });
els.eigenPlot?.addEventListener('touchcancel', handleEigenTouchEnd, { passive: false });

els.flightMode?.addEventListener('change', () => {
  syncFlightModePanels();
  updateFlightConditions();
});

if (els.flightMode) {
  syncFlightModePanels();
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
  uiState.text = els.fileText.value;
  syncTemplateParamsFromText(uiState.text);
  clearTimeout(fileUpdateTimer);
  fileUpdateTimer = setTimeout(() => {
    loadGeometryFromText(uiState.text, true);
  }, 250);
});
els.fileText.addEventListener('scroll', syncFileEditorScroll);
window.addEventListener('resize', fitFileEditorFontSize);
window.addEventListener('resize', scheduleOutputGridFontFit);
els.fileAircraftName?.addEventListener('change', applyAircraftNameRename);
els.fileAircraftName?.addEventListener('keydown', (evt) => {
  if (evt.key !== 'Enter') return;
  evt.preventDefault();
  applyAircraftNameRename();
});
[
  els.fileIysym, els.fileIzsym, els.fileZsym,
  els.fileSref, els.fileCref, els.fileBref,
  els.fileXref, els.fileYref, els.fileZref,
].forEach((el) => {
  if (!el) return;
  el.addEventListener('change', applyAircraftRefRename);
  el.addEventListener('keydown', (evt) => {
    if (evt.key !== 'Enter') return;
    evt.preventDefault();
    applyAircraftRefRename();
  });
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
    syncTemplateParamsFromText(uiState.text || '');
    const model = buildSolverModel(resolveTemplateParamsInText(uiState.text || ''));
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

  if (els.outAlpha) els.outAlpha.textContent = fmtSignedAligned(findVar('alpha'), 2);
  if (els.outBeta) els.outBeta.textContent = fmtSignedAligned(findVar('beta'), 2);
  if (els.outBank) els.outBank.textContent = fmt(phi, 2);
  if (els.outCL) els.outCL.textContent = fmtSignedAligned(cl, 5);
  if (els.outV) els.outV.textContent = fmt(vee, 2);
  if (els.outRad) els.outRad.textContent = rad > 0 ? fmt(rad, 2) : 'level';
  if (els.outFac) els.outFac.textContent = fmt(fac, 3);
  if (els.outThe) els.outThe.textContent = fmt(the, 2);
  if (els.outPb2v) els.outPb2v.textContent = fmtSignedAligned(findVar('p'), 3);
  if (els.outQc2v) els.outQc2v.textContent = fmtSignedAligned(findVar('q'), 3);
  if (els.outRb2v) els.outRb2v.textContent = fmtSignedAligned(findVar('r'), 3);
  if (els.outRates) els.outRates.textContent = `${fmt(findVar('p'), 2)}, ${fmt(findVar('q'), 2)}, ${fmt(findVar('r'), 2)}`;
  if (els.outDef) els.outDef.textContent = '-';
  renderControlRows([]);
  if (els.outMach) {
    const mach = Number(uiState.modelHeader?.mach);
    els.outMach.textContent = Number.isFinite(mach) ? fmtSignedAligned(mach, 3) : '-';
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

  const panel = canvas.parentElement;
  if (!hasTrefftz) {
    const legend = panel?.querySelector?.('.trefftz-legend');
    const axisLabels = panel?.querySelector?.('.trefftz-axis-labels');
    if (legend) legend.style.display = 'none';
    if (axisLabels) axisLabels.style.display = 'none';
    panel?.querySelector?.('.trefftz-ticks')?.remove?.();
    panel?.querySelector?.('.trefftz-axis-lines')?.remove?.();
    panel?.querySelector?.('.trefftz-grid-lines')?.remove?.();
    if (window.__trefftzTestHook) {
      window.__trefftzTestHook.gridY = [];
      window.__trefftzTestHook.zeroLine = null;
      window.__trefftzTestHook.mapAxis = null;
      window.__trefftzTestHook.trefftzRightAxis = null;
    }
    uiState.trefftzLayoutVersion = (uiState.trefftzLayoutVersion || 0) + 1;
    if (window.__trefftzTestHook) {
      window.__trefftzTestHook.layoutVersion = uiState.trefftzLayoutVersion;
      window.__trefftzTestHook.layoutReady = !document.fonts || document.fonts.status === 'loaded';
    }
    return;
  }

  let legend = canvas.parentElement?.querySelector?.('.trefftz-legend');
  if (!legend) {
    legend = document.createElement?.('div');
    if (legend) {
      legend.className = 'trefftz-legend';
      canvas.parentElement?.appendChild(legend);
    }
  }
  if (legend) legend.style.display = '';
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
  if (axisLabels) axisLabels.style.display = '';
  if (axisLabels) {
    axisLabels.innerHTML = `
      <div class="trefftz-axis-label x">Span</div>
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

  {
    const { strips, surfaces, cref } = uiState.trefftzData;
    let ymin = Infinity;
    let ymax = -Infinity;
    let fmin = 0.0;
    let fmax = 0.0;
    let cmin = 0.0;
    let cmax = 0.0;
    let wmin = 0.0;
    let wmax = 0.0;
    let wminData = Infinity;
    let wmaxData = -Infinity;

    strips.forEach(([y, _z, cnc, clVal, clPerp, dw]) => {
      if (y < ymin) ymin = y;
      if (y > ymax) ymax = y;
      fmin = Math.min(fmin, cnc, 0.0);
      fmax = Math.max(fmax, cnc, 0.0);
      cmin = Math.min(cmin, clPerp, clVal, 0.0);
      cmax = Math.max(cmax, clPerp, clVal, 0.0);
      const ai = -dw;
      wminData = Math.min(wminData, ai);
      wmaxData = Math.max(wmaxData, ai);
    });

    if (Number.isFinite(wminData)) wmin = wminData;
    if (Number.isFinite(wmaxData)) wmax = wmaxData;
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
    {
      const cSpanRaw = Math.abs(cmax - cmin);
      const cPad = Math.max(0.05, 0.1 * (cSpanRaw > 1e-6 ? cSpanRaw : Math.max(Math.abs(cmax), 0.1)));
      if (cmin >= 0) cmin = -cPad;
      if (cmax <= 0) cmax = cPad;
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

    const wminRaw = Math.min(wmin, 0);
    const wmaxRaw = Math.max(wmax, 0);
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
    const cSpan = Math.max(cmax - cmin, 1e-6);
    let zeroIdx = Math.round(((0 - cmin) / cSpan) * ticks);
    zeroIdx = Math.max(1, Math.min(ticks - 1, zeroIdx));
    leftTickNums[zeroIdx] = 0;
    leftTickVals[zeroIdx] = '0.0';

    const zeroFrac = zeroIdx / ticks;
    const zeroFracUsed = zeroFrac;
    const minSpanNeg = wminRaw < 0 ? Math.abs(wminRaw) / zeroFracUsed : 0.0;
    const minSpanPos = wmaxRaw > 0 ? Math.abs(wmaxRaw) / (1 - zeroFracUsed) : 0.0;
    const wSpan = Math.max(0.1, minSpanNeg, minSpanPos) * 1.05;
    wminAdj = -zeroFracUsed * wSpan;
    wmaxAdj = (1 - zeroFracUsed) * wSpan;

    const rightTickNums = [];
    for (let i = 0; i <= ticks; i += 1) {
      const t = i / ticks;
      const wVal = wminAdj + t * (wmaxAdj - wminAdj);
      rightTickNums.push(wVal);
      rightTickVals.push(fmt(wVal, 2));
    }
    let rightZeroIdx = 0;
    let rightZeroDist = Infinity;
    rightTickNums.forEach((val, idx) => {
      const dist = Math.abs(val);
      if (dist < rightZeroDist) {
        rightZeroDist = dist;
        rightZeroIdx = idx;
      }
    });
    if (rightTickVals[rightZeroIdx]) {
      rightTickVals[rightZeroIdx] = '0.00';
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
    const yForW = (val) => axisY(val, wminAdj, wmaxAdj);

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
      const zeroWLine = yForW(0);
      window.__trefftzTestHook.gridY = gridYPositions;
      window.__trefftzTestHook.zeroLine = zeroLine;
      window.__trefftzTestHook.mapAxis = {
        axisY,
        axisYWithZero,
      };
      window.__trefftzTestHook.trefftzRightAxis = {
        wminRaw,
        wmaxRaw,
        wminAdj,
        wmaxAdj,
        zeroFrac,
        zeroFracUsed,
        zeroWLine,
      };
    }

    if (!uiState.trefftzZeroLogged) {
      uiState.trefftzZeroLogged = true;
      const zeroWLine = yForW(0);
      logDebug(`Trefftz lines: y0=${fmt(y0, 2)} y1=${fmt(y1, 2)} zeroCL=${fmt(zeroLine, 2)} zeroAI=${fmt(zeroWLine, 2)}`);
    }

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
    setEigenModes(modes) {
      uiState.eigenModes = Array.isArray(modes) ? modes : [];
      uiState.selectedEigenMode = -1;
      uiState.eigenZoom = 1;
      uiState.eigenCenterRe = 0;
      uiState.eigenCenterIm = 0;
      drawEigenPlot();
    },
    getSelectedEigenMode() {
      return uiState.selectedEigenMode;
    },
    getEigenPoints() {
      return Array.isArray(uiState.eigenPoints) ? uiState.eigenPoints.map((p) => ({ ...p })) : [];
    },
    getEigenViewport() {
      return uiState.eigenViewport ? { ...uiState.eigenViewport } : null;
    },
    getRunCases() {
      return Array.isArray(uiState.runCases)
        ? uiState.runCases.map((entry) => ({
          ...entry,
          inputs: entry?.inputs && typeof entry.inputs === 'object' ? { ...entry.inputs } : {},
          constraints: Array.isArray(entry?.constraints) ? entry.constraints.map((row) => ({ ...row })) : [],
        }))
        : [];
    },
    getSelectedRunCaseIndex() {
      return Number(uiState.selectedRunCaseIndex);
    },
    getLastExecSummary() {
      const result = uiState.lastExecResult;
      if (!result || typeof result !== 'object') return null;
      const idx2 = (i, j, dim1) => i + dim1 * j;
      const par = result.PARVAL;
      const hasPar = Array.isArray(par) || (typeof ArrayBuffer !== 'undefined' && ArrayBuffer.isView?.(par));
      const machPar = hasPar ? Number(par[idx2(11, 1, 30)]) : Number.NaN;
      return {
        CDTOT: Number(result.CDTOT),
        CDVTOT: Number(result.CDVTOT),
        machPar: Number.isFinite(machPar) ? machPar : null,
      };
    },
    getViewerOverlayState() {
      const panelLinePos = panelSpacingGroup?.getObjectByName?.('panel-spacing-lines')?.geometry?.getAttribute?.('position');
      const vortexBoundPos = vortexGroup?.getObjectByName?.('bound-vortices')?.geometry?.getAttribute?.('position');
      const vortexLegPos = vortexGroup?.getObjectByName?.('leg-vortices')?.geometry?.getAttribute?.('position');
      return {
        showPanelSpacing: Boolean(uiState.showPanelSpacing),
        showVortices: Boolean(uiState.showVortices),
        showFlowField: Boolean(uiState.showFlowField),
        hasPanelSpacing: Boolean(panelSpacingGroup),
        hasVortices: Boolean(vortexGroup),
        hasFlow: Boolean(flowFieldGroup),
        panelSpacingVisible: Boolean(panelSpacingGroup?.visible),
        vorticesVisible: Boolean(vortexGroup?.visible),
        flowVisible: Boolean(flowFieldGroup?.visible),
        flowMode: uiState.flowFieldMode,
        panelSpacingLineSegments: Math.floor((Number(panelLinePos?.count) || 0) / 2),
        vortexBoundSegments: Math.floor((Number(vortexBoundPos?.count) || 0) / 2),
        vortexLegSegments: Math.floor((Number(vortexLegPos?.count) || 0) / 2),
      };
    },
    getVortexLegSymmetryStats(options = {}) {
      const segments = Array.isArray(latestVortexData?.segments) ? latestVortexData.segments : [];
      const minX = Number.isFinite(Number(options.minX)) ? Number(options.minX) : 5.3;
      const maxX = Number.isFinite(Number(options.maxX)) ? Number(options.maxX) : 7.2;
      const minZ = Number.isFinite(Number(options.minZ)) ? Number(options.minZ) : 0.2;
      const maxZ = Number.isFinite(Number(options.maxZ)) ? Number(options.maxZ) : 1.1;
      const minAbsY = Number.isFinite(Number(options.minAbsY)) ? Number(options.minAbsY) : 0.25;
      const maxAbsY = Number.isFinite(Number(options.maxAbsY)) ? Number(options.maxAbsY) : 2.4;
      const sum = {
        left: { n: 0, x: 0, y: 0, z: 0 },
        right: { n: 0, x: 0, y: 0, z: 0 },
      };
      segments.forEach((seg) => {
        if (String(seg?.kind || '') !== 'leg') return;
        const a = Array.isArray(seg?.a) ? seg.a : null;
        const b = Array.isArray(seg?.b) ? seg.b : null;
        if (!a || !b || a.length < 3 || b.length < 3) return;
        const x0 = Number(a[0]);
        const y0 = Number(a[1]);
        const z0 = Number(a[2]);
        const x1 = Number(b[0]);
        const y1 = Number(b[1]);
        const z1 = Number(b[2]);
        if (!Number.isFinite(x0) || !Number.isFinite(y0) || !Number.isFinite(z0)
          || !Number.isFinite(x1) || !Number.isFinite(y1) || !Number.isFinite(z1)) return;
        if (x0 < minX || x0 > maxX || z0 < minZ || z0 > maxZ) return;
        if (Math.abs(y0) < minAbsY || Math.abs(y0) > maxAbsY) return;
        const dx = x1 - x0;
        const dy = y1 - y0;
        const dz = z1 - z0;
        const mag = Math.hypot(dx, dy, dz);
        if (!(mag > 1e-9)) return;
        const side = y0 < 0 ? 'left' : 'right';
        sum[side].n += 1;
        sum[side].x += dx / mag;
        sum[side].y += dy / mag;
        sum[side].z += dz / mag;
      });
      const normalize = (entry) => {
        if (!entry.n) return null;
        const mx = entry.x / entry.n;
        const my = entry.y / entry.n;
        const mz = entry.z / entry.n;
        const m = Math.hypot(mx, my, mz);
        if (!(m > 1e-9)) return null;
        return { n: entry.n, x: mx / m, y: my / m, z: mz / m };
      };
      const left = normalize(sum.left);
      const right = normalize(sum.right);
      let angleDeg = null;
      if (left && right) {
        const dot = Math.max(-1, Math.min(1, left.x * right.x + left.y * right.y + left.z * right.z));
        angleDeg = Math.acos(dot) * (180 / Math.PI);
      }
      return {
        left,
        right,
        angleDeg,
        filters: { minX, maxX, minZ, maxZ, minAbsY, maxAbsY },
      };
    },
    getViewerViewState() {
      const mode = viewerState.viewModes[viewerState.viewIndex] || null;
      return {
        mode,
        label: mode ? (VIEW_MODE_LABELS[mode] || mode) : null,
        cameraPosition: camera ? {
          x: Number(camera.position?.x) || 0,
          y: Number(camera.position?.y) || 0,
          z: Number(camera.position?.z) || 0,
        } : null,
        cameraUp: camera ? {
          x: Number(camera.up?.x) || 0,
          y: Number(camera.up?.y) || 0,
          z: Number(camera.up?.z) || 0,
        } : null,
      };
    },
    getTemplateParams() {
      return Array.isArray(uiState.templateParams)
        ? uiState.templateParams.map((param) => ({
          name: param.name,
          kind: param.kind,
          defaultValue: Number(param.defaultValue),
          value: Number(param.value),
          min: Number(param.min),
          max: Number(param.max),
          step: Number(param.step),
          decimals: Number(param.decimals || 0),
          hasExplicitDefault: Boolean(param.hasExplicitDefault),
          positiveOnly: Boolean(param.positiveOnly),
        }))
        : [];
    },
    setTemplateParamValue(name, value, applyNow = true) {
      const key = String(name || '').trim();
      if (!key) return false;
      const params = Array.isArray(uiState.templateParams) ? uiState.templateParams : [];
      const param = params.find((item) => item?.name === key);
      if (!param) return false;
      const nextRaw = Number(value);
      if (!Number.isFinite(nextRaw)) return false;
      const next = param.kind === 'int'
        ? Math.round(clampNumber(nextRaw, param.min, param.max))
        : clampNumber(nextRaw, param.min, param.max);
      if (!Number.isFinite(next)) return false;
      param.value = next;
      param.touched = true;
      updateTemplateParamControlValues();
      if (applyNow) {
        if (templateParamApplyTimer) clearTimeout(templateParamApplyTimer);
        templateParamApplyTimer = null;
        uiState.text = els.fileText?.value || uiState.text || '';
        loadGeometryFromText(uiState.text, false);
        resetTrimSeed();
        scheduleAutoTrim();
      }
      return true;
    },
    getResolvedAvlText() {
      const rawText = els.fileText?.value || uiState.text || '';
      syncTemplateParamsFromText(rawText);
      const resolvedText = resolveTemplateParamsInText(rawText);
      uiState.resolvedText = resolvedText;
      return resolvedText;
    },
    setFlowSolverData(data) {
      const payload = (data && typeof data === 'object') ? data : {};
      uiState.lastExecResult = { ...(uiState.lastExecResult || {}), ...payload };
      if (aircraft && uiState.displayModel) {
        rebuildAuxOverlays(computeBounds(aircraft));
      }
    },
    setFlowFieldActive(active) {
      uiState.showFlowField = Boolean(active);
      if (aircraft && uiState.displayModel && !flowFieldGroup) {
        rebuildAuxOverlays(computeBounds(aircraft) || null);
      }
      applyAuxOverlayVisibility();
      updateViewerButtons();
    },
    getFlowFieldStats() {
      const tracer = flowFieldGroup?.getObjectByName?.('flow-tracer');
      const tracerCfg = tracer?.userData?.flowTracer;
      if (tracer && tracerCfg) {
        const trailLine = tracerCfg.trailLine;
        const trailAlphaAttr = trailLine?.geometry?.getAttribute?.('alpha');
        const cloudCount = Math.max(0, Math.floor(Number(tracerCfg.cloudCount) || 0));
        return {
          count: 1 + cloudCount,
          maxRadius: Math.hypot(
            Number(tracer.position?.x) || 0,
            Number(tracer.position?.y) || 0,
            Number(tracer.position?.z) || 0,
          ),
          lineOpacities: [],
          xUniqueCount: 1 + cloudCount,
          source: flowFieldGroup?.userData?.flowSource || null,
          vortexCount: Number(flowFieldGroup?.userData?.flowVortexCount) || 0,
          mode: flowFieldGroup?.userData?.flowMode || null,
          deltaOnly: Boolean(flowFieldGroup?.userData?.flowDeltaOnly),
          attachedToAircraft: Boolean(aircraft && flowFieldGroup?.parent === aircraft),
          includesRigidBodyRotation: Boolean(flowFieldGroup?.userData?.flowIncludesRigidBodyRotation),
          hasAlphaAttr: Boolean(trailAlphaAttr?.count),
          hasArrowheads: false,
          hasTrails: Boolean(flowFieldGroup?.userData?.flowTrails),
          cycleSeconds: null,
          isTracer: true,
          tracerRadius: Number(tracer.material?.size) || null,
        };
      }
      const flowLines = Array.isArray(flowFieldGroup?.children)
        ? flowFieldGroup.children.filter((obj) => obj?.isLineSegments && String(obj.name || '').startsWith('flow-vectors'))
        : [];
      if (!flowLines.length) return null;
      let maxRadius = 0;
      let count = 0;
      const uniqueX = new Set();
      const lineOpacities = [];
      let hasAlphaAttr = false;
      let cycleSeconds = null;
      let hasArrowheads = false;
      let hasTrails = false;
      flowLines.forEach((line) => {
        const pos = line.geometry?.getAttribute?.('position');
        const alphaAttr = line.geometry?.getAttribute?.('alpha');
        if (alphaAttr?.count) hasAlphaAttr = true;
        const vertsPerArrow = Number(line.userData?.flowAnim?.verticesPerArrow) || 2;
        if (Boolean(line.userData?.flowAnim?.hasArrowheads)) hasArrowheads = true;
        if ((Number(line.userData?.flowAnim?.trailCount) || 0) > 0) hasTrails = true;
        const cycleRate = Number(line.userData?.flowAnim?.cycleRate);
        if (Number.isFinite(cycleRate) && cycleRate > 1e-9 && cycleSeconds == null) {
          cycleSeconds = 1 / cycleRate;
        }
        if (pos?.count) {
          for (let i = 0; i < pos.count; i += vertsPerArrow) {
            const x = pos.getX(i);
            const y = pos.getY(i);
            const z = pos.getZ(i);
            const r = Math.hypot(x, y, z);
            if (r > maxRadius) maxRadius = r;
            uniqueX.add(Math.round(x * 1000));
            count += 1;
          }
        }
        const opacity = Number(line.material?.opacity);
        if (Number.isFinite(opacity)) lineOpacities.push(opacity);
      });
      lineOpacities.sort((a, b) => a - b);
      return {
        count,
        maxRadius,
        lineOpacities,
        xUniqueCount: uniqueX.size,
        source: flowFieldGroup?.userData?.flowSource || null,
        vortexCount: Number(flowFieldGroup?.userData?.flowVortexCount) || 0,
        mode: flowFieldGroup?.userData?.flowMode || null,
        deltaOnly: Boolean(flowFieldGroup?.userData?.flowDeltaOnly),
        attachedToAircraft: Boolean(aircraft && flowFieldGroup?.parent === aircraft),
        includesRigidBodyRotation: Boolean(flowFieldGroup?.userData?.flowIncludesRigidBodyRotation),
        hasAlphaAttr,
        hasArrowheads,
        hasTrails,
        cycleSeconds,
      };
    },
    getFlowAnimationProbe() {
      const flowLines = Array.isArray(flowFieldGroup?.children)
        ? flowFieldGroup.children.filter((obj) => obj?.isLineSegments && String(obj.name || '').startsWith('flow-vectors'))
        : [];
      const line = flowLines.find((obj) => obj?.geometry?.getAttribute?.('position')?.count >= 2);
      if (line) {
        const pos = line.geometry.getAttribute('position');
        return {
          sx: pos.getX(0),
          sy: pos.getY(0),
          sz: pos.getZ(0),
          ex: pos.getX(1),
          ey: pos.getY(1),
          ez: pos.getZ(1),
        };
      }
      const tracer = flowFieldGroup?.getObjectByName?.('flow-tracer');
      const cfg = tracer?.userData?.flowTracer;
      if (!tracer || !cfg) return null;
      const vx = Number(cfg.velocity?.x) || 0;
      const vy = Number(cfg.velocity?.y) || 0;
      const vz = Number(cfg.velocity?.z) || 0;
      return {
        sx: Number(tracer.position?.x) || 0,
        sy: Number(tracer.position?.y) || 0,
        sz: Number(tracer.position?.z) || 0,
        ex: (Number(tracer.position?.x) || 0) + vx,
        ey: (Number(tracer.position?.y) || 0) + vy,
        ez: (Number(tracer.position?.z) || 0) + vz,
        isTracer: true,
      };
    },
    getFlowTracerProbe() {
      const tracer = flowFieldGroup?.getObjectByName?.('flow-tracer');
      const cfg = tracer?.userData?.flowTracer;
      if (!tracer || !cfg) return null;
      return {
        x: Number(tracer.position?.x) || 0,
        y: Number(tracer.position?.y) || 0,
        z: Number(tracer.position?.z) || 0,
        startX: Number(cfg.start?.x) || 0,
        startY: Number(cfg.start?.y) || 0,
        startZ: Number(cfg.start?.z) || 0,
        vx: Number(cfg.velocity?.x) || 0,
        vy: Number(cfg.velocity?.y) || 0,
        vz: Number(cfg.velocity?.z) || 0,
        trailStepSeconds: Number(cfg.trailSampleSec) || null,
        trailDurationSeconds: Number(cfg.trailDurationSec) || null,
        trailStoredPoints: Math.max(0, Math.floor(Number(cfg.trailCount) || 0)),
      };
    },
    getFlowTrailProbe() {
      const flowLines = Array.isArray(flowFieldGroup?.children)
        ? flowFieldGroup.children.filter((obj) => obj?.isLineSegments && String(obj.name || '').startsWith('flow-vectors'))
        : [];
      const line = flowLines.find((obj) => obj?.geometry?.getAttribute?.('position')?.count >= 2);
      if (!line) {
        const tracer = flowFieldGroup?.getObjectByName?.('flow-tracer');
        const cfg = tracer?.userData?.flowTracer;
        const trailLine = cfg?.trailLine;
        const pos = trailLine?.geometry?.getAttribute?.('position');
        const alpha = trailLine?.geometry?.getAttribute?.('alpha');
        if (tracer && cfg && pos && alpha) {
          const trails = [];
          const segCount = Math.floor(pos.count / 2);
          for (let t = 0; t < segCount; t += 1) {
            const base = t * 2;
            trails.push({
              sx: pos.getX(base),
              sy: pos.getY(base),
              sz: pos.getZ(base),
              ex: pos.getX(base + 1),
              ey: pos.getY(base + 1),
              ez: pos.getZ(base + 1),
              alpha0: alpha.getX(base),
              alpha1: alpha.getX(base + 1),
            });
          }
          let cloudVisibleSegments = 0;
          let cloudTotalSegments = 0;
          const cloudAlpha = cfg.cloudTrailLine?.geometry?.getAttribute?.('alpha');
          if (cloudAlpha?.count) {
            cloudTotalSegments = Math.floor(cloudAlpha.count / 2);
            for (let i = 0; i < cloudTotalSegments; i += 1) {
              const a0 = Number(cloudAlpha.getX(i * 2) || 0);
              const a1 = Number(cloudAlpha.getX((i * 2) + 1) || 0);
              if (a0 > 0.02 || a1 > 0.02) cloudVisibleSegments += 1;
            }
          }
          return {
            trailCount: Number(cfg.trailPointCapacity) || 0,
            cycleSeconds: null,
            trailStepSeconds: Number(cfg.trailSampleSec) || null,
            trailDurationSeconds: Number(cfg.trailDurationSec) || null,
            trailStoredPoints: Math.max(0, Math.floor(Number(cfg.trailCount) || 0)),
            trails,
            cloudVisibleSegments,
            cloudTotalSegments,
            isTracer: true,
          };
        }
        return null;
      }
      const anim = line.userData?.flowAnim || {};
      const trailCount = Math.max(0, Math.floor(Number(anim.trailCount) || 0));
      const vertsPerArrow = Math.max(2, Number(anim.verticesPerArrow) || 2);
      const pos = line.geometry?.getAttribute?.('position');
      const alpha = line.geometry?.getAttribute?.('alpha');
      if (!pos || !alpha || pos.count < vertsPerArrow || alpha.count < vertsPerArrow) return null;
      const trails = [];
      for (let t = 0; t < trailCount; t += 1) {
        const base = (t + 1) * 2;
        if (base + 1 >= vertsPerArrow) break;
        trails.push({
          sx: pos.getX(base),
          sy: pos.getY(base),
          sz: pos.getZ(base),
          ex: pos.getX(base + 1),
          ey: pos.getY(base + 1),
          ez: pos.getZ(base + 1),
          alpha0: alpha.getX(base),
          alpha1: alpha.getX(base + 1),
        });
      }
      return {
        trailCount,
        cycleSeconds: Number.isFinite(Number(anim.cycleRate)) && Number(anim.cycleRate) > 1e-9
          ? 1 / Number(anim.cycleRate)
          : null,
        trailStepSeconds: Number(anim.trailStepSeconds) || null,
        trails,
      };
    },
    getNearestFlowArrowTo(x = 0, y = 0, z = 0) {
      const tx = Number(x);
      const ty = Number(y);
      const tz = Number(z);
      if (!Number.isFinite(tx) || !Number.isFinite(ty) || !Number.isFinite(tz)) return null;
      const flowLines = Array.isArray(flowFieldGroup?.children)
        ? flowFieldGroup.children.filter((obj) => obj?.isLineSegments && String(obj.name || '').startsWith('flow-vectors'))
        : [];
      let best = null;
      flowLines.forEach((line) => {
        const pos = line.geometry?.getAttribute?.('position');
        if (!pos?.count) return;
        const vertsPerArrow = Number(line.userData?.flowAnim?.verticesPerArrow) || 2;
        for (let i = 0; i + 1 < pos.count; i += vertsPerArrow) {
          const sx = pos.getX(i);
          const sy = pos.getY(i);
          const sz = pos.getZ(i);
          const ex = pos.getX(i + 1);
          const ey = pos.getY(i + 1);
          const ez = pos.getZ(i + 1);
          const d2 = ((sx - tx) ** 2) + ((sy - ty) ** 2) + ((sz - tz) ** 2);
          if (!best || d2 < best.d2) {
            best = { d2, sx, sy, sz, ex, ey, ez };
          }
        }
      });
      if (best) return best;
      const tracer = flowFieldGroup?.getObjectByName?.('flow-tracer');
      const cfg = tracer?.userData?.flowTracer;
      if (tracer && cfg) {
        const sx = Number(tracer.position?.x) || 0;
        const sy = Number(tracer.position?.y) || 0;
        const sz = Number(tracer.position?.z) || 0;
        return {
          d2: ((sx - tx) ** 2) + ((sy - ty) ** 2) + ((sz - tz) ** 2),
          sx,
          sy,
          sz,
          ex: sx + (Number(cfg.velocity?.x) || 0),
          ey: sy + (Number(cfg.velocity?.y) || 0),
          ez: sz + (Number(cfg.velocity?.z) || 0),
          isTracer: true,
        };
      }
      return best;
    },
    getOverlayAlignmentStats() {
      const flowLines = Array.isArray(flowFieldGroup?.children)
        ? flowFieldGroup.children.filter((obj) => obj?.isLineSegments && String(obj.name || '').startsWith('flow-vectors'))
        : [];
      let fx = 0;
      let fy = 0;
      let fz = 0;
      let fn = 0;
      flowLines.forEach((line) => {
        const pos = line.geometry?.getAttribute?.('position');
        if (!pos?.count) return;
        const vertsPerArrow = Number(line.userData?.flowAnim?.verticesPerArrow) || 2;
        for (let i = 0; i < pos.count; i += vertsPerArrow) {
          fx += pos.getX(i);
          fy += pos.getY(i);
          fz += pos.getZ(i);
          fn += 1;
        }
      });
      if (fn <= 0) {
        const tracer = flowFieldGroup?.getObjectByName?.('flow-tracer');
        if (tracer?.userData?.flowTracer) {
          fx = Number(tracer.position?.x) || 0;
          fy = Number(tracer.position?.y) || 0;
          fz = Number(tracer.position?.z) || 0;
          fn = 1;
        }
      }
      if (fn <= 0) return null;
      const flowCenter = { x: fx / fn, y: fy / fn, z: fz / fn };

      let vx = 0;
      let vy = 0;
      let vz = 0;
      let vn = 0;
      const boundLines = Array.isArray(vortexGroup?.children)
        ? vortexGroup.children.filter((obj) => String(obj?.name || '') === 'bound-vortices')
        : [];
      const tmp = (THREE && typeof THREE.Vector3 === 'function') ? new THREE.Vector3() : null;
      boundLines.forEach((line) => {
        const pos = line.geometry?.getAttribute?.('position');
        if (!pos?.count) return;
        line.updateMatrixWorld?.(true);
        for (let i = 0; i < pos.count; i += 1) {
          let x = pos.getX(i);
          let y = pos.getY(i);
          let z = pos.getZ(i);
          if (tmp && line.matrixWorld) {
            tmp.set(x, y, z).applyMatrix4(line.matrixWorld);
            x = tmp.x;
            y = tmp.y;
            z = tmp.z;
          }
          vx += x;
          vy += y;
          vz += z;
          vn += 1;
        }
      });
      if (vn <= 0) return { flowCenter, vortexCenter: null, dx: null, dy: null, dz: null };
      const vortexCenter = { x: vx / vn, y: vy / vn, z: vz / vn };
      return {
        flowCenter,
        vortexCenter,
        dx: flowCenter.x - vortexCenter.x,
        dy: flowCenter.y - vortexCenter.y,
        dz: flowCenter.z - vortexCenter.z,
      };
    },
    getReferenceMarkerPosition() {
      if (!aircraft) return null;
      const marker = aircraft.getObjectByName?.('reference-marker');
      if (!marker) return null;
      marker.updateMatrixWorld?.(true);
      if (marker.getWorldPosition) {
        const p = marker.getWorldPosition(new THREE.Vector3());
        return { x: p.x, y: p.y, z: p.z };
      }
      return {
        x: Number(marker.position?.x) || 0,
        y: Number(marker.position?.y) || 0,
        z: Number(marker.position?.z) || 0,
      };
    },
    sampleInducedAtWorld(x = 0, y = 0, z = 0) {
      const px = Number(x);
      const py = Number(y);
      const pz = Number(z);
      if (!Number.isFinite(px) || !Number.isFinite(py) || !Number.isFinite(pz)) return null;
      const flowOffset = {
        x: Number(aircraft?.position?.x) || 0,
        y: Number(aircraft?.position?.y) || 0,
        z: Number(aircraft?.position?.z) || 0,
      };
      const flowData = buildFlowVortexDataFromExec(uiState.lastExecResult, [], flowOffset);
      const horseshoes = Array.isArray(flowData?.horseshoes) ? flowData.horseshoes : [];
      const solverParams = {
        beta: Math.abs(Number(flowData?.beta) || 0) > 1e-8 ? Number(flowData.beta) : 1,
        iysym: Number(flowData?.iysym) || 0,
        izsym: Number(flowData?.izsym) || 0,
        ysym: Number(flowData?.ysym) || 0,
        zsym: Number(flowData?.zsym) || 0,
      };
      const acc = [0, 0, 0];
      for (let i = 0; i < horseshoes.length; i += 1) {
        addInducedVelocityFromHorseshoe(px, py, pz, horseshoes[i], solverParams, acc);
      }
      return {
        u: acc[0],
        v: acc[1],
        w: acc[2],
        count: horseshoes.length,
        offset: flowOffset,
      };
    },
  };
}

function computeEigenModesFromExec(result) {
  if (result?.EIGEN?.modes?.length) {
    return result.EIGEN.modes.map((m) => ({
      name: m.name || 'Mode',
      re: Number(m.re) || 0,
      im: Number(m.im) || 0,
      vec: m.vec || { rx: 0, ry: 0, rz: 0, tx: 0, ty: 0, tz: 0 },
      eigenvector: m.eigenvector || null,
      stateOrder: m.stateOrder || null,
    }));
  }
  return [];
}

function drawEigenPlot() {
  const canvas = els.eigenPlot;
  if (!canvas?.getContext) return;
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
  ctx.fillStyle = getCssVarColor('--panel', '#0f1115');
  ctx.fillRect(0, 0, width, height);

  const pad = { l: 44, r: 18, t: 20, b: 30 };
  const x0 = pad.l;
  const y0 = pad.t;
  const pw = Math.max(1, width - pad.l - pad.r);
  const ph = Math.max(1, height - pad.t - pad.b);

  const modes = uiState.eigenModes || [];
  if (!modes.length) {
    uiState.eigenPoints = [];
    uiState.eigenViewport = null;
    ctx.fillStyle = '#8ea2b8';
    ctx.font = '12px Consolas, "Courier New", monospace';
    ctx.fillText('No eigenmodes available yet. Run trim/EXEC first.', x0 + 8, y0 + 18);
    return;
  }

  let maxRe = 0.2;
  let maxIm = 0.2;
  modes.forEach((m) => {
    maxRe = Math.max(maxRe, Math.abs(Number(m.re) || 0));
    maxIm = Math.max(maxIm, Math.abs(Number(m.im) || 0));
  });
  const zoom = Math.max(0.5, Math.min(8, Number(uiState.eigenZoom) || 1));
  uiState.eigenZoom = zoom;
  maxRe = Math.max(0.05, (maxRe * 1.2) / zoom);
  maxIm = Math.max(0.05, (maxIm * 1.2) / zoom);
  const centerRe = Number.isFinite(uiState.eigenCenterRe) ? Number(uiState.eigenCenterRe) : 0;
  const centerIm = Number.isFinite(uiState.eigenCenterIm) ? Number(uiState.eigenCenterIm) : 0;

  const xFor = (re) => x0 + ((re - centerRe + maxRe) / (2 * maxRe)) * pw;
  const yFor = (im) => y0 + (1 - ((im - centerIm + maxIm) / (2 * maxIm))) * ph;
  const yMid = yFor(0);
  const xMid = xFor(0);
  uiState.eigenViewport = {
    xMid, yMid, maxRe, maxIm, zoom, centerRe, centerIm, x0, y0, pw, ph,
  };

  ctx.strokeStyle = 'rgba(255,255,255,0.13)';
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(x0, yMid);
  ctx.lineTo(x0 + pw, yMid);
  ctx.moveTo(xMid, y0);
  ctx.lineTo(xMid, y0 + ph);
  ctx.stroke();

  ctx.fillStyle = '#8ea2b8';
  ctx.font = '11px Consolas, "Courier New", monospace';
  ctx.fillText('Re', x0 + pw - 18, yMid - 6);
  ctx.fillText('Im', xMid + 6, y0 + 12);

  const runColor = activeRunCaseColor();
  const activeCaseIdx = activeRunCaseIndex();
  const isModeSelectable = (mode) => {
    const caseIdx = Number.isInteger(mode?.runCaseIndex) ? Number(mode.runCaseIndex) : -1;
    return caseIdx < 0 || caseIdx === activeCaseIdx || activeCaseIdx < 0;
  };
  if (uiState.selectedEigenMode >= 0 && !isModeSelectable(modes[uiState.selectedEigenMode])) {
    uiState.selectedEigenMode = -1;
    stopModeAnimation();
  }
  uiState.eigenPoints = [];
  modes.forEach((m, idx) => {
    const re = Number(m.re) || 0;
    const im = Number(m.im) || 0;
    const px = xFor(re);
    const py = yFor(im);
    const modeRunCaseIdx = Number.isInteger(m?.runCaseIndex) ? Number(m.runCaseIndex) : -1;
    const modeSelectable = isModeSelectable(m);
    const isSelected = idx === uiState.selectedEigenMode && modeSelectable;
    const modeColor = (modeRunCaseIdx >= 0 && Array.isArray(uiState.runCases) && modeRunCaseIdx < uiState.runCases.length)
      ? caseColorOrFallback(uiState.runCases[modeRunCaseIdx], modeRunCaseIdx)
      : runColor;
    if (modeColor) {
      if (modeSelectable) {
        ctx.strokeStyle = isSelected ? '#ffffff' : modeColor;
        ctx.fillStyle = isSelected ? modeColor : hexToRgba(modeColor, 0.35);
      } else {
        ctx.strokeStyle = hexToRgba(modeColor, 0.45);
        ctx.fillStyle = hexToRgba(modeColor, 0.12);
      }
    } else {
      if (modeSelectable) {
        ctx.strokeStyle = isSelected ? '#f59e0b' : '#60a5fa';
        ctx.fillStyle = isSelected ? '#f59e0b' : '#93c5fd';
      } else {
        ctx.strokeStyle = 'rgba(96,165,250,0.45)';
        ctx.fillStyle = 'rgba(147,197,253,0.12)';
      }
    }
    ctx.lineWidth = isSelected ? 2 : 1.5;
    ctx.beginPath();
    ctx.arc(px, py, isSelected ? 5 : 4, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
    if (Math.abs(im) > 1e-8) {
      const py2 = yFor(-im);
      ctx.beginPath();
      ctx.arc(px, py2, isSelected ? 5 : 4, 0, Math.PI * 2);
      ctx.fill();
      ctx.stroke();
    }
    ctx.fillStyle = modeSelectable ? '#c7d2fe' : '#6b7b95';
    ctx.font = '10px Consolas, "Courier New", monospace';
    ctx.fillText(m.name, px + 8, py - 6);
    if (isSelected) {
      const reTxt = (Number(m.re) || 0).toFixed(3);
      const imVal = Number(m.im) || 0;
      const sign = imVal >= 0 ? '+' : '-';
      const imTxt = Math.abs(imVal).toFixed(3);
      const eigTxt = `${reTxt} ${sign} ${imTxt}i`;
      ctx.fillStyle = '#fcd34d';
      ctx.font = '10px Consolas, "Courier New", monospace';
      ctx.fillText(eigTxt, px + 8, py + 10);
    }
    uiState.eigenPoints.push({
      idx,
      x: px,
      y: py,
      color: modeColor || null,
      selectable: modeSelectable,
      runCaseIndex: modeRunCaseIdx,
      dimmed: !modeSelectable,
    });
  });
}

function stopModeAnimation() {
  if (!modeAnimation) return;
  const basePos = modeAnimation.basePos?.clone?.();
  const baseRot = modeAnimation.baseRot?.clone?.();
  modeAnimation = null;
  if (!aircraft) return;
  if (basePos) aircraft.position.copy(basePos);
  if (baseRot) aircraft.rotation.set(baseRot.x, baseRot.y, baseRot.z);
}

function startModeAnimation(modeIndex) {
  const mode = uiState.eigenModes?.[modeIndex];
  if (!mode) return;
  stopModeAnimation();
  uiState.selectedEigenMode = modeIndex;
  if (!aircraft) {
    drawEigenPlot();
    return;
  }
  const freq = Math.max(0.2, Math.abs(Number(mode.im) || 0));
  const damp = Math.max(0.01, Math.abs(Number(mode.re) || 0));
  const raw = mode.vec || { rx: 0, ry: 0, rz: 0, tx: 0, ty: 0, tz: 0 };
  const rotMag = Math.max(Math.abs(raw.rx || 0), Math.abs(raw.ry || 0), Math.abs(raw.rz || 0), 1e-8);
  const posMag = Math.max(Math.abs(raw.tx || 0), Math.abs(raw.ty || 0), Math.abs(raw.tz || 0), 1e-8);
  const vec = {
    rx: (raw.rx || 0) * (0.18 / rotMag),
    ry: (raw.ry || 0) * (0.18 / rotMag),
    rz: (raw.rz || 0) * (0.18 / rotMag),
    tx: (raw.tx || 0) * (0.25 / posMag),
    ty: (raw.ty || 0) * (0.25 / posMag),
    tz: (raw.tz || 0) * (0.25 / posMag),
  };
  modeAnimation = {
    started: performance.now(),
    basePos: aircraft.position.clone(),
    baseRot: aircraft.rotation.clone(),
    vec,
    w: 2 * Math.PI * freq,
    sigma: damp * 0.25,
  };
  drawEigenPlot();
}

function handleEigenCanvasClick(evt) {
  if (!els.eigenPlot) return;
  const pts = (uiState.eigenPoints || []).filter((pt) => pt?.selectable !== false);
  if (!pts.length) return;
  const rect = els.eigenPlot.getBoundingClientRect();
  const x = evt.clientX - rect.left;
  const y = evt.clientY - rect.top;
  let best = null;
  pts.forEach((pt) => {
    const dx = pt.x - x;
    const dy = pt.y - y;
    const d2 = dx * dx + dy * dy;
    if (!best || d2 < best.d2) best = { idx: pt.idx, d2 };
  });
  if (!best || best.d2 > 144) return;
  if (best.idx === uiState.selectedEigenMode) {
    uiState.selectedEigenMode = -1;
    stopModeAnimation();
    drawEigenPlot();
    return;
  }
  startModeAnimation(best.idx);
}

function handleEigenCanvasWheel(evt) {
  if (!els.eigenPlot) return;
  evt.preventDefault();
  const current = Math.max(0.5, Math.min(8, Number(uiState.eigenZoom) || 1));
  const factor = evt.deltaY < 0 ? 1.12 : (1 / 1.12);
  const next = Math.max(0.5, Math.min(8, current * factor));
  if (Math.abs(next - current) < 1e-4) return;
  zoomEigenAtClient(next, evt.clientX, evt.clientY);
}

function zoomEigenAtClient(nextZoom, clientX, clientY) {
  const vp = uiState.eigenViewport;
  const canvas = els.eigenPlot;
  if (!vp || !canvas) {
    uiState.eigenZoom = nextZoom;
    drawEigenPlot();
    return;
  }
  const rect = canvas.getBoundingClientRect();
  const relX = Number.isFinite(clientX) ? (clientX - rect.left) : (rect.width * 0.5);
  const relY = Number.isFinite(clientY) ? (clientY - rect.top) : (rect.height * 0.5);
  const x = Math.max(vp.x0, Math.min(vp.x0 + vp.pw, relX));
  const y = Math.max(vp.y0, Math.min(vp.y0 + vp.ph, relY));
  const nx = ((x - vp.x0) / Math.max(1, vp.pw)) * 2 - 1;
  const ny = (y - vp.y0) / Math.max(1, vp.ph);
  const oldZoom = Math.max(0.5, Math.min(8, Number(vp.zoom) || Number(uiState.eigenZoom) || 1));
  const oldMaxRe = Number(vp.maxRe) || 0.1;
  const oldMaxIm = Number(vp.maxIm) || 0.1;
  const centerRe = Number(vp.centerRe) || 0;
  const centerIm = Number(vp.centerIm) || 0;
  const reAt = centerRe + nx * oldMaxRe;
  const imAt = centerIm + (1 - 2 * ny) * oldMaxIm;
  const baseRe = oldMaxRe * oldZoom;
  const baseIm = oldMaxIm * oldZoom;
  const newMaxRe = Math.max(0.05, baseRe / nextZoom);
  const newMaxIm = Math.max(0.05, baseIm / nextZoom);
  uiState.eigenCenterRe = reAt - nx * newMaxRe;
  uiState.eigenCenterIm = imAt - (1 - 2 * ny) * newMaxIm;
  uiState.eigenZoom = nextZoom;
  drawEigenPlot();
}

function touchDistance(touches) {
  if (!touches || touches.length < 2) return 0;
  const a = touches[0];
  const b = touches[1];
  const dx = Number(a.clientX) - Number(b.clientX);
  const dy = Number(a.clientY) - Number(b.clientY);
  return Math.hypot(dx, dy);
}

function handleEigenTouchStart(evt) {
  if (!evt?.touches || evt.touches.length < 2) return;
  evt.preventDefault();
  const dist = touchDistance(evt.touches);
  if (!Number.isFinite(dist) || dist <= 0) return;
  eigenTouchZoom = { active: true, lastDist: dist };
}

function handleEigenTouchMove(evt) {
  if (!eigenTouchZoom.active || !evt?.touches || evt.touches.length < 2) return;
  evt.preventDefault();
  const dist = touchDistance(evt.touches);
  if (!Number.isFinite(dist) || dist <= 0 || !Number.isFinite(eigenTouchZoom.lastDist) || eigenTouchZoom.lastDist <= 0) return;
  const scale = dist / eigenTouchZoom.lastDist;
  if (!Number.isFinite(scale) || Math.abs(scale - 1) < 0.01) return;
  const current = Math.max(0.5, Math.min(8, Number(uiState.eigenZoom) || 1));
  const next = Math.max(0.5, Math.min(8, current * scale));
  eigenTouchZoom.lastDist = dist;
  if (Math.abs(next - current) < 1e-4) return;
  const t0 = evt.touches[0];
  const t1 = evt.touches[1];
  const midX = (Number(t0.clientX) + Number(t1.clientX)) * 0.5;
  const midY = (Number(t0.clientY) + Number(t1.clientY)) * 0.5;
  zoomEigenAtClient(next, midX, midY);
}

function handleEigenTouchEnd(evt) {
  if (evt?.touches?.length >= 2) {
    const dist = touchDistance(evt.touches);
    eigenTouchZoom.lastDist = Number.isFinite(dist) && dist > 0 ? dist : eigenTouchZoom.lastDist;
    return;
  }
  eigenTouchZoom = { active: false, lastDist: 0 };
}

function updateModeAnimation() {
  if (!modeAnimation || !aircraft) return;
  const t = (performance.now() - modeAnimation.started) / 1000;
  const amp = Math.exp(-modeAnimation.sigma * t);
  const osc = Math.sin(modeAnimation.w * t);
  const s = amp * osc;
  const v = modeAnimation.vec;
  aircraft.position.set(
    modeAnimation.basePos.x + (v.tx || 0) * s,
    modeAnimation.basePos.y + (v.ty || 0) * s,
    modeAnimation.basePos.z + (v.tz || 0) * s,
  );
  aircraft.rotation.set(
    modeAnimation.baseRot.x + (v.rx || 0) * s,
    modeAnimation.baseRot.y + (v.ry || 0) * s,
    modeAnimation.baseRot.z + (v.rz || 0) * s,
  );
  if (amp < 0.02) {
    stopModeAnimation();
  }
}

function resetFlowTracerPosition(group = flowFieldGroup) {
  const tracer = group?.getObjectByName?.('flow-tracer');
  const cfg = tracer?.userData?.flowTracer;
  if (!tracer || !cfg?.start) return;
  tracer.position.set(
    Number(cfg.start.x) || 0,
    Number(cfg.start.y) || 0,
    Number(cfg.start.z) || 0,
  );
  if (!cfg.velocity || typeof cfg.velocity !== 'object') cfg.velocity = { x: 0, y: 0, z: 0 };
  cfg.velocity.x = 0;
  cfg.velocity.y = 0;
  cfg.velocity.z = 0;
  cfg.lastTimeSec = null;
  cfg.trailHead = -1;
  cfg.trailCount = 0;
  cfg.trailNextSampleSec = null;
  const cloud = cfg.cloudPoints;
  const cloudStart = cfg.cloudStartPositions;
  const cloudPosAttr = cloud?.geometry?.getAttribute?.('position');
  const cloudCount = Math.max(0, Math.floor(Number(cfg.cloudCount) || 0));
  if (cloudPosAttr?.array && cloudStart?.length) {
    const arr = cloudPosAttr.array;
    const n = Math.min(arr.length, cloudStart.length);
    for (let i = 0; i < n; i += 1) arr[i] = cloudStart[i];
    cloudPosAttr.needsUpdate = true;
  }
  if (cfg.cloudTrailHeads && cfg.cloudTrailCounts) {
    for (let i = 0; i < cfg.cloudTrailHeads.length; i += 1) cfg.cloudTrailHeads[i] = -1;
    cfg.cloudTrailCounts.fill(0);
  }
  const cloudNext = cfg.cloudTrailNextSampleSec;
  const cloudPhase = cfg.cloudTrailSamplePhase;
  if (cloudNext && cloudNext.length) {
    for (let i = 0; i < cloudNext.length; i += 1) {
      const phase = Number(cloudPhase?.[i]);
      cloudNext[i] = Number.isFinite(phase) ? phase : 0;
    }
  }
  if (cloudPosAttr?.array && cloudCount > 0) {
    const arr = cloudPosAttr.array;
    for (let i = 0; i < cloudCount; i += 1) {
      const off = i * 3;
      pushCloudTrailSampleForIndex(cfg, i, arr[off + 0], arr[off + 1], arr[off + 2]);
    }
  }
  pushFlowTracerTrailSample(cfg, tracer.position.x, tracer.position.y, tracer.position.z);
  writeFlowTracerTrailGeometry(tracer, cfg);
  writeCloudTrailGeometry(cfg);
}

function updateFlowFieldAnimation() {
  if (!flowFieldGroup) return;
  const tracer = flowFieldGroup.getObjectByName?.('flow-tracer');
  const cfg = tracer?.userData?.flowTracer;
  if (!tracer || !cfg) return;
  if (!uiState.showFlowField || !flowFieldGroup.visible) {
    resetFlowTracerPosition(flowFieldGroup);
    return;
  }
  const nowMs = performance.now();
  if ((nowMs - flowFieldAnimLastMs) < 16) return;
  const nowSec = nowMs * 0.001;
  const prevSec = Number(cfg.lastTimeSec);
  cfg.lastTimeSec = nowSec;
  flowFieldAnimLastMs = nowMs;
  const sampleSec = Math.max(1e-6, Number(cfg.trailSampleSec) || 1);
  if (!Number.isFinite(cfg.trailNextSampleSec)) {
    cfg.trailNextSampleSec = nowSec + sampleSec;
  }
  if (!Number.isFinite(prevSec)) {
    writeFlowTracerTrailGeometry(tracer, cfg);
    return;
  }
  let dt = nowSec - prevSec;
  if (!Number.isFinite(dt) || dt <= 0) return;
  dt = Math.min(0.05, dt);
  const advDt = dt * Math.max(0.01, Number(cfg.speedScale) || 1);
  const pos = tracer.position;
  const tracerVel = cfg.tmpTracerVel || (cfg.tmpTracerVel = [0, 0, 0]);
  sampleFlowVelocityAtPoint(pos.x, pos.y, pos.z, cfg, tracerVel);
  if (!cfg.velocity || typeof cfg.velocity !== 'object') cfg.velocity = { x: 0, y: 0, z: 0 };
  cfg.velocity.x = tracerVel[0];
  cfg.velocity.y = tracerVel[1];
  cfg.velocity.z = tracerVel[2];
  pos.x += cfg.velocity.x * advDt;
  pos.y += cfg.velocity.y * advDt;
  pos.z += cfg.velocity.z * advDt;
  const cloud = cfg.cloudPoints;
  const cloudPosAttr = cloud?.geometry?.getAttribute?.('position');
  const cloudArr = cloudPosAttr?.array;
  const cloudCount = Math.max(0, Math.floor(Number(cfg.cloudCount) || 0));
  if (cloudArr && cloudCount > 0) {
    const span = Math.max(1e-3, Number(cfg.spanLength) || 1);
    const cloudVel = cfg.tmpCloudVel || (cfg.tmpCloudVel = [0, 0, 0]);
    for (let i = 0; i < cloudCount; i += 1) {
      const off = i * 3;
      const cx = cloudArr[off + 0];
      const cy = cloudArr[off + 1];
      const cz = cloudArr[off + 2];
      sampleFlowVelocityAtPoint(cx, cy, cz, cfg, cloudVel);
      let nx = cx + (cloudVel[0] * advDt);
      let ny = cy + (cloudVel[1] * advDt);
      let nz = cz + (cloudVel[2] * advDt);
      const rr = Math.hypot(nx, ny, nz);
      if (rr > (span * 1.15)) {
        const start = cfg.cloudStartPositions;
        nx = Number(start?.[off + 0]) || 0;
        ny = Number(start?.[off + 1]) || 0;
        nz = Number(start?.[off + 2]) || 0;
        resetCloudTrailForIndex(cfg, i, nx, ny, nz);
      }
      cloudArr[off + 0] = nx;
      cloudArr[off + 1] = ny;
      cloudArr[off + 2] = nz;
    }
    cloudPosAttr.needsUpdate = true;
  }
  let nextSample = Number(cfg.trailNextSampleSec);
  if (!Number.isFinite(nextSample)) nextSample = nowSec + sampleSec;
  while (nowSec >= nextSample) {
    pushFlowTracerTrailSample(cfg, pos.x, pos.y, pos.z);
    nextSample += sampleSec;
  }
  cfg.trailNextSampleSec = nextSample;
  if (cloudArr && cloudCount > 0) {
    const cloudNext = cfg.cloudTrailNextSampleSec;
    const cloudPhase = cfg.cloudTrailSamplePhase;
    if (cloudNext && cloudNext.length >= cloudCount) {
      for (let i = 0; i < cloudCount; i += 1) {
        let cloudTs = Number(cloudNext[i]);
        if (!Number.isFinite(cloudTs) || cloudTs < 1e-9 || cloudTs < prevSec - 5) {
          const phase = Number(cloudPhase?.[i]) || 0;
          cloudTs = nowSec + phase;
        }
        while (nowSec >= cloudTs) {
          const off = i * 3;
          pushCloudTrailSampleForIndex(cfg, i, cloudArr[off + 0], cloudArr[off + 1], cloudArr[off + 2]);
          cloudTs += sampleSec;
        }
        cloudNext[i] = cloudTs;
      }
    } else {
      for (let i = 0; i < cloudCount; i += 1) {
        const off = i * 3;
        pushCloudTrailSampleForIndex(cfg, i, cloudArr[off + 0], cloudArr[off + 1], cloudArr[off + 2]);
      }
    }
  }
  writeFlowTracerTrailGeometry(tracer, cfg);
  writeCloudTrailGeometry(cfg);
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

function buildNacaProfileCoords(code, samples = 40) {
  const digits = String(code || '').padStart(4, '0');
  const thickness = Number(digits.slice(2, 4)) / 100;
  const m = Number(digits[0]) / 100;
  const p = Number(digits[1]) / 10;
  const xVals = [];
  for (let i = 0; i <= samples; i += 1) xVals.push(i / samples);
  const upper = [];
  const lower = [];
  xVals.forEach((x) => {
    const yt = 5 * thickness * (
      0.2969 * Math.sqrt(Math.max(x, 0))
      - 0.1260 * x
      - 0.3516 * x * x
      + 0.2843 * x * x * x
      - 0.1015 * x * x * x * x
    );
    let yc = 0;
    let dyc = 0;
    if (m > 0 && p > 0 && p < 1) {
      if (x < p) {
        yc = (m / (p * p)) * (2 * p * x - x * x);
        dyc = (2 * m / (p * p)) * (p - x);
      } else {
        yc = (m / ((1 - p) * (1 - p))) * ((1 - 2 * p) + 2 * p * x - x * x);
        dyc = (2 * m / ((1 - p) * (1 - p))) * (p - x);
      }
    }
    const theta = Math.atan(dyc);
    const xu = x - yt * Math.sin(theta);
    const zu = yc + yt * Math.cos(theta);
    const xl = x + yt * Math.sin(theta);
    const zl = yc - yt * Math.cos(theta);
    upper.push([xu, zu]);
    lower.push([xl, zl]);
  });
  return [...upper.slice().reverse(), ...lower.slice(1)];
}

function buildProfileSections(coords, samples = 26) {
  const safe = Array.isArray(coords) ? coords : [];
  if (safe.length < 2) {
    const xs = Array.from({ length: samples }, (_, i) => i / (samples - 1));
    return { upper: xs.map((x) => [x, 0]), lower: xs.map((x) => [x, 0]) };
  }
  const { upper, lower } = splitAirfoilSurfaces(safe);
  const xs = Array.from({ length: samples }, (_, i) => i / (samples - 1));
  const up = xs.map((x) => [x, interpY(upper, x)]);
  const lo = xs.map((x) => [x, interpY(lower, x)]);
  return { upper: up, lower: lo };
}

function pressureColorAlpha(cp, cmin, cmax) {
  const tRaw = (cp - cmin) / ((cmax - cmin) || 1);
  const t = Math.max(0, Math.min(1, tRaw));
  const blue = new THREE.Color(0x2f7bff);
  const red = new THREE.Color(0xef4444);
  const col = t <= 0.5
    ? blue.clone()
    : blue.clone().lerp(red, (t - 0.5) / 0.5);
  const alpha = t <= 0.35 ? (t / 0.35) * 0.9 : 0.9;
  return { color: col, alpha: Math.max(0, Math.min(0.95, alpha)) };
}

function enableVertexAlpha(material) {
  if (!material || material.userData?.vertexAlphaEnabled) return;
  material.userData = material.userData || {};
  material.userData.vertexAlphaEnabled = true;
  material.onBeforeCompile = (shader) => {
    shader.vertexShader = shader.vertexShader
      .replace('#include <common>', '#include <common>\nattribute float alpha;\nvarying float vAlpha;')
      .replace('#include <begin_vertex>', '#include <begin_vertex>\nvAlpha = alpha;');
    shader.fragmentShader = shader.fragmentShader
      .replace('#include <common>', '#include <common>\nvarying float vAlpha;')
      .replace('vec4 diffuseColor = vec4( diffuse, opacity );', 'vec4 diffuseColor = vec4( diffuse, opacity * vAlpha );');
  };
  material.customProgramCacheKey = () => 'vertex-alpha-v1';
  material.needsUpdate = true;
}

function buildPressureFieldFromExec(result) {
  const rv = result?.RV;
  const dcp = result?.DCP;
  if (!Array.isArray(rv) || !Array.isArray(dcp) || dcp.length <= 1) return null;
  const idx2 = (i, j, dim1) => i + dim1 * j;
  const samples = [];
  let cmin = Infinity;
  let cmax = -Infinity;
  for (let iv = 1; iv < dcp.length; iv += 1) {
    const cp = Number(dcp[iv]);
    if (!Number.isFinite(cp)) continue;
    const x = Number(rv[idx2(1, iv, 4)]);
    const y = Number(rv[idx2(2, iv, 4)]);
    const z = Number(rv[idx2(3, iv, 4)]);
    if (!Number.isFinite(x) || !Number.isFinite(y) || !Number.isFinite(z)) continue;
    samples.push({ x, y, z, cp });
    if (cp < cmin) cmin = cp;
    if (cp > cmax) cmax = cp;
  }
  if (!samples.length) return null;
  let ymin = samples[0].y;
  let ymax = samples[0].y;
  let zmin = samples[0].z;
  let zmax = samples[0].z;
  samples.forEach((s) => {
    if (s.y < ymin) ymin = s.y;
    if (s.y > ymax) ymax = s.y;
    if (s.z < zmin) zmin = s.z;
    if (s.z > zmax) zmax = s.z;
  });
  const yspan = Math.max(1e-6, ymax - ymin);
  const zspan = Math.max(1e-6, zmax - zmin);
  const cell = Math.max(0.03, (yspan + zspan) / 90);
  const bins = new Map();
  samples.forEach((s, idx) => {
    const iy = Math.floor((s.y - ymin) / cell);
    const iz = Math.floor((s.z - zmin) / cell);
    const key = `${iy},${iz}`;
    if (!bins.has(key)) bins.set(key, []);
    bins.get(key).push(idx);
  });
  return { samples, bins, cell, ymin, zmin, cmin, cmax };
}

function queryPressureAtPoint(field, x, y, z) {
  if (!field?.samples?.length) return null;
  const { bins, cell, ymin, zmin, samples } = field;
  const iy = Math.floor((y - ymin) / cell);
  const iz = Math.floor((z - zmin) / cell);
  let candidateIdx = [];
  for (let dy = -1; dy <= 1; dy += 1) {
    for (let dz = -1; dz <= 1; dz += 1) {
      const key = `${iy + dy},${iz + dz}`;
      if (bins.has(key)) candidateIdx = candidateIdx.concat(bins.get(key));
    }
  }
  if (!candidateIdx.length) {
    candidateIdx = Array.from({ length: samples.length }, (_, i) => i);
  }
  let best = null;
  let bestD2 = Infinity;
  candidateIdx.forEach((idx) => {
    const s = samples[idx];
    const dx = x - s.x;
    const dy = y - s.y;
    const dz = z - s.z;
    const d2 = dx * dx + dy * dy + dz * dz;
    if (d2 < bestD2) {
      bestD2 = d2;
      best = s;
    }
  });
  return best;
}

function updatePressureSurfaceColors() {
  if (!Array.isArray(surfaceSkinMeshes) || !surfaceSkinMeshes.length) return;
  const field = uiState.showPressure ? uiState.pressureField : null;
  surfaceSkinMeshes.forEach((mesh) => {
    const base = mesh.userData?.baseColor instanceof THREE.Color ? mesh.userData.baseColor : new THREE.Color(mesh.userData?.baseColor ?? 0x7dd3fc);
    const pressureBase = mesh.userData?.pressureBaseColor instanceof THREE.Color ? mesh.userData.pressureBaseColor : new THREE.Color(0xd8dce3);
    if (mesh.material?.color) {
      mesh.material.color.copy(field ? pressureBase : base);
      mesh.material.needsUpdate = true;
    }
  });

  if (!Array.isArray(surfacePressureMeshes) || !surfacePressureMeshes.length) return;
  surfacePressureMeshes.forEach((mesh) => {
    const geom = mesh.geometry;
    const pos = geom?.getAttribute?.('position');
    const colAttr = geom?.getAttribute?.('color');
    const alphaAttr = geom?.getAttribute?.('alpha');
    if (!pos || !colAttr || !alphaAttr) return;
    for (let i = 0; i < pos.count; i += 1) {
      if (!field) {
        colAttr.setXYZ(i, 0, 0, 0);
        alphaAttr.setX(i, 0);
        continue;
      }
      const x = pos.getX(i);
      const y = pos.getY(i);
      const z = pos.getZ(i);
      const sample = queryPressureAtPoint(field, x, y, z);
      if (!sample) {
        colAttr.setXYZ(i, 0, 0, 0);
        alphaAttr.setX(i, 0);
        continue;
      }
      const mapped = pressureColorAlpha(sample.cp, field.cmin, field.cmax);
      colAttr.setXYZ(i, mapped.color.r, mapped.color.g, mapped.color.b);
      alphaAttr.setX(i, mapped.alpha);
    }
    colAttr.needsUpdate = true;
    alphaAttr.needsUpdate = true;
  });
}

function applySurfaceRenderMode() {
  const mode = uiState.surfaceRenderMode;
  const showSurface = mode === 'surface' || mode === 'both';
  const showWire = mode === 'wireframe' || mode === 'both';
  surfaceSkinMeshes.forEach((mesh) => { mesh.visible = showSurface; });
  const showPressureOverlay = showSurface && uiState.showPressure && Boolean(uiState.pressureField);
  surfacePressureMeshes.forEach((mesh) => { mesh.visible = showPressureOverlay; });
  surfaceWireObjects.forEach((obj) => { obj.visible = showWire; });
  updatePressureSurfaceColors();
}

function createControlDeflectionMap() {
  const map = new Map();
  const delcon = uiState.lastExecResult?.DELCON;
  const controlMap = uiState.modelCache?.controlMap;
  if (!delcon || !controlMap?.size) return map;
  for (const [name, idx] of controlMap.entries()) {
    const val = Number(delcon[idx]);
    map.set(name, Number.isFinite(val) ? val : 0);
  }
  return map;
}

function createSectionPointTransform(surface, controlDeflections = new Map()) {
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

  return (section, rawPoint) => {
    const flap = applyControlDeflections(section, rawPoint);
    const rotated = rotateSectionPoint(section, flap);
    return applyTransforms(rotated);
  };
}

function pushLineSegment(verts, a, b) {
  verts.push(a[0], a[1], a[2], b[0], b[1], b[2]);
}

function resolveSectionSpanPanels(surface, sectionA, sectionB) {
  const candidates = [
    Number(sectionB?.nSpan),
    Number(sectionA?.nSpan),
    Number(surface?.nSpan),
  ];
  for (let i = 0; i < candidates.length; i += 1) {
    const val = candidates[i];
    if (Number.isFinite(val) && val > 0) return Math.max(1, Math.round(val));
  }
  return 1;
}

function buildPanelSpacingGroup(model) {
  if (!THREE || !model?.surfaces?.length) return null;
  const verts = [];
  const controlDeflections = createControlDeflectionMap();
  model.surfaces.forEach((surface) => {
    if (!surface?.sections?.length || surface.sections.length < 2) return;
    const transform = createSectionPointTransform(surface, controlDeflections);
    for (let s = 0; s < surface.sections.length - 1; s += 1) {
      const a = surface.sections[s];
      const b = surface.sections[s + 1];
      const nChord = Math.max(1, Math.round(Number(surface.nChord) || 1));
      const nSpan = resolveSectionSpanPanels(surface, a, b);
      const pointAt = (u, c) => {
        const xA = a.xle + a.chord * c;
        const xB = b.xle + b.chord * c;
        const zA = a.zle;
        const zB = b.zle;
        const raw = [
          xA + (xB - xA) * u,
          a.yle + (b.yle - a.yle) * u,
          zA + (zB - zA) * u,
        ];
        return transform(a, raw);
      };
      for (let j = 0; j <= nSpan; j += 1) {
        const u = j / nSpan;
        pushLineSegment(verts, pointAt(u, 0), pointAt(u, 1));
      }
      for (let i = 0; i <= nChord; i += 1) {
        const c = i / nChord;
        pushLineSegment(verts, pointAt(0, c), pointAt(1, c));
      }
    }
  });
  if (!verts.length) return null;
  const group = new THREE.Group();
  group.name = 'panel-spacing';
  const geom = new THREE.BufferGeometry();
  geom.setAttribute('position', new THREE.Float32BufferAttribute(verts, 3));
  const mat = new THREE.LineBasicMaterial({ color: 0x38bdf8, transparent: true, opacity: 0.55 });
  const lines = new THREE.LineSegments(geom, mat);
  lines.name = 'panel-spacing-lines';
  group.add(lines);
  return group;
}

function buildVortexData(model) {
  if (!model?.surfaces?.length) return null;
  const boundVerts = [];
  const legVerts = [];
  const segments = [];
  const controlDeflections = createControlDeflectionMap();
  const cref = Math.max(0.4, Number(model.header?.cref) || 1);
  const bref = Math.max(0.8, Number(model.header?.bref) || 2);
  const legLength = Math.max(1.8 * cref, 0.35 * bref);
  // AVL's horseshoe trailing legs are aligned with body +X.
  const wakeDir = { x: 1, y: 0, z: 0 };
  const sampleTarget = 220;
  model.surfaces.forEach((surface) => {
    if (!surface?.sections?.length || surface.sections.length < 2) return;
    const transform = createSectionPointTransform(surface, controlDeflections);
    for (let s = 0; s < surface.sections.length - 1; s += 1) {
      const a = surface.sections[s];
      const b = surface.sections[s + 1];
      const nChord = Math.max(1, Math.round(Number(surface.nChord) || 1));
      const nSpan = resolveSectionSpanPanels(surface, a, b);
      const sampleStride = Math.max(1, Math.ceil((nChord * nSpan) / sampleTarget));
      const pointAt = (u, c) => {
        const xA = a.xle + a.chord * c;
        const xB = b.xle + b.chord * c;
        const zA = a.zle;
        const zB = b.zle;
        const raw = [
          xA + (xB - xA) * u,
          a.yle + (b.yle - a.yle) * u,
          zA + (zB - zA) * u,
        ];
        return transform(a, raw);
      };
      for (let j = 0; j < nSpan; j += 1) {
        const u0 = j / nSpan;
        const u1 = (j + 1) / nSpan;
        for (let i = 0; i < nChord; i += 1) {
          const atTipBoundary = j === 0 || j === (nSpan - 1);
          if (!atTipBoundary && ((j * nChord) + i) % sampleStride !== 0) continue;
          const cBound = (i + 0.25) / nChord;
          const p0 = pointAt(u0, cBound);
          const p1 = pointAt(u1, cBound);
          const leg0 = [p0[0] + wakeDir.x * legLength, p0[1] + wakeDir.y * legLength, p0[2] + wakeDir.z * legLength];
          const leg1 = [p1[0] + wakeDir.x * legLength, p1[1] + wakeDir.y * legLength, p1[2] + wakeDir.z * legLength];
          const gamma = 1 / Math.max(1, nChord);
          pushLineSegment(boundVerts, p0, p1);
          pushLineSegment(legVerts, p0, leg0);
          pushLineSegment(legVerts, p1, leg1);
          segments.push({ a: p0, b: p1, gamma, kind: 'bound', tip: atTipBoundary });
          // Signed trailing legs so interior legs tend to cancel while tip-vortex legs remain.
          segments.push({ a: p1, b: leg1, gamma, kind: 'leg', tip: atTipBoundary });
          segments.push({ a: p0, b: leg0, gamma: -gamma, kind: 'leg', tip: atTipBoundary });
        }
      }
    }
  });
  if (!boundVerts.length && !legVerts.length && !segments.length) return null;
  return { boundVerts, legVerts, segments };
}

function buildFlowVortexDataFromExec(result, fallbackSegments = [], displayOffset = null) {
  const offX = Number(displayOffset?.x) || 0;
  const offY = Number(displayOffset?.y) || 0;
  const offZ = Number(displayOffset?.z) || 0;
  const fallback = Array.isArray(fallbackSegments) ? fallbackSegments : [];
  const solver = buildSolverHorseshoesFromExec(result);
  const horseshoesRaw = Array.isArray(solver?.horseshoes) ? solver.horseshoes : [];
  const horseshoes = horseshoesRaw.map((h) => ({
    ...h,
    x1: (Number(h.x1) || 0) + offX,
    y1: (Number(h.y1) || 0) + offY,
    z1: (Number(h.z1) || 0) + offZ,
    x2: (Number(h.x2) || 0) + offX,
    y2: (Number(h.y2) || 0) + offY,
    z2: (Number(h.z2) || 0) + offZ,
  }));
  const shiftedFallback = fallback.map((seg) => ({
    ...seg,
    a: [((Number(seg?.a?.[0]) || 0) + offX), ((Number(seg?.a?.[1]) || 0) + offY), ((Number(seg?.a?.[2]) || 0) + offZ)],
    b: [((Number(seg?.b?.[0]) || 0) + offX), ((Number(seg?.b?.[1]) || 0) + offY), ((Number(seg?.b?.[2]) || 0) + offZ)],
  }));

  if (!horseshoes.length) return { source: 'heuristic', segments: shiftedFallback };
  return {
    source: 'solver-vortices',
    horseshoes,
    beta: solver.beta,
    iysym: solver.iysym,
    izsym: solver.izsym,
    ysym: solver.ysym,
    zsym: solver.zsym,
    segments: shiftedFallback,
  };
}

function buildVortexGroup(vortexData) {
  if (!THREE || !vortexData) return null;
  const { boundVerts = [], legVerts = [] } = vortexData;
  if (!boundVerts.length && !legVerts.length) return null;
  const group = new THREE.Group();
  group.name = 'vortices';
  if (boundVerts.length) {
    const geom = new THREE.BufferGeometry();
    geom.setAttribute('position', new THREE.Float32BufferAttribute(boundVerts, 3));
    const mat = new THREE.LineBasicMaterial({ color: 0xf97316, transparent: true, opacity: 0.95 });
    const bound = new THREE.LineSegments(geom, mat);
    bound.name = 'bound-vortices';
    group.add(bound);
  }
  if (legVerts.length) {
    const geom = new THREE.BufferGeometry();
    geom.setAttribute('position', new THREE.Float32BufferAttribute(legVerts, 3));
    const mat = new THREE.LineBasicMaterial({ color: 0x22c55e, transparent: true, opacity: 0.85 });
    const legs = new THREE.LineSegments(geom, mat);
    legs.name = 'leg-vortices';
    group.add(legs);
  }
  return group;
}

function getBaseFlowVector() {
  const result = uiState.lastExecResult;
  const vinf = result?.VINF;
  if (Array.isArray(vinf) && vinf.length >= 3) {
    const v = new THREE.Vector3(Number(vinf[0]) || 0, Number(vinf[1]) || 0, Number(vinf[2]) || 0);
    if (v.lengthSq() > 1e-10) return v;
  }
  const speed = Math.max(1e-3, Number(els.vel?.value) || 1);
  const alpha = Number(result?.ALFA ?? 0);
  const beta = Number(result?.BETA ?? 0);
  return new THREE.Vector3(
    Math.cos(alpha) * Math.cos(beta),
    Math.sin(beta),
    Math.sin(alpha) * Math.cos(beta),
  ).multiplyScalar(speed);
}

function selectFlowTracerStartPoint(bounds) {
  const offX = Number(aircraft?.position?.x) || 0;
  const offY = Number(aircraft?.position?.y) || 0;
  const offZ = Number(aircraft?.position?.z) || 0;
  const surfaces = Array.isArray(uiState.displayModel?.surfaces) ? uiState.displayModel.surfaces : [];
  let best = null;
  surfaces.forEach((surface) => {
    const sections = Array.isArray(surface?.sections) ? surface.sections : [];
    if (!sections.length) return;
    let minX = Infinity;
    let maxX = -Infinity;
    let minY = Infinity;
    let maxY = -Infinity;
    let minZ = Infinity;
    let maxZ = -Infinity;
    sections.forEach((section) => {
      const x = Number(section?.xle);
      const y = Number(section?.yle);
      const z = Number(section?.zle);
      if (!Number.isFinite(x) || !Number.isFinite(y) || !Number.isFinite(z)) return;
      minX = Math.min(minX, x);
      maxX = Math.max(maxX, x);
      minY = Math.min(minY, y);
      maxY = Math.max(maxY, y);
      minZ = Math.min(minZ, z);
      maxZ = Math.max(maxZ, z);
    });
    if (!Number.isFinite(minX) || !Number.isFinite(maxX)) return;
    const spanY = Math.max(0, maxY - minY);
    const spanZ = Math.max(0, maxZ - minZ);
    const score = spanY - (0.35 * spanZ);
    if (!best || score > best.score || (Math.abs(score - best.score) < 1e-9 && spanY > best.spanY)) {
      best = {
        score,
        spanY,
        minX,
        yMid: 0.5 * (minY + maxY),
        zMid: 0.5 * (minZ + maxZ),
      };
    }
  });
  if (best) {
    return {
      x: best.minX - 0.5 + offX,
      y: best.yMid + offY,
      z: best.zMid + offZ,
    };
  }
  const cx = Number(bounds?.center?.x) || 0;
  const cy = Number(bounds?.center?.y) || 0;
  const cz = Number(bounds?.center?.z) || 0;
  const sx = Math.max(0.5, Number(bounds?.size?.x) || 1.0);
  return {
    x: (cx - (0.25 * sx) - 0.5) + offX,
    y: cy + offY,
    z: cz + offZ,
  };
}

function sampleFlowVelocityAtPoint(px, py, pz, cfg, out = null) {
  const outVec = out || [0, 0, 0];
  const includeRigidBodyRotation = Boolean(cfg?.includeRigidBodyRotation);
  const includeFreestream = Boolean(cfg?.includeFreestream);
  const base = cfg?.base || { x: 0, y: 0, z: 0 };
  const omega = cfg?.omega || { x: 0, y: 0, z: 0 };
  const horseshoes = Array.isArray(cfg?.horseshoes) ? cfg.horseshoes : [];
  const segments = Array.isArray(cfg?.segments) ? cfg.segments : [];
  const solverParams = cfg?.solverParams || {};
  const gammaScale = Number.isFinite(Number(cfg?.gammaScale)) ? Number(cfg.gammaScale) : 1;
  const core2 = Math.max(1e-12, Number(cfg?.core2) || 1e-6);
  const inducedClamp = Math.max(1e-6, Number(cfg?.inducedClamp) || 1e6);
  let vx = 0;
  let vy = 0;
  let vz = 0;
  if (includeRigidBodyRotation) {
    vx += (Number(omega.y) * pz) - (Number(omega.z) * py);
    vy += (Number(omega.z) * px) - (Number(omega.x) * pz);
    vz += (Number(omega.x) * py) - (Number(omega.y) * px);
  }
  if (includeFreestream) {
    vx += Number(base.x) || 0;
    vy += Number(base.y) || 0;
    vz += Number(base.z) || 0;
  }
  const acc = cfg?.tmpInducedAcc || [0, 0, 0];
  if (cfg && !cfg.tmpInducedAcc) cfg.tmpInducedAcc = acc;
  acc[0] = 0;
  acc[1] = 0;
  acc[2] = 0;
  if (horseshoes.length) {
    for (let i = 0; i < horseshoes.length; i += 1) {
      addInducedVelocityFromHorseshoe(px, py, pz, horseshoes[i], solverParams, acc);
    }
  } else if (segments.length) {
    for (let i = 0; i < segments.length; i += 1) {
      addInducedVelocityFromSegment(px, py, pz, segments[i], gammaScale, core2, acc);
    }
  }
  const imag = Math.hypot(acc[0], acc[1], acc[2]);
  if (imag > inducedClamp) {
    const s = inducedClamp / imag;
    acc[0] *= s;
    acc[1] *= s;
    acc[2] *= s;
  }
  vx += acc[0];
  vy += acc[1];
  vz += acc[2];
  outVec[0] = vx;
  outVec[1] = vy;
  outVec[2] = vz;
  return outVec;
}

function pushFlowTracerTrailSample(cfg, x, y, z) {
  const cap = Math.max(0, Math.floor(Number(cfg?.trailPointCapacity) || 0));
  if (!cap) return;
  if (!cfg.trailPositions || cfg.trailPositions.length < (cap * 3)) {
    cfg.trailPositions = new Float32Array(cap * 3);
  }
  let head = Math.floor(Number(cfg.trailHead));
  let count = Math.floor(Number(cfg.trailCount) || 0);
  if (!Number.isFinite(head) || head < 0 || head >= cap) {
    head = -1;
    count = 0;
  }
  head = (head + 1) % cap;
  const off = head * 3;
  cfg.trailPositions[off + 0] = Number(x) || 0;
  cfg.trailPositions[off + 1] = Number(y) || 0;
  cfg.trailPositions[off + 2] = Number(z) || 0;
  cfg.trailHead = head;
  cfg.trailCount = Math.min(cap, count + 1);
}

function pushCloudTrailSampleForIndex(cfg, pointIdx, x, y, z) {
  const pointCount = Math.max(0, Math.floor(Number(cfg?.cloudCount) || 0));
  const cap = Math.max(0, Math.floor(Number(cfg?.trailPointCapacity) || 0));
  if (!pointCount || !cap || pointIdx < 0 || pointIdx >= pointCount) return;
  const heads = cfg.cloudTrailHeads;
  const counts = cfg.cloudTrailCounts;
  const pos = cfg.cloudTrailPositions;
  if (!heads || !counts || !pos) return;
  let head = Math.floor(Number(heads[pointIdx]));
  let count = Math.floor(Number(counts[pointIdx] || 0));
  if (!Number.isFinite(head) || head < 0 || head >= cap) {
    head = -1;
    count = 0;
  }
  head = (head + 1) % cap;
  const base = (pointIdx * cap * 3) + (head * 3);
  pos[base + 0] = Number(x) || 0;
  pos[base + 1] = Number(y) || 0;
  pos[base + 2] = Number(z) || 0;
  heads[pointIdx] = head;
  counts[pointIdx] = Math.min(cap, count + 1);
}

function resetCloudTrailForIndex(cfg, pointIdx, x, y, z) {
  const pointCount = Math.max(0, Math.floor(Number(cfg?.cloudCount) || 0));
  if (!pointCount || pointIdx < 0 || pointIdx >= pointCount) return;
  const heads = cfg.cloudTrailHeads;
  const counts = cfg.cloudTrailCounts;
  if (!heads || !counts) return;
  heads[pointIdx] = -1;
  counts[pointIdx] = 0;
  pushCloudTrailSampleForIndex(cfg, pointIdx, x, y, z);
}

function writeCloudTrailGeometry(cfg) {
  const line = cfg?.cloudTrailLine;
  const posAttr = line?.geometry?.getAttribute?.('position');
  const alphaAttr = line?.geometry?.getAttribute?.('alpha');
  const cloudPosAttr = cfg?.cloudPoints?.geometry?.getAttribute?.('position');
  if (!posAttr?.array || !alphaAttr?.array || !cloudPosAttr?.array) return;
  const trailPos = cfg.cloudTrailPositions;
  const heads = cfg.cloudTrailHeads;
  const counts = cfg.cloudTrailCounts;
  const cloudCount = Math.max(0, Math.floor(Number(cfg.cloudCount) || 0));
  const cap = Math.max(0, Math.floor(Number(cfg.trailPointCapacity) || 0));
  const sampleSec = Math.max(1e-6, Number(cfg.trailSampleSec) || 1);
  const durationSec = Math.max(sampleSec, Number(cfg.trailDurationSec) || 30);
  if (!trailPos || !heads || !counts || !cloudCount || !cap) return;
  const pos = posAttr.array;
  const alpha = alphaAttr.array;
  const cloud = cloudPosAttr.array;
  let segGlobal = 0;
  const writeSeg = (sx, sy, sz, ex, ey, ez, a) => {
    const pOff = segGlobal * 6;
    const aOff = segGlobal * 2;
    pos[pOff + 0] = sx;
    pos[pOff + 1] = sy;
    pos[pOff + 2] = sz;
    pos[pOff + 3] = ex;
    pos[pOff + 4] = ey;
    pos[pOff + 5] = ez;
    const aa = Math.max(0, Math.min(0.5, Number(a) || 0));
    alpha[aOff + 0] = aa;
    alpha[aOff + 1] = aa;
    segGlobal += 1;
  };
  for (let i = 0; i < cloudCount; i += 1) {
    const count = Math.min(cap, Math.max(0, Math.floor(Number(counts[i]) || 0)));
    const maxTrailAgeSec = Math.max(
      sampleSec,
      Math.min(durationSec, Math.max(sampleSec, (count - 1) * sampleSec)),
    );
    const cx = cloud[(i * 3) + 0];
    const cy = cloud[(i * 3) + 1];
    const cz = cloud[(i * 3) + 2];
    const readSample = (idxFromNewest, out) => {
      if (idxFromNewest < 0 || idxFromNewest >= count) return false;
      const head = Math.floor(Number(heads[i]) || 0);
      const slot = (head - idxFromNewest + cap) % cap;
      const base = (i * cap * 3) + (slot * 3);
      out[0] = trailPos[base + 0];
      out[1] = trailPos[base + 1];
      out[2] = trailPos[base + 2];
      return true;
    };
    const tmpA = [0, 0, 0];
    const tmpB = [0, 0, 0];
    if (count > 0 && readSample(0, tmpA)) {
      writeSeg(cx, cy, cz, tmpA[0], tmpA[1], tmpA[2], 0.5);
      for (let k = 0; k < (count - 1); k += 1) {
        if (!readSample(k, tmpA) || !readSample(k + 1, tmpB)) break;
        const ageSec = (k + 1) * sampleSec;
        const fade = Math.max(0, Math.min(1, ageSec / maxTrailAgeSec));
        writeSeg(tmpA[0], tmpA[1], tmpA[2], tmpB[0], tmpB[1], tmpB[2], 0.5 * (1 - fade));
      }
    }
    for (let k = count; k < cap; k += 1) {
      writeSeg(cx, cy, cz, cx, cy, cz, 0);
    }
  }
  posAttr.needsUpdate = true;
  alphaAttr.needsUpdate = true;
}

function writeFlowTracerTrailGeometry(tracer, cfg) {
  const trailLine = cfg?.trailLine;
  const posAttr = trailLine?.geometry?.getAttribute?.('position');
  const alphaAttr = trailLine?.geometry?.getAttribute?.('alpha');
  if (!tracer || !cfg || !posAttr?.array || !alphaAttr?.array) return;
  const pos = posAttr.array;
  const alpha = alphaAttr.array;
  const cap = Math.max(0, Math.floor(Number(cfg.trailPointCapacity) || 0));
  const count = Math.min(cap, Math.max(0, Math.floor(Number(cfg.trailCount) || 0)));
  const trailPos = cfg.trailPositions;
  const sampleSec = Math.max(1e-6, Number(cfg.trailSampleSec) || 1);
  const durationSec = Math.max(sampleSec, Number(cfg.trailDurationSec) || 30);
  const maxTrailAgeSec = Math.max(
    sampleSec,
    Math.min(durationSec, Math.max(sampleSec, (count - 1) * sampleSec)),
  );
  const px = Number(tracer.position?.x) || 0;
  const py = Number(tracer.position?.y) || 0;
  const pz = Number(tracer.position?.z) || 0;
  const readSample = (idxFromNewest, out) => {
    if (!trailPos || idxFromNewest < 0 || idxFromNewest >= count) return false;
    const head = Math.floor(Number(cfg.trailHead) || 0);
    const slot = (head - idxFromNewest + cap) % cap;
    const off = slot * 3;
    out[0] = trailPos[off + 0];
    out[1] = trailPos[off + 1];
    out[2] = trailPos[off + 2];
    return true;
  };
  const tmpA = [0, 0, 0];
  const tmpB = [0, 0, 0];
  const maxSegments = cap;
  const setSegment = (segIdx, sx, sy, sz, ex, ey, ez, segAlpha) => {
    const pOff = segIdx * 6;
    const aOff = segIdx * 2;
    pos[pOff + 0] = sx;
    pos[pOff + 1] = sy;
    pos[pOff + 2] = sz;
    pos[pOff + 3] = ex;
    pos[pOff + 4] = ey;
    pos[pOff + 5] = ez;
    const a = Math.max(0, Math.min(0.5, Number(segAlpha) || 0));
    alpha[aOff + 0] = a;
    alpha[aOff + 1] = a;
  };

  let seg = 0;
  if (count > 0 && readSample(0, tmpA)) {
    const newestOpacity = 0.5;
    setSegment(seg, px, py, pz, tmpA[0], tmpA[1], tmpA[2], newestOpacity);
    seg += 1;
    for (let i = 0; i < (count - 1) && seg < maxSegments; i += 1, seg += 1) {
      if (!readSample(i, tmpA) || !readSample(i + 1, tmpB)) break;
      const ageSec = seg * sampleSec;
      const fade = Math.max(0, Math.min(1, ageSec / maxTrailAgeSec));
      const opacity = 0.5 * (1 - fade); // 50% opaque -> 100% transparent
      setSegment(seg, tmpA[0], tmpA[1], tmpA[2], tmpB[0], tmpB[1], tmpB[2], opacity);
    }
  }
  for (; seg < maxSegments; seg += 1) {
    setSegment(seg, px, py, pz, px, py, pz, 0);
  }
  posAttr.needsUpdate = true;
  alphaAttr.needsUpdate = true;
}

function addInducedVelocityFromSegment(px, py, pz, segment, gammaScale, core2, acc) {
  const ax = segment.a[0];
  const ay = segment.a[1];
  const az = segment.a[2];
  const bx = segment.b[0];
  const by = segment.b[1];
  const bz = segment.b[2];
  const r1x = px - ax;
  const r1y = py - ay;
  const r1z = pz - az;
  const r2x = px - bx;
  const r2y = py - by;
  const r2z = pz - bz;

  const cx = r1y * r2z - r1z * r2y;
  const cy = r1z * r2x - r1x * r2z;
  const cz = r1x * r2y - r1y * r2x;
  const c2 = (cx * cx) + (cy * cy) + (cz * cz) + core2;
  if (!Number.isFinite(c2) || c2 < 1e-14) return;

  const r1 = Math.hypot(r1x, r1y, r1z);
  const r2 = Math.hypot(r2x, r2y, r2z);
  if (r1 < 1e-7 || r2 < 1e-7) return;

  const ux = (r1x / r1) - (r2x / r2);
  const uy = (r1y / r1) - (r2y / r2);
  const uz = (r1z / r1) - (r2z / r2);
  const r0x = bx - ax;
  const r0y = by - ay;
  const r0z = bz - az;
  const dot = (r0x * ux) + (r0y * uy) + (r0z * uz);
  const coef = (gammaScale * Number(segment.gamma || 0) * dot) / (4 * Math.PI * c2);
  if (!Number.isFinite(coef)) return;
  acc[0] += cx * coef;
  acc[1] += cy * coef;
  acc[2] += cz * coef;
}

function addInducedVelocityFromVorvelc(
  px, py, pz,
  lbound,
  x1, y1, z1,
  x2, y2, z2,
  beta,
  rcore,
  gammaScale,
  acc,
) {
  addInducedVelocityFromVorvelcKernel(
    px, py, pz,
    lbound,
    x1, y1, z1,
    x2, y2, z2,
    beta,
    rcore,
    gammaScale,
    acc,
  );
}

function addInducedVelocityFromHorseshoe(px, py, pz, horse, solverParams, acc) {
  addInducedVelocityFromHorseshoeKernel(px, py, pz, horse, solverParams, acc);
}

function halton(index, base) {
  let i = Math.max(1, Math.floor(index));
  let f = 1;
  let r = 0;
  while (i > 0) {
    f /= base;
    r += f * (i % base);
    i = Math.floor(i / base);
  }
  return r;
}

function buildShuffledIndexArray(count) {
  const n = Math.max(0, Math.floor(Number(count) || 0));
  const arr = new Uint32Array(n);
  for (let i = 0; i < n; i += 1) arr[i] = i;
  for (let i = n - 1; i > 0; i -= 1) {
    const j = Math.floor(Math.random() * (i + 1));
    const tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }
  return arr;
}

function enableLineVertexAlpha(material) {
  if (!material || material.userData?.vertexAlphaEnabled) return;
  material.onBeforeCompile = (shader) => {
    shader.vertexShader = shader.vertexShader
      .replace('#include <common>', '#include <common>\nattribute float alpha;\nvarying float vAlpha;')
      .replace('#include <color_vertex>', '#include <color_vertex>\n  vAlpha = alpha;');
    shader.fragmentShader = shader.fragmentShader
      .replace('#include <common>', '#include <common>\nvarying float vAlpha;')
      .replace('vec4 diffuseColor = vec4( diffuse, opacity );', 'vec4 diffuseColor = vec4( diffuse, opacity * vAlpha );');
  };
  material.userData.vertexAlphaEnabled = true;
  material.needsUpdate = true;
}

function computeArrowHeadPoints(sx, sy, sz, ex, ey, ez, headBackScale = 0.35, headWidthScale = 0.22) {
  const dx = ex - sx;
  const dy = ey - sy;
  const dz = ez - sz;
  const len = Math.hypot(dx, dy, dz);
  if (len < 1e-12) {
    return {
      lx: ex, ly: ey, lz: ez,
      rx: ex, ry: ey, rz: ez,
    };
  }
  const ux = dx / len;
  const uy = dy / len;
  const uz = dz / len;
  const headBack = len * headBackScale;
  const headWidth = len * headWidthScale;

  let rx0 = 0;
  let ry0 = 0;
  let rz0 = 1;
  if (Math.abs(uz) > 0.85) {
    rx0 = 0;
    ry0 = 1;
    rz0 = 0;
  }

  let sxv = (uy * rz0) - (uz * ry0);
  let syv = (uz * rx0) - (ux * rz0);
  let szv = (ux * ry0) - (uy * rx0);
  let sLen = Math.hypot(sxv, syv, szv);
  if (sLen < 1e-9) {
    sxv = 1;
    syv = 0;
    szv = 0;
    sLen = 1;
  }
  sxv /= sLen;
  syv /= sLen;
  szv /= sLen;

  const hx = ex - (ux * headBack);
  const hy = ey - (uy * headBack);
  const hz = ez - (uz * headBack);

  return {
    lx: hx + (sxv * headWidth),
    ly: hy + (syv * headWidth),
    lz: hz + (szv * headWidth),
    rx: hx - (sxv * headWidth),
    ry: hy - (syv * headWidth),
    rz: hz - (szv * headWidth),
  };
}

function buildFlowFieldGroup(bounds, flowVortexData = null) {
  if (!THREE || !bounds) return null;
  const group = new THREE.Group();
  group.name = 'flow-field';
  const base = getBaseFlowVector();
  if (base.lengthSq() < 1e-12) return null;
  const size = bounds.size.clone();
  const maxDim = Math.max(size.x, size.y, size.z, 1e-3);
  const spanLength = Math.max(
    1e-3,
    Math.abs(Number(uiState.modelHeader?.bref ?? uiState.modelCache?.header?.bref ?? bounds.size.y ?? maxDim)),
  );
  const inputFlowData = flowVortexData && typeof flowVortexData === 'object'
    ? flowVortexData
    : { source: 'heuristic', segments: Array.isArray(flowVortexData) ? flowVortexData : [] };
  const horseshoes = Array.isArray(inputFlowData.horseshoes) ? inputFlowData.horseshoes : [];
  const hasSolverVortices = horseshoes.length > 0;
  const inputSegments = Array.isArray(inputFlowData.segments) ? inputFlowData.segments : [];
  const tipLegSegments = inputSegments.filter((seg) => seg?.tip && seg?.kind === 'leg');
  const segments = hasSolverVortices ? [] : (tipLegSegments.length ? tipLegSegments : inputSegments);
  const flowMode = FLOW_FIELD_MODES.includes(uiState.flowFieldMode) ? uiState.flowFieldMode : FLOW_FIELD_MODES[0];
  const flowDeltaOnly = flowMode !== 'full';
  const includeRigidBodyRotation = flowMode !== 'induced';
  const includeFreestream = flowMode === 'full';
  const w = uiState.lastExecResult?.WROT;
  const rotX = Number(w?.[0]) || 0; // p*b/(2V)
  const rotY = Number(w?.[1]) || 0; // q*c/(2V)
  const rotZ = Number(w?.[2]) || 0; // r*b/(2V)
  const baseMag = Math.max(base.length(), 1e-6);
  const brefRef = Math.max(1e-6, Math.abs(Number(uiState.modelHeader?.bref ?? uiState.modelCache?.header?.bref ?? 1)));
  const crefRef = Math.max(1e-6, Math.abs(Number(uiState.modelHeader?.cref ?? uiState.modelCache?.header?.cref ?? 1)));
  const omegaX = rotX * (2 * baseMag / brefRef);
  const omegaY = rotY * (2 * baseMag / crefRef);
  const omegaZ = rotZ * (2 * baseMag / brefRef);
  const core2 = Math.pow(0.03 * maxDim, 2);
  const gammaScale = baseMag * Math.max(0.08 * maxDim, 0.12);
  const inducedClamp = baseMag * 1.4;
  const solverParams = {
    beta: Math.abs(Number(inputFlowData.beta) || 0) > 1e-8 ? Number(inputFlowData.beta) : 1,
    iysym: Number(inputFlowData.iysym) || 0,
    izsym: Number(inputFlowData.izsym) || 0,
    ysym: Number(inputFlowData.ysym) || 0,
    zsym: Number(inputFlowData.zsym) || 0,
  };
  const start = selectFlowTracerStartPoint(bounds);
  const tracerGeom = new THREE.BufferGeometry();
  tracerGeom.setAttribute('position', new THREE.Float32BufferAttribute([0, 0, 0], 3));
  const tracerSize = 2.2;
  const tracerMat = new THREE.PointsMaterial({
    color: 0xbfdbfe,
    size: tracerSize,
    sizeAttenuation: false,
    transparent: true,
    opacity: 0.98,
    map: getFlowPointSpriteTexture(),
    alphaTest: 0.08,
    depthWrite: false,
  });
  const tracer = new THREE.Points(tracerGeom, tracerMat);
  tracer.name = 'flow-tracer';
  tracer.renderOrder = 6;
  tracer.position.set(start.x, start.y, start.z);
  const cloudCount = 960;
  const cloudPos = new Float32Array(cloudCount * 3);
  let seq = 1;
  let ci = 0;
  const maxAttempts = cloudCount * 24;
  while (ci < cloudCount && seq <= maxAttempts) {
    const xNorm = (2 * halton(seq, 2)) - 1;
    const yNorm = (2 * halton(seq, 3)) - 1;
    const zNorm = (2 * halton(seq, 5)) - 1;
    seq += 1;
    const r2 = (xNorm * xNorm) + (yNorm * yNorm) + (zNorm * zNorm);
    if (r2 > 1) continue;
    cloudPos[(ci * 3) + 0] = xNorm * spanLength;
    cloudPos[(ci * 3) + 1] = yNorm * spanLength;
    cloudPos[(ci * 3) + 2] = zNorm * spanLength;
    ci += 1;
  }
  const cloudGeom = new THREE.BufferGeometry();
  cloudGeom.setAttribute('position', new THREE.BufferAttribute(cloudPos, 3));
  const cloudMat = new THREE.PointsMaterial({
    color: 0x93c5fd,
    size: 3.2,
    sizeAttenuation: false,
    transparent: true,
    opacity: 0.78,
    map: getFlowPointSpriteTexture(),
    alphaTest: 0.08,
    depthTest: false,
    depthWrite: false,
  });
  const cloudPoints = new THREE.Points(cloudGeom, cloudMat);
  cloudPoints.name = 'flow-tracer-cloud';
  cloudPoints.renderOrder = 4;
  const trailSampleSec = 0.5;
  const trailDurationSec = Math.max(trailSampleSec, 5 * (brefRef / 15));
  const trailPointCapacity = Math.floor(trailDurationSec / trailSampleSec) + 1; // include t=0 sample
  const cloudTrailSegmentCapacity = Math.max(1, ci * trailPointCapacity);
  const cloudTrailGeom = new THREE.BufferGeometry();
  const cloudTrailPos = new Float32Array(cloudTrailSegmentCapacity * 2 * 3);
  const cloudTrailCol = new Float32Array(cloudTrailSegmentCapacity * 2 * 3);
  const cloudTrailAlpha = new Float32Array(cloudTrailSegmentCapacity * 2);
  const cloudTrailColor = new THREE.Color(0x93c5fd);
  for (let i = 0; i < (cloudTrailSegmentCapacity * 2); i += 1) {
    const off = i * 3;
    cloudTrailCol[off + 0] = cloudTrailColor.r;
    cloudTrailCol[off + 1] = cloudTrailColor.g;
    cloudTrailCol[off + 2] = cloudTrailColor.b;
  }
  cloudTrailGeom.setAttribute('position', new THREE.BufferAttribute(cloudTrailPos, 3));
  cloudTrailGeom.setAttribute('color', new THREE.BufferAttribute(cloudTrailCol, 3));
  cloudTrailGeom.setAttribute('alpha', new THREE.BufferAttribute(cloudTrailAlpha, 1));
  const cloudTrailMat = new THREE.LineBasicMaterial({ vertexColors: true, transparent: true, opacity: 1 });
  enableLineVertexAlpha(cloudTrailMat);
  const cloudTrailLine = new THREE.LineSegments(cloudTrailGeom, cloudTrailMat);
  cloudTrailLine.name = 'flow-tracer-cloud-trail';
  cloudTrailLine.renderOrder = 3;
  const trailSegmentCapacity = trailPointCapacity;
  const trailGeom = new THREE.BufferGeometry();
  const trailPos = new Float32Array(trailSegmentCapacity * 2 * 3);
  const trailCol = new Float32Array(trailSegmentCapacity * 2 * 3);
  const trailAlpha = new Float32Array(trailSegmentCapacity * 2);
  const trailColor = new THREE.Color(0x93c5fd);
  for (let i = 0; i < (trailSegmentCapacity * 2); i += 1) {
    const cOff = i * 3;
    trailCol[cOff + 0] = trailColor.r;
    trailCol[cOff + 1] = trailColor.g;
    trailCol[cOff + 2] = trailColor.b;
  }
  trailGeom.setAttribute('position', new THREE.BufferAttribute(trailPos, 3));
  trailGeom.setAttribute('color', new THREE.BufferAttribute(trailCol, 3));
  trailGeom.setAttribute('alpha', new THREE.BufferAttribute(trailAlpha, 1));
  const trailMat = new THREE.LineBasicMaterial({ vertexColors: true, transparent: true, opacity: 1 });
  enableLineVertexAlpha(trailMat);
  const trailLine = new THREE.LineSegments(trailGeom, trailMat);
  trailLine.name = 'flow-tracer-trail';
  trailLine.renderOrder = 5;
  const cloudTrailSamplePhase = new Float32Array(Math.max(1, ci));
  for (let i = 0; i < ci; i += 1) {
    // Stagger trail sampling so traces do not all update on the same frame.
    cloudTrailSamplePhase[i] = halton(i + 1, 7) * trailSampleSec;
  }
  tracer.userData.flowTracer = {
    start: { ...start },
    velocity: { x: 0, y: 0, z: 0 },
    lastTimeSec: null,
    includeRigidBodyRotation,
    includeFreestream,
    base: { x: base.x, y: base.y, z: base.z },
    omega: { x: omegaX, y: omegaY, z: omegaZ },
    horseshoes,
    segments,
    solverParams,
    gammaScale,
    core2,
    inducedClamp,
    spanLength,
    trailDurationSec,
    trailSampleSec,
    trailPointCapacity,
    trailPositions: new Float32Array(trailPointCapacity * 3),
    trailHead: -1,
    trailCount: 0,
    trailNextSampleSec: null,
    trailLine,
    cloudPoints,
    cloudCount: ci,
    cloudStartPositions: cloudPos.slice(),
    cloudTrailLine,
    cloudTrailPositions: new Float32Array(Math.max(1, ci * trailPointCapacity * 3)),
    cloudTrailSamplePhase,
    cloudTrailNextSampleSec: new Float64Array(Math.max(1, ci)),
    cloudTrailHeads: (() => {
      const arr = new Int16Array(Math.max(1, ci));
      for (let i = 0; i < arr.length; i += 1) arr[i] = -1;
      return arr;
    })(),
    cloudTrailCounts: new Uint8Array(Math.max(1, ci)),
    speedScale: flowMode === 'full' ? 1 : 100,
  };
  group.add(cloudTrailLine);
  group.add(cloudPoints);
  group.add(trailLine);
  group.add(tracer);

  group.userData.flowSource = hasSolverVortices ? (inputFlowData.source || 'solver-strips') : 'heuristic';
  group.userData.flowVortexCount = hasSolverVortices ? horseshoes.length : segments.length;
  group.userData.flowDeltaOnly = flowDeltaOnly;
  group.userData.flowIncludesRigidBodyRotation = includeRigidBodyRotation;
  group.userData.flowMode = flowMode;
  group.userData.flowArrowheads = false;
  group.userData.flowTrails = true;
  group.userData.flowTracer = true;
  return group;
}

function removeAuxOverlays() {
  if (panelSpacingGroup) {
    panelSpacingGroup.parent?.remove?.(panelSpacingGroup);
    panelSpacingGroup = null;
  }
  if (vortexGroup) {
    vortexGroup.parent?.remove?.(vortexGroup);
    vortexGroup = null;
  }
  if (flowFieldGroup) {
    flowFieldGroup.parent?.remove?.(flowFieldGroup);
    flowFieldGroup = null;
  }
}

function applyAuxOverlayVisibility() {
  if (panelSpacingGroup) panelSpacingGroup.visible = Boolean(uiState.showPanelSpacing);
  if (vortexGroup) vortexGroup.visible = Boolean(uiState.showVortices);
  if (flowFieldGroup) {
    flowFieldGroup.visible = Boolean(uiState.showFlowField);
    if (!uiState.showFlowField) resetFlowTracerPosition(flowFieldGroup);
  }
}

function rebuildAuxOverlays(bounds = null) {
  if (!aircraft || !uiState.displayModel) return;
  removeAuxOverlays();
  panelSpacingGroup = buildPanelSpacingGroup(uiState.displayModel);
  const vortexData = buildVortexData(uiState.displayModel);
  latestVortexData = vortexData;
  vortexGroup = buildVortexGroup(vortexData);
  const effectiveBounds = bounds || computeBounds(aircraft);
  const flowOffset = {
    x: Number(aircraft.position?.x) || 0,
    y: Number(aircraft.position?.y) || 0,
    z: Number(aircraft.position?.z) || 0,
  };
  const flowVortexData = buildFlowVortexDataFromExec(uiState.lastExecResult, vortexData?.segments || [], flowOffset);
  flowFieldGroup = buildFlowFieldGroup(effectiveBounds, flowVortexData);
  if (panelSpacingGroup) aircraft.add(panelSpacingGroup);
  if (vortexGroup) aircraft.add(vortexGroup);
  if (flowFieldGroup && scene) scene.add(flowFieldGroup);
  if (flowFieldGroup) resetFlowTracerPosition(flowFieldGroup);
  flowFieldAnimLastMs = 0;
  applyAuxOverlayVisibility();
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
  const profileVerts = [];
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

  const buildSectionProfilePoints = (section) => {
    let coords = section.airfoilCoords;
    if (!coords && section.airfoilFile) {
      const resolved = resolveProvidedAirfoilKey(section.airfoilFile);
      if (resolved) coords = airfoilCache.get(resolved) || null;
    }
    if ((!coords || !coords.length) && section.naca) {
      coords = buildNacaProfileCoords(section.naca, 40);
    }
    const profile = buildProfileSections(coords || [], 28);
    const toPoint = ([x, z]) => transformSectionPoint(section, [
      section.xle + x * section.chord,
      section.yle,
      section.zle + z * section.chord,
    ]);
    return {
      upper: profile.upper.map(toPoint),
      lower: profile.lower.map(toPoint),
    };
  };

  const pushProfileQuad = (a, b, c, d) => {
    profileVerts.push(...a, ...b, ...c, ...a, ...c, ...d);
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

  const sectionProfiles = surface.sections.map((section) => buildSectionProfilePoints(section));
  for (let s = 0; s < sectionProfiles.length - 1; s += 1) {
    const pa = sectionProfiles[s];
    const pb = sectionProfiles[s + 1];
    const upperN = Math.min(pa.upper.length, pb.upper.length);
    const lowerN = Math.min(pa.lower.length, pb.lower.length);
    for (let i = 0; i < upperN - 1; i += 1) {
      pushProfileQuad(pa.upper[i], pb.upper[i], pb.upper[i + 1], pa.upper[i + 1]);
    }
    for (let i = 0; i < lowerN - 1; i += 1) {
      pushProfileQuad(pa.lower[i], pa.lower[i + 1], pb.lower[i + 1], pb.lower[i]);
    }
    if (upperN > 1 && lowerN > 1) {
      pushProfileQuad(pa.upper[0], pb.upper[0], pb.lower[0], pa.lower[0]);
      pushProfileQuad(pa.upper[upperN - 1], pa.lower[lowerN - 1], pb.lower[lowerN - 1], pb.upper[upperN - 1]);
    }

    if (typeof surface.yduplicate === 'number') {
      const mirror = (p) => [p[0], 2 * surface.yduplicate - p[1], p[2]];
      for (let i = 0; i < upperN - 1; i += 1) {
        pushProfileQuad(mirror(pa.upper[i]), mirror(pb.upper[i]), mirror(pb.upper[i + 1]), mirror(pa.upper[i + 1]));
      }
      for (let i = 0; i < lowerN - 1; i += 1) {
        pushProfileQuad(mirror(pa.lower[i]), mirror(pa.lower[i + 1]), mirror(pb.lower[i + 1]), mirror(pb.lower[i]));
      }
      if (upperN > 1 && lowerN > 1) {
        pushProfileQuad(mirror(pa.upper[0]), mirror(pb.upper[0]), mirror(pb.lower[0]), mirror(pa.lower[0]));
        pushProfileQuad(mirror(pa.upper[upperN - 1]), mirror(pa.lower[lowerN - 1]), mirror(pb.lower[lowerN - 1]), mirror(pb.upper[upperN - 1]));
      }
    }
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
  outlineLine.userData.renderKind = 'wire';
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
    ctrlLine.userData.renderKind = 'wire';
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
    hingeLine.userData.renderKind = 'wire';
    group.add(hingeLine);
  }

  camberLines.forEach((line) => {
    const flat = line.flat();
    const camGeom = new THREE.BufferGeometry();
    camGeom.setAttribute('position', new THREE.Float32BufferAttribute(flat, 3));
    const camLine = new THREE.Line(camGeom, sectionMat);
    camLine.userData.renderKind = 'wire';
    group.add(camLine);
  });

  airfoilOutlines.forEach((line) => {
    const flat = line.flat();
    const foilGeom = new THREE.BufferGeometry();
    foilGeom.setAttribute('position', new THREE.Float32BufferAttribute(flat, 3));
    const foilLine = new THREE.Line(foilGeom, sectionMat);
    foilLine.userData.renderKind = 'wire';
    group.add(foilLine);
  });

  if (profileVerts.length) {
    const skinGeom = new THREE.BufferGeometry();
    skinGeom.setAttribute('position', new THREE.Float32BufferAttribute(profileVerts, 3));
    skinGeom.computeVertexNormals();
    const baseCol = new THREE.Color(color);
    const pressureBaseCol = new THREE.Color(0xd8dce3);
    const skinMat = new THREE.MeshStandardMaterial({
      color: baseCol,
      side: THREE.DoubleSide,
      metalness: 0.12,
      roughness: 0.5,
      transparent: true,
      opacity: 0.9,
    });
    const skinMesh = new THREE.Mesh(skinGeom, skinMat);
    skinMesh.name = 'surface-skin';
    skinMesh.userData.renderKind = 'skin';
    skinMesh.userData.baseColor = baseCol;
    skinMesh.userData.pressureBaseColor = pressureBaseCol;
    group.add(skinMesh);

    const overlayGeom = skinGeom.clone();
    const overlayColor = new Float32Array((profileVerts.length / 3) * 3);
    const overlayAlpha = new Float32Array(profileVerts.length / 3);
    overlayGeom.setAttribute('color', new THREE.BufferAttribute(overlayColor, 3));
    overlayGeom.setAttribute('alpha', new THREE.BufferAttribute(overlayAlpha, 1));
    const overlayMat = new THREE.MeshStandardMaterial({
      color: 0xffffff,
      vertexColors: true,
      side: THREE.DoubleSide,
      metalness: 0.0,
      roughness: 0.65,
      transparent: true,
      opacity: 1.0,
      depthWrite: false,
    });
    enableVertexAlpha(overlayMat);
    const overlayMesh = new THREE.Mesh(overlayGeom, overlayMat);
    overlayMesh.name = 'surface-pressure-overlay';
    overlayMesh.userData.renderKind = 'skin-overlay';
    overlayMesh.visible = false;
    overlayMesh.renderOrder = 3;
    group.add(overlayMesh);
  }

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
  sprite.userData.renderKind = 'wire';
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

function addReferenceMarker(target, header, maxDim) {
  if (!THREE || !target || !header) return;
  const xref = Number(header.xref);
  const yref = Number(header.yref);
  const zref = Number(header.zref);
  if (!Number.isFinite(xref) || !Number.isFinite(yref) || !Number.isFinite(zref)) return;
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
  // Marker is attached to aircraft local space; aircraft translation already recenters the model.
  marker.position.set(xref, yref, zref);
  target.add(marker);
}

function rebuildAircraftVisual(shouldFit = false) {
  if (!scene || !uiState.displayModel) return;
  modeAnimation = null;
  const newAircraft = buildAircraftFromAVL(uiState.displayModel);
  if (aircraft) scene.remove(aircraft);
  aircraft = newAircraft;
  scene.add(aircraft);
  surfaceSkinMeshes = [];
  surfacePressureMeshes = [];
  surfaceWireObjects = [];
  aircraft.traverse((obj) => {
    if (obj?.isMesh && obj.name === 'surface-skin') surfaceSkinMeshes.push(obj);
    if (obj?.isMesh && obj.name === 'surface-pressure-overlay') surfacePressureMeshes.push(obj);
    if (obj?.userData?.renderKind === 'wire') surfaceWireObjects.push(obj);
  });
  updateBank(Number(els.bank.value));
  const bounds = computeBounds(aircraft);
  if (bounds) {
    aircraft.position.sub(bounds.center);
    viewerState.bounds = bounds;
    viewerState.fitDistance = bounds.maxDim * 1.6 + 4.0;
    addReferenceMarker(aircraft, uiState.modelHeader, bounds.maxDim);
    if (shouldFit) {
      rebuildGrid(bounds.maxDim);
      applyGridMode();
      if (axesHelper) axesHelper.position.set(0, 0, 0);
    }
  } else {
    addReferenceMarker(aircraft, uiState.modelHeader, null);
  }
  if (shouldFit) fitCameraToObject(aircraft);
  updateLoadingVisualization();
  applySurfaceRenderMode();
  rebuildAuxOverlays(bounds || null);
}

let scene;
let camera;
let renderer;
let controls;
let aircraft;
let gridHelper;
let axesHelper;
let surfaceSkinMeshes = [];
let surfacePressureMeshes = [];
let surfaceWireObjects = [];
let panelSpacingGroup = null;
let vortexGroup = null;
let latestVortexData = null;
let flowFieldGroup = null;
let flowFieldAnimLastMs = 0;
let modeAnimation = null;
let eigenTouchZoom = { active: false, lastDist: 0 };
let flowProjectTmp = null;
let flowPointSpriteTexture = null;

function getFlowPointSpriteTexture() {
  if (!THREE) return null;
  if (flowPointSpriteTexture) return flowPointSpriteTexture;
  const size = 64;
  const canvas = document.createElement('canvas');
  canvas.width = size;
  canvas.height = size;
  const ctx = canvas.getContext('2d');
  if (!ctx) return null;
  const c = size * 0.5;
  const grad = ctx.createRadialGradient(c, c, 0, c, c, c);
  grad.addColorStop(0, 'rgba(255,255,255,1)');
  grad.addColorStop(0.45, 'rgba(255,255,255,0.95)');
  grad.addColorStop(0.75, 'rgba(255,255,255,0.25)');
  grad.addColorStop(1, 'rgba(255,255,255,0)');
  ctx.clearRect(0, 0, size, size);
  ctx.fillStyle = grad;
  ctx.fillRect(0, 0, size, size);
  flowPointSpriteTexture = new THREE.CanvasTexture(canvas);
  flowPointSpriteTexture.needsUpdate = true;
  return flowPointSpriteTexture;
}

function updateViewerButtons() {
  if (!els.viewerPan || !els.viewerView || !els.viewerGrid) return;
  els.viewerPan.classList.toggle('active', viewerState.mode === 'pan');
  const viewLabel = viewerState.viewModes[viewerState.viewIndex] || 'top';
  const viewTitle = VIEW_MODE_LABELS[viewLabel] || viewLabel;
  const gridLabel = viewerState.gridModes[viewerState.gridIndex] || 'xy';
  els.viewerView.title = `View: ${viewTitle}`;
  els.viewerGrid.title = `Grid: ${gridLabel.toUpperCase()}`;
  if (els.viewerLoad) {
    els.viewerLoad.classList.toggle('active', uiState.showLoading);
  }
  if (els.viewerSurface) {
    const mode = uiState.surfaceRenderMode;
    els.viewerSurface.classList.toggle('active', mode !== 'wireframe');
    if (mode === 'wireframe') els.viewerSurface.title = 'Render: wireframe only';
    else if (mode === 'both') els.viewerSurface.title = 'Render: surface + wireframe';
    else els.viewerSurface.title = 'Render: surface only';
  }
  if (els.viewerPressure) {
    els.viewerPressure.classList.toggle('active', uiState.showPressure);
    els.viewerPressure.title = uiState.showPressure ? 'Pressure shading on' : 'Pressure shading off';
  }
  if (els.viewerPanelSpacing) {
    els.viewerPanelSpacing.classList.toggle('active', uiState.showPanelSpacing);
    els.viewerPanelSpacing.title = uiState.showPanelSpacing ? 'Panel spacing on' : 'Panel spacing off';
  }
  if (els.viewerVortices) {
    els.viewerVortices.classList.toggle('active', uiState.showVortices);
    els.viewerVortices.title = uiState.showVortices ? 'Bound + leg vortices on' : 'Bound + leg vortices off';
  }
  if (els.viewerFlow) {
    els.viewerFlow.classList.toggle('active', uiState.showFlowField);
    const mode = FLOW_FIELD_MODES.includes(uiState.flowFieldMode) ? uiState.flowFieldMode : FLOW_FIELD_MODES[0];
    const modeLabel = mode === 'induced'
      ? 'induced only'
      : (mode === 'induced+rotation' ? 'induced + body-rotation' : 'full flow');
    els.viewerFlow.title = uiState.showFlowField
      ? `Flow: ${modeLabel} (tap to cycle)`
      : `Flow off (next: ${modeLabel})`;
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
  if (mode === 'top') {
    setCameraUp(new THREE.Vector3(0, 1, 0));
    camera.position.set(0.0001, 0, dist);
  } else if (mode === 'forward') {
    setCameraUp(new THREE.Vector3(0, 0, 1));
    camera.position.set(dist, 0, 0);
  } else if (mode === 'side') {
    setCameraUp(new THREE.Vector3(0, 0, 1));
    camera.position.set(0, dist, 0);
  } else {
    setCameraUp(new THREE.Vector3(0, 0, 1));
    camera.position.set(dist * 0.6, -dist * 0.4, dist * 0.28);
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
  aircraft.rotation.x = 0;
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
  const rawText = String(text || '');
  syncTemplateParamsFromText(rawText);
  const resolvedText = resolveTemplateParamsInText(rawText);
  uiState.resolvedText = resolvedText;
  const parsed = parseAVL(resolvedText);
  uiState.modelHeader = parsed.header || null;
  const solverModel = buildSolverModel(resolvedText);
  renderFileHeaderSummary(uiState.modelHeader, solverModel);
  uiState.controlMap = solverModel.controlMap;
  uiState.modelCache = solverModel;
  rebuildConstraintUI(solverModel);
  if (uiState.needsRunCaseConstraintSync) {
    const idx = activeRunCaseIndex();
    if (idx >= 0 && Array.isArray(uiState.runCases) && idx < uiState.runCases.length) {
      applyRunCaseToUI(uiState.runCases[idx]);
    }
    uiState.needsRunCaseConstraintSync = false;
  }
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

  const runCase = activeRunCaseEntry();
  const runInputs = runCase && typeof runCase.inputs === 'object' ? runCase.inputs : null;
  const runMach = Number(runInputs?.mach);
  const headerMach = Number(model.header?.mach);
  const machUse = (Number.isFinite(runMach) && runMach >= 0)
    ? runMach
    : (Number.isFinite(headerMach) ? headerMach : 0.0);

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
    UNITL: 1.0,
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
    LMASS: Boolean(uiState.massPropsFilename),
    LFLOAD: new Uint8Array(NSURF + 1),

    ALFA: 0.0,
    BETA: 0.0,
    MACH: Math.fround(machUse),
    AMACH: Math.fround(machUse),
    BETM: 0.0,
    VINF: new Float32Array(3),
    VINF_A: new Float32Array(3),
    VINF_B: new Float32Array(3),
    WROT: new Float32Array(3),
    AMASS: new Float32Array(9),
    AINER: new Float32Array(9),
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
    const massVal = Number(uiState.massProps?.mass ?? els.mass?.value ?? 0);
    const massUse = massVal > 0 ? massVal : 1.0;
    state.PARVAL[idx2(IPMASS, IR, IPTOT)] = Math.fround(massUse);
  }
  state.PARVAL[idx2(IPCL, IR, IPTOT)] = Math.fround(Number(els.cl?.value || 0));
  state.PARVAL[idx2(IPPHI, IR, IPTOT)] = Math.fround(Number(els.bank?.value || 0));
  state.PARVAL[idx2(IPXCG, IR, IPTOT)] = Math.fround(Number.isFinite(uiState.massProps?.xcg) ? uiState.massProps.xcg : (Number.isFinite(model.header.xref) ? model.header.xref : 0.0));
  state.PARVAL[idx2(IPYCG, IR, IPTOT)] = Math.fround(Number.isFinite(uiState.massProps?.ycg) ? uiState.massProps.ycg : (Number.isFinite(model.header.yref) ? model.header.yref : 0.0));
  state.PARVAL[idx2(IPZCG, IR, IPTOT)] = Math.fround(Number.isFinite(uiState.massProps?.zcg) ? uiState.massProps.zcg : (Number.isFinite(model.header.zref) ? model.header.zref : 0.0));
  state.PARVAL[idx2(IPIXX, IR, IPTOT)] = Math.fround(Number.isFinite(uiState.massProps?.ixx) ? uiState.massProps.ixx : 0.0);
  state.PARVAL[idx2(IPIYY, IR, IPTOT)] = Math.fround(Number.isFinite(uiState.massProps?.iyy) ? uiState.massProps.iyy : 0.0);
  state.PARVAL[idx2(IPIZZ, IR, IPTOT)] = Math.fround(Number.isFinite(uiState.massProps?.izz) ? uiState.massProps.izz : 0.0);
  // AVL stores products of inertia with opposite sign internally.
  state.PARVAL[idx2(IPIXY, IR, IPTOT)] = Math.fround(Number.isFinite(uiState.massProps?.ixy) ? -uiState.massProps.ixy : 0.0);
  state.PARVAL[idx2(IPIZX, IR, IPTOT)] = Math.fround(Number.isFinite(uiState.massProps?.ixz) ? -uiState.massProps.ixz : 0.0);
  state.PARVAL[idx2(IPIYZ, IR, IPTOT)] = Math.fround(Number.isFinite(uiState.massProps?.iyz) ? -uiState.massProps.iyz : 0.0);
  {
    const cd0 = Number(runInputs?.cd0);
    state.PARVAL[idx2(IPCD0, IR, IPTOT)] = Math.fround(Number.isFinite(cd0) ? cd0 : 0.0);
  }
  // Seed alpha to a small positive value to avoid singular trim steps.
  state.ALFA = Math.fround(2.0 * state.DTR);
  state.BETA = 0.0;

  // Trim variable selection (defaults; overridden by constraint UI)
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
  const rawText = String(text || '');
  syncTemplateParamsFromText(rawText);
  const resolvedText = resolveTemplateParamsInText(rawText);
  uiState.resolvedText = resolvedText;
  const model = buildSolverModel(resolvedText);
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
    logDebug('EXEC wasm toggle enabled; using JS EXEC kernels for drag parity.');
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
  const computedEigenModes = computeEigenModesFromExec(result);
  const activeCaseIdx = activeRunCaseIndex();
  if (activeCaseIdx >= 0 && Array.isArray(uiState.runCases) && uiState.runCases.length) {
    if (!uiState.eigenModesByRunCase || typeof uiState.eigenModesByRunCase !== 'object') {
      uiState.eigenModesByRunCase = {};
    }
    uiState.eigenModesByRunCase[activeCaseIdx] = computedEigenModes.map((m) => ({ ...m }));
    const merged = [];
    for (let i = 0; i < uiState.runCases.length; i += 1) {
      const modesForCase = uiState.eigenModesByRunCase[i];
      if (!Array.isArray(modesForCase) || !modesForCase.length) continue;
      modesForCase.forEach((m) => merged.push({ ...m, runCaseIndex: i }));
    }
    uiState.eigenModes = merged;
  } else {
    uiState.eigenModesByRunCase = {};
    uiState.eigenModes = computedEigenModes.map((m) => ({ ...m, runCaseIndex: -1 }));
  }
  if (uiState.selectedEigenMode >= uiState.eigenModes.length) {
    uiState.selectedEigenMode = -1;
  }
  drawEigenPlot();
  uiState.pressureField = buildPressureFieldFromExec(result);
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

  if (Number.isFinite(result.ALFA) && els.outAlpha) els.outAlpha.textContent = fmtSignedAligned(result.ALFA / (Math.PI / 180), 2);
  if (Number.isFinite(result.BETA) && els.outBeta) els.outBeta.textContent = fmtSignedAligned(result.BETA / (Math.PI / 180), 2);
  if (Number.isFinite(result.CLTOT) && els.outCL) els.outCL.textContent = fmtSignedAligned(result.CLTOT, 5);
  if (Number.isFinite(result.CDTOT) && els.outCD) els.outCD.textContent = fmtSignedAligned(result.CDTOT, 5);
  if (Number.isFinite(vee) && els.outV) els.outV.textContent = fmt(vee, 2);
  if (Number.isFinite(phi) && els.outBank) els.outBank.textContent = fmt(phi, 2);
  if (Number.isFinite(rad) && els.outRad) els.outRad.textContent = rad > 0 ? fmt(rad, 2) : 'level';
  if (Number.isFinite(fac) && els.outFac) els.outFac.textContent = fmt(fac, 3);
  if (Number.isFinite(the) && els.outThe) els.outThe.textContent = fmt(the, 2);
  if (els.outMach) {
    const machPar = parval ? Number(parval[idx2(11, IR, 30)]) : Number.NaN;
    const mach = Number.isFinite(machPar) ? machPar : Number(uiState.modelHeader?.mach);
    els.outMach.textContent = Number.isFinite(mach) ? fmtSignedAligned(mach, 3) : '-';
  }
  if (result.WROT) {
    if (els.outPb2v) els.outPb2v.textContent = fmtSignedAligned(result.WROT[0], 3);
    if (els.outQc2v) els.outQc2v.textContent = fmtSignedAligned(result.WROT[1], 3);
    if (els.outRb2v) els.outRb2v.textContent = fmtSignedAligned(result.WROT[2], 3);
  }
  if (result.CFTOT) {
    if (els.outCXtot) els.outCXtot.textContent = fmtSignedAligned(result.CFTOT[0], 5);
    if (els.outCYtot) els.outCYtot.textContent = fmtSignedAligned(result.CFTOT[1], 5);
    if (els.outCZtot) els.outCZtot.textContent = fmtSignedAligned(result.CFTOT[2], 5);
  }
  if (result.CMTOT) {
    if (els.outCltot) els.outCltot.textContent = fmtSignedAligned(result.CMTOT[0], 5);
    if (els.outCmtot) els.outCmtot.textContent = fmtSignedAligned(result.CMTOT[1], 5);
    if (els.outCntot) els.outCntot.textContent = fmtSignedAligned(result.CMTOT[2], 5);
  }
  if (Number.isFinite(result.CDVTOT)) {
    if (els.outCDvis) els.outCDvis.textContent = fmtSignedAligned(result.CDVTOT, 5);
    if (Number.isFinite(result.CDTOT) && els.outCDind) els.outCDind.textContent = fmtSignedAligned(result.CDTOT - result.CDVTOT, 5);
  }
  if (els.outEff) {
    els.outEff.textContent = '-';
    if (Number.isFinite(result.SPANEF)) {
      els.outEff.textContent = fmtSignedAligned(result.SPANEF, 4);
    } else if (Number.isFinite(result.CLTOT) && Number.isFinite(result.CDTOT) && Number.isFinite(result.CDVTOT)) {
      const sref = Number.isFinite(result.SREF) ? result.SREF : Number(uiState.modelHeader?.sref);
      const bref = Number.isFinite(result.BREF) ? result.BREF : Number(uiState.modelHeader?.bref);
      const cdi = result.CDTOT - result.CDVTOT;
      const ar = (Number.isFinite(sref) && Number.isFinite(bref) && sref > 0) ? ((bref * bref) / sref) : null;
      if (ar && cdi > 1e-8) {
        els.outEff.textContent = fmtSignedAligned((result.CLTOT * result.CLTOT) / (Math.PI * ar * cdi), 4);
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
        deflections.push({ name, value: fmtSignedAligned(val, 2) });
      }
    }
    renderControlRows(deflections);
    if (deflections.length && els.outDef) {
      els.outDef.textContent = deflections.map((d) => `${d.name} = ${d.value}`).join('\n');
    }
  }
  scheduleOutputGridFontFit();

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
  updateModeAnimation();
  updateFlowFieldAnimation();
  if (controls) controls.update();
  if (renderer && scene && camera) renderer.render(scene, camera);
}

function handleResize() {
  syncEditorPanelPlacement();
  if (!renderer || !camera || !els.viewer) return;
  const width = els.viewer.clientWidth;
  const height = els.viewer.clientHeight;
  renderer.setSize(width, height);
  camera.aspect = width / height;
  camera.updateProjectionMatrix();
  drawEigenPlot();
}

window.addEventListener('resize', handleResize);

async function bootApp() {
  initTopNav();
  initPanelCollapse();
  renderRunCasesList();
  updateRunCasesMeta();
  renderMassProps(makeDefaultMassProps());
  await loadDefaultAVL();
  syncTemplateParamsFromText(els.fileText.value || '');
  try {
    const model = buildSolverModel(resolveTemplateParamsInText(els.fileText.value || ''));
    rebuildConstraintUI(model);
  } catch {
    rebuildConstraintUI({ controlMap: new Map() });
  }
  await loadDefaultRunAndMass();
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
  drawEigenPlot();
  resetTrimSeed();
  applyTrim({ useSeed: false });
}

bootApp().catch((err) => logDebug(`Boot failed: ${err?.message ?? err}`));
