/*
 * Ported from AVL Fortran source by Mark Drela and Harold Youngren.
 * Derived work under GPL-2.0.
 * Original source: https://web.mit.edu/drela/Public/web/avl/
 */

const KEYWORD_PREFIXES = new Set([
  'SURF',
  'SECT',
  'BODY',
  'COMP',
  'YDUP',
  'SCAL',
  'TRAN',
  'ANGL',
  'NOWA',
  'NOLO',
  'NACA',
  'AFIL',
  'AIRF',
  'CONT',
]);

export function normalizeAirfoilPath(rawPath) {
  const raw = String(rawPath || '').trim();
  if (!raw) return '';
  return raw.replace(/^["']+|["']+$/g, '').trim();
}

export function stripInlineComment(line) {
  const text = String(line || '');
  const hashIdx = text.indexOf('#');
  const bangIdx = text.indexOf('!');
  let cutIdx = -1;
  if (hashIdx >= 0) cutIdx = hashIdx;
  if (bangIdx >= 0 && (cutIdx < 0 || bangIdx < cutIdx)) cutIdx = bangIdx;
  return cutIdx >= 0 ? text.slice(0, cutIdx) : text;
}

export function parseNumbers(line) {
  const clean = stripInlineComment(line).trim();
  if (!clean) return [];
  return clean
    .split(/\s+/)
    .map((v) => Number(v))
    .filter((v) => Number.isFinite(v));
}

function isCommentLine(line) {
  const t = String(line || '').trim();
  return !t || t.startsWith('#') || t.startsWith('!') || t.startsWith('%');
}

function isKeywordLine(line) {
  const t = stripInlineComment(line).trim().toUpperCase();
  if (!t) return false;
  return KEYWORD_PREFIXES.has(t.slice(0, 4));
}

function parseControlLine(raw, readNextLine) {
  let parts = raw.split(/\s+/).slice(1);
  if (parts.length < 2) {
    const next = readNextLine() || '';
    parts = next.trim().split(/\s+/);
  }
  const name = parts.shift() || 'CTRL';
  const nums = parts.map((v) => Number(v)).filter((v) => Number.isFinite(v));
  return {
    name,
    gain: nums[0] ?? 1.0,
    xhinge: nums[1] ?? 0.75,
    vhinge: [nums[2] ?? 0.0, nums[3] ?? 0.0, nums[4] ?? 0.0],
    sgnDup: nums[5] ?? 1.0,
  };
}

function parseAirfoilBlock(lines, indexRef) {
  const coords = [];
  while (indexRef.i < lines.length) {
    const raw = lines[indexRef.i];
    if (!raw) {
      indexRef.i += 1;
      continue;
    }
    const trimmed = raw.trim();
    if (!trimmed) {
      indexRef.i += 1;
      if (coords.length) break;
      continue;
    }
    if (isKeywordLine(trimmed)) break;
    indexRef.i += 1;
    if (isCommentLine(trimmed)) continue;
    const nums = parseNumbers(trimmed);
    if (nums.length >= 2) coords.push([nums[0], nums[1]]);
  }
  return coords;
}

export function parseAVL(text) {
  const lines = String(text || '').split(/\r?\n/);
  const surfaces = [];
  const inlineAirfoils = [];
  const indexRef = { i: 0 };

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

  const nextLine = () => {
    while (indexRef.i < lines.length && isCommentLine(lines[indexRef.i])) indexRef.i += 1;
    if (indexRef.i >= lines.length) return null;
    return stripInlineComment(lines[indexRef.i++]).trim();
  };

  const readValueLine = () => parseNumbers(nextLine());

  const titleLine = nextLine();
  if (titleLine) header.title = titleLine;
  const machLine = readValueLine();
  if (machLine.length) header.mach = machLine[0];
  const symLine = readValueLine();
  if (symLine.length >= 3) {
    header.iysym = symLine[0];
    header.izsym = symLine[1];
    header.zsym = symLine[2];
  }
  const refLine = readValueLine();
  if (refLine.length >= 3) {
    header.sref = refLine[0];
    header.cref = refLine[1];
    header.bref = refLine[2];
  }
  const xyzLine = readValueLine();
  if (xyzLine.length >= 3) {
    header.xref = xyzLine[0];
    header.yref = xyzLine[1];
    header.zref = xyzLine[2];
  }

  while (indexRef.i < lines.length) {
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
      angleDeg: 0.0,
      component: 0,
      lvalbe: true,
    };

    let currentSection = null;
    while (indexRef.i < lines.length) {
      const mark = lines[indexRef.i] ?? '';
      const trimmed = mark.trim();
      if (!trimmed || isCommentLine(trimmed)) {
        indexRef.i += 1;
        continue;
      }
      const subkey = trimmed.slice(0, 4).toUpperCase();
      if (subkey === 'SURF' || subkey === 'BODY') break;
      indexRef.i += 1;

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
      } else if (subkey === 'SECT') {
        const nums = parseNumbers(trimmed.slice(4));
        const vals = nums.length >= 5 ? nums : readValueLine();
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
        if (!currentSection) continue;
        const code = stripInlineComment(trimmed.slice(4)).trim() || (nextLine() || '');
        const match = code.match(/(\d{4})/);
        currentSection.naca = match ? match[1] : null;
      } else if (subkey === 'AFIL') {
        if (!currentSection) continue;
        const parts = stripInlineComment(trimmed).split(/\s+/);
        let pathValue = parts.length > 1 ? parts.slice(1).join(' ') : '';
        if (!pathValue) pathValue = nextLine() || '';
        const normalized = normalizeAirfoilPath(pathValue);
        currentSection.airfoilFile = normalized || null;
        if (normalized) inlineAirfoils.push(normalized);
      } else if (subkey === 'AIRF') {
        if (!currentSection) continue;
        currentSection.airfoilCoords = parseAirfoilBlock(lines, indexRef);
      } else if (subkey === 'CONT') {
        if (!currentSection) continue;
        currentSection.controls.push(parseControlLine(trimmed, nextLine));
      }
    }
    surfaces.push(surface);
  }

  return { header, surfaces, airfoilFiles: inlineAirfoils };
}

