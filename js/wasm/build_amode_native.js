import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { execFileSync } from 'node:child_process';

const __dirname = path.dirname(new URL(import.meta.url).pathname);
const jsRoot = path.resolve(__dirname, '..');
const repoRoot = path.resolve(jsRoot, '..');
const avlRoot = path.resolve(repoRoot, 'third_party', 'avl');
const amodeFile = path.join(avlRoot, 'src', 'amode.f');
const autilFile = path.join(avlRoot, 'src', 'autil.f');
const eispackFile = path.join(avlRoot, 'eispack', 'eispack.f');
const includeDir = path.join(avlRoot, 'src');
const distOut = path.join(jsRoot, 'dist', 'amode.wasm');

const F2C_HEADER = `#ifndef F2C_H
#define F2C_H

#include <stdint.h>

typedef int32_t integer;
typedef uint32_t uinteger;
typedef int16_t shortint;
typedef uint16_t ushortint;
typedef int8_t logical1;
typedef int32_t logical;
typedef float real;
typedef double doublereal;
typedef int32_t ftnlen;
typedef int32_t ftnint;
typedef int8_t integer1;
typedef uint8_t uinteger1;
typedef long long longint;
typedef unsigned long long ulongint;

typedef struct { real r, i; } complex;
typedef struct { doublereal r, i; } doublecomplex;

typedef int32_t flag;
typedef int32_t ftnlogical;

typedef struct { integer cierr; integer ciunit; integer ciend; const char *cifmt; integer cirec; } cilist;
typedef struct { integer icierr; const char *iciunit; integer iciend; const char *icifmt; integer icirlen; integer icirnum; } icilist;
typedef struct { integer oerr; integer ounit; integer ofnmlen; const char *ofnm; integer orl; integer osta; integer oacc; integer ofm; integer oblnk; } olist;
typedef struct { integer cerr; integer cunit; integer csta; } cllist;
typedef struct { integer aerr; integer aunit; } alist;
typedef struct { integer inerr; integer inunit; const char *infile; integer inex; integer inopen; integer innum; integer innamed; const char *inname; integer inacc; integer inseq; integer indir; const char *infmt; integer inform; integer inunf; integer inrecl; integer innrec; const char *inblank; } inlist;

#define TRUE_ (1)
#define FALSE_ (0)

#ifndef Extern
#define Extern extern
#endif
#ifndef Void
#define Void void
#endif
#ifndef VOID
#define VOID void
#endif

#ifndef abs
#define abs(x) ((x) >= 0 ? (x) : -(x))
#endif
#ifndef min
#define min(a,b) ((a) <= (b) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) ((a) >= (b) ? (a) : (b))
#endif

extern double sin(double);
extern double cos(double);
extern double tan(double);
extern double sqrt(double);
extern double log(double);
extern double exp(double);
#endif
`;

const BRIDGE_SOURCE = `#include "f2c.h"
#include <stdint.h>

#include "amode_core.c"
#include "autil.c"
#include "eispack.c"

static inline int32_t p2i(const void *p) { return (int32_t)(uintptr_t)p; }

__attribute__((visibility("default"))) int32_t AMODE_ptr_case_i_icon(void) { return p2i(&case_i__1.icon[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_i_nvtot(void) { return p2i(&case_i__1.nvtot); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_i_ncontrol(void) { return p2i(&case_i__1.ncontrol); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_i_neigen(void) { return p2i(&case_i__1.neigen[0]); }

__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_parval(void) { return p2i(&case_r__1.parval[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_vinf(void) { return p2i(&case_r__1.vinf[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_wrot(void) { return p2i(&case_r__1.wrot[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_cftot(void) { return p2i(&case_r__1.cftot[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_cmtot(void) { return p2i(&case_r__1.cmtot[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_cftot_u(void) { return p2i(&case_r__1.cftot_u__[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_cmtot_u(void) { return p2i(&case_r__1.cmtot_u__[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_cftot_d(void) { return p2i(&case_r__1.cftot_d__[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_cmtot_d(void) { return p2i(&case_r__1.cmtot_d__[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_sref(void) { return p2i(&case_r__1.sref); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_cref(void) { return p2i(&case_r__1.cref); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_bref(void) { return p2i(&case_r__1.bref); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_r_dtr(void) { return p2i(&case_r__1.dtr); }

__attribute__((visibility("default"))) int32_t AMODE_ptr_un_r_unitl(void) { return p2i(&un_r__1.unitl); }

__attribute__((visibility("default"))) int32_t AMODE_ptr_mass_r_amass(void) { return p2i(&mass_r__1.amass[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_mass_r_ainer(void) { return p2i(&mass_r__1.ainer[0]); }

__attribute__((visibility("default"))) int32_t AMODE_ptr_case_z_eval(void) { return p2i(&case_z__1.eval[0]); }
__attribute__((visibility("default"))) int32_t AMODE_ptr_case_z_evec(void) { return p2i(&case_z__1.evec[0]); }

__attribute__((visibility("default"))) int32_t AMODE_runchk(int32_t jrun) {
  integer jr = (integer)jrun;
  logical ok = TRUE_;
  runchk_(&jr, &ok);
  return (int32_t)ok;
}

__attribute__((visibility("default"))) int32_t AMODE_sysmat(int32_t ir, int32_t asys_ptr, int32_t bsys_ptr, int32_t rsys_ptr) {
  integer iir = (integer)ir;
  integer nsys = 0;
  sysmat_(&iir, (doublereal*)(uintptr_t)asys_ptr, (doublereal*)(uintptr_t)bsys_ptr, (doublereal*)(uintptr_t)rsys_ptr, &nsys);
  return (int32_t)nsys;
}

__attribute__((visibility("default"))) int32_t AMODE_appmat(int32_t ir, int32_t asys_ptr, int32_t bsys_ptr, int32_t rsys_ptr) {
  integer iir = (integer)ir;
  integer nsys = 0;
  appmat_(&iir, (doublereal*)(uintptr_t)asys_ptr, (doublereal*)(uintptr_t)bsys_ptr, (doublereal*)(uintptr_t)rsys_ptr, &nsys);
  return (int32_t)nsys;
}

__attribute__((visibility("default"))) int32_t AMODE_eigsol(int32_t ir, float etol, int32_t asys_ptr, int32_t nsys) {
  integer info = 0;
  integer iir = (integer)ir;
  integer nsysi = (integer)nsys;
  eigsol_(&info, &iir, &etol, (doublereal*)(uintptr_t)asys_ptr, &nsysi);
  return (int32_t)info;
}
`;

function extractSubroutine(lines, name) {
  const startRe = new RegExp(`^\\s*SUBROUTINE\\s+${name}\\s*\\(`, 'i');
  const endRe = /^\s*END(?:\s*!.*)?\s*$/i;
  let start = -1;
  for (let i = 0; i < lines.length; i += 1) {
    if (startRe.test(lines[i])) {
      start = i;
      break;
    }
  }
  if (start < 0) {
    throw new Error(`Could not find SUBROUTINE ${name} in amode.f`);
  }
  let end = -1;
  for (let i = start + 1; i < lines.length; i += 1) {
    if (endRe.test(lines[i])) {
      end = i;
      break;
    }
  }
  if (end < 0) {
    throw new Error(`Could not find END for SUBROUTINE ${name}`);
  }
  return `${lines.slice(start, end + 1).join('\n')}\n`;
}

function run(cmd, args, cwd) {
  execFileSync(cmd, args, { cwd, stdio: 'pipe' });
}

async function main() {
  const tmp = await fs.mkdtemp(path.join(os.tmpdir(), 'amode-native-'));
  try {
    const src = await fs.readFile(amodeFile, 'utf8');
    const lines = src.split(/\r?\n/);
    const kernels = [
      extractSubroutine(lines, 'RUNCHK'),
      extractSubroutine(lines, 'SYSMAT'),
      extractSubroutine(lines, 'APPMAT'),
      extractSubroutine(lines, 'EIGSOL'),
    ];
    await fs.writeFile(path.join(tmp, 'amode_core.f'), kernels.join('\n'));
    await fs.copyFile(autilFile, path.join(tmp, 'autil.f'));
    await fs.copyFile(eispackFile, path.join(tmp, 'eispack.f'));
    const incFiles = await fs.readdir(includeDir);
    for (const name of incFiles) {
      if (name.endsWith('.INC')) {
        await fs.copyFile(path.join(includeDir, name), path.join(tmp, name));
      }
    }
    await fs.writeFile(path.join(tmp, 'f2c.h'), F2C_HEADER);
    await fs.writeFile(path.join(tmp, 'amode_bridge.c'), BRIDGE_SOURCE);

    run('f2c', ['-A', '-R', 'amode_core.f', 'autil.f', 'eispack.f'], tmp);
    run('clang', [
      '--target=wasm32',
      '-O2',
      '-I.',
      '-nostdlib',
      '-Wl,--no-entry',
      '-Wl,--export-all',
      '-Wl,--allow-undefined',
      '-o',
      distOut,
      'amode_bridge.c',
    ], tmp);
    console.log(`Wrote ${distOut}`);
  } finally {
    await fs.rm(tmp, { recursive: true, force: true });
  }
}

main().catch((err) => {
  console.error(`amode native build failed: ${err?.message ?? err}`);
  process.exit(1);
});
