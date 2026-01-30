import fs from 'node:fs/promises';
import path from 'node:path';
import wabtInit from 'wabt';

const __dirname = path.dirname(new URL(import.meta.url).pathname);
const rootDir = path.resolve(__dirname, '..');
const outDir = path.join(rootDir, 'dist');
const wabt = await wabtInit();

async function buildOne(name) {
  const watPath = path.join(__dirname, `${name}.wat`);
  const wasmPath = path.join(outDir, `${name}.wasm`);
  const wat = await fs.readFile(watPath, 'utf8');
  const module = wabt.parseWat(watPath, wat, { features: { mutable_globals: true } });
  const { buffer } = module.toBinary({ write_debug_names: true });
  await fs.mkdir(outDir, { recursive: true });
  await fs.writeFile(wasmPath, Buffer.from(buffer));
  console.log(`Wrote ${wasmPath}`);
}

await buildOne('autil');
await buildOne('aic');
await buildOne('atpforc');
await buildOne('aero');
await buildOne('ba_trans');
await buildOne('cdcl');
await buildOne('vorvel');
await buildOne('vorvelc');
await buildOne('matrix');
await buildOne('matrix-lapacksp');
await buildOne('matrix-lapackdp');
await buildOne('matrix-lapacksubs');
await buildOne('second');
await buildOne('second_g77');
await buildOne('second_ifc');
await buildOne('spline');
await buildOne('sgutil');
await buildOne('airutil');
await buildOne('limits');
await buildOne('limits2');
await buildOne('amass');
await buildOne('atrim');
await buildOne('matrix-linpack');
await buildOne('avl');
await buildOne('asetup');
await buildOne('aoper');
await buildOne('aoutmrf');
await buildOne('amode');
