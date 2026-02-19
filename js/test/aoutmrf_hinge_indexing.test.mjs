import test from 'node:test';
import assert from 'node:assert/strict';
import { MRFHINGE } from '../src/aoutmrf.js';

test('MRFHINGE reads solver-style zero-based CHINGE array', () => {
  const state = {
    LNASA_SA: false,
    SREF: 1.0,
    CREF: 1.0,
    NCONTROL: 2,
    DNAME: [null, 'aileron', 'elevator'],
    // Solver stores CHINGE in zero-based control slots.
    CHINGE: new Float32Array([0.123, -0.456]),
  };

  const out = MRFHINGE(state, 6);

  const lines = out.split(/\r?\n/).filter((line) => line.includes('Control Hinge Moments'));
  assert.equal(lines.length, 2, 'expected one hinge line per control');

  const nums = lines.map((line) => {
    const m = line.match(/^\s*([-+]?\d+(?:\.\d*)?(?:[eEdD][-+]?\d+)?)/);
    return m ? Number(m[1].replace(/d/i, 'e')) : Number.NaN;
  });

  assert.ok(Number.isFinite(nums[0]) && Number.isFinite(nums[1]), 'failed to parse hinge values');
  assert.ok(Math.abs(nums[0] - 0.123) < 1e-6, `aileron mismatch: ${nums[0]}`);
  assert.ok(Math.abs(nums[1] + 0.456) < 1e-6, `elevator mismatch: ${nums[1]}`);
});
