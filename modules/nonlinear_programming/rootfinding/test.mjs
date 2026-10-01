import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import vm from 'node:vm';

const html = await readFile(new URL('./index.html', import.meta.url), 'utf8');
const source = html.match(/<script id="math-core">([\s\S]*?)<\/script>/)[1];
const { parse, bisection, newton } = vm.runInNewContext(source + '\nRootfinding');
const near = (a, b, tolerance = 1e-12) => assert.ok(Math.abs(a - b) < tolerance, `${a} ≈ ${b}`);

const f = parse('x^2 - 2');
const bis = bisection(f, 1, 2, 1e-6, 30);
assert.deepEqual(Array.from(bis.frames.slice(1, 4), row => row.c), [1.5, 1.25, 1.375]);
assert.ok(bis.ok);
assert.ok(bis.frames.at(-1).nb - bis.frames.at(-1).na < 1e-6);
for (const row of bis.frames.slice(1)) {
  assert.ok(row.na <= Math.SQRT2 && Math.SQRT2 <= row.nb);
  near(row.nb - row.na, (row.b - row.a) / 2);
}
const nr = newton(f, 1, 1e-6, 30);
near(nr.frames[1].nx, 1.5);
near(nr.frames[2].nx, 17 / 12);
near(nr.frames.at(-1).nx, Math.SQRT2);
assert.ok(nr.ok);

// Reverse the endpoint signs without changing the root or retained brackets.
const reversed = bisection(parse('2 - x^2'), 1, 2, 1e-6, 30);
assert.deepEqual(Array.from(reversed.frames.slice(1), s => s.c), Array.from(bis.frames.slice(1), s => s.c));
assert.ok(bisection(parse('x - 1'), 1, 2, 1e-6, 30).ok);
assert.ok(bisection(parse('x'), -1, 1, 1e-6, 30).ok);
assert.equal(bisection(parse('(x - 1)^2'), 0, 3, 1e-6, 30).ok, false);
assert.match(bisection(parse('1/x'), -1, 1, 1e-6, 30).message, /undefined/);
assert.match(newton(f, 0, 1e-6, 30).message, /derivative/);
assert.match(newton(parse('x^3 - 2*x + 2'), 0, 1e-6, 30).message, /cycling/);
assert.match(newton(parse('log(x)'), -1, 1e-6, 30).message, /domain/);
assert.match(newton(parse('log(x)'), 10, 1e-6, 30).message, /domain/);
assert.match(newton(parse('abs(x)+1'), 0, 1e-6, 30).message, /derivative/);
assert.match(newton(f, 1, 1e-14, 1).message, /limit/);
assert.match(bisection(f, 1, 2, 1e-14, 1).message, /limit/);
assert.ok(newton(parse('x-1'), 1, 1e-6, 30).ok);

// Mathematical precedence and derivatives, including negative bases.
near(parse('-x^2')(-2)[0], -4);
near(parse('2^3^2')(0)[0], 512);
near(parse('x^-2')(2)[0], 0.25);
near(parse('x^3')(-2)[1], 12);
near(parse('x^0')(0)[1], 0);
near(parse('exp(-x)')(1)[1], -Math.exp(-1));
near(parse('x^x')(2)[1], 4 * (Math.log(2) + 1));
near(parse('sin(pi/2) + ln(e)')(0)[0], 2);
for (const name of ['sin','cos','tan','exp','log','ln','sqrt','abs','sinh','cosh','atan']) {
  const fn = parse(`${name}(x)`), x = 0.7, h = 1e-5;
  near(fn(x)[1], (fn(x + h)[0] - fn(x - h)[0]) / (2 * h), 1e-8);
}
for (const invalid of ['', 'alert(1)', 'x;1', '2x', 'x=1', 'x.constructor', 'sin x', '1e999', '(x+1', 'x)']) {
  assert.throws(() => parse(invalid));
}
console.log('Parser, derivatives, note examples, bracketing invariants, and failure cases passed.');
