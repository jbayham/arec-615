# Rootfinding lab

Open `index.html` in a modern browser. It is a self-contained page with no build
step, dependencies, or internet requirement.

Enter an expression in `x`, set the bisection bracket and Newton initial guess,
and select **Build iterations**. Switch methods, then drag the step slider or
use Back, Next, or Play. **Zoom to current step** makes small late-stage changes
visible. Iteration history shows calculations through the selected step.

The default reproduces the `x^2 - 2` example in the NLP notes. Additional examples
show a nonlinear root, a profit first-order condition, a Newton cycle, and a
root with no sign change. Enter a first derivative to solve an optimization FOC;
the page itself solves roots, and does not classify extrema.

Supported syntax: numbers, `x`, `pi`, `e`, `+ - * / ^`, parentheses, and
`sin`, `cos`, `tan`, `exp`, `log`, `ln`, `sqrt`, `abs`, `sinh`, `cosh`, `atan`.
Multiplication must be explicit. Logs are natural; angles are radians.
Expressions use a restricted parser, and derivatives use forward-mode automatic
differentiation rather than finite differences.

Bisection stops when the retained interval width is below the tolerance or a
midpoint evaluates to zero. Continuity remains a user assumption: finite samples
cannot establish it. Newton stops when both the residual and absolute step are
below the tolerance, or a guess evaluates to zero. Domain errors, zero or
undefined derivatives, repeated guesses, and iteration limits are reported.
An initial Newton guess already below the residual tolerance needs no iteration.

To check the parser and algorithms with an existing Node installation:

```sh
node modules/nonlinear_programming/rootfinding/test.mjs
```
