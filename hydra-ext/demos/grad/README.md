# Automatic differentiation demo

A demo that performs source-to-source automatic differentiation on Hydra terms.
Mathematical functions are built as typed lambda calculus terms, differentiated symbolically,
and evaluated numerically — all within Hydra's cross-platform term representation.

## Quick start

```bash
cd hydra-ext/demos/grad
./bin/run.sh
```

## What it demonstrates

- **Symbolic differentiation**: transforms a `Term` into its derivative `Term` using calculus rules
  (chain rule, product rule, power rule, transcendental derivatives)
- **Term-to-term transformation**: the derivative is a new Hydra term, not a numeric approximation
- **Evaluation**: derivatives are evaluated at specific points via `reduceTerm`
- **Gradient checking**: AD results are verified against central finite differences

## Functions differentiated

| Function | Derivative | Rules exercised |
|----------|-----------|-----------------|
| x^2 | 2x | product rule (x*x) |
| x^3 | 3x^2 | general power rule |
| sin(x) | cos(x) | trig + chain rule |
| exp(x) | exp(x) | exponential |
| log(x) | 1/x | logarithmic |
| sqrt(x) | 1/(2*sqrt(x)) | square root |
| sin(cos(x)) | cos(cos(x))*(-sin(x)) | nested chain rule |
| x*sin(x) | sin(x) + x*cos(x) | product rule + trig |
| exp(x^2) | 2x*exp(x^2) | chain rule + product |

## How it works

The `hydra.differentiation` module implements differentiation as a recursive function
`differentiateTerm :: Name -> Term -> Term` that pattern-matches on all 18 Hydra term
constructors and applies standard calculus rules.
Derivative rules are provided for all differentiable Float64 primitives
(sin, cos, exp, log, sqrt, pow, and the hyperbolic/inverse trig functions).

The derivative itself is a Hydra `Term` — it can be code-generated to any target language
(Java, Python, Scala, Lisp) and will produce byte-identical results across all platforms.

## Output

The demo writes per-function derivative term files to the output directory,
plus a `summary.txt` with overall results.
