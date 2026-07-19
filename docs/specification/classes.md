<!-- NOTE: hand-authored spec chapter (not a generated module page). Drafted under #417 with
     the numeric/integral/fractional material contributed by #317's accepted design; the
     functor/monad instance records of #581 will be added when they land. -->

# Constraint classes

Hydra primitives may be polymorphic over a small, closed set of constraint classes.
A constraint appears in a type scheme before the arrow body, as in
`∀t. (ordering t) ⇒ t → t → Comparison`, and is propagated during type inference.
This page specifies each class: its member operations, its instances, and its laws.
Class membership is curated, not open: new classes and new instances are added by
specification change, not by user code.

## The classes

| Class | Member primitives | Instances |
|---|---|---|
| `equality` | `hydra.lib.equality.equal`, `hydra.lib.equality.notEqual` | every Hydra type |
| `ordering` | `hydra.lib.ordering.compare`, `gt`, `gte`, `lt`, `lte`, `max`, `min` | every Hydra type |
| `numeric` | `hydra.lib.math.add`, `sub`, `mul`, `negate`, `abs`, `signum` | the nine integer types (int8, int16, int32, int64, uint8, uint16, uint32, uint64, bigint) and the two floating-point types (float32, float64) |
| `integral` | `hydra.lib.math.div`, `mod`, `rem`, `even`, `odd` | the nine integer types |
| `fractional` | `hydra.lib.math.divide` | float32, float64 |

`equality` and `ordering` are universal: every Hydra value can be tested for structural
equality and compared under the total order.
Their value-level semantics — structural equality and the per-type-constructor total order,
including the extended total order for floating-point values — are specified in
[ordering-and-equality.md](ordering-and-equality.md).
`compare x y` is `equalTo` exactly when `equal x y` is `true`.

The arithmetic classes are not universal, and their instance lists above are exhaustive.
`decimal` has no arithmetic instance: it is a presentation and interchange type, exact
decimal arithmetic is not closed without a precision context (division has non-terminating
expansions, and host carriers disagree on ambient-context defaults), and several targets
carry decimal on a lossy adapted representation.
<!-- Re-open trigger (recorded with the decision): a tracker issue from a concrete consumer
     needing decimal computation (e.g. money aggregation); the design at that point is a
     numeric instance restricted to add/sub/mul/negate (exact, context-free), divide still
     excluded, contingent on the decimal carrier work landing first. -->
The transcendental and rounding functions of `hydra.lib.math` are monomorphic (float64, or
per-width for rounding) and belong to no class.

## Arithmetic semantics by representation class

Four representation classes cover all eleven arithmetic instances:

- **S(n)** — fixed-width signed integers (int8, int16, int32, int64), two's complement,
  width n ∈ {8, 16, 32, 64}.
- **U(n)** — fixed-width unsigned integers (uint8, uint16, uint32, uint64).
- **B** — bigint, arbitrary precision.
- **F(n)** — IEEE 754 binary floating point (float32 = binary32, float64 = binary64).

The normative contract for each member operation, per representation class:

| Op | S(n) | U(n) | B | F(n) |
|---|---|---|---|---|
| `add`, `sub`, `mul` | exact result mod 2^n, reinterpreted signed (silent two's-complement wrap) | exact result mod 2^n (`sub` wraps below 0: 0 − 1 = 2^n − 1) | exact, unbounded | correctly rounded under roundTiesToEven; NaN, ±∞, ±0 per IEEE 754 §5.4.1 |
| `negate` | 0 − x mod 2^n; `negate minBound` = minBound | 2^n − x mod 2^n; `negate 0` = 0 | exact | sign-bit flip (IEEE 754 §5.5.1): `negate ±0.0` = ∓0; `negate NaN` = NaN |
| `abs` | \|x\|, with `abs minBound` = minBound (wrap) | identity | exact | clear sign bit: `abs -0.0` = +0; `abs NaN` = NaN; `abs -∞` = +∞ |
| `signum` | −1, 0, or +1 (same width) | 0 or +1 | −1, 0, or +1 | ±1.0 for nonzero finite and ±∞; `signum ±0.0` = ±0; `signum NaN` = NaN |
| `div` | floor division; `none` iff divisor is 0; `div minBound -1` = `given minBound` (quotient wrap) | floor = truncation (non-negative domain); `none` iff 0 | floor; `none` iff 0 | — (not integral) |
| `mod` | sign follows the divisor (floor division); the invariant `x = div x y · y + mod x y` holds | plain remainder; invariant holds | sign follows the divisor | — |
| `rem` | sign follows the dividend (truncated division); `rem minBound -1` = `given 0` | equal to `mod` (non-negative domain) | sign follows the dividend | — |
| `even`, `odd` | parity of the two's-complement value (`even minBound` is `true`) | parity | parity | — |
| `divide` | — | — | — | IEEE 754 §5.4.1 division, total: x ÷ ±0 = ±∞ for finite nonzero x (sign = XOR of operand signs); ±0 ÷ ±0 = NaN; ±∞ ÷ ±∞ = NaN; NaN propagates |

Cross-cutting laws:

- Ring laws (associativity, commutativity, distributivity) hold exactly for B, and mod 2^n
  for S(n) and U(n).
- `x = div x y · y + mod x y` and `x = quot · y + rem x y` for every integral instance and
  every nonzero `y`.
- `abs x · signum x = x`, except at S(n) minBound.
- For F(n), no algebraic laws hold beyond IEEE 754 itself; in particular addition and
  multiplication are not associative, and specifications must not claim ring laws for
  floating-point instances.

Design notes, briefly: wrap (rather than trap or saturate) is the only fixed-width choice
that is total and preserves ring structure mod 2^n; floor division with divisor-sign `mod`
plus truncated-division `rem` extends the established int32 contract unchanged to all
integer widths; U(n) is the ring Z/2^n with no sign, so `negate` is the additive inverse
and `abs` is the identity; and float `signum` preserving ±0 and NaN (rather than collapsing
to 0) is what keeps `abs x · signum x = x` true for signed zeros.

## No polymorphic constants

The arithmetic classes have member *functions* only: there is no polymorphic `zero`, `one`,
or `fromInteger`.
Hydra literal values are self-typing — the complete type of an atomic value is inferred from
the value itself — so a constant `zero : ∀x. (numeric x) ⇒ x` would require result-driven
instance selection, which Hydra does not have.
Polymorphic numeric code therefore threads identity elements as arguments.

## Conformance surface

The interpreter path — evaluation over tagged terms — is the normative conformance surface
for class semantics, and is width-faithful on every host.
Generated-code arithmetic at types outside a target's declared language constraints follows
the *adapted* carrier's semantics: for example, uint8 arithmetic in generated code on a host
whose adapter widens uint8 to a larger carrier wraps at the carrier's width, not at 2^8.
This is a documented limitation of the adaptation boundary, not a conformance bug; the
cross-host conformance suite exercises the interpreter path.

## Future classes

The functor, applicative, and monad structure that several modules describe informally
(`hydra.lib.lists`, `optionals`, `eithers`, `effects` each note their monadic operations) is
planned to become explicit instance records rather than constraint classes in the above
sense; that design is tracked separately and will be added to this page when it lands.
