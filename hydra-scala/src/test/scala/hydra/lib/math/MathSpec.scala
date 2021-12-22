package hydra.lib.math

import hydra.HydraSpecBase

class MathSpec extends HydraSpecBase {
  "neg" should exp in {
    assert(neg(42) === -42)
    assert(neg(0) === 0)
    assert(neg(-1) === 1)
  }

  "add" should exp in {
    assert(add(1, 2) === 3)
    assert(add(2, 1) === 3)
    assert(add(0, 42) === 42)
  }

  "sub" should exp in {
    assert(sub(10, 42) === -32)
    assert(sub(4, 0) === 4)
  }

  "mul" should exp in {
    assert(mul(2, 4) === 8)
    assert(mul(0, 1) === 0)
    assert(mul(42, 0) === 0)
    assert(mul(-2, 4) === -8)
  }

  "div" should exp in {
    // Note: divide-by-zero is undefined
    assert(div(4, 2) === 2)
    assert(div(5, 2) === 2)
    assert(div(42, 1) === 42)
    assert(div(-5, 2) === -2)
    assert(div(-5, -2) === 2)
    assert(div(2, 5) == 0)
  }

  "mod" should exp in {
    // Note: mod-by-zero is undefined
    assert(mod(4, 2) === 0)
    assert(mod(11, 7) === 4)
    assert(mod(-11, 7) === 3)
    assert(mod(11, -7) === -3)
    assert(mod(-11, -7) === -4)
  }

  "rem" should exp in {
    // Note: rem-by-zero is undefined
    assert(rem(4, 2) === 0)
    assert(rem(11, 7) === 4)
    assert(rem(-11, 7) === -4)
    assert(rem(11, -7) === 4)
    assert(rem(-11, -7) === -4)
  }
}
