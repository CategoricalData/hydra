package hydra.lib.math;

public interface Math {
    static Integer neg(Integer num) {
        return (-1 * num);
    }

    static Integer add(Integer augend, Integer addend) {
        return (augend + addend);
    }

    static Integer sub(Integer minuend, Integer subtrahend) {
        return (minuend - subtrahend);
    }

    static Integer mul(Integer multiplier, Integer multiplicand) {
        return (multiplier * multiplicand);
    }

    static Integer div(Integer dividend, Integer divisor) {
        return (dividend / divisor);
    }

    static Integer mod(Integer dividend, Integer divisor) {
        return java.lang.Math.floorMod(dividend, divisor);
    }

    static Integer rem(Integer dividend, Integer divisor) {
        // % in Java is a mathematical remainder, not modulus
        return (dividend % divisor);
    }
}
