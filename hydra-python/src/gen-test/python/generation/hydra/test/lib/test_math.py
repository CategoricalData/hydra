# Note: this is an automatically generated file. Do not edit.
# hydra.lib.math primitives

from __future__ import annotations
from typing import cast
from decimal import Decimal
from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing
import hydra.accessors
import hydra.annotations
import hydra.ast
import hydra.classes
import hydra.coders
import hydra.compute
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.ext.haskell.operators
import hydra.extract.core
import hydra.extract.helpers
import hydra.formatting
import hydra.graph
import hydra.json.model
import hydra.lexical
import hydra.lib.chars
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.names
import hydra.parsing
import hydra.rewriting
import hydra.serialization
import hydra.show.core
import hydra.sorting
import hydra.tarjan
import hydra.testing
import hydra.topology
import hydra.typing
import hydra.util

# abs

def test_abs__positive():

    assert (hydra.lib.math.abs(5)) == (5)

def test_abs__negative():

    assert (hydra.lib.math.abs(-5)) == (5)

def test_abs__zero():

    assert (hydra.lib.math.abs(0)) == (0)

# add

def test_add__positive_numbers():

    assert (hydra.lib.math.add(3, 5)) == (8)

def test_add__negative_numbers():

    assert (hydra.lib.math.add(-3, -5)) == (-8)

def test_add__mixed_sign():

    assert (hydra.lib.math.add(10, -3)) == (7)

def test_add__with_zero():

    assert (hydra.lib.math.add(42, 0)) == (42)

# div

def test_div__exact_division():

    assert (hydra.lib.math.div(10, 2)) == (5)

def test_div__truncates_toward_negative_infinity():

    assert (hydra.lib.math.div(10, 3)) == (3)

def test_div__negative_dividend():

    assert (hydra.lib.math.div(-10, 3)) == (-4)

def test_div__negative_divisor():

    assert (hydra.lib.math.div(10, -3)) == (-4)

# even

def test_even__even_positive():

    assert (hydra.lib.math.even(4)) == (True)

def test_even__odd_positive():

    assert (hydra.lib.math.even(5)) == (False)

def test_even__even_negative():

    assert (hydra.lib.math.even(-4)) == (True)

def test_even__odd_negative():

    assert (hydra.lib.math.even(-5)) == (False)

def test_even__zero():

    assert (hydra.lib.math.even(0)) == (True)

# max

def test_max__first_is_larger():

    assert (hydra.lib.math.max(10, 5)) == (10)

def test_max__second_is_larger():

    assert (hydra.lib.math.max(5, 10)) == (10)

def test_max__equal_values():

    assert (hydra.lib.math.max(7, 7)) == (7)

def test_max__negative_numbers():

    assert (hydra.lib.math.max(-3, -5)) == (-3)

def test_max__mixed_sign():

    assert (hydra.lib.math.max(-5, 5)) == (5)

def test_max__with_zero():

    assert (hydra.lib.math.max(0, 42)) == (42)

# min

def test_min__first_is_smaller():

    assert (hydra.lib.math.min(5, 10)) == (5)

def test_min__second_is_smaller():

    assert (hydra.lib.math.min(10, 5)) == (5)

def test_min__equal_values():

    assert (hydra.lib.math.min(7, 7)) == (7)

def test_min__negative_numbers():

    assert (hydra.lib.math.min(-3, -5)) == (-5)

def test_min__mixed_sign():

    assert (hydra.lib.math.min(-5, 5)) == (-5)

def test_min__with_zero():

    assert (hydra.lib.math.min(0, 42)) == (0)

# mod

def test_mod__basic_modulo():

    assert (hydra.lib.math.mod(10, 3)) == (1)

def test_mod__exact_division():

    assert (hydra.lib.math.mod(10, 2)) == (0)

def test_mod__negative_dividend():

    assert (hydra.lib.math.mod(-10, 3)) == (2)

def test_mod__negative_divisor():

    assert (hydra.lib.math.mod(10, -3)) == (-2)

# mul

def test_mul__positive_numbers():

    assert (hydra.lib.math.mul(3, 5)) == (15)

def test_mul__negative_numbers():

    assert (hydra.lib.math.mul(-3, -5)) == (15)

def test_mul__mixed_sign():

    assert (hydra.lib.math.mul(3, -5)) == (-15)

def test_mul__with_zero():

    assert (hydra.lib.math.mul(42, 0)) == (0)

def test_mul__with_one():

    assert (hydra.lib.math.mul(42, 1)) == (42)

# negate

def test_negate__positive():

    assert (hydra.lib.math.negate(5)) == (-5)

def test_negate__negative():

    assert (hydra.lib.math.negate(-5)) == (5)

def test_negate__zero():

    assert (hydra.lib.math.negate(0)) == (0)

# odd

def test_odd__odd_positive():

    assert (hydra.lib.math.odd(5)) == (True)

def test_odd__even_positive():

    assert (hydra.lib.math.odd(4)) == (False)

def test_odd__odd_negative():

    assert (hydra.lib.math.odd(-5)) == (True)

def test_odd__even_negative():

    assert (hydra.lib.math.odd(-4)) == (False)

def test_odd__zero():

    assert (hydra.lib.math.odd(0)) == (False)

# pred

def test_pred__positive():

    assert (hydra.lib.math.pred(5)) == (4)

def test_pred__zero():

    assert (hydra.lib.math.pred(0)) == (-1)

def test_pred__negative():

    assert (hydra.lib.math.pred(-5)) == (-6)

# range

def test_range__ascending_range():

    assert (hydra.lib.math.range_(1, 5)) == ((1, 2, 3, 4, 5))

def test_range__single_element():

    assert (hydra.lib.math.range_(5, 5)) == ((5,))

def test_range__two_elements():

    assert (hydra.lib.math.range_(3, 4)) == ((3, 4))

def test_range__negative_start():

    assert (hydra.lib.math.range_(-2, 2)) == ((-2, -1, 0, 1, 2))

# rem

def test_rem__basic_remainder():

    assert (hydra.lib.math.rem(10, 3)) == (1)

def test_rem__exact_division():

    assert (hydra.lib.math.rem(10, 2)) == (0)

def test_rem__negative_dividend():

    assert (hydra.lib.math.rem(-10, 3)) == (-1)

def test_rem__negative_divisor():

    assert (hydra.lib.math.rem(10, -3)) == (1)

# signum

def test_signum__positive():

    assert (hydra.lib.math.signum(5)) == (1)

def test_signum__negative():

    assert (hydra.lib.math.signum(-5)) == (-1)

def test_signum__zero():

    assert (hydra.lib.math.signum(0)) == (0)

# sub

def test_sub__positive_numbers():

    assert (hydra.lib.math.sub(10, 3)) == (7)

def test_sub__negative_numbers():

    assert (hydra.lib.math.sub(-10, -3)) == (-7)

def test_sub__mixed_sign():

    assert (hydra.lib.math.sub(10, -3)) == (13)

def test_sub__with_zero():

    assert (hydra.lib.math.sub(42, 0)) == (42)

# succ

def test_succ__positive():

    assert (hydra.lib.math.succ(5)) == (6)

def test_succ__zero():

    assert (hydra.lib.math.succ(0)) == (1)

def test_succ__negative():

    assert (hydra.lib.math.succ(-5)) == (-4)

# e

def test_e__euler_s_number():

    assert (hydra.lib.math.e()) == (2.718281828459045)

# pi

def test_pi__pi_constant():

    assert (hydra.lib.math.pi()) == (3.141592653589793)

# sin

def test_sin__sin_0():

    assert (hydra.lib.math.sin(0.0)) == (0.0)

def test_sin__sin_pi_2():

    assert (hydra.lib.math.sin(1.5707963267948966)) == (1.0)

def test_sin__sin_pi():

    assert (hydra.lib.math.sin(3.141592653589793)) == (1.2246467991473532e-16)

# cos

def test_cos__cos_0():

    assert (hydra.lib.math.cos(0.0)) == (1.0)

def test_cos__cos_pi_2():

    assert (hydra.lib.math.cos(1.5707963267948966)) == (6.123233995736766e-17)

def test_cos__cos_pi():

    assert (hydra.lib.math.cos(3.141592653589793)) == (-1.0)

# tan

def test_tan__tan_0():

    assert (hydra.lib.math.tan(0.0)) == (0.0)

def test_tan__tan_pi_4():

    assert (hydra.lib.math.tan(0.7853981633974483)) == (0.9999999999999999)

# asin

def test_asin__asin_0():

    assert (hydra.lib.math.asin(0.0)) == (0.0)

def test_asin__asin_1():

    assert (hydra.lib.math.asin(1.0)) == (1.5707963267948966)

def test_asin__asin__1():

    assert (hydra.lib.math.asin(-1.0)) == (-1.5707963267948966)

# acos

def test_acos__acos_1():

    assert (hydra.lib.math.acos(1.0)) == (0.0)

def test_acos__acos_0():

    assert (hydra.lib.math.acos(0.0)) == (1.5707963267948966)

def test_acos__acos__1():

    assert (hydra.lib.math.acos(-1.0)) == (3.141592653589793)

# atan

def test_atan__atan_0():

    assert (hydra.lib.math.atan(0.0)) == (0.0)

def test_atan__atan_1():

    assert (hydra.lib.math.atan(1.0)) == (0.7853981633974483)

# atan2

def test_atan2__atan2_1_1():

    assert (hydra.lib.math.atan2(1.0, 1.0)) == (0.7853981633974483)

def test_atan2__atan2_1_0():

    assert (hydra.lib.math.atan2(1.0, 0.0)) == (1.5707963267948966)

def test_atan2__atan2_0_1():

    assert (hydra.lib.math.atan2(0.0, 1.0)) == (0.0)

# sinh

def test_sinh__sinh_0():

    assert (hydra.lib.math.sinh(0.0)) == (0.0)

def test_sinh__sinh_1():

    assert (hydra.lib.math.sinh(1.0)) == (1.1752011936438014)

# cosh

def test_cosh__cosh_0():

    assert (hydra.lib.math.cosh(0.0)) == (1.0)

def test_cosh__cosh_1():

    assert (hydra.lib.math.cosh(1.0)) == (1.5430806348152437)

# tanh

def test_tanh__tanh_0():

    assert (hydra.lib.math.tanh(0.0)) == (0.0)

def test_tanh__tanh_1():

    assert (hydra.lib.math.tanh(1.0)) == (0.7615941559557649)

# asinh

def test_asinh__asinh_0():

    assert (hydra.lib.math.asinh(0.0)) == (0.0)

def test_asinh__asinh_1():

    assert (hydra.lib.math.asinh(1.0)) == (0.881373587019543)

# acosh

def test_acosh__acosh_1():

    assert (hydra.lib.math.acosh(1.0)) == (0.0)

def test_acosh__acosh_2():

    assert (hydra.lib.math.acosh(2.0)) == (1.3169578969248166)

# atanh

def test_atanh__atanh_0():

    assert (hydra.lib.math.atanh(0.0)) == (0.0)

def test_atanh__atanh_0_5():

    assert (hydra.lib.math.atanh(0.5)) == (0.5493061443340549)

# exp

def test_exp__exp_0():

    assert (hydra.lib.math.exp(0.0)) == (1.0)

def test_exp__exp_1():

    assert (hydra.lib.math.exp(1.0)) == (2.718281828459045)

def test_exp__exp__1():

    assert (hydra.lib.math.exp(-1.0)) == (0.36787944117144233)

# log

def test_log__log_1():

    assert (hydra.lib.math.log(1.0)) == (0.0)

def test_log__log_e():

    assert (hydra.lib.math.log(2.718281828459045)) == (1.0)

# logBase

def test_logbase__log10_1():

    assert (hydra.lib.math.log_base(10.0, 1.0)) == (0.0)

def test_logbase__log10_10():

    assert (hydra.lib.math.log_base(10.0, 10.0)) == (1.0)

def test_logbase__log10_100():

    assert (hydra.lib.math.log_base(10.0, 100.0)) == (2.0)

def test_logbase__log2_8():

    assert (hydra.lib.math.log_base(2.0, 8.0)) == (3.0)

# pow

def test_pow__2_3():

    assert (hydra.lib.math.pow(2.0, 3.0)) == (8.0)

def test_pow__10_0():

    assert (hydra.lib.math.pow(10.0, 0.0)) == (1.0)

def test_pow__2__1():

    assert (hydra.lib.math.pow(2.0, -1.0)) == (0.5)

# sqrt

def test_sqrt__sqrt_4():

    assert (hydra.lib.math.sqrt(4.0)) == (2.0)

def test_sqrt__sqrt_9():

    assert (hydra.lib.math.sqrt(9.0)) == (3.0)

def test_sqrt__sqrt_2():

    assert (hydra.lib.math.sqrt(2.0)) == (1.4142135623730951)

def test_sqrt__sqrt_0():

    assert (hydra.lib.math.sqrt(0.0)) == (0.0)

# ceiling

def test_ceiling__ceiling_3_2():

    assert (hydra.lib.math.ceiling(3.2)) == (4)

def test_ceiling__ceiling_3_0():

    assert (hydra.lib.math.ceiling(3.0)) == (3)

def test_ceiling__ceiling__3_2():

    assert (hydra.lib.math.ceiling(-3.2)) == (-3)

def test_ceiling__ceiling__3_0():

    assert (hydra.lib.math.ceiling(-3.0)) == (-3)

# floor

def test_floor__floor_3_8():

    assert (hydra.lib.math.floor(3.8)) == (3)

def test_floor__floor_3_0():

    assert (hydra.lib.math.floor(3.0)) == (3)

def test_floor__floor__3_2():

    assert (hydra.lib.math.floor(-3.2)) == (-4)

def test_floor__floor__3_0():

    assert (hydra.lib.math.floor(-3.0)) == (-3)

# round

def test_round__round_3_4():

    assert (hydra.lib.math.round(3.4)) == (3)

def test_round__round_3_5():

    assert (hydra.lib.math.round(3.5)) == (4)

def test_round__round_3_6():

    assert (hydra.lib.math.round(3.6)) == (4)

def test_round__round__3_4():

    assert (hydra.lib.math.round(-3.4)) == (-3)

def test_round__round__3_5():

    assert (hydra.lib.math.round(-3.5)) == (-4)

# truncate

def test_truncate__truncate_3_8():

    assert (hydra.lib.math.truncate(3.8)) == (3)

def test_truncate__truncate_3_2():

    assert (hydra.lib.math.truncate(3.2)) == (3)

def test_truncate__truncate__3_8():

    assert (hydra.lib.math.truncate(-3.8)) == (-3)

def test_truncate__truncate__3_2():

    assert (hydra.lib.math.truncate(-3.2)) == (-3)
