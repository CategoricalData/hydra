"""Confirms and_/or_'s second operand (#802) is a genuine short-circuit, not just accepted."""

from hydra.core.overlay.python.lib.logic import and_, or_


def _exploding():
    def thunk():
        raise AssertionError("second operand was forced despite being short-circuited")
    return thunk


def test_and_short_circuits_on_false():
    assert and_(False, _exploding()) is False


def test_and_evaluates_second_operand_when_first_is_true():
    assert and_(True, lambda: True) is True
    assert and_(True, lambda: False) is False


def test_or_short_circuits_on_true():
    assert or_(True, _exploding()) is True


def test_or_evaluates_second_operand_when_first_is_false():
    assert or_(False, lambda: True) is True
    assert or_(False, lambda: False) is False


def test_and_or_still_accept_plain_bool_second_operand():
    assert and_(True, True) is True
    assert or_(False, False) is False
