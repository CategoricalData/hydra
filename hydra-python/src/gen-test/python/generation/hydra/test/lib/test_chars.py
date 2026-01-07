# Note: this is an automatically generated file. Do not edit.
# hydra.lib.chars primitives

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
import hydra.json
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

# isAlphaNum

def test_isalphanum__letter():

    assert (hydra.lib.chars.is_alpha_num(97)) == (True)

def test_isalphanum__digit():

    assert (hydra.lib.chars.is_alpha_num(53)) == (True)

def test_isalphanum__space():

    assert (hydra.lib.chars.is_alpha_num(32)) == (False)

def test_isalphanum__punctuation():

    assert (hydra.lib.chars.is_alpha_num(46)) == (False)

# isLower

def test_islower__lowercase():

    assert (hydra.lib.chars.is_lower(97)) == (True)

def test_islower__uppercase():

    assert (hydra.lib.chars.is_lower(65)) == (False)

def test_islower__digit():

    assert (hydra.lib.chars.is_lower(53)) == (False)

# isSpace

def test_isspace__space():

    assert (hydra.lib.chars.is_space(32)) == (True)

def test_isspace__tab():

    assert (hydra.lib.chars.is_space(9)) == (True)

def test_isspace__newline():

    assert (hydra.lib.chars.is_space(10)) == (True)

def test_isspace__letter():

    assert (hydra.lib.chars.is_space(97)) == (False)

# isUpper

def test_isupper__uppercase():

    assert (hydra.lib.chars.is_upper(65)) == (True)

def test_isupper__lowercase():

    assert (hydra.lib.chars.is_upper(97)) == (False)

def test_isupper__digit():

    assert (hydra.lib.chars.is_upper(53)) == (False)

# toLower

def test_tolower__uppercase():

    assert (hydra.lib.chars.to_lower(65)) == (97)

def test_tolower__lowercase():

    assert (hydra.lib.chars.to_lower(97)) == (97)

def test_tolower__digit():

    assert (hydra.lib.chars.to_lower(53)) == (53)

# toUpper

def test_toupper__lowercase():

    assert (hydra.lib.chars.to_upper(97)) == (65)

def test_toupper__uppercase():

    assert (hydra.lib.chars.to_upper(65)) == (65)

def test_toupper__digit():

    assert (hydra.lib.chars.to_upper(53)) == (53)
