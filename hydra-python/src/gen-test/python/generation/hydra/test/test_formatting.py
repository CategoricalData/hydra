# Note: this is an automatically generated file. Do not edit.
# formatting

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

# case conversion

def test_case_conversion___1__lower_snake_case____upper_snake_case_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.LOWER_SNAKE, hydra.util.CaseConvention.UPPER_SNAKE, "a_hello_world_42_a42_42a_b")) == ("A_HELLO_WORLD_42_A42_42A_B")

def test_case_conversion___2__lower_snake_case____camelcase_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.LOWER_SNAKE, hydra.util.CaseConvention.CAMEL, "a_hello_world_42_a42_42a_b")) == ("aHelloWorld42A4242aB")

def test_case_conversion___3__lower_snake_case____pascalcase_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.LOWER_SNAKE, hydra.util.CaseConvention.PASCAL, "a_hello_world_42_a42_42a_b")) == ("AHelloWorld42A4242aB")

def test_case_conversion___4__lower_snake_case____lower_snake_case_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.LOWER_SNAKE, hydra.util.CaseConvention.LOWER_SNAKE, "a_hello_world_42_a42_42a_b")) == ("a_hello_world_42_a42_42a_b")

def test_case_conversion___5__upper_snake_case____lower_snake_case_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.UPPER_SNAKE, hydra.util.CaseConvention.LOWER_SNAKE, "A_HELLO_WORLD_42_A42_42A_B")) == ("a_hello_world_42_a42_42a_b")

def test_case_conversion___6__upper_snake_case____camelcase_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.UPPER_SNAKE, hydra.util.CaseConvention.CAMEL, "A_HELLO_WORLD_42_A42_42A_B")) == ("aHelloWorld42A4242aB")

def test_case_conversion___7__upper_snake_case____pascalcase_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.UPPER_SNAKE, hydra.util.CaseConvention.PASCAL, "A_HELLO_WORLD_42_A42_42A_B")) == ("AHelloWorld42A4242aB")

def test_case_conversion___8__upper_snake_case____upper_snake_case_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.UPPER_SNAKE, hydra.util.CaseConvention.UPPER_SNAKE, "A_HELLO_WORLD_42_A42_42A_B")) == ("A_HELLO_WORLD_42_A42_42A_B")

def test_case_conversion___9__camelcase____lower_snake_case_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.LOWER_SNAKE, "aHelloWorld42A4242aB")) == ("a_hello_world42_a4242a_b")

def test_case_conversion___10__camelcase____upper_snake_case_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.UPPER_SNAKE, "aHelloWorld42A4242aB")) == ("A_HELLO_WORLD42_A4242A_B")

def test_case_conversion___11__camelcase____pascalcase_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.PASCAL, "aHelloWorld42A4242aB")) == ("AHelloWorld42A4242aB")

def test_case_conversion___12__camelcase____camelcase_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.CAMEL, "aHelloWorld42A4242aB")) == ("aHelloWorld42A4242aB")

def test_case_conversion___13__pascalcase____lower_snake_case_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.PASCAL, hydra.util.CaseConvention.LOWER_SNAKE, "AHelloWorld42A4242aB")) == ("a_hello_world42_a4242a_b")

def test_case_conversion___14__pascalcase____upper_snake_case_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.PASCAL, hydra.util.CaseConvention.UPPER_SNAKE, "AHelloWorld42A4242aB")) == ("A_HELLO_WORLD42_A4242A_B")

def test_case_conversion___15__pascalcase____camelcase_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.PASCAL, hydra.util.CaseConvention.CAMEL, "AHelloWorld42A4242aB")) == ("aHelloWorld42A4242aB")

def test_case_conversion___16__pascalcase____pascalcase_():

    assert (hydra.formatting.convert_case(hydra.util.CaseConvention.PASCAL, hydra.util.CaseConvention.PASCAL, "AHelloWorld42A4242aB")) == ("AHelloWorld42A4242aB")
