# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.variants."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.phantoms

elimination_variant_record = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.EliminationVariant"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

elimination_variant_union = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.EliminationVariant"), hydra.core.Field(hydra.core.Name("union"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

elimination_variant_wrap = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.EliminationVariant"), hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

function_variant_elimination = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.FunctionVariant"), hydra.core.Field(hydra.core.Name("elimination"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

function_variant_lambda = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.FunctionVariant"), hydra.core.Field(hydra.core.Name("lambda"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

literal_variant_binary = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("binary"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

literal_variant_boolean = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("boolean"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

literal_variant_float = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("float"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

literal_variant_integer = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("integer"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

literal_variant_string = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.LiteralVariant"), hydra.core.Field(hydra.core.Name("string"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_annotated = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("annotated"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_application = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("application"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_cases = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("cases"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_either = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("either"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_inject = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("inject"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_lambda = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("lambda"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_let = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("let"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_list = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("list"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_literal = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("literal"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_map = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("map"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_maybe = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("maybe"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_pair = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("pair"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_project = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("project"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_record = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_set = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("set"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_type_application = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("typeApplication"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_type_lambda = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("typeLambda"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_unit = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_unwrap = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("unwrap"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_variable = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("variable"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_variant_wrap = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TermVariant"), hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_annotated = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("annotated"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_application = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("application"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_either = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("either"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_forall = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("forall"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_function = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_list = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("list"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_literal = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("literal"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_map = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("map"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_maybe = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("maybe"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_pair = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("pair"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_record = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_set = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("set"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_union = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("union"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_unit = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_variable = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("variable"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_void = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("void"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

type_variant_wrap = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.variants.TypeVariant"), hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermUnit()))))))
