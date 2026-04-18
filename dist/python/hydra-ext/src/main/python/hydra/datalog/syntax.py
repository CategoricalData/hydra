# Note: this is an automatically generated file. Do not edit.

r"""A basic Datalog model."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class Constant(Node[str]):
    ...

Constant.TYPE_ = hydra.core.Name("hydra.datalog.syntax.Constant")

class Relation(Node[str]):
    ...

Relation.TYPE_ = hydra.core.Name("hydra.datalog.syntax.Relation")

class Variable(Node[str]):
    ...

Variable.TYPE_ = hydra.core.Name("hydra.datalog.syntax.Variable")

class Program(Node["frozenlist[Program_Elmt]"]):
    ...

Program.TYPE_ = hydra.core.Name("hydra.datalog.syntax.Program")

class Program_ElmtFact(Node["Fact"]):
    ...

class Program_ElmtRule(Node["Rule"]):
    ...

class _Program_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class Program_Elmt(metaclass=_Program_ElmtMeta):
    r"""Program_ElmtFact | Program_ElmtRule"""

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.Program_Elmt")
    FACT = hydra.core.Name("Fact")
    RULE = hydra.core.Name("Rule")

@dataclass(frozen=True)
class Fact:
    relation: Relation
    constant_list: ConstantList

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.Fact")
    RELATION = hydra.core.Name("Relation")
    CONSTANT_LIST = hydra.core.Name("ConstantList")

@dataclass(frozen=True)
class Rule:
    atom: Atom
    atom_list: AtomList

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.Rule")
    ATOM = hydra.core.Name("Atom")
    ATOM_LIST = hydra.core.Name("AtomList")

@dataclass(frozen=True)
class Atom:
    relation: Relation
    term_list: TermList

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.Atom")
    RELATION = hydra.core.Name("Relation")
    TERM_LIST = hydra.core.Name("TermList")

class AtomListSingle(Node["Atom"]):
    ...

class AtomListMultiple(Node["AtomList_Multiple"]):
    ...

class _AtomListMeta(type):
    def __getitem__(cls, item):
        return object

class AtomList(metaclass=_AtomListMeta):
    r"""AtomListSingle | AtomListMultiple"""

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.AtomList")
    SINGLE = hydra.core.Name("single")
    MULTIPLE = hydra.core.Name("multiple")

@dataclass(frozen=True)
class AtomList_Multiple:
    atom: Atom
    atom_list: AtomList

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.AtomList_Multiple")
    ATOM = hydra.core.Name("Atom")
    ATOM_LIST = hydra.core.Name("AtomList")

class TermConstant(Node["Constant"]):
    ...

class TermVariable(Node["Variable"]):
    ...

class _TermMeta(type):
    def __getitem__(cls, item):
        return object

class Term(metaclass=_TermMeta):
    r"""TermConstant | TermVariable"""

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.Term")
    CONSTANT = hydra.core.Name("Constant")
    VARIABLE = hydra.core.Name("Variable")

class TermListSingle(Node["Term"]):
    ...

class TermListMultiple(Node["TermList_Multiple"]):
    ...

class _TermListMeta(type):
    def __getitem__(cls, item):
        return object

class TermList(metaclass=_TermListMeta):
    r"""TermListSingle | TermListMultiple"""

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.TermList")
    SINGLE = hydra.core.Name("single")
    MULTIPLE = hydra.core.Name("multiple")

@dataclass(frozen=True)
class TermList_Multiple:
    term: Term
    term_list: TermList

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.TermList_Multiple")
    TERM = hydra.core.Name("Term")
    TERM_LIST = hydra.core.Name("TermList")

class ConstantListSingle(Node["Constant"]):
    ...

class ConstantListMultiple(Node["ConstantList_Multiple"]):
    ...

class _ConstantListMeta(type):
    def __getitem__(cls, item):
        return object

class ConstantList(metaclass=_ConstantListMeta):
    r"""ConstantListSingle | ConstantListMultiple"""

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.ConstantList")
    SINGLE = hydra.core.Name("single")
    MULTIPLE = hydra.core.Name("multiple")

@dataclass(frozen=True)
class ConstantList_Multiple:
    constant: Constant
    constant_list: ConstantList

    TYPE_ = hydra.core.Name("hydra.datalog.syntax.ConstantList_Multiple")
    CONSTANT = hydra.core.Name("Constant")
    CONSTANT_LIST = hydra.core.Name("ConstantList")
