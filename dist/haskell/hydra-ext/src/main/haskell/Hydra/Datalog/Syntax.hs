-- Note: this is an automatically generated file. Do not edit.

-- | A basic Datalog model

module Hydra.Datalog.Syntax where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

newtype Constant =
  Constant {
    unConstant :: String}
  deriving (Eq, Ord, Read, Show)

_Constant = Core.Name "hydra.datalog.syntax.Constant"

newtype Relation =
  Relation {
    unRelation :: String}
  deriving (Eq, Ord, Read, Show)

_Relation = Core.Name "hydra.datalog.syntax.Relation"

newtype Variable =
  Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = Core.Name "hydra.datalog.syntax.Variable"

newtype Program =
  Program {
    unProgram :: [Program_Elmt]}
  deriving (Eq, Ord, Read, Show)

_Program = Core.Name "hydra.datalog.syntax.Program"

data Program_Elmt =
  Program_ElmtFact Fact |
  Program_ElmtRule Rule
  deriving (Eq, Ord, Read, Show)

_Program_Elmt = Core.Name "hydra.datalog.syntax.Program_Elmt"

_Program_Elmt_Fact = Core.Name "Fact"

_Program_Elmt_Rule = Core.Name "Rule"

data Fact =
  Fact {
    factRelation :: Relation,
    factConstantList :: ConstantList}
  deriving (Eq, Ord, Read, Show)

_Fact = Core.Name "hydra.datalog.syntax.Fact"

_Fact_Relation = Core.Name "Relation"

_Fact_ConstantList = Core.Name "ConstantList"

data Rule =
  Rule {
    ruleAtom :: Atom,
    ruleAtomList :: AtomList}
  deriving (Eq, Ord, Read, Show)

_Rule = Core.Name "hydra.datalog.syntax.Rule"

_Rule_Atom = Core.Name "Atom"

_Rule_AtomList = Core.Name "AtomList"

data Atom =
  Atom {
    atomRelation :: Relation,
    atomTermList :: TermList}
  deriving (Eq, Ord, Read, Show)

_Atom = Core.Name "hydra.datalog.syntax.Atom"

_Atom_Relation = Core.Name "Relation"

_Atom_TermList = Core.Name "TermList"

data AtomList =
  AtomListSingle Atom |
  AtomListMultiple AtomList_Multiple
  deriving (Eq, Ord, Read, Show)

_AtomList = Core.Name "hydra.datalog.syntax.AtomList"

_AtomList_single = Core.Name "single"

_AtomList_multiple = Core.Name "multiple"

data AtomList_Multiple =
  AtomList_Multiple {
    atomList_MultipleAtom :: Atom,
    atomList_MultipleAtomList :: AtomList}
  deriving (Eq, Ord, Read, Show)

_AtomList_Multiple = Core.Name "hydra.datalog.syntax.AtomList_Multiple"

_AtomList_Multiple_Atom = Core.Name "Atom"

_AtomList_Multiple_AtomList = Core.Name "AtomList"

data Term =
  TermConstant Constant |
  TermVariable Variable
  deriving (Eq, Ord, Read, Show)

_Term = Core.Name "hydra.datalog.syntax.Term"

_Term_Constant = Core.Name "Constant"

_Term_Variable = Core.Name "Variable"

data TermList =
  TermListSingle Term |
  TermListMultiple TermList_Multiple
  deriving (Eq, Ord, Read, Show)

_TermList = Core.Name "hydra.datalog.syntax.TermList"

_TermList_single = Core.Name "single"

_TermList_multiple = Core.Name "multiple"

data TermList_Multiple =
  TermList_Multiple {
    termList_MultipleTerm :: Term,
    termList_MultipleTermList :: TermList}
  deriving (Eq, Ord, Read, Show)

_TermList_Multiple = Core.Name "hydra.datalog.syntax.TermList_Multiple"

_TermList_Multiple_Term = Core.Name "Term"

_TermList_Multiple_TermList = Core.Name "TermList"

data ConstantList =
  ConstantListSingle Constant |
  ConstantListMultiple ConstantList_Multiple
  deriving (Eq, Ord, Read, Show)

_ConstantList = Core.Name "hydra.datalog.syntax.ConstantList"

_ConstantList_single = Core.Name "single"

_ConstantList_multiple = Core.Name "multiple"

data ConstantList_Multiple =
  ConstantList_Multiple {
    constantList_MultipleConstant :: Constant,
    constantList_MultipleConstantList :: ConstantList}
  deriving (Eq, Ord, Read, Show)

_ConstantList_Multiple = Core.Name "hydra.datalog.syntax.ConstantList_Multiple"

_ConstantList_Multiple_Constant = Core.Name "Constant"

_ConstantList_Multiple_ConstantList = Core.Name "ConstantList"
