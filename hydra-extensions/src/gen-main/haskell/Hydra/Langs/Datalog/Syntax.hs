-- | A basic Datalog model

module Hydra.Langs.Datalog.Syntax where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype Constant = 
  Constant {
    unConstant :: String}
  deriving (Eq, Ord, Read, Show)

_Constant = (Core.Name "hydra/langs/datalog/syntax.Constant")

newtype Relation = 
  Relation {
    unRelation :: String}
  deriving (Eq, Ord, Read, Show)

_Relation = (Core.Name "hydra/langs/datalog/syntax.Relation")

newtype Variable = 
  Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra/langs/datalog/syntax.Variable")

newtype Program = 
  Program {
    unProgram :: [Program_Elmt]}
  deriving (Eq, Ord, Read, Show)

_Program = (Core.Name "hydra/langs/datalog/syntax.Program")

data Program_Elmt = 
  Program_ElmtFact Fact |
  Program_ElmtRule Rule
  deriving (Eq, Ord, Read, Show)

_Program_Elmt = (Core.Name "hydra/langs/datalog/syntax.Program.Elmt")

_Program_Elmt_fact = (Core.FieldName "fact")

_Program_Elmt_rule = (Core.FieldName "rule")

data Fact = 
  Fact {
    factRelation :: Relation,
    factConstantList :: ConstantList}
  deriving (Eq, Ord, Read, Show)

_Fact = (Core.Name "hydra/langs/datalog/syntax.Fact")

_Fact_relation = (Core.FieldName "relation")

_Fact_constantList = (Core.FieldName "constantList")

data Rule = 
  Rule {
    ruleAtom :: Atom,
    ruleAtomList :: AtomList}
  deriving (Eq, Ord, Read, Show)

_Rule = (Core.Name "hydra/langs/datalog/syntax.Rule")

_Rule_atom = (Core.FieldName "atom")

_Rule_atomList = (Core.FieldName "atomList")

data Atom = 
  Atom {
    atomRelation :: Relation,
    atomTermList :: TermList}
  deriving (Eq, Ord, Read, Show)

_Atom = (Core.Name "hydra/langs/datalog/syntax.Atom")

_Atom_relation = (Core.FieldName "relation")

_Atom_termList = (Core.FieldName "termList")

data AtomList = 
  AtomListSingle Atom |
  AtomListMultiple AtomList_Multiple
  deriving (Eq, Ord, Read, Show)

_AtomList = (Core.Name "hydra/langs/datalog/syntax.AtomList")

_AtomList_single = (Core.FieldName "single")

_AtomList_multiple = (Core.FieldName "multiple")

data AtomList_Multiple = 
  AtomList_Multiple {
    atomList_MultipleAtom :: Atom,
    atomList_MultipleAtomList :: AtomList}
  deriving (Eq, Ord, Read, Show)

_AtomList_Multiple = (Core.Name "hydra/langs/datalog/syntax.AtomList.Multiple")

_AtomList_Multiple_atom = (Core.FieldName "atom")

_AtomList_Multiple_atomList = (Core.FieldName "atomList")

data Term = 
  TermConstant Constant |
  TermVariable Variable
  deriving (Eq, Ord, Read, Show)

_Term = (Core.Name "hydra/langs/datalog/syntax.Term")

_Term_constant = (Core.FieldName "constant")

_Term_variable = (Core.FieldName "variable")

data TermList = 
  TermListSingle Term |
  TermListMultiple TermList_Multiple
  deriving (Eq, Ord, Read, Show)

_TermList = (Core.Name "hydra/langs/datalog/syntax.TermList")

_TermList_single = (Core.FieldName "single")

_TermList_multiple = (Core.FieldName "multiple")

data TermList_Multiple = 
  TermList_Multiple {
    termList_MultipleTerm :: Term,
    termList_MultipleTermList :: TermList}
  deriving (Eq, Ord, Read, Show)

_TermList_Multiple = (Core.Name "hydra/langs/datalog/syntax.TermList.Multiple")

_TermList_Multiple_term = (Core.FieldName "term")

_TermList_Multiple_termList = (Core.FieldName "termList")

data ConstantList = 
  ConstantListSingle Constant |
  ConstantListMultiple ConstantList_Multiple
  deriving (Eq, Ord, Read, Show)

_ConstantList = (Core.Name "hydra/langs/datalog/syntax.ConstantList")

_ConstantList_single = (Core.FieldName "single")

_ConstantList_multiple = (Core.FieldName "multiple")

data ConstantList_Multiple = 
  ConstantList_Multiple {
    constantList_MultipleConstant :: Constant,
    constantList_MultipleConstantList :: ConstantList}
  deriving (Eq, Ord, Read, Show)

_ConstantList_Multiple = (Core.Name "hydra/langs/datalog/syntax.ConstantList.Multiple")

_ConstantList_Multiple_constant = (Core.FieldName "constant")

_ConstantList_Multiple_constantList = (Core.FieldName "constantList")