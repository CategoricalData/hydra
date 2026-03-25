module Hydra.Ext.Sources.Other.Datalog where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T


ns :: Namespace
ns = Namespace "hydra.ext.datalog.syntax"

define :: String -> Type -> Binding
define = defineType ns

dl :: String -> Type
dl = typeref ns

module_ :: Module
module_ = Module ns (map toTypeDef elements) [] [] $
    Just "A basic Datalog model"
  where
    elements = [
      constant,
      relation,
      variable,
      program,
      program_Elmt,
      fact,
      rule_,
      atom,
      atomList,
      atomList_Multiple,
      term,
      termList,
      termList_Multiple,
      constantList,
      constantList_Multiple]

constant :: Binding
constant = define "Constant" $ T.wrap T.string

relation :: Binding
relation = define "Relation" $ T.wrap T.string

variable :: Binding
variable = define "Variable" $ T.wrap T.string

program :: Binding
program = define "Program" $ T.wrap $ T.list $ dl "Program_Elmt"

program_Elmt :: Binding
program_Elmt = define "Program_Elmt" $
  T.union [
    "Fact">: dl "Fact",
    "Rule">: dl "Rule"]

fact :: Binding
fact = define "Fact" $
  T.record [
    "Relation">: dl "Relation",
    "ConstantList">: dl "ConstantList"]

rule_ :: Binding
rule_ = define "Rule" $
  T.record [
    "Atom">: dl "Atom",
    "AtomList">: dl "AtomList"]

atom :: Binding
atom = define "Atom" $
  T.record [
    "Relation">: dl "Relation",
    "TermList">: dl "TermList"]

atomList :: Binding
atomList = define "AtomList" $
  T.union [
    "single">: dl "Atom",
    "multiple">: dl "AtomList_Multiple"]

atomList_Multiple :: Binding
atomList_Multiple = define "AtomList_Multiple" $
  T.record [
    "Atom">: dl "Atom",
    "AtomList">: dl "AtomList"]

term :: Binding
term = define "Term" $
  T.union [
    "Constant">: dl "Constant",
    "Variable">: dl "Variable"]

termList :: Binding
termList = define "TermList" $
  T.union [
    "single">: dl "Term",
    "multiple">: dl "TermList_Multiple"]

termList_Multiple :: Binding
termList_Multiple = define "TermList_Multiple" $
  T.record [
    "Term">: dl "Term",
    "TermList">: dl "TermList"]

constantList :: Binding
constantList = define "ConstantList" $
  T.union [
    "single">: dl "Constant",
    "multiple">: dl "ConstantList_Multiple"]

constantList_Multiple :: Binding
constantList_Multiple = define "ConstantList_Multiple" $
  T.record [
    "Constant">: dl "Constant",
    "ConstantList">: dl "ConstantList"]
