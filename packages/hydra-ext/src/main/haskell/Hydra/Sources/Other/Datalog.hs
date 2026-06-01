module Hydra.Sources.Other.Datalog where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T


ns :: ModuleName
ns = ModuleName "hydra.datalog.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [],
            moduleMetadata = descriptionMetadata (Just "A basic Datalog model")}
  where
    definitions = [
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

atom :: TypeDefinition
atom = define "Atom" $
  T.record [
    "Relation">: dl "Relation",
    "TermList">: dl "TermList"]

atomList :: TypeDefinition
atomList = define "AtomList" $
  T.union [
    "single">: dl "Atom",
    "multiple">: dl "AtomList_Multiple"]

atomList_Multiple :: TypeDefinition
atomList_Multiple = define "AtomList_Multiple" $
  T.record [
    "Atom">: dl "Atom",
    "AtomList">: dl "AtomList"]

constant :: TypeDefinition
constant = define "Constant" $ T.wrap T.string

constantList :: TypeDefinition
constantList = define "ConstantList" $
  T.union [
    "single">: dl "Constant",
    "multiple">: dl "ConstantList_Multiple"]

constantList_Multiple :: TypeDefinition
constantList_Multiple = define "ConstantList_Multiple" $
  T.record [
    "Constant">: dl "Constant",
    "ConstantList">: dl "ConstantList"]

dl :: String -> Type
dl = typeref ns

fact :: TypeDefinition
fact = define "Fact" $
  T.record [
    "Relation">: dl "Relation",
    "ConstantList">: dl "ConstantList"]

program :: TypeDefinition
program = define "Program" $ T.wrap $ T.list $ dl "Program_Elmt"

program_Elmt :: TypeDefinition
program_Elmt = define "Program_Elmt" $
  T.union [
    "Fact">: dl "Fact",
    "Rule">: dl "Rule"]

relation :: TypeDefinition
relation = define "Relation" $ T.wrap T.string

rule_ :: TypeDefinition
rule_ = define "Rule" $
  T.record [
    "Atom">: dl "Atom",
    "AtomList">: dl "AtomList"]

term :: TypeDefinition
term = define "Term" $
  T.union [
    "Constant">: dl "Constant",
    "Variable">: dl "Variable"]

termList :: TypeDefinition
termList = define "TermList" $
  T.union [
    "single">: dl "Term",
    "multiple">: dl "TermList_Multiple"]

termList_Multiple :: TypeDefinition
termList_Multiple = define "TermList_Multiple" $
  T.record [
    "Term">: dl "Term",
    "TermList">: dl "TermList"]

variable :: TypeDefinition
variable = define "Variable" $ T.wrap T.string
