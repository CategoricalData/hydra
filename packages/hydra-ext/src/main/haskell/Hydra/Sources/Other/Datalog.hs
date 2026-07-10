module Hydra.Sources.Other.Datalog where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T


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
      programElmt,
      fact,
      rule_,
      atom,
      atomList,
      atomListMultiple,
      term,
      termList,
      termListMultiple,
      constantList,
      constantListMultiple]

atom :: TypeDefinition
atom = define "Atom" $
  T.record [
    "Relation">: dl "Relation",
    "TermList">: dl "TermList"]

atomList :: TypeDefinition
atomList = define "AtomList" $
  T.union [
    "single">: dl "Atom",
    "many">: dl "AtomListMultiple"]

atomListMultiple :: TypeDefinition
atomListMultiple = define "AtomListMultiple" $
  T.record [
    "Atom">: dl "Atom",
    "AtomList">: dl "AtomList"]

constant :: TypeDefinition
constant = define "Constant" $ T.wrap T.string

constantList :: TypeDefinition
constantList = define "ConstantList" $
  T.union [
    "single">: dl "Constant",
    "many">: dl "ConstantListMultiple"]

constantListMultiple :: TypeDefinition
constantListMultiple = define "ConstantListMultiple" $
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
program = define "Program" $ T.wrap $ T.list $ dl "ProgramElmt"

programElmt :: TypeDefinition
programElmt = define "ProgramElmt" $
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
    "many">: dl "TermListMultiple"]

termListMultiple :: TypeDefinition
termListMultiple = define "TermListMultiple" $
  T.record [
    "Term">: dl "Term",
    "TermList">: dl "TermList"]

variable :: TypeDefinition
variable = define "Variable" $ T.wrap T.string
