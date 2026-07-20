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
      atom,
      atomList,
      atomListMultiple,
      constant,
      constantList,
      constantListMultiple,
      fact,
      program,
      programElmt,
      relation,
      rule_,
      term,
      termList,
      termListMultiple,
      variable]

atom :: TypeDefinition
atom = define "Atom" $
  doc "A relation applied to a list of terms" $
  T.record [
    "Relation">: dl "Relation",
    "TermList">: dl "TermList"]

atomList :: TypeDefinition
atomList = define "AtomList" $
  doc "A nonempty, comma-separated list of atoms" $
  T.union [
    "single">: dl "Atom",
    "many">: dl "AtomListMultiple"]

atomListMultiple :: TypeDefinition
atomListMultiple = define "AtomListMultiple" $
  doc "An atom followed by the remainder of a comma-separated atom list" $
  T.record [
    "Atom">: dl "Atom",
    "AtomList">: dl "AtomList"]

constant :: TypeDefinition
constant = define "Constant" $
  doc "A constant term, e.g. an atomic value such as a symbol or number" $
  T.wrap T.string

constantList :: TypeDefinition
constantList = define "ConstantList" $
  doc "A nonempty, comma-separated list of constants" $
  T.union [
    "single">: dl "Constant",
    "many">: dl "ConstantListMultiple"]

constantListMultiple :: TypeDefinition
constantListMultiple = define "ConstantListMultiple" $
  doc "A constant followed by the remainder of a comma-separated constant list" $
  T.record [
    "Constant">: dl "Constant",
    "ConstantList">: dl "ConstantList"]

dl :: String -> Type
dl = typeref ns

fact :: TypeDefinition
fact = define "Fact" $
  doc "An unconditional assertion of a relation applied to a list of constants" $
  T.record [
    "Relation">: dl "Relation",
    "ConstantList">: dl "ConstantList"]

program :: TypeDefinition
program = define "Program" $
  doc "A Datalog program: a list of facts and rules" $
  T.wrap $ T.list $ dl "ProgramElmt"

programElmt :: TypeDefinition
programElmt = define "ProgramElmt" $
  doc "A single element of a Datalog program: either a fact or a rule" $
  T.union [
    "Fact">: dl "Fact",
    "Rule">: dl "Rule"]

relation :: TypeDefinition
relation = define "Relation" $
  doc "The name of a predicate (relation) used in facts, rules, and queries" $
  T.wrap T.string

rule_ :: TypeDefinition
rule_ = define "Rule" $
  doc "A conditional assertion: a head atom implied by a conjunction of body atoms" $
  T.record [
    "Atom">: dl "Atom",
    "AtomList">: dl "AtomList"]

term :: TypeDefinition
term = define "Term" $
  doc "An argument to an atom: either a constant or a variable" $
  T.union [
    "Constant">: dl "Constant",
    "Variable">: dl "Variable"]

termList :: TypeDefinition
termList = define "TermList" $
  doc "A nonempty, comma-separated list of terms" $
  T.union [
    "single">: dl "Term",
    "many">: dl "TermListMultiple"]

termListMultiple :: TypeDefinition
termListMultiple = define "TermListMultiple" $
  doc "A term followed by the remainder of a comma-separated term list" $
  T.record [
    "Term">: dl "Term",
    "TermList">: dl "TermList"]

variable :: TypeDefinition
variable = define "Variable" $
  doc "A logic variable, which may be bound to different constants across a query" $
  T.wrap T.string
