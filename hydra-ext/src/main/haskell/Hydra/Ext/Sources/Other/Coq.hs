module Hydra.Ext.Sources.Other.Coq where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel hiding (letBindings)
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.fr.inria.coq.syntax"

define :: String -> Type -> Binding
define = defineType ns

coq :: String -> Type
coq = typeref ns

module_ :: Module
module_ = Module ns elements [] [] $
    Just ("A model for Coq core and extensions. Based on the Coq 8.15 grammar:\n" ++
      "  https://coq.github.io/doc/v8.15/refman/language/core/basic.html#essential-vocabulary")
  where
    elements = [
      annotatedApplication,
      application,
      arg,
      binder,
      caseItem,
      cofix,
      cofixBody,
      cofixQual,
      cofixWith,
      equation,
      existentialVariable,
      existentialVariableVariant,
      fieldIdent,
      fix_,
      fixAnnot,
      fixAnnot_Measure,
      fixAnnot_Wf,
      fix_Decl,
      fix_Qual,
      fixWith,
      forall_,
      forallOrFun,
      fun,
      generalizingBinder,
      ident,
      identArg,
      if_,
      implicitBinders,
      let_,
      letBinder,
      letBindings,
      letNamed,
      letDestructuring,
      letDestructuring_Variant1,
      letDestructuring_Variant2,
      letDestructuring_Variant3,
      match,
      name_,
      natural,
      naturalArg,
      normalApplication,
      number,
      oneTerm,
      openBinders,
      pattern_,
      pattern0,
      pattern1,
      pattern10,
      pattern10_As,
      pattern10_Patterns,
      pattern10_Qualid,
      primitiveNotations,
      qualid,
      qualidAndPattern,
      qualidAnnotated,
      returnAs,
      scopeKey,
      sort,
      string_,
      term,
      term0,
      term1,
      term10,
      term100,
      type_,
      typeCast,
      typeCastOperator,
      typeBinders,
      typeclassConstraint,
      univAnnot,
      universe,
      universe_Expr,
      universeLevel,
      universeName]

annotatedApplication :: Binding
annotatedApplication = define "AnnotatedApplication" $ T.record [
  "annot">: coq "QualidAnnotated",
  "terms">: nonemptyList $ coq "Term1"]

application :: Binding
application = define "Application" $ T.union [
  "normal">: coq "NormalApplication",
  "annotated">: coq "AnnotatedApplication"]

arg :: Binding
arg = define "Arg" $ T.union [
  "ident">: coq "IdentArg",
  "natural">: coq "NaturalArg",
  "term">: coq "Term1"]

binder :: Binding
binder = define "Binder" $ T.union [
  "name">: coq "Name",
  "type">: coq "TypeBinders",
  "term">: coq "LetBinder",
  "implicit">: coq "ImplicitBinders",
  "generalizing">: coq "GeneralizingBinder",
  -- 	( name : type | term )  -- TODO
  "pattern">: coq "Pattern0"]

caseItem :: Binding
caseItem = define "CaseItem" $ T.record [
  "term">: coq "Term100",
  "as">: T.maybe $ coq "Name",
  "in">: T.maybe $ coq "Pattern"]

cofix :: Binding
cofix = define "Cofix" $ T.record [
  "body">: coq "CofixBody",
  "qual">: T.maybe $ coq "CofixQual"]

cofixBody :: Binding
cofixBody = define "CofixBody" $ T.record [
  "ident">: coq "Ident",
  "binders">: T.list $ coq "Binder",
  "type">: T.maybe $ coq "Type",
  "term">: coq "Term"]

cofixQual :: Binding
cofixQual = define "CofixQual" $ T.union [
  "in">: coq "Term",
  "with">: coq "CofixWith"]

cofixWith :: Binding
cofixWith = define "CofixWith" $ T.record [
  "with">: nonemptyList $ coq "CofixBody",
  "for">: T.maybe $ coq "Ident"]

equation :: Binding
equation = define "Equation" $ T.record [
  "pattern">: nonemptyList $ nonemptyList $ coq "Pattern",
  "term">: coq "Term"]

existentialVariable :: Binding
existentialVariable = define "ExistentialVariable" $ T.record [
  "ident">: coq "Ident",
  "variant">: coq "ExistentialVariableVariant"]

existentialVariableVariant :: Binding
existentialVariableVariant = define "ExistentialVariableVariant" $ T.union [
  "placeholder">: T.unit,
  "inside1">: T.unit,
  "inside2">: T.unit,
  "outside">: T.maybe $ coq "IdentArg"]

fieldIdent :: Binding
fieldIdent = define "FieldIdent" $ T.wrap $ coq "Ident"

fix_ :: Binding
fix_ = define "Fix" $ T.union [
  "decl">: coq "Fix_Decl",
  "qual">: T.maybe $ coq "Fix_Qual"]

fixAnnot :: Binding
fixAnnot = define "FixAnnot" $ T.union [
  "struct">: coq "Ident",
  "wf">: coq "FixAnnot_Wf",
  "measure">: coq "FixAnnot_Measure"]

fixAnnot_Measure :: Binding
fixAnnot_Measure = define "FixAnnot_Measure" $ T.record [
  "term">: coq "OneTerm",
  "ident">: T.maybe $ coq "Ident",
  "term2">: T.maybe $ coq "OneTerm"]

fixAnnot_Wf :: Binding
fixAnnot_Wf = define "FixAnnot_Wf" $ T.record [
  "term">: coq "OneTerm",
  "ident">: coq "Ident"]

fix_Decl :: Binding
fix_Decl = define "Fix_Decl" $ T.record [
  "ident">: coq "Ident",
  "binders">: T.list $ coq "Binder",
  "annot">: T.maybe $ coq "FixAnnot",
  "type">: T.maybe $ coq "Type",
  "term">: coq "Term"]

fix_Qual :: Binding
fix_Qual = define "Fix_Qual" $ T.union [
  "in">: coq "Term",
  "with">: coq "FixWith"]

fixWith :: Binding
fixWith = define "FixWith" $ T.record [
  "decls">: nonemptyList $ coq "Fix_Decl",
  "for">: T.maybe $ coq "Ident"]

forall_ :: Binding
forall_ = define "Forall" $ T.record [
  "binders">: coq "OpenBinders",
  "type">: coq "Type"]

forallOrFun :: Binding
forallOrFun = define "ForallOrFun" $ T.union [
  "forall">: coq "Forall",
  "fun">: coq "Fun"]

fun :: Binding
fun = define "Fun" $ T.record [
  "binders">: coq "OpenBinders",
  "body">: coq "Term"]

generalizingBinder :: Binding
generalizingBinder = define "GeneralizingBinder" $ T.union [
  "explicit">:
    doc "Terms surrounded by `( ) introduce their free variables as explicit arguments" $
    coq "TypeclassConstraint",

  "implicitMaximallyInserted">:
    doc "Terms surrounded by `{ } introduce their free variables as maximally inserted implicit arguments" $
    coq "TypeclassConstraint",

  "implicitNonMaximallyInserted">:
    doc "Terms surrounded by `[ ] introduce them as non-maximally inserted implicit arguments" $
    coq "TypeclassConstraint"]

ident :: Binding
ident = define "Ident" $ T.wrap $ coq "String"

identArg :: Binding
identArg = define "IdentArg" $ T.record [
  "ident">: coq "Ident",
  "term">: coq "Term"]

if_ :: Binding
if_ = define "If" $
  doc "Pattern match on boolean values" $
  T.record [
    "condition">: coq "Term",
    "returnAs">: T.maybe $ coq "ReturnAs",
    "then">: coq "Term",
    "else">: coq "Term"]

implicitBinders :: Binding
implicitBinders = define "ImplicitBinders" $
  doc "In the context of a function definition, these forms specify that name is an implicit argument." $
  T.union [
    "maximallyInserted">:
      doc "The first form, with curly braces, makes name a maximally inserted implicit argument" $
      coq "TypeBinders",

    "nonMaximallyInserted">:
      doc "The second form, with square brackets, makes name a non-maximally inserted implicit argument." $
      coq "TypeBinders"]

let_ :: Binding
let_ = define "Let" $
  doc "A let-in definition" $
  T.record [
    "bindings">: coq "LetBindings",
    "in">: coq "Term"]

letBinder :: Binding
letBinder = define "LetBinder" $
  doc "Some constructions allow the binding of a variable to value. This is called a 'let-binder'." $
  T.record [
    "name">: coq "Name",
    "type">: T.maybe $ coq "Type",
    "term">: coq "Term"]

letBindings :: Binding
letBindings = define "LetBindings" $ T.union [
  "named">: coq "LetNamed",
  "destructuring">: coq "LetDestructuring"]

letNamed :: Binding
letNamed = define "LetNamed" $ T.record [
  "binder">: coq "LetBinder",
  "binders">: T.list $ coq "Binder"]

letDestructuring :: Binding
letDestructuring = define "LetDestructuring" $ T.union [
  "variant1">: coq "LetDestructuring_Variant1",
  "variant2">: coq "LetDestructuring_Variant2",
  "variant3">: coq "LetDestructuring_Variant3"]

letDestructuring_Variant1 :: Binding
letDestructuring_Variant1 = define "LetDestructuring_Variant1" $ T.record [
  "names">: T.list $ coq "Name",
  "returnAs">: T.maybe $ coq "ReturnAs",
  "term">: coq "Term"]

letDestructuring_Variant2 :: Binding
letDestructuring_Variant2 = define "LetDestructuring_Variant2" $ T.record [
  "pattern">: coq "Pattern",
  "term">: coq "Term",
  "return">: T.maybe $ coq "Term100"]

letDestructuring_Variant3 :: Binding
letDestructuring_Variant3 = define "LetDestructuring_Variant3" $ T.record [
  "pattern1">: coq "Pattern",
  "pattern2">: coq "Pattern",
  "term">: coq "Term",
  "return">: coq "Term100"]

match :: Binding
match = define "Match" $ T.record [
  "caseItems">: nonemptyList $ coq "CaseItem",
  "return">: T.maybe $ coq "Term100",
  "pipe">: T.boolean,
  "equations">: T.list $ coq "Equation"]

name_ :: Binding
name_ = define "Name" $ T.wrap $ T.maybe $ coq "Ident"

natural :: Binding
natural = define "Natural" $
  doc "A non-negative arbitrary-precision integer" $
  T.wrap T.bigint

naturalArg :: Binding
naturalArg = define "NaturalArg" $ T.record [
  "natural">: coq "Natural",
  "term">: coq "Term"]

normalApplication :: Binding
normalApplication = define "NormalApplication" $ T.record [
  "lhs">: coq "Term1",
  "rhs">: nonemptyList $ coq "Arg"]

number :: Binding
number = define "Number" $ T.wrap T.bigfloat

oneTerm :: Binding
oneTerm = define "OneTerm" $ T.union [
  "explicit">: coq "QualidAnnotated",
  "term1">: coq "Term1"]

openBinders :: Binding
openBinders = define "OpenBinders" $ T.union [
  "type">: coq "TypeBinders",
  "binders">: T.list $ coq "Binder"]

pattern_ :: Binding
pattern_ = define "Pattern" $ T.union [
  "pattern">: coq "Pattern10",
  "term">: T.maybe $ coq "Term"]

pattern0 :: Binding
pattern0 = define "Pattern0" $ T.union [
  "qualid">: coq "Qualid",
  "qualIdAndPattern">: coq "QualidAndPattern",
  "placeholder">: T.unit,
  "parens">: nonemptyList $ coq "Pattern",
  "number">: coq "Number",
  "string">: coq "String"]

pattern1 :: Binding
pattern1 = define "Pattern1" $ T.record [
  "pattern">: coq "Pattern0",
  "scope">: T.maybe $ coq "ScopeKey"]

pattern10 :: Binding
pattern10 = define "Pattern10" $ T.union [
  "as">: coq "Pattern10_As",
  "patterns">: coq "Pattern10_Patterns",
  "qualiid">: coq "Pattern10_Qualid"]

pattern10_As :: Binding
pattern10_As = define "Pattern10_As" $ T.record [
  "pattern">: coq "Pattern1",
  "as">: coq "Name"]

pattern10_Patterns :: Binding
pattern10_Patterns = define "Pattern10_Patterns" $ T.record [
  "pattern">: coq "Pattern1",
  "patterns">: T.list $ coq "Pattern1"]

pattern10_Qualid :: Binding
pattern10_Qualid = define "Pattern10_Qualid" $ T.record [
  "qualid">: coq "Qualid",
  "patterns">: T.list $ coq "Pattern1"]

primitiveNotations :: Binding
primitiveNotations = define "PrimitiveNotations" $ T.union [
  "number">: coq "Number",
  "string">: coq "String"]

qualid :: Binding
qualid = define "Qualid" $
  doc "A qualified identifier" $
  T.record [
    "id">: coq "Ident",
    "fieldIds">: T.list $ coq "FieldIdent"]

qualidAndPattern :: Binding
qualidAndPattern = define "QualidAndPattern" $ T.record [
  "qualid">: coq "Qualid",
  "pattern">: coq "Pattern"]

qualidAnnotated :: Binding
qualidAnnotated = define "QualidAnnotated" $ T.record [
  "qualid">: coq "Qualid",
  "univAnnot">: T.maybe $ coq "UnivAnnot"]

returnAs :: Binding
returnAs = define "ReturnAs" $ T.record [
  "as">: T.maybe $ coq "Name",
  "return">: coq "Term100"]

scopeKey :: Binding
scopeKey = define "ScopeKey" $ T.wrap $ coq "Ident"

sort :: Binding
sort = define "Sort" $
  doc "The types of types are called sorts." $
  T.union [
    "set">:
      doc "The sort 𝖲𝖾𝗍 intends to be the type of small sets." T.unit,
    "prop">:
      doc "The sort 𝖯𝗋𝗈𝗉 intends to be the type of logical propositions." T.unit,
    "sProp">:
      doc "The sort 𝖲𝖯𝗋𝗈𝗉 is like 𝖯𝗋𝗈𝗉 but the propositions in 𝖲𝖯𝗋𝗈𝗉 are known to have irrelevant proofs (all proofs are equal)." T.unit,
    "type">: T.unit,
    "typeWithAnyUniverse">: T.unit,
    "typeWithUniverse">: coq "Universe"]

string_ :: Binding
string_ = define "String" $ T.wrap T.string

term :: Binding
term = define "Term" $ T.union [
  "forallOrFun">: coq "ForallOrFun",
  "let">: coq "Let",
  "if">: coq "If",
  "fix">: coq "Fix",
  "cofix">: coq "Cofix",
  "term100">: coq "Term100"]

term0 :: Binding
term0 = define "Term0" $ T.union [
  "qualidAnnotated">: coq "QualidAnnotated",
  "sort">: coq "Sort",
  "primitiveNotations">: coq "PrimitiveNotations",
  "evar">: coq "ExistentialVariable",
  "match">: coq "Match",
  "record">: T.unit,
  "generalizing">: T.unit,
  -- 	[| term*; | term : type? |] univ_annot? -- TODO
  "ltac">: T.unit,
  "parens">: coq "Term"]

term1 :: Binding
term1 = define "Term1" $ T.union [
  "projection">: T.unit,
  "scope">: T.unit,
  "term0">: coq "Term0"]

term10 :: Binding
term10 = define "Term10" $ T.union [
  "application">: coq "Application",
  "oneTerm">: coq "OneTerm"]

term100 :: Binding
term100 = define "Term100" $ T.union [
  "cast">: coq "TypeCast",
  "term10">: coq "Term10"]

type_ :: Binding
type_ = define "Type" $ T.wrap $ coq "Term"

typeCast :: Binding
typeCast = define "TypeCast" $ T.record [
  "term">: coq "Term10",
  "type">: coq "Type",
  "operator">: coq "TypeCastOperator"]

typeCastOperator :: Binding
typeCastOperator = define "TypeCastOperator" $ T.union [
  "normal">:
    doc "The expression term10 : type is a type cast expression. It enforces the type of term10 to be type." T.unit,
  "vmCompute">:
    doc "term10 <: type specifies that the virtual machine will be used to type check that term10 has type type (see vm_compute)." T.unit,
  "nativeCompute">:
    doc "term10 <<: type specifies that compilation to OCaml will be used to type check that term10 has type type (see native_compute)." T.unit]

typeBinders :: Binding
typeBinders = define "TypeBinders" $ T.record [
  "names">: nonemptyList $ coq "Name",
  "type">: coq "Type"]

typeclassConstraint :: Binding
typeclassConstraint = define "TypeclassConstraint" $
  T.record [
    "name">: T.maybe $ coq "Name",
    "generalizing">: T.boolean,
    "term">: coq "Term"]

univAnnot :: Binding
univAnnot = define "UnivAnnot" $
  T.wrap $ T.list $ coq "UniverseLevel"

universe :: Binding
universe = define "Universe" $ T.union [
  "max">: nonemptyList $ coq "Universe_Expr",
  "expr">: coq "Universe_Expr"]

universe_Expr :: Binding
universe_Expr = define "Universe_Expr" $ T.record [
  "name">: coq "UniverseName",
  "number">: T.maybe $ coq "Natural"]

universeLevel :: Binding
universeLevel = define "UniverseLevel" $ T.union [
  "set">: T.unit,
  "prop">: T.unit,
  "type">: T.unit,
  "ignored">: T.unit,
  "qualid">: coq "Qualid"]

universeName :: Binding
universeName = define "UniverseName" $ T.union [
  "qualid">: coq "Qualid",
  "set">: T.unit,
  "prop">: T.unit]
