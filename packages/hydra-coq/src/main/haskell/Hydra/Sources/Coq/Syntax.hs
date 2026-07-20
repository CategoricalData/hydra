module Hydra.Sources.Coq.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel                    hiding (letBindings)
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.coq.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [],
            moduleMetadata = descriptionMetadata (Just ("A model for Coq core and extensions. Based on the Coq 8.15 grammar:\n" ++
      "  https://coq.github.io/doc/v8.15/refman/language/core/basic.html#essential-vocabulary\n" ++
      "  Extended with Vernacular commands for complete .v file generation."))}
  where
    -- Alphabetical order by local type name, per the definition-ordering style guide
    -- (Validate.Packaging.checkDefinitionOrdering has no section-boundary awareness,
    -- so the semantic grouping formerly used here -- term language vs. vernacular
    -- commands -- cannot be preserved in this list; the section comments remain in
    -- place below, next to the definitions themselves, as documentation).
    definitions = [
      annotatedApplication,
      application,
      arg,
      axiomDeclaration,
      binder,
      caseItem,
      cofix,
      cofixBody,
      cofixQual,
      cofixWith,
      comment,
      constructor,
      definition,
      document,
      equation,
      existentialVariable,
      existentialVariableVariant,
      fieldIdent,
      fix_,
      fixAnnot,
      fixAnnot_Measure,
      fixAnnot_Wf,
      fixWith,
      fix_Decl,
      fix_Qual,
      fixpointDefinition,
      forall_,
      forallOrFun,
      fun,
      generalizingBinder,
      ident,
      identArg,
      if_,
      implicitBinders,
      importQualification,
      inductiveBody,
      inductiveDefinition,
      let_,
      letBinder,
      letBindings,
      letDestructuring,
      letDestructuring_Variant1,
      letDestructuring_Variant2,
      letDestructuring_Variant3,
      letNamed,
      locality,
      match,
      moduleDefinition,
      name_,
      natural,
      naturalArg,
      normalApplication,
      notationDeclaration,
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
      recordBody,
      recordDefinition,
      recordField,
      requireImport,
      returnAs,
      scopeKey,
      sectionDefinition,
      sentence,
      sentenceContent,
      sort,
      string_,
      term,
      term0,
      term1,
      term10,
      term100,
      theoremBody,
      theoremKind,
      type_,
      typeBinders,
      typeCast,
      typeCastOperator,
      typeclassConstraint,
      univAnnot,
      universe,
      universeLevel,
      universeName,
      universe_Expr]

annotatedApplication :: TypeDefinition
annotatedApplication = define "AnnotatedApplication" $
  doc "A Coq application with an explicit universe/type annotation on the function" $
  T.record [
  "annot">: coq "QualidAnnotated",
  "terms">: nonemptyList $ coq "Term1"]

application :: TypeDefinition
application = define "Application" $
  doc "A Coq application: a normal application or an annotated application" $
  T.union [
  "normal">: coq "NormalApplication",
  "annotated">: coq "AnnotatedApplication"]

arg :: TypeDefinition
arg = define "Arg" $
  doc "A single argument of a Coq application: an identifier, a natural number, or a term" $
  T.union [
  "ident">: coq "IdentArg",
  "natural">: coq "NaturalArg",
  "term">: coq "Term1"]

axiomDeclaration :: TypeDefinition
axiomDeclaration = define "AxiomDeclaration" $
  doc "An Axiom declaration: `Axiom <name> : <type>.`" $
  T.record [
    "name">: coq "Ident",
    "type">: coq "Type"]

binder :: TypeDefinition
binder = define "Binder" $
  doc "A Coq binder: a name, typed binder, let-binder, implicit binder, generalizing binder, or pattern" $
  T.union [
  "name">: coq "Name",
  "type">: coq "TypeBinders",
  "term">: coq "LetBinder",
  "implicit">: coq "ImplicitBinders",
  "generalizing">: coq "GeneralizingBinder",
  -- 	( name : type | term )  -- TODO
  "pattern">: coq "Pattern0"]

caseItem :: TypeDefinition
caseItem = define "CaseItem" $
  doc "One scrutinee of a Coq match, with optional as-name and in-pattern clauses" $
  T.record [
  "term">: coq "Term100",
  "as">: T.optional $ coq "Name",
  "in">: T.optional $ coq "Pattern"]

cofix :: TypeDefinition
cofix = define "Cofix" $
  doc "A Coq cofix expression: a cofixpoint body with an optional qualifier" $
  T.record [
  "body">: coq "CofixBody",
  "qual">: T.optional $ coq "CofixQual"]

cofixBody :: TypeDefinition
cofixBody = define "CofixBody" $
  doc "The body of one cofixpoint definition: an identifier, binders, an optional type, and a term" $
  T.record [
  "ident">: coq "Ident",
  "binders">: T.list $ coq "Binder",
  "type">: T.optional $ coq "Type",
  "term">: coq "Term"]

cofixQual :: TypeDefinition
cofixQual = define "CofixQual" $
  doc "The qualifier following a Coq cofix expression: an in-term or a with clause" $
  T.union [
  "in">: coq "Term",
  "with">: coq "CofixWith"]

cofixWith :: TypeDefinition
cofixWith = define "CofixWith" $
  doc "A with clause listing mutually corecursive cofixpoint bodies" $
  T.record [
  "with">: nonemptyList $ coq "CofixBody",
  "for">: T.optional $ coq "Ident"]

comment :: TypeDefinition
comment = define "Comment" $
  doc "A Coq comment (* ... *)" $
  T.wrap T.string

constructor :: TypeDefinition
constructor = define "Constructor" $
  doc "A constructor in an Inductive definition" $
  T.record [
    "name">: coq "Ident",
    "binders">: T.list $ coq "Binder",
    "type">: T.optional $ coq "Type"]

coq :: String -> Type
coq = typeref ns

definition :: TypeDefinition
definition = define "Definition" $
  doc "A Definition or Let command: Definition name binders : type := term." $
  T.record [
    "locality">: T.optional $ coq "Locality",
    "name">: coq "Ident",
    "binders">: T.list $ coq "Binder",
    "type">: T.optional $ coq "Type",
    "body">: coq "Term"]

document :: TypeDefinition
document = define "Document" $
  doc "A complete Coq .v file" $
  T.record [
    "sentences">: T.list $ coq "Sentence"]

equation :: TypeDefinition
equation = define "Equation" $
  doc "One equation of a Coq match expression: a list of pattern lists together with the result term" $
  T.record [
  "pattern">: nonemptyList $ nonemptyList $ coq "Pattern",
  "term">: coq "Term"]

existentialVariable :: TypeDefinition
existentialVariable = define "ExistentialVariable" $
  doc "A Coq existential variable reference" $
  T.record [
  "ident">: coq "Ident",
  "variant">: coq "ExistentialVariableVariant"]

existentialVariableVariant :: TypeDefinition
existentialVariableVariant = define "ExistentialVariableVariant" $
  doc "The form of a Coq existential variable reference" $
  T.union [
  "placeholder">: T.unit,
  "inside1">: T.unit,
  "inside2">: T.unit,
  "outside">: T.optional $ coq "IdentArg"]

fieldIdent :: TypeDefinition
fieldIdent = define "FieldIdent" $
  doc "A field identifier used in a qualified identifier" $
  T.wrap $ coq "Ident"

fixAnnot :: TypeDefinition
fixAnnot = define "FixAnnot" $
  doc "An annotation on a Coq fix expression: a structural, well-founded, or measure annotation" $
  T.union [
  "struct">: coq "Ident",
  "wf">: coq "FixAnnot_Wf",
  "measure">: coq "FixAnnot_Measure"]

fixAnnot_Measure :: TypeDefinition
fixAnnot_Measure = define "FixAnnot_Measure" $
  doc "A measure annotation on a Coq fix expression, used for well-founded recursion by measure" $
  T.record [
  "term">: coq "OneTerm",
  "ident">: T.optional $ coq "Ident",
  "term2">: T.optional $ coq "OneTerm"]

fixAnnot_Wf :: TypeDefinition
fixAnnot_Wf = define "FixAnnot_Wf" $
  doc "A well-founded-relation annotation on a Coq fix expression" $
  T.record [
  "term">: coq "OneTerm",
  "ident">: coq "Ident"]

fixWith :: TypeDefinition
fixWith = define "FixWith" $
  doc "A with clause listing mutually recursive fixpoint declarations" $
  T.record [
  "decls">: nonemptyList $ coq "Fix_Decl",
  "for">: T.optional $ coq "Ident"]

fix_ :: TypeDefinition
fix_ = define "Fix" $
  doc "A Coq fix expression: a fixpoint declaration with an optional qualifier" $
  T.union [
  "decl">: coq "Fix_Decl",
  "qual">: T.optional $ coq "Fix_Qual"]

fix_Decl :: TypeDefinition
fix_Decl = define "Fix_Decl" $
  doc "A single fixpoint declaration: an identifier, binders, an optional annotation and type, and a term" $
  T.record [
  "ident">: coq "Ident",
  "binders">: T.list $ coq "Binder",
  "annot">: T.optional $ coq "FixAnnot",
  "type">: T.optional $ coq "Type",
  "term">: coq "Term"]

fix_Qual :: TypeDefinition
fix_Qual = define "Fix_Qual" $
  doc "The qualifier following a Coq fix expression: an in-term or a with clause" $
  T.union [
  "in">: coq "Term",
  "with">: coq "FixWith"]

fixpointDefinition :: TypeDefinition
fixpointDefinition = define "FixpointDefinition" $
  doc "A Fixpoint command for recursive definitions" $
  T.record [
    "locality">: T.optional $ coq "Locality",
    "name">: coq "Ident",
    "binders">: T.list $ coq "Binder",
    "annot">: T.optional $ coq "FixAnnot",
    "type">: T.optional $ coq "Type",
    "body">: coq "Term",
    "with">: T.list $ coq "Fix_Decl"]

forallOrFun :: TypeDefinition
forallOrFun = define "ForallOrFun" $
  doc "A Coq forall or fun term" $
  T.union [
  "forall">: coq "Forall",
  "fun">: coq "Fun"]

forall_ :: TypeDefinition
forall_ = define "Forall" $
  doc "A Coq forall (dependent product) term: binders together with a body type" $
  T.record [
  "binders">: coq "OpenBinders",
  "type">: coq "Type"]

fun :: TypeDefinition
fun = define "Fun" $
  doc "A Coq fun (lambda abstraction) term: binders together with a body term" $
  T.record [
  "binders">: coq "OpenBinders",
  "body">: coq "Term"]

generalizingBinder :: TypeDefinition
generalizingBinder = define "GeneralizingBinder" $
  doc "A Coq generalizing binder, whose free variables are introduced as explicit or implicit arguments" $
  T.union [
  "explicit">:
    doc "Terms surrounded by `( ) introduce their free variables as explicit arguments" $
    coq "TypeclassConstraint",

  "implicitMaximallyInserted">:
    doc "Terms surrounded by `{ } introduce their free variables as maximally inserted implicit arguments" $
    coq "TypeclassConstraint",

  "implicitNonMaximallyInserted">:
    doc "Terms surrounded by `[ ] introduce them as non-maximally inserted implicit arguments" $
    coq "TypeclassConstraint"]

ident :: TypeDefinition
ident = define "Ident" $
  doc "A Coq identifier" $
  T.wrap $ coq "String"

identArg :: TypeDefinition
identArg = define "IdentArg" $
  doc "An identifier argument bound to a term, as used in existential-variable instantiation" $
  T.record [
  "ident">: coq "Ident",
  "term">: coq "Term"]

if_ :: TypeDefinition
if_ = define "If" $
  doc "Pattern match on boolean values" $
  T.record [
    "condition">: coq "Term",
    "returnAs">: T.optional $ coq "ReturnAs",
    "then">: coq "Term",
    "else">: coq "Term"]

implicitBinders :: TypeDefinition
implicitBinders = define "ImplicitBinders" $
  doc "In the context of a function definition, these forms specify that name is an implicit argument." $
  T.union [
    "maximallyInserted">:
      doc "The first form, with curly braces, makes name a maximally inserted implicit argument" $
      coq "TypeBinders",

    "nonMaximallyInserted">:
      doc "The second form, with square brackets, makes name a non-maximally inserted implicit argument." $
      coq "TypeBinders"]

importQualification :: TypeDefinition
importQualification = define "ImportQualification" $
  doc "Qualification for Require/Import commands" $
  T.union [
    "import">: T.unit,
    "export">: T.unit]

inductiveBody :: TypeDefinition
inductiveBody = define "InductiveBody" $
  doc "A single body in an Inductive definition (supports mutual induction via 'with')" $
  T.record [
    "name">: coq "Ident",
    "binders">: T.list $ coq "Binder",
    "type">: T.optional $ coq "Type",
    "constructors">: T.list $ coq "Constructor"]

inductiveDefinition :: TypeDefinition
inductiveDefinition = define "InductiveDefinition" $
  doc "An Inductive or CoInductive definition with one or more mutually inductive bodies" $
  T.record [
    "locality">: T.optional $ coq "Locality",
    "coinductive">: T.boolean,
    "bodies">: nonemptyList $ coq "InductiveBody"]

letBinder :: TypeDefinition
letBinder = define "LetBinder" $
  doc "Some constructions allow the binding of a variable to value. This is called a 'let-binder'." $
  T.record [
    "name">: coq "Name",
    "type">: T.optional $ coq "Type",
    "term">: coq "Term"]

letBindings :: TypeDefinition
letBindings = define "LetBindings" $
  doc "The bindings of a Coq let expression: a named binder or a destructuring pattern" $
  T.union [
  "named">: coq "LetNamed",
  "destructuring">: coq "LetDestructuring"]

letDestructuring :: TypeDefinition
letDestructuring = define "LetDestructuring" $
  doc "A destructuring pattern in a Coq let expression, in one of three grammar variants" $
  T.union [
  "variant1">: coq "LetDestructuring_Variant1",
  "variant2">: coq "LetDestructuring_Variant2",
  "variant3">: coq "LetDestructuring_Variant3"]

letDestructuring_Variant1 :: TypeDefinition
letDestructuring_Variant1 = define "LetDestructuring_Variant1" $
  doc "A let-destructuring binding a list of names, with an optional return-as clause" $
  T.record [
  "names">: T.list $ coq "Name",
  "returnAs">: T.optional $ coq "ReturnAs",
  "term">: coq "Term"]

letDestructuring_Variant2 :: TypeDefinition
letDestructuring_Variant2 = define "LetDestructuring_Variant2" $
  doc "A let-destructuring binding a single pattern, with an optional return type" $
  T.record [
  "pattern">: coq "Pattern",
  "term">: coq "Term",
  "return">: T.optional $ coq "Term100"]

letDestructuring_Variant3 :: TypeDefinition
letDestructuring_Variant3 = define "LetDestructuring_Variant3" $
  doc "A let-destructuring binding a pair of patterns, with a return type" $
  T.record [
  "pattern1">: coq "Pattern",
  "pattern2">: coq "Pattern",
  "term">: coq "Term",
  "return">: coq "Term100"]

letNamed :: TypeDefinition
letNamed = define "LetNamed" $
  doc "A named let-binder together with any additional binders" $
  T.record [
  "binder">: coq "LetBinder",
  "binders">: T.list $ coq "Binder"]

let_ :: TypeDefinition
let_ = define "Let" $
  doc "A let-in definition" $
  T.record [
    "bindings">: coq "LetBindings",
    "in">: coq "Term"]

locality :: TypeDefinition
locality = define "Locality" $
  doc "Local or Global qualifier for commands" $
  T.union [
    "local">: T.unit,
    "global">: T.unit]

match :: TypeDefinition
match = define "Match" $
  doc "A Coq match expression: case items, an optional return clause, and its equations" $
  T.record [
  "caseItems">: nonemptyList $ coq "CaseItem",
  "return">: T.optional $ coq "Term100",
  "pipe">: T.boolean,
  "equations">: T.list $ coq "Equation"]

moduleDefinition :: TypeDefinition
moduleDefinition = define "ModuleDefinition" $
  doc "A Module ... End block" $
  doc "A Module ... End block" $
  T.record [
    "name">: coq "Ident",
    "sentences">: T.list $ coq "Sentence"]

name_ :: TypeDefinition
name_ = define "Name" $
  doc "A Coq name: an optional identifier (absent for the wildcard name)" $
  T.wrap $ T.optional $ coq "Ident"

natural :: TypeDefinition
natural = define "Natural" $
  doc "A non-negative arbitrary-precision integer" $
  T.wrap T.bigint

naturalArg :: TypeDefinition
naturalArg = define "NaturalArg" $
  doc "A natural-number argument bound to a term, as used in existential-variable instantiation" $
  T.record [
  "natural">: coq "Natural",
  "term">: coq "Term"]

normalApplication :: TypeDefinition
normalApplication = define "NormalApplication" $
  doc "A Coq function application: a head term applied to one or more arguments" $
  T.record [
  "lhs">: coq "Term1",
  "rhs">: nonemptyList $ coq "Arg"]

notationDeclaration :: TypeDefinition
notationDeclaration = define "NotationDeclaration" $
  doc "A Notation declaration" $
  T.record [
    "notation">: coq "String",
    "definition">: coq "Term",
    "level">: T.optional $ coq "Natural",
    "associativity">: T.optional T.string]

number :: TypeDefinition
number = define "Number" $
  doc "A Coq floating-point number literal" $
  T.wrap T.float64

oneTerm :: TypeDefinition
oneTerm = define "OneTerm" $
  doc "A single Coq term, either an explicit qualified identifier or a lower-precedence term" $
  T.union [
  "explicit">: coq "QualidAnnotated",
  "term1">: coq "Term1"]

openBinders :: TypeDefinition
openBinders = define "OpenBinders" $
  doc "The binders of a Coq forall or fun term: a single typed group or a list of binders" $
  T.union [
  "type">: coq "TypeBinders",
  "binders">: T.list $ coq "Binder"]

pattern0 :: TypeDefinition
pattern0 = define "Pattern0" $
  doc "The lowest-precedence Coq pattern: a qualified identifier, applied pattern, wildcard, parenthesized patterns, or a literal" $
  T.union [
  "qualid">: coq "Qualid",
  "qualIdAndPattern">: coq "QualidAndPattern",
  "placeholder">: T.unit,
  "parens">: nonemptyList $ coq "Pattern",
  "number">: coq "Number",
  "string">: coq "String"]

pattern1 :: TypeDefinition
pattern1 = define "Pattern1" $
  doc "A Coq pattern together with an optional notation scope" $
  T.record [
  "pattern">: coq "Pattern0",
  "scope">: T.optional $ coq "ScopeKey"]

pattern10 :: TypeDefinition
pattern10 = define "Pattern10" $
  doc "The highest-precedence Coq pattern: an as-pattern, applied patterns, or a qualified identifier pattern" $
  T.union [
  "as">: coq "Pattern10_As",
  "patterns">: coq "Pattern10_Patterns",
  "qualiid">: coq "Pattern10_Qualid"]

pattern10_As :: TypeDefinition
pattern10_As = define "Pattern10_As" $
  doc "A Coq pattern bound to a name via an as-clause" $
  T.record [
  "pattern">: coq "Pattern1",
  "as">: coq "Name"]

pattern10_Patterns :: TypeDefinition
pattern10_Patterns = define "Pattern10_Patterns" $
  doc "A Coq pattern applied to one or more sub-patterns" $
  T.record [
  "pattern">: coq "Pattern1",
  "patterns">: T.list $ coq "Pattern1"]

pattern10_Qualid :: TypeDefinition
pattern10_Qualid = define "Pattern10_Qualid" $
  doc "A Coq pattern matching a qualified identifier applied to sub-patterns" $
  T.record [
  "qualid">: coq "Qualid",
  "patterns">: T.list $ coq "Pattern1"]

pattern_ :: TypeDefinition
pattern_ = define "Pattern" $
  doc "A Coq pattern: an inner pattern with an optional type ascription" $
  T.union [
  "pattern">: coq "Pattern10",
  "term">: T.optional $ coq "Term"]

primitiveNotations :: TypeDefinition
primitiveNotations = define "PrimitiveNotations" $
  doc "A Coq primitive notation literal: a number or a string" $
  T.union [
  "number">: coq "Number",
  "string">: coq "String"]

qualid :: TypeDefinition
qualid = define "Qualid" $
  doc "A qualified identifier" $
  T.record [
    "id">: coq "Ident",
    "fieldIds">: T.list $ coq "FieldIdent"]

qualidAndPattern :: TypeDefinition
qualidAndPattern = define "QualidAndPattern" $
  doc "A qualified identifier together with a pattern, used in existential-variable and match forms" $
  T.record [
  "qualid">: coq "Qualid",
  "pattern">: coq "Pattern"]

qualidAnnotated :: TypeDefinition
qualidAnnotated = define "QualidAnnotated" $
  doc "A qualified identifier with an optional universe annotation" $
  T.record [
  "qualid">: coq "Qualid",
  "univAnnot">: T.optional $ coq "UnivAnnot"]

recordBody :: TypeDefinition
recordBody = define "RecordBody" $
  doc "The body of a Record definition" $
  T.record [
    "constructor">: T.optional $ coq "Ident",
    "fields">: T.list $ coq "RecordField"]

recordDefinition :: TypeDefinition
recordDefinition = define "RecordDefinition" $
  doc "A Record or Structure definition" $
  T.record [
    "locality">: T.optional $ coq "Locality",
    "name">: coq "Ident",
    "binders">: T.list $ coq "Binder",
    "sort">: T.optional $ coq "Sort",
    "body">: coq "RecordBody"]

recordField :: TypeDefinition
recordField = define "RecordField" $
  doc "A field in a Record definition" $
  T.record [
    "name">: coq "Ident",
    "type">: coq "Type"]

requireImport :: TypeDefinition
requireImport = define "RequireImport" $
  doc "A Require Import/Export command" $
  T.record [
    "from">: T.optional $ coq "Qualid",
    "require">: T.boolean,
    "qualification">: T.optional $ coq "ImportQualification",
    "modules">: nonemptyList $ coq "Qualid"]

returnAs :: TypeDefinition
returnAs = define "ReturnAs" $
  doc "The return clause of a Coq match or let, with an optional as-name" $
  T.record [
  "as">: T.optional $ coq "Name",
  "return">: coq "Term100"]

scopeKey :: TypeDefinition
scopeKey = define "ScopeKey" $
  doc "The name of a Coq notation scope" $
  T.wrap $ coq "Ident"

sectionDefinition :: TypeDefinition
sectionDefinition = define "SectionDefinition" $
  doc "A Section ... End block" $
  T.record [
    "name">: coq "Ident",
    "sentences">: T.list $ coq "Sentence"]

sentence :: TypeDefinition
sentence = define "Sentence" $
  doc "A top-level sentence in a Coq document, optionally preceded by a comment" $
  T.record [
    "comment">: T.optional $ coq "Comment",
    "content">: coq "SentenceContent"]

sentenceContent :: TypeDefinition
sentenceContent = define "SentenceContent" $
  doc "The content of a top-level sentence" $
  T.union [
    "axiom">: coq "AxiomDeclaration",
    "definition">: coq "Definition",
    "fixpoint">: coq "FixpointDefinition",
    "inductive">: coq "InductiveDefinition",
    "module">: coq "ModuleDefinition",
    "notation">: coq "NotationDeclaration",
    "record">: coq "RecordDefinition",
    "requireImport">: coq "RequireImport",
    "section">: coq "SectionDefinition",
    "theorem">: coq "TheoremBody"]

sort :: TypeDefinition
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

string_ :: TypeDefinition
string_ = define "String" $
  doc "A Coq string literal" $
  T.wrap T.string

term :: TypeDefinition
term = define "Term" $
  doc "A Coq term: the top-level term grammar production, covering all term forms" $
  T.union [
  "forallOrFun">: coq "ForallOrFun",
  "let">: coq "Let",
  "if">: coq "If",
  "fix">: coq "Fix",
  "cofix">: coq "Cofix",
  "term100">: coq "Term100"]

term0 :: TypeDefinition
term0 = define "Term0" $
  doc "A Coq term at the lowest precedence level, covering atomic and bracketed forms" $
  T.union [
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

term1 :: TypeDefinition
term1 = define "Term1" $
  doc "A Coq term with optional trailing projection or scope annotations" $
  T.union [
  "projection">: T.unit,
  "scope">: T.unit,
  "term0">: coq "Term0"]

term10 :: TypeDefinition
term10 = define "Term10" $
  doc "A Coq term at application precedence: an application or a single term" $
  T.union [
  "application">: coq "Application",
  "oneTerm">: coq "OneTerm"]

term100 :: TypeDefinition
term100 = define "Term100" $
  doc "A Coq term with an optional trailing type cast" $
  T.union [
  "cast">: coq "TypeCast",
  "term10">: coq "Term10"]

theoremBody :: TypeDefinition
theoremBody = define "TheoremBody" $
  doc "A Theorem/Lemma/Proposition with a proof term" $
  T.record [
    "kind">: coq "TheoremKind",
    "name">: coq "Ident",
    "binders">: T.list $ coq "Binder",
    "type">: coq "Type",
    "proof">: coq "Term"]

theoremKind :: TypeDefinition
theoremKind = define "TheoremKind" $
  doc "The kind of theorem command" $
  T.union [
    "theorem">: T.unit,
    "lemma">: T.unit,
    "proposition">: T.unit,
    "corollary">: T.unit,
    "example">: T.unit]

typeBinders :: TypeDefinition
typeBinders = define "TypeBinders" $
  doc "A group of Coq binder names sharing a single type annotation" $
  T.record [
  "names">: nonemptyList $ coq "Name",
  "type">: coq "Type"]

typeCast :: TypeDefinition
typeCast = define "TypeCast" $
  doc "A Coq type-cast expression: a term, its ascribed type, and the cast operator used" $
  T.record [
  "term">: coq "Term10",
  "type">: coq "Type",
  "operator">: coq "TypeCastOperator"]

typeCastOperator :: TypeDefinition
typeCastOperator = define "TypeCastOperator" $
  doc "The operator of a Coq type-cast expression: normal, vm_compute, or native_compute checking" $
  T.union [
  "normal">:
    doc "The expression term10 : type is a type cast expression. It enforces the type of term10 to be type." T.unit,
  "vmCompute">:
    doc "term10 <: type specifies that the virtual machine will be used to type check that term10 has type type (see vm_compute)." T.unit,
  "nativeCompute">:
    doc "term10 <<: type specifies that compilation to OCaml will be used to type check that term10 has type type (see native_compute)." T.unit]

type_ :: TypeDefinition
type_ = define "Type" $
  doc "A Coq type, represented as a term" $
  T.wrap $ coq "Term"

typeclassConstraint :: TypeDefinition
typeclassConstraint = define "TypeclassConstraint" $
  doc "A Coq typeclass constraint binder, with an optional name and a generalizing flag" $
  T.record [
    "name">: T.optional $ coq "Name",
    "generalizing">: T.boolean,
    "term">: coq "Term"]

univAnnot :: TypeDefinition
univAnnot = define "UnivAnnot" $
  doc "A universe annotation: a list of universe levels" $
  T.wrap $ T.list $ coq "UniverseLevel"

universe :: TypeDefinition
universe = define "Universe" $
  doc "A Coq universe expression: a maximum of universe expressions, or a single universe expression" $
  T.union [
  "max">: nonemptyList $ coq "Universe_Expr",
  "expr">: coq "Universe_Expr"]

universeLevel :: TypeDefinition
universeLevel = define "UniverseLevel" $
  doc "A Coq universe level: Set, Prop, Type, an ignored level, or a qualified identifier" $
  T.union [
  "set">: T.unit,
  "prop">: T.unit,
  "type">: T.unit,
  "ignored">: T.unit,
  "qualid">: coq "Qualid"]

universeName :: TypeDefinition
universeName = define "UniverseName" $
  doc "The name component of a universe expression: a qualified identifier, Set, or Prop" $
  T.union [
  "qualid">: coq "Qualid",
  "set">: T.unit,
  "prop">: T.unit]

-- ===========================================================================
-- Vernacular commands (for generating complete .v files)
-- ===========================================================================

universe_Expr :: TypeDefinition
universe_Expr = define "Universe_Expr" $
  doc "A single universe expression: a universe name with an optional offset" $
  T.record [
  "name">: coq "UniverseName",
  "number">: T.optional $ coq "Natural"]
