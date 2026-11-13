{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Print.Core where

-- Standard imports for kernel terms modules (slightly modified for conflict avoidance)
import Hydra.Kernel hiding (literalType)
import qualified Hydra.Core.Dsl.Paths    as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Ordering as Ordering
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms hiding (
  binding, field, fields, fieldType, floatType, floatValue, injection, integerType,
  integerValue, lambda, literal, literalType, project, term, type_, typeScheme)
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Serialization as Serialization
import Hydra.Core.Ast


ns :: ModuleName
ns = ModuleName "hydra.core.print.model"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Serialization.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.core.model types")}
  where
   definitions = [
     toDefinition binding,
     toDefinition bindingAliased,
     toDefinition bindingExprAliased,
     toDefinition caseStatement,
     toDefinition caseStatementExprAliased,
     toDefinition either_,
     toDefinition escapeBacktickChar,
     toDefinition escapeStringChar,
     toDefinition field,
     toDefinition fieldExprAliased,
     toDefinition fieldTermIsUnit,
     toDefinition fieldType,
     toDefinition fieldTypeExpr,
     toDefinition fields,
     toDefinition floatValue,
     toDefinition floatType,
     toDefinition gatherLambdaBinders,
     toDefinition gatherLetChain,
     toDefinition gatherTypeLambdaBinders,
     toDefinition hexDigit,
     toDefinition injection,
     toDefinition injectionExprAliased,
     toDefinition integerValue,
     toDefinition integerType,
     toDefinition isBareName,
     toDefinition isBareSegment,
     toDefinition isBinderChainTerm,
     toDefinition isBracketedTerm,
     toDefinition isReservedWord,
     toDefinition isVariableTerm,
     toDefinition lambda,
     toDefinition lambdaExprAliased,
     toDefinition let_,
     toDefinition letAliased,
     toDefinition letExprAliased,
     toDefinition list_,
     toDefinition literal,
     toDefinition literalType,
     toDefinition (map_ :: TypedTermDefinition ((Int -> String) -> (v -> String) -> M.Map Int v -> String)),
     toDefinition optional_,
     toDefinition pair_,
     toDefinition peelDescriptionAnnotation,
     toDefinition peelTermDescriptionAnnotation,
     toDefinition printBacktickedName,
     toDefinition printBinderName,
     toDefinition printName,
     toDefinition printNameAliased,
     toDefinition printStringLit,
     toDefinition projection,
     toDefinition (set_ :: TypedTermDefinition ((Int -> String) -> S.Set Int -> String)),
     toDefinition sugarField,
     toDefinition sugarFieldExprAliased,
     toDefinition sugarFields,
     toDefinition sugarFieldsExprAliased,
     toDefinition term,
     toDefinition termAliased,
     toDefinition termAtPrec,
     toDefinition termExprAliased,
     toDefinition termExprAtPrec,
     toDefinition termExprAtPrecBudget,
     toDefinition termSignature,
     toDefinition termSignatureAliased,
     toDefinition toHex2,
     toDefinition type_,
     toDefinition typeAliased,
     toDefinition typeAtPrec,
     toDefinition typeScheme,
     toDefinition typeSchemeAliased]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- Precedence levels, tightest (highest) to loosest (0), per docs/specification/syntax.md #3:
--   3: postfix (@ annotation, <T> type application) -- also used for "must be atomic" contexts
--   2: application (adjacency, left-associative) -- also used for function-type domains
--   1: -> (right-associative)
--   0: binder bodies (lambda, type-lambda, let, forall) -- also top level
--
-- Every term/type constructor has an "own precedence": the loosest level at which it can be
-- printed without parentheses. Binder-shaped constructs (lambda, type-lambda, let, forall) are
-- 0; application (term or type) is 1; function types are 1; every other, self-delimited
-- construct (brackets or keywords) is 3 and is never parenthesized. A construct is wrapped in
-- parentheses exactly when its own precedence is looser than the minimum precedence required by
-- the position it is printed in (the "context").
precPostfix, precApp, precArrow, precMin :: Int
precPostfix = 3
precApp = 2
precArrow = 1
precMin = 0

-- | The flat-width floor below which an application spine always stays inline, even when the
-- caller's available-width budget has been reduced below this by deep nesting (#711 line-layout
-- pass, staging review point #3: a short call like 'f x' or 'ParseInput[offset] input' should
-- not break just because the surrounding context is tight).
minApplicationWidth :: Int
minApplicationWidth = 40

-- | Wrap a rendered construct in parentheses iff its own precedence is looser than the required
-- context. `ownPrec` is a Haskell-level constant (known at DSL-authoring time, one of the
-- `prec*` constants above); `ctx` is the DSL-level (TypedTerm) context precedence threaded
-- through `termAtPrec`/`typeAtPrec`'s recursive calls.
parenthesizeIfNeeded :: Int -> TypedTerm Int -> TypedTerm String -> TypedTerm String
parenthesizeIfNeeded ownPrec ctx s = Logic.ifElse (Ordering.lt (int32 ownPrec) ctx)
  (Strings.concat $ list [string "(", s, string ")"])
  s

-- | Like 'parenthesizeIfNeeded', but for the 'Expr'-based term printer (#711 line-layout pass):
-- wraps in inline parentheses (never a block style, matching the string-based version's
-- behavior) iff the construct's own precedence is looser than the required context.
parenthesizeIfNeededExpr :: Int -> TypedTerm Int -> TypedTerm Expr -> TypedTerm Expr
parenthesizeIfNeededExpr ownPrec ctx e = Logic.ifElse (Ordering.lt (int32 ownPrec) ctx)
  (Serialization.parens @@ e)
  e

-- | The empty import-alias map: resolving any name against it falls through to the full dotted
-- form (see 'printNameAliased'). Every public, no-module-context entry point ('term', 'type_',
-- 'typeScheme', 'termSignature', 'binding', 'let_') delegates to its '*Aliased' sibling with this,
-- so their output is byte-for-byte what it was before #803.
noAliases :: TypedTerm (M.Map ModuleName String)
noAliases = Maps.empty

binding :: TypedTermDefinition (Binding -> String)
binding = define "binding" $
  doc "Show a binding as a string" $
  bindingAliased @@ noAliases

bindingAliased :: TypedTermDefinition (M.Map ModuleName String -> Binding -> String)
bindingAliased = define "bindingAliased" $
  doc "Show a binding as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "el" ~>
  "name" <~ printName @@ (Core.bindingName $ var "el") $
  "t" <~ Core.bindingTerm (var "el") $
  "typeStr" <~ Optionals.match (Core.bindingTypeScheme $ var "el") (string "") ("ts" ~> Strings.concat (list [string ":(", typeSchemeAliased @@ var "aliases" @@ var "ts", string ")"])) $
  Strings.concat $ list [
    var "name",
    var "typeStr",
    string " := ",
    termAliased @@ var "aliases" @@ var "t"]

-- | Like 'bindingAliased', but builds an 'Expr' (#711 line-layout pass) so a long bound value can
-- break onto following lines, indented, via 'Serialization.indentSubsequentLines'.
-- | Like 'bindingAliased', but builds an 'Expr' (#711 line-layout pass) so a long bound value can
-- break onto following lines, indented one level past the 'let' (heuristic #3): the binder
-- ('name := ') stays on the 'let' line, and a value that does not fit prints on the following
-- line(s), indented. 'budget' is the number of columns already consumed on the current line by
-- the surrounding context (e.g. 'let ') before this binding's own text starts -- subtracted from
-- 'Serialization.maxLineWidth' at each nested fit-or-break decision so a construct that measures
-- as "fits" in isolation does not silently overflow once its real on-page column is accounted
-- for (the limitation 'Serialization.chooseLayout' documents and asks callers to compensate for).
bindingExprAliased :: TypedTermDefinition (Int -> M.Map ModuleName String -> Binding -> Expr)
bindingExprAliased = define "bindingExprAliased" $
  doc "Show a binding as a line-layout expression, shortening nominal references via the given import-alias map (#803)" $
  "budget" ~> "aliases" ~> "el" ~>
  "name" <~ printName @@ (Core.bindingName $ var "el") $
  "t" <~ Core.bindingTerm (var "el") $
  "typeStr" <~ Optionals.match (Core.bindingTypeScheme $ var "el") (string "") ("ts" ~> Strings.concat (list [string ":(", typeSchemeAliased @@ var "aliases" @@ var "ts", string ")"])) $
  "headStr" <~ Strings.concat (list [var "name", var "typeStr", string " := "]) $
  "valueBudget" <~ Math.sub (var "budget") (Strings.length $ var "headStr") $
  "valueExpr" <~ termExprAtPrecBudget @@ var "valueBudget" @@ var "aliases" @@ int32 precMin @@ var "t" $
  "headOp" <~ Ast.op
    (Serialization.sym @@ string "")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  Logic.ifElse (isBracketedTerm @@ var "t")
    (Serialization.ifx @@ var "headOp" @@ (Serialization.cst @@ var "headStr") @@ var "valueExpr")
    (Serialization.indentSubsequentLines @@ Serialization.doubleSpace @@
      (Serialization.ifx @@ var "headOp" @@ (Serialization.cst @@ var "headStr") @@ var "valueExpr"))

caseStatement :: TypedTermDefinition (M.Map ModuleName String -> CaseStatement -> String)
caseStatement = define "caseStatement" $
  doc "Show a case statement as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "cs" ~>
  "tname" <~ printNameAliased @@ var "aliases" @@ (Core.caseStatementTypeName $ var "cs") $
  "mdef" <~ Core.caseStatementDefault (var "cs") $
  "csCases" <~ Core.caseStatementCases (var "cs") $
  "caseFields" <~ Lists.map
    ("alt" ~> Core.field (Core.caseAlternativeName $ var "alt") (Core.caseAlternativeHandler $ var "alt"))
    (var "csCases") $
  "defaultField" <~ Optionals.match (var "mdef") (list ([] :: [TypedTerm Field])) ("d" ~> list [Core.field (Core.name $ string "_") (var "d")]) $
  "allFields" <~ Lists.concat (list [var "caseFields", var "defaultField"]) $
  Strings.concat $ list [
    string "cases(",
    var "tname",
    string ")",
    fields @@ var "aliases" @@ var "allFields"]

-- | Like 'caseStatement', but builds an 'Expr' (#711 line-layout pass): the branch list breaks
-- one branch per line, indented, when it does not fit inline (heuristic #7). 'budget' is the
-- number of columns already consumed on the current line before this cases-statement's own text
-- starts (see 'bindingExprAliased' for why this is threaded through).
caseStatementExprAliased :: TypedTermDefinition (Int -> M.Map ModuleName String -> CaseStatement -> Expr)
caseStatementExprAliased = define "caseStatementExprAliased" $
  doc "Show a case statement as a line-layout expression, shortening nominal references via the given import-alias map (#803)" $
  "budget" ~> "aliases" ~> "cs" ~>
  "tname" <~ printNameAliased @@ var "aliases" @@ (Core.caseStatementTypeName $ var "cs") $
  "mdef" <~ Core.caseStatementDefault (var "cs") $
  "csCases" <~ Core.caseStatementCases (var "cs") $
  "caseFields" <~ Lists.map
    ("alt" ~> Core.field (Core.caseAlternativeName $ var "alt") (Core.caseAlternativeHandler $ var "alt"))
    (var "csCases") $
  "defaultField" <~ Optionals.match (var "mdef") (list ([] :: [TypedTerm Field])) ("d" ~> list [Core.field (Core.name $ string "_") (var "d")]) $
  "allFields" <~ Lists.concat (list [var "caseFields", var "defaultField"]) $
  "headStr" <~ Strings.concat (list [string "cases(", var "tname", string ")"]) $
  "fieldBudget" <~ Math.sub (Math.sub (var "budget") (Strings.length $ var "headStr")) (int32 2) $
  "fieldExprs" <~ Lists.map (fieldExprAliased @@ var "fieldBudget" @@ var "aliases") (var "allFields") $
  "headExpr" <~ Serialization.cst @@ var "headStr" $
  Serialization.ifx @@ (Ast.op
    (Serialization.sym @@ string "")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone) @@ var "headExpr" @@
    (Serialization.chooseLayout @@ var "budget"
      @@ (Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ var "fieldExprs")
      @@ (Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@ var "fieldExprs"))

either_ :: TypedTermDefinition ((a -> String) -> (b -> String) -> Prelude.Either a b -> String)
either_ = define "either" $
  doc "Show an Either value using given functions for left and right" $
  "showA" ~> "showB" ~> "e" ~>
  Eithers.either
    ("a" ~> Strings.concat2 (string "left(") (Strings.concat2 (var "showA" @@ var "a") (string ")")))
    ("b" ~> Strings.concat2 (string "right(") (Strings.concat2 (var "showB" @@ var "b") (string ")")))
    (var "e")

field :: TypedTermDefinition (M.Map ModuleName String -> Field -> String)
field = define "field" $
  doc "Show a field as a string, shortening nominal references in its term via the given alias map (#803)" $
  "aliases" ~> "field" ~>
  "fname" <~ printName @@ (Core.fieldName $ var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  Strings.concat $ list [var "fname", string "=", termAliased @@ var "aliases" @@ var "fterm"]

-- | Like 'field', but builds an 'Expr' (#711 line-layout pass) so a long term value can break
-- onto following lines, indented. 'budget' is the number of columns already consumed on the
-- current line before this field's own text starts (see 'bindingExprAliased').
-- | True iff a term's own line-layout 'Expr' self-indents its continuation lines via a bracket's
-- block style (records, lists, sets, maps), so a surrounding 'name = <value>'/'x := <value>'
-- wrapper must NOT also apply 'Serialization.indentSubsequentLines' -- doing so would double the
-- indent (#711 line-layout pass, staging review point #3: nested record fields were stepping +4
-- instead of +2). Only terms whose OWN printed form is a flat multi-line chain with no
-- self-indenting bracket (an application spine, a let/lambda) need the wrapper's extra indent.
isBracketedTerm :: TypedTermDefinition (Term -> Bool)
isBracketedTerm = define "isBracketedTerm" $
  doc "True iff a term prints as a self-indenting bracketed construct (record/list/set/map), for #711 double-indent avoidance" $
  "t" ~> match _Term (var "t") (Just false) [
    _Term_record>>: constant true,
    _Term_list>>: constant true,
    _Term_set>>: constant true,
    _Term_map>>: constant true,
    _Term_inject>>: constant true,
    _Term_wrap>>: constant true]

fieldExprAliased :: TypedTermDefinition (Int -> M.Map ModuleName String -> Field -> Expr)
fieldExprAliased = define "fieldExprAliased" $
  doc "Show a field as a line-layout expression, shortening nominal references in its term via the given alias map (#803)" $
  "budget" ~> "aliases" ~> "field" ~>
  "fname" <~ printName @@ (Core.fieldName $ var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  "valueBudget" <~ Math.sub (var "budget") (Math.add (Strings.length $ var "fname") (int32 1)) $
  "valueExpr" <~ termExprAtPrecBudget @@ var "valueBudget" @@ var "aliases" @@ int32 precMin @@ var "fterm" $
  "eqOp" <~ Ast.op
    (Serialization.sym @@ string "=")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  Logic.ifElse (isBracketedTerm @@ var "fterm")
    (Serialization.ifx @@ var "eqOp" @@ (Serialization.cst @@ var "fname") @@ var "valueExpr")
    (Serialization.indentSubsequentLines @@ Serialization.doubleSpace @@
      (Serialization.ifx @@ var "eqOp" @@ (Serialization.cst @@ var "fname") @@ var "valueExpr"))

-- | Peel a single '@{description = "..."}' annotation off the top of a type, if present, and
-- return its description string. The annotation payload is checked directly (a one-entry TermMap
-- whose key is the bare variable "description" and whose value is a string literal) rather than
-- via the heavier 'Hydra.Sources.Kernel.Terms.Annotations' decoders -- those need an
-- InferenceContext/Graph for the general case (any annotation key, term-valued), but a type's
-- doc-comment description (set by the DSL's own 'doc' helper, Core.Overlay.Haskell.Dsl.Annotations)
-- is always exactly this shape, so a direct pattern match suffices here and avoids pulling in
-- that dependency. Returns the annotation's body type unchanged, paired with the description, so
-- the caller can print the UNANNOTATED type and emit the description as a preceding '##' comment
-- instead of a trailing '@{...}' suffix (#711 field-layout readability pass).
peelDescriptionAnnotation :: TypedTermDefinition (Type -> (Maybe String, Type))
peelDescriptionAnnotation = define "peelDescriptionAnnotation" $
  doc "Peel a single '@{description = ...}' annotation off a type, returning (description, body) if found" $
  "typ" ~> match _Type (var "typ") (Just $ pair nothing (var "typ")) [
    _Type_annotated>>: "at" ~>
      "body" <~ Core.annotatedTypeBody (var "at") $
      "ann" <~ Core.annotatedTypeAnnotation (var "at") $
      match _Term (var "ann") (Just $ pair nothing (var "typ")) [
        _Term_map>>: "m" ~>
          "entries" <~ Maps.toList (var "m" :: TypedTerm (M.Map Term Term)) $
          Logic.ifElse (Equality.equal (Lists.length $ var "entries") (int32 1))
            ("entry" <~ (Optionals.withDefault (pair (Core.termUnit) (Core.termUnit)) (Lists.head $ var "entries")) $
             "k" <~ Pairs.first (var "entry") $
             "v" <~ Pairs.second (var "entry") $
             match _Term (var "k") (Just $ pair nothing (var "typ")) [
               _Term_variable>>: "n" ~>
                 Logic.ifElse (Equality.equal (Core.unName $ var "n") (string "description"))
                   (match _Term (var "v") (Just $ pair nothing (var "typ")) [
                     _Term_literal>>: "l" ~> match _Literal (var "l") (Just $ pair nothing (var "typ")) [
                       _Literal_string>>: "s" ~> pair (just $ var "s") (var "body")]])
                   (pair nothing (var "typ"))])
            (pair nothing (var "typ"))]]

-- | Peel a single 'description' entry off a term's OUTERMOST '@{...}' annotation map, if
-- present, returning (description, remaining term). Unlike 'peelDescriptionAnnotation' (types),
-- a term-level annotation map may carry OTHER keys alongside 'description' (#711 term
-- doc-comment pass, second readability request): only the 'description' entry is removed, and
-- the rest are kept in a trailing '@{...}' suffix on the returned term, so a term definition's
-- description always moves to a leading '## ...' comment while any other metadata stays inline.
-- An inner (non-outermost) description annotation is untouched, since this only inspects the
-- term's own top-level constructor.
peelTermDescriptionAnnotation :: TypedTermDefinition (Term -> (Maybe String, Term))
peelTermDescriptionAnnotation = define "peelTermDescriptionAnnotation" $
  doc "Peel a 'description' entry off a term's outermost '@{...}' annotation map, if present" $
  "t" ~> match _Term (var "t") (Just $ pair nothing (var "t")) [
    _Term_annotated>>: "at" ~>
      "body" <~ Core.annotatedTermBody (var "at") $
      "ann" <~ Core.annotatedTermAnnotation (var "at") $
      match _Term (var "ann") (Just $ pair nothing (var "t")) [
        _Term_map>>: "m" ~>
          "descKey" <~ Core.termVariable (Core.name $ string "description") $
          "mdescVal" <~ Maps.lookup (var "descKey") (var "m" :: TypedTerm (M.Map Term Term)) $
          "descStr" <~ Optionals.match (var "mdescVal")
            nothing
            ("v" ~> match _Term (var "v") (Just nothing) [
              _Term_literal>>: "l" ~> match _Literal (var "l") (Just nothing) [
                _Literal_string>>: "s" ~> just (var "s")]]) $
          Optionals.match (var "descStr")
            (pair nothing (var "t"))
            ("s" ~>
              "rest" <~ Maps.delete (var "descKey") (var "m" :: TypedTerm (M.Map Term Term)) $
              "remainingTerm" <~ Logic.ifElse (Maps.isEmpty (var "rest" :: TypedTerm (M.Map Term Term)))
                (var "body")
                (Core.termAnnotated $ Core.annotatedTerm (var "body") (Core.termMap $ var "rest")) $
              pair (just $ var "s") (var "remainingTerm"))]]

fieldType :: TypedTermDefinition (M.Map ModuleName String -> FieldType -> String)
fieldType = define "fieldType" $
  doc "Show a field type as a string, shortening nominal references in its type via the given alias map (#803)" $
  "aliases" ~> "ft" ~>
  "fname" <~ printName @@ (Core.fieldTypeName $ var "ft") $
  "ftyp" <~ Core.fieldTypeType (var "ft") $
  "peeled" <~ peelDescriptionAnnotation @@ var "ftyp" $
  Strings.concat $ list [
    var "fname",
    string ": ",
    typeAliased @@ var "aliases" @@ (Pairs.second $ var "peeled")]

-- | Show a field type as a block-layout 'Expr' (#711): a preceding '## description' comment line
-- (if the field's type carries a description annotation), then the 'name: type' declaration line.
-- Used by the record/union row-type printer to lay out each field on its own line, indented,
-- rather than inline with a trailing '@{description=...}' suffix.
fieldTypeExpr :: TypedTermDefinition (M.Map ModuleName String -> FieldType -> Expr)
fieldTypeExpr = define "fieldTypeExpr" $
  doc "Show a field type as a block-layout expression, with its description (if any) as a preceding '##' comment line" $
  "aliases" ~> "ft" ~>
  "ftyp" <~ Core.fieldTypeType (var "ft") $
  "peeled" <~ peelDescriptionAnnotation @@ var "ftyp" $
  "mdesc" <~ Pairs.first (var "peeled") $
  "fname" <~ printName @@ (Core.fieldTypeName $ var "ft") $
  "declLine" <~ Strings.concat (list [
    var "fname",
    string ": ",
    typeAliased @@ var "aliases" @@ (Pairs.second $ var "peeled")]) $
  Serialization.cst @@ Optionals.match (var "mdesc")
    (var "declLine")
    ("d" ~> Strings.concat $ list [string "## ", var "d", string "\n", var "declLine"])

fields :: TypedTermDefinition (M.Map ModuleName String -> [Field] -> String)
fields = define "fields" $
  doc "Show a list of fields as a string, shortening nominal references via the given alias map (#803)" $
  "aliases" ~> "flds" ~>
  "fieldStrs" <~ Lists.map (field @@ var "aliases") (var "flds") $
  Strings.concat $ list [
    string "{",
    Strings.join (string ", ") (var "fieldStrs"),
    string "}"]

-- | True iff a term is the unit term (Term.unit) -- used by 'sugarFields' to decide whether a field
-- may be abbreviated bareword ('name' instead of 'name = unit'), #809. Named 'fieldTermIsUnit' (not
-- 'isUnitTerm') to avoid colliding with the pre-existing kernel predicate hydra.core.predicates.
-- isUnitTerm, which is re-exported via Hydra.Kernel; a local define of the same name is ambiguous.
fieldTermIsUnit :: TypedTermDefinition (Term -> Bool)
fieldTermIsUnit = define "fieldTermIsUnit" $
  doc "True iff a term is the unit term (for bareword unit-field sugar, #809)" $
  "t" ~> match _Term (var "t") (Just false) [
    _Term_unit>>: constant true]

-- | True iff a term is a bare variable reference (Term.variable) -- used by the wrap-sugar printer
-- to fall back to the keyword form when the wrapped body would otherwise reparse as a one-bare-field
-- record (#809).
isVariableTerm :: TypedTermDefinition (Term -> Bool)
isVariableTerm = define "isVariableTerm" $
  doc "True iff a term is a bare variable reference (for wrap-sugar disambiguation, #809)" $
  "t" ~> match _Term (var "t") (Just false) [
    _Term_variable>>: constant true]

-- | Show a list of fields in nominal-construction-sugar form (#809): '{name = term, ...}', but a
-- unit-valued field abbreviates to a bareword 'name' (the unit-field shorthand). Shared by the
-- record and injection sugar printers. The brace style (no keyword wrapper) is what distinguishes
-- sugar from the legacy 'fields' helper, which this otherwise mirrors.
sugarFields :: TypedTermDefinition (M.Map ModuleName String -> [Field] -> String)
sugarFields = define "sugarFields" $
  doc "Show fields as nominal-construction sugar, abbreviating unit-valued fields bareword (#809)" $
  "aliases" ~> "flds" ~>
  "fieldStrs" <~ Lists.map (sugarField @@ var "aliases") (var "flds") $
  Strings.concat $ list [
    string "{",
    Strings.join (string ", ") (var "fieldStrs"),
    string "}"]

-- | Like 'sugarFields', but builds an 'Expr' (#711 line-layout pass): the field list breaks one
-- field per line, indented, when it does not fit inline (heuristic #6). 'budget' is the number of
-- columns already consumed on the current line before this field list's own text starts (see
-- 'bindingExprAliased').
sugarFieldsExprAliased :: TypedTermDefinition (Int -> M.Map ModuleName String -> [Field] -> Expr)
sugarFieldsExprAliased = define "sugarFieldsExprAliased" $
  doc "Show fields as a line-layout nominal-construction-sugar expression, abbreviating unit-valued fields bareword (#809)" $
  "budget" ~> "aliases" ~> "flds" ~>
  "fieldExprs" <~ Lists.map (sugarFieldExprAliased @@ (Math.sub (var "budget") (int32 2)) @@ var "aliases") (var "flds") $
  Serialization.chooseLayout @@ var "budget"
    @@ (Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ var "fieldExprs")
    @@ (Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@ var "fieldExprs")

-- | Show a single field in sugar form: 'name' if its term is unit (bareword shorthand), else
-- 'name = term' (#809).
sugarField :: TypedTermDefinition (M.Map ModuleName String -> Field -> String)
sugarField = define "sugarField" $
  doc "Show a single field in sugar form: bareword 'name' for a unit value, else 'name = term' (#809)" $
  "aliases" ~> "field" ~>
  "fname" <~ printName @@ (Core.fieldName $ var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  Logic.ifElse (fieldTermIsUnit @@ var "fterm")
    (var "fname")
    (Strings.concat $ list [var "fname", string " = ", termAliased @@ var "aliases" @@ var "fterm"])

-- | Like 'sugarField', but builds an 'Expr' (#711 line-layout pass) so a long term value can
-- break onto following lines, indented. 'budget' is the number of columns already consumed on
-- the current line before this field's own text starts (see 'bindingExprAliased').
sugarFieldExprAliased :: TypedTermDefinition (Int -> M.Map ModuleName String -> Field -> Expr)
sugarFieldExprAliased = define "sugarFieldExprAliased" $
  doc "Show a single field as a line-layout sugar expression: bareword 'name' for a unit value, else 'name = term' (#809)" $
  "budget" ~> "aliases" ~> "field" ~>
  "fname" <~ printName @@ (Core.fieldName $ var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  Logic.ifElse (fieldTermIsUnit @@ var "fterm")
    (Serialization.cst @@ var "fname")
    ("valueBudget" <~ Math.sub (var "budget") (Math.add (Strings.length $ var "fname") (int32 3)) $
     "valueExpr" <~ termExprAtPrecBudget @@ var "valueBudget" @@ var "aliases" @@ int32 precMin @@ var "fterm" $
     "eqOp" <~ Ast.op
       (Serialization.sym @@ string "=")
       (Ast.padding Ast.wsSpace Ast.wsSpace)
       (Ast.precedence $ int32 0)
       Ast.associativityNone $
     Logic.ifElse (isBracketedTerm @@ var "fterm")
       (Serialization.ifx @@ var "eqOp" @@ (Serialization.cst @@ var "fname") @@ var "valueExpr")
       (Serialization.indentSubsequentLines @@ Serialization.doubleSpace @@
         (Serialization.ifx @@ var "eqOp" @@ (Serialization.cst @@ var "fname") @@ var "valueExpr")))

floatValue :: TypedTermDefinition (FloatValue -> String)
floatValue = define "float" $
  doc "Show a float value as a string" $
  "fv" ~> match _FloatValue (var "fv") Nothing [
    _FloatValue_float32>>: "v" ~> Literals.printFloat32 (var "v") ++ (string ":float32"),
    _FloatValue_float64>>: "v" ~> Literals.printFloat64 (var "v") ++ (string ":float64")]

floatType :: TypedTermDefinition (FloatType -> String)
floatType = define "floatType" $
  doc "Show a float type as a string" $
  "ft" ~> match _FloatType (var "ft") Nothing [
    _FloatType_float32>>: constant $ string "float32",
    _FloatType_float64>>: constant $ string "float64"]

injection :: TypedTermDefinition (M.Map ModuleName String -> Injection -> String)
injection = define "injection" $
  doc "Show an injection as a string, shortening the type name via the given import-alias map (#803)" $
  "aliases" ~> "inj" ~>
  "tname" <~ Core.injectionTypeName (var "inj") $
  "f" <~ Core.injectionField (var "inj") $
  -- #809: injection construction sugar 'Name{field}' -- the SAME one-field record-style construct as
  -- a record; the reader corrects it back to an injection against the schema graph (one field over a
  -- union type). Unit-valued variants abbreviate bareword ('Color{red}') via 'sugarFields'.
  Strings.concat $ list [
    printNameAliased @@ var "aliases" @@ var "tname",
    sugarFields @@ var "aliases" @@ (list [var "f"])]

-- | Like 'injection', but builds an 'Expr' (#711 line-layout pass).
injectionExprAliased :: TypedTermDefinition (Int -> M.Map ModuleName String -> Injection -> Expr)
injectionExprAliased = define "injectionExprAliased" $
  doc "Show an injection as a line-layout expression, shortening the type name via the given import-alias map (#803)" $
  "budget" ~> "aliases" ~> "inj" ~>
  "tname" <~ Core.injectionTypeName (var "inj") $
  "f" <~ Core.injectionField (var "inj") $
  "tnameStr" <~ printNameAliased @@ var "aliases" @@ var "tname" $
  Serialization.ifx @@ (Ast.op
    (Serialization.sym @@ string "")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone) @@
    (Serialization.cst @@ var "tnameStr") @@
    (sugarFieldsExprAliased @@ (Math.sub (var "budget") (Strings.length $ var "tnameStr")) @@ var "aliases" @@ (list [var "f"]))

integerValue :: TypedTermDefinition (IntegerValue -> String)
integerValue = define "integer" $
  doc "Show an integer value as a string" $
  "iv" ~> match _IntegerValue (var "iv") Nothing [
    _IntegerValue_bigint>>: "v" ~> Literals.printBigint (var "v") ++ (string ":bigint"),
    _IntegerValue_int8>>: "v" ~> Literals.printInt8 (var "v") ++ (string ":int8"),
    _IntegerValue_int16>>: "v" ~> Literals.printInt16 (var "v") ++ (string ":int16"),
    _IntegerValue_int32>>: "v" ~> Literals.printInt32 (var "v") ++ (string ":int32"),
    _IntegerValue_int64>>: "v" ~> Literals.printInt64 (var "v") ++ (string ":int64"),
    _IntegerValue_uint8>>: "v" ~> Literals.printUint8 (var "v") ++ (string ":uint8"),
    _IntegerValue_uint16>>: "v" ~> Literals.printUint16 (var "v") ++ (string ":uint16"),
    _IntegerValue_uint32>>: "v" ~> Literals.printUint32 (var "v") ++ (string ":uint32"),
    _IntegerValue_uint64>>: "v" ~> Literals.printUint64 (var "v") ++ (string ":uint64")]

integerType :: TypedTermDefinition (IntegerType -> String)
integerType = define "integerType" $
  doc "Show an integer type as a string" $
  "it" ~> match _IntegerType (var "it") Nothing [
    _IntegerType_bigint>>: constant $ string "bigint",
    _IntegerType_int8>>: constant $ string "int8",
    _IntegerType_int16>>: constant $ string "int16",
    _IntegerType_int32>>: constant $ string "int32",
    _IntegerType_int64>>: constant $ string "int64",
    _IntegerType_uint8>>: constant $ string "uint8",
    _IntegerType_uint16>>: constant $ string "uint16",
    _IntegerType_uint32>>: constant $ string "uint32",
    _IntegerType_uint64>>: constant $ string "uint64"]

-- | The reserved words of docs/specification/syntax.md #2.7, verbatim, plus the module-grammar
-- keywords of #710's (not yet written) #6 ('module', 'import', 'as', 'from', 'type', 'term',
-- 'primitive', 'impure', 'partial') -- reserved for the same reason: a bare name matching one of
-- these is ambiguous with the keyword at any definition boundary (e.g. a plain, non-generic
-- primitive's result type followed by a blank line and the next 'primitive' keyword parses, with
-- no reservation, as a 3-way type application swallowing 'primitive' and the next identifier as
-- adjacency arguments -- confirmed via #711's round-trip harness). A bare name matching one of
-- these must be backtick-escaped.
reservedWords :: TypedTerm (S.Set String)
reservedWords = Sets.fromList $ list [
  string "_", string "as", string "bigint", string "binary", string "boolean", string "cases",
  string "decimal", string "do", string "effect", string "either", string "false", string "float32",
  string "float64", string "forall", string "from", string "given", string "impure", string "in",
  string "import", string "inject", string "int8", string "int16", string "int32", string "int64",
  string "left", string "let", string "list", string "map", string "module", string "none",
  string "optional", string "partial", string "primitive", string "project", string "record",
  string "right", string "set", string "string", string "term", string "true", string "type",
  string "uint8", string "uint16", string "uint32", string "uint64", string "union", string "unit",
  string "unwrap", string "void", string "wrap", string "NaN", string "Infinity"]

-- | True iff a name is bare: a nonempty, dot-separated sequence of bare segments (#2.4).
isBareName :: TypedTermDefinition (String -> Bool)
isBareName = define "isBareName" $
  doc "True iff a name is a nonempty sequence of dot-separated segments of alphanumeric characters." $
  "s" ~>
  "segs" <~ Strings.splitOn (string ".") (var "s") $
  Logic.and
    (Logic.not (Lists.isEmpty $ var "segs"))
    (Lists.foldl
      ("acc" ~> "seg" ~> Logic.and (var "acc") (isBareSegment @@ var "seg"))
      true
      (var "segs"))

-- | True iff every character of a name segment is alphanumeric (the bare alphabet of #2.4) and
-- the segment is nonempty.
isBareSegment :: TypedTermDefinition (String -> Bool)
isBareSegment = define "isBareSegment" $
  doc "True iff a name segment is a nonempty sequence of alphanumeric characters (the bare alphabet)." $
  "seg" ~>
  Logic.and
    (Logic.not (Strings.isEmpty $ var "seg"))
    (Lists.foldl
      ("acc" ~> "c" ~> Logic.and (var "acc") (Chars.isAlphaNum (var "c")))
      true
      (Strings.toList $ var "seg"))

-- | True iff a bare name is one of the reserved words of #2.7.
isReservedWord :: TypedTermDefinition (String -> Bool)
isReservedWord = define "isReservedWord" $
  doc "True iff the given string is a reserved word which must be backtick-escaped when used as a name." $
  "s" ~> Sets.member (var "s") reservedWords

-- | The 16 lowercase hex digits, indexed 0-15, for building \u00xx escapes by hand (no
-- hex-formatting primitive exists in hydra.lib).
hexDigits :: TypedTerm [String]
hexDigits = list [
  string "0", string "1", string "2", string "3", string "4", string "5", string "6", string "7",
  string "8", string "9", string "a", string "b", string "c", string "d", string "e", string "f"]

-- | Look up a hex digit by its nibble value (0-15); total in this module's usage since every
-- caller derives the index from `div`/`mod` by 16 on a value already known to be nonnegative.
hexDigit :: TypedTermDefinition (Int -> String)
hexDigit = define "hexDigit" $
  doc "Look up a hex digit (0-15) as a lowercase hex character." $
  "n" ~> Optionals.withDefault (string "0") (Lists.at (var "n") hexDigits)

-- | Render a code point in [0, 31] as a two-digit lowercase hex string (for \u00xx escapes).
toHex2 :: TypedTermDefinition (Int -> String)
toHex2 = define "toHex2" $
  doc "Render a small code point as a two-digit lowercase hex string." $
  "c" ~>
  "hi" <~ Optionals.withDefault (int32 0) (Math.div (var "c") (int32 16)) $
  "lo" <~ Optionals.withDefault (int32 0) (Math.mod (var "c") (int32 16)) $
  Strings.concat $ list [hexDigit @@ var "hi", hexDigit @@ var "lo"]

-- | Escape a single character for backticked-name content, per #2.5's escape set applied to the
-- backtick delimiter (in place of the double-quote delimiter #2.5 itself uses): the delimiter
-- character (backtick) and backslash are always escaped, the five shortcut escapes are used for
-- the corresponding control characters, and any other control character uses \u00xx (lowercase
-- hex); every other code point (including all non-ASCII) is emitted as-is.
escapeBacktickChar :: TypedTermDefinition (Int -> String)
escapeBacktickChar = define "escapeBacktickChar" $
  doc "Escape a single code point for backticked-name content." $
  "c" ~>
  "isChar" <~ ("code" ~> Equality.equal (var "c") (var "code")) $
  Logic.ifElse (var "isChar" @@ int32 96) (string "\\`") $      -- `
  Logic.ifElse (var "isChar" @@ int32 92) (string "\\\\") $     -- backslash
  Logic.ifElse (var "isChar" @@ int32 8)  (string "\\b") $      -- backspace
  Logic.ifElse (var "isChar" @@ int32 12) (string "\\f") $      -- form feed
  Logic.ifElse (var "isChar" @@ int32 10) (string "\\n") $      -- line feed
  Logic.ifElse (var "isChar" @@ int32 13) (string "\\r") $      -- carriage return
  Logic.ifElse (var "isChar" @@ int32 9)  (string "\\t") $      -- tab
  Logic.ifElse (Ordering.lt (var "c") (int32 32))
    (Strings.concat $ list [string "\\u00", toHex2 @@ var "c"])
    (Strings.fromList $ list [var "c"])

-- | Escape a single character for double-quoted string content, per #2.5's escape set: the
-- delimiter character (double quote) and backslash are always escaped, the five shortcut escapes
-- are used for the corresponding control characters, and any other control character uses
-- \u00xx (lowercase hex); every other code point (including all non-ASCII) is emitted as-is, per
-- the JSON wire format's documented contract (docs/specification/json-format.md#string-escapes) --
-- NOT 'hydra.lib.literals.printString', whose own documented contract (Haskell-syntax decimal
-- escapes for non-ASCII) is for general host-side display, not for the textual syntax.
escapeStringChar :: TypedTermDefinition (Int -> String)
escapeStringChar = define "escapeStringChar" $
  doc "Escape a single code point for double-quoted string content." $
  "c" ~>
  "isChar" <~ ("code" ~> Equality.equal (var "c") (var "code")) $
  Logic.ifElse (var "isChar" @@ int32 34) (string "\\\"") $     -- "
  Logic.ifElse (var "isChar" @@ int32 92) (string "\\\\") $     -- backslash
  Logic.ifElse (var "isChar" @@ int32 8)  (string "\\b") $      -- backspace
  Logic.ifElse (var "isChar" @@ int32 12) (string "\\f") $      -- form feed
  Logic.ifElse (var "isChar" @@ int32 10) (string "\\n") $      -- line feed
  Logic.ifElse (var "isChar" @@ int32 13) (string "\\r") $      -- carriage return
  Logic.ifElse (var "isChar" @@ int32 9)  (string "\\t") $      -- tab
  Logic.ifElse (Ordering.lt (var "c") (int32 32))
    (Strings.concat $ list [string "\\u00", toHex2 @@ var "c"])
    (Strings.fromList $ list [var "c"])

-- | Double-quote-escape a string per #2.5 and wrap in double quotes, for printing a string
-- literal in the textual syntax (NOT 'hydra.lib.literals.printString', see 'escapeStringChar').
printStringLit :: TypedTermDefinition (String -> String)
printStringLit = define "printStringLit" $
  doc "Render a string as a double-quoted string-literal token, escaping its content per #2.5." $
  "s" ~>
  Strings.concat $ list [
    string "\"",
    Strings.concat (Lists.map (asTerm escapeStringChar) (Strings.toList $ var "s")),
    string "\""]

-- | Backtick-escape the content of a name and wrap in backticks (#2.4's backtick form). The
-- backticked form must admit any name, so the delimiter itself (backtick) is part of the escape
-- set, unlike #2.5's own double-quote-delimited strings.
printBacktickedName :: TypedTermDefinition (String -> String)
printBacktickedName = define "printBacktickedName" $
  doc "Render a name in backticked form, escaping its content per #2.4." $
  "s" ~>
  Strings.concat $ list [
    string "`",
    Strings.concat (Lists.map (asTerm escapeBacktickChar) (Strings.toList $ var "s")),
    string "`"]

-- | Render a name in a binder head (the parameter of lambda, type-lambda, forall, or a
-- typeScheme's bound-variable list): here a dot is structural, so a bare binder name must be a
-- single dot-free segment (interference case 3 of #2.4), in addition to cases 1 and 2.
printBinderName :: TypedTermDefinition (Name -> String)
printBinderName = define "printBinderName" $
  doc "Show a binder-head name as a string; a dot forces backtick-escaping here (#2.4 case 3)." $
  "n" ~>
  "s" <~ Core.unName (var "n") $
  Logic.ifElse (Logic.and (isBareSegment @@ var "s") (Logic.not (isReservedWord @@ var "s")))
    (var "s")
    (printBacktickedName @@ var "s")

-- | Render a name for an ordinary (non-binder-head) position: dotted names are read greedily as
-- qualified names, so only interference cases 1 (non-bare characters) and 2 (reserved word) apply.
printName :: TypedTermDefinition (Name -> String)
printName = define "printName" $
  doc "Show a name as a string, backtick-escaping iff it is not bare or is a reserved word (#2.4)." $
  "n" ~>
  "s" <~ Core.unName (var "n") $
  Logic.ifElse (Logic.and (isBareName @@ var "s") (Logic.not (isReservedWord @@ var "s")))
    (var "s")
    (printBacktickedName @@ var "s")

-- | Render a nominal name reference, shortening it via the current module's import aliases (#803,
-- per syntax.md §6.4). The alias map is keyed by full 'ModuleName'; its values are the printed
-- aliases ('moduleAliases'/'Print/Packaging.hs'). Resolution: split the name into its module part
-- (all segments but the last, joined by '.') and local part (the last segment); if the module part
-- is in the alias map, print '<alias>.<local>' (or just '<local>' when the alias is empty); on any
-- miss -- a single-segment (unqualified, e.g. a lambda/let/forall-bound variable) name, or a module
-- not in the map -- fall back to the full dotted name via 'printName'. An empty alias map therefore
-- reproduces 'printName' exactly for every input, which is how every non-module-context caller
-- (tests, the lexicon generator, the JSON coder, standalone spec examples) keeps full-name output.
-- The chosen short form is routed back through 'printName' so the same backtick-escaping of the #2.4
-- interference cases still applies to the shortened (still dotted) name.
printNameAliased :: TypedTermDefinition (M.Map ModuleName String -> Name -> String)
printNameAliased = define "printNameAliased" $
  doc "Show a nominal name, shortened via the given import-alias map when its module is aliased (#803)." $
  "aliases" ~> "n" ~>
  "s" <~ Core.unName (var "n") $
  "segs" <~ Strings.splitOn (string ".") (var "s") $
  "segCount" <~ Lists.length (var "segs") $
  Logic.ifElse (Ordering.lt (var "segCount") (int32 2))
    (printName @@ var "n")
    ("dropCount" <~ Math.sub (var "segCount") (int32 1) $
     "localName" <~ Strings.join (string ".") (Lists.drop (var "dropCount") (var "segs")) $
     "modStr" <~ Strings.join (string ".") (Lists.take (var "dropCount") (var "segs")) $
     "modName" <~ Packaging.moduleName2 (var "modStr") $
     Optionals.match (Maps.lookup (var "modName") (var "aliases" :: TypedTerm (M.Map ModuleName String)))
       (printName @@ var "n")
       ("alias" ~> Logic.ifElse (Strings.isEmpty $ var "alias")
         (printName @@ (Core.name $ var "localName"))
         (printName @@ (Core.name $ Strings.concat $ list [var "alias", string ".", var "localName"]))))

-- | Collapse a chain of DIRECTLY nested Term.lambda constructors (not across an annotation or
-- other wrapper, so the round-trip stays an identity) into one multi-binder 'λa,b,c.' list
-- (#711, same treatment as forall/∀'s multi-binder collapsing). Each gathered binder keeps its
-- own (name, domain) pair, since each may carry an independent optional domain type.
gatherLambdaBinders :: TypedTermDefinition ([(Name, Maybe Type)] -> Term -> ([(Name, Maybe Type)], Term))
gatherLambdaBinders = define "gatherLambdaBinders" $
  doc "Gather a chain of directly-nested Term.lambda binders, returning (binders, innermost body)" $
  "prev" ~> "t" ~> match _Term (var "t")
    (Just $ pair (Lists.reverse $ var "prev") (var "t")) [
    _Term_lambda>>: "l" ~>
      "v" <~ Core.lambdaParameter (var "l") $
      "mt" <~ Core.lambdaDomain (var "l") $
      "body" <~ Core.lambdaBody (var "l") $
      gatherLambdaBinders @@ (Lists.cons (pair (var "v") (var "mt")) (var "prev")) @@ var "body"]

-- | Collapse a chain of DIRECTLY nested Term.typeLambda constructors into one multi-binder
-- 'Λa,b,c.' list (#711), mirroring 'gatherLambdaBinders' for the ordinary (value-level) lambda.
gatherTypeLambdaBinders :: TypedTermDefinition ([Name] -> Term -> ([Name], Term))
gatherTypeLambdaBinders = define "gatherTypeLambdaBinders" $
  doc "Gather a chain of directly-nested Term.typeLambda binders, returning (binders, innermost body)" $
  "prev" ~> "t" ~> match _Term (var "t")
    (Just $ pair (Lists.reverse $ var "prev") (var "t")) [
    _Term_typeLambda>>: "ta" ~>
      "v" <~ Core.typeLambdaParameter (var "ta") $
      "body" <~ Core.typeLambdaBody (var "ta") $
      gatherTypeLambdaBinders @@ (Lists.cons (var "v") (var "prev")) @@ var "body"]

lambda :: TypedTermDefinition (M.Map ModuleName String -> Lambda -> String)
lambda = define "lambda" $
  doc "Show a lambda as a string, shortening nominal references in its domain/body via the given alias map (#803)" $
  "aliases" ~> "l" ~>
  "gathered" <~ gatherLambdaBinders @@ (list ([] :: [TypedTerm (Name, Maybe Type)])) @@ (Core.termLambda $ var "l") $
  "binders" <~ Pairs.first (var "gathered") $
  "body" <~ Pairs.second (var "gathered") $
  "binderStr" <~ ("b" ~>
    "v" <~ printBinderName @@ (Pairs.first $ var "b") $
    "mt" <~ Pairs.second (var "b") $
    Optionals.match (var "mt") (var "v")
      ("t" ~> Strings.concat $ list [var "v", string ":", typeAliased @@ var "aliases" @@ var "t"])) $
  Strings.concat $ list [
    string "λ",
    Strings.join (string ",") (Lists.map (var "binderStr") (var "binders")),
    string ".",
    termAliased @@ var "aliases" @@ var "body"]

-- | Like 'lambda', but builds an 'Expr' (#711 line-layout pass): the binder list stays on the
-- current line, and the body breaks onto the next line, indented, when it does not fit
-- (heuristic #4).
-- | Like 'lambda', but builds an 'Expr' (#711 line-layout pass): the binder list stays on the
-- current line, and the body breaks onto the next line, indented, when it does not fit
-- (heuristic #4). 'budget' is the number of columns already consumed on the current line before
-- this lambda's own text starts (see 'bindingExprAliased').
-- | True iff a term is itself a binder-introducing construct (let or lambda) whose own head
-- would otherwise be appended directly after a PARENT binder's head on the same line (#711
-- line-layout pass, staging review point #2). Used by 'lambdaExprAliased' to decide whether to
-- force a break after its own binder list rather than relying on the nested construct's own
-- (purely local) fit-or-break decision, which can leave three or more binder heads glued onto
-- one line even though the overall chain does not fit.
isBinderChainTerm :: TypedTermDefinition (Term -> Bool)
isBinderChainTerm = define "isBinderChainTerm" $
  doc "True iff a term is a let or lambda (a binder-introducing construct, for #711 chained-binder line breaking)" $
  "t" ~> match _Term (var "t") (Just false) [
    _Term_let>>: constant true,
    _Term_lambda>>: constant true]

lambdaExprAliased :: TypedTermDefinition (Int -> M.Map ModuleName String -> Lambda -> Expr)
lambdaExprAliased = define "lambdaExprAliased" $
  doc "Show a lambda as a line-layout expression, shortening nominal references in its domain/body via the given alias map (#803)" $
  "budget" ~> "aliases" ~> "l" ~>
  "gathered" <~ gatherLambdaBinders @@ (list ([] :: [TypedTerm (Name, Maybe Type)])) @@ (Core.termLambda $ var "l") $
  "binders" <~ Pairs.first (var "gathered") $
  "body" <~ Pairs.second (var "gathered") $
  "binderStr" <~ ("b" ~>
    "v" <~ printBinderName @@ (Pairs.first $ var "b") $
    "mt" <~ Pairs.second (var "b") $
    Optionals.match (var "mt") (var "v")
      ("t" ~> Strings.concat $ list [var "v", string ":", typeAliased @@ var "aliases" @@ var "t"])) $
  "headStr" <~ Strings.concat (list [
    string "λ",
    Strings.join (string ",") (Lists.map (var "binderStr") (var "binders")),
    string "."]) $
  "bodyBudget" <~ Math.sub (var "budget") (Strings.length $ var "headStr") $
  "bodyExpr" <~ termExprAtPrecBudget @@ var "bodyBudget" @@ var "aliases" @@ int32 precMin @@ var "body" $
  -- When the body is itself another binder chain (let/lambda) AND the chain as a whole does not
  -- fit on one line, force a break right after this lambda's own '.' rather than gluing the next
  -- binder's head onto the same line -- otherwise 'λpred.let parse := λinput.<body>' accumulates
  -- three binder heads on one line before any of them actually breaks.
  "forceBreak" <~ Logic.and
    (isBinderChainTerm @@ var "body")
    (Ordering.gt (Serialization.expressionLength @@ (Serialization.spaceSep @@ list [Serialization.cst @@ var "headStr", var "bodyExpr"])) (var "budget")) $
  Logic.ifElse (var "forceBreak")
    (Serialization.ifx @@ (Ast.op
      (Serialization.sym @@ string "")
      (Ast.padding Ast.wsNone (Ast.wsBreakAndIndent $ asTerm Serialization.doubleSpace))
      (Ast.precedence $ int32 0)
      Ast.associativityNone) @@ (Serialization.cst @@ var "headStr") @@ var "bodyExpr")
    (Serialization.indentSubsequentLines @@ Serialization.doubleSpace @@
      (Serialization.ifx @@ (Ast.op
        (Serialization.sym @@ string "")
        (Ast.padding Ast.wsNone Ast.wsNone)
        (Ast.precedence $ int32 0)
        Ast.associativityNone) @@ (Serialization.cst @@ var "headStr") @@ var "bodyExpr"))

let_ :: TypedTermDefinition (Let -> String)
let_ = define "let" $
  doc "Show a let expression as a string" $
  letAliased @@ noAliases

letAliased :: TypedTermDefinition (M.Map ModuleName String -> Let -> String)
letAliased = define "letAliased" $
  doc "Show a let expression as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "l" ~>
  "bindings" <~ Core.letBindings (var "l") $
  "env" <~ Core.letBody (var "l") $
  "bindingStrs" <~ Lists.map (bindingAliased @@ var "aliases") (var "bindings") $
  Strings.concat $ list [
    string "let ",
    Strings.join (string ", ") (var "bindingStrs"),
    string " in ",
    termAliased @@ var "aliases" @@ var "env"]

-- | Gather a chain of directly-nested single-binding 'Term.let_'s (the DSL's own idiom for a
-- let-chain, #711 line-layout pass: each '"x" <~ e $' in the DSL desugars to a separate, nested
-- 'Term.let_' with one binding), returning (bindings-in-order, innermost non-let body). A
-- multi-binding 'Let' node (comma-separated bindings from a single surface 'let') is treated as
-- one chain link whose "binding" is the whole comma-list, so it still participates in the chain
-- if it is itself nested under or above other lets.
gatherLetChain :: TypedTermDefinition ([[Binding]] -> Term -> ([[Binding]], Term))
gatherLetChain = define "gatherLetChain" $
  doc "Gather a chain of directly-nested Term.let_ links, returning (binding-groups, innermost body)" $
  "prev" ~> "t" ~> match _Term (var "t")
    (Just $ pair (Lists.reverse $ var "prev") (var "t")) [
    _Term_let>>: "l" ~>
      "bindings" <~ Core.letBindings (var "l") $
      "body" <~ Core.letBody (var "l") $
      gatherLetChain @@ (Lists.cons (var "bindings") (var "prev")) @@ var "body"]

-- | Like 'letAliased', but builds an 'Expr' (#711 line-layout pass). 'budget' is the number of
-- columns already consumed on the current line before this let-expression's own text starts
-- (see 'bindingExprAliased'). Stays fully inline (the original 'let a := b in c' form) when the
-- whole chain fits within budget; otherwise breaks per heuristic #3:
--
--   * A chain of two or more links: each link's 'let b1, b2 in' prints on its own line, at the
--     same indent, and the final non-let body prints on its own line at that same indent.
--   * A chain of exactly one link: 'let x := v' and 'in body' print as two lines at the SAME
--     indent (no trailing ' in' glued onto the binding line) -- this is the common case (a
--     single let whose value itself needs to break) and reads better without the redundant
--     'in' crowding the binding line.
--
-- Either way, a binding whose own value does not fit breaks the value onto following lines,
-- indented one further level (via 'bindingExprAliased', which receives this same 'budget').
letExprAliased :: TypedTermDefinition (Int -> M.Map ModuleName String -> Let -> Expr)
letExprAliased = define "letExprAliased" $
  doc "Show a let expression as a line-layout expression, shortening nominal references via the given import-alias map (#803)" $
  "budget" ~> "aliases" ~> "l" ~>
  "gathered" <~ gatherLetChain @@ (list ([] :: [TypedTerm [Binding]])) @@ (Core.termLet $ var "l") $
  "bindingGroups" <~ Pairs.first (var "gathered") $
  "finalBody" <~ Pairs.second (var "gathered") $
  "groupExprs" <~ Lists.map ("bs" ~> Lists.map (bindingExprAliased @@ (Math.sub (var "budget") (int32 4)) @@ var "aliases") (var "bs")) (var "bindingGroups") $
  "groupInlineStrs" <~ Lists.map ("bes" ~> Serialization.commaSep @@ Serialization.inlineStyle @@ var "bes") (var "groupExprs") $
  "bodyExpr" <~ termExprAtPrecBudget @@ var "budget" @@ var "aliases" @@ int32 precMin @@ var "finalBody" $
  "inletOp" <~ Ast.op
    (Serialization.sym @@ string " in ")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  "letPrefixOp" <~ Ast.op
    (Serialization.sym @@ string "let ")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  "inlineForm" <~ Lists.foldr
    ("ge" ~> "acc" ~> Serialization.ifx @@ var "inletOp" @@
      (Serialization.ifx @@ var "letPrefixOp" @@ (Serialization.cst @@ string "") @@ var "ge") @@
      var "acc")
    (var "bodyExpr")
    (var "groupInlineStrs") $
  "letLineOp" <~ Ast.op
    (Serialization.sym @@ string "let ")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  "blockForm" <~ Logic.ifElse (Equality.equal (Lists.length $ var "groupInlineStrs") (int32 1))
    -- Single link: 'let x := v' / 'in body', two lines at the same indent (no trailing ' in').
    ("ge" <~ (Optionals.withDefault (Serialization.cst @@ string "") (Lists.head $ var "groupInlineStrs")) $
     Serialization.newlineSep @@ list [
       Serialization.ifx @@ var "letLineOp" @@ (Serialization.cst @@ string "") @@ var "ge",
       Serialization.ifx @@ (Ast.op
         (Serialization.sym @@ string "in ")
         (Ast.padding Ast.wsNone Ast.wsNone)
         (Ast.precedence $ int32 0)
         Ast.associativityNone) @@ (Serialization.cst @@ string "") @@ var "bodyExpr"])
    -- Multi-link chain: each 'let b1, b2 in' on its own line, then the final body.
    ("letLines" <~ Lists.map ("ge" ~> Serialization.ifx @@ var "letLineOp" @@ (Serialization.cst @@ string "") @@
        (Serialization.suffix @@ string " in" @@ var "ge")) (var "groupInlineStrs") $
     Serialization.newlineSep @@ (Lists.concat (list [var "letLines", list [var "bodyExpr"]]))) $
  Serialization.chooseLayout @@ var "budget" @@ var "inlineForm" @@ var "blockForm"

list_ :: TypedTermDefinition ((a -> String) -> [a] -> String)
list_ = define "list" $
  doc "Show a list using a given function to show each element" $
  "f" ~> "xs" ~>
  "elementStrs" <~ Lists.map (var "f") (var "xs") $
  Strings.concat $ list [
    string "[",
    Strings.join (string ", ") (var "elementStrs"),
    string "]"]

literal :: TypedTermDefinition (Literal -> String)
literal = define "literal" $
  doc "Show a literal as a string" $
  "l" ~> match _Literal (var "l") Nothing [
    _Literal_binary>>: "b" ~> Strings.concat $ list [
      Literals.printString (Literals.binaryToBase64 $ var "b"), string ":binary"],
    _Literal_boolean>>: "b" ~> Logic.ifElse (var "b") (string "true") (string "false"),
    _Literal_decimal>>: "d" ~> Literals.printDecimal $ var "d",
    _Literal_float>>: "fv" ~> floatValue @@ var "fv",
    _Literal_integer>>: "iv" ~> integerValue @@ var "iv",
    _Literal_string>>: "s" ~> printStringLit @@ var "s"]

literalType :: TypedTermDefinition (LiteralType -> String)
literalType = define "literalType" $
  doc "Show a literal type as a string" $
  "lt" ~> match _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $ string "binary",
    _LiteralType_boolean>>: constant $ string "boolean",
    _LiteralType_decimal>>: constant $ string "decimal",
    _LiteralType_float>>: "ft" ~> floatType @@ var "ft",
    _LiteralType_integer>>: "it" ~> integerType @@ var "it",
    _LiteralType_string>>: constant $ string "string"]

-- map_/set_ carry an `Ord` constraint + `forall` because the generated `Hydra.Core.Dsl.Lib.{Maps,Sets}`
-- expose the primitive's `Ord` key/element constraint (the old hand-written `Meta.Lib.*` did not),
-- which also forces a placeholder concrete type at registration in `definitions`. See #467.
map_ :: forall k v. Ord k => TypedTermDefinition ((k -> String) -> (v -> String) -> M.Map k v -> String)
map_ = define "map" $
  doc "Show a map using given functions to show keys and values, in canonical (ascending key) order" $
  "showK" ~> "showV" ~> "m" ~>
  "pairStrs" <~ Lists.map ("p" ~> Strings.concat $ list [
    var "showK" @@ (Pairs.first $ var "p"),
    string "=",
    var "showV" @@ (Pairs.second $ var "p")]) (Lists.sortBy (reify Pairs.first) (Maps.toList (var "m" :: TypedTerm (M.Map k v)))) $
  Strings.concat $ list [
    string "{",
    Strings.join (string ", ") (var "pairStrs"),
    string "}"]

optional_ :: TypedTermDefinition ((a -> String) -> Maybe a -> String)
optional_ = define "optional" $
  doc "Show an optional value using a given function to show the element" $
  "f" ~> "mx" ~>
  Optionals.match (var "mx") (string "none") ("x" ~> Strings.concat2 (string "given(") (Strings.concat2 (var "f" @@ var "x") (string ")")))

pair_ :: TypedTermDefinition ((a -> String) -> (b -> String) -> (a, b) -> String)
pair_ = define "pair" $
  doc "Show a pair using given functions to show each element" $
  "showA" ~> "showB" ~> "p" ~>
  Strings.concat $ list [
    string "(",
    var "showA" @@ (Pairs.first $ var "p"),
    string ", ",
    var "showB" @@ (Pairs.second $ var "p"),
    string ")"]

-- The projection's type name is a nominal reference and is shortened via the alias map; the field
-- name is projected within that type and is an unqualified local identifier, so 'printNameAliased'
-- leaves it unchanged (a single-segment name never matches a module alias). Aliasing both keeps
-- this symmetric with the parser's 'expandAliasesInTerm' (#803).
projection :: TypedTermDefinition (M.Map ModuleName String -> Projection -> String)
projection = define "projection" $
  doc "Show a projection as a string, shortening the type name via the given import-alias map (#803)" $
  "aliases" ~> "proj" ~>
  "tname" <~ printNameAliased @@ var "aliases" @@ (Core.projectionTypeName $ var "proj") $
  "fname" <~ printNameAliased @@ var "aliases" @@ (Core.projectionFieldName $ var "proj") $
  -- #809: projection elimination sugar 'Name[field]'. Reparses directly to Term.project.
  Strings.concat $ list [
    var "tname",
    string "[",
    var "fname",
    string "]"]

set_ :: forall a. Ord a => TypedTermDefinition ((a -> String) -> S.Set a -> String)
set_ = define "set" $
  doc "Show a set using a given function to show each element, in canonical (ascending) order" $
  "f" ~> "xs" ~>
  "elementStrs" <~ Lists.map (var "f") (Lists.sort (Sets.toList (var "xs" :: TypedTerm (S.Set a)))) $
  Strings.concat $ list [
    string "{",
    Strings.join (string ", ") (var "elementStrs"),
    string "}"]

-- | Show a term as a string (the canonical, minimally-parenthesized rendering). Entry point at
-- the loosest (top-level) context, with no import aliases (full dotted names everywhere).
term :: TypedTermDefinition (Term -> String)
term = define "term" $
  doc "Show a term as a string" $
  termAliased @@ noAliases

-- | Like 'term', but shortening nominal references via the given import-alias map (#803). This is
-- the module-context entry point used by 'Print/Packaging.hs' when printing a definition's body.
-- Delegates to 'termExprAliased' (#711 line-layout pass) and renders the resulting 'Expr' with
-- 'Serialization.printExpr', so every term definition gets the same fit-or-break layout as
-- every other construct built on the shared 'Expr'/'chooseLayout' machinery.
termAliased :: TypedTermDefinition (M.Map ModuleName String -> Term -> String)
termAliased = define "termAliased" $
  doc "Show a term as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "t" ~> Serialization.printExpr @@ (termExprAliased @@ var "aliases" @@ var "t")

-- | Show a term as a string in a given minimum-precedence context, wrapping in parentheses
-- exactly when the term's own construct is looser than that context. Thin 'String' wrapper over
-- 'termExprAtPrec' (#711 line-layout pass); kept for callers (and tests) that still want a plain
-- string at a given precedence without going through the top-level entry point.
termAtPrec :: TypedTermDefinition (M.Map ModuleName String -> Int -> Term -> String)
termAtPrec = define "termAtPrec" $
  doc "Show a term as a string at a given minimum context precedence, adding minimal parentheses" $
  "aliases" ~> "ctx" ~> "t" ~>
    Serialization.printExpr @@ (termExprAtPrec @@ var "aliases" @@ var "ctx" @@ var "t")

-- | Like 'termAliased', but builds an 'Expr' (#711 line-layout pass) instead of a 'String'
-- directly. Entry point at the loosest (top-level) context, with a full 'Serialization.maxLineWidth'
-- budget (nothing has consumed any columns yet).
termExprAliased :: TypedTermDefinition (M.Map ModuleName String -> Term -> Expr)
termExprAliased = define "termExprAliased" $
  doc "Show a term as a line-layout expression at the top-level context, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "t" ~> termExprAtPrecBudget @@ Serialization.maxLineWidth @@ var "aliases" @@ int32 precMin @@ var "t"

-- | Like 'termExprAtPrecBudget', but with a full 'Serialization.maxLineWidth' budget. Kept for
-- callers (and tests) that do not yet track an available-width budget of their own.
termExprAtPrec :: TypedTermDefinition (M.Map ModuleName String -> Int -> Term -> Expr)
termExprAtPrec = define "termExprAtPrec" $
  doc "Show a term as a line-layout expression at a given minimum context precedence, with a full line-width budget" $
  "aliases" ~> "ctx" ~> "t" ~> termExprAtPrecBudget @@ Serialization.maxLineWidth @@ var "aliases" @@ var "ctx" @@ var "t"

-- | Show a term as an 'Expr' in a given minimum-precedence context, wrapping in parentheses
-- exactly when the term's own construct is looser than that context (see the precedence
-- constants above, and docs/specification/syntax.md #3-4). The alias map (#803) is threaded
-- through every recursive call so nominal references inside the term are shortened consistently.
--
-- 'budget' is the number of columns still available on the CURRENT output line before this
-- term's own text starts counting against 'Serialization.maxLineWidth' -- i.e. 'maxLineWidth'
-- minus whatever fixed-width prefix the caller has already committed to printing on the same
-- line (a binder name, 'let '/'in ', an application's head, a field name and '='). Every nested
-- call below that adds such a prefix subtracts its width before passing 'budget' down, and every
-- 'chooseLayout' call in this module uses 'budget' (or a further-reduced derivative of it)
-- instead of the raw 'Serialization.maxLineWidth', which 'chooseLayout' on its own cannot see
-- (its own doc comment: "surrounding indentation may push content beyond the threshold... callers
-- that know they are nested can pass a smaller threshold to compensate" -- this is that
-- compensation, applied systematically). A construct that BREAKS (prints on its own new line
-- rather than continuing the current one) resets to a close-to-full budget for its own children,
-- since a fresh line starts at the indent column, not at the parent's old column; this module
-- approximates that reset by not threading a tighter budget into a block-form branch's own
-- sub-expressions beyond the fixed indent cost, since 'printExpr' applies indentation uniformly
-- per level rather than per dynamic column.
--
-- This is the #711 line-layout pass's core: every construct below builds a layout-aware 'Expr'
-- (via the shared 'Serialization' combinators) rather than concatenating strings directly, so a
-- long application spine, let chain, record, or case statement can break across lines, indented,
-- when it does not fit within budget -- see 'letExprAliased' (let chains), 'lambdaExprAliased'
-- (lambda bodies), the application branch below (argument spines), and
-- 'sugarFieldsExprAliased'/'caseStatementExprAliased' (field/branch lists).
termExprAtPrecBudget :: TypedTermDefinition (Int -> M.Map ModuleName String -> Int -> Term -> Expr)
termExprAtPrecBudget = define "termExprAtPrecBudget" $
  doc "Show a term as a line-layout expression at a given minimum context precedence and available-width budget, adding minimal parentheses" $
  "budget" ~> "aliases" ~> "ctx" ~> "t" ~>
  "gatherTerms" <~ ("prev" ~> "app" ~>
    "lhs" <~ Core.applicationFunction (var "app") $
    "rhs" <~ Core.applicationArgument (var "app") $
    match _Term (var "lhs")
      (Just $ Lists.cons (var "lhs") (Lists.cons (var "rhs") (var "prev"))) [
      _Term_application>>: "app2" ~> var "gatherTerms" @@ (Lists.cons (var "rhs") (var "prev")) @@ var "app2"]) $
  match _Term (var "t") Nothing [
    _Term_annotated>>: "at" ~>
      "bodyExpr" <~ termExprAtPrecBudget @@ var "budget" @@ var "aliases" @@ int32 precPostfix @@ (Core.annotatedTermBody $ var "at") $
      "annExpr" <~ termExprAtPrecBudget @@ var "budget" @@ var "aliases" @@ int32 precPostfix @@ (Core.annotatedTermAnnotation $ var "at") $
      parenthesizeIfNeededExpr precPostfix (var "ctx") $
        Serialization.ifx @@ (Ast.op
          (Serialization.sym @@ string "@")
          (Ast.padding Ast.wsNone Ast.wsNone)
          (Ast.precedence $ int32 0)
          Ast.associativityNone) @@ var "bodyExpr" @@ var "annExpr",
    -- Application spine (heuristic #5): the function and its arguments space-separate inline
    -- when they fit; otherwise the function stays ALONE on the first line, and EVERY argument
    -- (including what would have been the first inline argument) breaks onto its own line,
    -- indented one level -- not just the arguments after the first, which would otherwise glue
    -- the function and its first argument together on one (possibly still-too-long) line.
    -- A short application (flat width <= minApplicationWidth) always stays inline even when the
    -- caller's budget is tight -- a tight budget comes from deep nesting, not from THIS
    -- application being genuinely too wide, and breaking a short call like 'f x' or
    -- 'ParseInput[offset] input' needlessly (#711 line-layout pass, staging review point #3)
    -- reads worse than a few extra columns on an otherwise-short line.
    _Term_application>>: "app" ~>
      "terms" <~ var "gatherTerms" @@ (list ([] :: [TypedTerm Term])) @@ var "app" $
      "termExprs" <~ Lists.map ("e" ~> termExprAtPrecBudget @@ var "budget" @@ var "aliases" @@ int32 precPostfix @@ var "e") (var "terms") $
      parenthesizeIfNeededExpr precApp (var "ctx") $
        "headExpr" <~ (Optionals.withDefault (Serialization.cst @@ string "") (Lists.head $ var "termExprs")) $
        "argExprs" <~ Lists.drop (int32 1) (var "termExprs") $
        "effectiveBudget" <~ Ordering.max (var "budget") (int32 minApplicationWidth) $
        Logic.ifElse (Lists.isEmpty $ var "argExprs")
          (var "headExpr")
          (Serialization.chooseLayout @@ var "effectiveBudget"
            @@ (Serialization.spaceSep @@ var "termExprs")
            @@ (Serialization.ifx @@ (Ast.op
                  (Serialization.sym @@ string "")
                  (Ast.padding Ast.wsNone (Ast.wsBreakAndIndent $ asTerm Serialization.doubleSpace))
                  (Ast.precedence $ int32 0)
                  Ast.associativityNone) @@ var "headExpr" @@ (Serialization.newlineSep @@ var "argExprs"))),
    _Term_cases>>: "cs" ~> caseStatementExprAliased @@ var "budget" @@ var "aliases" @@ var "cs",
    _Term_either>>: "e" ~> Eithers.either
      ("l" ~> Serialization.suffix @@ string ")" @@ (Serialization.prefix @@ string "left(" @@ (termExprAtPrecBudget @@ (Math.sub (var "budget") (int32 5)) @@ var "aliases" @@ int32 precMin @@ var "l")))
      ("r" ~> Serialization.suffix @@ string ")" @@ (Serialization.prefix @@ string "right(" @@ (termExprAtPrecBudget @@ (Math.sub (var "budget") (int32 6)) @@ var "aliases" @@ int32 precMin @@ var "r")))
      (var "e"),
    _Term_lambda>>: "l" ~>
      parenthesizeIfNeededExpr precMin (var "ctx") $ lambdaExprAliased @@ var "budget" @@ var "aliases" @@ var "l",
    _Term_let>>: "l" ~>
      parenthesizeIfNeededExpr precMin (var "ctx") $ letExprAliased @@ var "budget" @@ var "aliases" @@ var "l",
    _Term_list>>: "els" ~>
      Serialization.bracketListAdaptive @@ Lists.map (termExprAliased @@ var "aliases") (var "els"),
    _Term_literal>>: "lit" ~> Serialization.cst @@ (literal @@ var "lit"),
    _Term_map>>: "m" ~>
      "entryExpr" <~ ("p" ~> Serialization.ifx @@ (Ast.op
          (Serialization.sym @@ string "=")
          (Ast.padding Ast.wsNone Ast.wsNone)
          (Ast.precedence $ int32 0)
          Ast.associativityNone) @@
          (termExprAliased @@ var "aliases" @@ (Pairs.first $ var "p")) @@
          (termExprAliased @@ var "aliases" @@ (Pairs.second $ var "p"))) $
      "entryExprs" <~ Lists.map (var "entryExpr") (Lists.sortBy (reify Pairs.first) (Maps.toList (var "m" :: TypedTerm (M.Map Term Term)))) $
      Serialization.chooseLayout @@ var "budget"
        @@ (Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ var "entryExprs")
        @@ (Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@ var "entryExprs"),
    _Term_optional>>: "mt" ~> Optionals.match (var "mt") (Serialization.cst @@ string "none")
      ("t" ~> Serialization.suffix @@ string ")" @@ (Serialization.prefix @@ string "given(" @@ (termExprAtPrecBudget @@ (Math.sub (var "budget") (int32 6)) @@ var "aliases" @@ int32 precMin @@ var "t"))),
    _Term_pair>>: "p" ~> Serialization.parenListAdaptive @@ list [
      termExprAliased @@ var "aliases" @@ (Pairs.first $ var "p"),
      termExprAliased @@ var "aliases" @@ (Pairs.second $ var "p")],
    _Term_project>>: "proj" ~> Serialization.cst @@ (projection @@ var "aliases" @@ var "proj"),
    _Term_record>>: "rec" ~>
      -- #809: nominal construction sugar. A record always prints as 'Name{...}' (the record-style
      -- construct), with unit-valued fields abbreviated bareword via 'sugarFieldsExprAliased'. It
      -- reparses as a record (and, for a one-field record over a union type, is corrected to an
      -- injection by the packaging pass -- but a genuine record is never over a union, so it
      -- round-trips as a record).
      "tname" <~ printNameAliased @@ var "aliases" @@ (Core.recordTypeName $ var "rec") $
      "flds" <~ Core.recordFields (var "rec") $
      Serialization.ifx @@ (Ast.op
        (Serialization.sym @@ string "")
        (Ast.padding Ast.wsNone Ast.wsNone)
        (Ast.precedence $ int32 0)
        Ast.associativityNone) @@ (Serialization.cst @@ var "tname") @@
        (sugarFieldsExprAliased @@ (Math.sub (var "budget") (Strings.length $ var "tname")) @@ var "aliases" @@ var "flds"),
    _Term_set>>: "s" ~>
      "elExprs" <~ Lists.map (termExprAliased @@ var "aliases") (Lists.sort (Sets.toList (var "s" :: TypedTerm (S.Set Term)))) $
      Serialization.chooseLayout @@ var "budget"
        @@ (Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ var "elExprs")
        @@ (Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@ var "elExprs"),
    _Term_typeLambda>>: "ta" ~>
      "gathered" <~ gatherTypeLambdaBinders @@ (list ([] :: [TypedTerm Name])) @@ (Core.termTypeLambda $ var "ta") $
      "vs" <~ Pairs.first (var "gathered") $
      "body" <~ Pairs.second (var "gathered") $
      "vNames" <~ Lists.map (asTerm printBinderName) (var "vs") $
      "headStr" <~ Strings.concat (list [
        string "Λ",
        Strings.join (string ",") (var "vNames"),
        string "."]) $
      "bodyExpr" <~ termExprAtPrecBudget @@ (Math.sub (var "budget") (Strings.length $ var "headStr")) @@ var "aliases" @@ int32 precMin @@ var "body" $
      parenthesizeIfNeededExpr precMin (var "ctx") $
        Serialization.indentSubsequentLines @@ Serialization.doubleSpace @@
          (Serialization.ifx @@ (Ast.op
            (Serialization.sym @@ string "")
            (Ast.padding Ast.wsNone Ast.wsNone)
            (Ast.precedence $ int32 0)
            Ast.associativityNone) @@ (Serialization.cst @@ var "headStr") @@ var "bodyExpr"),
    _Term_typeApplication>>: "tt" ~>
      "t2" <~ Core.typeApplicationTermBody (var "tt") $
      "typ" <~ Core.typeApplicationTermType (var "tt") $
      "bodyExpr" <~ termExprAtPrecBudget @@ var "budget" @@ var "aliases" @@ int32 precPostfix @@ var "t2" $
      parenthesizeIfNeededExpr precPostfix (var "ctx") $
        Serialization.suffix @@ string "⟩" @@
          (Serialization.ifx @@ (Ast.op
            (Serialization.sym @@ string "⟨")
            (Ast.padding Ast.wsNone Ast.wsNone)
            (Ast.precedence $ int32 0)
            Ast.associativityNone) @@ var "bodyExpr" @@ (Serialization.cst @@ (typeAliased @@ var "aliases" @@ var "typ"))),
    _Term_inject>>: "inj" ~> injectionExprAliased @@ var "budget" @@ var "aliases" @@ var "inj",
    _Term_unit>>: constant $ Serialization.cst @@ string "unit",
    -- #809: unwrap elimination sugar 'Name[]' (empty brackets = eliminate the wrapper's anonymous
    -- field). Reparses directly to Term.unwrap -- no graph correction needed.
    _Term_unwrap>>: "tname" ~> Serialization.cst @@ (Strings.concat $ list [
      printNameAliased @@ var "aliases" @@ var "tname",
      string "[]"]),
    _Term_variable>>: "name" ~> Serialization.cst @@ (printNameAliased @@ var "aliases" @@ var "name"),
    _Term_wrap>>: "wt" ~>
      -- #809: wrapper construction sugar 'Name{term}'. Safe ONLY when the wrapped body does NOT
      -- reparse as a record field -- i.e. not a bare variable (which would print as a lone name and
      -- reparse as a one-bare-field record). A variable body therefore keeps the keyword form
      -- 'wrap(Name){x}', which round-trips unambiguously. All other bodies (literals, lists,
      -- applications, nested constructs, ...) are not field-shaped, so they sugar safely.
      "tname" <~ printNameAliased @@ var "aliases" @@ (Core.wrappedTermTypeName $ var "wt") $
      "term1" <~ Core.wrappedTermBody (var "wt") $
      Logic.ifElse (isVariableTerm @@ var "term1")
        (Serialization.suffix @@ string "}" @@
          (Serialization.ifx @@ (Ast.op
            (Serialization.sym @@ string "){")
            (Ast.padding Ast.wsNone Ast.wsNone)
            (Ast.precedence $ int32 0)
            Ast.associativityNone) @@ (Serialization.cst @@ (Strings.concat $ list [string "wrap(", var "tname"])) @@ (termExprAliased @@ var "aliases" @@ var "term1")))
        (Serialization.suffix @@ string "}" @@
          (Serialization.ifx @@ (Ast.op
            (Serialization.sym @@ string "{")
            (Ast.padding Ast.wsNone Ast.wsNone)
            (Ast.precedence $ int32 0)
            Ast.associativityNone) @@ (Serialization.cst @@ var "tname") @@ (termExprAtPrecBudget @@ (Math.sub (var "budget") (Math.add (Strings.length $ var "tname") (int32 1))) @@ var "aliases" @@ int32 precMin @@ var "term1")))]

-- | Show a type as a string (the canonical, minimally-parenthesized rendering). Entry point at
-- the loosest (top-level) context, with no import aliases (full dotted names everywhere).
type_ :: TypedTermDefinition (Type -> String)
type_ = define "type" $
  doc "Show a type as a string" $
  typeAliased @@ noAliases

-- | Like 'type_', but shortening nominal references via the given import-alias map (#803). This is
-- the module-context entry point used by 'Print/Packaging.hs' when printing a type definition.
typeAliased :: TypedTermDefinition (M.Map ModuleName String -> Type -> String)
typeAliased = define "typeAliased" $
  doc "Show a type as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "typ" ~> typeAtPrec @@ var "aliases" @@ int32 precMin @@ var "typ"

-- | Show a type as a string in a given minimum-precedence context, wrapping in parentheses
-- exactly when the type's own construct is looser than that context (see the precedence
-- constants above, and docs/specification/syntax.md #3-4). The alias map (#803) is threaded
-- through every recursive call so nominal references inside the type are shortened consistently.
typeAtPrec :: TypedTermDefinition (M.Map ModuleName String -> Int -> Type -> String)
typeAtPrec = define "typeAtPrec" $
  doc "Show a type as a string at a given minimum context precedence, adding minimal parentheses" $
  "aliases" ~> "ctx" ~> "typ" ~>
  -- Lay out a record/union's fields one per line, indented, each preceded by its own '##'
  -- description comment (#711 readability pass) -- reusing the shared hydra.core.ast/
  -- hydra.core.serialization block-printing machinery ('curlyBracesList'/'printExpr') that every
  -- other Hydra coder already uses for this exact "braced, comma-separated, one-per-line" shape,
  -- rather than hand-rolling indentation here. 'curlyBracesList' itself renders the empty-fields
  -- case as a bare '{}', so no separate check is needed.
  "showRowType" <~ ("flds" ~>
    "fieldExprs" <~ Lists.map (fieldTypeExpr @@ var "aliases") (var "flds") $
    Serialization.printExpr @@ (Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@ var "fieldExprs")) $
  "gatherTypes" <~ ("prev" ~> "app" ~>
    "lhs" <~ Core.applicationTypeFunction (var "app") $
    "rhs" <~ Core.applicationTypeArgument (var "app") $
    match _Type (var "lhs")
      (Just $ Lists.cons (var "lhs") (Lists.cons (var "rhs") (var "prev"))) [
      _Type_application>>: "app2" ~> var "gatherTypes" @@ (Lists.cons (var "rhs") (var "prev")) @@ var "app2"]) $
  "gatherFunctionTypes" <~ ("prev" ~> "t" ~>
    match _Type (var "t")
      (Just $ Lists.reverse $ Lists.cons (var "t") (var "prev")) [
        _Type_function>>: "ft" ~>
          "dom" <~ Core.functionTypeDomain (var "ft") $
          "cod" <~ Core.functionTypeCodomain (var "ft") $
          var "gatherFunctionTypes" @@ (Lists.cons (var "dom") (var "prev")) @@ var "cod"]) $
  -- Collapse a chain of DIRECTLY nested Type.forall constructors (not across an annotation or
  -- other wrapper, so the round-trip stays an identity -- the parser's own desugaring only ever
  -- produces directly-nested chains, never nested-under-something-else) into one multi-binder
  -- 'forall a, b, c.' form (#711, mirrors '∀x,y,z.'s typeScheme printing).
  "gatherForallBinders" <~ ("prev" ~> "t" ~>
    match _Type (var "t")
      (Just $ pair (Lists.reverse $ var "prev") (var "t")) [
        _Type_forall>>: "ft" ~>
          "v" <~ Core.forallTypeParameter (var "ft") $
          "body" <~ Core.forallTypeBody (var "ft") $
          var "gatherForallBinders" @@ (Lists.cons (var "v") (var "prev")) @@ var "body"]) $
  match _Type (var "typ") Nothing [
    _Type_annotated>>: "at" ~>
      "bodyStr" <~ typeAtPrec @@ var "aliases" @@ int32 precPostfix @@ (Core.annotatedTypeBody $ var "at") $
      "annStr" <~ termAtPrec @@ var "aliases" @@ int32 precPostfix @@ (Core.annotatedTypeAnnotation $ var "at") $
      parenthesizeIfNeeded precPostfix (var "ctx") $
        Strings.concat $ list [var "bodyStr", string "@", var "annStr"],
    _Type_application>>: "app" ~>
      "types" <~ var "gatherTypes" @@ (list ([] :: [TypedTerm Type])) @@ var "app" $
      "typeStrs" <~ Lists.map ("e" ~> typeAtPrec @@ var "aliases" @@ int32 precPostfix @@ var "e") (var "types") $
      parenthesizeIfNeeded precApp (var "ctx") $
        Strings.join (string " ") (var "typeStrs"),
    _Type_effect>>: "etyp" ~> Strings.concat $ list [
      string "effect<",
      typeAliased @@ var "aliases" @@ var "etyp",
      string ">"],
    _Type_either>>: "et" ~>
      "leftTyp" <~ Core.eitherTypeLeft (var "et") $
      "rightTyp" <~ Core.eitherTypeRight (var "et") $
      Strings.concat $ list [
        string "either<",
        typeAliased @@ var "aliases" @@ var "leftTyp",
        string ", ",
        typeAliased @@ var "aliases" @@ var "rightTyp",
        string ">"],
    _Type_forall>>: "ft" ~>
      "binderVars" <~ var "gatherForallBinders" @@ (list ([] :: [TypedTerm Name])) @@ var "typ" $
      "vs" <~ Pairs.first (var "binderVars") $
      "body" <~ Pairs.second (var "binderVars") $
      "vNames" <~ Lists.map (asTerm printBinderName) (var "vs") $
      parenthesizeIfNeeded precMin (var "ctx") $
        Strings.concat $ list [
          string "forall ",
          Strings.join (string ", ") (var "vNames"),
          string ". ",
          typeAliased @@ var "aliases" @@ var "body"],
    _Type_function>>: "ft" ~>
      "types" <~ var "gatherFunctionTypes" @@ (list ([] :: [TypedTerm Type])) @@ var "typ" $
      "typeStrs" <~ Lists.map ("e" ~> typeAtPrec @@ var "aliases" @@ int32 precApp @@ var "e") (var "types") $
      parenthesizeIfNeeded precArrow (var "ctx") $
        Strings.join (string " → ") (var "typeStrs"),
    _Type_list>>: "etyp" ~> Strings.concat $ list [
      string "list<",
      typeAliased @@ var "aliases" @@ var "etyp",
      string ">"],
    _Type_literal>>: "lt" ~> literalType @@ var "lt",
    _Type_map>>: "mt" ~>
      "keyTyp" <~ Core.mapTypeKeys (var "mt") $
      "valTyp" <~ Core.mapTypeValues (var "mt") $
      Strings.concat $ list [
        string "map<",
        typeAliased @@ var "aliases" @@ var "keyTyp",
        string ", ",
        typeAliased @@ var "aliases" @@ var "valTyp",
        string ">"],
    _Type_optional>>: "etyp" ~> Strings.concat $ list [
      string "optional<",
      typeAliased @@ var "aliases" @@ var "etyp",
      string ">"],
    _Type_pair>>: "pt" ~>
      "firstTyp" <~ Core.pairTypeFirst (var "pt") $
      "secondTyp" <~ Core.pairTypeSecond (var "pt") $
      Strings.concat $ list [
        string "(",
        typeAliased @@ var "aliases" @@ var "firstTyp",
        string ", ",
        typeAliased @@ var "aliases" @@ var "secondTyp",
        string ")"],
    _Type_record>>: "rt" ~> Strings.concat2 (string "record") (var "showRowType" @@ var "rt"),
    _Type_set>>: "etyp" ~> Strings.concat $ list [
      string "set<",
      typeAliased @@ var "aliases" @@ var "etyp",
      string ">"],
    _Type_union>>: "rt" ~> Strings.concat2 (string "union") (var "showRowType" @@ var "rt"),
    _Type_unit>>: constant $ string "unit",
    _Type_variable>>: "name" ~> printNameAliased @@ var "aliases" @@ var "name",
    _Type_void>>: constant $ string "void",
    _Type_wrap>>: "wt" ~>
      Strings.concat $ list [string "wrap(", typeAliased @@ var "aliases" @@ var "wt", string ")"]]

typeScheme :: TypedTermDefinition (TypeScheme -> String)
typeScheme = define "typeScheme" $
  doc "Show a type scheme as a string" $
  typeSchemeAliased @@ noAliases

typeSchemeAliased :: TypedTermDefinition (M.Map ModuleName String -> TypeScheme -> String)
typeSchemeAliased = define "typeSchemeAliased" $
  doc "Show a type scheme as a string, shortening nominal references in its body via the given alias map (#803)" $
  "aliases" ~> "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "body" <~ Core.typeSchemeBody (var "ts") $
  "varNames" <~ Lists.map (asTerm printBinderName) (var "vars") $
  "constraintClassName" <~ ("c" ~>
    cases _TypeClassConstraint Nothing [
      _TypeClassConstraint_simple>>: "n" ~> Core.unName (var "n")] @@ (var "c")) $
  "toConstraintPair" <~ ("v" ~> "c" ~> Strings.concat $ list [
    var "constraintClassName" @@ var "c",
    string " ",
    Core.unName (var "v")]) $
  "toConstraintPairs" <~ ("p" ~> Lists.map
    (var "toConstraintPair" @@ (Pairs.first $ var "p")) $
    Lists.sortBy (var "constraintClassName") $
    Sets.toList $ Core.typeVariableConstraintsClasses $ Pairs.second $ var "p") $
  "tc" <~ Lists.concat (Lists.map (var "toConstraintPairs") $
    Lists.sortBy (reify Pairs.first) $
    Maps.toList (Core.typeSchemeConstraints (var "ts") :: TypedTerm (M.Map Name TypeVariableConstraints))) $
  "bodyStr" <~ typeAliased @@ var "aliases" @@ var "body" $
  -- Omit the '∀.' header entirely when there are no type variables (and therefore no
  -- constraints, which are always attached to a quantified variable) -- a monomorphic
  -- type definition like 'type Flag = boolean' reads better than 'type Flag = ∀.boolean'.
  -- Mirrors 'termSignatureAliased's identical convention, settled for signatures first.
  Logic.ifElse (Lists.isEmpty $ var "varNames")
    (var "bodyStr")
    (Strings.concat $ list [
      string "∀",
      Strings.join (string ",") (var "varNames"),
      string ".",
      Logic.ifElse (Lists.isEmpty $ var "tc")
        (string "")
        (Strings.concat $ list [
          string "(",
          Strings.join (string ", ") (var "tc"),
          string ") ⇒ "]),
      var "bodyStr"])

-- | Show a term signature as a string: like typeScheme's '∀vars.[constraints⇒]' header, but the
-- arrow chain is printed from the signature's own parameters/result rather than from a bare
-- Type body, so that a parameter's 'lazy' keyword (Parameter.isLazy; #800) can be emitted. This
-- is the printer counterpart used at term/primitive-definition sites (Print/Packaging.hs),
-- deliberately bypassing the lossy TermSignature->TypeScheme bridge (Scoping.hs), which erases
-- isLazy -- that bridge remains correct for contexts with no TermSignature at all (e.g. a
-- let-binding's ':(typeScheme)' ascription).
termSignature :: TypedTermDefinition (TermSignature -> String)
termSignature = define "termSignature" $
  doc "Show a term signature as a string, preserving each parameter's lazy marking" $
  termSignatureAliased @@ noAliases

termSignatureAliased :: TypedTermDefinition (M.Map ModuleName String -> TermSignature -> String)
termSignatureAliased = define "termSignatureAliased" $
  doc "Show a term signature as a string (preserving lazy markings), shortening nominal references via the given alias map (#803)" $
  "aliases" ~> "sig" ~>
  "typeParams" <~ Typing.termSignatureTypeParameters (var "sig") $
  "params" <~ Typing.termSignatureParameters (var "sig") $
  "result" <~ Typing.termSignatureResult (var "sig") $
  "varNames" <~ Lists.map ("tp" ~> printBinderName @@ (Typing.typeParameterName $ var "tp")) (var "typeParams") $
  "constraintClassName" <~ ("c" ~>
    cases _TypeClassConstraint Nothing [
      _TypeClassConstraint_simple>>: "n" ~> Core.unName (var "n")] @@ (var "c")) $
  "toConstraintPairsForParam" <~ ("tp" ~> "v" <~ Typing.typeParameterName (var "tp") $
    Lists.map ("c" ~> Strings.concat $ list [
      var "constraintClassName" @@ var "c", string " ", Core.unName (var "v")]) $
    Lists.sortBy (var "constraintClassName") (Typing.typeParameterConstraints $ var "tp")) $
  "tc" <~ Lists.concat (Lists.map (var "toConstraintPairsForParam") (var "typeParams")) $
  "paramStr" <~ ("p" ~>
    "tyStr" <~ typeAtPrec @@ var "aliases" @@ int32 precApp @@ (Typing.parameterType $ var "p") $
    Logic.ifElse (Typing.parameterIsLazy $ var "p")
      (Strings.concat2 (string "lazy ") (var "tyStr"))
      (var "tyStr")) $
  "paramStrs" <~ Lists.map (var "paramStr") (var "params") $
  "resultStr" <~ typeAtPrec @@ var "aliases" @@ int32 precApp @@ (Typing.resultType $ var "result") $
  "bodyStr" <~ Strings.join (string " → ") (Lists.concat2 (var "paramStrs") (list [var "resultStr"])) $
  -- Omit the '∀.' header entirely when there are no type variables (and therefore no
  -- constraints, which are always attached to a quantified variable) -- a monomorphic
  -- signature like 'not : boolean → boolean' reads better than 'not : ∀.boolean → boolean'.
  -- typeScheme's header is unchanged (not in scope here); only termSignature drops it.
  Logic.ifElse (Lists.isEmpty $ var "varNames")
    (var "bodyStr")
    (Strings.concat $ list [
      string "∀",
      Strings.join (string ",") (var "varNames"),
      string ".",
      Logic.ifElse (Lists.isEmpty $ var "tc")
        (string "")
        (Strings.concat $ list [
          string "(",
          Strings.join (string ", ") (var "tc"),
          string ") ⇒ "]),
      var "bodyStr"])
