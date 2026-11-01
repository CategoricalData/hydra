module Hydra.Sources.Kernel.Terms.Positions where

-- Standard imports for kernel terms modules
import Hydra.Kernel
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
import qualified Hydra.Core.Dsl.Lib.Ordering as Ordering
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
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
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
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
import qualified Hydra.Sources.Kernel.Terms.Annotations as AnnotationsTerms
import qualified Hydra.Sources.Kernel.Terms.Rewriting as RewritingTerms
import           Prelude hiding ((++))
import qualified Data.Char                   as Char
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.core.positions"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$>
              ([AnnotationsTerms.ns, RewritingTerms.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just
              ("Source positions: reserved span annotations on parsed terms, resolution of a SubtermPath to a span,"
                L.++ " and conversion of codepoint offsets to (line, column) positions."))}
  where
   definitions = [
     toDefinition annotateApplicationSpan,
     toDefinition annotateSpan,
     toDefinition keySourceSpan,
     toDefinition offsetToPosition,
     toDefinition peelSpans,
     toDefinition spanAtPath,
     toDefinition spanAtSteps,
     toDefinition spanFromTerm,
     toDefinition spanLayer,
     toDefinition spanToTerm,
     toDefinition stripSpans,
     toDefinition subtermAtStep,
     toDefinition termSpan]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Annotate an application with the span from the start of its function to the end of its argument
annotateApplicationSpan :: TypedTermDefinition (Term -> Term -> Term -> Term)
annotateApplicationSpan = define "annotateApplicationSpan" $
  doc ("Annotate an application term with the span running from the start of its function to the end of its"
    <> " argument, when both have spans; otherwise leave it unannotated") $
  "function" ~> "argument" ~> "application" ~>
  Optionals.match (termSpan @@ var "function") (var "application") ("fs" ~>
    Optionals.match (termSpan @@ var "argument") (var "application") ("as" ~>
      annotateSpan @@ (Parsing.sourceSpan (Parsing.sourceSpanStart $ var "fs") (Parsing.sourceSpanEnd $ var "as")) @@ var "application"))

-- | Wrap a term in a span-only annotation layer
annotateSpan :: TypedTermDefinition (SourceSpan -> Term -> Term)
annotateSpan = define "annotateSpan" $
  doc ("Wrap a term in a layer whose only annotation is the reserved span key. This is surface trivia, like"
    <> " comments: it is exempt from the round-trip contract and is removed by stripSpans.") $
  "span" ~> "term" ~>
  Core.termAnnotated (Core.annotatedTerm (var "term")
    (AnnotationsTerms.wrapAnnotationMap @@ (Maps.singleton (asTerm keySourceSpan) (spanToTerm @@ var "span"))))

-- | The reserved annotation key under which a parsed node's source span is recorded
keySourceSpan :: TypedTermDefinition Name
keySourceSpan = define "keySourceSpan" $
  doc "Reserved annotation key for source spans recorded by the textual parser" $
  wrap _Name $ string "sourceSpan"

-- | Convert a codepoint offset into a (line, column) position
offsetToPosition :: TypedTermDefinition (String -> Int -> SourcePosition)
offsetToPosition = define "offsetToPosition" $
  doc ("Convert a codepoint offset within a text into a zero-based line and a zero-based column counted in UTF-16"
    <> " code units (the Language Server Protocol's convention). Lines end at LF, CR, or CR LF; an offset"
    <> " beyond the end of the text is clamped to the end.") $
  "text" ~> "offset" ~>
  "all" <~ Strings.toList (var "text") $
  "cps" <~ Lists.take (var "offset") (var "all") $
  "nexts" <~ Lists.concat2 (Lists.drop (int32 1) (var "all")) (list [int32 0]) $
  "step" <~ ("acc" ~> "cn" ~>
    "c" <~ Pairs.first (var "cn") $
    "next" <~ Pairs.second (var "cn") $
    "line" <~ Pairs.first (var "acc") $
    "col" <~ Pairs.second (var "acc") $
    "newline" <~ Logic.or
      (Equality.equal (var "c") (int32 10))
      (Logic.and (Equality.equal (var "c") (int32 13)) (Logic.not $ Equality.equal (var "next") (int32 10))) $
    "width" <~ Logic.ifElse (Ordering.gte (var "c") (int32 65536)) (int32 2) (int32 1) $
    Logic.ifElse (var "newline")
      (pair (Math.add (var "line") (int32 1)) (int32 0))
      (pair (var "line") (Math.add (var "col") (var "width")))) $
  "end" <~ Lists.foldl (var "step") (pair (int32 0) (int32 0)) (Lists.zip (var "cps") (var "nexts")) $
  Parsing.sourcePosition (Pairs.first $ var "end") (Pairs.second $ var "end")

-- | Remove all span-only annotation layers from the top of a term
peelSpans :: TypedTermDefinition (Term -> Term)
peelSpans = define "peelSpans" $
  doc "Remove span-only annotation layers from the top of a term, leaving the term itself" $
  "term" ~>
  Optionals.match (spanLayer @@ var "term") (var "term") ("layer" ~> peelSpans @@ (Pairs.second $ var "layer"))

-- | The span of the node at the end of a subterm path, or of its nearest enclosing node that has one
spanAtPath :: TypedTermDefinition (SubtermPath -> Term -> Maybe SourceSpan)
spanAtPath = define "spanAtPath" $
  doc ("Resolve a subterm path (as carried by type-checking errors, computed on the span-free term) against a"
    <> " span-annotated term, returning the span of the node it reaches. If the node itself has no span, the span of"
    <> " its nearest enclosing node that has one is returned; if the path cannot be followed, the span of the last"
    <> " node reached.") $
  "path" ~> "term" ~>
  spanAtSteps @@ (unwrap _SubtermPath @@ var "path") @@ var "term"

spanAtSteps :: TypedTermDefinition ([SubtermStep] -> Term -> Maybe SourceSpan)
spanAtSteps = define "spanAtSteps" $
  doc "Resolve a list of subterm steps against a span-annotated term; see spanAtPath" $
  "steps" ~> "term" ~>
  "outer" <~ (termSpan @@ var "term") $
  Optionals.match (Lists.uncons $ var "steps") (var "outer") ("uc" ~>
    Optionals.match (subtermAtStep @@ (Pairs.first $ var "uc") @@ (peelSpans @@ var "term")) (var "outer") ("child" ~>
      Optionals.match (spanAtSteps @@ (Pairs.second $ var "uc") @@ var "child") (var "outer") ("found" ~> just $ var "found")))

-- | Decode a span from its annotation value
spanFromTerm :: TypedTermDefinition (Term -> Maybe SourceSpan)
spanFromTerm = define "spanFromTerm" $
  doc "Decode a span from its annotation value, a pair of int32 literals (start, end)" $
  "term" ~>
  "int32Of" <~ ("t" ~> match _Term (var "t") (Just nothing) [
    _Term_literal>>: "l" ~> match _Literal (var "l") (Just nothing) [
      _Literal_integer>>: "i" ~> match _IntegerValue (var "i") (Just nothing) [
        _IntegerValue_int32>>: "n" ~> just $ var "n"]]]) $
  match _Term (var "term") (Just nothing) [
    _Term_pair>>: "p" ~>
      Optionals.bind (var "int32Of" @@ (Pairs.first $ var "p")) ("s" ~>
        Optionals.map ("e" ~> Parsing.sourceSpan (var "s") (var "e")) (var "int32Of" @@ (Pairs.second $ var "p")))]

-- | If the term is a span-only annotation layer, its span and the term it wraps
spanLayer :: TypedTermDefinition (Term -> Maybe (SourceSpan, Term))
spanLayer = define "spanLayer" $
  doc "If a term is a span-only annotation layer (see annotateSpan), return its span and the term it wraps" $
  "term" ~>
  match _Term (var "term") (Just nothing) [
    _Term_annotated>>: "at" ~>
      "anns" <~ (AnnotationsTerms.getAnnotationMap @@ (Core.annotatedTermAnnotation $ var "at")) $
      Logic.ifElse (Equality.equal (Maps.size (var "anns" :: TypedTerm (M.Map Name Term))) (int32 1))
        (Optionals.bind (Maps.lookup (asTerm keySourceSpan) (var "anns" :: TypedTerm (M.Map Name Term))) ("v" ~>
          Optionals.map ("sp" ~> pair (var "sp") (Core.annotatedTermBody $ var "at")) (spanFromTerm @@ var "v")))
        nothing]

-- | Encode a span as an annotation value
spanToTerm :: TypedTermDefinition (SourceSpan -> Term)
spanToTerm = define "spanToTerm" $
  doc "Encode a span as an annotation value: a pair of int32 literals (start, end)" $
  "span" ~>
  "int32Term" <~ ("n" ~> Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ var "n") $
  Core.termPair $ pair
    (var "int32Term" @@ (Parsing.sourceSpanStart $ var "span"))
    (var "int32Term" @@ (Parsing.sourceSpanEnd $ var "span"))

-- | Remove every span annotation from a term
stripSpans :: TypedTermDefinition (Term -> Term)
stripSpans = define "stripSpans" $
  doc ("Remove every span-only annotation layer from a term, including those inside user annotation payloads."
    <> " Applied to a freshly parsed term, this yields exactly the term that parsing without spans produces.") $
  "term" ~>
  RewritingTerms.rewriteTerm
    @@ ("recurse" ~> "t" ~>
      Optionals.match (spanLayer @@ var "t")
        (match _Term (var "t") (Just (var "recurse" @@ var "t")) [
          _Term_annotated>>: "at" ~> Core.termAnnotated $ Core.annotatedTerm
            (stripSpans @@ (Core.annotatedTermBody $ var "at"))
            (stripSpans @@ (Core.annotatedTermAnnotation $ var "at"))])
        ("layer" ~> stripSpans @@ (Pairs.second $ var "layer")))
    @@ var "term"

-- | Follow one subterm step from a term
subtermAtStep :: TypedTermDefinition (SubtermStep -> Term -> Maybe Term)
subtermAtStep = define "subtermAtStep" $
  doc ("Follow a single subterm step from a (span-free) term to the immediate subterm it names, if the term has"
    <> " that shape. Map-entry and set-element steps depend on a term ordering and are not resolved.") $
  "step" ~> "term" ~>
  "fieldTerm" <~ ("fname" ~> "fields" ~>
    Optionals.map ("f" ~> Core.fieldTerm $ var "f")
      (Lists.find ("f" ~> Equality.equal (Core.fieldName $ var "f") (var "fname")) (var "fields"))) $
  match _SubtermStep (var "step") (Just nothing) [
    _SubtermStep_annotatedAnnotation>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_annotated>>: "a" ~> just $ Core.annotatedTermAnnotation $ var "a"],
    _SubtermStep_annotatedBody>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_annotated>>: "a" ~> just $ Core.annotatedTermBody $ var "a"],
    _SubtermStep_applicationArgument>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_application>>: "a" ~> just $ Core.applicationArgument $ var "a"],
    _SubtermStep_applicationFunction>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_application>>: "a" ~> just $ Core.applicationFunction $ var "a"],
    _SubtermStep_casesCase>>: "cname" ~> match _Term (var "term") (Just nothing) [
      _Term_cases>>: "cs" ~>
        Optionals.map ("a" ~> Core.caseAlternativeHandler $ var "a")
          (Lists.find ("a" ~> Equality.equal (Core.caseAlternativeName $ var "a") (var "cname"))
            (Core.caseStatementCases $ var "cs"))],
    _SubtermStep_casesDefault>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_cases>>: "cs" ~> Core.caseStatementDefault $ var "cs"],
    _SubtermStep_eitherLeft>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_either>>: "e" ~> Eithers.either ("l" ~> just $ var "l") ("r" ~> nothing) (var "e")],
    _SubtermStep_eitherRight>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_either>>: "e" ~> Eithers.either ("l" ~> nothing) ("r" ~> just $ var "r") (var "e")],
    _SubtermStep_injectField>>: "fname" ~> match _Term (var "term") (Just nothing) [
      _Term_inject>>: "i" ~> var "fieldTerm" @@ var "fname" @@ list [Core.injectionField $ var "i"]],
    _SubtermStep_lambdaBody>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_lambda>>: "l" ~> just $ Core.lambdaBody $ var "l"],
    _SubtermStep_letBinding>>: "bname" ~> match _Term (var "term") (Just nothing) [
      _Term_let>>: "lt" ~>
        Optionals.map ("b" ~> Core.bindingTerm $ var "b")
          (Lists.find ("b" ~> Equality.equal (Core.bindingName $ var "b") (var "bname"))
            (Core.letBindings $ var "lt"))],
    _SubtermStep_letBody>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_let>>: "lt" ~> just $ Core.letBody $ var "lt"],
    _SubtermStep_listElement>>: "i" ~> match _Term (var "term") (Just nothing) [
      _Term_list>>: "els" ~> Lists.at (var "i") (var "els")],
    _SubtermStep_optionalGiven>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_optional>>: "m" ~> var "m"],
    _SubtermStep_pairFirst>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_pair>>: "p" ~> just $ Pairs.first $ var "p"],
    _SubtermStep_pairSecond>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_pair>>: "p" ~> just $ Pairs.second $ var "p"],
    _SubtermStep_recordField>>: "fname" ~> match _Term (var "term") (Just nothing) [
      _Term_record>>: "r" ~> var "fieldTerm" @@ var "fname" @@ (Core.recordFields $ var "r")],
    _SubtermStep_typeApplicationBody>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_typeApplication>>: "ta" ~> just $ Core.typeApplicationTermBody $ var "ta"],
    _SubtermStep_typeLambdaBody>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_typeLambda>>: "tl" ~> just $ Core.typeLambdaBody $ var "tl"],
    _SubtermStep_wrapBody>>: constant $ match _Term (var "term") (Just nothing) [
      _Term_wrap>>: "w" ~> just $ Core.wrappedTermBody $ var "w"]]

-- | The span recorded on a term's outermost annotation layer
termSpan :: TypedTermDefinition (Term -> Maybe SourceSpan)
termSpan = define "termSpan" $
  doc "The source span recorded on a term by the parser, if the term's outermost layer is a span annotation" $
  "term" ~>
  Optionals.map ("layer" ~> Pairs.first $ var "layer") (spanLayer @@ var "term")
