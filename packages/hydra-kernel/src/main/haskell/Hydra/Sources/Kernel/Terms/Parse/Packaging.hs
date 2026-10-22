{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Parse.Packaging where

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

import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Parse.Core as ParseCore
import qualified Hydra.Sources.Kernel.Terms.Parsers as P
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting


-- NOTE: mirrors Print/Packaging.hs (see that file's own header note). Uses the parser-combinator
-- library (Hydra.Sources.Kernel.Terms.Parsers) plus hydra.core.parse.core (#497 W2) for term/type/
-- typeScheme bodies. Everything else (module header, imports, definition keywords, primitive
-- modifiers, definition assembly) is implemented here.
--
-- Still blocked on open design questions (see
-- claude-hydra-messages/outbox/draft-syntax-extension-497-and-710.md): doc-comment ('##') parsing
-- (EntityMetadata) is not attempted here -- every parsed definition and module gets
-- metadata = Nothing, matching the printer's own skip of doc-comment output for this reason.
--
-- NO type-environment pre-pass is needed: the landed syntax.md grammar has no TypeName{...}
-- nominal-instance sugar (an earlier design-doc draft's concern, not part of the ratified
-- grammar) -- every type/term reference is a plain 'name', resolved identically to
-- 'variableTerm'/'variableType', with no forward-reference machinery required.

ns :: ModuleName
ns = ModuleName "hydra.core.parse.packaging"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Names.ns, ParseCore.ns, P.ns, Rewriting.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Parsers for hydra.core.packaging types")}
  where
   definitions = [
     toDefinition definitionParser,
     toDefinition expandAliasedName,
     toDefinition expandAliasesInDefinition,
     toDefinition expandAliasesInTerm,
     toDefinition expandAliasesInTermSignature,
     toDefinition expandAliasesInType,
     toDefinition identifier,
     toDefinition importDecl,
     toDefinition keyword,
     toDefinition moduleHeader,
     toDefinition moduleNameParser,
     toDefinition modulePackage,
     toDefinition moduleParser,
     toDefinition parseModule,
     toDefinition primitiveDefinitionParser,
     toDefinition primitiveModifiers,
     toDefinition qualifyLocalName,
     toDefinition termDefinitionParser,
     toDefinition token,
     toDefinition typeDefinitionParser,
     toDefinition whitespace]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Skip zero or more whitespace characters. Placeholder: a real implementation needs to also
-- skip '#'-prefixed line comments (syntax.md 2.2) -- NOT attempted here, since comment-skipping
-- interacts with doc-comment ('##') recognition, which is itself blocked on the open
-- EntityMetadata design questions.
whitespace :: TypedTermDefinition (Parser ())
whitespace = define "whitespace" $
  doc "Skip zero or more whitespace characters (TODO: also skip '#' line comments)" $
  P.map @@ ("_" ~> unit) @@ (P.many @@ (P.satisfy @@ ("c" ~> Chars.isSpace (var "c"))))

-- | Parse a token followed by trailing whitespace. Whitespace is consumed AFTER a token, never
-- before one -- this is the convention the existing hydra.core.json.parser ('token' there, see
-- Json/Parser.hs) already uses, and it matters structurally, not just stylistically: Parsers.hs's
-- 'alt'/'optional' only backtrack when the failed branch consumed NO input (see 'alt's own doc
-- comment). A parser shaped 'skipLeadingSpace >> keyword' would consume the leading space and
-- then fail on a non-matching keyword WITHOUT backtracking, breaking 'optional' around it. An
-- earlier draft of this file had exactly that bug in importDecl/primitiveModifiers (whitespace
-- consumed before an optional keyword); fixed by switching to the 'token' convention throughout,
-- matching Json/Parser.hs rather than inventing a different (broken) shape locally.
token :: TypedTermDefinition (Parser a -> Parser a)
token = define "token" $
  doc "Parse a token followed by optional trailing whitespace" $
  "p" ~> P.bind @@ var "p" @@ ("x" ~> P.bind @@ whitespace @@ (constant $ P.pure @@ var "x"))

-- | Parse a specific keyword as a token (trailing whitespace consumed, per 'token' above).
keyword :: TypedTermDefinition (String -> Parser String)
keyword = define "keyword" $
  doc "Parse a specific keyword, followed by optional trailing whitespace" $
  "kw" ~> token @@ (P.string_ @@ var "kw")

-- | Parse a single dotted identifier segment: alphanumeric characters only (syntax.md 2.4's bare
-- name alphabet). Backtick-escaped names (also per 2.4) are NOT handled here -- real
-- implementation needs that case too.
identifier :: TypedTermDefinition (Parser String)
identifier = define "identifier" $
  doc "Parse a bare dotted name (TODO: backtick-escaped form per syntax.md 2.4)" $
  token @@ (P.map @@ ("xs" ~> Strings.fromList (var "xs")) @@ (P.some @@ (P.satisfy @@ ("c" ~> Chars.isAlphaNum (var "c")))))

-- | Parse a dotted module name, e.g. "hydra.example.geometry", greedily (syntax.md 2.4: dotted
-- names outside binder heads read greedily). NOTE: 'identifier' already consumes trailing
-- whitespace per-segment (it's a token); the '.' separator parser below must NOT also be a
-- token, or "hydra. core" (space after a dot) would wrongly parse as one name -- dots bind
-- tightly to their segments, per the grammar's own 'moduleName ::= name' (no whitespace allowed
-- inside a dotted name). Confirmed by NOT wrapping the '.' parser in 'token'.
-- NOTE: Haskell binding named moduleNameParser, not moduleName, to avoid an ambiguous-occurrence
-- clash with Module.moduleName (a field of hydra.core.packaging.Module, also in scope via the
-- broad Hydra.Kernel import). The DSL term name registered below ("moduleName") is unaffected --
-- only the Haskell-level identifier changed.
moduleNameParser :: TypedTermDefinition (Parser ModuleName)
moduleNameParser = define "moduleName" $
  doc "Parse a dotted module name" $
  P.map @@ ("segs" ~> wrap _ModuleName (Strings.join (string ".") (var "segs"))) @@
    (P.sepBy1 @@ (P.map @@ ("xs" ~> Strings.fromList (var "xs")) @@ (P.some @@ (P.satisfy @@ ("c" ~> Chars.isAlphaNum (var "c")))))
               @@ (P.char @@ int32 46))  -- '.' -- NOT 'identifier' (see note above re: no
                                         -- trailing-whitespace tokens inside a dotted name)

-- | Parse the 'module <dotted-name>' header line.
moduleHeader :: TypedTermDefinition (Parser ModuleName)
moduleHeader = define "moduleHeader" $
  doc "Parse the module header: 'module' followed by a dotted module name" $
  P.bind @@ (keyword @@ string "module") @@ (constant $ token @@ moduleNameParser)

-- | Parse an optional 'from <packageName>' clause, e.g. for 'import hydra.foo from hydra-kernel'.
-- TODO (#710, open question #3): the 'from' keyword/placement is a proposal, not yet explicitly
-- confirmed by Josh. This parser matches the grammar as currently drafted in
-- draft-syntax-md-section6-modules.md; revisit if that's revised.
modulePackage :: TypedTermDefinition (Parser PackageName)
modulePackage = define "modulePackage" $
  doc "Parse a 'from <packageName>' clause" $
  P.bind @@ (keyword @@ string "from") @@ (constant $
    P.map @@ ("s" ~> wrap _PackageName (var "s")) @@ identifier)

-- | Parse an import declaration: 'import <moduleName> [as <alias>]'. Returns the
-- 'ModuleDependency' (for 'Module.dependencies') paired with the optional alias. #710 design
-- settled 2026-10-11: aliasing is PURELY a print/parse-local convenience with no AST
-- representation at all ('ModuleDependency' has no alias field, by design, not oversight) --
-- so the alias returned here is NOT lost, but it is also not meant to survive past this
-- module's own parse. The not-yet-written module-level parser (blocked on #497's W2 for
-- term/type bodies regardless) is responsible for building an alias -> ModuleName lookup table
-- from every importDecl's alias BEFORE parsing any term body in the same file, exactly
-- paralleling the type-environment pre-pass already required for TypeName{...} forward
-- references (see the file-level note at the top of this module). This function alone cannot
-- do that assembly -- it only avoids silently discarding the one piece of information the
-- assembly step will need.
importDecl :: TypedTermDefinition (Parser (ModuleDependency, Maybe String))
importDecl = define "importDecl" $
  doc "Parse an import declaration, returning its ModuleDependency paired with the optional alias" $
  P.bind @@ (keyword @@ string "import") @@ (constant $
    P.bind @@ (token @@ moduleNameParser) @@ ("modName" ~>
      P.bind @@ (P.optional @@ (
        P.bind @@ (keyword @@ string "as") @@ (constant identifier))) @@ ("malias" ~>
        P.bind @@ (P.optional @@ modulePackage) @@ ("mpkg" ~>
          P.pure @@ pair (Packaging.moduleDependency (var "modName") (var "mpkg")) (var "malias")))))

-- | Parse the '[impure] [partial]' primitive-definition prefix modifiers, returning (isPure,
-- isTotal) -- confirmed order impure-before-partial, both default true when absent. #710 design
-- settled 2026-10-08.
primitiveModifiers :: TypedTermDefinition (Parser (Bool, Bool))
primitiveModifiers = define "primitiveModifiers" $
  doc "Parse the optional 'impure'/'partial' primitive-definition modifier keywords" $
  P.bind @@ (P.optional @@ (keyword @@ string "impure")) @@ ("mimp" ~>
    P.bind @@ (P.optional @@ (keyword @@ string "partial")) @@ ("mpar" ~>
      P.pure @@ (pair
        (Logic.not (Optionals.isGiven (var "mimp")))
        (Logic.not (Optionals.isGiven (var "mpar"))))))

-- | Qualify a bare local identifier (as read by 'identifier') into a full 'Name' under the given
-- module, mirroring PrintCore's 'Names.localNameOf' in reverse: 'Name (moduleNameString ++ "." ++
-- local)'.
qualifyLocalName :: TypedTermDefinition (ModuleName -> String -> Name)
qualifyLocalName = define "qualifyLocalName" $
  doc "Qualify a bare local identifier into a full Name under the given module" $
  "modName" ~> "local" ~>
  Names.unqualifyName @@ (Util.qualifiedName (just (var "modName")) (var "local"))

-- | Expand an aliased name ('alias.Local') into its fully-qualified form ('hydra.full.module.Local'),
-- per syntax.md #6.3's parser-side alias resolution: a name's first dotted segment is checked
-- against the given alias table (built from the module's own 'import X as alias' declarations by
-- 'moduleParser'); if it matches, that segment is replaced by the aliased module's full dotted
-- name, and the remaining segments (the local part) are kept as-is. A name whose first segment is
-- not a known alias is returned unchanged -- this covers plain fully-dotted names, which always
-- resolve correctly on their own and need no rewriting. NOTE (scope): bare-unqualified-import
-- resolution ('import X' with no alias, bringing X's definitions into unqualified scope) is NOT
-- implemented here -- unlike the aliased case, resolving an unqualified reference requires
-- knowing WHICH local names the imported module actually defines, which this single-file parser
-- cannot determine without loading that module's own definitions (a real multi-module environment,
-- out of scope for a standalone textual parser). This is not a regression: the printer (per
-- syntax.md #6.4, 'moduleAliases') ALWAYS aliases every import it emits and never produces a bare
-- unqualified import, so the parser's aliased-name resolution alone is sufficient to round-trip
-- anything this printer itself produces.
expandAliasedName :: TypedTermDefinition (M.Map String ModuleName -> Name -> Name)
expandAliasedName = define "expandAliasedName" $
  doc "Expand a name's aliased first segment to its full module name, per the given alias table" $
  "aliases" ~> "n" ~>
  "s" <~ Core.unName (var "n") $
  "segs" <~ Strings.splitOn (string ".") (var "s") $
  Optionals.match (Lists.uncons (var "segs"))
    (var "n")
    ("uc" ~>
      "first_" <~ Pairs.first (var "uc") $
      "rest" <~ Pairs.second (var "uc") $
      Optionals.match (Maps.lookup (var "first_") (var "aliases" :: TypedTerm (M.Map String ModuleName)))
        (var "n")
        ("modName" ~> Core.name (Strings.concat $ Lists.cons (Packaging.unModuleName (var "modName")) (Lists.map ("seg" ~> Strings.concat2 (string ".") (var "seg")) (var "rest")))))

-- | Expand every aliased name reference inside a term, per the given alias table (see
-- 'expandAliasedName'). Only 'variableTerm' (the one term-level name-reference position the base
-- grammar has -- 'record'/'inject'/'wrap'/'project'/'unwrap'/'case' names are also 'name'
-- positions and are covered too, since they all share the same 'Name' field shape rewritten
-- in-place by the generic term rewriter) needs special handling; every other case recurses
-- unchanged via 'Rewriting.rewriteTerm's walker.
expandAliasesInTerm :: TypedTermDefinition (M.Map String ModuleName -> Term -> Term)
expandAliasesInTerm = define "expandAliasesInTerm" $
  doc "Expand every aliased name reference inside a term, per the given alias table" $
  "aliases" ~> "t0" ~>
  "expand" <~ (expandAliasedName @@ (var "aliases" :: TypedTerm (M.Map String ModuleName))) $
  "rewrite" <~ ("recurse" ~> "term" ~>
    match _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_variable>>: "n" ~> Core.termVariable (var "expand" @@ var "n"),
      _Term_project>>: "p" ~> Core.termProject (Core.projection
        (var "expand" @@ (Core.projectionTypeName $ var "p")) (var "expand" @@ (Core.projectionFieldName $ var "p"))),
      _Term_record>>: "r" ~> var "recurse" @@
        (Core.termRecord (Core.record (var "expand" @@ (Core.recordTypeName $ var "r")) (Core.recordFields $ var "r"))),
      _Term_unwrap>>: "n" ~> Core.termUnwrap (var "expand" @@ var "n"),
      _Term_wrap>>: "w" ~> var "recurse" @@
        (Core.termWrap (Core.wrappedTerm (var "expand" @@ (Core.wrappedTermTypeName $ var "w")) (Core.wrappedTermBody $ var "w"))),
      _Term_cases>>: "cs" ~> var "recurse" @@
        (Core.termCases (Core.caseStatement (var "expand" @@ (Core.caseStatementTypeName $ var "cs"))
          (Core.caseStatementDefault $ var "cs") (Core.caseStatementCases $ var "cs"))),
      _Term_inject>>: "inj" ~> var "recurse" @@
        (Core.termInject (Core.injection (var "expand" @@ (Core.injectionTypeName $ var "inj")) (Core.injectionField $ var "inj")))]) $
  Rewriting.rewriteTerm @@ var "rewrite" @@ var "t0"

-- | Expand every aliased name reference inside a type, per the given alias table (see
-- 'expandAliasedName'). Only 'variableType' needs special handling (the base grammar's only
-- type-level name-reference position); 'record'/'union' type fields carry no standalone type
-- name (they are row types, per syntax.md #3's 'recordType'/'unionType' productions), so no
-- other case needs rewriting.
expandAliasesInType :: TypedTermDefinition (M.Map String ModuleName -> Type -> Type)
expandAliasesInType = define "expandAliasesInType" $
  doc "Expand every aliased name reference inside a type, per the given alias table" $
  "aliases" ~> "typ0" ~>
  "rewrite" <~ ("recurse" ~> "typ" ~>
    match _Type (var "typ")
      (Just $ var "recurse" @@ var "typ") [
      _Type_variable>>: "n" ~> Core.typeVariable (expandAliasedName @@ (var "aliases" :: TypedTerm (M.Map String ModuleName)) @@ var "n")]) $
  Rewriting.rewriteType @@ var "rewrite" @@ var "typ0"

-- | Expand every aliased name reference inside a term signature's parameter and result types,
-- per the given alias table. Operates directly on TermSignature (not via a TypeScheme bridge),
-- preserving parameter names, descriptions, and isLazy flags (#800) exactly.
expandAliasesInTermSignature :: TypedTermDefinition (M.Map String ModuleName -> TermSignature -> TermSignature)
expandAliasesInTermSignature = define "expandAliasesInTermSignature" $
  doc "Expand every aliased name reference inside a term signature's parameter and result types" $
  "aliases" ~> "sig" ~>
  "expand" <~ (expandAliasesInType @@ (var "aliases" :: TypedTerm (M.Map String ModuleName))) $
  Typing.termSignature
    (Typing.termSignatureTypeParameters $ var "sig")
    (Lists.map
      ("p" ~> Typing.parameter (Typing.parameterName $ var "p") (Typing.parameterDescription $ var "p")
        (var "expand" @@ (Typing.parameterType $ var "p")) (Typing.parameterIsLazy $ var "p"))
      (Typing.termSignatureParameters $ var "sig"))
    ("r" <~ Typing.termSignatureResult (var "sig") $
      Typing.result (Typing.resultDescription $ var "r") (var "expand" @@ (Typing.resultType $ var "r")))

-- | Expand every aliased name reference inside a definition's body/signature, per the given
-- alias table. Dispatches by definition kind, rewriting only the parts that can carry name
-- references (a type definition's body, a term definition's body and signature, a primitive
-- definition's signature and default implementation).
expandAliasesInDefinition :: TypedTermDefinition (M.Map String ModuleName -> Definition -> Definition)
expandAliasesInDefinition = define "expandAliasesInDefinition" $
  doc "Expand every aliased name reference inside a definition's body/signature" $
  "aliases" ~> "def" ~>
  "expandScheme" <~ ("ts" ~> Core.typeScheme (Core.typeSchemeVariables $ var "ts")
    (expandAliasesInType @@ (var "aliases" :: TypedTerm (M.Map String ModuleName)) @@ (Core.typeSchemeBody $ var "ts"))
    (Core.typeSchemeConstraints $ var "ts")) $
  match _Definition (var "def") Nothing [
    _Definition_type>>: "td" ~> Packaging.definitionType (Packaging.typeDefinition
      (Packaging.typeDefinitionName $ var "td") (Packaging.typeDefinitionMetadata $ var "td")
      (var "expandScheme" @@ (Packaging.typeDefinitionBody $ var "td"))),
    _Definition_term>>: "td" ~> Packaging.definitionTerm (Packaging.termDefinition
      (Packaging.termDefinitionName $ var "td") (Packaging.termDefinitionMetadata $ var "td")
      (Optionals.map (expandAliasesInTermSignature @@ (var "aliases" :: TypedTerm (M.Map String ModuleName)))
        (Packaging.termDefinitionSignature $ var "td"))
      (expandAliasesInTerm @@ (var "aliases" :: TypedTerm (M.Map String ModuleName)) @@ (Packaging.termDefinitionBody $ var "td"))),
    _Definition_primitive>>: "pd" ~> Packaging.definitionPrimitive (Packaging.primitiveDefinition
      (Packaging.primitiveDefinitionName $ var "pd") (Packaging.primitiveDefinitionMetadata $ var "pd")
      (expandAliasesInTermSignature @@ (var "aliases" :: TypedTerm (M.Map String ModuleName)) @@ (Packaging.primitiveDefinitionSignature $ var "pd"))
      (Packaging.primitiveDefinitionIsPure $ var "pd") (Packaging.primitiveDefinitionIsTotal $ var "pd")
      (Optionals.map (expandAliasesInTerm @@ (var "aliases" :: TypedTerm (M.Map String ModuleName))) (Packaging.primitiveDefinitionDefaultImplementation $ var "pd")))]

-- | Parse a type definition: 'type name = typeScheme'. No doc-comment ('##') parsing (open
-- design question, not attempted here) -- metadata is always Nothing.
typeDefinitionParser :: TypedTermDefinition (ModuleName -> Parser Definition)
typeDefinitionParser = define "typeDefinitionParser" $
  doc "Parse a type definition: 'type name = typeScheme'" $
  "modName" ~>
  P.bind @@ (keyword @@ string "type") @@ (constant $
    P.bind @@ identifier @@ ("local" ~>
      P.bind @@ (keyword @@ string "=") @@ (constant $
        P.map @@ ("ts" ~> Packaging.definitionType
            (Packaging.typeDefinition (qualifyLocalName @@ var "modName" @@ var "local") nothing (var "ts"))) @@
          (asTerm ParseCore.typeScheme))))

-- | Parse a term definition: 'term name [: termSignature] = term'. The optional ': termSignature'
-- ascription becomes the TermDefinition's signature, parsed directly as a TermSignature (not via
-- a TypeScheme bridge) so that a parameter's 'lazy' keyword (#800) round-trips. No doc-comment
-- parsing -- metadata is always Nothing.
termDefinitionParser :: TypedTermDefinition (ModuleName -> Parser Definition)
termDefinitionParser = define "termDefinitionParser" $
  doc "Parse a term definition: 'term name [: termSignature] = term'" $
  "modName" ~>
  P.bind @@ (keyword @@ string "term") @@ (constant $
    P.bind @@ identifier @@ ("local" ~>
      P.bind @@ (P.optional @@ ascribedSignature) @@ ("msig" ~>
        P.bind @@ (keyword @@ string "=") @@ (constant $
          P.map @@ ("t" ~> Packaging.definitionTerm
              (Packaging.termDefinition (qualifyLocalName @@ var "modName" @@ var "local") nothing (var "msig") (var "t"))) @@
            (asTerm ParseCore.term)))))
  where
    ascribedSignature =
      P.bind @@ (keyword @@ string ":") @@ (constant $ asTerm ParseCore.termSignature)

-- | Parse a primitive definition: '[impure] [partial] primitive name : termSignature [= term]'.
-- No doc-comment parsing -- metadata is always Nothing.
primitiveDefinitionParser :: TypedTermDefinition (ModuleName -> Parser Definition)
primitiveDefinitionParser = define "primitiveDefinitionParser" $
  doc "Parse a primitive definition: '[impure] [partial] primitive name : termSignature [= term]'" $
  "modName" ~>
  P.bind @@ primitiveModifiers @@ ("mods" ~>
    "isPure" <~ Pairs.first (var "mods") $
    "isTotal" <~ Pairs.second (var "mods") $
    P.bind @@ (keyword @@ string "primitive") @@ (constant $
      P.bind @@ identifier @@ ("local" ~>
        P.bind @@ (keyword @@ string ":") @@ (constant $
          P.bind @@ (asTerm ParseCore.termSignature) @@ ("sig" ~>
            P.map @@ ("mimpl" ~> Packaging.definitionPrimitive
                (Packaging.primitiveDefinition (qualifyLocalName @@ var "modName" @@ var "local") nothing
                  (var "sig") (var "isPure") (var "isTotal") (var "mimpl"))) @@
              (P.optional @@ defaultImpl))))))
  where
    defaultImpl = P.bind @@ (keyword @@ string "=") @@ (constant $ asTerm ParseCore.term)

-- | Parse a definition: a type, term, or primitive definition, dispatching on the leading
-- keyword (the primitive-modifier keywords 'impure'/'partial' also mark a primitive definition,
-- so they are tried via 'primitiveDefinitionParser' itself, which consumes them unconditionally
-- before deciding; 'type'/'term'/'primitive' otherwise disambiguate directly, so no backtracking
-- is required beyond the three-way 'P.choice').
definitionParser :: TypedTermDefinition (ModuleName -> Parser Definition)
definitionParser = define "definitionParser" $
  doc "Parse a single definition: type, term, or primitive" $
  "modName" ~>
  P.choice @@ list [
    typeDefinitionParser @@ var "modName",
    termDefinitionParser @@ var "modName",
    primitiveDefinitionParser @@ var "modName"]

-- | Parse a complete module: moduleHeader importDecl* definition* (per #710's grammar, §6.2).
-- Aliases from 'import ... as alias' are parsed (via 'importDecl') but not yet threaded into an
-- alias lookup table for qualifying unqualified references in definition bodies -- 'identifier'
-- (used by definition headers) and 'name' (used inside term/type bodies, via Parse.Core) both
-- read plain dotted names with no alias expansion. This is a known gap matching the printer's own
-- alias round-trip gap (see importDecl's own note above and open question #0 in
-- draft-syntax-extension-497-and-710.md) -- not fixed here, tracked there.
moduleParser :: TypedTermDefinition (Parser Module)
moduleParser = define "moduleParser" $
  doc ("Parse a complete module: header, imports, then definitions. Aliased name references"
    <> " inside definition bodies (per syntax.md #6.3's 'import X as alias' convenience) are"
    <> " expanded to their fully-qualified form after parsing, using the alias table built from"
    <> " this module's own import declarations -- see 'expandAliasesInDefinition'.") $
  P.bind @@ moduleHeader @@ ("modName" ~>
    P.bind @@ (P.many @@ importDecl) @@ ("imports" ~>
      "aliases" <~ (Maps.fromList (Optionals.givens (Lists.map
          ("p" ~> Optionals.map ("alias" ~> pair (var "alias") (Packaging.moduleDependencyModule (Pairs.first $ var "p"))) (Pairs.second $ var "p"))
          (var "imports"))) :: TypedTerm (M.Map String ModuleName)) $
      P.map @@ ("defs" ~> Packaging.module_ (var "modName") nothing
          (Lists.map ("p" ~> Pairs.first (var "p")) (var "imports"))
          (Lists.map (expandAliasesInDefinition @@ (var "aliases" :: TypedTerm (M.Map String ModuleName))) (var "defs"))) @@
        (P.many @@ (definitionParser @@ var "modName"))))

-- | Parse a complete module from a string, requiring the parse to consume all input (after
-- trailing whitespace).
parseModule :: TypedTermDefinition (String -> Either String Module)
parseModule = define "parseModule" $
  doc "Parse a complete module from a string, requiring the parse to consume all input" $
  "input" ~>
  match _ParseResult (P.runParser @@ (P.bind @@ whitespace @@ (constant $ asTerm moduleParser)) @@ var "input") Nothing [
    _ParseResult_success>>: "s" ~>
      Logic.ifElse (Lists.isEmpty (Parsing.parseSuccessRemainder $ var "s"))
        (right (Parsing.parseSuccessValue (var "s")))
        (left (string "trailing input after module")),
    _ParseResult_failure>>: "e" ~> left (Parsing.parseErrorMessage (var "e"))]
