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
import qualified Hydra.Sources.Kernel.Terms.Parse.Docs as ParseDocs
import qualified Hydra.Sources.Kernel.Terms.Parsers as P
import qualified Hydra.Sources.Kernel.Terms.Positions as Positions
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip


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
-- #809 adds nominal construction/elimination sugar. Construction ('Name{...}') is parsed by
-- ParseCore GRAPH-FREE: a field-list body always becomes a Term.record (the "record-style
-- construct"), since a union injection is syntactically indistinguishable from a one-field record
-- without the schema. A type-environment post-pass here ('correctInjectionsInDefinition') then
-- CORRECTS a one-field Term.record whose type name resolves to a UNION into a Term.inject. The env
-- is built from THIS module's own type definitions ('typeEnvFromDefinitions'), which covers the
-- mixed-module forward-reference case (a term using 'Color{red}' where 'Color' is defined later in
-- the same module). Imported-type resolution would extend the env with the ambient schema; the
-- correction takes a plain 'Map Name Type' so that extension is additive. Multi-field-record-over-
-- union is left exactly as parsed, for the type checker to reject (the correction is not a
-- validator). Elimination sugar ('Name[field]'/'Name[]') needs no correction -- it parses directly
-- to Term.project/Term.unwrap.

ns :: ModuleName
ns = ModuleName "hydra.core.parse.packaging"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Names.ns, ParseCore.ns, ParseDocs.ns, P.ns, Positions.ns, Rewriting.ns, Strip.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Parsers for hydra.core.packaging types")}
  where
   definitions = [
     toDefinition correctCrossModuleInjectionsInDefinition,
     toDefinition correctCrossModuleInjectionsInTerm,
     toDefinition correctInjectionsInDefinition,
     toDefinition correctInjectionsInTerm,
     toDefinition definitionParser,
     toDefinition docCommentBlock,
     toDefinition dottedIdentifier,
     toDefinition expandAliasedName,
     toDefinition expandAliasesInDefinition,
     toDefinition expandAliasesInTerm,
     toDefinition expandAliasesInTermSignature,
     toDefinition expandAliasesInType,
     toDefinition expandAliasesInTypeSelfAware,
     toDefinition expandAliasesInTypeSelfAwareGo,
     toDefinition expandTypeNameRefSelfAware,
     toDefinition identifier,
     toDefinition importDecl,
     toDefinition isUnionType,
     toDefinition keyword,
     toDefinition moduleHeader,
     toDefinition moduleNameParser,
     toDefinition modulePackage,
     toDefinition moduleParser,
     toDefinition parseModule,
     toDefinition parseModuleWith,
     toDefinition primitiveDefinitionParser,
     toDefinition primitiveModifiers,
     toDefinition qualifyLocalName,
     toDefinition resolveNameRefWith,
     toDefinition resolveNameRefsInDefinitionWith,
     toDefinition resolveNameRefsInTermSignatureWith,
     toDefinition resolveNameRefsInTermWith,
     toDefinition resolveNameRefsInTypeWith,
     toDefinition stripSpansInType,
     toDefinition termDefinitionParser,
     toDefinition token,
     toDefinition typeDefinitionParser,
     toDefinition typeEnvFromDefinitions,
     toDefinition whitespace]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Skip zero or more whitespace characters, including '#' line comments (syntax.md 2.2).
-- Mirrors Parse/Core.hs's own 'whitespace' (duplicated rather than shared: that module depends
-- on Hydra.Sources.Kernel.Terms.All, so importing it here would create a module-graph cycle).
--
-- Deliberately does NOT consume a '##' (doc-comment) line -- only an ordinary single-'#'
-- comment. A doc comment is grammatically significant (it becomes a definition's or module's
-- 'EntityMetadata' -- see 'docCommentBlock' below), unlike an ordinary comment, which really is
-- pure throwaway whitespace per syntax.md #2.2. If this function swallowed '##' lines too, by
-- the time a definition/module parser tried to capture its own leading doc comment, the
-- PRECEDING token's trailing whitespace-skip would already have consumed and discarded it --
-- this is the same hazard 'typeFieldsBlock's non-comment-eating 'typeFieldsPunct' was introduced
-- to avoid at the type-field level (#711); the two-hash lookahead here fixes it once, for every
-- caller of 'whitespace'/'token'/'keyword', instead of needing a parallel non-comment-eating
-- token family at every definition/module boundary.
whitespace :: TypedTermDefinition (Parser ())
whitespace = define "whitespace" $
  doc "Skip zero or more whitespace characters and ordinary (non-'##') '#' line comments" $
  P.map @@ ("_" ~> unit) @@ (P.many @@ (P.alt @@ wsChar @@ lineComment))
  where
    wsChar = P.map @@ (constant unit) @@ (P.satisfy @@ ("c" ~> Chars.isSpace (var "c")))
    -- The whole attempt -- consume '#', then require it's NOT followed by a second '#' -- is
    -- wrapped in 'try' here (not just the inner not-a-second-'#' check), so that on a genuine
    -- '##' line, the failure rewinds all the way back to BEFORE the leading '#' was consumed.
    -- Without this outer 'try', the bare leading 'P.char' below succeeds and consumes the first
    -- '#' unconditionally; when the inner check then fails (it IS a second '#'), 'lineComment'
    -- as a whole has still consumed one character, so the surrounding 'P.alt @@ wsChar @@
    -- lineComment' (which only backtracks on a NO-input-consumed failure) cannot recover --
    -- the leading '#' of the next '##' line is silently swallowed here, never reaching
    -- 'docCommentBlock'. (Found via a definition-level doc comment vanishing for every
    -- definition but the first in a module -- #711.)
    lineComment = P.try @@ (
      P.bind @@ (P.char @@ int32 35) @@ (constant $    -- '#'
        -- Not a second '#' -- either the input ends right here (a bare trailing '#' with no
        -- content), or the next character genuinely isn't '#'. Either way this is an ordinary
        -- comment, not a '##' doc-comment line.
        P.bind @@ (P.alt @@ P.eof @@ (P.map @@ (constant unit) @@ (P.satisfy @@ ("c" ~> Logic.not (Equality.equal (var "c") (int32 35)))))) @@ (constant $
          P.map @@ (constant unit) @@
            (P.many @@ (P.satisfy @@ ("c" ~> Logic.not (Equality.equal (var "c") (int32 10))))))))  -- not '\n'

-- | Capture zero or more consecutive '##'-prefixed lines (a doc-comment block), returning each
-- line's content with the '## ' (or bare '##') prefix stripped. Consumes only plain whitespace
-- between/after lines (never an ordinary '#' comment or further input) -- this parser is meant
-- to run at exactly the point a module/definition's leading doc comment belongs, where
-- 'whitespace' has already stopped short of the first '##' line (see 'whitespace's own doc
-- comment above). Returns the empty list, consuming nothing, when no '##' line is present.
docCommentLines :: TypedTerm (Parser [String])
docCommentLines = P.many @@ docCommentLine
  where
    plainWhitespace = P.map @@ ("_" ~> unit) @@ (P.many @@ (P.satisfy @@ ("c" ~> Chars.isSpace (var "c"))))
    docCommentLine =
      P.bind @@ (P.char @@ int32 35) @@ (constant $    -- '#'
        P.bind @@ (P.char @@ int32 35) @@ (constant $    -- '#'
          P.bind @@ (P.optional @@ (P.char @@ int32 32)) @@ (constant $    -- optional ' '
            P.bind @@ (P.map @@ ("cs" ~> Strings.fromList (var "cs")) @@
                (P.many @@ (P.satisfy @@ ("c" ~> Logic.not (Equality.equal (var "c") (int32 10)))))) @@ ("s" ~>
              P.bind @@ plainWhitespace @@ (constant $
                P.pure @@ var "s")))))

-- | Split a doc-comment block's lines into sections on blank-line ('##' with no text) boundaries,
-- dropping empty sections (consecutive or leading/trailing blank lines collapse away). Mirrors
-- 'entityMetadataDoc's own section-join ('\n##\n' between non-empty [description, comments,
-- keyLines] sections) in reverse.
docCommentSections :: TypedTerm ([String] -> [[String]])
docCommentSections = "lns" ~>
  "step" <~ ("acc" ~> "ln" ~>
    Logic.ifElse (Strings.isEmpty $ var "ln")
      (Lists.concat2 (var "acc") (list [list ([] :: [TypedTerm String])]))
      ("i" <~ Math.sub (Lists.length (var "acc" :: TypedTerm [[String]])) (int32 1) $
       "cur" <~ Optionals.withDefault (list ([] :: [TypedTerm String])) (Lists.head (Lists.drop (var "i") (var "acc" :: TypedTerm [[String]]))) $
       Lists.concat2 (Lists.take (var "i") (var "acc")) (list [Lists.concat2 (var "cur") (list [var "ln"])]))) $
  Lists.filter ("sec" ~> Logic.not (Lists.isEmpty (var "sec" :: TypedTerm [String])))
    (Lists.foldl (var "step") (list [list ([] :: [TypedTerm String])]) (var "lns"))

-- | True iff a doc-comment line is a '@tag ...' key line (seeAlso/availableSince/
-- deprecatedSince/requirement/recommendation), as opposed to plain description/comment prose.
isDocKeyLine :: TypedTerm (String -> Bool)
isDocKeyLine = "s" ~>
  Optionals.match (Strings.charAt (int32 0) (var "s")) (boolean False) ("c" ~> Equality.equal (var "c") (int32 64))    -- '@'

-- | Parse a single '@seeAlso <ref>' key line's payload into an 'EntityReference', per
-- 'entityReferenceDoc'/'ParseDocs.parseDocAnnotation' (the same tag vocabulary used by the
-- existing doc-escape-annotation mechanism, #710 \167 6.6). Returns 'Nothing' if the line isn't
-- tagged '@seeAlso', or for a malformed/unrecognized reference (matching 'parseDocAnnotation's
-- own behavior).
parseSeeAlsoLine :: TypedTerm (String -> Maybe EntityReference)
parseSeeAlsoLine = "ln" ~>
  "parts" <~ Strings.splitOn (string " ") (var "ln") $
  Logic.ifElse (Equality.equal (Optionals.withDefault (string "") (Lists.head $ var "parts")) (string "@seeAlso"))
    (ParseDocs.parseDocAnnotation @@ (Strings.join (string " ") (Lists.drop (int32 1) $ var "parts")))
    nothing

-- | Parse a single '@requirement <localName>: <statement>' or '@recommendation <localName>:
-- <statement>' key line into a 'Provision', the inverse of 'entityMetadataDoc's provision-line
-- format. 'localName' is re-qualified under 'modName' (mirroring 'qualifyLocalName', used the
-- same way for type/term/primitive names elsewhere in this file) since the printed form -- per
-- 'Names.localNameOf' -- always drops the module prefix.
parseProvisionLine :: TypedTerm (ModuleName -> String -> Maybe Provision)
parseProvisionLine = "modName" ~> "ln" ~>
  "tag" <~ Optionals.withDefault (string "") (Lists.head (Strings.splitOn (string " ") (var "ln"))) $
  "afterTag" <~ Strings.join (string " ") (Lists.drop (int32 1) (Strings.splitOn (string " ") (var "ln"))) $
  "nameAndStmt" <~ Strings.splitOn (string ": ") (var "afterTag") $
  "localName" <~ Optionals.withDefault (string "") (Lists.head (var "nameAndStmt" :: TypedTerm [String])) $
  "stmt" <~ Strings.join (string ": ") (Lists.drop (int32 1) (var "nameAndStmt" :: TypedTerm [String])) $
  Logic.ifElse (Equality.equal (var "tag") (string "@requirement"))
    (just $ Packaging.provision (qualifyLocalName @@ var "modName" @@ var "localName") Packaging.provisionKindRequirement (var "stmt"))
  $ Logic.ifElse (Equality.equal (var "tag") (string "@recommendation"))
    (just $ Packaging.provision (qualifyLocalName @@ var "modName" @@ var "localName") Packaging.provisionKindRecommendation (var "stmt"))
    nothing

-- | Parse a '@availableSince <version>' or '@deprecatedSince <version>' key line's version
-- payload, given the expected tag. Returns 'Nothing' if the line's tag doesn't match.
parseVersionLine :: TypedTerm (String -> String -> Maybe Version)
parseVersionLine = "tag" ~> "ln" ~>
  "parts" <~ Strings.splitOn (string " ") (var "ln") $
  Logic.ifElse (Equality.equal (Optionals.withDefault (string "") (Lists.head $ var "parts")) (var "tag"))
    (just $ Packaging.version (Strings.join (string " ") (Lists.drop (int32 1) $ var "parts")))
    nothing

-- | Build an 'EntityMetadata' from a doc-comment block's already-captured lines (per
-- 'docCommentLines') and the enclosing module's name (needed only to qualify '@requirement'/
-- '@recommendation' provision names, per 'parseProvisionLine'). The inverse of
-- 'entityMetadataDoc': splits the lines into sections (per 'docCommentSections'), classifying
-- each by its first line -- a section whose first line is a '@tag' key line (per
-- 'isDocKeyLine') contributes to seeAlso/lifecycle/provisions; any other section is treated as
-- the description if it is the FIRST such section encountered, or as a block of plain comments
-- (one entry per line, per 'entityMetadataDoc's own 'commentBlocks') if it is a later one --
-- matching 'entityMetadataDoc's fixed emission order [description, comments, keyLines], each
-- independently optional. Shared by 'docCommentBlock' (definition-level: lines captured and
-- turned into metadata in one step) and 'moduleParser' (module-level: lines must be captured
-- BEFORE 'moduleHeader' parses the module name out of the text that follows, so this step is
-- applied separately, once the name is known).
docCommentLinesToMetadata :: TypedTerm (ModuleName -> [String] -> EntityMetadata)
docCommentLinesToMetadata = "modName" ~> "lns" ~>
  "sections" <~ (docCommentSections @@ var "lns") $
  "isKeySection" <~ ("sec" ~> Optionals.match (Lists.head (var "sec" :: TypedTerm [String])) false (isDocKeyLine)) $
  "keyLines" <~ Lists.concat (Lists.filter (var "isKeySection") (var "sections" :: TypedTerm [[String]])) $
  "nonKeySections" <~ Lists.filter ("sec" ~> Logic.not (var "isKeySection" @@ var "sec")) (var "sections" :: TypedTerm [[String]]) $
  "mdesc" <~ Optionals.map ("sec" ~> Strings.join (string "\n") (var "sec")) (Lists.head (var "nonKeySections" :: TypedTerm [[String]])) $
  "comments" <~ Optionals.withDefault (list ([] :: [TypedTerm String])) (Lists.head (Lists.drop (int32 1) (var "nonKeySections" :: TypedTerm [[String]]))) $
  "seeAlso" <~ Optionals.givens (Lists.map (asTerm parseSeeAlsoLine) (var "keyLines" :: TypedTerm [String])) $
  "avail" <~ Optionals.givens (Lists.map (parseVersionLine @@ string "@availableSince") (var "keyLines" :: TypedTerm [String])) $
  "depr" <~ Optionals.givens (Lists.map (parseVersionLine @@ string "@deprecatedSince") (var "keyLines" :: TypedTerm [String])) $
  "provisions" <~ Optionals.givens (Lists.map (parseProvisionLine @@ var "modName") (var "keyLines" :: TypedTerm [String])) $
  "mlifecycle" <~ Logic.ifElse (Logic.and (Lists.isEmpty (var "avail" :: TypedTerm [Version])) (Lists.isEmpty (var "depr" :: TypedTerm [Version])))
    nothing
    (just $ Packaging.lifecycleInfo (Lists.head (var "avail" :: TypedTerm [Version])) (Lists.head (var "depr" :: TypedTerm [Version]))) $
  Packaging.entityMetadata (var "mdesc") (var "comments") (var "seeAlso") (var "mlifecycle") (var "provisions")

-- | Parse an optional leading doc-comment block (per 'docCommentLines') into an 'EntityMetadata'
-- (per 'docCommentLinesToMetadata'), or 'Nothing' (having consumed nothing) when no '##' line is
-- present at all -- matching 'entityMetadataDoc's own "empty EntityMetadata prints nothing" rule
-- in reverse. The definition-level counterpart of 'moduleParser's own module-level doc-comment
-- handling (see that function's comment for why the module case can't just call this directly).
docCommentBlock :: TypedTermDefinition (ModuleName -> Parser (Maybe EntityMetadata))
docCommentBlock = define "docCommentBlock" $
  doc "Parse an optional leading '##' doc-comment block into an EntityMetadata, the inverse of entityMetadataDoc" $
  "modName" ~>
  P.bind @@ (asTerm docCommentLines) @@ ("lns" ~>
    Logic.ifElse (Lists.isEmpty (var "lns" :: TypedTerm [String]))
      (P.pure @@ nothing)
      (P.pure @@ (just $ docCommentLinesToMetadata @@ var "modName" @@ var "lns")))

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
-- | Parse a dotted name as a plain joined String -- shared segment-parsing core for
-- 'moduleNameParser' (wraps in ModuleName) and an import alias (syntax.md #710 §6.4: a computed
-- alias may itself be multi-segment when a shorter candidate collides, e.g. 'error.file' -- see
-- Print/Packaging.hs's 'aliasFromSuffix'/'growAliasesStep'). NOT 'identifier': dots must bind
-- tightly to their segments with no trailing-whitespace token in between (see moduleNameParser's
-- original note, preserved below).
dottedIdentifier :: TypedTermDefinition (Parser String)
dottedIdentifier = define "dottedIdentifier" $
  doc "Parse a dotted name (one or more alphanumeric segments joined by '.') as a String" $
  P.map @@ ("segs" ~> Strings.join (string ".") (var "segs")) @@
    (P.sepBy1 @@ (P.map @@ ("xs" ~> Strings.fromList (var "xs")) @@ (P.some @@ (P.satisfy @@ ("c" ~> Chars.isAlphaNum (var "c")))))
               @@ (P.char @@ int32 46))  -- '.' -- NOT 'identifier' (see note above re: no
                                         -- trailing-whitespace tokens inside a dotted name)

moduleNameParser :: TypedTermDefinition (Parser ModuleName)
moduleNameParser = define "moduleName" $
  doc "Parse a dotted module name" $
  P.map @@ ("s" ~> wrap _ModuleName (var "s")) @@ dottedIdentifier

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
        P.bind @@ (keyword @@ string "as") @@ (constant $ token @@ dottedIdentifier))) @@ ("malias" ~>
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
-- | Expand an aliased leading prefix of a name to its full module name, per the given alias table.
-- Aliases may be multi-segment (syntax.md §6.4's asymmetric collision rule can assign e.g.
-- 'error.file' to 'hydra.core.error.file' when it collides with 'hydra.core.file as file'), so the
-- lookup tries the LONGEST matching leading prefix first: for a name with k segments, it tests the
-- first (k-1), (k-2), ... , 1 segments joined by '.' as an alias key, stopping at the first hit.
-- The matched prefix is replaced by its full module name and the remaining (local) segments are
-- re-appended. At least one trailing segment is always kept (a name is never entirely alias), and
-- a name with no matching prefix -- including a single-segment, unqualified name -- is returned
-- unchanged. This is the exact inverse of 'Print/Core.hs's 'printNameAliased' (#803).
expandAliasedName :: TypedTermDefinition (M.Map String ModuleName -> Name -> Name)
expandAliasedName = define "expandAliasedName" $
  doc "Expand a name's aliased leading prefix (longest match) to its full module name, per the given alias table" $
  "aliases" ~> "n" ~>
  "s" <~ Core.unName (var "n") $
  "segs" <~ Strings.splitOn (string ".") (var "s") $
  "segCount" <~ Lists.length (var "segs") $
  -- Try each proper leading prefix of length k = 1 .. segCount-1 as an alias key, expanding to the
  -- full module name followed by the remaining (local) segments. 'tryLen' yields the expanded Name
  -- when the k-segment prefix is an alias, Nothing otherwise. Longer prefixes come later in the
  -- 1..segCount-1 order, so the LONGEST match is the last surviving candidate (Lists.last).
  "tryLen" <~ ("k" ~>
    "prefix" <~ Strings.join (string ".") (Lists.take (var "k") (var "segs")) $
    Optionals.map
      ("modName" ~> Core.name (Strings.join (string ".") (Lists.cons (Packaging.unModuleName (var "modName")) (Lists.drop (var "k") (var "segs")))))
      (Maps.lookup (var "prefix") (var "aliases" :: TypedTerm (M.Map String ModuleName)))) $
  -- Build [1 .. segCount-1] by discarding the last of [1 .. segCount] (one int per segment).
  "prefixLens" <~ Lists.foldl
    ("acc" ~> "_seg" ~> Lists.concat2 (var "acc") (list [Math.add (Lists.length $ var "acc") (int32 1)]))
    (list ([] :: [TypedTerm Int]))
    (Optionals.withDefault (list ([] :: [TypedTerm String])) (Lists.init $ var "segs")) $
  "matches" <~ Optionals.mapOptional (var "tryLen") (var "prefixLens") $
  Optionals.withDefault (var "n") (Lists.last $ var "matches")

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

-- | Core helper for 'expandAliasesInTypeSelfAware': expand a single name reference, given the
-- currently bound type-variable set and the printed module's own declared local names
-- ('ownNames'). A bound name is left alone (it's a genuine type variable, not a nominal
-- reference); otherwise it is expanded via the alias table, falling back to a same-module
-- self-reference (#711) ONLY when the name is bare (single segment, no dots), no alias matched,
-- AND the name is actually one of the module's own declared definitions ('ownNames') -- a bare
-- name that is neither bound, aliased, nor locally declared is a reference into one of the
-- module's BARE (alias-less) imports, and must be left untouched so the printer's own
-- 'moduleAliases'/MWIS pass can still see it as an unqualified dependency reference and emit the
-- bare 'import' line for it. Wrongly qualifying it to 'modName' here (the older, pre-fix
-- behavior) made it look like a same-module self-reference instead, silently discarding the
-- dependency and dropping its import line entirely on a print-parse-print round trip -- a real
-- regression, not just surface-trivia loss, caught by #804's bare-import round-trip test. An
-- unaliased but already fully-dotted name is left as-is either way (out of scope per
-- 'expandAliasedName's own doc comment).
expandTypeNameRefSelfAware :: TypedTermDefinition (ModuleName -> M.Map String ModuleName -> S.Set String -> S.Set String -> Name -> Name)
expandTypeNameRefSelfAware = define "expandTypeNameRefSelfAware" $
  doc "Expand a single type-level name reference, treating a bare unbound name as a self-reference only when it is one of the module's own declared names" $
  "modName" ~> "aliases" ~> "ownNames" ~> "bound" ~> "n" ~>
  "s" <~ Core.unName (var "n") $
  Logic.ifElse (Sets.member (var "s") (var "bound" :: TypedTerm (S.Set String)))
    (var "n")
    ("expanded" <~ (expandAliasedName @@ (var "aliases" :: TypedTerm (M.Map String ModuleName)) @@ var "n") $
     Logic.ifElse (Equality.equal (var "expanded") (var "n"))
       (Logic.ifElse (Logic.and
           (Equality.equal (Lists.length (Strings.splitOn (string ".") (var "s"))) (int32 1))
           (Sets.member (var "s") (var "ownNames" :: TypedTerm (S.Set String))))
         (qualifyLocalName @@ var "modName" @@ var "s")
         (var "n"))
       (var "expanded"))

-- | Recursive walker for 'expandAliasesInTypeSelfAware': descends through a type, extending
-- 'bound' with each nested 'Type_forall's own parameter before recursing into its body (a rare,
-- not currently used by the kernel, but syntactically valid shadowing case). Must be a registered
-- top-level 'TypedTermDefinition', not a local recursive let-binding -- a bare self-referencing
-- 'TypedTerm' inlines its own body at every recursive call site (AsTerm's non-definition instance
-- is 'id'), producing an infinite Haskell-level term graph that overflows the stack the instant
-- anything traverses it (same hazard 'termSignatureArrowChain' documents and avoids the same way).
expandAliasesInTypeSelfAwareGo :: TypedTermDefinition (ModuleName -> M.Map String ModuleName -> S.Set String -> S.Set String -> Type -> Type)
expandAliasesInTypeSelfAwareGo = define "expandAliasesInTypeSelfAwareGo" $
  doc "Recursively expand aliased and bare self-referencing names inside a type, tracking bound type variables" $
  "modName" ~> "aliases" ~> "ownNames" ~> "bound" ~> "typ" ~>
  "recurse" <~ (expandAliasesInTypeSelfAwareGo @@ var "modName" @@ var "aliases" @@ var "ownNames" @@ var "bound") $
  "forField" <~ ("field" ~> Core.fieldTypeWithType (var "field") (var "recurse" @@ (Core.fieldTypeType $ var "field"))) $
  match _Type (var "typ") Nothing [
    _Type_annotated>>: "at" ~> Core.typeAnnotated $ Core.annotatedType
      (var "recurse" @@ (Core.annotatedTypeBody $ var "at")) (Core.annotatedTypeAnnotation $ var "at"),
    _Type_application>>: "app" ~> Core.typeApplication $ Core.applicationType
      (var "recurse" @@ (Core.applicationTypeFunction $ var "app")) (var "recurse" @@ (Core.applicationTypeArgument $ var "app")),
    _Type_effect>>: "t" ~> Core.typeEffect $ var "recurse" @@ var "t",
    _Type_either>>: "et" ~> Core.typeEither $ Core.eitherType
      (var "recurse" @@ (Core.eitherTypeLeft $ var "et")) (var "recurse" @@ (Core.eitherTypeRight $ var "et")),
    _Type_forall>>: "ft" ~>
      "param" <~ Core.forallTypeParameter (var "ft") $
      "bound'" <~ Sets.insert (Core.unName $ var "param") (var "bound" :: TypedTerm (S.Set String)) $
      Core.typeForall (Core.forallType (var "param")
        (expandAliasesInTypeSelfAwareGo @@ var "modName" @@ var "aliases" @@ var "ownNames" @@ var "bound'" @@ (Core.forallTypeBody $ var "ft"))),
    _Type_function>>: "fun" ~> Core.typeFunction $ Core.functionType
      (var "recurse" @@ (Core.functionTypeDomain $ var "fun")) (var "recurse" @@ (Core.functionTypeCodomain $ var "fun")),
    _Type_list>>: "t" ~> Core.typeList $ var "recurse" @@ var "t",
    _Type_literal>>: "lt" ~> Core.typeLiteral $ var "lt",
    _Type_map>>: "mt" ~> Core.typeMap $ Core.mapType
      (var "recurse" @@ (Core.mapTypeKeys $ var "mt")) (var "recurse" @@ (Core.mapTypeValues $ var "mt")),
    _Type_optional>>: "t" ~> Core.typeOptional $ var "recurse" @@ var "t",
    _Type_pair>>: "pt" ~> Core.typePair $ Core.pairType
      (var "recurse" @@ (Core.pairTypeFirst $ var "pt")) (var "recurse" @@ (Core.pairTypeSecond $ var "pt")),
    _Type_record>>: "rt" ~> Core.typeRecord $ Lists.map (var "forField") (var "rt"),
    _Type_set>>: "t" ~> Core.typeSet $ var "recurse" @@ var "t",
    _Type_union>>: "rt" ~> Core.typeUnion $ Lists.map (var "forField") (var "rt"),
    _Type_unit>>: constant Core.typeUnit,
    _Type_variable>>: "n" ~> Core.typeVariable
      (expandTypeNameRefSelfAware @@ var "modName" @@ (var "aliases" :: TypedTerm (M.Map String ModuleName)) @@ var "ownNames" @@ var "bound" @@ var "n"),
    _Type_void>>: constant Core.typeVoid,
    _Type_wrap>>: "wt" ~> Core.typeWrap $ var "recurse" @@ var "wt"]

-- | Like 'expandAliasesInType', but ALSO expands a bare (single-segment, unaliased) name to a
-- self-reference under 'modName' -- the current module's own namespace -- unless that bare name
-- is a currently in-scope bound type variable, OR is not one of 'ownNames' (the module's own
-- declared definitions; see 'expandTypeNameRefSelfAware's own doc comment for why that guard is
-- required). This distinguishes 'CodeBlock' (a reference to a same-module type definition,
-- printed bare per the printer's own self-reference shortening, #711) from both 'a' in
-- '\8704a.record{...a...}' (a genuinely bound, non-nominal type variable) and 'FilePath' from a
-- BARE (alias-less) import (a genuine cross-module reference that merely happens to print
-- unqualified) -- all three are syntactically identical bare single-segment names, so the
-- distinction can only be made by tracking which names are currently bound and which are locally
-- declared. 'boundNames' starts as the type SCHEME's own quantified variables (the caller's
-- responsibility -- see 'expandAliasesInDefinition').
-- Does NOT walk into terms (a type's only term-valued position is an annotation's payload, which
-- carries no type-level names) -- scope intentionally narrower than 'expandAliasesInTerm' (#805).
expandAliasesInTypeSelfAware :: TypedTermDefinition (ModuleName -> S.Set String -> S.Set String -> M.Map String ModuleName -> Type -> Type)
expandAliasesInTypeSelfAware = define "expandAliasesInTypeSelfAware" $
  doc "Expand aliased AND bare self-referencing names inside a type, tracking bound type variables and the module's own declared names" $
  "modName" ~> "ownNames" ~> "bound0" ~> "aliases" ~> "typ0" ~>
  expandAliasesInTypeSelfAwareGo @@ var "modName" @@ var "aliases" @@ var "ownNames" @@ var "bound0" @@ var "typ0"

-- | Resolve a single bare (post-alias-expansion) name reference against the printer's universe,
-- per Josh's readability request #5 (2026-11-09): the parser-side counterpart of
-- 'Print.Packaging.moduleAliasesWith's bareness gate. Called only on names that have ALREADY run
-- through 'expandAliasedName' (so a genuinely aliased prefix is already expanded) and are not
-- currently in-scope bound variables ('bound', checked by the caller before reaching here).
-- Resolution order, exactly mirroring the spec: (1) if the bare local name is one of 'ownNames'
-- (the module's own declared definitions), it is ALREADY correct as a same-module reference and
-- is returned unchanged -- no qualification needed, since a same-module reference is round-trip-
-- safe without one (#711/#803); (2) otherwise, look it up in 'universe' (every OTHER module's
-- full declared-name set, excluding this module's own namespace) and require EXACTLY one owner --
-- zero or two-or-more owners is a hard error (never guess, per the spec); (3) a dotted (already
-- multi-segment) name reaching here was neither a bound variable nor an own-name nor an
-- alias-table hit, so it is left as-is (same "out of scope" fallback 'expandAliasedName' uses for
-- any name it can't resolve -- a name that is already fully/partly qualified is not this
-- function's problem to solve further).
resolveNameRefWith :: TypedTermDefinition (ModuleName -> S.Set String -> M.Map ModuleName (S.Set Name) -> Name -> Either String Name)
resolveNameRefWith = define "resolveNameRefWith" $
  doc "Resolve a single bare name reference against the printer's universe: own-definitions first, then the unique owning import, hard error on zero or multiple owners (#5/#711)" $
  "modName" ~> "ownNames" ~> "universe" ~> "n" ~>
  "s" <~ Core.unName (var "n") $
  Logic.ifElse (Logic.not (Equality.equal (Lists.length (Strings.splitOn (string ".") (var "s"))) (int32 1)))
    (right (var "n"))
    (Logic.ifElse (Sets.member (var "s") (var "ownNames" :: TypedTerm (S.Set String)))
      (right (var "n"))
      ("owners" <~ Lists.filter
          ("entry" ~> Logic.and
            (Logic.not (Equality.equal (Pairs.first $ var "entry") (var "modName")))
            (Sets.member (var "s" :: TypedTerm String) (Sets.map (asTerm Names.localNameOf) (Pairs.second $ var "entry") :: TypedTerm (S.Set String))))
          (Maps.toList (var "universe" :: TypedTerm (M.Map ModuleName (S.Set Name)))) $
       Logic.ifElse (Equality.equal (Lists.length $ var "owners") (int32 1))
         ("owner" <~ Pairs.first (Optionals.withDefault (pair (var "modName") (Sets.empty :: TypedTerm (S.Set Name))) (Lists.head $ var "owners")) $
          right (Names.qname @@ var "owner" @@ var "s"))
         (Logic.ifElse (Equality.equal (Lists.length $ var "owners") (int32 0))
           (left (Strings.concat (list [
             string "unresolved bare name reference: '", var "s",
             string "' is not a local binder, not one of the module's own definitions, and not declared by any module in scope"])))
           (left (Strings.concat (list [
             string "ambiguous bare name reference: '", var "s", string "' is declared by ",
             Literals.printInt32 (Lists.length $ var "owners"), string " different imported modules in scope"]))))))

-- | Recursively resolve bare name references inside a type against the printer's universe,
-- tracking bound type variables (forall parameters) so a genuinely bound variable is never
-- mistaken for a nominal reference -- same recursive shape as 'expandAliasesInTypeSelfAwareGo',
-- but calling 'resolveNameRefWith' instead of 'expandTypeNameRefSelfAware' for the 'variable'
-- case, and threading 'Either' for the hard-error path.
resolveNameRefsInTypeWith :: TypedTermDefinition (ModuleName -> S.Set String -> M.Map ModuleName (S.Set Name) -> S.Set String -> Type -> Either String Type)
resolveNameRefsInTypeWith = define "resolveNameRefsInTypeWith" $
  doc "Recursively resolve bare name references inside a type against the printer's universe, tracking bound type variables" $
  "modName" ~> "ownNames" ~> "universe" ~> "bound" ~> "typ" ~>
  "recurse" <~ (resolveNameRefsInTypeWith @@ var "modName" @@ var "ownNames" @@ var "universe") $
  "forField" <~ ("boundHere" ~> "field" ~>
    Eithers.map ("t" ~> Core.fieldTypeWithType (var "field") (var "t")) (var "recurse" @@ var "boundHere" @@ (Core.fieldTypeType $ var "field"))) $
  match _Type (var "typ") Nothing [
    _Type_annotated>>: "at" ~> Eithers.map
      ("b" ~> Core.typeAnnotated $ Core.annotatedType (var "b") (Core.annotatedTypeAnnotation $ var "at"))
      (var "recurse" @@ var "bound" @@ (Core.annotatedTypeBody $ var "at")),
    _Type_application>>: "app" ~> Eithers.bind (var "recurse" @@ var "bound" @@ (Core.applicationTypeFunction $ var "app"))
      ("f" ~> Eithers.map ("a" ~> Core.typeApplication $ Core.applicationType (var "f") (var "a")) (var "recurse" @@ var "bound" @@ (Core.applicationTypeArgument $ var "app"))),
    _Type_effect>>: "t" ~> Eithers.map (reify Core.typeEffect) (var "recurse" @@ var "bound" @@ var "t"),
    _Type_either>>: "et" ~> Eithers.bind (var "recurse" @@ var "bound" @@ (Core.eitherTypeLeft $ var "et"))
      ("l" ~> Eithers.map ("r" ~> Core.typeEither $ Core.eitherType (var "l") (var "r")) (var "recurse" @@ var "bound" @@ (Core.eitherTypeRight $ var "et"))),
    _Type_forall>>: "ft" ~>
      "param" <~ Core.forallTypeParameter (var "ft") $
      "bound'" <~ Sets.insert (Core.unName $ var "param") (var "bound" :: TypedTerm (S.Set String)) $
      Eithers.map
        ("b" ~> Core.typeForall $ Core.forallType (var "param") (var "b"))
        (resolveNameRefsInTypeWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ var "bound'" @@ (Core.forallTypeBody $ var "ft")),
    _Type_function>>: "fun" ~> Eithers.bind (var "recurse" @@ var "bound" @@ (Core.functionTypeDomain $ var "fun"))
      ("d" ~> Eithers.map ("c" ~> Core.typeFunction $ Core.functionType (var "d") (var "c")) (var "recurse" @@ var "bound" @@ (Core.functionTypeCodomain $ var "fun"))),
    _Type_list>>: "t" ~> Eithers.map (reify Core.typeList) (var "recurse" @@ var "bound" @@ var "t"),
    _Type_literal>>: "lt" ~> right (Core.typeLiteral $ var "lt"),
    _Type_map>>: "mt" ~> Eithers.bind (var "recurse" @@ var "bound" @@ (Core.mapTypeKeys $ var "mt"))
      ("k" ~> Eithers.map ("v" ~> Core.typeMap $ Core.mapType (var "k") (var "v")) (var "recurse" @@ var "bound" @@ (Core.mapTypeValues $ var "mt"))),
    _Type_optional>>: "t" ~> Eithers.map (reify Core.typeOptional) (var "recurse" @@ var "bound" @@ var "t"),
    _Type_pair>>: "pt" ~> Eithers.bind (var "recurse" @@ var "bound" @@ (Core.pairTypeFirst $ var "pt"))
      ("f" ~> Eithers.map ("s" ~> Core.typePair $ Core.pairType (var "f") (var "s")) (var "recurse" @@ var "bound" @@ (Core.pairTypeSecond $ var "pt"))),
    _Type_record>>: "rt" ~> Eithers.map (reify Core.typeRecord) (Eithers.mapList (var "forField" @@ var "bound") (var "rt")),
    _Type_set>>: "t" ~> Eithers.map (reify Core.typeSet) (var "recurse" @@ var "bound" @@ var "t"),
    _Type_union>>: "rt" ~> Eithers.map (reify Core.typeUnion) (Eithers.mapList (var "forField" @@ var "bound") (var "rt")),
    _Type_unit>>: constant (right Core.typeUnit),
    _Type_variable>>: "n" ~>
      Logic.ifElse (Sets.member (Core.unName $ var "n") (var "bound" :: TypedTerm (S.Set String)))
        (right (Core.typeVariable (var "n")))
        (Eithers.map (reify Core.typeVariable) (resolveNameRefWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ var "n")),
    _Type_void>>: constant (right Core.typeVoid),
    _Type_wrap>>: "wt" ~> Eithers.map (reify Core.typeWrap) (var "recurse" @@ var "bound" @@ var "wt")]

-- | Recursively resolve bare name references inside a term against the printer's universe,
-- tracking bound term variables (lambda parameters, let-binding names) so a genuinely bound
-- variable is never mistaken for a nominal reference -- the term-level counterpart
-- 'expandAliasesInTerm' (by design, #805) leaves unimplemented, now needed for #5's resolution
-- pass. Covers the same name-valued positions 'expandAliasesInTerm' does: 'variable', 'project',
-- 'record', 'unwrap', 'wrap', 'cases', 'inject'.
resolveNameRefsInTermWith :: TypedTermDefinition (ModuleName -> S.Set String -> M.Map ModuleName (S.Set Name) -> S.Set String -> Term -> Either String Term)
resolveNameRefsInTermWith = define "resolveNameRefsInTermWith" $
  doc "Recursively resolve bare name references inside a term against the printer's universe, tracking bound term variables" $
  "modName" ~> "ownNames" ~> "universe" ~> "bound" ~> "term" ~>
  "recurse" <~ (resolveNameRefsInTermWith @@ var "modName" @@ var "ownNames" @@ var "universe") $
  "resolveN" <~ (resolveNameRefWith @@ var "modName" @@ var "ownNames" @@ var "universe") $
  "forField" <~ ("boundHere" ~> "field" ~>
    Eithers.map ("t" ~> Core.fieldWithTerm (var "field") (var "t")) (var "recurse" @@ var "boundHere" @@ (Core.fieldTerm $ var "field"))) $
  "forCaseAlternative" <~ ("boundHere" ~> "alt" ~>
    Eithers.map ("h" ~> Core.caseAlternativeWithHandler (var "alt") (var "h")) (var "recurse" @@ var "boundHere" @@ (Core.caseAlternativeHandler $ var "alt"))) $
  match _Term (var "term") Nothing [
    _Term_annotated>>: "at" ~> Eithers.map
      ("b" ~> Core.termAnnotated $ Core.annotatedTerm (var "b") (Core.annotatedTermAnnotation $ var "at"))
      (var "recurse" @@ var "bound" @@ (Core.annotatedTermBody $ var "at")),
    _Term_application>>: "a" ~> Eithers.bind (var "recurse" @@ var "bound" @@ (Core.applicationFunction $ var "a"))
      ("f" ~> Eithers.map ("x" ~> Core.termApplication $ Core.application (var "f") (var "x")) (var "recurse" @@ var "bound" @@ (Core.applicationArgument $ var "a"))),
    _Term_cases>>: "cs" ~>
      "tn" <<~ var "resolveN" @@ (Core.caseStatementTypeName $ var "cs") $
      "rdflt" <<~ Eithers.mapOptional (var "recurse" @@ var "bound") (Core.caseStatementDefault $ var "cs") $
      Eithers.map
        ("rcases" ~> Core.termCases $ Core.caseStatement (var "tn") (var "rdflt") (var "rcases"))
        (Eithers.mapList (var "forCaseAlternative" @@ var "bound") (Core.caseStatementCases $ var "cs")),
    _Term_either>>: "e" ~>
      "re" <<~ Eithers.either
        ("l" ~> Eithers.map (reify left) $ var "recurse" @@ var "bound" @@ var "l")
        ("r" ~> Eithers.map (reify right) $ var "recurse" @@ var "bound" @@ var "r")
        (var "e") $
      right $ Core.termEither $ var "re",
    _Term_lambda>>: "l" ~>
      "param" <~ Core.lambdaParameter (var "l") $
      "bound'" <~ Sets.insert (Core.unName $ var "param") (var "bound" :: TypedTerm (S.Set String)) $
      Eithers.map
        ("b" ~> Core.termLambda $ Core.lambda (var "param") (Core.lambdaDomain $ var "l") (var "b"))
        (resolveNameRefsInTermWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ var "bound'" @@ (Core.lambdaBody $ var "l")),
    _Term_let>>: "lt" ~>
      "bindingNames" <~ Sets.fromList (Lists.map ("b" ~> Core.unName (Core.bindingName $ var "b")) (Core.letBindings $ var "lt")) $
      "bound'" <~ Sets.union (var "bound" :: TypedTerm (S.Set String)) (var "bindingNames" :: TypedTerm (S.Set String)) $
      "mapBinding" <~ ("b" ~> Eithers.map
        ("t" ~> Core.binding (Core.bindingName $ var "b") (var "t") (Core.bindingTypeScheme $ var "b"))
        (resolveNameRefsInTermWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ var "bound'" @@ (Core.bindingTerm $ var "b"))) $
      "rbindings" <<~ Eithers.mapList (var "mapBinding") (Core.letBindings $ var "lt") $
      Eithers.map
        ("renv" ~> Core.termLet $ Core.let_ (var "rbindings") (var "renv"))
        (resolveNameRefsInTermWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ var "bound'" @@ (Core.letBody $ var "lt")),
    _Term_list>>: "els" ~> Eithers.map (reify Core.termList) (Eithers.mapList (var "recurse" @@ var "bound") (var "els")),
    _Term_literal>>: "v" ~> right (Core.termLiteral $ var "v"),
    _Term_map>>: "m" ~>
      "forPair" <~ ("p" ~>
        "k" <<~ var "recurse" @@ var "bound" @@ (Pairs.first $ var "p") $
        Eithers.map ("v" ~> pair (var "k") (var "v")) (var "recurse" @@ var "bound" @@ (Pairs.second $ var "p"))) $
      Eithers.map ("rpairs" ~> Core.termMap (Maps.fromList $ var "rpairs")) (Eithers.mapList (var "forPair") (Maps.toList (var "m" :: TypedTerm (M.Map Term Term)))),
    _Term_optional>>: "m" ~> Eithers.map (reify Core.termOptional) (Eithers.mapOptional (var "recurse" @@ var "bound") (var "m")),
    _Term_pair>>: "p" ~> Eithers.bind (var "recurse" @@ var "bound" @@ (Pairs.first $ var "p"))
      ("f" ~> Eithers.map ("s" ~> Core.termPair $ pair (var "f") (var "s")) (var "recurse" @@ var "bound" @@ (Pairs.second $ var "p"))),
    _Term_project>>: "p" ~>
      -- The field name is a row-type field label, not a module-level reference (unlike the type
      -- name) -- it must be left untouched, mirroring 'expandAliasesInTerm's own (pre-existing)
      -- bug-for-bug-compatible-but-harmless handling there; here it would hard-error instead of
      -- silently no-oping, so the fix is load-bearing.
      Eithers.map ("tn" ~> Core.termProject $ Core.projection (var "tn") (Core.projectionFieldName $ var "p")) (var "resolveN" @@ (Core.projectionTypeName $ var "p")),
    _Term_record>>: "r" ~>
      "tn" <<~ var "resolveN" @@ (Core.recordTypeName $ var "r") $
      Eithers.map
        ("rfields" ~> Core.termRecord $ Core.record (var "tn") (var "rfields"))
        (Eithers.mapList (var "forField" @@ var "bound") (Core.recordFields $ var "r")),
    _Term_set>>: "s" ~> Eithers.map ("rlist" ~> Core.termSet (Sets.fromList $ var "rlist")) (Eithers.mapList (var "recurse" @@ var "bound") (Sets.toList (var "s" :: TypedTerm (S.Set Term)))),
    _Term_typeApplication>>: "tt" ~> Eithers.map
      ("b" ~> Core.termTypeApplication $ Core.typeApplicationTerm (var "b") (Core.typeApplicationTermType $ var "tt"))
      (var "recurse" @@ var "bound" @@ (Core.typeApplicationTermBody $ var "tt")),
    _Term_typeLambda>>: "ta" ~> Eithers.map
      ("b" ~> Core.termTypeLambda $ Core.typeLambda (Core.typeLambdaParameter $ var "ta") (var "b"))
      (var "recurse" @@ var "bound" @@ (Core.typeLambdaBody $ var "ta")),
    _Term_inject>>: "inj" ~>
      "tn" <<~ var "resolveN" @@ (Core.injectionTypeName $ var "inj") $
      Eithers.map
        ("rfield" ~> Core.termInject $ Core.injection (var "tn") (var "rfield"))
        (var "forField" @@ var "bound" @@ (Core.injectionField $ var "inj")),
    _Term_unit>>: constant (right Core.termUnit),
    _Term_unwrap>>: "n" ~> Eithers.map (reify Core.termUnwrap) (var "resolveN" @@ var "n"),
    _Term_variable>>: "v" ~>
      Logic.ifElse (Sets.member (Core.unName $ var "v") (var "bound" :: TypedTerm (S.Set String)))
        (right (Core.termVariable (var "v")))
        (Eithers.map (reify Core.termVariable) (var "resolveN" @@ var "v")),
    _Term_wrap>>: "wt" ~>
      "tn" <<~ var "resolveN" @@ (Core.wrappedTermTypeName $ var "wt") $
      Eithers.map
        ("b" ~> Core.termWrap $ Core.wrappedTerm (var "tn") (var "b"))
        (var "recurse" @@ var "bound" @@ (Core.wrappedTermBody $ var "wt"))]

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
-- definition's signature and default implementation). 'modName' is the enclosing module's own
-- name, and 'ownNames' is the set of ALL of that module's own declared definition names (across
-- every definition in the module, not just this one) -- both needed only by the type-definition
-- branch's bare-self-reference expansion (see 'expandAliasesInTypeSelfAware') -- term/primitive
-- bodies don't yet get this treatment (#805: the term side has the same bound-vs-free ambiguity
-- for lambda/case/record/wrap names, deferred as a separate, correctly-scoped-out parser
-- limitation; the Types/*.hs batch has no term definitions, so it isn't exercised here).
expandAliasesInDefinition :: TypedTermDefinition (ModuleName -> S.Set String -> M.Map String ModuleName -> Definition -> Definition)
expandAliasesInDefinition = define "expandAliasesInDefinition" $
  doc "Expand every aliased name reference inside a definition's body/signature" $
  "modName" ~> "ownNames" ~> "aliases" ~> "def" ~>
  "expandScheme" <~ ("ts" ~>
    "vars" <~ Core.typeSchemeVariables (var "ts") $
    "boundNames" <~ Sets.fromList (Lists.map ("v" ~> Core.unName (var "v")) (var "vars" :: TypedTerm [Name])) $
    Core.typeScheme (var "vars")
      (expandAliasesInTypeSelfAware @@ (var "modName" :: TypedTerm ModuleName) @@ (var "ownNames" :: TypedTerm (S.Set String)) @@ (var "boundNames" :: TypedTerm (S.Set String)) @@ (var "aliases" :: TypedTerm (M.Map String ModuleName)) @@ (Core.typeSchemeBody $ var "ts"))
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

-- | Resolve bare name references inside a term signature's parameter and result types against
-- the printer's universe (#5/#711), the type-only counterpart of 'resolveNameRefsInTermWith'
-- used by 'resolveNameRefsInDefinitionWith''s primitive-definition branch.
resolveNameRefsInTermSignatureWith :: TypedTermDefinition (ModuleName -> S.Set String -> M.Map ModuleName (S.Set Name) -> TermSignature -> Either String TermSignature)
resolveNameRefsInTermSignatureWith = define "resolveNameRefsInTermSignatureWith" $
  doc "Resolve bare name references inside a term signature's parameter and result types against the printer's universe" $
  "modName" ~> "ownNames" ~> "universe" ~> "sig" ~>
  -- The signature's own '∀'-bound type parameters (e.g. 'bimap : ∀a,b,c,d. ...') are in scope
  -- throughout its parameter/result types -- seeded here, mirroring 'resolveNameRefsInDefinitionWith's
  -- 'resolveScheme' handling of a type definition's 'TypeScheme.variables' the same way. Leaving this
  -- empty (the pre-fix bug) treated every type parameter as an unresolvable nominal reference.
  "boundNames" <~ Sets.fromList (Lists.map ("tp" ~> Core.unName (Typing.typeParameterName $ var "tp")) (Typing.termSignatureTypeParameters $ var "sig")) $
  "resolveT" <~ (resolveNameRefsInTypeWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ (var "boundNames" :: TypedTerm (S.Set String))) $
  "mapParam" <~ ("p" ~> Eithers.map
    ("t" ~> Typing.parameter (Typing.parameterName $ var "p") (Typing.parameterDescription $ var "p") (var "t") (Typing.parameterIsLazy $ var "p"))
    (var "resolveT" @@ (Typing.parameterType $ var "p"))) $
  "rparams" <<~ Eithers.mapList (var "mapParam") (Typing.termSignatureParameters $ var "sig") $
  "r" <~ Typing.termSignatureResult (var "sig") $
  Eithers.map
    ("rt" ~> Typing.termSignature (Typing.termSignatureTypeParameters $ var "sig") (var "rparams")
      (Typing.result (Typing.resultDescription $ var "r") (var "rt")))
    (var "resolveT" @@ (Typing.resultType $ var "r"))

-- | Resolve every remaining bare name reference inside a definition's body/signature against the
-- printer's universe (#5/#711) -- the dispatcher 'parseModuleWith' runs AFTER 'moduleParser'
-- (which has already expanded every ALIASED reference via 'expandAliasesInDefinition'; what
-- reaches here is only the names that were bare, unaliased, and not one of the module's own
-- declarations, i.e. exactly the references 'Print.Packaging.moduleAliasesWith' may have decided
-- to leave bare). A type definition's own type-scheme variables seed the bound set, same as
-- 'expandAliasesInDefinition''s 'expandScheme'.
resolveNameRefsInDefinitionWith :: TypedTermDefinition (ModuleName -> S.Set String -> M.Map ModuleName (S.Set Name) -> Definition -> Either String Definition)
resolveNameRefsInDefinitionWith = define "resolveNameRefsInDefinitionWith" $
  doc "Resolve every remaining bare name reference inside a definition's body/signature against the printer's universe" $
  "modName" ~> "ownNames" ~> "universe" ~> "def" ~>
  "resolveScheme" <~ ("ts" ~>
    "vars" <~ Core.typeSchemeVariables (var "ts") $
    "boundNames" <~ Sets.fromList (Lists.map ("v" ~> Core.unName (var "v")) (var "vars" :: TypedTerm [Name])) $
    Eithers.map
      ("b" ~> Core.typeScheme (var "vars") (var "b") (Core.typeSchemeConstraints $ var "ts"))
      (resolveNameRefsInTypeWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ (var "boundNames" :: TypedTerm (S.Set String)) @@ (Core.typeSchemeBody $ var "ts"))) $
  match _Definition (var "def") Nothing [
    _Definition_type>>: "td" ~> Eithers.map
      ("ts" ~> Packaging.definitionType (Packaging.typeDefinition (Packaging.typeDefinitionName $ var "td") (Packaging.typeDefinitionMetadata $ var "td") (var "ts")))
      (var "resolveScheme" @@ (Packaging.typeDefinitionBody $ var "td")),
    _Definition_term>>: "td" ~>
      "rsig" <<~ Eithers.mapOptional (resolveNameRefsInTermSignatureWith @@ var "modName" @@ var "ownNames" @@ var "universe") (Packaging.termDefinitionSignature $ var "td") $
      Eithers.map
        ("rbody" ~> Packaging.definitionTerm (Packaging.termDefinition (Packaging.termDefinitionName $ var "td") (Packaging.termDefinitionMetadata $ var "td") (var "rsig") (var "rbody")))
        (resolveNameRefsInTermWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ (Sets.empty :: TypedTerm (S.Set String)) @@ (Packaging.termDefinitionBody $ var "td")),
    _Definition_primitive>>: "pd" ~>
      "rsig" <<~ resolveNameRefsInTermSignatureWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ (Packaging.primitiveDefinitionSignature $ var "pd") $
      Eithers.map
        ("rimpl" ~> Packaging.definitionPrimitive (Packaging.primitiveDefinition (Packaging.primitiveDefinitionName $ var "pd") (Packaging.primitiveDefinitionMetadata $ var "pd") (var "rsig") (Packaging.primitiveDefinitionIsPure $ var "pd") (Packaging.primitiveDefinitionIsTotal $ var "pd") (var "rimpl")))
        (Eithers.mapOptional (resolveNameRefsInTermWith @@ var "modName" @@ var "ownNames" @@ var "universe" @@ (Sets.empty :: TypedTerm (S.Set String))) (Packaging.primitiveDefinitionDefaultImplementation $ var "pd"))]

-- | Parse a complete module from a string, given the declared-name sets of modules available to
-- look up ('Print.Packaging.moduleAliasesWith's exact counterpart, #5/#711). Reuses 'parseModule's
-- grammar and alias expansion unchanged (every ALIASED reference is already resolved by the time
-- this function sees the parsed 'Module'), then resolves every remaining bare reference: local
-- binder (left alone by the walkers above), then the module's own definitions (left alone --
-- already correct), then the unique module AMONG THIS FILE'S OWN RECOVERED IMPORTS
-- ('Packaging.moduleDependencies' on the just-parsed 'Module' -- every real dependency, aliased
-- or bare-safe, always has a printed import line per staging's 2026-11-01 ruling: dropping the
-- line for a bare-safe dependency would mean the text no longer states its own dependencies,
-- undermining '.hydra' becoming a self-contained build input) that declares it (rewritten to
-- fully qualified). 'universe' is consulted ONLY to look up each of THOSE imports' declared-name
-- sets -- it is not itself the resolution scope, and modules outside the file's own import list
-- are never consulted even if 'universe' happens to contain them (an unrelated module sharing a
-- local name poses no real ambiguity risk, since it was never relevant to the printer's bareness
-- decision either). Zero or multiple declaring modules among the file's own imports is a hard
-- parse error -- never guess, per the spec. With an empty universe this is identical to
-- 'parseModule' (every bare name either resolves to 'ownNames' or is left untouched, since no
-- import's declared set is known to prove anything), matching
-- 'moduleAliases'/'moduleAliasesWith's own empty-map symmetry on the printer side. This function
-- is therefore SELF-CONTAINED given 'universe' -- unlike an earlier draft, it does not require the
-- caller to separately narrow 'universe' down to "the original module's own imports" out of band
-- (that context only exists while a non-textual source of truth is still around; after '.hydra'
-- ingestion lands, there is no "original module" to ask -- only this file's own text).
-- | 'parseModuleWith', additionally given the set of every fully-qualified UNION type name across
-- the whole universe (#5/#837), so a cross-module one-field construct (e.g.
-- 'variants.LiteralVariant{binary}', a union defined in ANOTHER module) parses as an injection
-- rather than a mis-parsed record. 'correctInjectionsInTerm' (run inside 'parseModule', before this
-- function ever sees the parsed 'Module') only has the printed module's OWN type definitions to
-- consult, so it cannot tell a cross-module union from a cross-module record; this pass runs AFTER
-- bare-name resolution (so every type name reaching it is already fully qualified) and corrects
-- exactly the cases 'correctInjectionsInTerm' could not -- an own-module union was already
-- corrected by the first pass and is a no-op here (its one-field record is already an injection).
-- An empty 'unionTypeNames' set is a no-op, matching 'parseModuleWith's own empty-universe
-- degenerate case.
parseModuleWith :: TypedTermDefinition (M.Map ModuleName (S.Set Name) -> S.Set Name -> String -> Either String Module)
parseModuleWith = define "parseModuleWith" $
  doc "Parse a complete module from a string, resolving bare name references and cross-module union injections against the universe (#5/#711/#837)" $
  "universe" ~> "unionTypeNames" ~> "input" ~>
  "parsed" <<~ parseModule @@ var "input" $
  "modName" <~ Packaging.moduleName (var "parsed") $
  "ownNames" <~ Sets.fromList (Lists.map
    ("def" ~> Names.localNameOf @@ (match _Definition (var "def") Nothing [
      _Definition_type>>: "td" ~> Packaging.typeDefinitionName (var "td"),
      _Definition_term>>: "td" ~> Packaging.termDefinitionName (var "td"),
      _Definition_primitive>>: "pd" ~> Packaging.primitiveDefinitionName (var "pd")]))
    (Packaging.moduleDefinitions $ var "parsed")) $
  -- Resolution candidates are the file's own recovered imports -- never the whole 'universe' the
  -- caller happens to supply, and never derived from anything but THIS file's text (see this
  -- function's own doc comment for why).
  "declaredImportModNames" <~ Lists.map ("d" ~> Packaging.moduleDependencyModule $ var "d") (Packaging.moduleDependencies $ var "parsed") $
  "scopedUniverse" <~ Maps.fromList (Optionals.givens (Lists.map
    ("nm" ~> Optionals.map ("declared" ~> pair (var "nm") (var "declared")) (Maps.lookup (var "nm") (var "universe" :: TypedTerm (M.Map ModuleName (S.Set Name)))))
    (var "declaredImportModNames"))) $
  "rdefs" <<~ Eithers.mapList (resolveNameRefsInDefinitionWith @@ var "modName" @@ var "ownNames" @@ (var "scopedUniverse" :: TypedTerm (M.Map ModuleName (S.Set Name)))) (Packaging.moduleDefinitions $ var "parsed") $
  "cdefs" <~ Lists.map (correctCrossModuleInjectionsInDefinition @@ (var "unionTypeNames" :: TypedTerm (S.Set Name))) (var "rdefs") $
  right (Packaging.module_ (var "modName") (Packaging.moduleMetadata $ var "parsed") (Packaging.moduleDependencies $ var "parsed") (var "cdefs"))

-- | True iff a type, once stripped of its 'forall' quantifiers and annotations, is a union type
-- (#809). Used to decide whether a one-field record-style construct over this type name is actually
-- an injection.
isUnionType :: TypedTermDefinition (Type -> Bool)
isUnionType = define "isUnionType" $
  doc "True iff a type (after stripping foralls/annotations) is a union type (#809)" $
  "typ" ~> match _Type (Strip.deannotateTypeParameters @@ var "typ") (Just false) [
    _Type_union>>: constant true]

-- | Build a type environment from a module's own type definitions: a map from each defined type's
-- (fully-qualified) name to its scheme body (#809). The only consumer is 'correctInjectionsInTerm',
-- which needs just enough schema to tell a union type from a record type. Imported types are not
-- included here (a future extension would merge the ambient schema); this suffices for same-module
-- references, including forward references within the module.
typeEnvFromDefinitions :: TypedTermDefinition ([Definition] -> M.Map Name Type)
typeEnvFromDefinitions = define "typeEnvFromDefinitions" $
  doc "Map each of a module's own type definitions to its (deannotated) scheme body (#809)" $
  "defs" ~>
  Maps.fromList (Optionals.givens (Lists.map
    ("def" ~> match _Definition (var "def") (Just nothing) [
      _Definition_type>>: "td" ~> just (pair
        (Packaging.typeDefinitionName $ var "td")
        (Core.typeSchemeBody $ Packaging.typeDefinitionBody $ var "td"))])
    (var "defs")))

-- | Correct the nominal-construction sugar's record/injection ambiguity (#809): rewrite a
-- one-field Term.record whose type name resolves (in the given env) to a UNION type into a
-- Term.inject with that single field. Records over record types, records with 0 or 2+ fields, and
-- records over types absent from the env are left unchanged (a multi-field record over a union is
-- malformed and left for the type checker). Recurses through the whole term via Rewriting.rewriteTerm.
correctInjectionsInTerm :: TypedTermDefinition (M.Map Name Type -> Term -> Term)
correctInjectionsInTerm = define "correctInjectionsInTerm" $
  doc "Rewrite a one-field record over a union type into an injection, per the type env (#809)" $
  "env" ~> "t0" ~>
  "rewrite" <~ ("recurse" ~> "term" ~>
    match _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_record>>: "r" ~>
        "tname" <~ Core.recordTypeName (var "r") $
        "flds" <~ Core.recordFields (var "r") $
        Optionals.match (Maps.lookup (var "tname") (var "env" :: TypedTerm (M.Map Name Type)))
          -- type name not in env: leave as a record
          (var "recurse" @@ var "term")
          ("ty" ~>
            -- union type with exactly one field: correct to an injection (matching 'Lists.head' to
            -- extract that single field -- 'head' is optional-returning, so this also guards the
            -- length-1 invariant). Any other shape (record type, 0 or 2+ fields) stays a record.
            Logic.ifElse
              (Logic.and (isUnionType @@ var "ty")
                (Equality.equal (Lists.length $ var "flds") (int32 1)))
              (Optionals.match (Lists.head $ var "flds")
                (var "recurse" @@ var "term")
                ("f" ~> var "recurse" @@ (Core.termInject (Core.injection (var "tname") (var "f")))))
              (var "recurse" @@ var "term"))]) $
  Rewriting.rewriteTerm @@ var "rewrite" @@ var "t0"

-- | Apply 'correctInjectionsInTerm' to a definition's term-bearing bodies (term body + primitive
-- default implementation), per the given type env (#809). Type definitions carry no term body.
correctInjectionsInDefinition :: TypedTermDefinition (M.Map Name Type -> Definition -> Definition)
correctInjectionsInDefinition = define "correctInjectionsInDefinition" $
  doc "Apply the record->injection correction to a definition's term bodies, per the type env (#809)" $
  "env" ~> "def" ~>
  "correct" <~ (correctInjectionsInTerm @@ (var "env" :: TypedTerm (M.Map Name Type))) $
  match _Definition (var "def") Nothing [
    _Definition_type>>: constant $ var "def",
    _Definition_term>>: "td" ~> Packaging.definitionTerm (Packaging.termDefinition
      (Packaging.termDefinitionName $ var "td") (Packaging.termDefinitionMetadata $ var "td")
      (Packaging.termDefinitionSignature $ var "td")
      (var "correct" @@ (Packaging.termDefinitionBody $ var "td"))),
    _Definition_primitive>>: "pd" ~> Packaging.definitionPrimitive (Packaging.primitiveDefinition
      (Packaging.primitiveDefinitionName $ var "pd") (Packaging.primitiveDefinitionMetadata $ var "pd")
      (Packaging.primitiveDefinitionSignature $ var "pd")
      (Packaging.primitiveDefinitionIsPure $ var "pd") (Packaging.primitiveDefinitionIsTotal $ var "pd")
      (Optionals.map (var "correct") (Packaging.primitiveDefinitionDefaultImplementation $ var "pd")))]

-- | Rewrite a one-field 'Term.record' whose type name is a KNOWN UNION NAME (from the given set of
-- fully-qualified union type names, #5/#837) into a 'Term.inject' -- the cross-module counterpart
-- of 'correctInjectionsInTerm', which only sees the printed module's OWN type definitions and so
-- cannot tell a cross-module union from a cross-module record (a bare one-field
-- 'variants.LiteralVariant{binary}' construct stays a mis-parsed record forever without this).
-- Compares the (already fully-qualified, post-alias-expansion) type name directly against the set
-- -- no local-name stripping, since by the time 'parseModuleWith' runs this pass every name in the
-- term is already fully qualified (own-module bare references were left bare on purpose and so
-- never match a cross-module union's fully-qualified name here, which is correct: a bare reference
-- can only mean an OWN definition or an unqualified import, never literally match a dotted name).
correctCrossModuleInjectionsInTerm :: TypedTermDefinition (S.Set Name -> Term -> Term)
correctCrossModuleInjectionsInTerm = define "correctCrossModuleInjectionsInTerm" $
  doc "Rewrite a one-field record over a KNOWN cross-module union type name into an injection (#5/#837)" $
  "unionNames" ~> "t0" ~>
  "rewrite" <~ ("recurse" ~> "term" ~>
    match _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_record>>: "r" ~>
        "tname" <~ Core.recordTypeName (var "r") $
        "flds" <~ Core.recordFields (var "r") $
        Logic.ifElse
          (Logic.and (Sets.member (var "tname" :: TypedTerm Name) (var "unionNames" :: TypedTerm (S.Set Name)))
            (Equality.equal (Lists.length $ var "flds") (int32 1)))
          (Optionals.match (Lists.head $ var "flds")
            (var "recurse" @@ var "term")
            ("f" ~> var "recurse" @@ (Core.termInject (Core.injection (var "tname") (var "f")))))
          (var "recurse" @@ var "term")]) $
  Rewriting.rewriteTerm @@ var "rewrite" @@ var "t0"

-- | Apply 'correctCrossModuleInjectionsInTerm' to a definition's term-bearing bodies, mirroring
-- 'correctInjectionsInDefinition' (#5/#837).
correctCrossModuleInjectionsInDefinition :: TypedTermDefinition (S.Set Name -> Definition -> Definition)
correctCrossModuleInjectionsInDefinition = define "correctCrossModuleInjectionsInDefinition" $
  doc "Apply the cross-module record->injection correction to a definition's term bodies (#5/#837)" $
  "unionNames" ~> "def" ~>
  "correct" <~ (correctCrossModuleInjectionsInTerm @@ (var "unionNames" :: TypedTerm (S.Set Name))) $
  match _Definition (var "def") Nothing [
    _Definition_type>>: constant $ var "def",
    _Definition_term>>: "td" ~> Packaging.definitionTerm (Packaging.termDefinition
      (Packaging.termDefinitionName $ var "td") (Packaging.termDefinitionMetadata $ var "td")
      (Packaging.termDefinitionSignature $ var "td")
      (var "correct" @@ (Packaging.termDefinitionBody $ var "td"))),
    _Definition_primitive>>: "pd" ~> Packaging.definitionPrimitive (Packaging.primitiveDefinition
      (Packaging.primitiveDefinitionName $ var "pd") (Packaging.primitiveDefinitionMetadata $ var "pd")
      (Packaging.primitiveDefinitionSignature $ var "pd")
      (Packaging.primitiveDefinitionIsPure $ var "pd") (Packaging.primitiveDefinitionIsTotal $ var "pd")
      (Optionals.map (var "correct") (Packaging.primitiveDefinitionDefaultImplementation $ var "pd")))]

-- | Remove every span annotation from a type. A type's only TERM-valued position is an
-- annotation's payload (e.g. a whole-definition or field-level '@{description=...}' map, #711) --
-- unlike 'Positions.stripSpans' (which walks a 'Term' and recurses into nested terms directly),
-- this walks the surrounding 'Type' structure via 'Rewriting.rewriteType' (mirroring
-- 'Strip.removeTypeAnnotations', the proven precedent for a 'Type -> Type' span/annotation
-- remover) and applies 'Positions.stripSpans' only at each 'Type_annotated' node's own annotation
-- payload. Needed because the parser attaches source spans to every parsed term (#815),
-- including the description term embedded in a parsed type's annotation -- a freshly-parsed
-- type's description otherwise carries a stray 'sourceSpan' entry that a hand-built (unparsed)
-- type never has, breaking structural comparison between the two. Lives here (rather than in
-- 'Strip.hs', alongside 'removeTypeAnnotations') because 'Strip.hs' is upstream of 'Positions.hs'
-- in the module dependency graph (via 'Annotations.hs'), so importing 'Positions' from 'Strip'
-- would create a cycle; this module already depends on 'Positions' directly.
stripSpansInType :: TypedTermDefinition (Type -> Type)
stripSpansInType = define "stripSpansInType" $
  doc "Remove every span annotation from a type's own annotation payloads" $
  "typ" ~>
  "remove" <~ ("recurse" ~> "typ" ~>
    "rewritten" <~ var "recurse" @@ var "typ" $
    match _Type (var "rewritten")
      (Just $ var "rewritten") [
      _Type_annotated>>: "at" ~> Core.typeAnnotated $ Core.annotatedType
        (Core.annotatedTypeBody $ var "at")
        (Positions.stripSpans @@ (Core.annotatedTypeAnnotation $ var "at"))]) $
  Rewriting.rewriteType @@ var "remove" @@ var "typ"

-- | Parse a type definition: ['##' doc comment] 'type name = typeScheme'. The parsed scheme's
-- body is run through 'stripSpansInType' to remove the source spans the parser attaches to every
-- parsed term (#815), including the description term nested in an '@{description=...}'
-- annotation -- without this, a freshly-parsed type's description carries a stray 'sourceSpan'
-- entry that a hand-built (unparsed) type never has, breaking structural comparison (#711).
typeDefinitionParser :: TypedTermDefinition (ModuleName -> Parser Definition)
typeDefinitionParser = define "typeDefinitionParser" $
  doc "Parse a type definition: ['##' doc comment] 'type name = typeScheme'" $
  "modName" ~>
  P.bind @@ (docCommentBlock @@ var "modName") @@ ("mmeta" ~>
    P.bind @@ (keyword @@ string "type") @@ (constant $
      P.bind @@ identifier @@ ("local" ~>
        P.bind @@ (keyword @@ string "=") @@ (constant $
          P.map @@ ("ts" ~> Packaging.definitionType
              (Packaging.typeDefinition (qualifyLocalName @@ var "modName" @@ var "local")
                (var "mmeta")
                (Core.typeScheme (Core.typeSchemeVariables $ var "ts")
                  (stripSpansInType @@ (Core.typeSchemeBody $ var "ts"))
                  (Core.typeSchemeConstraints $ var "ts")))) @@
            (token @@ (asTerm ParseCore.typeScheme))))))

-- | Parse a term definition: 'term name [: termSignature] = term'. The optional ': termSignature'
-- ascription becomes the TermDefinition's signature, parsed directly as a TermSignature (not via
-- a TypeScheme bridge) so that a parameter's 'lazy' keyword (#800) round-trips. No doc-comment
-- parsing -- metadata is always Nothing.
termDefinitionParser :: TypedTermDefinition (ModuleName -> Parser Definition)
termDefinitionParser = define "termDefinitionParser" $
  doc "Parse a term definition: ['##' doc comment] 'term name [: termSignature] = term'" $
  "modName" ~>
  P.bind @@ (docCommentBlock @@ var "modName") @@ ("mmeta" ~>
    P.bind @@ (keyword @@ string "term") @@ (constant $
      P.bind @@ identifier @@ ("local" ~>
        P.bind @@ (P.optional @@ ascribedSignature) @@ ("msig" ~>
          P.bind @@ (keyword @@ string "=") @@ (constant $
            P.map @@ ("t" ~> Packaging.definitionTerm
                (Packaging.termDefinition (qualifyLocalName @@ var "modName" @@ var "local") (var "mmeta") (var "msig") (var "t"))) @@
              (token @@ (P.map @@ Positions.stripSpans @@ (asTerm ParseCore.term))))))))
  where
    ascribedSignature =
      P.bind @@ (keyword @@ string ":") @@ (constant $ asTerm ParseCore.termSignature)

-- | Parse a primitive definition: '[impure] [partial] primitive name : termSignature [= term]'.
-- No doc-comment parsing -- metadata is always Nothing.
primitiveDefinitionParser :: TypedTermDefinition (ModuleName -> Parser Definition)
primitiveDefinitionParser = define "primitiveDefinitionParser" $
  doc "Parse a primitive definition: ['##' doc comment] '[impure] [partial] primitive name : termSignature [= term]'" $
  "modName" ~>
  P.bind @@ (docCommentBlock @@ var "modName") @@ ("mmeta" ~>
    P.bind @@ primitiveModifiers @@ ("mods" ~>
      "isPure" <~ Pairs.first (var "mods") $
      "isTotal" <~ Pairs.second (var "mods") $
      P.bind @@ (keyword @@ string "primitive") @@ (constant $
        P.bind @@ identifier @@ ("local" ~>
          P.bind @@ (keyword @@ string ":") @@ (constant $
            P.bind @@ (asTerm ParseCore.termSignature) @@ ("sig" ~>
              P.map @@ ("mimpl" ~> Packaging.definitionPrimitive
                  (Packaging.primitiveDefinition (qualifyLocalName @@ var "modName" @@ var "local") (var "mmeta")
                    (var "sig") (var "isPure") (var "isTotal") (var "mimpl"))) @@
                (P.optional @@ defaultImpl)))))))
  where
    defaultImpl = P.bind @@ (keyword @@ string "=") @@ (constant $ token @@ (P.map @@ Positions.stripSpans @@ (asTerm ParseCore.term)))

-- | Parse a definition: a type, term, or primitive definition, dispatching on the leading
-- keyword (the primitive-modifier keywords 'impure'/'partial' also mark a primitive definition,
-- so they are tried via 'primitiveDefinitionParser' itself, which consumes them unconditionally
-- before deciding; 'type'/'term'/'primitive' otherwise disambiguate directly, so no backtracking
-- is required beyond the three-way 'P.choice').
-- | Parse a single definition: type, term, or primitive. Each alternative starts with its own
-- independent 'docCommentBlock' call (so a leading '##' is consumed before the keyword that
-- actually decides which kind of definition this is), so a definition kind mismatch (e.g. a
-- '##' block followed by 'term', tried first against 'typeDefinitionParser') fails only AFTER
-- consuming the doc comment -- non-zero consumption, so 'P.choice'/'alt's PEG-style
-- backtracking can't fall through to the next alternative without 'P.try' wrapping each one.
definitionParser :: TypedTermDefinition (ModuleName -> Parser Definition)
definitionParser = define "definitionParser" $
  doc "Parse a single definition: type, term, or primitive" $
  "modName" ~>
  P.choice @@ list [
    P.try @@ (typeDefinitionParser @@ var "modName"),
    P.try @@ (termDefinitionParser @@ var "modName"),
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
  doc ("Parse a complete module: ['##' doc comment] header, imports, then definitions. Aliased"
    <> " name references inside definition bodies (per syntax.md #6.3's 'import X as alias'"
    <> " convenience) are expanded to their fully-qualified form after parsing, using the alias"
    <> " table built from this module's own import declarations -- see"
    <> " 'expandAliasesInDefinition'. The module's own leading doc comment is captured as plain"
    <> " lines (via 'docCommentLines') BEFORE 'moduleHeader' runs -- unlike a definition's doc"
    <> " comment ('docCommentBlock'), which already has its enclosing module's name in scope to"
    <> " qualify provision names with, the module-level comment precedes the very 'module ...'"
    <> " line that introduces that name; the lines are captured first and only turned into an"
    <> " 'EntityMetadata' (via 'docCommentLinesToMetadata') once 'modName' is available. After"
    <> " alias expansion, the record/injection sugar ambiguity (#809) is corrected against a type"
    <> " env built from the (now fully-qualified) type definitions.") $
  P.bind @@ (asTerm docCommentLines) @@ ("lns" ~>
    P.bind @@ moduleHeader @@ ("modName" ~>
      P.bind @@ (P.many @@ importDecl) @@ ("imports" ~>
        "aliases" <~ (Maps.fromList (Optionals.givens (Lists.map
            ("p" ~> Optionals.map ("alias" ~> pair (var "alias") (Packaging.moduleDependencyModule (Pairs.first $ var "p"))) (Pairs.second $ var "p"))
            (var "imports"))) :: TypedTerm (M.Map String ModuleName)) $
        "mmeta" <~ Logic.ifElse (Lists.isEmpty (var "lns" :: TypedTerm [String]))
          nothing
          (just $ docCommentLinesToMetadata @@ var "modName" @@ var "lns") $
        P.map @@ ("defs" ~>
            -- 1. expand import aliases to fully-qualified names (#803/#804), AND resolve
            --    same-module bare self-references vs. bound type variables (#711) -- both need
            --    the module's own name. A bare name only counts as a self-reference if it is
            --    actually one of THIS module's own declared names ('ownNames', mirroring the
            --    printer's identical 'ownLocalNames' computation in 'moduleAliases') -- otherwise
            --    it is a genuine reference into one of the module's bare (alias-less) imports,
            --    and must be left untouched so the printer can still see it as an unqualified
            --    dependency and print the bare 'import' line for it (round-trip fix, #711).
            "ownNames" <~ Sets.fromList (Lists.map
              ("def" ~> Names.localNameOf @@ (match _Definition (var "def") Nothing [
                _Definition_type>>: "td" ~> Packaging.typeDefinitionName (var "td"),
                _Definition_term>>: "td" ~> Packaging.termDefinitionName (var "td"),
                _Definition_primitive>>: "pd" ~> Packaging.primitiveDefinitionName (var "pd")]))
              (var "defs")) $
            "expanded" <~ Lists.map (expandAliasesInDefinition @@ (var "modName" :: TypedTerm ModuleName) @@ (var "ownNames" :: TypedTerm (S.Set String)) @@ (var "aliases" :: TypedTerm (M.Map String ModuleName))) (var "defs") $
            -- 2. build the type env from the (now fully-qualified) type definitions, then correct
            --    the record/injection sugar ambiguity against it (#809). Done AFTER alias
            --    expansion so the env keys and the record type names being looked up are both
            --    fully qualified.
            "env" <~ (typeEnvFromDefinitions @@ var "expanded") $
            Packaging.module_ (var "modName") (var "mmeta")
              (Lists.map ("p" ~> Pairs.first (var "p")) (var "imports"))
              (Lists.map (correctInjectionsInDefinition @@ (var "env" :: TypedTerm (M.Map Name Type))) (var "expanded"))) @@
          (P.many @@ (definitionParser @@ var "modName")))))

-- | Parse a complete module from a string, requiring the parse to consume all input (after
-- trailing whitespace).
parseModule :: TypedTermDefinition (String -> Either String Module)
parseModule = define "parseModule" $
  doc "Parse a complete module from a string, requiring the parse to consume all input" $
  "input" ~>
  match _ParseResult (P.runParser @@ (P.bind @@ whitespace @@ (constant $ asTerm moduleParser)) @@ var "input") Nothing [
    _ParseResult_success>>: "s" ~>
      Logic.ifElse (Lists.isEmpty (Parsing.parseInputCodepoints $ Parsing.parseSuccessRemainder $ var "s"))
        (right (Parsing.parseSuccessValue (var "s")))
        (left (string "trailing input after module")),
    _ParseResult_failure>>: "e" ~> left (Parsing.parseErrorMessage (var "e"))]
