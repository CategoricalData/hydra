{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Lsp.Wire where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (Result)
import qualified Hydra.Core.Dsl.Lib.Strings                as Strings
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Core.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Core.Dsl.Paths                      as Paths
import qualified Hydra.Core.Dsl.Ast                        as Ast
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base                       as MetaBase
import qualified Hydra.Core.Dsl.Coders                     as Coders
import qualified Hydra.Core.Dsl.Util                    as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core                       as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Core.Dsl.Json.Model                       as Json
import qualified Hydra.Core.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality               as Equality
import qualified Hydra.Core.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Core.Dsl.Lib.Literals               as Literals
import qualified Hydra.Core.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Core.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Core.Dsl.Lib.Math                   as Math
import qualified Hydra.Core.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Core.Dsl.Packaging                     as Packaging
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Core.Dsl.Topology                   as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Core.Dsl.Typing                     as Typing
import qualified Hydra.Core.Dsl.Util                       as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants  as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Scientific                           as Sci
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Core.Json.Model as JM
import qualified Hydra.Ext.Lsp.Protocol as Lsp
import qualified Hydra.Sources.Json.Model as JsonModelSource
import qualified Hydra.Sources.Lsp.Protocol as LspProtocol

-- | The LSP wire translation layer (#836). LSP's JSON is NOT Hydra's native JSON format: enumerations are fixed integers
-- (e.g. DiagnosticSeverity Error=1..Hint=4, TextDocumentSyncKind None=0..Incremental=2), optional fields are omitted
-- rather than null, and records are plain objects. These definitions translate explicitly between
-- hydra.ext.lsp.protocol types and LSP JSON, in Hydra so they generate to every host.
-- Status: DRAFT, not yet compiled (no Haskell lane was available); expect fixes on first typecheck.

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.ext.lsp.wire"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (LspProtocol.ns:JsonModelSource.ns:KernelTypes.kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Explicit translation between hydra.ext.lsp.protocol types and the LSP JSON wire format")}
  where
    definitions = [
      toDefinition decodeDidChange,
      toDefinition decodeDidClose,
      toDefinition decodeDidOpen,
      toDefinition decodeFormatting,
      toDefinition encodeDiagnostic,
      toDefinition encodeInitializeResult,
      toDefinition encodePublishDiagnostics,
      toDefinition encodeRange,
      toDefinition encodeSeverity,
      toDefinition encodeSyncKind,
      toDefinition encodeTextEdit,
      toDefinition fieldInt,
      toDefinition fieldObject,
      toDefinition fieldString,
      toDefinition jsonInt,
      toDefinition jsonObject,
      toDefinition jsonString,
      toDefinition lookupField]

-- Local helpers building JSON values -------------------------------------------------------------

jstr :: TypedTerm String -> TypedTerm JM.Value
jstr s = inject JM._Value JM._Value_string s

-- Definitions ------------------------------------------------------------------------------------

jsonInt :: TypedTermDefinition (Int -> JM.Value)
jsonInt = define "jsonInt" $
  doc "A JSON number from an int32" $
  lambda "i" $ inject JM._Value JM._Value_number (Literals.bigintToDecimal (Literals.int32ToBigint (var "i")))

jsonObject :: TypedTermDefinition ([(String, JM.Value)] -> JM.Value)
jsonObject = define "jsonObject" $
  doc "A JSON object from key/value pairs" $
  lambda "ps" $ inject JM._Value JM._Value_object (var "ps")

jsonString :: TypedTermDefinition (String -> JM.Value)
jsonString = define "jsonString" $
  doc "A JSON string" $
  lambda "s" $ jstr (var "s")

encodeSeverity :: TypedTermDefinition (Lsp.DiagnosticSeverity -> JM.Value)
encodeSeverity = define "encodeSeverity" $
  doc "DiagnosticSeverity as its LSP integer: error=1, warning=2, information=3, hint=4" $
  lambda "s" $ jsonInt @@ (match Lsp._DiagnosticSeverity (var "s") Nothing [
    Lsp._DiagnosticSeverity_error>>: constant (int32 1),
    Lsp._DiagnosticSeverity_warning>>: constant (int32 2),
    Lsp._DiagnosticSeverity_information>>: constant (int32 3),
    Lsp._DiagnosticSeverity_hint>>: constant (int32 4)])

encodeSyncKind :: TypedTermDefinition (Lsp.TextDocumentSyncKind -> JM.Value)
encodeSyncKind = define "encodeSyncKind" $
  doc "TextDocumentSyncKind as its LSP integer: none=0, full=1, incremental=2" $
  lambda "k" $ jsonInt @@ (match Lsp._TextDocumentSyncKind (var "k") Nothing [
    Lsp._TextDocumentSyncKind_none>>: constant (int32 0),
    Lsp._TextDocumentSyncKind_full>>: constant (int32 1),
    Lsp._TextDocumentSyncKind_incremental>>: constant (int32 2)])

encodePosition' :: TypedTerm Lsp.Position -> TypedTerm JM.Value
encodePosition' p = jsonObject @@ list [
  pair (string "line") (jsonInt @@ (project Lsp._Position Lsp._Position_line @@ p)),
  pair (string "character") (jsonInt @@ (project Lsp._Position Lsp._Position_character @@ p))]

encodeRange :: TypedTermDefinition (Lsp.Range -> JM.Value)
encodeRange = define "encodeRange" $
  doc "A Range as {start:{line,character}, end:{line,character}}" $
  lambda "r" $ jsonObject @@ list [
    pair (string "start") (encodePosition' (project Lsp._Range Lsp._Range_start @@ var "r")),
    pair (string "end") (encodePosition' (project Lsp._Range Lsp._Range_end @@ var "r"))]

encodeDiagnostic :: TypedTermDefinition (Lsp.Diagnostic -> JM.Value)
encodeDiagnostic = define "encodeDiagnostic" $
  doc "A Diagnostic; absent optional fields are omitted, never null" $
  lambda "d" $ jsonObject @@ Lists.concat (list [
    list [pair (string "range") (encodeRange @@ (project Lsp._Diagnostic Lsp._Diagnostic_range @@ var "d"))],
    Optionals.match (project Lsp._Diagnostic Lsp._Diagnostic_severity @@ var "d") (list ([] :: [TypedTerm (String, JM.Value)]))
      (lambda "s" $ list [pair (string "severity") (encodeSeverity @@ var "s")]),
    Optionals.match (project Lsp._Diagnostic Lsp._Diagnostic_code @@ var "d") (list ([] :: [TypedTerm (String, JM.Value)]))
      (lambda "c" $ list [pair (string "code") (jstr (var "c"))]),
    Optionals.match (project Lsp._Diagnostic Lsp._Diagnostic_source @@ var "d") (list ([] :: [TypedTerm (String, JM.Value)]))
      (lambda "s" $ list [pair (string "source") (jstr (var "s"))]),
    list [pair (string "message") (jstr (project Lsp._Diagnostic Lsp._Diagnostic_message @@ var "d"))]])

encodePublishDiagnostics :: TypedTermDefinition (Lsp.PublishDiagnosticsParams -> JM.Value)
encodePublishDiagnostics = define "encodePublishDiagnostics" $
  doc "Params of textDocument/publishDiagnostics" $
  lambda "p" $ jsonObject @@ Lists.concat (list [
    list [pair (string "uri") (jstr (project Lsp._PublishDiagnosticsParams Lsp._PublishDiagnosticsParams_uri @@ var "p"))],
    Optionals.match (project Lsp._PublishDiagnosticsParams Lsp._PublishDiagnosticsParams_version @@ var "p") (list ([] :: [TypedTerm (String, JM.Value)]))
      (lambda "v" $ list [pair (string "version") (jsonInt @@ var "v")]),
    list [pair (string "diagnostics") (inject JM._Value JM._Value_array
      (Lists.map (asTerm encodeDiagnostic) (project Lsp._PublishDiagnosticsParams Lsp._PublishDiagnosticsParams_diagnostics @@ var "p")))]])

encodeInitializeResult :: TypedTermDefinition (Lsp.InitializeResult -> JM.Value)
encodeInitializeResult = define "encodeInitializeResult" $
  doc "InitializeResult: {capabilities:{textDocumentSync:<int>, documentFormattingProvider:<bool>}}" $
  lambda "r" $
    lets ["caps">: project Lsp._InitializeResult Lsp._InitializeResult_capabilities @@ var "r"] $
    jsonObject @@ list [pair (string "capabilities") (jsonObject @@ list [
      pair (string "textDocumentSync") (encodeSyncKind @@ (project Lsp._ServerCapabilities Lsp._ServerCapabilities_textDocumentSync @@ var "caps")),
      pair (string "documentFormattingProvider") (inject JM._Value JM._Value_boolean
        (project Lsp._ServerCapabilities Lsp._ServerCapabilities_documentFormattingProvider @@ var "caps"))])]

encodeTextEdit :: TypedTermDefinition (Lsp.TextEdit -> JM.Value)
encodeTextEdit = define "encodeTextEdit" $
  doc "A TextEdit as {range, newText}" $
  lambda "e" $ jsonObject @@ list [
    pair (string "range") (encodeRange @@ (project Lsp._TextEdit Lsp._TextEdit_range @@ var "e")),
    pair (string "newText") (jstr (project Lsp._TextEdit Lsp._TextEdit_newText @@ var "e"))]

lookupField :: TypedTermDefinition (String -> JM.Value -> Either String JM.Value)
lookupField = define "lookupField" $
  doc "Look up a key in a JSON object, failing if the value is not an object or the key is absent" $
  lambda "k" $ lambda "v" $
    match JM._Value (var "v") (Just (Phantoms.left (string "expected a JSON object"))) [
      JM._Value_object>>: lambda "ps" $
        Optionals.match (Maps.lookup (var "k") (Maps.fromList (var "ps")))
          (Phantoms.left (Strings.concat2 (string "missing field: ") (var "k")))
          (lambda "x" $ Phantoms.right (var "x"))]

fieldString :: TypedTermDefinition (String -> JM.Value -> Either String String)
fieldString = define "fieldString" $
  doc "A required string field of a JSON object" $
  lambda "k" $ lambda "v" $
    Eithers.bind (lookupField @@ var "k" @@ var "v") (lambda "x" $
      match JM._Value (var "x") (Just (Phantoms.left (Strings.concat2 (string "expected a string for field: ") (var "k")))) [
        JM._Value_string>>: lambda "s" $ Phantoms.right (var "s")])

fieldInt :: TypedTermDefinition (String -> JM.Value -> Either String Int)
fieldInt = define "fieldInt" $
  doc "A required integer field of a JSON object" $
  lambda "k" $ lambda "v" $
    Eithers.bind (lookupField @@ var "k" @@ var "v") (lambda "x" $
      match JM._Value (var "x") (Just (Phantoms.left (Strings.concat2 (string "expected a number for field: ") (var "k")))) [
        JM._Value_number>>: lambda "n" $ Phantoms.right (Literals.bigintToInt32 (Literals.decimalToBigint (var "n")))])

fieldObject :: TypedTermDefinition (String -> JM.Value -> Either String JM.Value)
fieldObject = define "fieldObject" $
  doc "A required object-valued field of a JSON object" $
  lambda "k" $ lambda "v" $ lookupField @@ var "k" @@ var "v"

decodeDidOpen :: TypedTermDefinition (JM.Value -> Either String Lsp.DidOpenParams)
decodeDidOpen = define "decodeDidOpen" $
  doc "Decode textDocument/didOpen params" $
  lambda "v" $
    Eithers.bind (fieldObject @@ string "textDocument" @@ var "v") (lambda "td" $
    Eithers.bind (fieldString @@ string "uri" @@ var "td") (lambda "uri" $
    Eithers.bind (fieldString @@ string "languageId" @@ var "td") (lambda "lang" $
    Eithers.bind (fieldInt @@ string "version" @@ var "td") (lambda "ver" $
    Eithers.bind (fieldString @@ string "text" @@ var "td") (lambda "text" $
      Phantoms.right $ record Lsp._DidOpenParams [
        Lsp._DidOpenParams_textDocument>>: record Lsp._TextDocumentItem [
          Lsp._TextDocumentItem_uri>>: var "uri",
          Lsp._TextDocumentItem_languageId>>: var "lang",
          Lsp._TextDocumentItem_version>>: var "ver",
          Lsp._TextDocumentItem_text>>: var "text"]])))))

decodeDidChange :: TypedTermDefinition (JM.Value -> Either String Lsp.DidChangeParams)
decodeDidChange = define "decodeDidChange" $
  doc "Decode textDocument/didChange params (full sync: every change carries the whole text)" $
  lambda "v" $
    Eithers.bind (fieldObject @@ string "textDocument" @@ var "v") (lambda "td" $
    Eithers.bind (fieldString @@ string "uri" @@ var "td") (lambda "uri" $
    Eithers.bind (fieldInt @@ string "version" @@ var "td") (lambda "ver" $
    Eithers.bind (fieldObject @@ string "contentChanges" @@ var "v") (lambda "cs" $
      match JM._Value (var "cs") (Just (Phantoms.left (string "expected contentChanges array"))) [
        JM._Value_array>>: lambda "xs" $
          Eithers.map
            (lambda "texts" $ record Lsp._DidChangeParams [
              Lsp._DidChangeParams_textDocument>>: record Lsp._VersionedTextDocumentIdentifier [
                Lsp._VersionedTextDocumentIdentifier_uri>>: var "uri",
                Lsp._VersionedTextDocumentIdentifier_version>>: var "ver"],
              Lsp._DidChangeParams_contentChanges>>: Lists.map
                (lambda "t" $ record Lsp._TextDocumentContentChangeEvent [Lsp._TextDocumentContentChangeEvent_text>>: var "t"])
                (var "texts")])
            (Eithers.mapList (fieldString @@ string "text") (var "xs"))]))))

decodeDidClose :: TypedTermDefinition (JM.Value -> Either String Lsp.DidCloseParams)
decodeDidClose = define "decodeDidClose" $
  doc "Decode textDocument/didClose params" $
  lambda "v" $
    Eithers.bind (fieldObject @@ string "textDocument" @@ var "v") (lambda "td" $
    Eithers.map
      (lambda "uri" $ record Lsp._DidCloseParams [
        Lsp._DidCloseParams_textDocument>>: record Lsp._TextDocumentIdentifier [Lsp._TextDocumentIdentifier_uri>>: var "uri"]])
      (fieldString @@ string "uri" @@ var "td"))

decodeFormatting :: TypedTermDefinition (JM.Value -> Either String Lsp.DocumentFormattingParams)
decodeFormatting = define "decodeFormatting" $
  doc "Decode textDocument/formatting params; options default to tabSize 2 / insertSpaces true when absent" $
  lambda "v" $
    Eithers.bind (fieldObject @@ string "textDocument" @@ var "v") (lambda "td" $
    Eithers.map
      (lambda "uri" $ record Lsp._DocumentFormattingParams [
        Lsp._DocumentFormattingParams_textDocument>>: record Lsp._TextDocumentIdentifier [Lsp._TextDocumentIdentifier_uri>>: var "uri"],
        Lsp._DocumentFormattingParams_options>>: record Lsp._FormattingOptions [
          Lsp._FormattingOptions_tabSize>>: int32 2,
          Lsp._FormattingOptions_insertSpaces>>: boolean True]])
      (fieldString @@ string "uri" @@ var "td"))
