module Hydra.Sources.Lsp.Protocol where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Dsl.Annotations
import           Hydra.Core.Overlay.Haskell.Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

-- Additional imports
import qualified Hydra.Sources.Json.Model        as JsonModel

ns :: ModuleName
ns = ModuleName "hydra.ext.lsp.protocol"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [JsonModel.ns, Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A subset of the Language Server Protocol 3.17 types, sufficient for diagnostics and formatting. "
              ++ "Positions are 0-based; characters are UTF-16 code units. "
              ++ "Enumerations (e.g. DiagnosticSeverity) are unions; the JSON codec maps them to the protocol's integers "
              ++ "(error=1, warning=2, information=3, hint=4; TextDocumentSyncKind none=0, full=1, incremental=2)."))}
  where
    def = datatype ns
    lsp = typeref ns
    json = typeref JsonModel.ns

    -- Alphabetical order by local type name, per the definition-ordering style guide.
    definitions = [
      def "Diagnostic" $
        doc "A diagnostic, such as a compiler error or warning" $
        T.record [
          "range">: lsp "Range",
          "severity">: T.optional $ lsp "DiagnosticSeverity",
          "code">: T.optional T.string,
          "source">: T.optional T.string,
          "message">: T.string],

      def "DiagnosticSeverity" $
        doc "The severity of a diagnostic; encoded on the wire as an integer 1-4" $
        T.union [
          "error">: T.unit,
          "warning">: T.unit,
          "information">: T.unit,
          "hint">: T.unit],

      def "DidChangeParams" $
        doc "Parameters of textDocument/didChange" $
        T.record [
          "textDocument">: lsp "VersionedTextDocumentIdentifier",
          "contentChanges">: T.list $ lsp "TextDocumentContentChangeEvent"],

      def "DidCloseParams" $
        doc "Parameters of textDocument/didClose" $
        T.record [
          "textDocument">: lsp "TextDocumentIdentifier"],

      def "DidOpenParams" $
        doc "Parameters of textDocument/didOpen" $
        T.record [
          "textDocument">: lsp "TextDocumentItem"],

      def "DocumentFormattingParams" $
        doc "Parameters of textDocument/formatting" $
        T.record [
          "textDocument">: lsp "TextDocumentIdentifier",
          "options">: lsp "FormattingOptions"],

      def "FormattingOptions" $
        doc "Formatting options (subset)" $
        T.record [
          "tabSize">: T.int32,
          "insertSpaces">: T.boolean],

      def "InitializeParams" $
        doc "Parameters of the initialize request (subset)" $
        T.record [
          "processId">: T.optional T.int32,
          "rootUri">: T.optional T.string],

      def "InitializeResult" $
        doc "The result of the initialize request" $
        T.record [
          "capabilities">: lsp "ServerCapabilities"],

      def "JsonRpcMessage" $
        doc "A JSON-RPC 2.0 message envelope; params, result and error payloads are generic JSON, decoded by the handlers" $
        T.record [
          "id">: T.optional $ json "Value",
          "method">: T.optional T.string,
          "params">: T.optional $ json "Value",
          "result">: T.optional $ json "Value",
          "error">: T.optional $ json "Value"],

      def "Position" $
        doc "A zero-based position in a text document; character is a UTF-16 code-unit offset" $
        T.record [
          "line">: T.int32,
          "character">: T.int32],

      def "PublishDiagnosticsParams" $
        doc "Parameters of textDocument/publishDiagnostics" $
        T.record [
          "uri">: T.string,
          "version">: T.optional T.int32,
          "diagnostics">: T.list $ lsp "Diagnostic"],

      def "Range" $
        doc "A range in a text document, from start (inclusive) to end (exclusive)" $
        T.record [
          "start">: lsp "Position",
          "end">: lsp "Position"],

      def "ServerCapabilities" $
        doc "The capabilities this server advertises; only implemented features are listed" $
        T.record [
          "textDocumentSync">: lsp "TextDocumentSyncKind",
          "documentFormattingProvider">: T.boolean],

      def "TextDocumentContentChangeEvent" $
        doc "A content change; under full sync this is the entire new text" $
        T.record [
          "text">: T.string],

      def "TextDocumentIdentifier" $
        doc "Identifies a text document by URI" $
        T.record [
          "uri">: T.string],

      def "TextDocumentItem" $
        doc "An opened text document" $
        T.record [
          "uri">: T.string,
          "languageId">: T.string,
          "version">: T.int32,
          "text">: T.string],

      def "TextDocumentSyncKind" $
        doc "How document changes are synced; encoded on the wire as an integer 0-2" $
        T.union [
          "none">: T.unit,
          "full">: T.unit,
          "incremental">: T.unit],

      def "TextEdit" $
        doc "A textual edit applied to a document" $
        T.record [
          "range">: lsp "Range",
          "newText">: T.string],

      def "VersionedTextDocumentIdentifier" $
        doc "Identifies a specific version of a text document" $
        T.record [
          "uri">: T.string,
          "version">: T.int32]]
