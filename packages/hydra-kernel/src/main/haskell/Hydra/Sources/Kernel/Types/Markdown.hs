module Hydra.Sources.Kernel.Types.Markdown where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T


ns :: ModuleName
ns = ModuleName "hydra.markdown"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = [],
            moduleMetadata = descriptionMetadata (Just "A Markdown document model, used as the target representation for generated documentation")}
  where
    definitions = [
      block,
      codeBlock,
      document,
      heading,
      inline,
      link,
      list,
      listItem,
      paragraph,
      section,
      table,
      tableCell,
      tableRow]

block :: TypeDefinition
block = define "Block" $
  doc "A block-level Markdown element" $
  T.union [
    "codeBlock">:
      doc "A fenced code block"
      codeBlock,
    "list">:
      doc "An ordered or unordered list"
      list,
    "paragraph">:
      doc "A paragraph of inline content"
      paragraph,
    "raw">:
      doc ("Raw, verbatim passthrough content (e.g. an HTML comment), emitted with no escaping or"
        ++ " reformatting. Used for constructs the rest of the Block union cannot represent, such as"
        ++ " the generated-file notice banner at the top of a generated page.")
      T.string,
    "section">:
      doc "A heading followed by its own block content, nested to form a document outline"
      section,
    "table">:
      doc "A table"
      table]

codeBlock :: TypeDefinition
codeBlock = define "CodeBlock" $
  doc "A fenced code block" $
  T.record [
    "language">:
      doc "The optional language tag used for syntax highlighting, e.g. \"haskell\"" $
      T.optional T.string,
    "content">:
      doc "The literal text content of the code block"
      T.string]

document :: TypeDefinition
document = define "Document" $
  doc "A complete Markdown document" $
  T.record [
    "title">:
      doc "The document's top-level title, rendered as an H1 heading"
      T.string,
    "content">:
      doc "The block-level content of the document, following the title" $
      T.list block]

heading :: TypeDefinition
heading = define "Heading" $
  doc "A section heading" $
  T.record [
    "level">:
      doc "The heading level, where 1 corresponds to an H1 heading; levels increase with nesting depth"
      T.int32,
    "content">:
      doc "The inline content of the heading" $
      T.list inline,
    "anchor">:
      doc ("An optional explicit anchor identifier for linking directly to this heading. Headings"
        ++ " can usually derive an anchor from their rendered text, but an explicit anchor is"
        ++ " provided for cases (e.g. renderer-specific slug rules) where that derivation is"
        ++ " ambiguous or undesired.") $
      T.optional T.string]

inline :: TypeDefinition
inline = define "Inline" $
  doc "An inline Markdown element" $
  T.union [
    "code">:
      doc "An inline code span"
      T.string,
    "emphasis">:
      doc "Emphasized (italicized) text" $
      T.list inline,
    "link">:
      doc "A hyperlink"
      link,
    "strong">:
      doc "Strongly emphasized (bolded) text" $
      T.list inline,
    "text">:
      doc "A raw text fragment"
      T.string]

link :: TypeDefinition
link = define "Link" $
  doc "A hyperlink" $
  T.record [
    "text">:
      doc "The visible link text" $
      T.list inline,
    "target">:
      doc "The link target, either a URL or a relative path"
      T.string]

list :: TypeDefinition
list = define "List" $
  doc "An ordered or unordered list" $
  T.record [
    "ordered">:
      doc "Whether the list is numbered (ordered) rather than bulleted (unordered)"
      T.boolean,
    "items">:
      doc "The items of the list" $
      T.list listItem]

listItem :: TypeDefinition
listItem = define "ListItem" $
  doc "A single item of a list" $
  T.wrap $ T.list block

paragraph :: TypeDefinition
paragraph = define "Paragraph" $
  doc "A paragraph of inline content" $
  T.record [
    "content">:
      doc "The inline content of the paragraph" $
      T.list inline,
    "anchor">:
      doc ("An optional explicit anchor identifier for linking directly to this paragraph. Needed"
        ++ " for elements such as rendered provisions, which are bold-led prose within a paragraph"
        ++ " rather than a heading, so they carry no heading-derived anchor of their own.") $
      T.optional T.string]

section :: TypeDefinition
section = define "Section" $
  doc "A heading followed by its own block content, nested to form a document outline" $
  T.record [
    "heading">:
      doc "The section's heading"
      heading,
    "content">:
      doc "The block-level content of the section, following the heading" $
      T.list block,
    "anchor">:
      doc ("An optional explicit anchor identifier for linking directly to this section, distinct"
        ++ " from the heading's own anchor (a section's anchor identifies the section as a whole;"
        ++ " a heading's identifies the heading line specifically).") $
      T.optional T.string]

table :: TypeDefinition
table = define "Table" $
  doc "A table, consisting of a header row and any number of data rows" $
  T.record [
    "header">:
      doc "The header row of the table"
      tableRow,
    "rows">:
      doc "The data rows of the table" $
      T.list tableRow]

tableCell :: TypeDefinition
tableCell = define "TableCell" $
  doc "A single cell of a table row" $
  T.wrap $ T.list inline

tableRow :: TypeDefinition
tableRow = define "TableRow" $
  doc "A single row of a table" $
  T.wrap $ T.list tableCell
