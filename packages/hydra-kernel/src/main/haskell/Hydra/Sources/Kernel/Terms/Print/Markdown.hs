module Hydra.Sources.Kernel.Terms.Print.Markdown where

-- Standard imports for kernel terms modules
-- _Table/_Table_header/_Table_rows are hidden: hydra.tabular already defines a Table type,
-- and this module defines its own hydra.markdown Table renderer/name-constants -- same
-- resolution idiom used throughout Terms/*.hs for a local binding shadowing a Hydra.Kernel
-- re-export (e.g. Environment.hs, Checking.hs).
import Hydra.Kernel hiding (_Table, _Table_header, _Table_rows)
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms hiding (list)
-- Separate QUALIFIED-ONLY import for the one Phantoms name this module's own `list` renderer
-- collides with: `hiding` on an unqualified import removes the name from ALL access (including
-- qualified `Phantoms.list`), not just unqualified use, so a second qualified-only import is
-- needed to keep `Phantoms.list` callable at its one genuine call site below.
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms (list)
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.print.markdown"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Formatting.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Serialization of hydra.markdown documents to Markdown text")}
  where
   definitions = [
     toDefinition block,
     toDefinition blocks,
     toDefinition codeBlock,
     toDefinition document,
     toDefinition heading,
     toDefinition inline,
     toDefinition inlines,
     toDefinition link,
     toDefinition list,
     toDefinition table,
     toDefinition tableCell,
     toDefinition tableRow]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- Name constants for hydra.markdown types and fields (not yet in generated dist).
-- Field-name constants follow the codebase's `_<TypeName>_<fieldName>` convention for
-- `project`, confirmed against existing usage (e.g. `project _TypeDefinition
-- _TypeDefinition_name` in Environment.hs) -- distinct from the `_<TypeName>_<variantName>`
-- convention used for union variants with `cases`/`match` (e.g. `_Inline_text` below).
_Block :: Name
_Block = Name "hydra.markdown.Block"
_Block_codeBlock :: Name
_Block_codeBlock = Name "codeBlock"
_Block_list :: Name
_Block_list = Name "list"
_Block_paragraph :: Name
_Block_paragraph = Name "paragraph"
_Block_raw :: Name
_Block_raw = Name "raw"
_Block_section :: Name
_Block_section = Name "section"
_Block_table :: Name
_Block_table = Name "table"

_CodeBlock :: Name
_CodeBlock = Name "hydra.markdown.CodeBlock"
_CodeBlock_language :: Name
_CodeBlock_language = Name "language"
_CodeBlock_content :: Name
_CodeBlock_content = Name "content"

_Document :: Name
_Document = Name "hydra.markdown.Document"
_Document_title :: Name
_Document_title = Name "title"
_Document_content :: Name
_Document_content = Name "content"

_Heading :: Name
_Heading = Name "hydra.markdown.Heading"
_Heading_level :: Name
_Heading_level = Name "level"
_Heading_content :: Name
_Heading_content = Name "content"
_Heading_anchor :: Name
_Heading_anchor = Name "anchor"

_Inline :: Name
_Inline = Name "hydra.markdown.Inline"
_Inline_code :: Name
_Inline_code = Name "code"
_Inline_emphasis :: Name
_Inline_emphasis = Name "emphasis"
_Inline_link :: Name
_Inline_link = Name "link"
_Inline_strong :: Name
_Inline_strong = Name "strong"
_Inline_text :: Name
_Inline_text = Name "text"

_Link :: Name
_Link = Name "hydra.markdown.Link"
_Link_text :: Name
_Link_text = Name "text"
_Link_target :: Name
_Link_target = Name "target"

_List :: Name
_List = Name "hydra.markdown.List"
_List_ordered :: Name
_List_ordered = Name "ordered"
_List_items :: Name
_List_items = Name "items"

_ListItem :: Name
_ListItem = Name "hydra.markdown.ListItem"

_Paragraph :: Name
_Paragraph = Name "hydra.markdown.Paragraph"
_Paragraph_content :: Name
_Paragraph_content = Name "content"
_Paragraph_anchor :: Name
_Paragraph_anchor = Name "anchor"

_Section :: Name
_Section = Name "hydra.markdown.Section"
_Section_heading :: Name
_Section_heading = Name "heading"
_Section_content :: Name
_Section_content = Name "content"
_Section_anchor :: Name
_Section_anchor = Name "anchor"

_Table :: Name
_Table = Name "hydra.markdown.Table"
_Table_header :: Name
_Table_header = Name "header"
_Table_rows :: Name
_Table_rows = Name "rows"

_TableCell :: Name
_TableCell = Name "hydra.markdown.TableCell"

_TableRow :: Name
_TableRow = Name "hydra.markdown.TableRow"

block :: TypedTermDefinition (Term -> String)
block = define "block" $
  doc "Render a single Block to its Markdown text form" $
  lambda "b" $
  cases _Block Nothing [
    _Block_codeBlock>>: lambda "cb" $ codeBlock @@ var "cb",
    _Block_list>>:      lambda "l"  $ list @@ var "l",
    _Block_paragraph>>: lambda "p"  $ inlines @@ (project _Paragraph _Paragraph_content @@ var "p"),
    _Block_raw>>:       lambda "s"  $ var "s",
    _Block_section>>:   lambda "s"  $
      "h" <~ (project _Section _Section_heading @@ var "s") $
      "content" <~ (project _Section _Section_content @@ var "s") $
      Strings.join (string "\n\n") $ Lists.cons (heading @@ var "h") (Lists.map (asTerm block) (var "content")),
    _Block_table>>:     lambda "t"  $ table @@ var "t"]
  @@ var "b"

blocks :: TypedTermDefinition ([Term] -> String)
blocks = define "blocks" $
  doc "Render a list of Blocks, joined by a blank line" $
  lambda "bs" $ Strings.join (string "\n\n") (Lists.map (asTerm block) (var "bs"))

codeBlock :: TypedTermDefinition (Term -> String)
codeBlock = define "codeBlock" $
  doc "Render a CodeBlock as a fenced code block, with the language tag on the opening fence if present" $
  lambda "cb" $
  "lang" <~ (project _CodeBlock _CodeBlock_language @@ var "cb") $
  "content" <~ (project _CodeBlock _CodeBlock_content @@ var "cb") $
  "fence" <~ (string "```" ++ Optionals.withDefault (string "") (var "lang")) $
  Strings.join (string "\n") (Phantoms.list [var "fence", var "content", string "```"])

document :: TypedTermDefinition (Term -> String)
document = define "document" $
  doc "Render a Document as its complete Markdown text, an H1 title followed by its blocks" $
  lambda "d" $
  "title" <~ (project _Document _Document_title @@ var "d") $
  "content" <~ (project _Document _Document_content @@ var "d") $
  Strings.join (string "\n\n") $ Lists.cons (string "# " ++ var "title") (Lists.map (asTerm block) (var "content"))

heading :: TypedTermDefinition (Term -> String)
heading = define "heading" $
  doc "Render a Heading, prefixing its inline content with the appropriate number of `#` characters" $
  lambda "h" $
  "level" <~ (project _Heading _Heading_level @@ var "h") $
  "content" <~ (project _Heading _Heading_content @@ var "h") $
  "hashes" <~ Strings.concat (Lists.replicate (var "level") (string "#")) $
  var "hashes" ++ string " " ++ (inlines @@ var "content")

inline :: TypedTermDefinition (Term -> String)
inline = define "inline" $
  doc "Render a single Inline to its Markdown text form" $
  lambda "i" $
  cases _Inline Nothing [
    _Inline_code>>:     lambda "s"  $ string "`" ++ var "s" ++ string "`",
    _Inline_emphasis>>: lambda "is" $ string "*" ++ (inlines @@ var "is") ++ string "*",
    _Inline_link>>:     lambda "lk" $ link @@ var "lk",
    _Inline_strong>>:   lambda "is" $ string "**" ++ (inlines @@ var "is") ++ string "**",
    _Inline_text>>:     lambda "s"  $ var "s"]
  @@ var "i"

inlines :: TypedTermDefinition ([Term] -> String)
inlines = define "inlines" $
  doc "Render a list of Inlines, concatenated with no separator" $
  lambda "is" $ Strings.concat (Lists.map (asTerm inline) (var "is"))

link :: TypedTermDefinition (Term -> String)
link = define "link" $
  doc "Render a Link in the standard `[text](target)` form" $
  lambda "lk" $
  "text" <~ (project _Link _Link_text @@ var "lk") $
  "target" <~ (project _Link _Link_target @@ var "lk") $
  string "[" ++ (inlines @@ var "text") ++ string "](" ++ var "target" ++ string ")"

list :: TypedTermDefinition (Term -> String)
list = define "list" $
  doc "Render a List as bulleted (`-`) or numbered (`1.`, `2.`, ...) lines, one per item; item content is joined with a blank line for items spanning multiple blocks" $
  lambda "l" $
  "ordered" <~ (project _List _List_ordered @@ var "l") $
  "items" <~ (project _List _List_items @@ var "l") $
  "itemContent" <~ ("it" ~> blocks @@ (unwrap _ListItem @@ var "it")) $
  "indices" <~ (Math.range (int32 0) (Lists.length $ var "items")) $
  "indexedItems" <~ (Lists.zip (var "indices") (var "items")) $
  "renderIndexed" <~ ("ip" ~>
    "idx" <~ Pairs.first (var "ip") $
    "it" <~ Pairs.second (var "ip") $
    "marker" <~ Logic.ifElse (var "ordered")
      (Literals.printInt32 (Math.add (var "idx") (int32 1)) ++ string ".")
      (string "-") $
    var "marker" ++ string " " ++ (var "itemContent" @@ var "it")) $
  Strings.join (string "\n") (Lists.map (var "renderIndexed") (var "indexedItems"))

table :: TypedTermDefinition (Term -> String)
table = define "table" $
  doc "Render a Table in GitHub-flavored Markdown pipe-table form, with a `---` separator row after the header" $
  lambda "t" $
  "header" <~ (project _Table _Table_header @@ var "t") $
  "rows" <~ (project _Table _Table_rows @@ var "t") $
  "headerCells" <~ (unwrap _TableRow @@ var "header") $
  "sepRow" <~ (string "|" ++ Strings.concat (Lists.map ("_" ~> string " --- |") (var "headerCells"))) $
  Strings.join (string "\n") $
    Lists.cons (tableRow @@ var "header") $
    Lists.cons (var "sepRow") $
    Lists.map (asTerm tableRow) (var "rows")

tableCell :: TypedTermDefinition (Term -> String)
tableCell = define "tableCell" $
  doc "Render a TableCell's inline content" $
  lambda "c" $ inlines @@ (unwrap _TableCell @@ var "c")

tableRow :: TypedTermDefinition (Term -> String)
tableRow = define "tableRow" $
  doc "Render a TableRow as a pipe-delimited line" $
  lambda "r" $
  "cells" <~ (unwrap _TableRow @@ var "r") $
  string "| " ++ Strings.join (string " | ") (Lists.map (asTerm tableCell) (var "cells")) ++ string " |"
