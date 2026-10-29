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


ns :: ModuleName
ns = ModuleName "hydra.core.print.model"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.core.model types")}
  where
   definitions = [
     toDefinition binding,
     toDefinition bindingAliased,
     toDefinition caseStatement,
     toDefinition either_,
     toDefinition escapeBacktickChar,
     toDefinition field,
     toDefinition fieldTermIsUnit,
     toDefinition fieldType,
     toDefinition fields,
     toDefinition floatValue,
     toDefinition floatType,
     toDefinition hexDigit,
     toDefinition injection,
     toDefinition integerValue,
     toDefinition integerType,
     toDefinition isBareName,
     toDefinition isBareSegment,
     toDefinition isReservedWord,
     toDefinition isVariableTerm,
     toDefinition lambda,
     toDefinition let_,
     toDefinition letAliased,
     toDefinition list_,
     toDefinition literal,
     toDefinition literalType,
     toDefinition (map_ :: TypedTermDefinition ((Int -> String) -> (v -> String) -> M.Map Int v -> String)),
     toDefinition optional_,
     toDefinition pair_,
     toDefinition printBacktickedName,
     toDefinition printBinderName,
     toDefinition printName,
     toDefinition printNameAliased,
     toDefinition projection,
     toDefinition (set_ :: TypedTermDefinition ((Int -> String) -> S.Set Int -> String)),
     toDefinition sugarField,
     toDefinition sugarFields,
     toDefinition term,
     toDefinition termAliased,
     toDefinition termAtPrec,
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

-- | Wrap a rendered construct in parentheses iff its own precedence is looser than the required
-- context. `ownPrec` is a Haskell-level constant (known at DSL-authoring time, one of the
-- `prec*` constants above); `ctx` is the DSL-level (TypedTerm) context precedence threaded
-- through `termAtPrec`/`typeAtPrec`'s recursive calls.
parenthesizeIfNeeded :: Int -> TypedTerm Int -> TypedTerm String -> TypedTerm String
parenthesizeIfNeeded ownPrec ctx s = Logic.ifElse (Ordering.lt (int32 ownPrec) ctx)
  (Strings.concat $ list [string "(", s, string ")"])
  s

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

fieldType :: TypedTermDefinition (M.Map ModuleName String -> FieldType -> String)
fieldType = define "fieldType" $
  doc "Show a field type as a string, shortening nominal references in its type via the given alias map (#803)" $
  "aliases" ~> "ft" ~>
  "fname" <~ printName @@ (Core.fieldTypeName $ var "ft") $
  "ftyp" <~ Core.fieldTypeType (var "ft") $
  Strings.concat $ list [
    var "fname",
    string ":",
    typeAliased @@ var "aliases" @@ var "ftyp"]

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
  string "decimal", string "effect", string "either", string "false", string "float32",
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

lambda :: TypedTermDefinition (M.Map ModuleName String -> Lambda -> String)
lambda = define "lambda" $
  doc "Show a lambda as a string, shortening nominal references in its domain/body via the given alias map (#803)" $
  "aliases" ~> "l" ~>
  "v" <~ printBinderName @@ (Core.lambdaParameter $ var "l") $
  "mt" <~ Core.lambdaDomain (var "l") $
  "body" <~ Core.lambdaBody (var "l") $
  "typeStr" <~ Optionals.match (var "mt") (string "") ("t" ~> Strings.concat2 (string ":") (typeAliased @@ var "aliases" @@ var "t")) $
  Strings.concat $ list [
    string "λ",
    var "v",
    var "typeStr",
    string ".",
    termAliased @@ var "aliases" @@ var "body"]

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
    _Literal_string>>: "s" ~> Literals.printString $ var "s"]

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
termAliased :: TypedTermDefinition (M.Map ModuleName String -> Term -> String)
termAliased = define "termAliased" $
  doc "Show a term as a string, shortening nominal references via the given import-alias map (#803)" $
  "aliases" ~> "t" ~> termAtPrec @@ var "aliases" @@ int32 precMin @@ var "t"

-- | Show a term as a string in a given minimum-precedence context, wrapping in parentheses
-- exactly when the term's own construct is looser than that context (see the precedence
-- constants above, and docs/specification/syntax.md #3-4). The alias map (#803) is threaded
-- through every recursive call so nominal references inside the term are shortened consistently.
termAtPrec :: TypedTermDefinition (M.Map ModuleName String -> Int -> Term -> String)
termAtPrec = define "termAtPrec" $
  doc "Show a term as a string at a given minimum context precedence, adding minimal parentheses" $
  "aliases" ~> "ctx" ~> "t" ~>
  "gatherTerms" <~ ("prev" ~> "app" ~>
    "lhs" <~ Core.applicationFunction (var "app") $
    "rhs" <~ Core.applicationArgument (var "app") $
    match _Term (var "lhs")
      (Just $ Lists.cons (var "lhs") (Lists.cons (var "rhs") (var "prev"))) [
      _Term_application>>: "app2" ~> var "gatherTerms" @@ (Lists.cons (var "rhs") (var "prev")) @@ var "app2"]) $
  match _Term (var "t") Nothing [
    _Term_annotated>>: "at" ~>
      "bodyStr" <~ termAtPrec @@ var "aliases" @@ int32 precPostfix @@ (Core.annotatedTermBody $ var "at") $
      "annStr" <~ termAtPrec @@ var "aliases" @@ int32 precPostfix @@ (Core.annotatedTermAnnotation $ var "at") $
      parenthesizeIfNeeded precPostfix (var "ctx") $
        Strings.concat $ list [var "bodyStr", string "@", var "annStr"],
    _Term_application>>: "app" ~>
      "terms" <~ var "gatherTerms" @@ (list ([] :: [TypedTerm Term])) @@ var "app" $
      "termStrs" <~ Lists.map ("e" ~> termAtPrec @@ var "aliases" @@ int32 precPostfix @@ var "e") (var "terms") $
      parenthesizeIfNeeded precApp (var "ctx") $
        Strings.join (string " ") (var "termStrs"),
    _Term_cases>>: "cs" ~> caseStatement @@ var "aliases" @@ var "cs",
    _Term_either>>: "e" ~> Eithers.either
      ("l" ~> Strings.concat $ list [
        string "left(",
        termAliased @@ var "aliases" @@ var "l",
        string ")"])
      ("r" ~> Strings.concat $ list [
        string "right(",
        termAliased @@ var "aliases" @@ var "r",
        string ")"])
      (var "e"),
    _Term_lambda>>: "l" ~>
      parenthesizeIfNeeded precMin (var "ctx") $ lambda @@ var "aliases" @@ var "l",
    _Term_let>>: "l" ~>
      parenthesizeIfNeeded precMin (var "ctx") $ letAliased @@ var "aliases" @@ var "l",
    _Term_list>>: "els" ~>
      "termStrs" <~ Lists.map (termAliased @@ var "aliases") (var "els") $
      Strings.concat $ list [
        string "[",
        Strings.join (string ", ") (var "termStrs"),
        string "]"],
    _Term_literal>>: "lit" ~> literal @@ var "lit",
    _Term_map>>: "m" ~>
      "entry" <~ ("p" ~> Strings.concat $ list [
        termAliased @@ var "aliases" @@ (Pairs.first $ var "p"),
        string "=",
        termAliased @@ var "aliases" @@ (Pairs.second $ var "p")]) $
      Strings.concat $ list [
        string "{",
        Strings.join (string ", ") $ Lists.map (var "entry") $
          Lists.sortBy (reify Pairs.first) $ Maps.toList (var "m" :: TypedTerm (M.Map Term Term)),
        string "}"],
    _Term_optional>>: "mt" ~> Optionals.match (var "mt") (string "none") ("t" ~> Strings.concat $ list [
        string "given(",
        termAliased @@ var "aliases" @@ var "t",
        string ")"]),
    _Term_pair>>: "p" ~> Strings.concat $ list [
      string "(",
      termAliased @@ var "aliases" @@ (Pairs.first $ var "p"),
      string ", ",
      termAliased @@ var "aliases" @@ (Pairs.second $ var "p"),
      string ")"],
    _Term_project>>: "proj" ~> projection @@ var "aliases" @@ var "proj",
    _Term_record>>: "rec" ~>
      -- #809: nominal construction sugar. A record always prints as 'Name{...}' (the record-style
      -- construct), with unit-valued fields abbreviated bareword via 'sugarFields'. It reparses as a
      -- record (and, for a one-field record over a union type, is corrected to an injection by the
      -- packaging pass -- but a genuine record is never over a union, so it round-trips as a record).
      "tname" <~ printNameAliased @@ var "aliases" @@ (Core.recordTypeName $ var "rec") $
      "flds" <~ Core.recordFields (var "rec") $
      Strings.concat $ list [
        var "tname",
        sugarFields @@ var "aliases" @@ var "flds"],
    _Term_set>>: "s" ~>
      Strings.concat $ list [
        string "{",
        Strings.join (string ", ") (Lists.map (termAliased @@ var "aliases") $ Lists.sort $ Sets.toList $ var "s"),
        string "}"],
    _Term_typeLambda>>: "ta" ~>
      "param" <~ printBinderName @@ (Core.typeLambdaParameter $ var "ta") $
      "body" <~ Core.typeLambdaBody (var "ta") $
      parenthesizeIfNeeded precMin (var "ctx") $
        Strings.concat $ list [
          string "Λ",
          var "param",
          string ".",
          termAliased @@ var "aliases" @@ var "body"],
    _Term_typeApplication>>: "tt" ~>
      "t2" <~ Core.typeApplicationTermBody (var "tt") $
      "typ" <~ Core.typeApplicationTermType (var "tt") $
      "bodyStr" <~ termAtPrec @@ var "aliases" @@ int32 precPostfix @@ var "t2" $
      parenthesizeIfNeeded precPostfix (var "ctx") $
        Strings.concat $ list [
          var "bodyStr",
          string "⟨",
          typeAliased @@ var "aliases" @@ var "typ",
          string "⟩"],
    _Term_inject>>: "inj" ~> injection @@ var "aliases" @@ var "inj",
    _Term_unit>>: constant $ string "unit",
    -- #809: unwrap elimination sugar 'Name[]' (empty brackets = eliminate the wrapper's anonymous
    -- field). Reparses directly to Term.unwrap -- no graph correction needed.
    _Term_unwrap>>: "tname" ~> Strings.concat $ list [
      printNameAliased @@ var "aliases" @@ var "tname",
      string "[]"],
    _Term_variable>>: "name" ~> printNameAliased @@ var "aliases" @@ var "name",
    _Term_wrap>>: "wt" ~>
      -- #809: wrapper construction sugar 'Name{term}'. Safe ONLY when the wrapped body does NOT
      -- reparse as a record field -- i.e. not a bare variable (which would print as a lone name and
      -- reparse as a one-bare-field record). A variable body therefore keeps the keyword form
      -- 'wrap(Name){x}', which round-trips unambiguously. All other bodies (literals, lists,
      -- applications, nested constructs, ...) are not field-shaped, so they sugar safely.
      "tname" <~ printNameAliased @@ var "aliases" @@ (Core.wrappedTermTypeName $ var "wt") $
      "term1" <~ Core.wrappedTermBody (var "wt") $
      Logic.ifElse (isVariableTerm @@ var "term1")
        (Strings.concat $ list [
          string "wrap(",
          var "tname",
          string "){",
          termAliased @@ var "aliases" @@ var "term1",
          string "}"])
        (Strings.concat $ list [
          var "tname",
          string "{",
          termAliased @@ var "aliases" @@ var "term1",
          string "}"])]

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
  "showRowType" <~ ("flds" ~>
    "fieldStrs" <~ Lists.map (fieldType @@ var "aliases") (var "flds") $
    Strings.concat $ list [
      string "{",
      Strings.join (string ", ") (var "fieldStrs"),
      string "}"]) $
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
      "v" <~ printBinderName @@ (Core.forallTypeParameter $ var "ft") $
      "body" <~ Core.forallTypeBody (var "ft") $
      parenthesizeIfNeeded precMin (var "ctx") $
        Strings.concat $ list [
          string "forall ",
          var "v",
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
  Strings.concat $ list [
    string "∀",
    Strings.join (string ",") (var "varNames"),
    string ".",
    Logic.ifElse (Lists.isEmpty $ var "tc")
      (string "")
      (Strings.concat $ list [
        string "(",
        Strings.join (string ", ") (var "tc"),
        string ") ⇒ "]),
    typeAliased @@ var "aliases" @@ var "body"]

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
  Strings.concat $ list [
    string "∀",
    Strings.join (string ",") (var "varNames"),
    string ".",
    Logic.ifElse (Lists.isEmpty $ var "tc")
      (string "")
      (Strings.concat $ list [
        string "(",
        Strings.join (string ", ") (var "tc"),
        string ") ⇒ "]),
    var "bodyStr"]
