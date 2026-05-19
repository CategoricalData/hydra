-- | TypeScript code generator in Hydra DSL.
--
-- Initial scope (types only): converts a Hydra module's type definitions to a
-- single TypeScript source file under `dist/typescript/.../<module>.ts`.
--
--   * Hydra records  → `interface` declarations (readonly fields).
--   * Hydra unions   → discriminated-union `type` aliases of the form
--                       `type Foo = { tag: "a"; value: A } | { tag: "b"; value: B }`.
--   * Hydra wraps    → `interface Foo { readonly value: T; readonly _tag: "Foo" }`.
--   * Other shapes   → `type Foo<T...> = <encoded>` aliases.
--
-- Term-level (function) definitions are deferred to a later iteration.

module Hydra.Sources.TypeScript.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Errors                          as Error
import qualified Hydra.Dsl.Packaging                       as Packaging
import qualified Hydra.Dsl.Util                            as Util
import qualified Hydra.Sources.Kernel.Terms.Environment    as Environment
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables      as Variables
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- Additional imports for the TypeScript AST
import qualified Hydra.TypeScript.Syntax as TS
import qualified Hydra.Sources.TypeScript.Syntax as TypeScriptSyntax
import qualified Hydra.Sources.TypeScript.Language as TypeScriptLanguageSource
import qualified Hydra.Sources.TypeScript.Serde as TypeScriptSerdeSource


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.typeScript.coder"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleDependencies =
              [moduleNamespace TypeScriptLanguageSource.module_,
               moduleNamespace TypeScriptSerdeSource.module_,
               Environment.ns, Formatting.ns, Names.ns, Rewriting.ns,
               Sorting.ns, Strip.ns, Variables.ns]
              L.++ (TypeScriptSyntax.ns : KernelTypes.kernelTypesNamespaces),
            moduleDescription = Just "TypeScript code generator: emits TypeScript type declarations from Hydra modules"}
  where
    definitions = [
      toDefinition collectForallParams,
      toDefinition collectImports,
      toDefinition collectTermImports,
      toDefinition encodeLiteral,
      toDefinition filterNonLocalNames,
      toDefinition encodeLiteralType,
      toDefinition encodeTerm,
      toDefinition encodeTermDefinition,
      toDefinition encodeLazyCall,
      toDefinition flattenApplication,
      toDefinition termHeadVariable,
      toDefinition encodeType,
      toDefinition encodeTypeDefinition,
      toDefinition importsToText,
      toDefinition moduleToTypeScript,
      toDefinition printInterfaceDeclaration,
      toDefinition printLiteral,
      toDefinition printModuleItem,
      toDefinition printPropertySignature,
      toDefinition printTypeAliasDeclaration,
      toDefinition printTypeExpression,
      toDefinition printTypeParameter,
      toDefinition printTypeParameterList,
      toDefinition sortTermDefsTopologically,
      toDefinition stripForalls,
      toDefinition tsEscapeString,
      toDefinition tsIdent,
      toDefinition tsNamedType,
      toDefinition tsParam,
      toDefinition tsParamApp1,
      toDefinition tsParamApp2,
      toDefinition tsPropSig,
      toDefinition tsReadonlyMap,
      toDefinition tsReadonlySet,
      toDefinition tsTuple]

-- =============================================================================
-- Constructors for TypeScript AST fragments
-- =============================================================================

-- | Wrap a String into a TypeScript Identifier.
tsIdent :: TTermDefinition (String -> TS.Identifier)
tsIdent = def "tsIdent" $
  lambda "s" $ wrap TS._Identifier (var "s")

-- | Escape a Hydra string for embedding as a double-quoted TypeScript string
-- literal. Handles backslash, double-quote, newline, carriage return, tab,
-- backspace, and formfeed; other characters pass through verbatim (which
-- keeps multibyte Unicode intact).
tsEscapeString :: TTermDefinition (String -> String)
tsEscapeString = def "tsEscapeString" $
  lambda "s" $
    "escapeChar" <~ ("c" ~>
      Logic.ifElse (Equality.equal (var "c") (int32 34))    (string "\\\"") $
      Logic.ifElse (Equality.equal (var "c") (int32 92))    (string "\\\\") $
      Logic.ifElse (Equality.equal (var "c") (int32 10))    (string "\\n")   $
      Logic.ifElse (Equality.equal (var "c") (int32 13))    (string "\\r")   $
      Logic.ifElse (Equality.equal (var "c") (int32 9))     (string "\\t")   $
      Logic.ifElse (Equality.equal (var "c") (int32 8))     (string "\\b")   $
      Logic.ifElse (Equality.equal (var "c") (int32 12))    (string "\\f")   $
        Strings.fromList $ Lists.pure (var "c")) $
    Strings.cat $ list [
      string "\"",
      Strings.cat $ Lists.map (var "escapeChar") (Strings.toList (var "s")),
      string "\""]

-- | A bare named type reference like `Foo`.
tsNamedType :: TTermDefinition (String -> TS.TypeExpression)
tsNamedType = def "tsNamedType" $
  lambda "n" $
    inject TS._TypeExpression TS._TypeExpression_identifier (tsIdent @@ var "n")

-- | A parameterized type `Name<T>`.
tsParamApp1 :: TTermDefinition (String -> TS.TypeExpression -> TS.TypeExpression)
tsParamApp1 = def "tsParamApp1" $
  lambda "n" $ lambda "arg" $
    inject TS._TypeExpression TS._TypeExpression_parameterized $
      record TS._ParameterizedTypeExpression [
        TS._ParameterizedTypeExpression_base>>: tsNamedType @@ var "n",
        TS._ParameterizedTypeExpression_arguments>>: list [var "arg"]]

-- | A parameterized type `Name<K, V>`.
tsParamApp2 :: TTermDefinition (String -> TS.TypeExpression -> TS.TypeExpression -> TS.TypeExpression)
tsParamApp2 = def "tsParamApp2" $
  lambda "n" $ lambda "a" $ lambda "b" $
    inject TS._TypeExpression TS._TypeExpression_parameterized $
      record TS._ParameterizedTypeExpression [
        TS._ParameterizedTypeExpression_base>>: tsNamedType @@ var "n",
        TS._ParameterizedTypeExpression_arguments>>: list [var "a", var "b"]]

-- | A tuple type `[A, B, ...]`.
tsTuple :: TTermDefinition ([TS.TypeExpression] -> TS.TypeExpression)
tsTuple = def "tsTuple" $
  lambda "ts" $
    inject TS._TypeExpression TS._TypeExpression_tuple (var "ts")

-- | A `ReadonlyMap<K, V>`.
tsReadonlyMap :: TTermDefinition (TS.TypeExpression -> TS.TypeExpression -> TS.TypeExpression)
tsReadonlyMap = def "tsReadonlyMap" $
  lambda "k" $ lambda "v" $ tsParamApp2 @@ string "ReadonlyMap" @@ var "k" @@ var "v"

-- | A `ReadonlySet<T>`.
tsReadonlySet :: TTermDefinition (TS.TypeExpression -> TS.TypeExpression)
tsReadonlySet = def "tsReadonlySet" $
  lambda "t" $ tsParamApp1 @@ string "ReadonlySet" @@ var "t"

-- | A generic type parameter with no constraint.
tsParam :: TTermDefinition (String -> TS.TypeParameter)
tsParam = def "tsParam" $
  lambda "n" $
    record TS._TypeParameter [
      TS._TypeParameter_name>>: tsIdent @@ var "n",
      TS._TypeParameter_constraint>>: nothing,
      TS._TypeParameter_default>>: nothing]

-- | A readonly property signature. The name is sanitized for TS reserved
-- words (so a Hydra field named `case` becomes `case_`).
tsPropSig :: TTermDefinition (String -> Bool -> TS.TypeExpression -> TS.PropertySignature)
tsPropSig = def "tsPropSig" $
  lambda "name" $ lambda "optional" $ lambda "typ" $
    "safe" <~ (Formatting.sanitizeWithUnderscores
      @@ TypeScriptLanguageSource.typeScriptReservedWords
      @@ var "name") $
    record TS._PropertySignature [
      TS._PropertySignature_name>>: tsIdent @@ var "safe",
      TS._PropertySignature_type>>: var "typ",
      TS._PropertySignature_optional>>: var "optional",
      TS._PropertySignature_readonly>>: boolean True]

-- =============================================================================
-- Literal-type encoding
-- =============================================================================

-- | Map a Hydra LiteralType to a TypeScript TypeExpression.
encodeLiteralType :: TTermDefinition (LiteralType -> TS.TypeExpression)
encodeLiteralType = def "encodeLiteralType" $
  lambda "lt" $ cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $
      tsParamApp1 @@ string "ReadonlyArray" @@ (tsNamedType @@ string "number"),
    _LiteralType_boolean>>: constant $
      tsNamedType @@ string "boolean",
    _LiteralType_float>>: lambda "ft" $
      cases _FloatType (var "ft") Nothing [
        _FloatType_float32>>: constant $ tsNamedType @@ string "number",
        _FloatType_float64>>: constant $ tsNamedType @@ string "number"],
    _LiteralType_integer>>: lambda "it" $
      cases _IntegerType (var "it") Nothing [
        _IntegerType_bigint>>: constant $ tsNamedType @@ string "bigint",
        _IntegerType_int8>>:   constant $ tsNamedType @@ string "number",
        _IntegerType_int16>>:  constant $ tsNamedType @@ string "number",
        _IntegerType_int32>>:  constant $ tsNamedType @@ string "number",
        _IntegerType_int64>>:  constant $ tsNamedType @@ string "bigint",
        _IntegerType_uint8>>:  constant $ tsNamedType @@ string "number",
        _IntegerType_uint16>>: constant $ tsNamedType @@ string "number",
        _IntegerType_uint32>>: constant $ tsNamedType @@ string "number",
        _IntegerType_uint64>>: constant $ tsNamedType @@ string "bigint"],
    _LiteralType_string>>: constant $
      tsNamedType @@ string "string"]

-- =============================================================================
-- Type encoding
-- =============================================================================

-- | Map a Hydra Type to a TypeScript TypeExpression.
--
-- Anonymous records and unions are rejected — they must appear as the body of
-- a named TypeDefinition so the coder can emit an interface or discriminated-
-- union alias.
encodeType :: TTermDefinition (Context -> Graph -> Type -> Either Error TS.TypeExpression)
encodeType = def "encodeType" $
  "cx" ~> "g" ~> lambda "t" $
    "typ" <~ (Strip.deannotateType @@ var "t") $
    cases _Type (var "typ") Nothing [
      _Type_annotated>>: lambda "at" $
        encodeType @@ var "cx" @@ var "g" @@ Core.annotatedTypeBody (var "at"),
      -- Type application: unwind nested ApplicationType chains so a Hydra
      -- App(App(F, A), B) becomes a TS `F<A, B>`. The head must encode to
      -- an identifier (otherwise we fall back to the head alone — a known
      -- limitation when the head is e.g. another parameterized application).
      _Type_application>>: lambda "at" $
        "fnTyp" <~ Core.applicationTypeFunction (var "at") $
        "argTyp" <~ Core.applicationTypeArgument (var "at") $
        "encFn" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "fnTyp") $
        "encArg" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "argTyp") $
          cases TS._TypeExpression (var "encFn") (Just (right (var "encFn"))) [
            -- F<arg>: head was a bare identifier
            TS._TypeExpression_identifier>>: lambda "_id" $
              right (inject TS._TypeExpression TS._TypeExpression_parameterized $
                record TS._ParameterizedTypeExpression [
                  TS._ParameterizedTypeExpression_base>>: var "encFn",
                  TS._ParameterizedTypeExpression_arguments>>: list [var "encArg"]]),
            -- F<a><b> -- chain: append to existing argument list
            TS._TypeExpression_parameterized>>: lambda "p" $
              right (inject TS._TypeExpression TS._TypeExpression_parameterized $
                record TS._ParameterizedTypeExpression [
                  TS._ParameterizedTypeExpression_base>>:
                    project TS._ParameterizedTypeExpression TS._ParameterizedTypeExpression_base @@ var "p",
                  TS._ParameterizedTypeExpression_arguments>>:
                    Lists.concat2
                      (project TS._ParameterizedTypeExpression TS._ParameterizedTypeExpression_arguments @@ var "p")
                      (list [var "encArg"])])],
      _Type_forall>>: lambda "fa" $
        encodeType @@ var "cx" @@ var "g" @@ Core.forallTypeBody (var "fa"),
      _Type_unit>>: constant $
        right (inject TS._TypeExpression TS._TypeExpression_void unit),
      _Type_void>>: constant $
        right (inject TS._TypeExpression TS._TypeExpression_never unit),
      _Type_literal>>: lambda "lt" $
        right (encodeLiteralType @@ var "lt"),
      _Type_list>>: lambda "inner" $
        Eithers.map (lambda "enc" $ tsParamApp1 @@ string "ReadonlyArray" @@ var "enc")
          (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
      _Type_set>>: lambda "inner" $
        Eithers.map (asTerm tsReadonlySet)
          (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
      _Type_map>>: lambda "mt" $
        "kt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeKeys (var "mt")) $
        "vt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeValues (var "mt")) $
          right (tsReadonlyMap @@ var "kt" @@ var "vt"),
      _Type_maybe>>: lambda "inner" $
        Eithers.map (lambda "enc" $
          inject TS._TypeExpression TS._TypeExpression_optional (var "enc"))
          (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
      -- Either is emitted inline as `{ tag: "left", value: L } | { tag: "right", value: R }`
      -- to avoid needing a runtime `Either` import in every generated module.
      _Type_either>>: lambda "et" $
        "lt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeLeft (var "et")) $
        "rt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeRight (var "et")) $
          "leftArm" <~ (inject TS._TypeExpression TS._TypeExpression_object $ list [
            tsPropSig @@ string "tag" @@ boolean False
              @@ (inject TS._TypeExpression TS._TypeExpression_literal $
                    inject TS._Literal TS._Literal_string $
                    record TS._StringLiteral [
                      TS._StringLiteral_value>>: string "left",
                      TS._StringLiteral_singleQuote>>: boolean False]),
            tsPropSig @@ string "value" @@ boolean False @@ var "lt"]) $
          "rightArm" <~ (inject TS._TypeExpression TS._TypeExpression_object $ list [
            tsPropSig @@ string "tag" @@ boolean False
              @@ (inject TS._TypeExpression TS._TypeExpression_literal $
                    inject TS._Literal TS._Literal_string $
                    record TS._StringLiteral [
                      TS._StringLiteral_value>>: string "right",
                      TS._StringLiteral_singleQuote>>: boolean False]),
            tsPropSig @@ string "value" @@ boolean False @@ var "rt"]) $
          right (inject TS._TypeExpression TS._TypeExpression_union $ list [
            var "leftArm", var "rightArm"]),
      _Type_pair>>: lambda "pt" $
        "ft" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeFirst (var "pt")) $
        "st" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeSecond (var "pt")) $
          right (tsTuple @@ list [var "ft", var "st"]),
      _Type_function>>: lambda "ft" $
        "dom" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeDomain (var "ft")) $
        "cod" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeCodomain (var "ft")) $
          right (inject TS._TypeExpression TS._TypeExpression_function $
            record TS._FunctionTypeExpression [
              TS._FunctionTypeExpression_typeParameters>>: list ([] :: [TTerm TS.TypeParameter]),
              TS._FunctionTypeExpression_parameters>>: list [var "dom"],
              TS._FunctionTypeExpression_returnType>>: var "cod"]),
      _Type_variable>>: lambda "name" $
        right (tsNamedType @@ (Formatting.capitalize @@ (Names.localNameOf @@ var "name"))),
      _Type_wrap>>: lambda "wt" $
        encodeType @@ var "cx" @@ var "g" @@ var "wt",
      -- Anonymous records: emit as inline object types `{ readonly f: T, ... }`.
      _Type_record>>: lambda "fts" $
        "members" <<~ (Eithers.mapList
          (lambda "ft" $
            "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
            "ftyp"  <~ Core.fieldTypeType (var "ft") $
            "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftyp") $
              right (tsPropSig @@ var "fname" @@ boolean False @@ var "sftyp"))
          (var "fts")) $
          right (inject TS._TypeExpression TS._TypeExpression_object (var "members")),
      -- Anonymous unions: emit as a TypeScript union of discriminated arms.
      _Type_union>>: lambda "fts" $
        "arms" <<~ (Eithers.mapList
          (lambda "ft" $
            "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
            "ftyp"  <~ Core.fieldTypeType (var "ft") $
            "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftyp") $
              right (inject TS._TypeExpression TS._TypeExpression_object $ list [
                tsPropSig @@ string "tag" @@ boolean False
                  @@ (inject TS._TypeExpression TS._TypeExpression_literal $
                        inject TS._Literal TS._Literal_string $
                        record TS._StringLiteral [
                          TS._StringLiteral_value>>: var "fname",
                          TS._StringLiteral_singleQuote>>: boolean False]),
                tsPropSig @@ string "value" @@ boolean False @@ var "sftyp"]))
          (var "fts")) $
          right (inject TS._TypeExpression TS._TypeExpression_union (var "arms"))]

-- =============================================================================
-- Cross-module import collection
-- =============================================================================

-- | Find every qualified name referenced by a Type that lives in a different
-- namespace than the current module's. Used to compute the imports needed
-- at the top of the emitted .ts file. Uses Variables.freeVariablesInType
-- to walk the whole tree (it handles foralls correctly by excluding bound
-- parameters).
collectImports :: TTermDefinition (Namespace -> Type -> S.Set Name)
collectImports = def "collectImports" $
  lambda "currentNs" $ lambda "t" $
    "vars" <~ (Variables.freeVariablesInType @@ var "t") $
    filterNonLocalNames @@ var "currentNs" @@ var "vars"

-- | Same as `collectImports` but walks a Term, gathering free term-level
-- variables that resolve to a different module (i.e. references to other
-- generated `export const` definitions or to runtime lib primitives).
collectTermImports :: TTermDefinition (Namespace -> Term -> S.Set Name)
collectTermImports = def "collectTermImports" $
  lambda "currentNs" $ lambda "t" $
    "vars" <~ (Variables.freeVariablesInTerm @@ var "t") $
    filterNonLocalNames @@ var "currentNs" @@ var "vars"

-- | Shared filter: keep only Names that have a Namespace distinct from
-- the current module's. Names without a Namespace (bare lambda params)
-- and Names in the current namespace are dropped.
filterNonLocalNames :: TTermDefinition (Namespace -> S.Set Name -> S.Set Name)
filterNonLocalNames = def "filterNonLocalNames" $
  lambda "currentNs" $ lambda "names" $
    Sets.fromList $ Maybes.cat $ Lists.map
      (lambda "n" $
        Maybes.cases (Names.namespaceOf @@ var "n")
          nothing
          (lambda "nameNs" $
            Logic.ifElse
              (Equality.equal
                (unwrap _Namespace @@ var "currentNs")
                (unwrap _Namespace @@ var "nameNs"))
              nothing
              (just $ var "n")))
      (Sets.toList (var "names"))

-- | Render a Set of qualified Names as TypeScript import statements grouped
-- by source module. The current module's own namespace is dropped from the
-- output so the file does not import itself.
-- | Render imports for a Set of cross-module Names.
--
-- `kind` selects the import flavor:
--   "type"  → emits `import type { ... }` with PascalCased names (for type refs).
--   "value" → emits `import { ... }` with names verbatim (for term-level refs).
importsToText :: TTermDefinition (String -> Namespace -> S.Set Name -> String)
importsToText = def "importsToText" $
  lambda "kind" $ lambda "currentNs" $ lambda "names" $
    -- Pair each Name with its optional Namespace; drop ones with no namespace
    -- or whose namespace matches the current module.
    "pairs" <~ (Maybes.cat $ Lists.map
      (lambda "n" $
        Maybes.cases (Names.namespaceOf @@ var "n")
          nothing
          (lambda "ns" $
            Logic.ifElse
              (Equality.equal
                (unwrap _Namespace @@ var "currentNs")
                (unwrap _Namespace @@ var "ns"))
              nothing
              (just $ pair (var "ns") (var "n"))))
      (Sets.toList (var "names"))) $
    -- Apply the kind's case convention. `kind == "type"` → capitalize.
    -- Value-kind imports also get reserved-word sanitization so e.g. `null`
    -- becomes `null_`, matching the runtime's exported binding name.
    "transformLocal" <~ (lambda "s" $
      Logic.ifElse (Equality.equal (var "kind") (string "type"))
        (Formatting.capitalize @@ var "s")
        (Formatting.sanitizeWithUnderscores
          @@ TypeScriptLanguageSource.typeScriptReservedWords @@ var "s")) $
    "importKeyword" <~ Logic.ifElse (Equality.equal (var "kind") (string "type"))
      (string "import type")
      (string "import") $
    -- Group by namespace via fold into a Map<Namespace, [String]>.
    "grouped" <~ (Lists.foldl
      (lambda "acc" $ lambda "p" $
        "ns" <~ Pairs.first (var "p") $
        "n" <~ Pairs.second (var "p") $
        "local" <~ (var "transformLocal" @@ (Names.localNameOf @@ var "n")) $
        "existing" <~ (Maybes.fromMaybe (list ([] :: [TTerm String]))
          (Maps.lookup (var "ns") (var "acc"))) $
        Maps.insert (var "ns") (Lists.cons (var "local") (var "existing")) (var "acc"))
      (Maps.empty :: TTerm (M.Map Namespace [String]))
      (var "pairs")) $
    -- Path computation: every Hydra namespace has the form `hydra.<a>.<b>...`.
    -- The file for ns `hydra.a.b.c` lives at `hydra/a/b/c.ts` — depth n-1
    -- (where n is the number of segments after `hydra`). So from the
    -- current file's directory, we walk up (n_current - 1) times then descend
    -- through the target's path segments.
    --
    -- Test modules (those whose namespace starts with `hydra.test.`) live in
    -- a sibling tree at `<dist>/src/test/typescript/hydra/test/...`, while
    -- main modules live at `<dist>/src/main/typescript/hydra/...`. A cross-
    -- tree import from a test module to a main module needs to escape from
    -- src/test/typescript into src/main/typescript, hence the extra
    -- `../../main/typescript/` segment appended to upPrefix.
    "currentSegs" <~ (Lists.drop (int32 1)
      (Strings.splitOn (string ".") (unwrap _Namespace @@ var "currentNs"))) $
    "currentDepth" <~ (Lists.length (var "currentSegs")) $
    "currentIsTest" <~ Logic.and
      (Logic.not (Lists.null (var "currentSegs")))
      (Equality.equal (Maybes.fromMaybe (string "") (Lists.maybeHead (var "currentSegs"))) (string "test")) $
    "baseUpPrefix" <~ Logic.ifElse
      (Equality.equal (var "currentDepth") (int32 1))
      (string "./")
      (Strings.cat $ Lists.replicate (Math.sub (var "currentDepth") (int32 1)) (string "../")) $
    "lines" <~ (Lists.map
      (lambda "entry" $
        "ns" <~ Pairs.first (var "entry") $
        "locals" <~ Pairs.second (var "entry") $
        "targetSegs" <~ (Lists.drop (int32 1)
          (Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns"))) $
        "targetIsTest" <~ Logic.and
          (Logic.not (Lists.null (var "targetSegs")))
          (Equality.equal (Maybes.fromMaybe (string "") (Lists.maybeHead (var "targetSegs"))) (string "test")) $
        "targetPath" <~ Strings.intercalate (string "/") (var "targetSegs") $
        -- Cross-tree paths: test → main needs extra "../../main/typescript/".
        -- main → test would need the inverse, but currently no main module
        -- imports test, so that branch is omitted.
        -- When current is test and target is main, the path becomes:
        --   baseUpPrefix + "../../../main/typescript/hydra/"
        -- baseUpPrefix gets us to <test-root>/hydra/. From there:
        --   ../   → <test-root>/typescript/ (out of hydra/)
        --   ../   → <test-root>/test/       (out of typescript/)
        --   ../   → <test-root>/src/        (out of test/)
        -- then "main/typescript/hydra/" enters the main tree.
        "upPrefix" <~ Logic.ifElse
          (Logic.and (var "currentIsTest") (Logic.not (var "targetIsTest")))
          (Strings.cat2 (var "baseUpPrefix") (string "../../../main/typescript/hydra/"))
          (var "baseUpPrefix") $
        -- For type imports keep the named form (TS supports same-named
        -- types imported from multiple sources via separate aliases, but
        -- we don't expect type clashes today). For value imports use
        -- namespace-style `import * as <alias>` to dodge collisions
        -- between like-named helpers (e.g. `map` from lists/maybes/eithers).
        -- Build a unique alias by joining all post-`hydra.` segments with
        -- underscores. So `hydra.test.checking.all` → `test_checking_all`,
        -- `hydra.test.hoisting.all` → `test_hoisting_all`, avoiding the
        -- collisions that bare last-segment aliasing would produce.
        -- Prefix module aliases with `_` to avoid shadowing by local Hydra
        -- identifiers. E.g. `hydra.arity` would alias as `arity`, which a
        -- local `const arity = ...` then shadows (TDZ in JS even though
        -- legal in Haskell). Prefix with `_` to keep aliases out of the
        -- local-name space.
        "moduleAlias" <~ Strings.cat2 (string "$mod_")
          (Strings.intercalate (string "_") (var "targetSegs")) $
        Logic.ifElse (Equality.equal (var "kind") (string "type"))
          (Strings.cat (list [
            var "importKeyword",
            string " { ",
            Strings.intercalate (string ", ") (var "locals"),
            string " } from \"",
            var "upPrefix",
            var "targetPath",
            string ".js\";\n"]))
          (Strings.cat (list [
            string "import * as ",
            -- The `$mod_` prefix in moduleAlias keeps the alias out of
            -- the local-name space and reserves any clashes for the
            -- generator. No further sanitization needed — `$` is a
            -- valid JS identifier character and Hydra source can't
            -- produce a `$` in a name segment.
            var "moduleAlias",
            string " from \"",
            var "upPrefix",
            var "targetPath",
            string ".js\";\n"])))
      (Maps.toList (var "grouped"))) $
    Strings.cat (var "lines")

-- =============================================================================
-- Forall extraction helpers
-- =============================================================================

-- | Strip leading forall wrappers from a type, returning the unwrapped body.
-- Annotations on intervening layers are also stripped.
-- =============================================================================
-- Application flattening + laziness wrapping
-- =============================================================================
--
-- Hydra's `ifElse` and several `maybes`/`eithers` primitives have lazy
-- semantics in the Haskell source: the unselected branch is never
-- evaluated. JavaScript is strict, so we wrap the lazy positions in
-- nullary arrow functions `() => expr` and the runtime primitive
-- invokes them on demand. See docs/recipes/new-implementation.md
-- (section "Lazy evaluation and thunking") for the full rationale.
--
-- The lazy-arg positions, mirroring Java's `wrapLazyArguments`:
--   hydra.lib.logic.ifElse        — positions 1, 2 (then, else)
--   hydra.lib.maybes.cases        — position 1 (nothing case)
--   hydra.lib.maybes.maybe        — position 0 (default)
--   hydra.lib.maybes.fromMaybe    — position 0 (default)
--   hydra.lib.eithers.fromLeft    — position 0 (default)
--   hydra.lib.eithers.fromRight   — position 0 (default)
--   hydra.lib.maps.findWithDefault — position 0 (default)
--
-- The TS-side primitive accepts either a value or a thunk and calls the
-- thunk when needed (mirroring Python's `callable()` check).

-- | Walk an application spine, returning (innermost head term, args in
-- application order). For `App (App (App f a) b) c)` returns `(f, [a,b,c])`.
flattenApplication :: TTermDefinition (Term -> (Term, [Term]))
flattenApplication = def "flattenApplication" $
  lambda "t" $
    "dt" <~ (Strip.deannotateTerm @@ var "t") $
    cases _Term (var "dt")
      (Just $ pair (var "t") (list ([] :: [TTerm Term]))) [
      _Term_application>>: lambda "app" $
        "inner" <~ (flattenApplication @@ Core.applicationFunction (var "app")) $
        "head_" <~ Pairs.first (var "inner") $
        "prevArgs" <~ Pairs.second (var "inner") $
        pair (var "head_")
             (Lists.concat2 (var "prevArgs")
                            (Lists.singleton (Core.applicationArgument (var "app"))))]

-- | Emit a fully-applied primitive call with selected arguments wrapped
-- in `() => expr` thunks. The `lazyFlags` list parallels `args`:
-- True positions are wrapped, False positions are emitted normally.
-- Forward-declared as a record-of-strings so encodeTerm can use it.
encodeLazyCall :: TTermDefinition (Context -> Graph -> Namespace -> Term -> [Term] -> [Bool] -> String)
encodeLazyCall = def "encodeLazyCall" $
  lambda "cx" $ lambda "g" $ lambda "currentNs" $ lambda "headTerm" $ lambda "args" $ lambda "lazyFlags" $
    "headExpr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm") $
    "paired" <~ Lists.zip (var "args") (var "lazyFlags") $
    "renderArg" <~ (lambda "p" $
      "argTerm" <~ Pairs.first (var "p") $
      "isLazy" <~ Pairs.second (var "p") $
      "expr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "argTerm") $
      Logic.ifElse (var "isLazy")
        (Strings.cat (list [string "(() => (", var "expr", string "))"]))
        (Strings.cat (list [string "(", var "expr", string ")"]))) $
    "argParts" <~ Lists.map (var "renderArg") (var "paired") $
    Strings.cat (list [
      string "(", var "headExpr", string ")",
      Strings.cat (var "argParts")])

-- | If a term reduces (through annotations and type applications) to
-- `Term_variable name`, return `Just name`; else `Nothing`. Polymorphic
-- primitives are typically wrapped in one or more `Term_typeApplication`
-- layers (e.g. `ifElse @TypeArg`), which the runtime erases — so we
-- erase them here too when looking for the head.
termHeadVariable :: TTermDefinition (Term -> Maybe Name)
termHeadVariable = def "termHeadVariable" $
  lambda "t" $
    "dt" <~ (Strip.deannotateTerm @@ var "t") $
    cases _Term (var "dt")
      (Just (nothing :: TTerm (Maybe Name))) [
      _Term_variable>>: lambda "n" $ just (var "n"),
      _Term_typeApplication>>: lambda "ta" $
        termHeadVariable @@ Core.typeApplicationTermBody (var "ta")]

stripForalls :: TTermDefinition (Type -> Type)
stripForalls = def "stripForalls" $
  lambda "t" $
    "dt" <~ (Strip.deannotateType @@ var "t") $
    cases _Type (var "dt") (Just $ var "dt") [
      _Type_forall>>: lambda "fa" $
        stripForalls @@ Core.forallTypeBody (var "fa")]

-- | Collect the bound parameter names from a chain of nested foralls, in
-- outer-to-inner order. Stops at the first non-forall type.
collectForallParams :: TTermDefinition (Type -> [Name])
collectForallParams = def "collectForallParams" $
  lambda "t" $
    "dt" <~ (Strip.deannotateType @@ var "t") $
    cases _Type (var "dt") (Just $ list ([] :: [TTerm Name])) [
      _Type_forall>>: lambda "fa" $
        Lists.cons
          (Core.forallTypeParameter (var "fa"))
          (collectForallParams @@ Core.forallTypeBody (var "fa"))]

-- =============================================================================
-- Type-definition encoding
-- =============================================================================

-- | Encode a Hydra type definition as either an InterfaceDeclaration (records),
-- a TypeAliasDeclaration of a discriminated union (unions), or a plain type
-- alias (everything else).
encodeTypeDefinition :: TTermDefinition (Context -> Graph -> TypeDefinition -> Either Error TS.ModuleItem)
encodeTypeDefinition = def "encodeTypeDefinition" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Packaging.typeDefinitionName (var "tdef") $
    "typScheme" <~ Packaging.typeDefinitionTypeScheme (var "tdef") $
    "rawTyp" <~ Core.typeSchemeBody (var "typScheme") $
    "lname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ var "name")) $
    -- Walk leading foralls to harvest type parameters; the body is what we
    -- actually encode. This matches how Hydra represents generic types — the
    -- outer foralls bind the variables that appear inside.
    "forallParams" <~ (collectForallParams @@ var "rawTyp") $
    "typ" <~ (stripForalls @@ var "rawTyp") $
    "typeParams" <~ (Lists.map
      (lambda "v" $ tsParam @@ (Formatting.capitalize @@ Core.unName (var "v")))
      (var "forallParams")) $
    "dtyp" <~ (Strip.deannotateType @@ var "typ") $
    cases _Type (var "dtyp") (Just $
        -- Fallback: a plain type alias `type Foo<T...> = <encoded>;`
        "styp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
          right (inject TS._ModuleItem TS._ModuleItem_typeAlias $
            record TS._TypeAliasDeclaration [
              TS._TypeAliasDeclaration_name>>: tsIdent @@ var "lname",
              TS._TypeAliasDeclaration_typeParameters>>: var "typeParams",
              TS._TypeAliasDeclaration_type>>: var "styp"]))
      [_Type_record>>: lambda "fts" $
        "members" <<~ (Eithers.mapList
          (lambda "ft" $
            "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
            "ftyp"  <~ Core.fieldTypeType (var "ft") $
            "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftyp") $
              right (tsPropSig @@ var "fname" @@ boolean False @@ var "sftyp"))
          (var "fts")) $
          right (inject TS._ModuleItem TS._ModuleItem_interface $
            record TS._InterfaceDeclaration [
              TS._InterfaceDeclaration_name>>: tsIdent @@ var "lname",
              TS._InterfaceDeclaration_typeParameters>>: var "typeParams",
              TS._InterfaceDeclaration_extends>>: list ([] :: [TTerm TS.TypeExpression]),
              TS._InterfaceDeclaration_members>>: var "members"]),
      _Type_union>>: lambda "fts" $
        -- Each union field becomes one discriminated-union arm:
        --   `{ readonly tag: "<fname>"; readonly value: <encoded fty> }`
        "arms" <<~ (Eithers.mapList
          (lambda "ft" $
            "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
            "ftyp"  <~ Core.fieldTypeType (var "ft") $
            "dtyp2" <~ (Strip.deannotateType @@ var "ftyp") $
            -- For unit-shaped variants, emit `{ readonly tag: "fname" }`
            -- (no `value` field).
            cases _Type (var "dtyp2") (Just $
              "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftyp") $
                right (inject TS._TypeExpression TS._TypeExpression_object $ list [
                  tsPropSig @@ string "tag" @@ boolean False
                    @@ (inject TS._TypeExpression TS._TypeExpression_literal $
                          inject TS._Literal TS._Literal_string $
                          record TS._StringLiteral [
                            TS._StringLiteral_value>>: var "fname",
                            TS._StringLiteral_singleQuote>>: boolean False]),
                  tsPropSig @@ string "value" @@ boolean False @@ var "sftyp"]))
              [_Type_unit>>: constant $
                right (inject TS._TypeExpression TS._TypeExpression_object $ list [
                  tsPropSig @@ string "tag" @@ boolean False
                    @@ (inject TS._TypeExpression TS._TypeExpression_literal $
                          inject TS._Literal TS._Literal_string $
                          record TS._StringLiteral [
                            TS._StringLiteral_value>>: var "fname",
                            TS._StringLiteral_singleQuote>>: boolean False])])])
          (var "fts")) $
          right (inject TS._ModuleItem TS._ModuleItem_typeAlias $
            record TS._TypeAliasDeclaration [
              TS._TypeAliasDeclaration_name>>: tsIdent @@ var "lname",
              TS._TypeAliasDeclaration_typeParameters>>: var "typeParams",
              TS._TypeAliasDeclaration_type>>: inject TS._TypeExpression TS._TypeExpression_union (var "arms")]),
      _Type_wrap>>: lambda "wt" $
        -- A wrap type becomes a single-field interface with a `_tag` brand.
        "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "wt") $
          right (inject TS._ModuleItem TS._ModuleItem_interface $
            record TS._InterfaceDeclaration [
              TS._InterfaceDeclaration_name>>: tsIdent @@ var "lname",
              TS._InterfaceDeclaration_typeParameters>>: var "typeParams",
              TS._InterfaceDeclaration_extends>>: list ([] :: [TTerm TS.TypeExpression]),
              TS._InterfaceDeclaration_members>>: list [
                tsPropSig @@ string "value" @@ boolean False @@ var "sftyp",
                tsPropSig @@ string "_tag" @@ boolean False
                  @@ (inject TS._TypeExpression TS._TypeExpression_literal $
                        inject TS._Literal TS._Literal_string $
                        record TS._StringLiteral [
                          TS._StringLiteral_value>>: var "lname",
                          TS._StringLiteral_singleQuote>>: boolean False])]])]

-- =============================================================================
-- Term-level encoding (direct-to-text, minimal subset)
-- =============================================================================
--
-- Encodes Hydra terms directly to TypeScript expression text. Covers the
-- shapes that appear in the DSL helper modules:
--   * literals (string, integer, float, boolean)
--   * variables → identifier
--   * lambdas → arrow functions
--   * applications → function calls
--   * records → object literals
--   * inject → discriminated-union construction `{ tag: "x", value: ... }`
--   * wrap → wrapper invocation (treated like inject for now)
--   * list / set / map / pair → literal forms
--   * optional → undefined or value
--   * let → IIFE with sequential bindings
--   * annotated → strip annotations
-- Unsupported variants (cases, project, unwrap, either, typeApplication,
-- typeLambda) emit a `/* unsupported */` comment so the file at least lints.

-- | Render a Hydra Literal as a TypeScript expression.
encodeLiteral :: TTermDefinition (Literal -> String)
encodeLiteral = def "encodeLiteral" $
  lambda "lit" $ cases _Literal (var "lit") (Just $ string "null") [
    _Literal_binary>>: lambda "b" $
      -- Binary literals emit as a base64-encoded JS string. The runtime
      -- decoder (extractCore.binary) accepts either Uint8Array or
      -- base64-encoded strings, so emitting strings keeps the JSON
      -- payload portable and the source readable.
      tsEscapeString @@ (Literals.binaryToString (var "b")),
    _Literal_boolean>>: lambda "b" $
      Logic.ifElse (var "b") (string "true") (string "false"),
    _Literal_decimal>>: lambda "d" $
      Literals.showDecimal (var "d"),
    _Literal_string>>: lambda "s" $
      tsEscapeString @@ var "s",
    _Literal_integer>>: lambda "iv" $
      cases _IntegerValue (var "iv") (Just $ string "0") [
        _IntegerValue_bigint>>: lambda "n" $
          Strings.cat2 (Literals.showBigint (var "n")) (string "n"),
        _IntegerValue_int8>>:   lambda "n" $ Literals.showInt8   (var "n"),
        _IntegerValue_int16>>:  lambda "n" $ Literals.showInt16  (var "n"),
        _IntegerValue_int32>>:  lambda "n" $ Literals.showInt32  (var "n"),
        _IntegerValue_int64>>:  lambda "n" $
          Strings.cat2 (Literals.showInt64 (var "n")) (string "n"),
        _IntegerValue_uint8>>:  lambda "n" $ Literals.showUint8  (var "n"),
        _IntegerValue_uint16>>: lambda "n" $ Literals.showUint16 (var "n"),
        _IntegerValue_uint32>>: lambda "n" $ Literals.showUint32 (var "n"),
        _IntegerValue_uint64>>: lambda "n" $
          Strings.cat2 (Literals.showUint64 (var "n")) (string "n")],
    _Literal_float>>: lambda "fv" $
      cases _FloatValue (var "fv") (Just $ string "0") [
        _FloatValue_float32>>: lambda "f" $ Literals.showFloat32 (var "f"),
        _FloatValue_float64>>: lambda "f" $ Literals.showFloat64 (var "f")]]

-- | Render a Hydra Term as a TypeScript expression.
--
-- The first argument is the current module's namespace; it's used to decide
-- whether a `Term_variable` reference is local (emit bare name) or external
-- (emit `<alias>.<name>` matching the namespace-style import).
encodeTerm :: TTermDefinition (Context -> Graph -> Namespace -> Term -> String)
encodeTerm = def "encodeTerm" $
  lambda "cx" $ lambda "g" $ lambda "currentNs" $ lambda "term" $ cases _Term (var "term")
    (Just $ string "/* unsupported term */ null")
    [_Term_annotated>>: lambda "at" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.annotatedTermBody (var "at"),
     _Term_literal>>: lambda "lit" $
       encodeLiteral @@ var "lit",
     _Term_variable>>: lambda "n" $
       "local" <~ (Formatting.sanitizeWithUnderscores
         @@ TypeScriptLanguageSource.typeScriptReservedWords
         @@ (Names.localNameOf @@ var "n")) $
       -- Same-namespace (or no namespace = lambda-bound variable): emit
       -- just the local name. Different namespace: emit `<alias>.<local>`
       -- matching the namespace-style import in the file header.
       Maybes.cases (Names.namespaceOf @@ var "n")
         (var "local")
         (lambda "ns" $
           Logic.ifElse
             (Equality.equal
               (unwrap _Namespace @@ var "currentNs")
               (unwrap _Namespace @@ var "ns"))
             (var "local")
             ("nsSegs" <~ (Lists.drop (int32 1)
                (Strings.splitOn (string ".") (unwrap _Namespace @@ var "ns"))) $
              -- Must match the prefix used in importsToText.
              "alias" <~ Strings.cat2 (string "$mod_")
                (Strings.intercalate (string "_") (var "nsSegs")) $
              Strings.cat (list [var "alias", string ".", var "local"]))),
     -- Lambda emission: if the parameter has a declared domain type
     -- (Hydra `Lambda.domain :: Maybe Type`), emit `(p: <encoded type>) =>
     -- (body)` so the generated TS code has a typed parameter. If domain
     -- is absent or encoding the type fails, fall back to `(p) => (body)`
     -- — TS `--noImplicitAny: false` accepts this. Domain types appear
     -- on top-level lambdas after Hydra's eta-expansion + inference
     -- pass; nested lambdas and synthesized helpers often lack them.
     _Term_lambda>>: lambda "lam" $
       "p" <~ (Formatting.sanitizeWithUnderscores @@ TypeScriptLanguageSource.typeScriptReservedWords
         @@ (Names.localNameOf @@ Core.lambdaParameter (var "lam"))) $
       "b" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.lambdaBody (var "lam")) $
       "domMaybe" <~ Core.lambdaDomain (var "lam") $
       "paramText" <~ Maybes.cases (var "domMaybe")
         (var "p")
         (lambda "dom" $
           Eithers.either_
             (lambda "_e" $ var "p")
             (lambda "te" $ Strings.cat (list [
               var "p", string ": ", printTypeExpression @@ var "te"]))
             (encodeType @@ var "cx" @@ var "g" @@ var "dom")) $
       Strings.cat (list [
         string "(", var "paramText", string ") => (", var "b", string ")"]),
     _Term_application>>: lambda "app" $
       -- Flatten the application spine and check whether the head is a
       -- known-lazy primitive. If so, wrap the lazy-positioned arg(s)
       -- in `() => expr` so the runtime primitive controls when (and
       -- whether) those expressions evaluate. See the comment on
       -- `flattenApplication` for the full list.
       "asTerm" <~ inject _Term _Term_application (var "app") $
       "flat" <~ (flattenApplication @@ var "asTerm") $
       "headTerm" <~ Pairs.first (var "flat") $
       "args" <~ Pairs.second (var "flat") $
       "mName" <~ (termHeadVariable @@ var "headTerm") $
       "argc" <~ Lists.length (var "args") $
       -- Returns Just <full call> if this head matches one of the lazy
       -- primitives at exactly the expected arity, with the right args
       -- already wrapped. Nothing otherwise.
       "lazyMaybe" <~ Maybes.cases (var "mName")
         (nothing :: TTerm (Maybe String))
         (lambda "n" $
           "qn" <~ Core.unName (var "n") $
           Logic.ifElse
             (Logic.and (Equality.equal (var "qn") (string "hydra.lib.logic.ifElse"))
                        (Equality.equal (var "argc") (int32 3)))
             (just (encodeLazyCall
                     @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm" @@ var "args"
                     @@ list [false, true, true]))
             (Logic.ifElse
               (Logic.and (Equality.equal (var "qn") (string "hydra.lib.maybes.cases"))
                          (Equality.equal (var "argc") (int32 3)))
               (just (encodeLazyCall
                       @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm" @@ var "args"
                       @@ list [false, true, false]))
               (Logic.ifElse
                 (Logic.and
                   (Logic.or (Equality.equal (var "qn") (string "hydra.lib.maybes.maybe"))
                             (Equality.equal (var "qn") (string "hydra.lib.maybes.fromMaybe")))
                   (Equality.equal (var "argc") (int32 2)))
                 (just (encodeLazyCall
                         @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm" @@ var "args"
                         @@ list [true, false]))
                 (Logic.ifElse
                   (Logic.and
                     (Logic.or (Equality.equal (var "qn") (string "hydra.lib.eithers.fromLeft"))
                               (Equality.equal (var "qn") (string "hydra.lib.eithers.fromRight")))
                     (Equality.equal (var "argc") (int32 2)))
                   (just (encodeLazyCall
                           @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm" @@ var "args"
                           @@ list [true, false]))
                   (Logic.ifElse
                     (Logic.and (Equality.equal (var "qn") (string "hydra.lib.maps.findWithDefault"))
                                (Equality.equal (var "argc") (int32 3)))
                     (just (encodeLazyCall
                             @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm" @@ var "args"
                             @@ list [true, false, false]))
                     (nothing :: TTerm (Maybe String))))))) $
       Maybes.cases (var "lazyMaybe")
         -- Default eager emission.
         ("fn" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.applicationFunction (var "app")) $
          "ag" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.applicationArgument (var "app")) $
          Strings.cat (list [
            string "(", var "fn", string ")(", var "ag", string ")"]))
         (lambda "s" $ var "s"),
     _Term_unit>>: constant $ string "undefined",
     _Term_list>>: lambda "els" $
       Strings.cat (list [
         string "[",
         Strings.intercalate (string ", ") (Lists.map (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs") (var "els")),
         string "]"]),
     _Term_set>>: lambda "s" $
       Strings.cat (list [
         string "new Set([",
         Strings.intercalate (string ", ") (Lists.map (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs") (Sets.toList (var "s"))),
         string "])"]),
     _Term_map>>: lambda "m" $
       Strings.cat (list [
         string "new Map([",
         Strings.intercalate (string ", ")
           (Lists.map
             (lambda "entry" $ Strings.cat (list [
               string "[",
               encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Pairs.first (var "entry"),
               string ", ",
               encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Pairs.second (var "entry"),
               string "]"]))
             (Maps.toList (var "m"))),
         string "])"]),
     _Term_pair>>: lambda "p" $
       Strings.cat (list [
         string "[",
         encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Pairs.first (var "p"),
         string ", ",
         encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Pairs.second (var "p"),
         string "] as const"]),
     -- A Hydra Maybe term encodes to the runtime Maybe shape:
     --   Nothing → { tag: "nothing" }
     --   Just v  → { tag: "just", value: <v> }
     -- This matches the discriminated-union encoding used by the
     -- hand-written core.ts and the lib/maybes runtime helpers.
     _Term_maybe>>: lambda "mt" $
       Maybes.cases (var "mt")
         (string "{ tag: \"nothing\" }")
         (lambda "v" $ Strings.cat (list [
           string "{ tag: \"just\", value: ",
           encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "v",
           string " }"])),
     _Term_record>>: lambda "rec" $
       "fields" <~ Core.recordFields (var "rec") $
       Strings.cat (list [
         string "{ ",
         Strings.intercalate (string ", ")
           (Lists.map
             (lambda "f" $ Strings.cat (list [
               Formatting.sanitizeWithUnderscores @@ TypeScriptLanguageSource.typeScriptReservedWords @@ Core.unName (Core.fieldName (var "f")),
               string ": ",
               encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.fieldTerm (var "f")]))
             (var "fields")),
         string " }"]),
     _Term_inject>>: lambda "inj" $
       "fname" <~ Core.unName (Core.fieldName (Core.injectionField (var "inj"))) $
       "fnameLit" <~ (tsEscapeString @@ var "fname") $
       "fterm" <~ Core.fieldTerm (Core.injectionField (var "inj")) $
       "isUnit" <~ (cases _Term (Strip.deannotateTerm @@ var "fterm")
         (Just false) [
         _Term_unit>>: constant true]) $
       Logic.ifElse (var "isUnit")
         (Strings.cat (list [
           string "{ tag: ", var "fnameLit", string " }"]))
         (Strings.cat (list [
           string "{ tag: ", var "fnameLit",
           string ", value: ",
           encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "fterm",
           string " }"])),
     -- A Hydra wrap (newtype) term encodes as `{ value: <body> }`. The
     -- kernel routinely reads the body via `(.value)` projections, so the
     -- wrapper layer must be preserved at runtime rather than erased.
     _Term_wrap>>: lambda "wt" $
       Strings.cat (list [
         string "{ value: ",
         encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.wrappedTermBody (var "wt"),
         string " }"]),
     _Term_let>>: lambda "lt" $
       -- Encode a let as an IIFE with sequentially-evaluated bindings:
       --   (() => { const x = e1; const y = e2; return body; })()
       --
       -- Self-recursive bindings (where the body references the binding
       -- name) get emitted as `function` declarations so that the inner
       -- self-reference resolves through hoisting + late binding. This
       -- catches the `let recurse = f (fsub recurse)` fixpoint pattern
       -- common in Hydra's visitor-style traversals.
       "bindings" <~ Core.letBindings (var "lt") $
       "body" <~ Core.letBody (var "lt") $
       Strings.cat (list [
         string "(() => { ",
         Strings.cat (Lists.map
           (lambda "b" $
             "bname" <~ Core.bindingName (var "b") $
             "lname" <~ (Formatting.sanitizeWithUnderscores
               @@ TypeScriptLanguageSource.typeScriptReservedWords
               @@ (Names.localNameOf @@ var "bname")) $
             "bterm" <~ Core.bindingTerm (var "b") $
             "isRecursive" <~ Sets.member (var "bname")
               (Variables.freeVariablesInTerm @@ var "bterm") $
             Logic.ifElse (var "isRecursive")
               -- For self-recursive bindings, emit a unary function
               -- declaration so the inner self-reference resolves via
               -- hoisting + late binding. We intentionally take exactly
               -- one argument: JS callers like `xs.map(recurse)` pass
               -- (value, index, array) — if we forwarded all of those,
               -- the second iteration would try to call a value as a
               -- function. Hydra's recursive functions are unary in their
               -- first arg; the curry chain unfolds at the call site.
               (Strings.cat (list [
                 string "function ", var "lname", string "(__a0) { return (",
                 encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "bterm",
                 string ")(__a0); } "]))
               (Strings.cat (list [
                 string "const ", var "lname", string " = ",
                 encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "bterm",
                 string "; "])))
           (var "bindings")),
         string "return ",
         encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "body",
         string "; })()"]),
     -- TypeApplication and TypeLambda are erased at the value level.
     _Term_typeApplication>>: lambda "ta" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.typeApplicationTermBody (var "ta"),
     _Term_typeLambda>>: lambda "tl" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.typeLambdaBody (var "tl"),
     -- Record projection: emit `(x) => x.fname`. The Hydra projection knows
     -- only the typeName and field; the value is supplied at the call site.
     _Term_project>>: lambda "proj" $
       "fname" <~ (Formatting.sanitizeWithUnderscores
         @@ TypeScriptLanguageSource.typeScriptReservedWords
         @@ Core.unName (Core.projectionField (var "proj"))) $
       Strings.cat (list [
         string "((__x) => __x.", var "fname", string ")"]),
     -- Wrapper unwrap: emit `(x) => x.value`. As above, the value is supplied
     -- at the call site; the unwrap term itself just carries the wrapper's
     -- type name.
     _Term_unwrap>>: lambda "_n" $
       string "((__w) => __w.value)",
     -- Union elimination (case statement). Emits a switch-style function:
     --   ((__u) => __u.tag === "a" ? aHandler(__u.value) : __u.tag === "b"
     --     ? bHandler(__u.value) : defaultHandler(__u))
     -- Each arm's handler is a TS function; we apply it to the matched
     -- value (or to the whole tagged object for unit-shaped arms).
     _Term_cases>>: lambda "cs" $
       "armFields" <~ Core.caseStatementCases (var "cs") $
       "defaultMaybe" <~ Core.caseStatementDefault (var "cs") $
       "armsText" <~ (Strings.cat $ Lists.map
         (lambda "f" $ Strings.cat (list [
           string "__u.tag === ",
           tsEscapeString @@ Core.unName (Core.fieldName (var "f")),
           string " ? (",
           encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.fieldTerm (var "f"),
           string ")(__u.value) : "]))
         (var "armFields")) $
       -- Hydra's `cases _Type t (Just defaultTerm) arms`: the default is a
       -- bare value (not a function of the matched union). The per-arm
       -- handlers receive the union's payload; the default does not.
       "tailText" <~ Maybes.cases (var "defaultMaybe")
         (string "(() => { throw new Error(\"unmatched case\"); })()")
         (lambda "dt" $ Strings.cat (list [
           string "(", encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "dt", string ")"])) $
       Strings.cat (list [
         string "((__u) => (",
         var "armsText",
         var "tailText",
         string "))"]),
     -- Either constructor at the term level. Hydra represents `either L R`
     -- as `Either Term Term`; we emit a discriminated-union value mirroring
     -- the type-level encoding from `_Type_either`.
     _Term_either>>: lambda "e" $
       Eithers.either_
         (lambda "l" $ Strings.cat (list [
           string "{ tag: \"left\", value: ",
           encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "l",
           string " }"]))
         (lambda "r" $ Strings.cat (list [
           string "{ tag: \"right\", value: ",
           encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "r",
           string " }"]))
         (var "e")]

-- | Render a Hydra term definition as a TypeScript `export const` statement.
encodeTermDefinition :: TTermDefinition (Context -> Graph -> Namespace -> TermDefinition -> String)
encodeTermDefinition = def "encodeTermDefinition" $
  lambda "cx" $ lambda "g" $ lambda "currentNs" $ lambda "td" $
    "name" <~ Packaging.termDefinitionName (var "td") $
    "lname" <~ (Formatting.sanitizeWithUnderscores @@ TypeScriptLanguageSource.typeScriptReservedWords
      @@ (Names.localNameOf @@ var "name")) $
    "rawTerm" <~ Packaging.termDefinitionTerm (var "td") $
    -- Lambdas at the top level emit as `export function name(x) { return ... }`
    -- so they hoist; this avoids TDZ errors when one alphabetically-ordered
    -- definition references another. Non-lambda definitions stay as
    -- `export const` (they're values that need init order respected, and
    -- Hydra's let-hoisting takes care of that).
    "dterm" <~ (Strip.deannotateTerm @@ var "rawTerm") $
    cases _Term (var "dterm")
      (Just $ Strings.cat (list [
        string "export const ",
        var "lname",
        string " = ",
        encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "rawTerm",
        string ";\n"])) [
      _Term_lambda>>: lambda "lam" $
        "p" <~ (Formatting.sanitizeWithUnderscores
          @@ TypeScriptLanguageSource.typeScriptReservedWords
          @@ (Names.localNameOf @@ Core.lambdaParameter (var "lam"))) $
        "body" <~ Core.lambdaBody (var "lam") $
        -- Top-level lambda definitions: emit a typed parameter if a
        -- domain type is declared on this lambda. Falls back to untyped
        -- on encodeType failure (e.g. unresolved forall vars).
        "domMaybe" <~ Core.lambdaDomain (var "lam") $
        "paramText" <~ Maybes.cases (var "domMaybe")
          (var "p")
          (lambda "dom" $
            Eithers.either_
              (lambda "_e" $ var "p")
              (lambda "te" $ Strings.cat (list [
                var "p", string ": ", printTypeExpression @@ var "te"]))
              (encodeType @@ var "cx" @@ var "g" @@ var "dom")) $
        Strings.cat (list [
          string "export function ",
          var "lname",
          string "(",
          var "paramText",
          string ") { return ",
          encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "body",
          string "; }\n"])]

-- =============================================================================
-- Direct text serializer for TypeScript AST fragments
-- =============================================================================
--
-- A minimal printer for TS.ModuleItem -> String. Avoids the indirection
-- through Hydra.Serialization / Hydra.Ast.Expr used by Java/Scala/Rust
-- Serde modules. The TypeScript surface we emit is small enough to print
-- directly: just interface declarations and type aliases. Layout is
-- minimal — one declaration per block, members one per line.

-- | Render a TypeScript Literal as source text. Only the variants that
-- appear in generated declarations are handled in detail.
printLiteral :: TTermDefinition (TS.Literal -> String)
printLiteral = def "printLiteral" $
  lambda "lit" $ cases TS._Literal (var "lit") (Just $ string "null") [
    TS._Literal_string>>: lambda "sl" $
      tsEscapeString @@ (project TS._StringLiteral TS._StringLiteral_value @@ var "sl"),
    TS._Literal_boolean>>: lambda "b" $
      Logic.ifElse (var "b") (string "true") (string "false"),
    TS._Literal_null>>: constant $ string "null",
    TS._Literal_undefined>>: constant $ string "undefined"]

-- | Render a TypeScript TypeExpression as source text.
printTypeExpression :: TTermDefinition (TS.TypeExpression -> String)
printTypeExpression = def "printTypeExpression" $
  lambda "t" $ cases TS._TypeExpression (var "t") (Just $ string "unknown") [
    TS._TypeExpression_identifier>>: lambda "i" $
      unwrap TS._Identifier @@ var "i",
    TS._TypeExpression_literal>>: lambda "l" $
      printLiteral @@ var "l",
    TS._TypeExpression_array>>: lambda "inner" $
      Strings.cat (list [
        string "ReadonlyArray<",
        printTypeExpression @@ (unwrap TS._ArrayTypeExpression @@ var "inner"),
        string ">"]),
    TS._TypeExpression_tuple>>: lambda "ts" $
      Strings.cat (list [
        string "readonly [",
        Strings.intercalate (string ", ") (Lists.map (asTerm printTypeExpression) (var "ts")),
        string "]"]),
    TS._TypeExpression_union>>: lambda "ts" $
      Strings.intercalate (string " | ")
        (Lists.map (asTerm printTypeExpression) (var "ts")),
    TS._TypeExpression_intersection>>: lambda "ts" $
      Strings.intercalate (string " & ")
        (Lists.map (asTerm printTypeExpression) (var "ts")),
    TS._TypeExpression_parameterized>>: lambda "p" $
      Strings.cat (list [
        printTypeExpression @@ (project TS._ParameterizedTypeExpression TS._ParameterizedTypeExpression_base @@ var "p"),
        string "<",
        Strings.intercalate (string ", ")
          (Lists.map (asTerm printTypeExpression)
            (project TS._ParameterizedTypeExpression TS._ParameterizedTypeExpression_arguments @@ var "p")),
        string ">"]),
    TS._TypeExpression_optional>>: lambda "inner" $
      Strings.cat (list [
        printTypeExpression @@ var "inner",
        string " | undefined"]),
    TS._TypeExpression_readonly>>: lambda "inner" $
      Strings.cat2 (string "readonly ") (printTypeExpression @@ var "inner"),
    TS._TypeExpression_object>>: lambda "ms" $
      Strings.cat (list [
        string "{ ",
        Strings.intercalate (string "; ")
          (Lists.map (asTerm printPropertySignature) (var "ms")),
        string " }"]),
    TS._TypeExpression_function>>: lambda "f" $
      Strings.cat (list [
        string "(",
        Strings.intercalate (string ", ")
          (Lists.map
            (lambda "p" $ Strings.cat2 (string "_: ") (printTypeExpression @@ var "p"))
            (project TS._FunctionTypeExpression TS._FunctionTypeExpression_parameters @@ var "f")),
        string ") => ",
        printTypeExpression @@ (project TS._FunctionTypeExpression TS._FunctionTypeExpression_returnType @@ var "f")]),
    TS._TypeExpression_any>>: constant $ string "any",
    TS._TypeExpression_unknown>>: constant $ string "unknown",
    TS._TypeExpression_void>>: constant $ string "void",
    TS._TypeExpression_never>>: constant $ string "never"]

-- | Render a property signature: `[readonly ]<name>[?]: <type>`.
printPropertySignature :: TTermDefinition (TS.PropertySignature -> String)
printPropertySignature = def "printPropertySignature" $
  lambda "ps" $
    Strings.cat (list [
      Logic.ifElse (project TS._PropertySignature TS._PropertySignature_readonly @@ var "ps")
        (string "readonly ") (string ""),
      unwrap TS._Identifier @@ (project TS._PropertySignature TS._PropertySignature_name @@ var "ps"),
      Logic.ifElse (project TS._PropertySignature TS._PropertySignature_optional @@ var "ps")
        (string "?") (string ""),
      string ": ",
      printTypeExpression @@ (project TS._PropertySignature TS._PropertySignature_type @@ var "ps")])

-- | Render one generic parameter (with optional `extends` constraint).
printTypeParameter :: TTermDefinition (TS.TypeParameter -> String)
printTypeParameter = def "printTypeParameter" $
  lambda "tp" $
    "name" <~ (unwrap TS._Identifier @@ (project TS._TypeParameter TS._TypeParameter_name @@ var "tp")) $
    "constraint" <~ (project TS._TypeParameter TS._TypeParameter_constraint @@ var "tp") $
    Maybes.cases (var "constraint")
      (var "name")
      (lambda "c" $ Strings.cat (list [
        var "name",
        string " extends ",
        printTypeExpression @@ var "c"]))

-- | Render a `<T, U extends Foo>` block, or empty string when no parameters.
printTypeParameterList :: TTermDefinition ([TS.TypeParameter] -> String)
printTypeParameterList = def "printTypeParameterList" $
  lambda "tps" $
    Logic.ifElse (Lists.null (var "tps"))
      (string "")
      (Strings.cat (list [
        string "<",
        Strings.intercalate (string ", ")
          (Lists.map (asTerm printTypeParameter) (var "tps")),
        string ">"]))

-- | Render an interface declaration:
--
--     export interface Foo<T> extends Bar {
--       readonly field: T;
--     }
printInterfaceDeclaration :: TTermDefinition (TS.InterfaceDeclaration -> String)
printInterfaceDeclaration = def "printInterfaceDeclaration" $
  lambda "decl" $
    "name" <~ (unwrap TS._Identifier @@ (project TS._InterfaceDeclaration TS._InterfaceDeclaration_name @@ var "decl")) $
    "params" <~ (printTypeParameterList @@ (project TS._InterfaceDeclaration TS._InterfaceDeclaration_typeParameters @@ var "decl")) $
    "exts" <~ (project TS._InterfaceDeclaration TS._InterfaceDeclaration_extends @@ var "decl") $
    "extClause" <~ Logic.ifElse (Lists.null (var "exts"))
      (string "")
      (Strings.cat2 (string " extends ")
        (Strings.intercalate (string ", ")
          (Lists.map (asTerm printTypeExpression) (var "exts")))) $
    "members" <~ (project TS._InterfaceDeclaration TS._InterfaceDeclaration_members @@ var "decl") $
    "body" <~ Logic.ifElse (Lists.null (var "members"))
      (string "")
      (Strings.cat (list [
        string "\n  ",
        Strings.intercalate (string ";\n  ")
          (Lists.map (asTerm printPropertySignature) (var "members")),
        string ";\n"])) $
    Strings.cat (list [
      string "export interface ",
      var "name",
      var "params",
      var "extClause",
      string " {",
      var "body",
      string "}\n"])

-- | Render a type alias declaration:
--
--     export type Foo<T> = …;
printTypeAliasDeclaration :: TTermDefinition (TS.TypeAliasDeclaration -> String)
printTypeAliasDeclaration = def "printTypeAliasDeclaration" $
  lambda "decl" $
    "name" <~ (unwrap TS._Identifier @@ (project TS._TypeAliasDeclaration TS._TypeAliasDeclaration_name @@ var "decl")) $
    "params" <~ (printTypeParameterList @@ (project TS._TypeAliasDeclaration TS._TypeAliasDeclaration_typeParameters @@ var "decl")) $
    "rhs" <~ (printTypeExpression @@ (project TS._TypeAliasDeclaration TS._TypeAliasDeclaration_type @@ var "decl")) $
    Strings.cat (list [
      string "export type ",
      var "name",
      var "params",
      string " = ",
      var "rhs",
      string ";\n"])

-- | Render a top-level module item. Only interface and type-alias arms emit
-- text today; the other arms (statement / import / export) return "".
printModuleItem :: TTermDefinition (TS.ModuleItem -> String)
printModuleItem = def "printModuleItem" $
  lambda "mi" $ cases TS._ModuleItem (var "mi") (Just $ string "") [
    TS._ModuleItem_interface>>: asTerm printInterfaceDeclaration,
    TS._ModuleItem_typeAlias>>: asTerm printTypeAliasDeclaration]

-- =============================================================================
-- Topological sort of term definitions
-- =============================================================================

-- | Reorder term definitions so each definition appears after the
-- intra-module definitions it depends on. This avoids JS Temporal Dead
-- Zone errors for `const` declarations that reference other constants
-- defined later in alphabetical order (which is Hydra's source order).
sortTermDefsTopologically :: TTermDefinition (Namespace -> [TermDefinition] -> [TermDefinition])
sortTermDefsTopologically = def "sortTermDefsTopologically" $
  lambda "currentNs" $ lambda "tdefs" $
    -- Build a Map<Name, TermDefinition> keyed by full Hydra name.
    "byName" <~ (Maps.fromList $ Lists.map
      (lambda "td" $
        pair (Packaging.termDefinitionName (var "td")) (var "td"))
      (var "tdefs")) $
    -- Build adjacency: each Name → list of in-module Names referenced
    -- by its body.
    "adjacency" <~ (Lists.map
      (lambda "td" $
        "tname" <~ Packaging.termDefinitionName (var "td") $
        "tterm" <~ Packaging.termDefinitionTerm (var "td") $
        "freeVars" <~ (Variables.freeVariablesInTerm @@ var "tterm") $
        -- Keep only references that point to defs in this module.
        "deps" <~ (Lists.filter
          (lambda "n" $ Maps.member (var "n") (var "byName"))
          (Sets.toList (var "freeVars"))) $
        pair (var "tname") (var "deps"))
      (var "tdefs")) $
    -- topologicalSortComponents returns SCCs in dependency-first order,
    -- which is exactly what we need for hoisting-safe init order.
    "sccs" <~ (Sorting.topologicalSortComponents @@ var "adjacency") $
    -- Flatten SCCs (cycles are tolerated; their order within is the input
    -- order) and map names back to TermDefinitions.
    Maybes.cat $ Lists.map
      (lambda "n" $ Maps.lookup (var "n") (var "byName"))
      (Lists.concat (var "sccs"))

-- =============================================================================
-- Module top-level
-- =============================================================================

-- | Convert a Hydra module to a map from `.ts` file path to TypeScript source.
--
-- Emits both type definitions (interfaces, type aliases) and term definitions
-- (export const). Imports are collected from type definitions only today;
-- term-level cross-module refs may be missing imports.
moduleToTypeScript :: TTermDefinition (Module -> [Definition] -> Context -> Graph -> Either Error (M.Map FilePath String))
moduleToTypeScript = def "moduleToTypeScript" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~>
    "currentNs" <~ Packaging.moduleNamespace (var "mod") $
    "partitioned" <~ (Environment.partitionDefinitions @@ var "defs") $
    "typeDefs" <~ Pairs.first (var "partitioned") $
    "rawTermDefs" <~ Pairs.second (var "partitioned") $
    -- Topologically sort term definitions so dependencies appear before
    -- dependents. JS const declarations are TDZ-sensitive: forward
    -- references to a not-yet-initialized const throw at module init.
    -- Lambda definitions emit as `function` (hoisted), but value
    -- definitions emit as `const` and need correct ordering.
    "termDefs" <~ (sortTermDefsTopologically @@ var "currentNs" @@ var "rawTermDefs") $
    -- Collect cross-module references from each TypeDefinition body and from
    -- each TermDefinition body. The union goes into a single import block.
    "typeImports" <~ (Lists.foldl
      (lambda "acc" $ lambda "td" $
        Sets.union (var "acc")
          (collectImports @@ var "currentNs"
            @@ (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme (var "td")))))
      (Sets.empty :: TTerm (S.Set Name))
      (var "typeDefs")) $
    "termImports" <~ (Lists.foldl
      (lambda "acc" $ lambda "td" $
        Sets.union (var "acc")
          (collectTermImports @@ var "currentNs"
            @@ (Packaging.termDefinitionTerm (var "td"))))
      (Sets.empty :: TTerm (S.Set Name))
      (var "termDefs")) $
    "typeImportsBlock" <~ (importsToText @@ string "type" @@ var "currentNs" @@ var "typeImports") $
    "termImportsBlock" <~ (importsToText @@ string "value" @@ var "currentNs" @@ var "termImports") $
    "importsBlock" <~ Strings.cat2 (var "typeImportsBlock") (var "termImportsBlock") $
    "items" <<~ (Eithers.mapList (encodeTypeDefinition @@ var "cx" @@ var "g") (var "typeDefs")) $
    "header" <~ string "// Note: this is an automatically generated file. Do not edit.\n\n" $
    "typeBody" <~ (Strings.intercalate (string "\n")
      (Lists.map (asTerm printModuleItem) (var "items"))) $
    "termBody" <~ (Strings.cat
      (Lists.map (encodeTermDefinition @@ var "cx" @@ var "g" @@ var "currentNs") (var "termDefs"))) $
    "filePath" <~ (Names.namespaceToFilePath
      @@ Util.caseConventionCamel
      @@ wrap _FileExtension (string "ts")
      @@ (Packaging.moduleNamespace (var "mod"))) $
    right (Maps.singleton (var "filePath")
      (Strings.cat (list [
        var "header",
        var "importsBlock",
        Logic.ifElse (Equality.equal (var "importsBlock") (string "")) (string "") (string "\n"),
        var "typeBody",
        Logic.ifElse (Equality.equal (var "termBody") (string "")) (string "") (string "\n"),
        var "termBody"])))
