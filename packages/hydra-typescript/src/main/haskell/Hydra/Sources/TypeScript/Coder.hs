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
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
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
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Errors                          as Error
import qualified Hydra.Dsl.Packaging                       as Packaging
import qualified Hydra.Dsl.Typing                          as Typing
import qualified Hydra.Dsl.Util                            as Util
import qualified Hydra.Sources.Kernel.Terms.Analysis       as Analysis
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Environment    as Environment
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Scoping        as Scoping
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
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


ns :: ModuleName
ns = ModuleName "hydra.typeScript.coder"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> (
              [moduleName TypeScriptLanguageSource.module_,
               moduleName TypeScriptSerdeSource.module_,
               Analysis.ns, Annotations.ns, Environment.ns, Formatting.ns, Names.ns, Rewriting.ns,
               Serialization.ns, Sorting.ns, Strip.ns, Variables.ns]
              L.++ (TypeScriptSyntax.ns : KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = descriptionMetadata (Just "TypeScript code generator: emits TypeScript type declarations from Hydra modules")}
  where
    definitions = [
      toDefinition collectForallParams,
      toDefinition collectImports,
      toDefinition collectTermImports,
      toDefinition collectInnerTypeImports,
      toDefinition encodeLiteral,
      toDefinition filterNonLocalNames,
      toDefinition encodeLiteralType,
      toDefinition encodeBindingAsStatement,
      toDefinition encodeParam,
      toDefinition tsEnvGetGraph,
      toDefinition tsEnvSetGraph,
      toDefinition analyzeTypeScriptFunction,
      toDefinition encodeTerm,
      toDefinition encodeTermDefinition,
      toDefinition encodeLazyCall,
      toDefinition lazyFlagsForPrimitive,
      toDefinition encodeTypeOrAny,
      toDefinition functionDeclarationFromTerm,
      toDefinition sanitizeParamName,
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
      toDefinition sortBindingsTopologically,
      toDefinition sortTermDefsTopologically,
      toDefinition stripForalls,
      toDefinition tsArray,
      toDefinition tsArrow,
      toDefinition tsArrowTyped,
      toDefinition tsCall,
      toDefinition tsCond,
      toDefinition tsEscapeString,
      toDefinition tsExprIdent,
      toDefinition tsExprStr,
      toDefinition tsIdent,
      toDefinition tsMember,
      toDefinition tsTypedIdent,
      toDefinition tsNamedType,
      toDefinition tsNew,
      toDefinition tsObject,
      toDefinition tsAsAny,
      toDefinition tsParam,
      toDefinition tsParamApp1,
      toDefinition tsParamApp2,
      toDefinition tsParen,
      toDefinition tsPropSig,
      toDefinition tsPropSigWithDoc,
      toDefinition mkDocComment,
      toDefinition tsReadonlyMap,
      toDefinition tsReadonlySet,
      toDefinition tsTuple,
      toDefinition tsUndefined]

-- =============================================================================
-- Constructors for TypeScript AST fragments
-- =============================================================================

-- | Concrete TS-side wrapper around `Analysis.analyzeFunctionTerm` with
-- `env = Graph`. Mirrors `analyzeJavaFunction` / `analyzePythonFunction`.
analyzeTypeScriptFunction :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error (FunctionStructure Graph))
analyzeTypeScriptFunction = def "analyzeTypeScriptFunction" $
  "cx" ~> "g" ~> "term" ~>
    Analysis.analyzeFunctionTerm @@ var "cx"
      @@ tsEnvGetGraph
      @@ tsEnvSetGraph
      @@ var "g" @@ var "term"

-- | Collect the bound parameter names from a chain of nested foralls, in
-- outer-to-inner order. Stops at the first non-forall type.
collectForallParams :: TypedTermDefinition (Type -> [Name])
collectForallParams = def "collectForallParams" $
  lambda "t" $
    "dt" <~ (Strip.deannotateType @@ var "t") $
    cases _Type (var "dt") (Just $ list ([] :: [TypedTerm Name])) [
      _Type_forall>>: lambda "fa" $
        Lists.cons
          (Core.forallTypeParameter (var "fa"))
          (collectForallParams @@ Core.forallTypeBody (var "fa"))]

-- =============================================================================
-- Type-definition encoding
-- =============================================================================

-- | Find every qualified name referenced by a Type that lives in a different
-- namespace than the current module's. Used to compute the imports needed
-- at the top of the emitted .ts file. Uses Variables.freeVariablesInType
-- to walk the whole tree (it handles foralls correctly by excluding bound
-- parameters).
collectImports :: TypedTermDefinition (ModuleName -> Type -> S.Set Name)
collectImports = def "collectImports" $
  lambda "currentNs" $ lambda "t" $
    "vars" <~ (Variables.freeVariablesInType @@ var "t") $
    filterNonLocalNames @@ var "currentNs" @@ var "vars"

-- | Walk a term and collect free TYPE variables from lambda domains,
-- type applications, and let-binding type schemes. The top-level
-- `typeImportsFromTerms` only walks the top-level typeScheme, which
-- misses types referenced only in inner lambdas (e.g. `function helper(
-- ids: ReadonlyMap<Name, SubtermNode>, ...) { ... }`). This helper
-- supplements that by walking the term tree.
collectInnerTypeImports :: TypedTermDefinition (ModuleName -> Term -> S.Set Name)
collectInnerTypeImports = def "collectInnerTypeImports" $
  lambda "currentNs" $ lambda "term" $
    "subs" <~ (Rewriting.subterms @@ var "term") $
    -- Recursively gather type names from this term plus all subterms.
    "ownVars" <~ (cases _Term (Strip.deannotateTerm @@ var "term")
      (Just (Sets.empty :: TypedTerm (S.Set Name))) [
      _Term_lambda>>: lambda "lam" $
        Maybes.cases (Core.lambdaDomain (var "lam"))
          (Sets.empty :: TypedTerm (S.Set Name))
          (lambda "d" $ Variables.freeVariablesInType @@ var "d"),
      _Term_typeApplication>>: lambda "ta" $
        Variables.freeVariablesInType @@ Core.typeApplicationTermType (var "ta"),
      _Term_typeLambda>>: constant $ (Sets.empty :: TypedTerm (S.Set Name)),
      _Term_let>>: lambda "lt" $
        -- A let-binding's type scheme can mention types not in the body's
        -- typeScheme — collect them too.
        Lists.foldl
          (lambda "acc" $ lambda "b" $
            Maybes.cases (Core.bindingTypeScheme (var "b"))
              (var "acc")
              (lambda "ts" $ Sets.union (var "acc")
                (Variables.freeVariablesInType @@ Core.typeSchemeBody (var "ts"))))
          (Sets.empty :: TypedTerm (S.Set Name))
          (Core.letBindings (var "lt"))]) $
    "childVars" <~ (Lists.foldl
      (lambda "acc" $ lambda "s" $
        Sets.union (var "acc")
          (collectInnerTypeImports @@ var "currentNs" @@ var "s"))
      (Sets.empty :: TypedTerm (S.Set Name))
      (var "subs")) $
    filterNonLocalNames @@ var "currentNs" @@ (Sets.union (var "ownVars") (var "childVars"))

-- | Same as `collectImports` but walks a Term, gathering free term-level
-- variables that resolve to a different module (i.e. references to other
-- generated `export const` definitions or to runtime lib primitives).
collectTermImports :: TypedTermDefinition (ModuleName -> Term -> S.Set Name)
collectTermImports = def "collectTermImports" $
  lambda "currentNs" $ lambda "t" $
    "vars" <~ (Variables.freeVariablesInTerm @@ var "t") $
    filterNonLocalNames @@ var "currentNs" @@ var "vars"

def :: String -> TypedTerm a -> TypedTermDefinition a
def = definitionInModule module_

-- | Encode a let-binding as a TS `Statement` inside an enclosing function body.
-- If the binding's value is a lambda chain, emit a nested `function` declaration
-- (which hoists). Otherwise emit a `const name = <expr>;` variable declaration.
encodeBindingAsStatement :: TypedTermDefinition (InferenceContext -> Graph -> ModuleName -> Binding -> TS.Statement)
encodeBindingAsStatement = def "encodeBindingAsStatement" $
  "cx" ~> "g" ~> "currentNs" ~> "b" ~>
    "bname" <~ Core.bindingName (var "b") $
    "lname" <~ (Formatting.sanitizeWithUnderscores
      @@ TypeScriptLanguageSource.typeScriptReservedWords
      @@ (Names.localNameOf @@ var "bname")) $
    "bterm" <~ Core.bindingTerm (var "b") $
    "dterm" <~ (Strip.deannotateTerm @@ var "bterm") $
    cases _Term (var "dterm")
      (Just $
        -- Non-lambda value: `const name = <expr>;`
        "expr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "bterm") $
        "declarator" <~ (record TS._VariableDeclarator [
          TS._VariableDeclarator_id>>: inject TS._Pattern TS._Pattern_identifier (tsIdent @@ var "lname"),
          TS._VariableDeclarator_init>>: just (var "expr")]) $
        "varDecl" <~ (record TS._VariableDeclaration [
          TS._VariableDeclaration_kind>>: inject TS._VariableKind TS._VariableKind_const unit,
          TS._VariableDeclaration_declarations>>: list [var "declarator"]]) $
        inject TS._Statement TS._Statement_variableDeclaration (var "varDecl")) [
      _Term_lambda>>: lambda "_lam" $
        -- Lambda value: emit `function name(...) { ... }` so it hoists and can
        -- self-reference (recursive let). The inner function is itself built
        -- via the function-structure pipeline.
        "innerFunDecl" <~ (functionDeclarationFromTerm @@ var "cx" @@ var "g" @@ var "currentNs"
          @@ var "lname" @@ var "bterm"
          @@ (Maybes.bind (Core.bindingTypeScheme (var "b"))
                ("ts" ~> just (Core.typeSchemeBody (var "ts"))))) $
        inject TS._Statement TS._Statement_functionDeclaration (var "innerFunDecl")]

-- | Emit a fully-applied primitive call with selected arguments wrapped
-- in `() => expr` thunks. The `lazyFlags` list parallels `args`:
-- True positions are wrapped, False positions are emitted normally.
-- Returns a single call expression with all args supplied at once.
encodeLazyCall :: TypedTermDefinition (InferenceContext -> Graph -> ModuleName -> Term -> [Term] -> [Bool] -> TS.Expression)
encodeLazyCall = def "encodeLazyCall" $
  lambda "cx" $ lambda "g" $ lambda "currentNs" $ lambda "headTerm" $ lambda "args" $ lambda "lazyFlags" $
    "headExpr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm") $
    "paired" <~ Lists.zip (var "args") (var "lazyFlags") $
    "renderArg" <~ (lambda "p" $
      "argTerm" <~ Pairs.first (var "p") $
      "isLazy" <~ Pairs.second (var "p") $
      "expr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "argTerm") $
      Logic.ifElse (var "isLazy")
        (tsArrow @@ list ([] :: [TypedTerm String]) @@ var "expr")
        (var "expr")) $
    "argExprs" <~ Lists.map (var "renderArg") (var "paired") $
    tsCall @@ var "headExpr" @@ var "argExprs"

-- | Look up a primitive by name and return its per-parameter laziness flags
-- (the `isLazy` flag of each parameter of its signature), in parameter order.
-- Returns the empty list if the name is not a registered primitive. Coders
-- consult these flags instead of hard-coding which primitive arguments are
-- lazy. See issue #391.
lazyFlagsForPrimitive :: TypedTermDefinition (Graph -> Name -> [Bool])
lazyFlagsForPrimitive = def "lazyFlagsForPrimitive" $
  lambda "g" $ lambda "name" $
    Maybes.cases (Maps.lookup (var "name") (Graph.graphPrimitives (var "g")))
      (list ([] :: [TypedTerm Bool]))
      (lambda "prim" $
        Lists.map (lambda "p" $ Typing.parameterIsLazy (var "p"))
          (Typing.termSignatureParameters
            (Packaging.primitiveDefinitionSignature
              (Graph.primitiveDefinition (var "prim")))))

-- | Render a Hydra Literal as a TypeScript Expression.
--
-- Notes on numeric encoding gaps in the current AST:
--   * `NumericLiteral` covers Int64 and Double; that suffices for int8/16/32
--     and float32/64 but not bigint or u/int64. For those, emit `BigInt("n")`
--     as a call expression. (TS has a `123n` literal syntax but the AST
--     doesn't represent it yet.)
--   * Decimals carry no exact representation in TS; emit as a Double via
--     `Literals.readDecimal` since the runtime treats them as `number`.
--   * Binary literals encode as base64-tagged string literals; the runtime
--     decoder (extractCore.binary) accepts either Uint8Array or base64.
encodeLiteral :: TypedTermDefinition (Literal -> TS.Expression)
encodeLiteral = def "encodeLiteral" $
  "litExpr" <~ (lambda "lit" $
    inject TS._Expression TS._Expression_literal (var "lit")) $
  "numLit" <~ (lambda "i" $
    var "litExpr" @@ inject TS._Literal TS._Literal_number
      (inject TS._NumericLiteral TS._NumericLiteral_integer (var "i"))) $
  "floatLit" <~ (lambda "f" $
    var "litExpr" @@ inject TS._Literal TS._Literal_number
      (inject TS._NumericLiteral TS._NumericLiteral_float (var "f"))) $
  "strLit" <~ (lambda "s" $
    var "litExpr" @@ inject TS._Literal TS._Literal_string
      (record TS._StringLiteral [
        TS._StringLiteral_value>>: var "s",
        TS._StringLiteral_singleQuote>>: boolean False])) $
  "boolLit" <~ (lambda "b" $
    var "litExpr" @@ inject TS._Literal TS._Literal_boolean (var "b")) $
  "bigIntCall" <~ (lambda "txt" $
    tsCall @@ (tsExprIdent @@ string "BigInt") @@ list [var "strLit" @@ var "txt"]) $
  lambda "lit" $ cases _Literal (var "lit")
    (Just $ var "litExpr" @@ inject TS._Literal TS._Literal_null unit) [
    _Literal_binary>>: lambda "b" $
      var "strLit" @@ (Literals.binaryToString (var "b")),
    _Literal_boolean>>: lambda "b" $
      var "boolLit" @@ var "b",
    _Literal_decimal>>: lambda "d" $
      -- Decimals carry no exact representation in TS; convert through bigint
      -- to keep the value but drop fractional precision. (Hydra's `Decimal`
      -- is rarely fractional in generated kernel code.)
      var "numLit" @@ (Literals.bigintToInt64 (Literals.decimalToBigint (var "d"))),
    _Literal_string>>: lambda "s" $
      var "strLit" @@ var "s",
    _Literal_integer>>: lambda "iv" $
      cases _IntegerValue (var "iv") (Just $ var "numLit" @@ (Literals.bigintToInt64 (Literals.int32ToBigint (int32 0)))) [
        _IntegerValue_bigint>>: lambda "n" $
          var "bigIntCall" @@ (Literals.showBigint (var "n")),
        _IntegerValue_int8>>:   lambda "n" $
          var "numLit" @@ (Literals.bigintToInt64 (Literals.int8ToBigint (var "n"))),
        _IntegerValue_int16>>:  lambda "n" $
          var "numLit" @@ (Literals.bigintToInt64 (Literals.int16ToBigint (var "n"))),
        _IntegerValue_int32>>:  lambda "n" $
          var "numLit" @@ (Literals.bigintToInt64 (Literals.int32ToBigint (var "n"))),
        _IntegerValue_int64>>:  lambda "n" $
          var "bigIntCall" @@ (Literals.showInt64 (var "n")),
        _IntegerValue_uint8>>:  lambda "n" $
          var "numLit" @@ (Literals.bigintToInt64 (Literals.uint8ToBigint (var "n"))),
        _IntegerValue_uint16>>: lambda "n" $
          var "numLit" @@ (Literals.bigintToInt64 (Literals.uint16ToBigint (var "n"))),
        _IntegerValue_uint32>>: lambda "n" $
          var "numLit" @@ (Literals.bigintToInt64 (Literals.uint32ToBigint (var "n"))),
        _IntegerValue_uint64>>: lambda "n" $
          var "bigIntCall" @@ (Literals.showUint64 (var "n"))],
    _Literal_float>>: lambda "fv" $
      cases _FloatValue (var "fv") (Just $ var "floatLit" @@ (float64 0.0)) [
        _FloatValue_float32>>: lambda "f" $ var "floatLit" @@ (Literals.float32ToFloat64 (var "f")),
        _FloatValue_float64>>: lambda "f" $ var "floatLit" @@ var "f"]]

-- | Map a Hydra LiteralType to a TypeScript TypeExpression.
encodeLiteralType :: TypedTermDefinition (LiteralType -> TS.TypeExpression)
encodeLiteralType = def "encodeLiteralType" $
  lambda "lt" $ cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $
      tsParamApp1 @@ string "ReadonlyArray" @@ (tsNamedType @@ string "number"),
    _LiteralType_boolean>>: constant $
      tsNamedType @@ string "boolean",
    _LiteralType_decimal>>: constant $
      tsNamedType @@ string "number",
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

-- | Build a TS function parameter as a `Pattern`. Wraps the parameter name in a
-- `_Pattern_typed` if the domain encodes to anything other than the analyze
-- pass's `_`-typed-variable sentinel; otherwise emits an untyped identifier.
encodeParam :: TypedTermDefinition (InferenceContext -> Graph -> Name -> Type -> TS.Pattern)
encodeParam = def "encodeParam" $
  "cx" ~> "g" ~> "pname" ~> "dom" ~>
    "nstr" <~ (sanitizeParamName @@ var "pname") $
    cases _Type (Strip.deannotateType @@ var "dom")
      (Just $ tsTypedIdent @@ var "nstr"
        @@ (encodeTypeOrAny @@ var "cx" @@ var "g" @@ var "dom")) [
      -- Bare type variables (e.g. `t0`) come from polymorphic top-level
      -- defs. Function declarations have no generic-binder syntax in our
      -- AST yet, so we cannot bring `T0` into scope. Emit `any` for these
      -- params. The analyze pass uses `_` as a sentinel for missing-type;
      -- previously we emitted an untyped identifier for `_`, but that
      -- trips `noImplicitAny`. Emit `: any` uniformly for all bare type
      -- variables.
      _Type_variable>>: constant $
        tsTypedIdent @@ var "nstr"
          @@ (inject TS._TypeExpression TS._TypeExpression_any unit)]

-- | Render a Hydra Term as a TypeScript Expression.
--
-- The first argument is the current module's namespace; it's used to decide
-- whether a `Term_variable` reference is local (emit bare name) or external
-- (emit `<alias>.<name>` matching the namespace-style import).
--
-- All emission goes through the `TS.Expression` AST and is serialized via
-- `Serde.expressionToExpr` + `Serialization.printExpr`. This is the same
-- pipeline the Java/Scala/Python coders use; it gives proper precedence,
-- newlines, and indentation without manual string assembly.
encodeTerm :: TypedTermDefinition (InferenceContext -> Graph -> ModuleName -> Term -> TS.Expression)
encodeTerm = def "encodeTerm" $
  lambda "cx" $ lambda "g" $ lambda "currentNs" $ lambda "term" $ cases _Term (var "term")
    (Just $ tsExprIdent @@ string "null")
    [_Term_annotated>>: lambda "at" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.annotatedTermBody (var "at"),
     _Term_literal>>: lambda "lit" $
       encodeLiteral @@ var "lit",
     _Term_variable>>: lambda "n" $
       "local" <~ (Formatting.sanitizeWithUnderscores
         @@ TypeScriptLanguageSource.typeScriptReservedWords
         @@ (Names.localNameOf @@ var "n")) $
       -- Same-namespace (or no namespace = lambda-bound variable): emit
       -- just the local identifier. Different namespace: emit `alias.local`
       -- as a member expression, matching the namespace-style import in
       -- the file header.
       Maybes.cases (Names.moduleNameOf @@ var "n")
         (tsExprIdent @@ var "local")
         (lambda "ns" $
           Logic.ifElse
             (Equality.equal
               (unwrap _ModuleName @@ var "currentNs")
               (unwrap _ModuleName @@ var "ns"))
             (tsExprIdent @@ var "local")
             ("nsSegs" <~ (Lists.drop (int32 1)
                (Strings.splitOn (string ".") (unwrap _ModuleName @@ var "ns"))) $
              -- Must match the prefix used in importsToText.
              "alias" <~ Strings.cat2 (string "$mod_")
                (Strings.intercalate (string "_") (var "nsSegs")) $
              tsMember @@ (tsExprIdent @@ var "alias") @@ var "local")),
     -- Lambda emission: peel through nested `λx.λy.λz.body` into a single
     -- `(x, y, z) => body` uncurried arrow, mirroring Python's
     -- `makeUncurriedLambda` driven by `analyzePythonFunction`. The kernel
     -- emits curried Hydra terms, but every call site (and every lib runtime)
     -- speaks flat positional args, so the two must agree on the lambda shape.
     -- Type annotations on parameters are not yet emitted; once the AST grows
     -- a typed-parameter slot we can populate it from `fsDoms`.
     _Term_lambda>>: lambda "_lam" $
       "lamTerm" <~ inject _Term _Term_lambda (var "_lam") $
       "fsLE" <~ (analyzeTypeScriptFunction @@ var "cx" @@ var "g" @@ var "lamTerm") $
       "fsL" <~ Eithers.either_
         (lambda "_err" $ record _FunctionStructure [
           _FunctionStructure_typeParams>>: list ([] :: [TypedTerm Name]),
           _FunctionStructure_params>>: list ([] :: [TypedTerm Name]),
           _FunctionStructure_bindings>>: list ([] :: [TypedTerm Binding]),
           _FunctionStructure_body>>: var "lamTerm",
           _FunctionStructure_domains>>: list ([] :: [TypedTerm Type]),
           _FunctionStructure_codomain>>: (nothing :: TypedTerm (Maybe Type)),
           _FunctionStructure_environment>>: var "g"])
         (lambda "ok" $ var "ok")
         (var "fsLE") $
       "fsLParams" <~ (project _FunctionStructure _FunctionStructure_params @@ var "fsL") $
       "fsLDoms" <~ (project _FunctionStructure _FunctionStructure_domains @@ var "fsL") $
       "fsLBindings" <~ (project _FunctionStructure _FunctionStructure_bindings @@ var "fsL") $
       "fsLBody" <~ (project _FunctionStructure _FunctionStructure_body @@ var "fsL") $
       "fsLEnv" <~ (project _FunctionStructure _FunctionStructure_environment @@ var "fsL") $
       -- If `analyzeFunctionTerm` lifted any let-bindings out of the lambda
       -- body, re-introduce them as a `Term_let` wrapping the inner body.
       -- That recurses back through `_Term_let`'s IIFE-chain encoder, so
       -- the multi-arg arrow still gets the lifted bindings in scope.
       "innerBody" <~ Logic.ifElse (Lists.null (var "fsLBindings"))
         (var "fsLBody")
         (Core.termLet (Core.let_ (var "fsLBindings") (var "fsLBody"))) $
       -- Emit `(p: unknown) => body` so `noImplicitAny` is satisfied.
       -- We use `unknown` rather than the inferred domain because inner
       -- lambdas may reference type variables (`T0`, `T1`, …) that are only
       -- bound at the enclosing function-decl's generic clause — arrows have
       -- no generic-binder syntax. Top-level term-defs still use proper
       -- typed params via `encodeParam` in `functionDeclarationFromTerm`.
       -- Walk params left-to-right, threading an index counter. When the
       -- local name is the wildcard `_` (which the kernel uses for ignored
       -- params), append the index so the emitted arrow has unique param
       -- names — ESM/strict mode rejects `(_, _) =>` etc.
       "paramAcc" <~ (Lists.foldl
         (lambda "acc" $ lambda "pn" $
           "idx" <~ Pairs.first (var "acc") $
           "pats" <~ Pairs.second (var "acc") $
           "raw" <~ (Names.localNameOf @@ var "pn") $
           "uniq" <~ Logic.ifElse (Equality.equal (var "raw") (string "_"))
             (Strings.cat2 (string "_") (Literals.showInt32 (var "idx")))
             (var "raw") $
           "pat" <~ (tsTypedIdent
             @@ (Formatting.sanitizeWithUnderscores
                   @@ TypeScriptLanguageSource.typeScriptReservedWords
                   @@ var "uniq")
             -- `any` here, not `unknown`: inline arrow params are passed into
             -- typed positions (e.g. as foldl/map callbacks) where `unknown`
             -- triggers TS2345 ("unknown not assignable to T"). `any` opts
             -- out of checking for these synthesized inner arrows; the
             -- top-level function decl is still strictly typed via encodeParam.
             @@ (inject TS._TypeExpression TS._TypeExpression_any unit)) $
           pair (Math.add (var "idx") (int32 1))
                (Lists.concat2 (var "pats") (list [var "pat"])))
         (pair (int32 0) (list ([] :: [TypedTerm TS.Pattern])))
         (var "fsLParams")) $
       "paramPatterns" <~ Pairs.second (var "paramAcc") $
       "bExpr" <~ (encodeTerm @@ var "cx" @@ var "fsLEnv" @@ var "currentNs" @@ var "innerBody") $
       tsArrowTyped @@ var "paramPatterns" @@ var "bExpr",
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
       -- Returns `Just <full call Expression>` if the head is a registered
       -- primitive, applied to exactly its full arity, with at least one
       -- parameter flagged lazy in the primitive's signature metadata; the
       -- flagged args are then wrapped as thunks. `Nothing` otherwise. The
       -- lazy positions come from the primitive's `isLazy` flags, not a
       -- hard-coded name table (issue #391).
       "lazyMaybe" <~ Maybes.cases (var "mName")
         (nothing :: TypedTerm (Maybe TS.Expression))
         (lambda "n" $
           "lazyFlags" <~ (lazyFlagsForPrimitive @@ var "g" @@ var "n") $
           "anyLazy" <~ Lists.foldl (lambda "b" $ lambda "f" $ Logic.or (var "b") (var "f"))
                          false (var "lazyFlags") $
           Logic.ifElse
             (Logic.and (var "anyLazy")
                        (Equality.equal (var "argc") (Lists.length (var "lazyFlags"))))
             (just (encodeLazyCall
                     @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm" @@ var "args"
                     @@ var "lazyFlags"))
             (nothing :: TypedTerm (Maybe TS.Expression))) $
       Maybes.cases (var "lazyMaybe")
         -- Default eager emission: detect projection/unwrap heads applied
         -- to one or more args, and inline as `firstArg.field(restArgs...)`
         -- instead of `(x => x.field)(firstArg, restArgs...)`. This mirrors
         -- Python's `encodeApplicationInner` project/unwrap branches.
         -- Without this, flattening a spine across a projection head
         -- collapses all args into the lambda's parameter slot — the
         -- lambda takes only one, ignoring the rest, returning a function
         -- instead of evaluating.
         ("dHead" <~ (Strip.deannotateAndDetypeTerm @@ var "headTerm") $
          "encArgs" <~ Lists.map (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs") (var "args") $
          cases _Term (var "dHead")
            (Just $
              "headExpr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm") $
              tsCall @@ var "headExpr" @@ var "encArgs") [
            _Term_project>>: lambda "proj" $
              Logic.ifElse (Lists.null (var "encArgs"))
                -- No args: emit the bare arrow `(x) => x.field`.
                ("headExpr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm") $
                 var "headExpr")
                ("fname" <~ (Formatting.sanitizeWithUnderscores
                   @@ TypeScriptLanguageSource.typeScriptReservedWords
                   @@ Core.unName (Core.projectionFieldName (var "proj"))) $
                 "firstA" <~ Maybes.fromMaybe (tsExprIdent @@ string "undefined") (Lists.maybeHead (var "encArgs")) $
                 "restA" <~ (Lists.drop (int32 1) (var "encArgs")) $
                 "fieldExpr" <~ (tsMember @@ var "firstA" @@ var "fname") $
                 Logic.ifElse (Lists.null (var "restA"))
                   (var "fieldExpr")
                   (tsCall @@ var "fieldExpr" @@ var "restA")),
            _Term_unwrap>>: constant $
              Logic.ifElse (Lists.null (var "encArgs"))
                ("headExpr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "headTerm") $
                 var "headExpr")
                ("firstA" <~ Maybes.fromMaybe (tsExprIdent @@ string "undefined") (Lists.maybeHead (var "encArgs")) $
                 "restA" <~ (Lists.drop (int32 1) (var "encArgs")) $
                 "valueExpr" <~ (tsMember @@ var "firstA" @@ string "value") $
                 Logic.ifElse (Lists.null (var "restA"))
                   (var "valueExpr")
                   (tsCall @@ var "valueExpr" @@ var "restA"))])
         (lambda "e" $ var "e"),
     _Term_unit>>: constant $ tsUndefined,
     _Term_list>>: lambda "els" $
       tsArray @@ Lists.map (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs") (var "els"),
     _Term_set>>: lambda "s" $
       -- new Set([e1, e2, ...])
       tsNew @@ (tsExprIdent @@ string "Set") @@ list [
         tsArray @@ Lists.map (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs") (Sets.toList (var "s"))],
     _Term_map>>: lambda "m" $
       -- new Map([[k1, v1], [k2, v2], ...])
       tsNew @@ (tsExprIdent @@ string "Map") @@ list [
         tsArray @@ Lists.map
           (lambda "entry" $ tsArray @@ list [
             encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Pairs.first (var "entry"),
             encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Pairs.second (var "entry")])
           (Maps.toList (var "m"))],
     _Term_pair>>: lambda "p" $
       -- A Hydra pair (a, b) emits as `[a, b] as any`. The `as any` cast
       -- bypasses tsc's array-vs-tuple inference: a bare `[a, b]` literal
       -- infers as `(A | B)[]` rather than `readonly [A, B]`, so passing
       -- it to a parameter typed as a specific tuple shape (e.g.
       -- `readonly [SubtermStep, Term]`) triggers TS2345. The runtime
       -- representation is identical.
       tsAsAny @@ (tsArray @@ list [
         encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Pairs.first (var "p"),
         encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Pairs.second (var "p")]),
     -- A Hydra Maybe term encodes to the runtime Maybe shape:
     --   Nothing → ({ tag: "nothing" } as any)
     --   Just v  → ({ tag: "just", value: <v> } as any)
     -- The `as any` cast lets the literal flow into nominal positions
     -- typed as `Maybe<X>` / `Term` / `Type` without TS2322 churn.
     _Term_maybe>>: lambda "mt" $
       Maybes.cases (var "mt")
         (tsAsAny @@ (tsObject @@ list [pair (string "tag") (tsExprStr @@ string "nothing")]))
         (lambda "v" $ tsAsAny @@ (tsObject @@ list [
           pair (string "tag") (tsExprStr @@ string "just"),
           pair (string "value") (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "v")])),
     _Term_record>>: lambda "rec" $
       "fields" <~ Core.recordFields (var "rec") $
       tsObject @@ Lists.map
         (lambda "f" $ pair
           (Formatting.sanitizeWithUnderscores @@ TypeScriptLanguageSource.typeScriptReservedWords
             @@ Core.unName (Core.fieldName (var "f")))
           (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.fieldTerm (var "f")))
         (var "fields"),
     -- A Hydra union injection encodes as `({ tag: <name> } as any)` or
     -- `({ tag: <name>, value: <body> } as any)`. The `as any` cast bypasses
     -- the narrow discriminated-union types tsc infers for the literal,
     -- which often fail to unify with the broader nominal target type
     -- (e.g. `Type` or `Term`) without expensive annotations.
     _Term_inject>>: lambda "inj" $
       "fname" <~ Core.unName (Core.fieldName (Core.injectionField (var "inj"))) $
       "fterm" <~ Core.fieldTerm (Core.injectionField (var "inj")) $
       "isUnit" <~ (cases _Term (Strip.deannotateTerm @@ var "fterm")
         (Just false) [
         _Term_unit>>: constant true]) $
       Logic.ifElse (var "isUnit")
         (tsAsAny @@ (tsObject @@ list [pair (string "tag") (tsExprStr @@ var "fname")]))
         (tsAsAny @@ (tsObject @@ list [
           pair (string "tag") (tsExprStr @@ var "fname"),
           pair (string "value") (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "fterm")])),
     -- A Hydra wrap (newtype) term encodes as `{ value: <body> }`. The
     -- kernel routinely reads the body via `(.value)` projections, so the
     -- wrapper layer must be preserved at runtime rather than erased.
     _Term_wrap>>: lambda "wt" $
       tsObject @@ list [pair (string "value")
         (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.wrappedTermBody (var "wt"))],
     -- A let encodes as an IIFE: `(() => { const x = e1; ...; return body; })()`.
     -- The AST doesn't yet have a clean way to inline this as a block-bodied
     -- arrow (the Pattern node lacks a typed-init slot), so we use nested
     -- arrows: each binding becomes a tail-position arrow application,
     -- producing `(x => (y => (... => body)(eN))(...))(e1)`. This preserves
     -- evaluation order and lexical scoping at the cost of left-leaning
     -- nesting. Recursive bindings are handled by emitting `(x => body)`
     -- where `body` self-references — JavaScript closures resolve this via
     -- TDZ + late binding within the arrow's scope.
     _Term_let>>: lambda "lt" $
       -- Emit `(() => { stmts...; return body; })()` rather than the
       -- IIFE chain `((b => acc)(def))`. Each binding is encoded via
       -- `encodeBindingAsStatement`, which emits `function name(...)`
       -- for lambda-valued bindings (so recursive let works through
       -- function-name hoisting) and `const name = expr;` otherwise.
       "bindings" <~ (sortBindingsTopologically @@ Core.letBindings (var "lt")) $
       "body" <~ Core.letBody (var "lt") $
       "encodedBody" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "body") $
       "bindingStmts" <~ Lists.map
         (lambda "b" $ encodeBindingAsStatement @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "b")
         (var "bindings") $
       "returnStmt" <~ (inject TS._Statement TS._Statement_return (just (var "encodedBody"))) $
       "stmts" <~ (Lists.concat2 (var "bindingStmts") (list [var "returnStmt"])) $
       "iifeArrow" <~ (inject TS._Expression TS._Expression_arrow $
         record TS._ArrowFunctionExpression [
           TS._ArrowFunctionExpression_params>>: list ([] :: [TypedTerm TS.Pattern]),
           TS._ArrowFunctionExpression_body>>:
             inject TS._ArrowFunctionBody TS._ArrowFunctionBody_block (var "stmts"),
           TS._ArrowFunctionExpression_async>>: boolean False]) $
       tsCall @@ var "iifeArrow" @@ list ([] :: [TypedTerm TS.Expression]),
     -- TypeApplication and TypeLambda are erased at the value level.
     _Term_typeApplication>>: lambda "ta" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.typeApplicationTermBody (var "ta"),
     _Term_typeLambda>>: lambda "tl" $
       encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.typeLambdaBody (var "tl"),
     -- Record projection: `(x: any) => x.fname`. Typed as `any` to satisfy
     -- noImplicitAny when this expression appears in `unknown`-typed
     -- positions (such as continuation args).
     _Term_project>>: lambda "proj" $
       "fname" <~ (Formatting.sanitizeWithUnderscores
         @@ TypeScriptLanguageSource.typeScriptReservedWords
         @@ Core.unName (Core.projectionFieldName (var "proj"))) $
       tsArrowTyped @@ list [
         tsTypedIdent @@ string "x"
           @@ (inject TS._TypeExpression TS._TypeExpression_any unit)]
         @@ (tsMember @@ (tsExprIdent @@ string "x") @@ var "fname"),
     -- Wrapper unwrap: `(x: any) => x.value`.
     _Term_unwrap>>: lambda "_n" $
       tsArrowTyped @@ list [
         tsTypedIdent @@ string "x"
           @@ (inject TS._TypeExpression TS._TypeExpression_any unit)]
         @@ (tsMember @@ (tsExprIdent @@ string "x") @@ string "value"),
     -- Union elimination. Emits a tag-discriminating arrow:
     --   (u) => u.tag === "a" ? armA(u.value) : ... : defaultHandler
     -- The arm bodies are applied to `u.value` (or to `u` itself for
     -- unit-shaped arms); the default handler is a bare value, not a fn.
     _Term_cases>>: lambda "cs" $
       -- Emit a switch-statement IIFE rather than a right-fold of
       -- ternaries. The previous nested-ternary form (`tag === "a" ?
       -- armA(u.value) : tag === "b" ? armB(u.value) : default`) compiles
       -- correctly but adds two stack frames per arm (one ternary, one
       -- IIFE-application). For a 21-arm `cases` like the kernel's
       -- `typeOf` dispatcher, that's 42 extra frames per call — and
       -- typeOf chains through deeply-nested terms blow V8's ~8K-frame
       -- stack when running under Rosetta-emulated Node on Apple Silicon.
       -- The switch-statement form has constant per-arm overhead (one
       -- call frame per arm body) and is also clearer.
       --
       -- Emitted shape:
       --   ((u) => {
       --     switch ((u as any).tag) {
       --       case "<arm1Name>": return arm1Expr((u as any).value);
       --       ...
       --       default: return defaultExpr;  // or throw
       --     }
       --   })
       --
       -- Hosts that prefer hoisting `cases` out of inline positions can
       -- still do so via `doHoistCase`; this emission then naturally
       -- becomes the body of a top-level `function name(u) { switch ...
       -- }` instead of an IIFE.
       "armFields" <~ Core.caseStatementCases (var "cs") $
       "defaultMaybe" <~ Core.caseStatementDefault (var "cs") $
       "uVar" <~ string "u" $
       -- Cast `u` to any so `.value` works for unit-shaped variants too.
       "uExpr" <~ (tsAsAny @@ (tsExprIdent @@ var "uVar")) $
       "uTag" <~ (tsMember @@ var "uExpr" @@ string "tag") $
       "uValue" <~ (tsMember @@ var "uExpr" @@ string "value") $
       -- Build one `SwitchCase` per arm: `case "<fname>": return
       -- <armExpr>(u.value);`.
       "armCases" <~ Lists.map
         (lambda "f" $
           "fname" <~ Core.unName (Core.fieldName (var "f")) $
           "armExpr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ Core.fieldTerm (var "f")) $
           "callExpr" <~ (tsCall @@ var "armExpr" @@ list [var "uValue"]) $
           record TS._SwitchCase [
             TS._SwitchCase_test>>: just (tsExprStr @@ var "fname"),
             TS._SwitchCase_consequent>>: list [
               inject TS._Statement TS._Statement_return (just (var "callExpr"))]])
         (var "armFields") $
       -- Default arm: emit either `default: return <default-expr>;` if
       -- the user supplied one, or `default: throw new Error("unmatched
       -- case");` otherwise.
       "defaultCase" <~ Maybes.cases (var "defaultMaybe")
         -- No default: throw. The TS AST doesn't have a Throw statement
         -- variant, so emit the throw via an unsanitized identifier
         -- inside an Expression statement (return of an IIFE that
         -- throws); see the older fold form for the same trick.
         (record TS._SwitchCase [
           TS._SwitchCase_test>>: (nothing :: TypedTerm (Maybe TS.Expression)),
           TS._SwitchCase_consequent>>: list [
             inject TS._Statement TS._Statement_return (just
               (tsCall @@ (tsExprIdent @@ string "(() => { throw new Error('unmatched case'); })")
                       @@ list ([] :: [TypedTerm TS.Expression])))]])
         (lambda "dt" $
           "dExpr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "dt") $
           record TS._SwitchCase [
             TS._SwitchCase_test>>: (nothing :: TypedTerm (Maybe TS.Expression)),
             TS._SwitchCase_consequent>>: list [
               inject TS._Statement TS._Statement_return (just (var "dExpr"))]]) $
       "allCases" <~ (Lists.concat2 (var "armCases") (list [var "defaultCase"])) $
       -- Assemble the switch statement and wrap in a block-bodied arrow.
       "switchStmt" <~ (inject TS._Statement TS._Statement_switch
         (record TS._SwitchStatement [
           TS._SwitchStatement_discriminant>>: var "uTag",
           TS._SwitchStatement_cases>>: var "allCases"])) $
       inject TS._Expression TS._Expression_arrow
         (record TS._ArrowFunctionExpression [
           TS._ArrowFunctionExpression_params>>: list [
             inject TS._Pattern TS._Pattern_identifier (tsIdent @@ var "uVar")],
           TS._ArrowFunctionExpression_body>>:
             inject TS._ArrowFunctionBody TS._ArrowFunctionBody_block (list [var "switchStmt"]),
           TS._ArrowFunctionExpression_async>>: boolean False]),
     -- Either constructor at the term level: { tag: "left", value: l } or
     -- { tag: "right", value: r }.
     _Term_either>>: lambda "e" $
       Eithers.either_
         (lambda "l" $ tsAsAny @@ (tsObject @@ list [
           pair (string "tag") (tsExprStr @@ string "left"),
           pair (string "value") (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "l")]))
         (lambda "r" $ tsAsAny @@ (tsObject @@ list [
           pair (string "tag") (tsExprStr @@ string "right"),
           pair (string "value") (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "r")]))
         (var "e")]

-- | Render a Hydra term definition as a TypeScript module item.
--
-- Drives encoding through `Analysis.analyzeFunctionTerm`, mirroring the Java
-- and Python coders: the top-level term is peeled into a FunctionStructure
-- (explicit params, typed domains, hoisted let bindings, inner body), then
-- emitted as either `export const name = expr;` (zero-arg) or
-- `export function name(p1: T1, ..., pN: TN) { ...bindings... return body; }`.
-- The returned pair carries an optional JSDoc description (pulled from
-- the term's description annotation, if any) alongside the ModuleItem.
encodeTermDefinition :: TypedTermDefinition (InferenceContext -> Graph -> ModuleName -> TermDefinition -> (Maybe String, TS.ModuleItem))
encodeTermDefinition = def "encodeTermDefinition" $
  lambda "cx" $ lambda "g" $ lambda "currentNs" $ lambda "td" $
    "name" <~ Packaging.termDefinitionName (var "td") $
    "lname" <~ (Formatting.sanitizeWithUnderscores @@ TypeScriptLanguageSource.typeScriptReservedWords
      @@ (Names.localNameOf @@ var "name")) $
    "rawTerm" <~ Packaging.termDefinitionTerm (var "td") $
    -- Pull the term's description annotation. Extraction errors (rare —
    -- only when the value isn't a string) collapse to "no doc" so we
    -- never fail the whole module emit over a single bad annotation.
    "mdoc" <~ Eithers.either_ (constant nothing) identity
      (Annotations.getTermDescription @@ var "cx" @@ var "g" @@ var "rawTerm") $
    "asExport" <~ (lambda "stmt" $
      inject TS._ModuleItem TS._ModuleItem_export
        (inject TS._ExportDeclaration TS._ExportDeclaration_declaration (var "stmt"))) $
    "mScheme" <~ (Maybes.bind (Packaging.termDefinitionSignature (var "td"))
      ("sig" ~> just (Core.typeSchemeBody (Scoping.termSignatureToTypeScheme @@ var "sig")))) $
    -- Determine whether the term is a function (Term_lambda) or carries any
    -- typeLambda wrappers (the typical pattern for polymorphic top-level
    -- defs: `typeLambda T1. typeLambda T2. lambda x. body`). In both cases
    -- emit a function declaration via the function-structure pipeline,
    -- which peels typeLambdas/lambdas itself. Otherwise emit
    -- `export const lname = <expr>;`.
    "dterm" <~ (Strip.deannotateTerm @@ var "rawTerm") $
    "funDecl" <~ (functionDeclarationFromTerm @@ var "cx" @@ var "g" @@ var "currentNs"
      @@ var "lname" @@ var "rawTerm" @@ var "mScheme") $
    "asFunDecl" <~ (var "asExport" @@ (inject TS._Statement TS._Statement_functionDeclaration (var "funDecl"))) $
    "item" <~ cases _Term (var "dterm")
      (Just $
        "expr" <~ (encodeTerm @@ var "cx" @@ var "g" @@ var "currentNs" @@ var "rawTerm") $
        "declarator" <~ (record TS._VariableDeclarator [
          TS._VariableDeclarator_id>>: inject TS._Pattern TS._Pattern_identifier (tsIdent @@ var "lname"),
          TS._VariableDeclarator_init>>: just (var "expr")]) $
        "varDecl" <~ (record TS._VariableDeclaration [
          TS._VariableDeclaration_kind>>: inject TS._VariableKind TS._VariableKind_const unit,
          TS._VariableDeclaration_declarations>>: list [var "declarator"]]) $
        var "asExport" @@ (inject TS._Statement TS._Statement_variableDeclaration (var "varDecl"))) [
      _Term_lambda>>: lambda "_lam" $ var "asFunDecl",
      _Term_typeLambda>>: lambda "_tl" $ var "asFunDecl"] $
    pair (var "mdoc") (var "item")

-- =============================================================================
-- Direct text serializer for TypeScript AST fragments
-- =============================================================================
--
-- A minimal printer for TS.ModuleItem -> String. Avoids the indirection
-- through Hydra.Serialization / Hydra.Ast.Expr used by Java/Scala/Rust
-- Serde modules. The TypeScript surface we emit is small enough to print
-- directly: just interface declarations and type aliases. Layout is
-- minimal — one declaration per block, members one per line.

-- | Map a Hydra Type to a TypeScript TypeExpression.
--
-- Anonymous records and unions are rejected — they must appear as the body of
-- a named TypeDefinition so the coder can emit an interface or discriminated-
-- union alias.
encodeType :: TypedTermDefinition (InferenceContext -> Graph -> Type -> Either Error TS.TypeExpression)
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
      -- Encode `Maybe T` inline as `{tag: "just", value: T} | {tag: "nothing"}`
      -- matching the runtime value encoding, rather than `T | undefined`
      -- (which mismatched the kernel's `{tag: "just", value: x}` literals).
      -- Inlining (like Either) avoids needing a Maybe import in every module.
      _Type_maybe>>: lambda "inner" $
        Eithers.map (lambda "enc" $
          inject TS._TypeExpression TS._TypeExpression_union (list [
            inject TS._TypeExpression TS._TypeExpression_object $ list [
              tsPropSig @@ string "tag" @@ boolean False
                @@ (inject TS._TypeExpression TS._TypeExpression_literal $
                      inject TS._Literal TS._Literal_string $
                      record TS._StringLiteral [
                        TS._StringLiteral_value>>: string "just",
                        TS._StringLiteral_singleQuote>>: boolean False]),
              tsPropSig @@ string "value" @@ boolean False @@ var "enc"],
            inject TS._TypeExpression TS._TypeExpression_object $ list [
              tsPropSig @@ string "tag" @@ boolean False
                @@ (inject TS._TypeExpression TS._TypeExpression_literal $
                      inject TS._Literal TS._Literal_string $
                      record TS._StringLiteral [
                        TS._StringLiteral_value>>: string "nothing",
                        TS._StringLiteral_singleQuote>>: boolean False])]]))
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
              TS._FunctionTypeExpression_typeParameters>>: list ([] :: [TypedTerm TS.TypeParameter]),
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

-- | Encode a Hydra type definition as either an InterfaceDeclaration (records),
-- a TypeAliasDeclaration of a discriminated union (unions), or a plain type
-- alias (everything else).
-- The returned pair carries an optional JSDoc description (pulled from
-- the type's description annotation, if any) alongside the ModuleItem.
-- `moduleToTypeScript` prepends the doc above the rendered item.
encodeTypeDefinition :: TypedTermDefinition (InferenceContext -> Graph -> TypeDefinition -> Either Error (Maybe String, TS.ModuleItem))
encodeTypeDefinition = def "encodeTypeDefinition" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Packaging.typeDefinitionName (var "tdef") $
    "typScheme" <~ Packaging.typeDefinitionTypeScheme (var "tdef") $
    "rawTyp" <~ Core.typeSchemeBody (var "typScheme") $
    "lname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ var "name")) $
    "mdoc" <<~ (Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "rawTyp") $
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
          right (pair (var "mdoc") (inject TS._ModuleItem TS._ModuleItem_typeAlias $
            record TS._TypeAliasDeclaration [
              TS._TypeAliasDeclaration_name>>: tsIdent @@ var "lname",
              TS._TypeAliasDeclaration_typeParameters>>: var "typeParams",
              TS._TypeAliasDeclaration_type>>: var "styp"])))
      [_Type_record>>: lambda "fts" $
        "members" <<~ (Eithers.mapList
          (lambda "ft" $
            "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
            "ftyp"  <~ Core.fieldTypeType (var "ft") $
            "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftyp") $
            "mfdoc" <<~ (Annotations.commentsFromFieldType @@ var "cx" @@ var "g" @@ var "ft") $
              right (tsPropSigWithDoc @@ var "fname" @@ boolean False @@ var "sftyp" @@ (mkDocComment @@ var "mfdoc")))
          (var "fts")) $
          right (pair (var "mdoc") (inject TS._ModuleItem TS._ModuleItem_interface $
            record TS._InterfaceDeclaration [
              TS._InterfaceDeclaration_name>>: tsIdent @@ var "lname",
              TS._InterfaceDeclaration_typeParameters>>: var "typeParams",
              TS._InterfaceDeclaration_extends>>: list ([] :: [TypedTerm TS.TypeExpression]),
              TS._InterfaceDeclaration_members>>: var "members"])),
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
          right (pair (var "mdoc") (inject TS._ModuleItem TS._ModuleItem_typeAlias $
            record TS._TypeAliasDeclaration [
              TS._TypeAliasDeclaration_name>>: tsIdent @@ var "lname",
              TS._TypeAliasDeclaration_typeParameters>>: var "typeParams",
              TS._TypeAliasDeclaration_type>>: inject TS._TypeExpression TS._TypeExpression_union (var "arms")])),
      _Type_wrap>>: lambda "wt" $
        -- A wrap type becomes a single-field interface with a `_tag` brand.
        -- Wrap-type interface: single `value` field. We previously emitted
        -- a `_tag: "TypeName"` brand for nominal typing, but the kernel
        -- term-level emission for `Term_wrap` doesn't add `_tag`, and
        -- doing so breaks runtime value-equality keying in `lib/maps.ts`
        -- (canonical strings differ between primitive Names and term Names).
        -- Structural typing without the brand is good enough — TS unions
        -- still discriminate on shape because each wrap type has a unique
        -- payload-type combination.
        "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "wt") $
          right (pair (var "mdoc") (inject TS._ModuleItem TS._ModuleItem_interface $
            record TS._InterfaceDeclaration [
              TS._InterfaceDeclaration_name>>: tsIdent @@ var "lname",
              TS._InterfaceDeclaration_typeParameters>>: var "typeParams",
              TS._InterfaceDeclaration_extends>>: list ([] :: [TypedTerm TS.TypeExpression]),
              TS._InterfaceDeclaration_members>>: list [
                tsPropSig @@ string "value" @@ boolean False @@ var "sftyp"]]))]

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

-- | Try to encode a Hydra `Core.Type` as a TS `TypeExpression`. If `encodeType`
-- fails (e.g. anonymous record), fall back to `any`.
encodeTypeOrAny :: TypedTermDefinition (InferenceContext -> Graph -> Type -> TS.TypeExpression)
encodeTypeOrAny = def "encodeTypeOrAny" $
  "cx" ~> "g" ~> "typ" ~>
    Eithers.either_
      (lambda "_e" $ inject TS._TypeExpression TS._TypeExpression_any unit)
      (lambda "te" $ var "te")
      (encodeType @@ var "cx" @@ var "g" @@ var "typ")

-- | Shared filter: keep only Names that have a ModuleName distinct from
-- the current module's. Names without a ModuleName (bare lambda params)
-- and Names in the current namespace are dropped.
filterNonLocalNames :: TypedTermDefinition (ModuleName -> S.Set Name -> S.Set Name)
filterNonLocalNames = def "filterNonLocalNames" $
  lambda "currentNs" $ lambda "names" $
    Sets.fromList $ Maybes.cat $ Lists.map
      (lambda "n" $
        Maybes.cases (Names.moduleNameOf @@ var "n")
          nothing
          (lambda "nameNs" $
            Logic.ifElse
              (Equality.equal
                (unwrap _ModuleName @@ var "currentNs")
                (unwrap _ModuleName @@ var "nameNs"))
              nothing
              (just $ var "n")))
      (Sets.toList (var "names"))

-- | Walk an application spine, returning (innermost head term, args in
-- application order). For `App (App (App f a) b) c)` returns `(f, [a,b,c])`.
flattenApplication :: TypedTermDefinition (Term -> (Term, [Term]))
flattenApplication = def "flattenApplication" $
  lambda "t" $
    "dt" <~ (Strip.deannotateTerm @@ var "t") $
    cases _Term (var "dt")
      (Just $ pair (var "t") (list ([] :: [TypedTerm Term]))) [
      _Term_application>>: lambda "app" $
        "inner" <~ (flattenApplication @@ Core.applicationFunction (var "app")) $
        "head_" <~ Pairs.first (var "inner") $
        "prevArgs" <~ Pairs.second (var "inner") $
        pair (var "head_")
             (Lists.concat2 (var "prevArgs")
                            (Lists.singleton (Core.applicationArgument (var "app"))))]

-- | Build a TS `FunctionDeclaration` from a Hydra term. Uses
-- `Analysis.analyzeFunctionTerm` to peel lambdas (and any nested let bindings)
-- into the FunctionStructure shape: explicit params + statements + body.
-- The `mScheme :: Maybe Type` arg is the binding's known type scheme body
-- (for codomain inference); pass `Nothing` if not available.
functionDeclarationFromTerm :: TypedTermDefinition (InferenceContext -> Graph -> ModuleName -> String -> Term -> Maybe Type -> TS.FunctionDeclaration)
functionDeclarationFromTerm = def "functionDeclarationFromTerm" $
  "cx" ~> "g" ~> "currentNs" ~> "lname" ~> "term" ~> "_mScheme" ~>
    "fsE" <~ (analyzeTypeScriptFunction @@ var "cx" @@ var "g" @@ var "term") $
    "fs" <~ Eithers.either_
      (lambda "_err" $ record _FunctionStructure [
        _FunctionStructure_typeParams>>: list ([] :: [TypedTerm Name]),
        _FunctionStructure_params>>: list ([] :: [TypedTerm Name]),
        _FunctionStructure_bindings>>: list ([] :: [TypedTerm Binding]),
        _FunctionStructure_body>>: var "term",
        _FunctionStructure_domains>>: list ([] :: [TypedTerm Type]),
        _FunctionStructure_codomain>>: (nothing :: TypedTerm (Maybe Type)),
        _FunctionStructure_environment>>: var "g"])
      (lambda "ok" $ var "ok")
      (var "fsE") $
    "fsParams" <~ (project _FunctionStructure _FunctionStructure_params @@ var "fs") $
    "fsDoms" <~ (project _FunctionStructure _FunctionStructure_domains @@ var "fs") $
    "fsBindings" <~ (project _FunctionStructure _FunctionStructure_bindings @@ var "fs") $
    "fsBody" <~ (project _FunctionStructure _FunctionStructure_body @@ var "fs") $
    "fsEnv" <~ (project _FunctionStructure _FunctionStructure_environment @@ var "fs") $
    -- Pad `fsDoms` with a Type_variable "_" sentinel up to fsParams length:
    -- eta-expanded params (from kernel adapt's doExpand) come back with
    -- empty domains, so a naive zip drops them entirely, producing
    -- `function name() { ... }` with the parameter elided from the
    -- signature. Padding ensures every param appears, typed as `any`
    -- via the `_Type_variable "_"` arm of encodeParam.
    "domPad" <~ Core.typeVariable (Core.name (string "_")) $
    "fsDomsPadded" <~ (Lists.concat2 (var "fsDoms")
      (Lists.replicate
        (Math.sub (Lists.length (var "fsParams")) (Lists.length (var "fsDoms")))
        (var "domPad"))) $
    "paramPatterns" <~ (Lists.map
      (lambda "pair" $ encodeParam @@ var "cx" @@ var "fsEnv" @@ (Pairs.first $ var "pair") @@ (Pairs.second $ var "pair"))
      (Lists.zip (var "fsParams") (var "fsDomsPadded"))) $
    "sortedBindings" <~ (sortBindingsTopologically @@ var "fsBindings") $
    "bindingStmts" <~ (Lists.map
      (lambda "b" $ encodeBindingAsStatement @@ var "cx" @@ var "fsEnv" @@ var "currentNs" @@ var "b")
      (var "sortedBindings")) $
    "bodyExpr" <~ (encodeTerm @@ var "cx" @@ var "fsEnv" @@ var "currentNs" @@ var "fsBody") $
    "returnStmt" <~ (inject TS._Statement TS._Statement_return (just (var "bodyExpr"))) $
    "block" <~ (Lists.concat2 (var "bindingStmts") (list [var "returnStmt"])) $
    record TS._FunctionDeclaration [
      TS._FunctionDeclaration_id>>: tsIdent @@ var "lname",
      TS._FunctionDeclaration_params>>: var "paramPatterns",
      TS._FunctionDeclaration_body>>: var "block",
      TS._FunctionDeclaration_async>>: boolean False,
      TS._FunctionDeclaration_generator>>: boolean False]

-- | Render a Set of qualified Names as TypeScript import statements grouped
-- by source module. The current module's own namespace is dropped from the
-- output so the file does not import itself.
-- | Render imports for a Set of cross-module Names.
--
-- `kind` selects the import flavor:
--   "type"  → emits `import type { ... }` with PascalCased names (for type refs).
--   "value" → emits `import { ... }` with names verbatim (for term-level refs).
importsToText :: TypedTermDefinition (String -> ModuleName -> S.Set Name -> String)
importsToText = def "importsToText" $
  lambda "kind" $ lambda "currentNs" $ lambda "names" $
    -- Pair each Name with its optional ModuleName; drop ones with no namespace
    -- or whose namespace matches the current module.
    "pairs" <~ (Maybes.cat $ Lists.map
      (lambda "n" $
        Maybes.cases (Names.moduleNameOf @@ var "n")
          nothing
          (lambda "ns" $
            Logic.ifElse
              (Equality.equal
                (unwrap _ModuleName @@ var "currentNs")
                (unwrap _ModuleName @@ var "ns"))
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
    -- Group by namespace via fold into a Map<ModuleName, [String]>.
    "grouped" <~ (Lists.foldl
      (lambda "acc" $ lambda "p" $
        "ns" <~ Pairs.first (var "p") $
        "n" <~ Pairs.second (var "p") $
        "local" <~ (var "transformLocal" @@ (Names.localNameOf @@ var "n")) $
        "existing" <~ (Maybes.fromMaybe (list ([] :: [TypedTerm String]))
          (Maps.lookup (var "ns") (var "acc"))) $
        Maps.insert (var "ns") (Lists.cons (var "local") (var "existing")) (var "acc"))
      (Maps.empty :: TypedTerm (M.Map ModuleName [String]))
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
      (Strings.splitOn (string ".") (unwrap _ModuleName @@ var "currentNs"))) $
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
          (Strings.splitOn (string ".") (unwrap _ModuleName @@ var "ns"))) $
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
-- Hydra's `ifElse` and several `maybes`/`eithers`/`maps` primitives have
-- lazy semantics in the Haskell source: an unselected branch or unused
-- default is never evaluated. JavaScript is strict, so we wrap the lazy
-- positions in nullary arrow functions `() => expr` and the runtime
-- primitive invokes them on demand. See docs/recipes/new-implementation.md
-- (section "Lazy evaluation and thunking") for the full rationale.
--
-- Which argument positions are lazy is determined by each primitive's
-- per-parameter `isLazy` metadata (the `parameterIsLazy` flag on the
-- primitive's signature), looked up via `lazyFlagsForPrimitive`. This
-- replaces the former hard-coded primitive-name table (issue #391); the
-- kernel signatures in Hydra.Sources.Kernel.Lib.{Logic,Maybes,Eithers,Maps}
-- are the single source of truth.
--
-- The TS-side primitive accepts either a value or a thunk and calls the
-- thunk when needed (mirroring Python's `callable()` check).

-- | Build a `DocumentationComment` from an optional description string.
-- Returns Nothing when the description is missing or empty so callers can
-- propagate "no doc" through the AST. Tags are always empty here; we only
-- emit narrative descriptions, not @param/@returns tags (those would be
-- duplicative with TS's own type annotations).
mkDocComment :: TypedTermDefinition (Maybe String -> Maybe TS.DocumentationComment)
mkDocComment = def "mkDocComment" $
  lambda "mdesc" $ Maybes.cases (var "mdesc")
    nothing
    (lambda "d" $ Logic.ifElse (Equality.equal (var "d") (string ""))
      nothing
      (just (record TS._DocumentationComment [
        TS._DocumentationComment_description>>: var "d",
        TS._DocumentationComment_tags>>: list ([] :: [TypedTerm TS.DocumentationTag])])))

-- | Convert a Hydra module to a map from `.ts` file path to TypeScript source.
--
-- Emits both type definitions (interfaces, type aliases) and term definitions
-- (export const). Imports are collected from type definitions only today;
-- term-level cross-module refs may be missing imports.
moduleToTypeScript :: TypedTermDefinition (Module -> [Definition] -> InferenceContext -> Graph -> Either Error (M.Map FilePath String))
moduleToTypeScript = def "moduleToTypeScript" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~>
    "currentNs" <~ Packaging.moduleName (var "mod") $
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
    "typeImportsFromTypes" <~ (Lists.foldl
      (lambda "acc" $ lambda "td" $
        Sets.union (var "acc")
          (collectImports @@ var "currentNs"
            @@ (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme (var "td")))))
      (Sets.empty :: TypedTerm (S.Set Name))
      (var "typeDefs")) $
    -- Also collect imports from each term-definition's typeScheme: typed
    -- parameters and codomain annotations introduce references to types
    -- from other modules (Term, Graph, InferenceContext, ...) that aren't otherwise
    -- present in the term's free-variable set.
    "typeImportsFromTerms" <~ (Lists.foldl
      (lambda "acc" $ lambda "td" $
        Maybes.cases (Packaging.termDefinitionSignature (var "td"))
          (var "acc")
          (lambda "sig" $ Sets.union (var "acc")
            (collectImports @@ var "currentNs" @@ (Core.typeSchemeBody (Scoping.termSignatureToTypeScheme @@ var "sig")))))
      (Sets.empty :: TypedTerm (S.Set Name))
      (var "termDefs")) $
    -- Also walk inside each term to find type references in inner lambda
    -- domains and nested let-binding schemes. Without this, types only
    -- referenced from nested helper functions (e.g. `function helper(ids:
    -- ReadonlyMap<Name, SubtermNode>, ...)`) fail to import, producing
    -- TS2304 "Cannot find name SubtermNode".
    "typeImportsFromInner" <~ (Lists.foldl
      (lambda "acc" $ lambda "td" $
        Sets.union (var "acc")
          (collectInnerTypeImports @@ var "currentNs"
            @@ (Packaging.termDefinitionTerm (var "td"))))
      (Sets.empty :: TypedTerm (S.Set Name))
      (var "termDefs")) $
    "typeImports" <~ Sets.union (Sets.union (var "typeImportsFromTypes") (var "typeImportsFromTerms"))
                                (var "typeImportsFromInner") $
    "termImports" <~ (Lists.foldl
      (lambda "acc" $ lambda "td" $
        Sets.union (var "acc")
          (collectTermImports @@ var "currentNs"
            @@ (Packaging.termDefinitionTerm (var "td"))))
      (Sets.empty :: TypedTerm (S.Set Name))
      (var "termDefs")) $
    "typeImportsBlock" <~ (importsToText @@ string "type" @@ var "currentNs" @@ var "typeImports") $
    "termImportsBlock" <~ (importsToText @@ string "value" @@ var "currentNs" @@ var "termImports") $
    "importsBlock" <~ Strings.cat2 (var "typeImportsBlock") (var "termImportsBlock") $
    "typeItems" <<~ (Eithers.mapList (encodeTypeDefinition @@ var "cx" @@ var "g") (var "typeDefs")) $
    "termItems" <~ (Lists.map (encodeTermDefinition @@ var "cx" @@ var "g" @@ var "currentNs") (var "termDefs")) $
    "allItems" <~ Lists.concat2 (var "typeItems") (var "termItems") $
    -- Module-level description becomes a JSDoc block at the top of the
    -- file, between the auto-generated warning and the imports.
    "mModuleDoc" <~ (Maybes.bind (Packaging.moduleMetadata (var "mod")) ("em" ~> Packaging.entityMetadataDescription (var "em"))) $
    "moduleDocText" <~ Maybes.cases (var "mModuleDoc")
      (string "")
      (lambda "d" $ Strings.cat2
        (TypeScriptSerdeSource.toTypeScriptComments @@ var "d" @@ (list ([] :: [TypedTerm TS.DocumentationTag])))
        (string "\n\n")) $
    "header" <~ Strings.cat2
      (string "// Note: this is an automatically generated file. Do not edit.\n\n")
      (var "moduleDocText") $
    -- Each item carries an optional doc string sidecar; render the item
    -- via printModuleItem, then prepend the JSDoc when present. Items
    -- are joined by blank lines (uniform per-definition separation).
    "renderItem" <~ ("docAndItem" ~>
      "mdoc" <~ Pairs.first (var "docAndItem") $
      "item" <~ Pairs.second (var "docAndItem") $
      "itemText" <~ (printModuleItem @@ var "item") $
      Maybes.cases (var "mdoc")
        (var "itemText")
        (lambda "d" $ Strings.cat (list [
          TypeScriptSerdeSource.toTypeScriptComments @@ var "d" @@ (list ([] :: [TypedTerm TS.DocumentationTag])),
          string "\n",
          var "itemText"]))) $
    "body" <~ (Strings.intercalate (string "\n\n")
      (Lists.map (var "renderItem") (var "allItems"))) $
    "filePath" <~ (Names.moduleNameToFilePath
      @@ Util.caseConventionCamel
      @@ wrap _FileExtension (string "ts")
      @@ (Packaging.moduleName (var "mod"))) $
    right (Maps.singleton (var "filePath")
      (Strings.cat (list [
        var "header",
        var "importsBlock",
        Logic.ifElse (Equality.equal (var "importsBlock") (string "")) (string "") (string "\n"),
        var "body",
        Logic.ifElse (Equality.equal (var "body") (string "")) (string "") (string "\n")])))

-- | Render an interface declaration:
--
--     export interface Foo<T> extends Bar {
--       readonly field: T;
--     }
printInterfaceDeclaration :: TypedTermDefinition (TS.InterfaceDeclaration -> String)
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
    "renderMember" <~ ("ps" ~>
      Strings.intercalate (string "\n  ")
        (Strings.lines (asTerm printPropertySignature @@ var "ps"))) $
    "body" <~ Logic.ifElse (Lists.null (var "members"))
      (string "")
      (Strings.cat (list [
        string "\n  ",
        Strings.intercalate (string ";\n  ")
          (Lists.map (var "renderMember") (var "members")),
        string ";\n"])) $
    Strings.cat (list [
      string "export interface ",
      var "name",
      var "params",
      var "extClause",
      string " {",
      var "body",
      string "}\n"])

-- | Render a TypeScript Literal as source text. Only the variants that
-- appear in generated declarations are handled in detail.
printLiteral :: TypedTermDefinition (TS.Literal -> String)
printLiteral = def "printLiteral" $
  lambda "lit" $ cases TS._Literal (var "lit") (Just $ string "null") [
    TS._Literal_string>>: lambda "sl" $
      tsEscapeString @@ (project TS._StringLiteral TS._StringLiteral_value @@ var "sl"),
    TS._Literal_boolean>>: lambda "b" $
      Logic.ifElse (var "b") (string "true") (string "false"),
    TS._Literal_null>>: constant $ string "null",
    TS._Literal_undefined>>: constant $ string "undefined"]

-- | Render a top-level module item. Only interface and type-alias arms emit
-- text today; the other arms (statement / import / export) return "".
-- | Render any ModuleItem. Interface and type-alias arms route through the
-- coder-local declaration printers (which produce the readable, type-aware
-- output the type encoder already knows about). Statement/import/export
-- arms delegate to Serde + Serialization.printExpr — the same pipeline the
-- other coders use, giving proper indentation and newlines for terms.
printModuleItem :: TypedTermDefinition (TS.ModuleItem -> String)
printModuleItem = def "printModuleItem" $
  lambda "mi" $ cases TS._ModuleItem (var "mi") (Just $ string "") [
    TS._ModuleItem_interface>>: asTerm printInterfaceDeclaration,
    TS._ModuleItem_typeAlias>>: asTerm printTypeAliasDeclaration,
    TS._ModuleItem_statement>>: lambda "_s" $
      Serialization.printExpr @@ (TypeScriptSerdeSource.moduleItemToExpr @@ var "mi"),
    TS._ModuleItem_import>>: lambda "_i" $
      Serialization.printExpr @@ (TypeScriptSerdeSource.moduleItemToExpr @@ var "mi"),
    TS._ModuleItem_export>>: lambda "_e" $
      Serialization.printExpr @@ (TypeScriptSerdeSource.moduleItemToExpr @@ var "mi")]

-- =============================================================================
-- Topological sort of term definitions
-- =============================================================================

-- | Render a property signature: optional JSDoc comment, then
-- `[readonly ]<name>[?]: <type>`. The comment, when present, is prepended
-- followed by a newline so the formatted member is e.g.
--   /**
--    * Doc text.
--    */
--   readonly fieldName: T;
printPropertySignature :: TypedTermDefinition (TS.PropertySignature -> String)
printPropertySignature = def "printPropertySignature" $
  lambda "ps" $
    "mcomments" <~ (project TS._PropertySignature TS._PropertySignature_comments @@ var "ps") $
    "line" <~ Strings.cat (list [
      Logic.ifElse (project TS._PropertySignature TS._PropertySignature_readonly @@ var "ps")
        (string "readonly ") (string ""),
      unwrap TS._Identifier @@ (project TS._PropertySignature TS._PropertySignature_name @@ var "ps"),
      Logic.ifElse (project TS._PropertySignature TS._PropertySignature_optional @@ var "ps")
        (string "?") (string ""),
      string ": ",
      printTypeExpression @@ (project TS._PropertySignature TS._PropertySignature_type @@ var "ps")]) $
    Maybes.cases (var "mcomments")
      (var "line")
      (lambda "dc" $ Strings.cat (list [
        TypeScriptSerdeSource.toTypeScriptComments
          @@ (project TS._DocumentationComment TS._DocumentationComment_description @@ var "dc")
          @@ (project TS._DocumentationComment TS._DocumentationComment_tags @@ var "dc"),
        string "\n",
        var "line"]))

-- | Render a type alias declaration:
--
--     export type Foo<T> = …;
printTypeAliasDeclaration :: TypedTermDefinition (TS.TypeAliasDeclaration -> String)
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

-- | Render a TypeScript TypeExpression as source text.
printTypeExpression :: TypedTermDefinition (TS.TypeExpression -> String)
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
    -- Function types rendered as `(...args: any[]) => any`. The kernel's
    -- curried types (`A -> B -> C`) don't match the coder's flat-call ABI
    -- (`(a, b) => c`); variadic-any sidesteps arity mismatch and ts2304
    -- "Cannot find name TN" from unbound type variables in inner contexts.
    TS._TypeExpression_function>>: constant $
      string "((...args: any[]) => any)",
    TS._TypeExpression_any>>: constant $ string "any",
    TS._TypeExpression_unknown>>: constant $ string "unknown",
    TS._TypeExpression_void>>: constant $ string "void",
    TS._TypeExpression_never>>: constant $ string "never"]

-- | Render one generic parameter (with optional `extends` constraint).
printTypeParameter :: TypedTermDefinition (TS.TypeParameter -> String)
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
printTypeParameterList :: TypedTermDefinition ([TS.TypeParameter] -> String)
printTypeParameterList = def "printTypeParameterList" $
  lambda "tps" $
    Logic.ifElse (Lists.null (var "tps"))
      (string "")
      (Strings.cat (list [
        string "<",
        Strings.intercalate (string ", ")
          (Lists.map (asTerm printTypeParameter) (var "tps")),
        string ">"]))

-- | Sanitize a Hydra parameter name into a valid (and non-reserved) TS identifier.
sanitizeParamName :: TypedTermDefinition (Name -> String)
sanitizeParamName = def "sanitizeParamName" $
  lambda "n" $ Formatting.sanitizeWithUnderscores
    @@ TypeScriptLanguageSource.typeScriptReservedWords
    @@ (Names.localNameOf @@ var "n")

-- | Reorder let-bindings so each binding appears after the sibling
-- bindings it depends on. Avoids JS Temporal Dead Zone errors when the
-- emitted block has `const a = b;` ahead of `const b = ...;` because
-- Hydra's `let` block has simultaneous-binding semantics (Haskell-style)
-- whereas JS `const` requires definitions-before-uses.
sortBindingsTopologically :: TypedTermDefinition ([Binding] -> [Binding])
sortBindingsTopologically = def "sortBindingsTopologically" $
  lambda "bindings" $
    "byName" <~ (Maps.fromList $ Lists.map
      (lambda "b" $ pair (Core.bindingName (var "b")) (var "b"))
      (var "bindings")) $
    "adjacency" <~ (Lists.map
      (lambda "b" $
        "bname" <~ Core.bindingName (var "b") $
        "bterm" <~ Core.bindingTerm (var "b") $
        "freeVars" <~ (Variables.freeVariablesInTerm @@ var "bterm") $
        "deps" <~ (Lists.filter
          (lambda "n" $ Maps.member (var "n") (var "byName"))
          (Sets.toList (var "freeVars"))) $
        pair (var "bname") (var "deps"))
      (var "bindings")) $
    "sccs" <~ (Sorting.topologicalSortComponents @@ var "adjacency") $
    Maybes.cat $ Lists.map
      (lambda "n" $ Maps.lookup (var "n") (var "byName"))
      (Lists.concat (var "sccs"))

-- | Reorder term definitions so each definition appears after the
-- intra-module definitions it depends on. This avoids JS Temporal Dead
-- Zone errors for `const` declarations that reference other constants
-- defined later in alphabetical order (which is Hydra's source order).
sortTermDefsTopologically :: TypedTermDefinition (ModuleName -> [TermDefinition] -> [TermDefinition])
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

stripForalls :: TypedTermDefinition (Type -> Type)
stripForalls = def "stripForalls" $
  lambda "t" $
    "dt" <~ (Strip.deannotateType @@ var "t") $
    cases _Type (var "dt") (Just $ var "dt") [
      _Type_forall>>: lambda "fa" $
        stripForalls @@ Core.forallTypeBody (var "fa")]

-- | If a term reduces (through annotations and type applications) to
-- `Term_variable name`, return `Just name`; else `Nothing`. Polymorphic
-- primitives are typically wrapped in one or more `Term_typeApplication`
-- layers (e.g. `ifElse @TypeArg`), which the runtime erases — so we
-- erase them here too when looking for the head.
termHeadVariable :: TypedTermDefinition (Term -> Maybe Name)
termHeadVariable = def "termHeadVariable" $
  lambda "t" $
    "dt" <~ (Strip.deannotateTerm @@ var "t") $
    cases _Term (var "dt")
      (Just (nothing :: TypedTerm (Maybe Name))) [
      _Term_variable>>: lambda "n" $ just (var "n"),
      _Term_typeApplication>>: lambda "ta" $
        termHeadVariable @@ Core.typeApplicationTermBody (var "ta")]

-- | `[e1, e2, ...]` array expression. Each element is wrapped in
-- `ArrayElementExpression` (the AST also supports holes / spreads which we
-- don't emit).
tsArray :: TypedTermDefinition ([TS.Expression] -> TS.Expression)
tsArray = def "tsArray" $
  lambda "elems" $
    inject TS._Expression TS._Expression_array $
      Lists.map (lambda "e" $ inject TS._ArrayElement TS._ArrayElement_expression (var "e"))
        (var "elems")

-- | `(p1, p2, ...) => body` arrow function with an expression body. Parameters
-- emit as untyped identifier patterns; type annotations are deferred until the
-- AST grows a typed-parameter slot.
tsArrow :: TypedTermDefinition ([String] -> TS.Expression -> TS.Expression)
tsArrow = def "tsArrow" $
  lambda "params" $ lambda "body" $
    inject TS._Expression TS._Expression_arrow $
      record TS._ArrowFunctionExpression [
        TS._ArrowFunctionExpression_params>>:
          Lists.map (lambda "p" $ inject TS._Pattern TS._Pattern_identifier (tsIdent @@ var "p"))
            (var "params"),
        TS._ArrowFunctionExpression_body>>:
          inject TS._ArrowFunctionBody TS._ArrowFunctionBody_expression (var "body"),
        TS._ArrowFunctionExpression_async>>: boolean False]

-- | `(p1: T1, p2: T2, ...) => body` arrow function with typed patterns.
-- Used by inline `_Term_lambda` emission to satisfy `noImplicitAny`.
tsArrowTyped :: TypedTermDefinition ([TS.Pattern] -> TS.Expression -> TS.Expression)
tsArrowTyped = def "tsArrowTyped" $
  lambda "patterns" $ lambda "body" $
    inject TS._Expression TS._Expression_arrow $
      record TS._ArrowFunctionExpression [
        TS._ArrowFunctionExpression_params>>: var "patterns",
        TS._ArrowFunctionExpression_body>>:
          inject TS._ArrowFunctionBody TS._ArrowFunctionBody_expression (var "body"),
        TS._ArrowFunctionExpression_async>>: boolean False]

-- | Wrap an expression in a TypeScript `as any` cast. Used at sites where
-- the kernel emits a structural object literal that tsc would reject
-- against a nominal discriminated-union target (`{tag: "...", value: ...}`
-- vs `Term`/`Type`/`Maybe<T>`). The runtime is unaffected.
tsAsAny :: TypedTermDefinition (TS.Expression -> TS.Expression)
tsAsAny = def "tsAsAny" $
  lambda "e" $
    inject TS._Expression TS._Expression_asExpression $
      record TS._AsExpression [
        TS._AsExpression_expression>>: var "e",
        TS._AsExpression_type>>: inject TS._TypeExpression TS._TypeExpression_any unit]

-- | `callee(arg, ...)` call expression. Wraps the callee in parens iff it's
-- an arrow function or other expression that's ambiguous as a call target.
tsCall :: TypedTermDefinition (TS.Expression -> [TS.Expression] -> TS.Expression)
tsCall = def "tsCall" $
  lambda "callee" $ lambda "args" $
    inject TS._Expression TS._Expression_call $
      record TS._CallExpression [
        TS._CallExpression_callee>>: var "callee",
        TS._CallExpression_arguments>>: var "args",
        TS._CallExpression_optional>>: boolean False]

-- | `test ? consequent : alternate`.
tsCond :: TypedTermDefinition (TS.Expression -> TS.Expression -> TS.Expression -> TS.Expression)
tsCond = def "tsCond" $
  lambda "test" $ lambda "cons" $ lambda "alt" $
    inject TS._Expression TS._Expression_conditional $
      record TS._ConditionalExpression [
        TS._ConditionalExpression_test>>: var "test",
        TS._ConditionalExpression_consequent>>: var "cons",
        TS._ConditionalExpression_alternate>>: var "alt"]

-- | `\g -> g`. Identity getter for `analyzeFunctionTerm` when env == Graph.
-- Defined as a top-level helper (with explicit type) so the kernel-to-host
-- translation can resolve it; inline anonymous lambdas trip the Java/Python
-- coders' "untyped term variable" check when the surrounding HOF (here,
-- `Analysis.analyzeFunctionTerm`) is polymorphic.
tsEnvGetGraph :: TypedTermDefinition (Graph -> Graph)
tsEnvGetGraph = def "tsEnvGetGraph" $ lambda "g" $ var "g"

-- | `\new old -> new`. setter for `analyzeFunctionTerm` when env == Graph.
tsEnvSetGraph :: TypedTermDefinition (Graph -> Graph -> Graph)
tsEnvSetGraph = def "tsEnvSetGraph" $ lambda "newG" $ lambda "_old" $ var "newG"

-- | Escape a Hydra string for embedding as a double-quoted TypeScript string
-- literal. Handles backslash, double-quote, newline, carriage return, tab,
-- backspace, and formfeed; other characters pass through verbatim (which
-- keeps multibyte Unicode intact).
tsEscapeString :: TypedTermDefinition (String -> String)
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

-- | A bare identifier expression like `x`.
tsExprIdent :: TypedTermDefinition (String -> TS.Expression)
tsExprIdent = def "tsExprIdent" $
  lambda "s" $ inject TS._Expression TS._Expression_identifier (tsIdent @@ var "s")

-- | A string-literal expression. Embeds the value verbatim through
-- `TS.StringLiteral`; serde handles escaping.
tsExprStr :: TypedTermDefinition (String -> TS.Expression)
tsExprStr = def "tsExprStr" $
  lambda "s" $
    inject TS._Expression TS._Expression_literal $
      inject TS._Literal TS._Literal_string $
        record TS._StringLiteral [
          TS._StringLiteral_value>>: var "s",
          TS._StringLiteral_singleQuote>>: boolean False]

-- | Wrap a String into a TypeScript Identifier.
tsIdent :: TypedTermDefinition (String -> TS.Identifier)
tsIdent = def "tsIdent" $
  lambda "s" $ wrap TS._Identifier (var "s")

-- | `obj.prop` member access.
tsMember :: TypedTermDefinition (TS.Expression -> String -> TS.Expression)
tsMember = def "tsMember" $
  lambda "obj" $ lambda "prop" $
    inject TS._Expression TS._Expression_member $
      record TS._MemberExpression [
        TS._MemberExpression_object>>: var "obj",
        TS._MemberExpression_property>>: tsExprIdent @@ var "prop",
        TS._MemberExpression_computed>>: boolean False,
        TS._MemberExpression_optional>>: boolean False]

-- | A bare named type reference like `Foo`.
tsNamedType :: TypedTermDefinition (String -> TS.TypeExpression)
tsNamedType = def "tsNamedType" $
  lambda "n" $
    inject TS._TypeExpression TS._TypeExpression_identifier (tsIdent @@ var "n")

-- | `new C(arg, ...)`.
tsNew :: TypedTermDefinition (TS.Expression -> [TS.Expression] -> TS.Expression)
tsNew = def "tsNew" $
  lambda "callee" $ lambda "args" $
    inject TS._Expression TS._Expression_new $
      record TS._CallExpression [
        TS._CallExpression_callee>>: var "callee",
        TS._CallExpression_arguments>>: var "args",
        TS._CallExpression_optional>>: boolean False]

-- | `{ k1: v1, k2: v2, ... }` object literal. Keys are identifier-style;
-- callers that need string-keyed properties can build a Property directly.
tsObject :: TypedTermDefinition ([(String, TS.Expression)] -> TS.Expression)
tsObject = def "tsObject" $
  lambda "props" $
    inject TS._Expression TS._Expression_object $
      Lists.map
        (lambda "kv" $
          "k" <~ Pairs.first (var "kv") $
          "v" <~ Pairs.second (var "kv") $
          record TS._Property [
            TS._Property_key>>: tsExprIdent @@ var "k",
            TS._Property_value>>: var "v",
            TS._Property_kind>>: inject TS._PropertyKind TS._PropertyKind_init unit,
            TS._Property_computed>>: boolean False,
            TS._Property_shorthand>>: boolean False])
        (var "props")

-- | A generic type parameter with no constraint.
tsParam :: TypedTermDefinition (String -> TS.TypeParameter)
tsParam = def "tsParam" $
  lambda "n" $
    record TS._TypeParameter [
      TS._TypeParameter_name>>: tsIdent @@ var "n",
      TS._TypeParameter_constraint>>: nothing,
      TS._TypeParameter_default>>: nothing]

-- | A parameterized type `Name<T>`.
tsParamApp1 :: TypedTermDefinition (String -> TS.TypeExpression -> TS.TypeExpression)
tsParamApp1 = def "tsParamApp1" $
  lambda "n" $ lambda "arg" $
    inject TS._TypeExpression TS._TypeExpression_parameterized $
      record TS._ParameterizedTypeExpression [
        TS._ParameterizedTypeExpression_base>>: tsNamedType @@ var "n",
        TS._ParameterizedTypeExpression_arguments>>: list [var "arg"]]

-- | A parameterized type `Name<K, V>`.
tsParamApp2 :: TypedTermDefinition (String -> TS.TypeExpression -> TS.TypeExpression -> TS.TypeExpression)
tsParamApp2 = def "tsParamApp2" $
  lambda "n" $ lambda "a" $ lambda "b" $
    inject TS._TypeExpression TS._TypeExpression_parameterized $
      record TS._ParameterizedTypeExpression [
        TS._ParameterizedTypeExpression_base>>: tsNamedType @@ var "n",
        TS._ParameterizedTypeExpression_arguments>>: list [var "a", var "b"]]

-- | Wrap an Expression in `ExpressionParenthesized` when its serialized form
-- needs grouping. Used by the term encoder for arrow-function bodies and
-- nested calls; cheaper than re-deriving precedence at every site.
tsParen :: TypedTermDefinition (TS.Expression -> TS.Expression)
tsParen = def "tsParen" $
  lambda "e" $ inject TS._Expression TS._Expression_parenthesized (var "e")

-- | A readonly property signature. The name is sanitized for TS reserved
-- words (so a Hydra field named `case` becomes `case_`).
tsPropSig :: TypedTermDefinition (String -> Bool -> TS.TypeExpression -> TS.PropertySignature)
tsPropSig = def "tsPropSig" $
  lambda "name" $ lambda "optional" $ lambda "typ" $
    tsPropSigWithDoc @@ var "name" @@ var "optional" @@ var "typ" @@ nothing

-- | A readonly property signature with an optional JSDoc comment that gets
-- emitted above the property line. Used for record-interface fields where
-- the field has a description annotation in the kernel module.
tsPropSigWithDoc :: TypedTermDefinition (String -> Bool -> TS.TypeExpression -> Maybe TS.DocumentationComment -> TS.PropertySignature)
tsPropSigWithDoc = def "tsPropSigWithDoc" $
  lambda "name" $ lambda "optional" $ lambda "typ" $ lambda "mcomments" $
    "safe" <~ (Formatting.sanitizeWithUnderscores
      @@ TypeScriptLanguageSource.typeScriptReservedWords
      @@ var "name") $
    record TS._PropertySignature [
      TS._PropertySignature_name>>: tsIdent @@ var "safe",
      TS._PropertySignature_type>>: var "typ",
      TS._PropertySignature_optional>>: var "optional",
      TS._PropertySignature_readonly>>: boolean True,
      TS._PropertySignature_comments>>: var "mcomments"]

-- =============================================================================
-- Literal-type encoding
-- =============================================================================

-- | A `ReadonlyMap<K, V>`.
tsReadonlyMap :: TypedTermDefinition (TS.TypeExpression -> TS.TypeExpression -> TS.TypeExpression)
tsReadonlyMap = def "tsReadonlyMap" $
  lambda "k" $ lambda "v" $ tsParamApp2 @@ string "ReadonlyMap" @@ var "k" @@ var "v"

-- | A `ReadonlySet<T>`.
tsReadonlySet :: TypedTermDefinition (TS.TypeExpression -> TS.TypeExpression)
tsReadonlySet = def "tsReadonlySet" $
  lambda "t" $ tsParamApp1 @@ string "ReadonlySet" @@ var "t"

-- | A tuple type `[A, B, ...]`.
tsTuple :: TypedTermDefinition ([TS.TypeExpression] -> TS.TypeExpression)
tsTuple = def "tsTuple" $
  lambda "ts" $
    inject TS._TypeExpression TS._TypeExpression_tuple (var "ts")

-- | Build a typed-identifier `Pattern` (`name: T`). Used for function
-- parameters where the parameter's domain type is known.
tsTypedIdent :: TypedTermDefinition (String -> TS.TypeExpression -> TS.Pattern)
tsTypedIdent = def "tsTypedIdent" $
  lambda "name" $ lambda "typ" $
    inject TS._Pattern TS._Pattern_typed $
      record TS._TypedPattern [
        TS._TypedPattern_pattern>>:
          inject TS._Pattern TS._Pattern_identifier (tsIdent @@ var "name"),
        TS._TypedPattern_type>>: var "typ"]

-- | The `undefined` value as an Expression.
tsUndefined :: TypedTermDefinition TS.Expression
tsUndefined = def "tsUndefined" $ tsExprIdent @@ string "undefined"
