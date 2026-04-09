-- | Go code generator for Hydra.
-- Converts Hydra modules (type and term definitions) into Go source code.
-- Uses an incremental import-tracking strategy: each encoding function accumulates
-- the Go imports it needs, and the final module assembly reads the collected set.

module Hydra.Go.Coder (
  moduleToGo,
  goLanguage,
) where

import Hydra.Kernel hiding (pure)
import qualified Hydra.Core as Core
import qualified Hydra.Go.Language as GoLang
import qualified Hydra.Go.Syntax as Go
import Hydra.Sources.Go.Serde (writeModule)
import qualified Hydra.Lib.Strings as Strings

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- ============================================================================
-- Import tracking
-- ============================================================================

-- | State threaded through encoding to collect Go imports incrementally.
data GoState = GoState {
  goStateImports :: S.Set String,       -- ^ Go import paths accumulated during encoding
  goStateCurrentNs :: Namespace,        -- ^ Current module's namespace
  goStateNsMap :: M.Map Namespace String, -- ^ Map from Hydra namespace to Go package path
  goStateTypeSubst :: M.Map Core.Name Core.Type, -- ^ Type variable substitutions for generics
  goStateInTypeDef :: Bool, -- ^ True when encoding type definitions (keep type vars as params)
  goStateFuncTypeParams :: S.Set Core.Name, -- ^ Forall-bound type vars of the current generic function
  goStateVarTypes :: M.Map String Bool, -- ^ Tracks Go type of local vars: True = any, False = concrete
  goStateFuncParamTypes :: M.Map String Core.Type, -- ^ First param type of let-bound function bindings
  goStateCallableVars :: S.Set String, -- ^ Variables known to be callable (function-typed params)
  goStateExpectedType :: Maybe Core.Type, -- ^ Expected type for the current expression (bidirectional)
  goStateTermParamTypes :: M.Map Core.Name Core.Type -- ^ Hydra types of function params (for type inference)
}

-- | Result of encoding: a value paired with updated state.
type GoResult a = Either (InContext Error) (a, GoState)

addImport :: String -> GoState -> GoState
addImport imp st = st { goStateImports = S.insert imp (goStateImports st) }

-- | Create the initial state for a module.
initState :: Module -> GoState
initState mod_ = GoState {
  goStateImports = S.empty,
  goStateCurrentNs = moduleNamespace mod_,
  goStateNsMap = buildNsMap mod_,
  goStateTypeSubst = M.empty,
  goStateInTypeDef = False,
  goStateVarTypes = M.empty,
  goStateFuncParamTypes = M.empty,
  goStateCallableVars = S.empty,
  goStateFuncTypeParams = S.empty,
  goStateExpectedType = Nothing,
  goStateTermParamTypes = M.empty
}

-- | Build a mapping from Hydra namespaces to Go import paths.
-- Hydra namespace "hydra.core" maps to Go package path "hydra/core".
buildNsMap :: Module -> M.Map Namespace String
buildNsMap mod_ = M.fromList $ fmap toEntry allNs
  where
    allNs = moduleNamespace mod_ : moduleTermDependencies mod_ ++ moduleTypeDependencies mod_
    toEntry ns@(Namespace name) = (ns, namespaceToGoPath name)

-- | Go module path. Must contain a dot to avoid confusion with the Go standard library.
goModulePath :: String
goModulePath = "hydra.dev"

namespaceToGoPath :: String -> String
namespaceToGoPath ns = goModulePath ++ "/" ++ fmap (\c -> if c == '.' then '/' else c) ns

-- | Get the Go package alias for a namespace.
-- For two-level namespaces (hydra.core), uses the last segment: core
-- For deeper namespaces (hydra.encode.core), combines parent+child: encodecore
-- This avoids import collisions between e.g. hydra/core and hydra/encode/core.
namespaceToGoPackage :: Namespace -> String
namespaceToGoPackage (Namespace name) =
  let parts = Strings.splitOn "." name
  in case parts of
    -- Single segment (e.g., stdlib "big") → use as-is
    [p] -> p
    -- Two segments (hydra.core, math.big) → last segment
    [_, p] -> sanitizePackageName p
    -- Deeper (hydra.encode.core) → join all non-hydra segments for uniqueness
    -- e.g. hydra.encode.core → encodecore, hydra.encode.json.model → encodejsonmodel
    _ -> let relevant = if head parts == "hydra"
               then drop 1 parts  -- Drop "hydra" prefix
               else parts
         in sanitizePackageName $ concat relevant

-- | Sanitize a Go package name (some Hydra names conflict with Go keywords).
sanitizePackageName :: String -> String
sanitizePackageName "module" = "hmodule"
sanitizePackageName p = p

-- ============================================================================
-- Naming conventions
-- ============================================================================

-- | Convert a Hydra name to a Go exported identifier (PascalCase).
toGoExported :: String -> String
toGoExported = capitalize . sanitizeGoName

-- | Convert a Hydra name to a Go unexported identifier (camelCase, first letter lowercase).
toGoUnexported :: String -> String
toGoUnexported s = case sanitizeGoName s of
  [] -> "_"
  (c:cs) -> C.toLower c : cs

-- | Sanitize a string for use as a Go identifier.
sanitizeGoName :: String -> String
sanitizeGoName = sanitizeWithUnderscores goReservedWordsSet

goReservedWordsSet :: S.Set String
goReservedWordsSet = GoLang.goReservedWords

-- | Extract the local part of a qualified Hydra name.
goLocalName :: Name -> String
goLocalName = localNameOf

-- | Resolve a qualified Hydra Name to a Go type reference, adding imports as needed.
-- If the name is in the current namespace, returns an unqualified Go type name.
-- If cross-namespace, returns a qualified Go type name and records the import.
resolveTypeRef :: Core.Name -> [Go.Type] -> GoState -> (Go.Type, GoState)
resolveTypeRef name targs st =
  let qn = qualifyName name
      local = toGoExported (qualifiedNameLocal qn)
  in case qualifiedNameNamespace qn of
    Nothing -> (goQualTypeName Nothing local targs, st)
    Just ns
      | ns == goStateCurrentNs st -> (goQualTypeName Nothing local targs, st)
      | otherwise ->
        let pkg = namespaceToGoPackage ns
            impPath = case M.lookup ns (goStateNsMap st) of
              Just p -> p
              Nothing -> namespaceToGoPath (unNamespace ns)
        in (goQualTypeName (Just pkg) local targs, addImport impPath st)

-- | Resolve a reference to a type in "hydra.util" (Either, Pair, etc.),
-- handling same-namespace optimization.
resolveUtilType :: String -> [Go.Type] -> GoState -> (Go.Type, GoState)
resolveUtilType name targs st =
  let utilNs = Namespace "hydra.util"
  in if goStateCurrentNs st == utilNs
    then (goQualTypeName Nothing name targs, st)
    else (goQualTypeName (Just "util") name targs, addImport (goModulePath ++ "/hydra/util") st)

-- | Resolve a reference to an expression in "hydra.util" (Left, Right, etc.),
-- handling same-namespace optimization.
resolveUtilExpr :: String -> GoState -> (Go.Expression, GoState)
resolveUtilExpr name st =
  let utilNs = Namespace "hydra.util"
  in if goStateCurrentNs st == utilNs
    then (goNameExpr name, st)
    else (goQualNameExpr "util" name, addImport (goModulePath ++ "/hydra/util") st)

-- | Resolve a qualified Hydra Name to a Go expression reference, adding imports as needed.
-- Unqualified names (local variables) stay unexported (camelCase).
-- Qualified names (element references) become exported (PascalCase).
resolveExprRef :: Core.Name -> GoState -> (Go.Expression, GoState)
resolveExprRef name st =
  let qn = qualifyName name
      localRaw = qualifiedNameLocal qn
  in case qualifiedNameNamespace qn of
    Nothing ->
      -- Local variable (lambda parameter, let binding) — use unexported (camelCase)
      (goNameExpr (toGoUnexported localRaw), st)
    Just ns
      | ns == goStateCurrentNs st ->
        -- Same namespace element reference — use exported (PascalCase)
        (goNameExpr (toGoExported localRaw), st)
      | otherwise ->
        let pkg = namespaceToGoPackage ns
            local = toGoExported localRaw
            impPath = case M.lookup ns (goStateNsMap st) of
              Just p -> p
              Nothing -> namespaceToGoPath (unNamespace ns)
        in (goQualNameExpr pkg local, addImport impPath st)

-- | Extract a TypeName from a Type (for use in composite literals).
typeToTypeName :: Go.Type -> Go.TypeName
typeToTypeName (Go.TypeName_ tn) = tn
typeToTypeName _ = Go.TypeName (Go.QualifiedIdent Nothing (Go.Identifier "any")) []

-- | Construct a Go Identifier.
goIdent :: String -> Go.Identifier
goIdent = Go.Identifier

-- | Construct a Go qualified identifier (possibly with a package prefix).
goQualIdent :: Maybe Go.Identifier -> String -> Go.QualifiedIdent
goQualIdent mpkg name = Go.QualifiedIdent mpkg (goIdent name)

-- | Construct a simple (unqualified) Go type name with no type arguments.
goSimpleTypeName :: String -> Go.Type
goSimpleTypeName name = Go.TypeName_ $ Go.TypeName (goQualIdent Nothing name) []

-- | Construct a qualified Go type name with optional type arguments.
goQualTypeName :: Maybe String -> String -> [Go.Type] -> Go.Type
goQualTypeName mpkg name targs = Go.TypeName_ $ Go.TypeName
  (goQualIdent (goIdent <$> mpkg) name) targs

-- | Construct a Go expression that is a simple name reference.
goNameExpr :: String -> Go.Expression
goNameExpr name = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandName_ $
    Go.OperandName (goQualIdent Nothing name) []

-- | Construct a Go expression that is a qualified name reference.
goQualNameExpr :: String -> String -> Go.Expression
goQualNameExpr pkg name = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandName_ $
    Go.OperandName (goQualIdent (Just $ goIdent pkg) name) []

-- | Construct a function call expression.
goCall :: Go.PrimaryExpr -> [Go.Expression] -> Go.Expression
goCall fun args = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprCall $ Go.CallExpr fun $ Go.Arguments Nothing args False

-- | Construct a call expression from a simple name.
goCallName :: String -> [Go.Expression] -> Go.Expression
goCallName name args = goCall
  (Go.PrimaryExprOperand $ Go.OperandName_ $ Go.OperandName (goQualIdent Nothing name) [])
  args

-- | Construct a selector expression (e.g., x.Field).
goSelector :: Go.PrimaryExpr -> String -> Go.PrimaryExpr
goSelector expr field = Go.PrimaryExprSelector $ Go.SelectorExpr expr (goIdent field)

-- | Construct a field access on a name (e.g., v.Field).
goFieldAccess :: String -> String -> Go.Expression
goFieldAccess varName field = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  goSelector (Go.PrimaryExprOperand $ Go.OperandName_ $
    Go.OperandName (goQualIdent Nothing varName) []) field

-- | Construct a Go string literal expression.
goStringLitExpr :: String -> Go.Expression
goStringLitExpr s = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralBasic $
    Go.BasicLitString $ Go.StringLitInterpreted $ Go.InterpretedStringLit s

-- | Construct a Go integer literal expression.
goIntLitExpr :: Integer -> Go.Expression
goIntLitExpr n = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralBasic $
    Go.BasicLitInt $ Go.IntLit n

-- | Construct a Go float literal expression.
goFloatLitExpr :: Double -> Go.Expression
goFloatLitExpr f = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralBasic $
    Go.BasicLitFloat $ Go.FloatLit f

-- | Construct a Go boolean literal expression.
goBoolLitExpr :: Bool -> Go.Expression
goBoolLitExpr b = goNameExpr (if b then "true" else "false")

-- | Construct a function literal (closure).
goFuncLit :: [Go.ParameterDecl] -> Maybe Go.Result -> [Go.Statement] -> Go.Expression
goFuncLit params result stmts = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralFunction $ Go.FunctionLit
    (Go.Signature (Go.Parameters params) result)
    (Go.FunctionBody $ Go.Block stmts)

-- | Construct a return statement.
goReturn :: [Go.Expression] -> Go.Statement
goReturn exprs = Go.StatementReturn $ Go.ReturnStmt exprs

-- | Construct a short variable declaration statement: name := value
goShortVar :: String -> Go.Expression -> Go.Statement
goShortVar name val = Go.StatementSimple $ Go.SimpleStmtShortVarDecl $
  Go.ShortVarDecl [goIdent name] [val]

-- | Construct a Go struct literal (composite literal with struct type).
goStructLit :: Go.TypeName -> [(String, Go.Expression)] -> Go.Expression
goStructLit tn fields = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralComposite $
    Go.CompositeLit (Go.LiteralTypeName tn) $ Go.LiteralValue $
      fmap (\(k, v) -> Go.KeyedElement (Just $ Go.KeyField $ goIdent k)
                                         (Go.ElementExpression v)) fields

-- | The empty struct literal: struct{}{}
goEmptyStructLit :: Go.Expression
goEmptyStructLit = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralComposite $
    Go.CompositeLit (Go.LiteralTypeStruct $ Go.StructType []) $ Go.LiteralValue []

-- | Construct a Go slice literal.
goSliceLit :: Go.Type -> [Go.Expression] -> Go.Expression
goSliceLit elemType elems = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralComposite $
    Go.CompositeLit (Go.LiteralTypeSlice $ Go.SliceType elemType) $ Go.LiteralValue $
      fmap (\e -> Go.KeyedElement Nothing (Go.ElementExpression e)) elems

-- | Construct a Go map literal.
goMapLit :: Go.Type -> Go.Type -> [(Go.Expression, Go.Expression)] -> Go.Expression
goMapLit kt vt entries = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralComposite $
    Go.CompositeLit (Go.LiteralTypeMap $ Go.MapType kt vt) $ Go.LiteralValue $
      fmap (\(k, v) -> Go.KeyedElement (Just $ Go.KeyExpression k) (Go.ElementExpression v)) entries

-- | The Go pointer type.
goPointerType :: Go.Type -> Go.Type
goPointerType = Go.TypeLiteral . Go.TypeLitPointer . Go.PointerType

-- | The Go slice type.
goSliceType :: Go.Type -> Go.Type
goSliceType = Go.TypeLiteral . Go.TypeLitSlice . Go.SliceType

-- | The Go map type.
goMapType :: Go.Type -> Go.Type -> Go.Type
goMapType k v = Go.TypeLiteral $ Go.TypeLitMap $ Go.MapType k v

-- | The Go function type.
goFuncType :: [Go.Type] -> Go.Type -> Go.Type
goFuncType paramTypes retType = Go.TypeLiteral $ Go.TypeLitFunction $ Go.FunctionType $
  Go.Signature
    (Go.Parameters [Go.ParameterDecl [] False t | t <- paramTypes])
    (Just $ Go.ResultType retType)

-- | The Go struct type.
goStructType :: [(String, Go.Type)] -> Go.Type
goStructType fields = Go.TypeLiteral $ Go.TypeLitStruct $ Go.StructType $
  fmap (\(n, t) -> Go.FieldDeclNamed $ Go.NamedField [goIdent n] t Nothing) fields

-- | The Go empty struct type: struct{}
goEmptyStructType :: Go.Type
goEmptyStructType = Go.TypeLiteral $ Go.TypeLitStruct $ Go.StructType []

-- | The Go interface type (empty for any, or with methods).
goInterfaceType :: [Go.InterfaceElem] -> Go.Type
goInterfaceType = Go.TypeLiteral . Go.TypeLitInterface . Go.InterfaceType

-- | The Go "any" type (empty interface).
goAnyType :: Go.Type
goAnyType = goSimpleTypeName "any"

-- | Construct a Go [2]any{a, b} array literal.
goArrayLit2 :: Go.Expression -> Go.Expression -> Go.Expression
goArrayLit2 a b = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralComposite $
    Go.CompositeLit
      (Go.LiteralTypeArray $ Go.ArrayType (goIntLitExpr 2) goAnyType)
      (Go.LiteralValue
        [ Go.KeyedElement Nothing (Go.ElementExpression a)
        , Go.KeyedElement Nothing (Go.ElementExpression b)])

-- ============================================================================
-- Type encoding
-- ============================================================================

-- | Encode a Hydra literal type as a Go type.
encodeLiteralType :: Core.LiteralType -> GoState -> GoResult Go.Type
encodeLiteralType lt st = case lt of
  Core.LiteralTypeBinary -> pure (goSliceType (goSimpleTypeName "byte"), st)
  Core.LiteralTypeBoolean -> pure (goSimpleTypeName "bool", st)
  Core.LiteralTypeFloat ft -> encodeFloatType ft st
  Core.LiteralTypeInteger it -> encodeIntegerType it st
  Core.LiteralTypeString -> pure (goSimpleTypeName "string", st)

encodeFloatType :: Core.FloatType -> GoState -> GoResult Go.Type
encodeFloatType ft st = case ft of
  Core.FloatTypeBigfloat -> pure (goSimpleTypeName "float64", st)  -- Go has no bigfloat; use float64
  Core.FloatTypeFloat32 -> pure (goSimpleTypeName "float32", st)
  Core.FloatTypeFloat64 -> pure (goSimpleTypeName "float64", st)

encodeIntegerType :: Core.IntegerType -> GoState -> GoResult Go.Type
encodeIntegerType it st = case it of
  Core.IntegerTypeBigint -> pure (goPointerType $ goQualTypeName (Just "big") "Int" [],
    addImport "math/big" st)
  Core.IntegerTypeInt8 -> pure (goSimpleTypeName "int8", st)
  Core.IntegerTypeInt16 -> pure (goSimpleTypeName "int16", st)
  Core.IntegerTypeInt32 -> pure (goSimpleTypeName "int32", st)
  Core.IntegerTypeInt64 -> pure (goSimpleTypeName "int64", st)
  Core.IntegerTypeUint8 -> pure (goSimpleTypeName "uint8", st)
  Core.IntegerTypeUint16 -> pure (goSimpleTypeName "uint16", st)
  Core.IntegerTypeUint32 -> pure (goSimpleTypeName "uint32", st)
  Core.IntegerTypeUint64 -> pure (goSimpleTypeName "uint64", st)

-- | Encode a Hydra type as a Go type.
encodeType :: Context -> Graph -> Core.Type -> GoState -> GoResult Go.Type
encodeType cx g typ st = case deannotateType typ of
  Core.TypeAnnotated at -> encodeType cx g (Core.annotatedTypeBody at) st
  Core.TypeApplication at ->
    -- Check if this is a generic type instantiation (base is a named type)
    let (baseTy, argTys) = collectTypeArgs typ
    in case deannotateType baseTy of
      Core.TypeVariable name -> do
        -- Generic type instantiation: Coder[V1, V2]
        -- Only include type args for non-phantom type params
        let goTypeParamCount = length (lookupForallVars g name)
            effectiveArgs = take goTypeParamCount argTys
        (goArgTypes, st1) <- encodeTypes cx g effectiveArgs st
        let (goTyp, st2) = resolveTypeRef name goArgTypes st1
        pure (goTyp, st2)
      _ ->
        -- Not generic instantiation: strip one application layer (original behavior)
        encodeType cx g (Core.applicationTypeFunction at) st
  Core.TypeUnit -> pure (goEmptyStructType, st)
  Core.TypeLiteral lt -> encodeLiteralType lt st
  -- Collections use any-typed elements to match the term-level representation.
  -- Go's type system cannot express the polymorphism of Hydra collections
  -- in struct field types without creating compilation mismatches with
  -- the any-typed term encoding.
  Core.TypeList _ -> pure (goSliceType goAnyType, st)
  Core.TypeSet _ -> pure (goSliceType goAnyType, st)   -- Sets use []any (deduplicated lists)
  Core.TypeMap _ -> pure (goSliceType goAnyType, st)   -- Maps use []any of [2]any{k,v} pairs
  Core.TypeMaybe _ -> pure (goAnyType, st)  -- nil for Nothing, value for Just
  Core.TypeEither _ -> pure (goAnyType, st)  -- Either is erased to any at the Go type level
  Core.TypePair _ -> pure (goAnyType, st)    -- Pair is erased to any at the Go type level
  Core.TypeFunction ft -> do
    (dom, st1) <- encodeType cx g (Core.functionTypeDomain ft) st
    (cod, st2) <- encodeType cx g (Core.functionTypeCodomain ft) st1
    pure (goFuncType [dom] cod, st2)
  Core.TypeRecord rt -> do
    (fields, st') <- encodeStructFields cx g rt st
    pure (Go.TypeLiteral $ Go.TypeLitStruct $ Go.StructType fields, st')
  Core.TypeUnion _ -> pure (goAnyType, st)  -- Anonymous unions become any
  Core.TypeWrap wt -> encodeType cx g wt st  -- Anonymous wraps unwrap
  Core.TypeVariable name ->
    let qn = qualifyName name
    in case qualifiedNameNamespace qn of
      Nothing ->
        -- Unqualified type variable: check substitution map first
        case M.lookup name (goStateTypeSubst st) of
          Just concreteType -> do
            -- Temporarily remove this var from the substitution to prevent infinite
            -- loops during encoding of the substituted type, then restore it.
            let st' = st { goStateTypeSubst = M.delete name (goStateTypeSubst st) }
            (goTyp, st'') <- encodeType cx g concreteType st'
            pure (goTyp, st'' { goStateTypeSubst = goStateTypeSubst st })
          Nothing ->
            -- Not in substitution: check if it's a type definition context,
            -- or a forall-bound type param of the current generic function.
            if goStateInTypeDef st || S.member name (goStateFuncTypeParams st)
              then let (goTyp, st') = resolveTypeRef name [] st
                   in pure (goTyp, st')
              else pure (goAnyType, st)  -- Unresolved type var → any
      _ -> do
        -- Qualified type name: resolve with type args from substitution
        let resolvedArgs = resolveTypeArgs g name st
        (goTypeArgs, st') <- encodeTypes cx g resolvedArgs st
        let (goTyp, st'') = resolveTypeRef name goTypeArgs st'
        pure (goTyp, st'')
  Core.TypeForall fa -> encodeType cx g (Core.forallTypeBody fa) st

-- ============================================================================
-- Literal encoding
-- ============================================================================

-- | Encode a Hydra literal as a Go expression.
encodeLiteral :: Core.Literal -> GoState -> GoResult Go.Expression
encodeLiteral lit st = case lit of
  Core.LiteralBoolean b -> pure (goBoolLitExpr b, st)
  Core.LiteralString s -> pure (goStringLitExpr s, st)
  Core.LiteralFloat fv -> encodeFloatValue fv st
  Core.LiteralInteger iv -> encodeIntegerValue iv st
  Core.LiteralBinary _ -> pure (goSliceLit (goSimpleTypeName "byte") [], st)

encodeFloatValue :: Core.FloatValue -> GoState -> GoResult Go.Expression
encodeFloatValue fv st = case fv of
  Core.FloatValueFloat32 f -> pure (goFloatLitExpr (realToFrac f), st)
  Core.FloatValueFloat64 f -> pure (goFloatLitExpr f, st)
  Core.FloatValueBigfloat f -> pure (goFloatLitExpr f, st)

encodeIntegerValue :: Core.IntegerValue -> GoState -> GoResult Go.Expression
encodeIntegerValue iv st = case iv of
  Core.IntegerValueBigint i -> do
    let st' = addImport "math/big" st
    if i >= -2^63 && i < 2^63
      then do
        -- Fits in int64: big.NewInt(n)
        let newIntExpr = Go.PrimaryExprOperand $ Go.OperandName_ $
              Go.OperandName (goQualIdent (Just $ goIdent "big") "NewInt") []
        pure (goCall newIntExpr [goIntLitExpr i], st')
      else do
        -- Overflow: func() *big.Int { n, _ := new(big.Int).SetString("...", 10); return n }()
        -- Simplified: use a helper that constructs from string
        let setStringCall = goCallName "func() any { n, _ := new(big.Int).SetString"
              [goStringLitExpr (show i), goIntLitExpr 10]
        -- Actually, just generate a func literal IIFE
        pure (goNameExpr ("func() *big.Int { n, _ := new(big.Int).SetString(\"" ++ show i ++ "\", 10); return n }()"), st')
  Core.IntegerValueInt8 i -> pure (goIntLitExpr (fromIntegral i), st)
  Core.IntegerValueInt16 i -> pure (goIntLitExpr (fromIntegral i), st)
  Core.IntegerValueInt32 i -> pure (goIntLitExpr (fromIntegral i), st)
  Core.IntegerValueInt64 i -> pure (goIntLitExpr (fromIntegral i), st)
  Core.IntegerValueUint8 i -> pure (goIntLitExpr (fromIntegral i), st)
  Core.IntegerValueUint16 i -> pure (goIntLitExpr (fromIntegral i), st)
  Core.IntegerValueUint32 i -> pure (goIntLitExpr (fromIntegral i), st)
  Core.IntegerValueUint64 i -> pure (goIntLitExpr (fromIntegral i), st)

-- ============================================================================
-- Term encoding
-- ============================================================================

-- | Encode a Hydra term as a Go expression.
-- If goStateExpectedType is set, use it to build local type substitutions.
encodeTerm :: Context -> Graph -> Core.Term -> GoState -> GoResult Go.Expression
encodeTerm cx g term st0 = do
  -- Apply expected type to build local substitutions
  let st = case goStateExpectedType st0 of
        Just expectedType ->
          let localSubst = extractSubstFromType g expectedType
              -- Keep expected type only for terms whose encoding consumes it
              -- (lambdas need it for parameter domain inference; let bodies
              -- propagate it to the body expression, etc.)
              keepExpected = case deannotateTerm term of
                Core.TermLambda _ -> True
                Core.TermApplication _ -> True  -- call result should match expected type
                Core.TermTypeApplication _ -> True
                Core.TermTypeLambda _ -> True
                Core.TermAnnotated _ -> True
                Core.TermLet _ -> True  -- let body should match expected type
                _ -> False
          in if M.null localSubst
             then if keepExpected then st0 else st0 { goStateExpectedType = Nothing }
             else st0 { goStateTypeSubst = M.union localSubst (goStateTypeSubst st0),
                         goStateExpectedType = if keepExpected then goStateExpectedType st0 else Nothing }
        Nothing -> st0
  encodeTermInner cx g term st

-- | Inner term encoding (after expected type processing).
encodeTermInner :: Context -> Graph -> Core.Term -> GoState -> GoResult Go.Expression
encodeTermInner cx g term st = case term of
  Core.TermAnnotated at -> encodeTerm cx g (Core.annotatedTermBody at) st
  Core.TermApplication app -> do
    -- Uncurry nested applications: Application(Application(f, a), b) -> f(a, b)
    -- But keep curried form for primitive function calls (which are curried in Go)
    let (funTerm, argTerms) = collectApplicationArgs term
        -- Also collect type args from TypeApplication wrappers on the function.
        -- collectApplicationArgs strips TypeApplication, so check the ORIGINAL function chain.
        funTypeArgs = collectFuncTypeArgs term
        bareFun = funTerm
    -- Special case: elimination applied to argument — pass marg through
    case (deannotateTerm funTerm, argTerms) of
      -- Default-only union elimination (constant function): just return default
      (Core.TermCases cs, _)
        | null (Core.caseStatementCases cs) ->
          case Core.caseStatementDefault cs of
            Just dt -> encodeTerm cx g dt st
            Nothing -> pure (goNameExpr "nil", st)
      -- Record projection applied to an any-typed argument: emit arg.(Type).Field directly
      -- This preserves the field's concrete type instead of wrapping in func(any) any.
      -- Only do this when the arg produces any (concrete args can't be type-asserted).
      (Core.TermProject proj, [arg])
        | producesAny st arg ->
          encodeProjection cx g proj (Just arg) st
      -- Unwrap applied to an any-typed argument: pass through
      (Core.TermUnwrap wrapName0, [arg])
        | producesAny st arg ->
          encodeUnwrap cx g wrapName0 (Just arg) st
      -- Unwrap of function-typed wrap applied to multiple args:
      -- e.g., Application(Application(unwrap Parser, p1), input)
      -- For function-typed wraps with type params, generate type conversion + call:
      -- func(string) ParseResult[T0](p1)(input)
      (Core.TermUnwrap wrapName, (wrappedVal:callArgs))
        | not (null callArgs) -> do
          let wrapUnder = lookupWrapUnderlyingType g wrapName
          case wrapUnder of
            Just underType -> do
              -- Build a LOCAL substitution from the wrapped value's type.
              -- E.g., if wrappedVal is pa :: Parser[T0], extract {a → T0}
              -- so the underlying type func(string) ParseResult[a] becomes
              -- func(string) ParseResult[T0], NOT func(string) ParseResult[func(T0) T1].
              let valType = inferTermType g wrappedVal st
                  localSubst = case valType of
                    Just vt -> extractSubstFromType g vt
                    Nothing -> M.empty
                  stForUnder = if M.null localSubst then st
                    else st { goStateTypeSubst = M.union localSubst (goStateTypeSubst st) }
              (goUnderType, st1) <- encodeTypeForTerm cx g underType stForUnder
              -- Restore substitution after encoding underlying type
              let st1' = st1 { goStateTypeSubst = goStateTypeSubst st }
              (wrappedExpr, st2) <- encodeTerm cx g wrappedVal st1'
              -- If the underlying type is a function with specific types (not func(any) any),
              -- generate a Go type conversion: UnderType(wrappedExpr)(callArgs...)
              if isFuncGoType goUnderType && goUnderType /= goFuncType [goAnyType] goAnyType
                then do
                  let converted = Go.ExpressionUnary $ Go.UnaryExprPrimary $
                        Go.PrimaryExprConversion $ Go.Conversion goUnderType wrappedExpr
                  -- Coerce call args to the underlying function's param types
                  let underParamTypes = case deannotateType underType of
                        Core.TypeFunction ft -> [Core.functionTypeDomain ft]
                        Core.TypeWrap wt -> case deannotateType wt of
                          Core.TypeFunction ft -> [Core.functionTypeDomain ft]
                          _ -> []
                        _ -> []
                  (callArgExprs, st3) <- encodeAndCoerceArgs cx g callArgs underParamTypes st2
                  pure (goCall (exprToPrimary converted) callArgExprs, st3)
                else do
                  -- Non-function wrap: fall through to curried call handler
                  (callArgExprs, st3) <- encodeTermList cx g callArgs st2
                  let firstCall = goCall (exprToPrimary wrappedExpr) [head callArgExprs]
                      curried = L.foldl' (\acc arg ->
                        goCall (goTypeAssertFunc $ exprToPrimary acc) [arg])
                        firstCall (tail callArgExprs)
                  pure (curried, st3)
            Nothing -> do
              -- Unknown wrap: fall through to curried call handler
              (funExpr, st1) <- encodeTerm cx g funTerm st
              (argExprs, st2) <- encodeTermList cx g argTerms st1
              let firstCall = goCall (exprToPrimary funExpr) [head argExprs]
                  curried = L.foldl' (\acc arg ->
                    goCall (goTypeAssertFunc $ exprToPrimary acc) [arg])
                    firstCall (tail argExprs)
              pure (curried, st2)
      _ -> if isCurriedRef funTerm
      then do
        -- Curried call: f(a).(func(any) any)(b)
        -- Used for primitives, local variables, and lambdas
        --
        -- For IIFE patterns (inline lambda applied to args), set expected type
        -- from the first arg's type to resolve the lambda's domain type vars.
        let argExpectedType = case argTerms of
              (firstArg:_) -> inferTermType g firstArg st
              _ -> Nothing
            -- Set expected type AND directly merge the substitution
            argSubst = case argExpectedType of
              Just et -> extractSubstFromType g et
              Nothing -> M.empty
            stForFun = case argExpectedType of
              Just et | not (M.null argSubst) ->
                st { goStateExpectedType = Just et, goStateTypeSubst = argSubst }
              Just et -> st { goStateExpectedType = Just et }
              Nothing -> st
        (funExpr, st1) <- encodeTerm cx g funTerm stForFun
        -- For non-callable terms, just return the term (drop arguments).
        -- Also check: local variables that we know are not function-typed.
        -- Check if the function is a local variable with a known concrete non-function type.
        -- Such variables (string, struct, etc.) can't be called.
        let isNonCallableVar = case deannotateTerm funTerm of
              Core.TermVariable name ->
                let n = toGoUnexported (unName name)
                    isLocal = case qualifiedNameNamespace (qualifyName name) of
                      Nothing -> True
                      Just _ -> False
                    -- concrete type AND not a known function (check both tracking maps)
                    isConcrete = M.lookup n (goStateVarTypes st1) == Just False
                    isFunc = M.member n (goStateFuncParamTypes st1)
                             || S.member n (goStateCallableVars st1)
                in isLocal && isConcrete && not isFunc
              _ -> False
        case deannotateTerm funTerm of
          _ | isNonCallableVar -> pure (funExpr, st1)
          Core.TermLiteral _ -> pure (funExpr, st1)
          Core.TermList _ -> pure (funExpr, st1)
          Core.TermEither _ -> pure (funExpr, st1)
          Core.TermPair _ -> pure (funExpr, st1)
          Core.TermRecord _ -> pure (funExpr, st1)
          Core.TermUnion _ -> pure (funExpr, st1)
          Core.TermMap _ -> pure (funExpr, st1)
          Core.TermSet _ -> pure (funExpr, st1)
          Core.TermUnit -> pure (funExpr, st1)
          Core.TermMaybe _ -> pure (funExpr, st1)
          _ -> do
            -- Look up the first param type for local function calls
            let mFuncParamType = case deannotateTerm funTerm of
                  Core.TermVariable name ->
                    M.lookup (toGoUnexported $ unName name) (goStateFuncParamTypes st1)
                  _ -> Nothing
            case argTerms of
              [] -> pure (funExpr, st1)
              (firstArg:restArgs) -> do
                (firstExpr, st2) <- encodeTerm cx g firstArg st1
                -- Coerce the first argument if we know the function's param type
                (coercedFirst, st2') <- case mFuncParamType of
                  Just pt | producesAny st2 firstArg ->
                    coerceToType cx g firstExpr pt firstArg st2
                  _ -> pure (firstExpr, st2)
                (restExprs, st3) <- encodeTermList cx g restArgs st2'
                -- Check if the function is a callable var (returns concrete, not any)
                let isCallableFun = case deannotateTerm funTerm of
                      Core.TermVariable name ->
                        S.member (toGoUnexported (unName name)) (goStateCallableVars st)
                      _ -> False
                    firstCall = goCall (exprToPrimary funExpr) [coercedFirst]
                case restExprs of
                  [] -> pure (firstCall, st3)
                  _ ->
                    -- For callable vars, the intermediate result IS a function;
                    -- no .(func(any) any) assertion needed. For others, assert.
                    let curried = L.foldl' (\acc arg ->
                          if isCallableFun
                            then goCall (exprToPrimary acc) [arg]
                            else goCall (goTypeAssertFunc $ exprToPrimary acc) [arg])
                          firstCall restExprs
                    in pure (curried, st3)
      else do
        -- Multi-arg call for top-level generated functions: f(a, b, c)
        -- But if the "function" is a literal or non-callable term, just return it
        -- Save the expected type before encoding the function (it may get cleared)
        let savedExpectedType = goStateExpectedType st
        (rawFunExpr, st1) <- encodeTerm cx g funTerm st
        -- If the function has type args and is qualified, add Go type instantiation
        -- Only add type instantiation for qualified generic functions
        -- Check if the called function has Go-visible type params
        let calledSchemeVars0 = lookupCalledSchemeVars g funTerm
            calledParamTypes0 = lookupFunctionParamTypes g funTerm
            calledRetType0 = case stripTermWrappers funTerm of
              Core.TermVariable fn -> case M.lookup fn (graphBoundTypes g) of
                Just ts -> let (_, it) = unpackForallType (Core.typeSchemeType ts)
                               (_, rt) = unpackFunctionType it
                           in rt
                Nothing -> Core.TypeVariable (Core.Name "_")
              _ -> Core.TypeVariable (Core.Name "_")
            calledHasTypeParams = any (\v -> typeVarNeedsGoParam g v (calledParamTypes0 ++ [calledRetType0])) calledSchemeVars0
        (funExpr, st1a) <- if not (null funTypeArgs) && calledHasTypeParams && isQualifiedName (case deannotateTerm bareFun of { Core.TermVariable n -> n; _ -> Core.Name "" })
          then case funTypeArgs of
            [singleArg] -> do
              (goArg, st') <- encodeTypeForTerm cx g singleArg st1
              -- Only generate instantiation for simple type names (T0, core.Type, etc.)
              -- Composite types ([]any, func...) can't be represented in IndexExpr
              if isSimpleGoType goArg
                then do
                  let typeExpr = typeToExpr goArg
                  pure (Go.ExpressionUnary $ Go.UnaryExprPrimary $
                    Go.PrimaryExprIndex $ Go.IndexExpr
                      (exprToPrimary rawFunExpr) typeExpr, st')
                else pure (rawFunExpr, st')
            _ -> pure (rawFunExpr, st1)
          else pure (rawFunExpr, st1)
        case deannotateTerm funTerm of
          Core.TermLiteral _ -> pure (funExpr, st1a)
          Core.TermList _ -> pure (funExpr, st1a)
          _ -> do
            -- Look up function param types and alpha-rename the called function's
            -- type variables to avoid collisions with the enclosing scope.
            let rawParamTypes = lookupFunctionParamTypes g funTerm
                -- Get the called function's scheme vars and create fresh names
                -- to avoid collisions with the enclosing scope's type vars.
                calledSchemeVars = lookupCalledSchemeVars g funTerm
                renaming = M.fromList [(v, Core.TypeVariable $ Core.Name (unName v ++ "_"))
                  | v <- calledSchemeVars, S.member v (goStateFuncTypeParams st1a)]
                -- Alpha-rename param types to use fresh names
                renamedParamTypes = if M.null renaming then rawParamTypes
                  else fmap (applyTypeSubst renaming) rawParamTypes
                -- Infer actual type args from the arguments.
                -- argTypeSubst maps ORIGINAL scheme vars to actual types.
                argTypeSubst = inferArgTypeSubst g funTerm argTerms st1a
                -- Also infer return type var from expected type (saved before function encoding)
                retTypeSubst = case savedExpectedType of
                  Just expectedRet -> inferRetTypeSubst g funTerm calledSchemeVars expectedRet
                  Nothing -> M.empty
                -- Combine all inferred substitutions
                allInferred = M.union argTypeSubst retTypeSubst
                -- Rename the keys to match the renamed param types
                renamedInferred = M.fromList
                  [(Core.Name (unName k ++ "_"), v) | (k, v) <- M.toList allInferred,
                    S.member k (goStateFuncTypeParams st1)]
                  `M.union` allInferred
                -- Apply to get final resolved param types
                resolvedParamTypes = if M.null renamedInferred then renamedParamTypes
                  else fmap (applyTypeSubst renamedInferred) renamedParamTypes
            (argExprs, st2) <- encodeAndCoerceArgs cx g argTerms resolvedParamTypes st1a
            -- Restore substitution
            let st2' = st2 { goStateTypeSubst = goStateTypeSubst st1a }
            pure (goCall (exprToPrimary funExpr) argExprs, st2')
  Core.TermEither e -> case e of
    Left l -> do
      (le, st') <- encodeTerm cx g l st
      -- [2]any{"left", value} — matches the eithers lib representation
      pure (goArrayLit2 (goStringLitExpr "left") le, st')
    Right r -> do
      (re, st') <- encodeTerm cx g r st
      pure (goArrayLit2 (goStringLitExpr "right") re, st')
  Core.TermLambda lam -> encodeLambda cx g lam st
  Core.TermProject proj -> encodeProjection cx g proj Nothing st
  Core.TermCases cs -> encodeCases cx g cs Nothing st
  Core.TermUnwrap wname -> encodeUnwrap cx g wname Nothing st
  Core.TermLet lt -> do
    let bindings = Core.letBindings lt
        body = Core.letBody lt
        -- Pre-compute type substitutions for bindings that have TypeLambda params.
        -- For each binding with TypeLambdas, scan the let body (and other bindings)
        -- for TypeApplication args, and build a substitution mapping TypeLambda params
        -- to the actual type args used at call sites.
        bindingTypeSubsts = M.fromList $ concatMap (\b ->
          let bname = Core.bindingName b
              tlParams = extractTypeLambdaParams (Core.bindingTerm b)
          in if null tlParams then []
             else case findTypeApplicationArgs bname body of
               Just typeArgs ->
                 -- Map each TypeLambda param to the corresponding TypeApplication arg
                 [(param, arg) | (param, arg) <- zip tlParams typeArgs]
               Nothing -> []) bindings
        stWithBindingSubst = if M.null bindingTypeSubsts then st
          else st { goStateTypeSubst = M.union bindingTypeSubsts (goStateTypeSubst st) }
    (stmts, st1) <- encodeBindings cx g bindings stWithBindingSubst
    (bodyExpr, st2) <- encodeTerm cx g body st1
    -- In Go, let bindings become short variable declarations in a block,
    -- wrapped in an immediately-invoked function literal that returns the body.
    let allStmts = stmts ++ [goReturn [bodyExpr]]
        retType = goAnyType  -- We use any as return type for now
    pure (goCall
      (Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralFunction $ Go.FunctionLit
        (Go.Signature (Go.Parameters []) (Just $ Go.ResultType retType))
        (Go.FunctionBody $ Go.Block allStmts))
      [], st2)
  Core.TermList els -> do
    (goEls, st') <- encodeTermList cx g els st
    -- Use []any{...} for heterogeneous lists; specific types where inference works
    pure (goSliceLit goAnyType goEls, st')
  Core.TermLiteral lit -> encodeLiteral lit st
  Core.TermMap m -> do
    -- Maps are encoded as []any of [2]any{key, value} pairs (association lists)
    let entries = M.toList m
    (goEntries, st') <- encodeMapEntries cx g entries st
    let pairExprs = fmap (\(k, v) -> goArrayLit2 k v) goEntries
    pure (goSliceLit goAnyType pairExprs, st')
  Core.TermMaybe mt -> case mt of
    Nothing -> pure (goNameExpr "nil", st)
    Just val -> do
      (ve, st') <- encodeTerm cx g val st
      -- func() any { v := val; return &v }() — temp var for addressability
      pure (goCall (Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralFunction $ Go.FunctionLit
        (Go.Signature (Go.Parameters []) (Just $ Go.ResultType goAnyType))
        (Go.FunctionBody $ Go.Block [
          goShortVar "_v" ve,
          goReturn [Go.ExpressionUnary $ Go.UnaryExprOp $
            Go.UnaryOperation Go.UnaryOpAddressOf
              (Go.UnaryExprPrimary $ Go.PrimaryExprOperand $ Go.OperandName_ $
                Go.OperandName (goQualIdent Nothing "_v") [])]])) [], st')
  Core.TermPair p -> do
    (fe, st1) <- encodeTerm cx g (fst p) st
    (se, st2) <- encodeTerm cx g (snd p) st1
    -- [2]any{first, second} — matches the pairs lib representation
    pure (goArrayLit2 fe se, st2)
  Core.TermRecord rec -> do
    let tname = Core.recordTypeName rec
        fields = Core.recordFields rec
        -- Resolve type arguments from the current substitution context
        resolvedArgs = resolveTypeArgs g tname st
    (goTypeArgs, st1) <- encodeTypes cx g resolvedArgs st
    let (goTyp, st2) = resolveTypeRef tname goTypeArgs st1
        tn = typeToTypeName goTyp
        -- Build substitution for the record's own type params → field encoding
        fieldSubst = buildTypeSubst g tname resolvedArgs
        stWithSubst = st2 { goStateTypeSubst = M.union fieldSubst (goStateTypeSubst st2) }
        fieldTypes = lookupRecordFieldTypes g tname
    (goFields, st3) <- encodeFieldsTyped cx g fields fieldTypes stWithSubst
    -- Restore substitution after field encoding
    pure (goStructLit tn goFields, st3 { goStateTypeSubst = goStateTypeSubst st })
  Core.TermSet s -> do
    (goEls, st') <- encodeTermList cx g (S.toList s) st
    -- Sets are represented as map[T]struct{}, but for simplicity we use a slice here
    -- TODO: proper set representation
    pure (goSliceLit goAnyType goEls, st')
  Core.TermUnion inj -> do
    let tname = Core.injectionTypeName inj
        field = Core.injectionField inj
        fname = Core.fieldName field
        fterm = Core.fieldTerm field
        goFieldName = toGoExported (unName fname)
        -- Build variant type name: BaseType + VariantName
        variantLocalName = localNameOf tname ++ "." ++ unName fname
        goBaseName = toGoExported (localNameOf tname)
        variantQName = case namespaceOf tname of
          Just (Namespace ns) -> Core.Name (ns ++ "." ++ goBaseName ++ goFieldName)
          Nothing -> Core.Name (goBaseName ++ goFieldName)
        -- Resolve type arguments from the current substitution context
        resolvedArgs = resolveTypeArgs g tname st
    (goTypeArgs, st0) <- encodeTypes cx g resolvedArgs st
    let (variantTyp, st1) = resolveTypeRef variantQName goTypeArgs st0
        variantTn = typeToTypeName variantTyp
    dterm <- pure $ deannotateTerm fterm
    let isUnit = case dterm of
          Core.TermUnit -> True
          Core.TermRecord r -> null (Core.recordFields r)
          _ -> False
    if isUnit
      then pure (goStructLit variantTn [], st1)
      else do
        -- Look up the variant's value type and set up substitution for the inner term
        let variantFieldType = lookupUnionVariantType g tname fname
            -- If the variant type is a type application (e.g., ParserTestCase[Value]),
            -- extract substitution for the inner type's type params
            innerSubst = case variantFieldType of
              Just vtype -> extractSubstFromType g vtype
              Nothing -> M.empty
            stWithInner = st1 { goStateTypeSubst = M.union innerSubst (goStateTypeSubst st1) }
        (ve, st2) <- encodeTerm cx g fterm stWithInner
        (typedVe, st3) <- case variantFieldType of
          Just vtype -> coerceToType cx g ve vtype fterm st2
          Nothing -> pure (ve, st2)
        -- Restore substitution
        pure (goStructLit variantTn [("Value", typedVe)],
              st3 { goStateTypeSubst = goStateTypeSubst st })
  Core.TermUnit -> pure (goEmptyStructLit, st)
  Core.TermVariable name ->
    let (expr, st') = resolveExprRef name st
    in pure (expr, st')
  Core.TermWrap wt -> do
    let tname = Core.wrappedTermTypeName wt
        inner = Core.wrappedTermBody wt
        -- Resolve type arguments from the current substitution context
        resolvedArgs = resolveTypeArgs g tname st
    (goTypeArgs, st0) <- encodeTypes cx g resolvedArgs st
    let (wrapType, st1) = resolveTypeRef tname goTypeArgs st0
    (ie, st2) <- encodeTerm cx g inner st1
    -- Use Go type conversion: WrapType(innerValue)
    -- For any-typed inner values, add a type assertion to the underlying type first
    let goConversion innerExpr = Go.ExpressionUnary $ Go.UnaryExprPrimary $
          Go.PrimaryExprConversion $ Go.Conversion wrapType innerExpr
    let wrapUnder = lookupWrapUnderlyingType g tname
        needsCoerce = producesAny st inner
    case wrapUnder of
      Just underType -> do
        (goUnderType, st3) <- encodeTypeForTerm cx g underType st2
        -- For function types, generate a wrapper func to adapt return types
        -- (function types are not interfaces, so assertion doesn't work).
        -- Always wrap when the underlying type is a function with type params,
        -- since the inner expression returns any but the wrap expects specific types.
        if isFuncGoType goUnderType && goUnderType /= goFuncType [goAnyType] goAnyType
          then do
            let innerDom = funcDomType goUnderType
                innerCod = funcCodType goUnderType
                -- func(_w DomType) CodType { return ie(_w).(CodType) }
                wrapperExpr = goFuncLit
                  [Go.ParameterDecl [goIdent "_w"] False innerDom]
                  (Just $ Go.ResultType innerCod)
                  [goReturn [goTypeAssertExpr
                    (goCall (exprToPrimary ie) [goNameExpr "_w"]) innerCod]]
            pure (goConversion wrapperExpr, st3)
          else if needsCoerce
            then pure (goConversion (goTypeAssertExpr ie goUnderType), st3)
            else pure (goConversion ie, st3)
      Nothing -> pure (goConversion ie, st2)
  Core.TermTypeLambda ta -> encodeTerm cx g (Core.typeLambdaBody ta) st
  Core.TermTypeApplication ta -> do
    -- TermTypeApplication provides a type argument for the inner term's type parameter.
    -- This is critical for struct literals: TypeApplication(Union(ParseResult, ...), t1)
    -- means ParseResult's forall var 'a' should map to t1.
    -- Collect ALL type args from nested TypeApplications.
    let (innerTerm, typeArgs) = collectTermTypeArgs term
        -- Build substitution by matching type args against the inner term's type name
        innerTypeName = case deannotateTerm innerTerm of
          Core.TermRecord rec -> Just (Core.recordTypeName rec)
          Core.TermUnion inj -> Just (Core.injectionTypeName inj)
          Core.TermWrap wt -> Just (Core.wrappedTermTypeName wt)
          _ -> Nothing
    case innerTypeName of
      Just tname -> do
        -- Struct/union/wrap: build substitution from type args
        let typeArgSubst = buildTypeSubst g tname typeArgs
            stWithArgs = if M.null typeArgSubst then st
              else st { goStateTypeSubst = M.union typeArgSubst (goStateTypeSubst st) }
        encodeTerm cx g innerTerm stWithArgs
      Nothing -> case deannotateTerm innerTerm of
        -- Variable with type args: generate Go generic function instantiation
        -- e.g., TypeApplication(Variable "alt", t0) → Alt[T0]
        Core.TermVariable name
          | not (null typeArgs) && isQualifiedName name -> do
          let (baseExpr, st1) = resolveExprRef name st
          -- Encode type args as Go types, then use IndexExpr for instantiation
          -- For single type arg: f[T0]. For multiple: chain IndexExpr.
          case typeArgs of
            [singleArg] -> do
              (goArg, st2) <- encodeTypeForTerm cx g singleArg st1
              -- Only generate instantiation for simple type names
              if isSimpleGoType goArg
                then do
                  let typeExpr = typeToExpr goArg
                  pure (Go.ExpressionUnary $ Go.UnaryExprPrimary $
                    Go.PrimaryExprIndex $ Go.IndexExpr
                      (exprToPrimary baseExpr) typeExpr, st2)
                else pure (baseExpr, st2)  -- Composite type: skip instantiation
            _ ->
              -- Multiple type args or zero: fall through to regular encoding
              encodeTerm cx g innerTerm st
        -- Application with type args: the type args are for the FUNCTION being called
        -- e.g., TypeApplication(Application(fail, msg), t0) → Fail[T0](msg)
        Core.TermApplication _ | not (null typeArgs) -> do
          -- Encode the application's function and args, then add type instantiation
          let (appFun, appArgs) = collectApplicationArgs innerTerm
          case deannotateTerm appFun of
            Core.TermVariable name | isQualifiedName name && not (null typeArgs) -> do
              let (baseExpr, st1) = resolveExprRef name st
              -- Add type instantiation
              (goTypeArgs, st2) <- encodeTypes cx g typeArgs st1
              let instantiated = case goTypeArgs of
                    [goArg] | isSimpleGoType goArg ->
                      let typeExpr = typeToExpr goArg
                      in Go.ExpressionUnary $ Go.UnaryExprPrimary $
                           Go.PrimaryExprIndex $ Go.IndexExpr (exprToPrimary baseExpr) typeExpr
                    _ -> baseExpr
              -- Encode args and call
              (argExprs, st3) <- encodeTermList cx g appArgs st2
              if null argExprs
                then pure (instantiated, st3)
                else pure (goCall (exprToPrimary instantiated) argExprs, st3)
            _ -> encodeTerm cx g innerTerm st
        -- Other: pass through
        _ -> encodeTerm cx g innerTerm st
  _ -> failGo cx "unsupported term variant"

-- | Encode a Hydra lambda as a Go function literal.
-- Post-#332: lambdas are a direct variant of Term (no longer wrapped in Function).
encodeLambda :: Context -> Graph -> Core.Lambda -> GoState -> GoResult Go.Expression
encodeLambda cx g lam st = do
    let param = toGoUnexported (unName $ Core.lambdaParameter lam)
        mDomain = Core.lambdaDomain lam
        -- If there's an expected function type, use its domain instead of the
        -- lambda's own domain annotation. This handles type variable name collisions
        -- between nested function type schemes (e.g., bind's t0 vs between's t0).
        mEffectiveDomain = case goStateExpectedType st of
          Just expectedType -> case deannotateType expectedType of
            Core.TypeFunction ft -> Just (Core.functionTypeDomain ft)
            Core.TypeWrap wt -> case deannotateType wt of
              Core.TypeFunction ft -> Just (Core.functionTypeDomain ft)
              _ -> mDomain
            _ -> mDomain
          Nothing -> mDomain
    -- Encode the actual domain type (clear expected type to prevent leaking)
    let stClearExpected = st { goStateExpectedType = Nothing }
    (paramType, st1) <- case mEffectiveDomain of
      Just dom -> encodeTypeForTerm cx g dom stClearExpected
      Nothing -> pure (goAnyType, stClearExpected)
    let isParamAny = paramType == goAnyType
        -- Register param type: concrete (False) if typed, any (True) if any
        -- Register function-typed lambda params in:
        -- - goStateFuncParamTypes: for call-site arg coercion
        -- - goStateCallableVars: to prevent adapter wrapping in coerceToType
        -- Extract local substitution from the domain type (for the lambda body).
        -- E.g., domain Parser[func(t0) t1] → {a → func(t0) t1)} for Parser's forall var.
        domainSubst = case mDomain of
          Just dom -> extractSubstFromType g dom
          Nothing -> M.empty
        stWithParam = st1 {
          goStateVarTypes = M.insert param isParamAny (goStateVarTypes st1),
          goStateTypeSubst = M.union domainSubst (goStateTypeSubst st1),
          goStateCallableVars = case mDomain of
            Just dom | isFunctionType dom -> S.insert param (goStateCallableVars st1)
            _ -> goStateCallableVars st1,
          goStateFuncParamTypes = case mDomain of
            Just dom | isFunctionType dom -> case deannotateType dom of
              Core.TypeFunction ft ->
                M.insert param (Core.functionTypeDomain ft) (goStateFuncParamTypes st1)
              _ -> goStateFuncParamTypes st1
            _ -> goStateFuncParamTypes st1
        }
    -- Propagate expected type's codomain as the body's expected type
    let bodyExpectedType = case goStateExpectedType st of
          Just et -> case deannotateType et of
            Core.TypeFunction ft -> Just (Core.functionTypeCodomain ft)
            Core.TypeWrap wt -> case deannotateType wt of
              Core.TypeFunction ft -> Just (Core.functionTypeCodomain ft)
              _ -> Nothing
            _ -> Nothing
          Nothing -> Nothing
        stWithBody = stWithParam { goStateExpectedType = bodyExpectedType }
    (bodyExpr, st2) <- encodeTerm cx g (Core.lambdaBody lam) stWithBody
    -- Restore var types and func param types after encoding body (lambda scope)
    let st3 = st2 { goStateVarTypes = goStateVarTypes st,
                     goStateTypeSubst = goStateTypeSubst st,
                     goStateFuncParamTypes = goStateFuncParamTypes st,
                     goStateCallableVars = goStateCallableVars st }
    pure (goFuncLit
      [Go.ParameterDecl [goIdent param] False paramType]
      (Just $ Go.ResultType goAnyType)
      [goReturn [bodyExpr]], st3)

-- | Resolve a Hydra primitive name to a Go expression, adding the import.
-- "hydra.lib.strings.cat" -> strings.Cat (with import "hydra/lib/strings")
resolvePrimRef :: Core.Name -> GoState -> (Go.Expression, GoState)
resolvePrimRef (Core.Name name) st = case Strings.splitOn "." name of
  ["hydra", "lib", lib, fn] ->
    let ns = Namespace ("hydra.lib." ++ lib)
        pkg = namespaceToGoPackage ns
        impPath = goModulePath ++ "/hydra/lib/" ++ lib
    in (goQualNameExpr pkg (capitalize fn), addImport impPath st)
  _ -> (goNameExpr name, st)

-- | Encode a Hydra record projection as a Go expression. Post-#332: takes a
-- Projection directly (no longer wrapped in Elimination/Function).
encodeProjection :: Context -> Graph -> Core.Projection -> Maybe Core.Term -> GoState
  -> GoResult Go.Expression
encodeProjection cx g proj marg st = do
    let fname = toGoExported (unName $ Core.projectionField proj)
        typeName = Core.projectionTypeName proj
        -- Resolve type arguments from the current substitution context
        resolvedArgs = resolveTypeArgs g typeName st
    (goTypeArgs, st0) <- encodeTypes cx g resolvedArgs st
    let (recType, st1) = resolveTypeRef typeName goTypeArgs st0
    case marg of
      Nothing ->
        -- Unapplied projection: func(v any) any { return v.(RecordType).Field }
        pure (goFuncLit
          [Go.ParameterDecl [goIdent "v"] False goAnyType]
          (Just $ Go.ResultType goAnyType)
          [goReturn [Go.ExpressionUnary $ Go.UnaryExprPrimary $
            goSelector (Go.PrimaryExprTypeAssertion $
              Go.TypeAssertionExpr
                (Go.PrimaryExprOperand $ Go.OperandName_ $
                  Go.OperandName (goQualIdent Nothing "v") [])
                recType) fname]], st1)
      Just arg -> do
        (argExpr, st2) <- encodeTerm cx g arg st1
        pure (Go.ExpressionUnary $ Go.UnaryExprPrimary $
          goSelector (Go.PrimaryExprTypeAssertion $
            Go.TypeAssertionExpr (exprToPrimary argExpr) recType) fname, st2)

-- | Encode a Hydra case statement (union elimination) as a Go expression.
-- Post-#332: takes a CaseStatement directly.
encodeCases :: Context -> Graph -> Core.CaseStatement -> Maybe Core.Term -> GoState
  -> GoResult Go.Expression
encodeCases cx g cs marg st = do
    let csTypeName = Core.caseStatementTypeName cs
        tname = toGoExported (goLocalName csTypeName)
        caseFields = Core.caseStatementCases cs
        defCase = Core.caseStatementDefault cs
    (arms, st1) <- encodeCaseArms cx g csTypeName caseFields st
    (defArm, st2) <- case defCase of
      Nothing -> pure ([], st1)
      Just dt -> do
        -- If the default handler is a lambda, apply it to v. Other
        -- function-like terms (project/cases/unwrap) likewise get applied.
        -- Plain values are returned as-is.
        let dtTerm = case deannotateTerm dt of
              Core.TermLambda _ ->
                Core.TermApplication $ Core.Application dt (Core.TermVariable $ Core.Name "v")
              Core.TermProject _ ->
                Core.TermApplication $ Core.Application dt (Core.TermVariable $ Core.Name "v")
              Core.TermCases _ ->
                Core.TermApplication $ Core.Application dt (Core.TermVariable $ Core.Name "v")
              Core.TermUnwrap _ ->
                Core.TermApplication $ Core.Application dt (Core.TermVariable $ Core.Name "v")
              _ -> dt
        (dBody, st') <- encodeTerm cx g dtTerm st1
        pure ([Go.TypeCaseClause Nothing [goReturn [dBody]]], st')
    -- If no typed cases and only a default, skip the type switch entirely
    if null arms
      then case defCase of
        Just dt -> case marg of
          Nothing -> encodeTerm cx g dt st2  -- Return default function
          Just arg -> encodeTerm cx g
            (Core.TermApplication $ Core.Application dt arg) st2
        Nothing -> pure (goNameExpr "nil", st2)  -- No cases at all
      else do
        let allArms = arms ++ defArm
            switchStmt = Go.StatementSwitch $ Go.SwitchStmtType $ Go.TypeSwitchStmt
              Nothing
              (Go.TypeSwitchGuard (Just $ goIdent "v")
                (Go.PrimaryExprOperand $ Go.OperandName_ $
                  Go.OperandName (goQualIdent Nothing "x") []))
              allArms
        case marg of
          Nothing ->
            pure (goFuncLit
              [Go.ParameterDecl [goIdent "x"] False goAnyType]
              (Just $ Go.ResultType goAnyType)
              [switchStmt, goReturn [goNameExpr "nil"]], st2)
          Just arg -> do
            (argExpr, st') <- encodeTerm cx g arg st2
            let body = [goShortVar "x" argExpr, switchStmt, goReturn [goNameExpr "nil"]]
            pure (goCall (Go.PrimaryExprOperand $ Go.OperandLiteral $ Go.LiteralFunction $ Go.FunctionLit
              (Go.Signature (Go.Parameters []) (Just $ Go.ResultType goAnyType))
              (Go.FunctionBody $ Go.Block body)) [], st')

-- | Encode a Hydra unwrap (newtype elimination) as a Go expression.
-- Post-#332: takes a Name (the wrapped type name) directly.
encodeUnwrap :: Context -> Graph -> Core.Name -> Maybe Core.Term -> GoState
  -> GoResult Go.Expression
encodeUnwrap cx g _ marg st = case marg of
    Nothing ->
      -- Unwrap: func(v any) any { return v.(WrappedType).Value }
      -- For newtypes in Go, the unwrap is a type conversion
      pure (goFuncLit
        [Go.ParameterDecl [goIdent "v"] False goAnyType]
        (Just $ Go.ResultType goAnyType)
        [goReturn [goNameExpr "v"]], st)
    Just arg -> do
      (argExpr, st') <- encodeTerm cx g arg st
      pure (argExpr, st')

-- | Encode case arms for a union type switch.
encodeCaseArms :: Context -> Graph -> Core.Name -> [Core.Field] -> GoState
  -> GoResult [Go.TypeCaseClause]
encodeCaseArms _ _ _ [] st = pure ([], st)
encodeCaseArms cx g baseName (cf:cfs) st = do
  let cfname = toGoExported (unName $ Core.fieldName cf)
      cfterm = Core.fieldTerm cf
      -- Build variant type name using the same logic as TermUnion
      variantQName = case namespaceOf baseName of
        Just (Namespace ns) -> Core.Name (ns ++ "." ++ toGoExported (localNameOf baseName) ++ cfname)
        Nothing -> Core.Name (toGoExported (localNameOf baseName) ++ cfname)
      resolvedArgs = resolveTypeArgs g baseName st
  (goTypeArgs, st0) <- encodeTypes cx g resolvedArgs st
  let (variantTyp, st1) = resolveTypeRef variantQName goTypeArgs st0
  -- In the type switch, v has the variant struct type. The case arm function
  -- expects the INNER value. Use v.Value for non-unit variants.
  let vExpr = Core.TermVariable $ Core.Name "v"
  -- Check if the variant type is unit (no Value field)
  let variantFieldType = lookupUnionVariantType g baseName (Core.fieldName cf)
      isUnit = case variantFieldType of
        Just vt -> case deannotateType vt of
          Core.TypeUnit -> True
          Core.TypeRecord rt -> null rt
          _ -> False
        Nothing -> False
  -- Build substitution from the variant's field type for the case arm body.
  -- The variant field type (e.g., ParseSuccess[a]) with the ambient substitution
  -- gives the concrete type (e.g., ParseSuccess[t0]). Extract substitutions from this
  -- to map the inner type's forall vars (e.g., p → t0 for ParseSuccess).
  let variantSubst = case variantFieldType of
        Just vft ->
          let resolvedVft = applyTypeSubst (goStateTypeSubst st1) vft
          in extractSubstFromType g resolvedVft
        Nothing -> M.empty
      stForArm = if M.null variantSubst then st1
        else st1 { goStateTypeSubst = M.union variantSubst (goStateTypeSubst st1) }
  (armBody, st2) <- if isUnit
    then encodeTerm cx g (Core.TermApplication $ Core.Application cfterm vExpr) stForArm
    else do
      -- For non-unit variants, pass v.Value to the case function
      let vValue = Core.TermAnnotated $ Core.AnnotatedTerm vExpr M.empty  -- placeholder
      -- Generate: cfterm(v.Value) by encoding as function application where the arg is a field access
      (cfExpr, st1') <- encodeTerm cx g cfterm stForArm
      let goVValue = goFieldAccess "v" "Value"
      pure (goCall (exprToPrimary cfExpr) [goVValue], st1')
  let arm = Go.TypeCaseClause
        (Just [variantTyp])
        [goReturn [armBody]]
  (rest, st3) <- encodeCaseArms cx g baseName cfs st2
  pure (arm : rest, st3)

-- ============================================================================
-- Helper: encode multiple items
-- ============================================================================

encodeTermList :: Context -> Graph -> [Core.Term] -> GoState -> GoResult [Go.Expression]
encodeTermList _ _ [] st = pure ([], st)
encodeTermList cx g (t:ts) st = do
  (e, st1) <- encodeTerm cx g t st
  (es, st2) <- encodeTermList cx g ts st1
  pure (e:es, st2)

encodeMapEntries :: Context -> Graph -> [(Core.Term, Core.Term)] -> GoState
  -> GoResult [(Go.Expression, Go.Expression)]
encodeMapEntries _ _ [] st = pure ([], st)
encodeMapEntries cx g ((k,v):kvs) st = do
  (ke, st1) <- encodeTerm cx g k st
  (ve, st2) <- encodeTerm cx g v st1
  (rest, st3) <- encodeMapEntries cx g kvs st2
  pure ((ke, ve):rest, st3)

encodeBindings :: Context -> Graph -> [Core.Binding] -> GoState -> GoResult [Go.Statement]
encodeBindings _ _ [] st = pure ([], st)
encodeBindings cx g (b:bs) st = do
  let bname = toGoUnexported (unName $ Core.bindingName b)
      bterm = Core.bindingTerm b
      isRecursive = Core.bindingName b `S.member` freeVariablesInTerm bterm
      -- For recursive bindings, strip the lambda domain type so the lambda uses any params.
      -- For recursive bindings, encode with the original term (keep domain annotations).
      -- The forward declaration will use the actual param type from the lambda domain.
      btermForEncode = bterm
      -- Extract the lambda's domain type for the forward declaration
      stripTypeWrappers t = case deannotateTerm t of
        Core.TermTypeApplication ta -> stripTypeWrappers (Core.typeApplicationTermBody ta)
        Core.TermTypeLambda tl -> stripTypeWrappers (Core.typeLambdaBody tl)
        other -> other
      mRecLamDomain = if isRecursive
        then case stripTypeWrappers bterm of
          Core.TermLambda lam -> Core.lambdaDomain lam
          _ -> Nothing
        else Nothing
      -- Pre-register recursive bindings so the body can reference them
      stForEncode = if isRecursive
        then st {
          goStateVarTypes = M.insert bname False (goStateVarTypes st),
          goStateFuncParamTypes = case mRecLamDomain of
            Just pt -> M.insert bname pt (goStateFuncParamTypes st)
            Nothing -> M.insert bname (Core.TypeVariable $ Core.Name "_any") (goStateFuncParamTypes st)
        }
        else st
  (bval, st1) <- encodeTerm cx g btermForEncode stForEncode
  (rest, st2) <- encodeBindings cx g bs st1
  if isRecursive
    then do
      -- Recursive binding: var name func(ParamType) any; name = value
      -- Use the actual lambda domain type for the forward declaration
      (goRecParamType, st2a) <- case mRecLamDomain of
        Just dom -> encodeTypeForTerm cx g dom st2
        Nothing -> pure (goAnyType, st2)
      -- Register the recursive function in tracking state
      let st2b = st2a {
            goStateVarTypes = M.insert bname False (goStateVarTypes st2a),
            goStateFuncParamTypes = case mRecLamDomain of
              Just pt -> M.insert bname pt (goStateFuncParamTypes st2a)
              Nothing -> M.insert bname (Core.TypeVariable $ Core.Name "_any") (goStateFuncParamTypes st2a)
          }
          varDecl = Go.StatementDeclaration $ Go.DeclarationVar $ Go.VarDecl
            [Go.VarSpec [goIdent bname] (Just $ goFuncType [goRecParamType] goAnyType) []]
          assignStmt = Go.StatementSimple $ Go.SimpleStmtAssignment $ Go.Assignment
            [goNameExpr bname] Go.AssignOpSimple [bval]
      pure (varDecl : assignStmt : rest, st2b)
    else do
      -- Use var x any = expr for non-function values.
      -- This ensures struct literal field assertions work (source must be any).
      -- For function values, use := so the concrete func type is inferred
      -- (needed for the function to be callable without type assertion).
      -- Check if the binding has a function type (from the type annotation).
      -- Function-typed bindings keep := to preserve callability.
      -- Non-function bindings use var any for assertability.
      let isFuncBinding = case Core.bindingType b of
            Just tscheme -> isFunctionType (Core.typeSchemeType tscheme)
            Nothing -> case stripTermWrappers (Core.bindingTerm b) of
              Core.TermLambda _ -> True
              _ -> False
      -- Register binding type: func bindings are concrete (False), others are any (True)
      -- Also register the first param type for function bindings (for call-site coercion)
      -- Get first param type from lambda domain or binding type scheme
      let firstParamType = case stripTermWrappers (Core.bindingTerm b) of
            Core.TermLambda lam -> case Core.lambdaDomain lam of
              Just d -> Just d
              Nothing -> case Core.bindingType b of
                Just tscheme ->
                  let (_, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
                      (paramTypes, _) = unpackFunctionType innerTyp
                  in case paramTypes of
                    (pt:_) -> Just pt
                    _ -> Nothing
                Nothing -> Nothing
            _ -> Nothing
          -- Register all function bindings in goStateFuncParamTypes to mark them as callable.
          -- Use the actual domain type if available, or a dummy type variable as sentinel.
          st2' = st2 {
            goStateVarTypes = M.insert bname (not isFuncBinding) (goStateVarTypes st2),
            goStateFuncParamTypes = if isFuncBinding
              then M.insert bname (case firstParamType of
                Just pt -> pt
                Nothing -> Core.TypeVariable $ Core.Name "_any") (goStateFuncParamTypes st2)
              else goStateFuncParamTypes st2
          }
      if isFuncBinding
        then pure (goShortVar bname bval : rest, st2')
        else do
          let varDecl = Go.StatementDeclaration $ Go.DeclarationVar $ Go.VarDecl
                [Go.VarSpec [goIdent bname] (Just goAnyType) [bval]]
          pure (varDecl : rest, st2')

encodeFields :: Context -> Graph -> [Core.Field] -> GoState
  -> GoResult [(String, Go.Expression)]
encodeFields _ _ [] st = pure ([], st)
encodeFields cx g (f:fs) st = do
  let fname = toGoExported (unName $ Core.fieldName f)
  (fe, st1) <- encodeTerm cx g (Core.fieldTerm f) st
  (rest, st2) <- encodeFields cx g fs st1
  pure ((fname, fe):rest, st2)

-- | Encode record fields with type assertions based on declared field types.
-- Each field value is wrapped in expr.(GoFieldType) to satisfy Go's type checker.
encodeFieldsTyped :: Context -> Graph -> [Core.Field] -> M.Map Core.Name Core.Type -> GoState
  -> GoResult [(String, Go.Expression)]
encodeFieldsTyped _ _ [] _ st = pure ([], st)
encodeFieldsTyped cx g (f:fs) fieldTypes st = do
  let fname = Core.fieldName f
      goFname = toGoExported (unName fname)
  -- If the field type is a type application or alias, set up substitution for the inner term
  let fieldSubst = case M.lookup fname fieldTypes of
        Just ftype -> extractSubstFromType g ftype
        Nothing -> M.empty
      stForField = if M.null fieldSubst then st
        else st { goStateTypeSubst = M.union fieldSubst (goStateTypeSubst st) }
  (fe, st1) <- encodeTerm cx g (Core.fieldTerm f) stForField
  -- Restore substitution, then coerce the field value to the declared type
  let st1' = st1 { goStateTypeSubst = goStateTypeSubst st }
  (typedFe, st2) <- case M.lookup fname fieldTypes of
    Just ftype -> coerceToType cx g fe ftype (Core.fieldTerm f) st1'
    Nothing -> pure (fe, st1')
  (rest, st3) <- encodeFieldsTyped cx g fs fieldTypes st2
  pure ((goFname, typedFe):rest, st3)

-- | Check if a field type needs a type assertion when assigned from any.
needsTypeAssertion :: Graph -> Core.Type -> Bool
needsTypeAssertion g t = case deannotateType t of
  Core.TypeUnit -> False
  Core.TypeFunction _ -> False
  Core.TypeVariable name ->
    case qualifiedNameNamespace (qualifyName name) of
      Nothing -> False
      Just _ -> True  -- All named types need assertion from any
  _ -> True

-- | Check if a named type resolves to a concrete (non-interface) Go type.
-- Records and wraps are concrete; unions are interfaces.
isConcreteNamedType :: Graph -> Core.Name -> Bool
isConcreteNamedType g name =
  case M.lookup name (graphSchemaTypes g) of
    Just tscheme ->
      let (_, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
      in case deannotateType innerTyp of
        Core.TypeUnion _ -> False  -- Interface
        _ -> True                  -- Struct/wrap/other = concrete
    Nothing -> True  -- Unknown = assume concrete (conservative)

-- | Check if a term produces an any-typed Go expression.
-- Struct literals, union injections, wraps, and literals produce concrete types.
-- Variable references, function applications, and let bodies produce any.
producesAny :: GoState -> Core.Term -> Bool
producesAny st t = case deannotateTerm t of
  Core.TermLiteral _ -> False
  Core.TermRecord _ -> False
  Core.TermUnion _ -> False
  Core.TermWrap _ -> False
  Core.TermList _ -> False
  Core.TermSet _ -> False
  Core.TermMap _ -> False
  Core.TermUnit -> False
  Core.TermMaybe _ -> False
  Core.TermEither _ -> False
  Core.TermPair _ -> False
  Core.TermVariable name ->
    case qualifiedNameNamespace (qualifyName name) of
      Just _ -> False   -- Qualified: concrete type
      Nothing ->
        -- Check if the local variable has a tracked concrete type.
        -- Try both raw name and Go-sanitized name (for reserved words like close → close_)
        let raw = unName name
            goName = toGoUnexported raw
        in case M.lookup goName (goStateVarTypes st) of
          Just isAny -> isAny
          Nothing -> case M.lookup raw (goStateVarTypes st) of
            Just isAny -> isAny
            Nothing -> True  -- Unknown: assume any
  Core.TermTypeLambda tl -> producesAny st (Core.typeLambdaBody tl)
  Core.TermTypeApplication ta -> producesAny st (Core.typeApplicationTermBody ta)
  -- Application analysis: check what the function is
  Core.TermApplication app ->
    let (funTerm, argTerms) = collectApplicationArgs t
    in case stripTermWrappers funTerm of
      -- Function call: primitives and local functions return any.
      -- Generated cross-module functions return concrete types.
      Core.TermVariable name ->
        let goName = toGoUnexported (unName name)
            isLocal = case qualifiedNameNamespace (qualifyName name) of
              Nothing -> True
              Just _ -> False
            isCallable = S.member goName (goStateCallableVars st)
        -- Callable vars (function-typed params) return their actual type, not any
        in if isCallable then False
           else isLocal || isPrimitiveRef (Core.TermVariable name)
      Core.TermVariable _ -> True
      -- Projection applied to any-typed arg: result is concrete (field's type)
      Core.TermProject _ ->
        case argTerms of
          [arg] | producesAny st arg -> False  -- Direct field access returns concrete type
          _ -> True
      _ -> True
  -- 0-arg primitive constants (maps.empty, sets.empty) produce concrete values
  Core.TermVariable (Core.Name n)
    | n `elem` ["hydra.lib.maps.empty", "hydra.lib.sets.empty"] -> False
  _ -> True

-- | Check if a named type is a union type (interface in Go) by looking it up in the graph.
isUnionType :: Graph -> Core.Name -> Bool
isUnionType g name =
  case M.lookup name (graphSchemaTypes g) of
    Just tscheme ->
      let (_, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
      in case deannotateType innerTyp of
        Core.TypeUnion _ -> True
        _ -> False
    Nothing -> False  -- Unknown type — don't assert (conservative)

-- | Look up the field types of a record type from the graph's schema.
lookupRecordFieldTypes :: Graph -> Core.Name -> M.Map Core.Name Core.Type
lookupRecordFieldTypes g tname =
  case M.lookup tname (graphSchemaTypes g) of
    Just tscheme ->
      let (_, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
      in case deannotateType innerTyp of
        Core.TypeRecord fts -> M.fromList [(Core.fieldTypeName ft, Core.fieldTypeType ft) | ft <- fts]
        _ -> M.empty
    Nothing -> M.empty

-- | Look up the type of a union variant's value field.
lookupUnionVariantType :: Graph -> Core.Name -> Core.Name -> Maybe Core.Type
lookupUnionVariantType g unionName variantName =
  case M.lookup unionName (graphSchemaTypes g) of
    Just tscheme ->
      let (_, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
      in case deannotateType innerTyp of
        Core.TypeUnion fts ->
          case [Core.fieldTypeType ft | ft <- fts, Core.fieldTypeName ft == variantName] of
            (t:_) -> Just t
            [] -> Nothing
        _ -> Nothing
    Nothing -> Nothing

-- | Look up the underlying type of a wrap (newtype) for type assertions.
-- For wraps like `type Name string`, the schema stores the underlying type (string).
lookupWrapUnderlyingType :: Graph -> Core.Name -> Maybe Core.Type
lookupWrapUnderlyingType g name =
  case M.lookup name (graphSchemaTypes g) of
    Just tscheme ->
      let (_, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
      in Just innerTyp  -- The schema type IS the underlying type for wraps
    Nothing -> Nothing

-- | Look up the forall-bound type variable names of a named type.
lookupForallVars :: Graph -> Core.Name -> [Core.Name]
lookupForallVars g name =
  case M.lookup name (graphSchemaTypes g) of
    Just tscheme ->
      let schemeVars = Core.typeSchemeVariables tscheme
          (forallVars, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
          allVars = if null forallVars then schemeVars else forallVars
          -- Filter out phantom type vars (not used in the inner type body)
          innerFreeVars = freeVariablesInType innerTyp
      in filter (\v -> v `S.member` innerFreeVars) allVars
    Nothing -> []

-- | Build a type substitution map from a generic type's forall vars and concrete type args.
-- E.g., for ParserTestCase with forall ["a"] and args [jsonmodel.Value],
-- returns {Name "a" -> TypeVariable "hydra.json.model.Value"}.
-- Only includes entries where the type arg is NOT a self-referencing type variable
-- (to prevent infinite loops in encodeType).
buildTypeSubst :: Graph -> Core.Name -> [Core.Type] -> M.Map Core.Name Core.Type
buildTypeSubst g tname typeArgs =
  let forallVars = lookupForallVars g tname
  in if length forallVars == length typeArgs
     then M.fromList [(v, t) | (v, t) <- zip forallVars typeArgs,
                                not (isSelfRef v t)]
     else M.empty
  where
    isSelfRef v (Core.TypeVariable n) = v == n  -- Skip self-referencing: a → a
    isSelfRef _ _ = False

-- | Resolve the type arguments for a generic type from the current substitution.
-- For each forall var, look it up in the substitution. If not found, leave as-is
-- (will be encoded as any by encodeType for unqualified unresolved vars).
resolveTypeArgs :: Graph -> Core.Name -> GoState -> [Core.Type]
resolveTypeArgs g tname st =
  let forallVars = lookupForallVars g tname
  in fmap (\v -> M.findWithDefault (Core.TypeVariable v) v (goStateTypeSubst st)) forallVars

-- | Extract type variable substitutions from a type.
-- For type applications like ParserTestCase[Value], extracts {a -> Value}.
-- For type aliases like JsonParserTestCase (which is ParserTestCase[Value]),
-- expands the alias and extracts substitutions.
extractSubstFromType :: Graph -> Core.Type -> M.Map Core.Name Core.Type
extractSubstFromType g typ = go 5 typ  -- Limit expansion depth to prevent loops
  where
    go 0 _ = M.empty
    go depth t = case deannotateType t of
      Core.TypeApplication _ ->
        let (baseType, typeArgs) = collectTypeArgs t
        in case deannotateType baseType of
          Core.TypeVariable tname -> buildTypeSubst g tname typeArgs
          _ -> M.empty
      Core.TypeVariable tname ->
        -- Check if this is a type alias that expands to a type application
        case M.lookup tname (graphSchemaTypes g) of
          Just tscheme ->
            let (vars, body) = unpackForallType (Core.typeSchemeType tscheme)
            in if null vars && null (Core.typeSchemeVariables tscheme)
               then go (depth - 1) body  -- Expand non-parametric alias
               else M.empty
          Nothing -> M.empty
      _ -> M.empty

-- | Look up how many type parameters a named type has.
lookupTypeParamCount :: Graph -> Core.Name -> Int
lookupTypeParamCount g name =
  case M.lookup name (graphSchemaTypes g) of
    Just tscheme ->
      let schemeVars = length (Core.typeSchemeVariables tscheme)
          (forallVars, _) = unpackForallType (Core.typeSchemeType tscheme)
      in max schemeVars (length forallVars)
    Nothing -> 0

-- | Check if a collection-typed field is assigned from a qualified variable.
-- In this case, the Go variable already has the concrete collection type ([]any),
-- so asserting .([]any) on it would fail ([]any is not an interface).
-- | Check if a collection field should skip the type assertion.
-- Only skip when the value is ALREADY a concrete collection type
-- (from qualified variables/primitives or direct construction).
-- | Check if a term produces a callable value (function type in Go).
-- Used to decide if let bindings should use := (preserving callable type)
-- or var any (making the value assertable).
isCallableTerm :: Core.Term -> Bool
isCallableTerm t = case deannotateTerm t of
  Core.TermLambda _ -> True
  Core.TermProject _ -> True
  Core.TermCases _ -> True
  Core.TermUnwrap _ -> True
  Core.TermApplication _ -> True  -- Application results might be callable (curried)
  Core.TermVariable _ -> True     -- Variable refs might be functions
  Core.TermTypeLambda tl -> isCallableTerm (Core.typeLambdaBody tl)
  Core.TermTypeApplication ta -> isCallableTerm (Core.typeApplicationTermBody ta)
  _ -> False

showTermVariant :: Core.Term -> String
showTermVariant t = case t of
  Core.TermAnnotated _ -> "Annotated"
  Core.TermApplication _ -> "Application"
  Core.TermVariable (Core.Name n) -> "Var:" ++ n
  Core.TermLambda _ -> "Lambda"
  Core.TermProject _ -> "Project"
  Core.TermCases _ -> "Cases"
  Core.TermUnwrap _ -> "Unwrap"
  Core.TermTypeLambda _ -> "TypeLambda"
  Core.TermTypeApplication _ -> "TypeApp"
  Core.TermLiteral _ -> "Literal"
  Core.TermList _ -> "List"
  _ -> "Other"

-- | Check if a collection field should skip type assertion.
-- Only skip for QUALIFIED variables (top-level/cross-module refs) and direct
-- collection constructions (list/set/map literals). Local variables may be
-- any-typed and need assertions.
isCollectionAssignedFromQualified :: Core.Type -> Core.Term -> Bool
isCollectionAssignedFromQualified ftype fterm =
  let isCollection = case deannotateType ftype of
        Core.TypeList _ -> True
        Core.TypeSet _ -> True
        Core.TypeMap _ -> True
        _ -> False
      isConcreteSource = case stripTermWrappers fterm of
        Core.TermVariable name ->
          case qualifiedNameNamespace (qualifyName name) of
            Just _ -> True   -- Qualified var: concrete type
            Nothing -> False -- Local var: might be any
        Core.TermList _ -> True   -- Direct list literal
        Core.TermSet _ -> True
        Core.TermMap _ -> True
        _ -> False
  in isCollection && isConcreteSource

-- | Strip all wrappers from a term to find the underlying function/variable.
stripTermWrappers :: Core.Term -> Core.Term
stripTermWrappers t = case deannotateTerm t of
  Core.TermTypeLambda tl -> stripTermWrappers (Core.typeLambdaBody tl)
  Core.TermTypeApplication ta -> stripTermWrappers (Core.typeApplicationTermBody ta)
  Core.TermApplication app -> stripTermWrappers (Core.applicationFunction app)
  other -> other

-- | Coerce an expression to a target type. If the expression produces any
-- (from any-typed encoding) and the target is a concrete type, wraps in
-- a type assertion. For function type mismatches, wraps in an adapter.
coerceToType :: Context -> Graph -> Go.Expression -> Core.Type -> Core.Term -> GoState
  -> GoResult Go.Expression
coerceToType cx g expr targetType sourceTerm st
  -- Function type coercion: generate an adapter wrapper
  -- Needed when source function signature doesn't match target function signature.
  -- Skip for callable vars (function-typed params) that already have their actual Go type.
  | isFunctionType targetType && not (isCallableVar st sourceTerm) =
    case deannotateType targetType of
      Core.TypeFunction ft -> do
        (goDom, st1) <- encodeTypeForTerm cx g (Core.functionTypeDomain ft) st
        (goCod, st2) <- encodeTypeForTerm cx g (Core.functionTypeCodomain ft) st1
        -- Look up the source function's domain type for param coercion.
        -- Check both graphBoundTypes (top-level) and goStateFuncParamTypes (local).
        let sourceFuncParamTypes = lookupFunctionParamTypes g sourceTerm
            mSourceDom = case sourceFuncParamTypes of
              (t:_) -> Just t
              _ -> case deannotateTerm sourceTerm of
                Core.TermVariable name -> M.lookup (unName name) (goStateFuncParamTypes st)
                Core.TermTypeApplication ta -> case deannotateTerm (Core.typeApplicationTermBody ta) of
                  Core.TermVariable name -> M.lookup (unName name) (goStateFuncParamTypes st)
                  _ -> Nothing
                _ -> Nothing
        (goSourceDom, st3) <- case mSourceDom of
          Just sd -> encodeTypeForTerm cx g sd st2
          Nothing -> pure (goAnyType, st2)
        let sourceReturnsAny = isPrimitiveRef sourceTerm || producesAny st sourceTerm
              || isLocalFuncVar st sourceTerm
            needsDomCoerce = goDom /= goSourceDom
            needsCodCoerce = goCod /= goAnyType && sourceReturnsAny
        -- Skip wrapping if no coercion is needed at all
        if not needsDomCoerce && not needsCodCoerce
          then pure (expr, st3)
          else do
            -- func(_p TargetDom) TargetCod { return expr(argExpr).(TargetCod?) }
            -- For higher-order domains: when _p is func(A)B but source expects func(any)B,
            -- wrap _p: func(_q any) B { return _p(_q.(A)) }
            let argExpr
                  | needsDomCoerce && goDom == goAnyType && goSourceDom /= goAnyType =
                    -- Simple case: target domain is any, source expects concrete
                    goTypeAssertExpr (goNameExpr "_p") goSourceDom
                  | needsDomCoerce && isFuncGoType goDom && goSourceDom == goAnyType =
                    -- Higher-order: _p is func(A)B, source expects any
                    -- Since func(A)B is assignable to any, just pass _p directly
                    goNameExpr "_p"
                  | needsDomCoerce && isFuncGoType goDom =
                    -- Higher-order: _p is func(A)B, source expects func(any)C
                    -- Generate: func(_q any) C { return _p(_q.(A)) }
                    let innerParam = funcDomType goDom  -- A from target
                        innerRet = if isFuncGoType goSourceDom
                                     then funcCodType goSourceDom  -- C from source
                                     else funcCodType goDom        -- B from target
                    in goFuncLit
                         [Go.ParameterDecl [goIdent "_q"] False goAnyType]
                         (Just $ Go.ResultType innerRet)
                         [goReturn [goCall
                           (exprToPrimary $ goNameExpr "_p")
                           [goTypeAssertExpr (goNameExpr "_q") innerParam]]]
                  | otherwise = goNameExpr "_p"
                callExpr = goCall (exprToPrimary expr) [argExpr]
                bodyExpr = if needsCodCoerce
                  then goTypeAssertExpr callExpr goCod
                  else callExpr
            pure (goFuncLit
              [Go.ParameterDecl [goIdent "_p"] False goDom]
              (Just $ Go.ResultType goCod)
              [goReturn [bodyExpr]], st3)
      _ -> pure (expr, st)
  | not (producesAny st sourceTerm) = pure (expr, st)
  | otherwise = do
    (goTarget, st') <- encodeTypeForTerm cx g targetType st
    -- Only add type assertion for types that are valid Go assertion targets.
    -- any is skipped (no-op). Slices, arrays, maps, and primitive types can't
    -- be asserted on (they're not interfaces). Only named types (structs, wraps,
    -- interfaces) and function types are valid targets.
    -- Skip assertion for any, and for type params when the source is NOT any
    -- (type param assertion is valid from any but not from another type param)
    let isTypeParam = case targetType of
          Core.TypeVariable name -> S.member name (goStateFuncTypeParams st)
          _ -> False
    if goTarget == goAnyType
      then pure (expr, st')
      else if isTypeParam && not (producesAny st sourceTerm)
        then pure (expr, st')  -- source already has the right type, skip redundant assertion
        else pure (goTypeAssertExpr expr goTarget, st')

-- | Check if a Name is qualified (has a namespace/package prefix).
isQualifiedName :: Core.Name -> Bool
isQualifiedName name = case qualifiedNameNamespace (qualifyName name) of
  Just _ -> True
  Nothing -> False

-- | Convert a Go Type to an Expression (for use in generic function instantiation).
-- In Go, f[T] has T in the "index" position which is an expression, but actually
-- represents a type. We convert the Go Type AST to an expression form.
-- | Check if a Go Type can be represented as a simple expression (for IndexExpr).
-- Only simple named types can be used in IndexExpr. Composite types ([]any, func, etc.)
-- can't be represented as expressions in the current Go AST.
isSimpleGoType :: Go.Type -> Bool
isSimpleGoType (Go.TypeName_ _) = True
isSimpleGoType _ = False

typeToExpr :: Go.Type -> Go.Expression
typeToExpr (Go.TypeName_ tn) =
  Go.ExpressionUnary $ Go.UnaryExprPrimary $
    Go.PrimaryExprOperand $ Go.OperandName_ $
      Go.OperandName (Go.typeNameName tn) []
typeToExpr _ =
  Go.ExpressionUnary $ Go.UnaryExprPrimary $
    Go.PrimaryExprOperand $ Go.OperandName_ $
      Go.OperandName (goQualIdent Nothing "any") []

-- | Wrap an expression in a type assertion: expr.(GoType)
goTypeAssertExpr :: Go.Expression -> Go.Type -> Go.Expression
goTypeAssertExpr expr goType = Go.ExpressionUnary $ Go.UnaryExprPrimary $
  Go.PrimaryExprTypeAssertion $ Go.TypeAssertionExpr (exprToPrimary expr) goType

-- | Convert an Expression to PrimaryExpr (wrapping in parens if needed).
exprToPrimary :: Go.Expression -> Go.PrimaryExpr
exprToPrimary (Go.ExpressionUnary (Go.UnaryExprPrimary p)) = p
exprToPrimary expr = Go.PrimaryExprOperand $ Go.OperandParen expr

-- | Check if a term should use curried calling convention.
-- Primitives, local variables, lambdas, and non-callable terms are curried.
-- Only top-level qualified names (generated functions) are uncurried.
isCurriedRef :: Core.Term -> Bool
isCurriedRef t = case deannotateTerm t of
  Core.TermVariable name ->
    case qualifiedNameNamespace (qualifyName name) of
      Nothing -> True  -- Local variable: curried
      Just _ -> isPrimitiveRef t  -- Qualified: curried only if primitive
  Core.TermLambda _ -> True    -- Lambda: curried
  Core.TermProject _ -> True   -- Projection elimination: curried
  Core.TermCases _ -> True     -- Case statement elimination: curried
  Core.TermUnwrap _ -> True    -- Unwrap elimination: curried
  Core.TermLiteral _ -> True   -- Literal in function position: constant function
  Core.TermList _ -> True      -- Not callable
  Core.TermEither _ -> True    -- [2]any literal, not callable
  Core.TermPair _ -> True      -- [2]any literal, not callable
  Core.TermRecord _ -> True    -- Struct literal, not callable
  Core.TermUnion _ -> True     -- Struct literal, not callable
  Core.TermMap _ -> True       -- Collection, not callable
  Core.TermSet _ -> True       -- Collection, not callable
  Core.TermTypeApplication ta -> isCurriedRef (Core.typeApplicationTermBody ta)
  Core.TermTypeLambda tl -> isCurriedRef (Core.typeLambdaBody tl)
  _ -> False  -- Default: try multi-arg

-- | Check if a term references a local (unqualified) variable.
isLocalRef :: Core.Term -> Bool
isLocalRef t = case deannotateTerm t of
  Core.TermVariable name ->
    case qualifiedNameNamespace (qualifyName name) of
      Nothing -> True   -- Unqualified = local variable
      Just _ -> False   -- Qualified = top-level definition
  Core.TermTypeApplication ta -> isLocalRef (Core.typeApplicationTermBody ta)
  Core.TermTypeLambda tl -> isLocalRef (Core.typeLambdaBody tl)
  _ -> False

-- | Check if a term references a primitive function (hydra.lib.*)
-- Must also unwrap type applications and type lambdas, which the
-- inference/expansion passes may have added around variable references.
isPrimitiveRef :: Core.Term -> Bool
isPrimitiveRef t = case deannotateTerm t of
  Core.TermVariable (Core.Name n) -> L.isPrefixOf "hydra.lib." n
  Core.TermVariable (Core.Name n) -> L.isPrefixOf "hydra.lib." n
  Core.TermTypeApplication ta -> isPrimitiveRef (Core.typeApplicationTermBody ta)
  Core.TermTypeLambda tl -> isPrimitiveRef (Core.typeLambdaBody tl)
  _ -> False

-- | Collect nested applications into a function and its arguments list.
-- Application(Application(f, a), b) -> (f, [a, b])
-- Also skips type applications and type lambdas to find the real function.
collectApplicationArgs :: Core.Term -> (Core.Term, [Core.Term])
collectApplicationArgs t = case deannotateTerm t of
  Core.TermApplication app ->
    let (fun, args) = collectApplicationArgs (Core.applicationFunction app)
    in (fun, args ++ [Core.applicationArgument app])
  Core.TermTypeApplication ta -> collectApplicationArgs (Core.typeApplicationTermBody ta)
  Core.TermTypeLambda tl -> collectApplicationArgs (Core.typeLambdaBody tl)
  _ -> (t, [])

-- | Check if a term is directly callable without a type assertion.
-- Only qualified variable references (top-level definitions) and explicit
-- function terms are directly callable. Local variables (lambda params,
-- let bindings) are any-typed and need func(any)any assertion.
isDirectlyCallable :: Core.Term -> Bool
isDirectlyCallable t = case deannotateTerm t of
  Core.TermVariable name ->
    -- Only qualified names (with namespace) are top-level definitions
    case qualifiedNameNamespace (qualifyName name) of
      Just _ -> True
      Nothing -> False
  Core.TermVariable _ -> True
  Core.TermLambda _ -> True
  Core.TermProject _ -> True
  Core.TermTypeApplication ta -> isDirectlyCallable (Core.typeApplicationTermBody ta)
  Core.TermTypeLambda tl -> isDirectlyCallable (Core.typeLambdaBody tl)
  _ -> False

-- | Wrap a PrimaryExpr in a type assertion to func(any) any.
-- This is needed when calling curried results in Go, since intermediate
-- results are any-typed but need to be callable.
-- Produces: expr.(func(any) any)
goTypeAssertFunc :: Go.PrimaryExpr -> Go.PrimaryExpr
goTypeAssertFunc expr = Go.PrimaryExprTypeAssertion $ Go.TypeAssertionExpr
  expr (goFuncType [goAnyType] goAnyType)

-- ============================================================================
-- Type definition encoding
-- ============================================================================

-- | Encode a Hydra type definition as a Go top-level declaration.
encodeTypeDefinition :: Context -> Graph -> TypeDefinition -> GoState
  -> GoResult Go.TopLevelDecl
encodeTypeDefinition cx g tdef st = do
  let name = typeDefinitionName tdef
      typ = typeDefinitionType tdef
      goName = toGoExported (goLocalName name)
      (forallVars, innerTyp) = unpackForallType typ
      freeVars = filter isLocalVar $ S.toList (freeVariablesInType typ)
      -- Filter out phantom type vars (forall vars not used in the inner type body)
      innerFreeVars = freeVariablesInType innerTyp
      nonPhantomForallVars = filter (\v -> v `S.member` innerFreeVars) forallVars
      allTypeVars = nonPhantomForallVars ++ freeVars
      tparams = if null allTypeVars then Nothing
                else Just $ Go.TypeParameters
                  [Go.TypeParamDecl (fmap (goIdent . capitalize . unName) allTypeVars)
                    (Go.TypeConstraint $ Go.TypeElem
                      [Go.TypeTerm False goAnyType])]
  let stDef = st { goStateInTypeDef = True }
  case deannotateType innerTyp of
    Core.TypeRecord rt -> do
      (fields, st') <- encodeStructFields cx g rt stDef
      pure (goTypeDecl goName tparams (Go.TypeLiteral $ Go.TypeLitStruct $ Go.StructType fields), st')
    Core.TypeUnion rt -> do
      -- Go doesn't have union types directly; we generate an interface + struct per variant
      (decls, st') <- encodeUnionType cx g goName tparams rt stDef
      -- Return the first decl; the rest will be added separately
      -- TODO: handle multiple declarations properly
      pure (head decls, st')
    Core.TypeWrap wt -> do
      (inner, st') <- encodeType cx g wt stDef
      pure (goTypeDecl goName tparams inner, st')
    -- Type applications WITHOUT forall (e.g., JsonParserTestCase = ParserTestCase[Value])
    -- generate Go type aliases (type X = Y[Z]) for assignment compatibility.
    -- Types with forall need regular definitions with type parameters.
    Core.TypeApplication _ | null allTypeVars -> do
      (goTyp, st') <- encodeType cx g innerTyp stDef
      pure (goTypeAlias goName goTyp, st')
    _ -> do
      (goTyp, st') <- encodeType cx g typ stDef
      pure (goTypeDecl goName tparams goTyp, st')

-- | Construct a type declaration.
goTypeDecl :: String -> Maybe Go.TypeParameters -> Go.Type -> Go.TopLevelDecl
goTypeDecl name tparams typ = Go.TopLevelDeclDeclaration $ Go.DeclarationType $
  Go.TypeDecl [Go.TypeSpecDefinition $ Go.TypeDef (goIdent name) tparams typ]

-- | Construct a type alias declaration (type X = Y).
goTypeAlias :: String -> Go.Type -> Go.TopLevelDecl
goTypeAlias name typ = Go.TopLevelDeclDeclaration $ Go.DeclarationType $
  Go.TypeDecl [Go.TypeSpecAlias $ Go.AliasDecl (goIdent name) typ]

-- | Encode record fields as Go struct fields.
encodeStructFields :: Context -> Graph -> [Core.FieldType] -> GoState
  -> GoResult [Go.FieldDecl]
encodeStructFields _ _ [] st = pure ([], st)
encodeStructFields cx g (ft:fts) st = do
  let fname = toGoExported (unName $ Core.fieldTypeName ft)
  (ftyp, st1) <- encodeType cx g (Core.fieldTypeType ft) st
  (rest, st2) <- encodeStructFields cx g fts st1
  pure (Go.FieldDeclNamed (Go.NamedField [goIdent fname] ftyp Nothing) : rest, st2)

-- | Encode a union type as Go interface + variant structs.
-- Returns a list of top-level declarations (interface + one struct per variant).
encodeUnionType :: Context -> Graph -> String -> Maybe Go.TypeParameters
  -> [Core.FieldType] -> GoState -> GoResult [Go.TopLevelDecl]
encodeUnionType cx g name tparams variants st = do
  -- Generate the interface with a sealed marker method
  let markerMethod = Go.InterfaceElemMethod $ Go.MethodElem
        (goIdent $ "is" ++ name)
        (Go.Signature (Go.Parameters []) Nothing)
      ifaceDecl = goTypeDecl name tparams (goInterfaceType [markerMethod])
  -- Generate a struct for each variant
  (variantDecls, st') <- encodeVariantStructs cx g name tparams variants st
  pure (ifaceDecl : variantDecls, st')

encodeVariantStructs :: Context -> Graph -> String -> Maybe Go.TypeParameters
  -> [Core.FieldType] -> GoState -> GoResult [Go.TopLevelDecl]
encodeVariantStructs _ _ _ _ [] st = pure ([], st)
encodeVariantStructs cx g baseName tparams (ft:fts) st = do
  let vname = toGoExported (unName $ Core.fieldTypeName ft)
      structName = baseName ++ vname
      vtype = Core.fieldTypeType ft
  dtyp <- pure $ deannotateType vtype
  let isUnit = case dtyp of
        Core.TypeUnit -> True
        Core.TypeRecord rt -> null rt
        _ -> False
  (structFields, st1) <- if isUnit
    then pure ([], st)
    else do
      (ft', st') <- encodeType cx g vtype st
      pure ([Go.FieldDeclNamed $ Go.NamedField [goIdent "Value"] ft' Nothing], st')
  let structDecl = goTypeDecl structName tparams
        (Go.TypeLiteral $ Go.TypeLitStruct $ Go.StructType structFields)
      -- Generate marker method: func (StructName[T]) isBaseName() {}
      -- Must include type params if the parent union is generic
      receiverType = case tparams of
        Just (Go.TypeParameters [Go.TypeParamDecl paramNames _]) ->
          goQualTypeName Nothing structName
            (fmap (\(Go.Identifier n) -> goSimpleTypeName n) paramNames)
        _ -> goSimpleTypeName structName
      markerMethod = Go.TopLevelDeclMethod $ Go.MethodDecl
        (Go.Receiver Nothing receiverType)
        (goIdent $ "is" ++ baseName)
        (Go.Signature (Go.Parameters []) Nothing)
        (Just $ Go.FunctionBody $ Go.Block [])
  (rest, st2) <- encodeVariantStructs cx g baseName tparams fts st1
  pure (structDecl : markerMethod : rest, st2)

-- | Encode a type for use as a struct field type.
-- Uses any for non-scalar types to avoid the any-vs-concrete mismatch
-- when struct literals are constructed from any-typed term expressions.
-- Scalars (string, int*, float*, bool) keep their concrete types.
encodeFieldType :: Context -> Graph -> Core.Type -> GoState -> GoResult Go.Type
encodeFieldType cx g typ st = case deannotateType typ of
  Core.TypeLiteral lt -> encodeLiteralType lt st  -- Keep scalar types
  Core.TypeUnit -> pure (goEmptyStructType, st)   -- Keep unit
  _ -> pure (goAnyType, st)                       -- Everything else → any

-- | Check if a name is a local (unqualified) type variable.
isLocalVar :: Core.Name -> Bool
isLocalVar (Core.Name n) = '.' `notElem` n

-- | Encode a Hydra type for term-level usage.
-- Unbound type variables (not in goStateTypeSubst) resolve to any.
-- Bound type variables resolve to their substituted concrete types.
encodeTypeForTerm :: Context -> Graph -> Core.Type -> GoState -> GoResult Go.Type
encodeTypeForTerm = encodeType

-- | Check if a Hydra type is a function type.
isFunctionType :: Core.Type -> Bool
isFunctionType t = case deannotateType t of
  Core.TypeFunction _ -> True
  Core.TypeForall fa -> isFunctionType (Core.forallTypeBody fa)
  Core.TypeAnnotated at -> isFunctionType (Core.annotatedTypeBody at)
  _ -> False

-- | Check if a Go type is a function type literal.
isFuncGoType :: Go.Type -> Bool
isFuncGoType (Go.TypeLiteral (Go.TypeLitFunction _)) = True
isFuncGoType _ = False

-- | Extract the domain type from a Go function type.
funcDomType :: Go.Type -> Go.Type
funcDomType (Go.TypeLiteral (Go.TypeLitFunction ft)) =
  let sig = Go.unFunctionType ft
      Go.Parameters ps = Go.signatureParameters sig
  in case ps of
    (Go.ParameterDecl _ _ t : _) -> t
    _ -> goAnyType
funcDomType _ = goAnyType

-- | Extract the codomain type from a Go function type.
funcCodType :: Go.Type -> Go.Type
funcCodType (Go.TypeLiteral (Go.TypeLitFunction ft)) =
  let sig = Go.unFunctionType ft
  in case Go.signatureResult sig of
    Just (Go.ResultType t) -> t
    _ -> goAnyType
funcCodType _ = goAnyType

-- | Check whether a type variable needs to become a Go type parameter.
-- A type var needs a Go param if it appears as a type argument to a named generic type
-- (like ParserTestCase[a], ParseResult[a]) in any of the given types.
-- Type vars used only in collections (List, Map, Set), functions, or primitives
-- don't need Go params because those are always any-typed in Go.
typeVarNeedsGoParam :: Graph -> Core.Name -> [Core.Type] -> Bool
typeVarNeedsGoParam g varName types = any (varInGoVisiblePos varName) types
  where
    varInGoVisiblePos :: Core.Name -> Core.Type -> Bool
    varInGoVisiblePos v t = case deannotateType t of
      -- Type application: check if v is used as a type arg to a named generic type
      Core.TypeApplication _ ->
        let (base, args) = collectTypeArgs t
        in case deannotateType base of
          Core.TypeVariable tname
            | not (null (lookupForallVars g tname)) ->
              any (\a -> case deannotateType a of
                Core.TypeVariable n -> n == v
                _ -> varInGoVisiblePos v a) args
            | otherwise -> False
          _ -> any (varInGoVisiblePos v) args
      -- Bare type variable in a Go-visible position (function param/return type)
      Core.TypeVariable n -> n == v
      -- Record/union/wrap: check field types
      Core.TypeRecord fts -> any (varInGoVisiblePos v . Core.fieldTypeType) fts
      Core.TypeUnion fts -> any (varInGoVisiblePos v . Core.fieldTypeType) fts
      Core.TypeWrap wt -> varInGoVisiblePos v wt
      -- Function types: type vars in function domain/codomain are Go-visible
      Core.TypeFunction ft ->
        varInGoVisiblePos v (Core.functionTypeDomain ft)
        || varInGoVisiblePos v (Core.functionTypeCodomain ft)
      -- Forall: recurse into body
      Core.TypeForall fa -> varInGoVisiblePos v (Core.forallTypeBody fa)
      -- Collections, literals, etc.: not Go-visible (always []any)
      Core.TypeList _ -> False
      Core.TypeSet _ -> False
      Core.TypeMap _ -> False
      Core.TypeMaybe _ -> False
      _ -> False

-- | Check if a function type has a function-typed domain (higher-order).
-- Used to skip adapter generation for higher-order functions (not yet supported).
hasHigherOrderDomain :: Core.Type -> Bool
hasHigherOrderDomain t = case deannotateType t of
  Core.TypeFunction ft -> isFunctionType (Core.functionTypeDomain ft)
  Core.TypeForall fa -> hasHigherOrderDomain (Core.forallTypeBody fa)
  _ -> False

-- | Check if a term is a function (lambda or elimination — primitives are
-- variables post-#332 and are NOT counted here).
isFunctionTerm :: Core.Term -> Bool
isFunctionTerm t = case deannotateTerm t of
  Core.TermLambda _ -> True
  Core.TermProject _ -> True
  Core.TermCases _ -> True
  Core.TermUnwrap _ -> True
  _ -> False

-- | Check if a term is a function-typed param (lambda or top-level) that already
-- has its actual Go type. These should NOT get adapter wrapping in coerceToType.
isCallableVar :: GoState -> Core.Term -> Bool
isCallableVar st t = case deannotateTerm t of
  Core.TermVariable name -> S.member (toGoUnexported $ unName name) (goStateCallableVars st)
  Core.TermTypeApplication ta -> isCallableVar st (Core.typeApplicationTermBody ta)
  Core.TermTypeLambda tl -> isCallableVar st (Core.typeLambdaBody tl)
  _ -> False

-- | Check if a term is a local variable that is a known function binding.
-- Checks both goStateFuncParamTypes (func vars with known param types)
-- and goStateVarTypes (concrete = False means func binding via :=).
isLocalFuncVar :: GoState -> Core.Term -> Bool
isLocalFuncVar st t = case deannotateTerm t of
  Core.TermVariable name ->
    let n = toGoUnexported (unName name)
    -- Must be in goStateFuncParamTypes (let-bound func) but NOT in goStateCallableVars
    -- (function-typed params have actual Go types, not func(T) any).
    in M.member n (goStateFuncParamTypes st) && not (S.member n (goStateCallableVars st))
  Core.TermTypeApplication ta -> isLocalFuncVar st (Core.typeApplicationTermBody ta)
  Core.TermTypeLambda tl -> isLocalFuncVar st (Core.typeLambdaBody tl)
  _ -> False

-- | Collect type arguments from the innermost function in an application chain.
-- For Application(Application(f, a), b) where f may be TypeApplication-wrapped,
-- returns the type args from f.
collectFuncTypeArgs :: Core.Term -> [Core.Type]
collectFuncTypeArgs t = case deannotateTerm t of
  Core.TermApplication app -> collectFuncTypeArgs (Core.applicationFunction app)
  Core.TermTypeApplication ta -> Core.typeApplicationTermType ta : collectFuncTypeArgs (Core.typeApplicationTermBody ta)
  Core.TermTypeLambda tl -> collectFuncTypeArgs (Core.typeLambdaBody tl)
  _ -> []

-- | Collect type arguments from nested TermTypeApplication wrappers.
-- TypeApplication(TypeApplication(term, A), B) -> (term, [A, B])
collectTermTypeArgs :: Core.Term -> (Core.Term, [Core.Type])
collectTermTypeArgs t = case deannotateTerm t of
  Core.TermTypeApplication ta ->
    let (inner, args) = collectTermTypeArgs (Core.typeApplicationTermBody ta)
    in (inner, args ++ [Core.typeApplicationTermType ta])
  Core.TermTypeLambda tl -> collectTermTypeArgs (Core.typeLambdaBody tl)
  _ -> (t, [])

-- | Collect nested type applications: App(App(F, A), B) -> (F, [A, B])
collectTypeArgs :: Core.Type -> (Core.Type, [Core.Type])
collectTypeArgs t = case deannotateType t of
  Core.TypeApplication at ->
    let (base, args) = collectTypeArgs (Core.applicationTypeFunction at)
    in (base, args ++ [Core.applicationTypeArgument at])
  _ -> (t, [])

-- | Unwrap nested forall types, collecting the bound type variable names.
unpackForallType :: Core.Type -> ([Core.Name], Core.Type)
unpackForallType t = case deannotateType t of
  Core.TypeForall fa ->
    let (vars, inner) = unpackForallType (Core.forallTypeBody fa)
    in (Core.forallTypeParameter fa : vars, inner)
  _ -> ([], t)

-- ============================================================================
-- Term definition encoding
-- ============================================================================

-- | Encode a Hydra term definition as a Go top-level declaration.
-- Unpacks lambda chains into proper func declarations with typed parameters.
encodeTermDefinition :: Context -> Graph -> TermDefinition -> GoState
  -> GoResult Go.TopLevelDecl
encodeTermDefinition cx g tdef st = do
  let name = termDefinitionName tdef
      term = termDefinitionTerm tdef
      tscheme = termDefinitionType tdef
      goName = toGoExported (goLocalName name)
      typ = Core.typeSchemeType tscheme
      -- Unpack forall type variables
      (forallVars, innerTyp) = unpackForallType typ
      -- Unpack function type into parameter types and return type
      (paramTypes, retTyp) = unpackFunctionType innerTyp
      -- Unpack lambda chain into parameter names, domains, body, and type var subst
      (params, lambdaDomains, body, typeVarSubst) = unpackLambdasWithDomains term
  -- For generic functions, determine which type variables need to become Go type params.
  -- Only type vars that appear in "Go-visible" positions (named type args like Parser[A])
  -- need Go type params. Vars used only in collections/functions are erased to any.
  let allSchemeVars = case Core.typeSchemeVariables tscheme of
        [] -> forallVars
        vs -> vs
      -- Check each type var: does it appear in a named type argument position?
      goVisibleVars = S.fromList [v | v <- allSchemeVars,
        typeVarNeedsGoParam g v (paramTypes ++ [retTyp])]
      stWithTypeParams = st { goStateFuncTypeParams = S.union goVisibleVars (goStateFuncTypeParams st) }
  -- Encode actual parameter types and return type from the type scheme.
  (goParamTypes, st1) <- encodeTypesForTerm cx g paramTypes stWithTypeParams
  (goRetType, st2) <- encodeTypeForTerm cx g retTyp st1
  -- Build type substitution from parameter types, but only include CONSISTENT entries.
  -- When different params map the same type var to different types (e.g., Apply has
  -- Parser[func(T0) T1] and Parser[T0], both mapping 'a' differently), omit that entry.
  let allParamSubsts = [extractSubstFromType g pt | pt <- paramTypes ++ [retTyp]]
      paramSubst = M.mapMaybeWithKey (\k v ->
        if all (\s -> case M.lookup k s of { Just v' -> v' == v; Nothing -> True }) allParamSubsts
          then Just v
          else Nothing)
        (M.unions allParamSubsts)
      -- Build a mapping from internal type vars to scheme vars by unifying
      -- lambda domain annotations with the corresponding param types from the type scheme.
      -- E.g., if lambda domain has t5 and param type has t0, we get {t5 → t0}.
      -- Build substitution mapping ALL internal type vars in the body's lambda
      -- domains to the corresponding scheme vars from the function's type.
      -- This handles the kernel's code generation using internal var names (t3, t5)
      -- that differ from the scheme vars (t0, t1).
      -- Unify each lambda domain annotation with the corresponding param type.
      -- Also unify the body's internal structure recursively.
      domainParamSubst = M.fromList $ concatMap (\(mDom, pt) ->
        case mDom of
          Just dom -> unifyTypeVars dom pt
          Nothing -> []) (zip lambdaDomains paramTypes)
      -- Compose typeVarSubst and domainParamSubst with paramSubst so that
      -- internal vars (t5) map transitively through scheme vars (t0) to Go type params (T0).
      -- E.g., typeVarSubst = {t5 → TypeVariable "t0"}, paramSubst = {t0 → TypeVariable "T0"}
      -- → composedTypeVarSubst = {t5 → TypeVariable "T0"}
      composeSubst base target = M.map (\v -> applyTypeSubst target v) base
      composedTypeVarSubst = composeSubst typeVarSubst paramSubst
      composedDomainSubst = composeSubst domainParamSubst paramSubst
      -- Register Hydra types of function params for type inference
      termParamTypes = M.fromList [(p, pt) | (p, pt) <- zip params paramTypes]
      st2a = st2 { goStateTypeSubst = M.union composedTypeVarSubst (M.union composedDomainSubst (M.union paramSubst (goStateTypeSubst st2))),
                    goStateTermParamTypes = M.union termParamTypes (goStateTermParamTypes st2) }
  -- Register param types in state for producesAny tracking and call-site coercion.
  -- Function-typed params go in goStateCallableVars (they're callable but don't return any).
  -- Also register their first param type in goStateFuncParamTypes for call-site arg coercion,
  -- but mark them specially so isLocalFuncVar doesn't treat them as "returns any".
  let paramNames = fmap (toGoUnexported . unName) params
      paramIsAny = fmap (== goAnyType) goParamTypes
      callableParams = S.fromList [n | (n, pt) <- zip paramNames paramTypes, isFunctionType pt]
      -- Register function-typed params in goStateFuncParamTypes for call-site arg coercion
      -- AND in goStateCallableVars so coerceToType skips adapter wrapping for them.
      funcParamEntries = [(n, Core.functionTypeDomain ft)
        | (n, pt) <- zip paramNames paramTypes,
          isFunctionType pt,
          Core.TypeFunction ft <- [deannotateType pt]]
      st2' = st2a {
        goStateVarTypes = L.foldl' (\m (n, isAny) -> M.insert n isAny m) (goStateVarTypes st2a) (zip paramNames paramIsAny),
        goStateCallableVars = S.union callableParams (goStateCallableVars st2a),
        goStateFuncParamTypes = L.foldl' (\m (n, ft) -> M.insert n ft m) (goStateFuncParamTypes st2a) funcParamEntries
      }
  -- Encode body with expected return type for bidirectional type propagation
  let st2'' = if not (S.null goVisibleVars)
        then st2' { goStateExpectedType = Just retTyp }
        else st2'
  (rawBodyExpr, st3) <- encodeTerm cx g body st2''
  -- Coerce the body expression to the return type
  (bodyExpr, st4) <- if goRetType /= goAnyType
    then coerceToType cx g rawBodyExpr retTyp body st3
    else pure (rawBodyExpr, st3)
  -- Build parameter declarations (zip names with types)
  let goParams = zipParams params goParamTypes
      -- Type parameters: only Go-visible type vars
      goVisibleVarsList = S.toList goVisibleVars
      tparams = if null goVisibleVarsList then Nothing
                else Just $ Go.TypeParameters
                  [Go.TypeParamDecl (fmap (goIdent . capitalize . unName) goVisibleVarsList)
                    (Go.TypeConstraint $ Go.TypeElem [Go.TypeTerm False goAnyType])]
  if null params
    then
      -- No parameters: generate a var declaration
      pure (Go.TopLevelDeclDeclaration $ Go.DeclarationVar $ Go.VarDecl
        [Go.VarSpec [goIdent goName] Nothing [bodyExpr]], st4)
    else
      -- Has parameters: generate a func declaration
      pure (Go.TopLevelDeclFunction $ Go.FunctionDecl
        (goIdent goName)
        tparams
        (Go.Signature
          (Go.Parameters goParams)
          (Just $ Go.ResultType goRetType))
        (Just $ Go.FunctionBody $ Go.Block [goReturn [bodyExpr]]), st4)

-- | Unpack a function type into parameter types and return type.
unpackFunctionType :: Core.Type -> ([Core.Type], Core.Type)
unpackFunctionType t = case deannotateType t of
  Core.TypeFunction ft ->
    let (rest, ret) = unpackFunctionType (Core.functionTypeCodomain ft)
    in (Core.functionTypeDomain ft : rest, ret)
  _ -> ([], t)

-- | Unpack a lambda chain into parameter names and the body term.
unpackLambdas :: Core.Term -> ([Core.Name], Core.Term)
unpackLambdas t = case deannotateTerm t of
  Core.TermLambda lam ->
    let (rest, body) = unpackLambdas (Core.lambdaBody lam)
    in (Core.lambdaParameter lam : rest, body)
  Core.TermTypeLambda tl ->
    unpackLambdas (Core.typeLambdaBody tl)
  _ -> ([], t)

-- | Unpack a lambda chain returning parameter names, domain annotations, body,
-- and type variable substitutions from TypeLambda/TypeApplication pairs.
unpackLambdasWithDomains :: Core.Term -> ([Core.Name], [Maybe Core.Type], Core.Term, M.Map Core.Name Core.Type)
unpackLambdasWithDomains t = case deannotateTerm t of
  Core.TermLambda lam ->
    let (rest, doms, body, tsubst) = unpackLambdasWithDomains (Core.lambdaBody lam)
    in (Core.lambdaParameter lam : rest, Core.lambdaDomain lam : doms, body, tsubst)
  Core.TermTypeLambda tl ->
    -- TypeLambda introduces a type variable; look for a TypeApplication to map it
    let inner = Core.typeLambdaBody tl
        tyParam = Core.typeLambdaParameter tl
    in case deannotateTerm inner of
      Core.TermTypeApplication ta ->
        -- TypeLambda(t5, TypeApplication(body, t0)) → {t5 → t0}
        let (rest, doms, body, tsubst) = unpackLambdasWithDomains (Core.typeApplicationTermBody ta)
            typeArg = Core.typeApplicationTermType ta
        in (rest, doms, body, M.insert tyParam typeArg tsubst)
      _ ->
        let (rest, doms, body, tsubst) = unpackLambdasWithDomains inner
        in (rest, doms, body, tsubst)
  Core.TermTypeApplication ta ->
    let (rest, doms, body, tsubst) = unpackLambdasWithDomains (Core.typeApplicationTermBody ta)
    in (rest, doms, body, tsubst)
  _ -> ([], [], t, M.empty)

-- | Extract outer TypeLambda params from a term.
extractTypeLambdaParams :: Core.Term -> [Core.Name]
extractTypeLambdaParams t = case deannotateTerm t of
  Core.TermTypeLambda tl ->
    Core.typeLambdaParameter tl : extractTypeLambdaParams (Core.typeLambdaBody tl)
  _ -> []

-- | Find the first TypeApplication args for a given variable in a term.
-- Searches for TypeApp(TypeApp(...(Variable name, arg1), arg2)..., argN)
-- and returns [arg1, arg2, ..., argN] (innermost first).
findTypeApplicationArgs :: Core.Name -> Core.Term -> Maybe [Core.Type]
findTypeApplicationArgs targetName = go
  where
    go t = case deannotateTerm t of
      Core.TermTypeApplication ta ->
        let (innerTerm, typeArgs) = collectTermTypeArgs t
        in case deannotateTerm innerTerm of
          Core.TermVariable name | name == targetName -> Just typeArgs
          _ -> go (Core.typeApplicationTermBody ta)  -- search deeper
      Core.TermApplication app ->
        go (Core.applicationFunction app) `orElse` go (Core.applicationArgument app)
      Core.TermLambda lam ->
        go (Core.lambdaBody lam)
      Core.TermLet lt ->
        let inBindings = L.find (/= Nothing) $
              fmap (\b -> go (Core.bindingTerm b)) (Core.letBindings lt)
            inBody = go (Core.letBody lt)
        in case inBindings of
          Just (Just args) -> Just args
          _ -> inBody
      Core.TermTypeLambda tl -> go (Core.typeLambdaBody tl)
      Core.TermAnnotated at -> go (Core.annotatedTermBody at)
      Core.TermCases cs ->
        let inCases = L.find (/= Nothing) $
              fmap (\f -> go (Core.fieldTerm f)) (Core.caseStatementCases cs)
            inDefault = case Core.caseStatementDefault cs of
              Just dt -> go dt
              Nothing -> Nothing
        in case inCases of
          Just (Just args) -> Just args
          _ -> inDefault
      _ -> Nothing
    orElse (Just x) _ = Just x
    orElse Nothing y = y

-- | Zip parameter names with types to create Go parameter declarations.
zipParams :: [Core.Name] -> [Go.Type] -> [Go.ParameterDecl]
zipParams [] _ = []
zipParams _ [] = []
zipParams (n:ns) (t:ts) =
  Go.ParameterDecl [goIdent $ toGoUnexported (unName n)] False t : zipParams ns ts

-- | Encode a list of types for term-level usage (erasing type variables).
encodeTypesForTerm :: Context -> Graph -> [Core.Type] -> GoState -> GoResult [Go.Type]
encodeTypesForTerm _ _ [] st = pure ([], st)
encodeTypesForTerm cx g (t:ts) st = do
  (gt, st1) <- encodeTypeForTerm cx g t st
  (gts, st2) <- encodeTypesForTerm cx g ts st1
  pure (gt : gts, st2)

-- | Look up the parameter types of a function from the graph's bound types.
lookupFunctionParamTypes :: Graph -> Core.Term -> [Core.Type]
lookupFunctionParamTypes g term =
  case stripTermWrappers term of
    Core.TermVariable name ->
      case M.lookup name (graphBoundTypes g) of
        Just tscheme ->
          let (_, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
              (paramTypes, _) = unpackFunctionType innerTyp
          in paramTypes
        Nothing -> []
    _ -> []

-- | Infer type variable substitutions at a call site by matching the called
-- function's param types against the current substitution context.
-- E.g., if the called function has param type ParserTestCase[t0] and the current
-- substitution has a → jsonmodel.Value (where a is the TYPE definition's var
-- and t0 is the TERM definition's var), this produces {t0 → jsonmodel.Value}
-- by finding t0 in the param types and matching it against the existing subst.
inferCallSiteSubst :: Graph -> Core.Term -> [Core.Type] -> M.Map Core.Name Core.Type
  -> M.Map Core.Name Core.Type
inferCallSiteSubst g funTerm paramTypes currentSubst =
  case stripTermWrappers funTerm of
    Core.TermVariable funName ->
      case M.lookup funName (graphBoundTypes g) of
        Just tscheme ->
          let schemeVars = case Core.typeSchemeVariables tscheme of
                [] -> let (fv, _) = unpackForallType (Core.typeSchemeType tscheme) in fv
                vs -> vs
              -- For each param type, collect type variable occurrences
              -- and try to resolve them from the current substitution
              resolvedVars = concatMap (matchTypeVars g schemeVars currentSubst) paramTypes
          in M.fromList resolvedVars
        Nothing -> M.empty
    _ -> M.empty

-- | Match type variables in a type against the current substitution.
-- If a type application like ParserTestCase[t0] appears where
-- the type ParserTestCase has forall var a, and a → value is in the subst,
-- then we learn t0 → value.
matchTypeVars :: Graph -> [Core.Name] -> M.Map Core.Name Core.Type -> Core.Type
  -> [(Core.Name, Core.Type)]
matchTypeVars g schemeVars subst typ = case deannotateType typ of
  Core.TypeApplication _ ->
    let (base, args) = collectTypeArgs typ
    in case deannotateType base of
      Core.TypeVariable tname ->
        let forallVars = lookupForallVars g tname
            -- For each arg that is a scheme var, check if the corresponding
            -- forall var is in the current substitution
            pairs = [(sv, resolved)
              | (arg, forallVar) <- zip args forallVars,
                Core.TypeVariable sv <- [deannotateType arg],
                sv `elem` schemeVars,
                resolved <- case M.lookup forallVar subst of
                  Just r -> [r]
                  Nothing -> []]
        in pairs
      _ -> []
  Core.TypeFunction ft ->
    matchTypeVars g schemeVars subst (Core.functionTypeDomain ft)
    ++ matchTypeVars g schemeVars subst (Core.functionTypeCodomain ft)
  _ -> []

-- | Try to infer the Hydra type of a term from its structure.
-- Used for bidirectional type propagation: the type of an argument
-- can inform the function's domain type variables.
inferTermType :: Graph -> Core.Term -> GoState -> Maybe Core.Type
inferTermType g term st = case deannotateTerm term of
  -- Strip type wrappers
  Core.TermTypeApplication ta -> inferTermType g (Core.typeApplicationTermBody ta) st
  Core.TermTypeLambda tl -> inferTermType g (Core.typeLambdaBody tl) st
  -- Application of unwrap to a variable: the type is the wrap's underlying type
  -- with type args resolved from the variable's known type
  Core.TermApplication app ->
    let (funTerm, args) = collectApplicationArgs term
    in case (deannotateTerm funTerm, args) of
      (Core.TermUnwrap wrapName, (wrappedArg:callArgs)) ->
        -- Unwrap applied to arg(s): result type is the wrap's underlying type
        -- with type args from the wrapped arg's type.
        -- If there are additional call args, the result is the codomain.
        case inferTermType g wrappedArg st of
          Just argType ->
            let argSubst = extractSubstFromType g argType
                underType = lookupWrapUnderlyingType g wrapName
            in case underType of
              Just ut ->
                let substitutedType = if M.null argSubst then ut else applyTypeSubst argSubst ut
                in if null callArgs then Just substitutedType
                   else let unwrapped = case deannotateType substitutedType of
                              Core.TypeWrap wt -> wt
                              other -> other
                        in case deannotateType unwrapped of
                          Core.TypeFunction ft -> Just (Core.functionTypeCodomain ft)
                          _ -> Just substitutedType
              _ -> Nothing
          Nothing -> lookupWrapUnderlyingType g wrapName
      -- General function application: infer function type and return the codomain
      _ -> case inferTermType g funTerm st of
        Just funType -> case deannotateType funType of
          Core.TypeFunction ft -> Just (Core.functionTypeCodomain ft)
          _ -> Nothing
        Nothing -> Nothing
  -- Variable: look up its type in the graph or in the current function's param types
  Core.TermVariable name ->
    case M.lookup name (goStateTermParamTypes st) of
      Just pt -> Just pt
      Nothing -> case M.lookup name (graphBoundTypes g) of
        Just tscheme -> Just (Core.typeSchemeType tscheme)
        Nothing -> Nothing
  _ -> Nothing

-- | Apply a type substitution to a Hydra type.
applyTypeSubst :: M.Map Core.Name Core.Type -> Core.Type -> Core.Type
applyTypeSubst subst typ = case typ of
  Core.TypeVariable name -> M.findWithDefault typ name subst
  Core.TypeFunction ft -> Core.TypeFunction $ ft {
    Core.functionTypeDomain = applyTypeSubst subst (Core.functionTypeDomain ft),
    Core.functionTypeCodomain = applyTypeSubst subst (Core.functionTypeCodomain ft) }
  Core.TypeApplication at -> Core.TypeApplication $ at {
    Core.applicationTypeFunction = applyTypeSubst subst (Core.applicationTypeFunction at),
    Core.applicationTypeArgument = applyTypeSubst subst (Core.applicationTypeArgument at) }
  Core.TypeList t -> Core.TypeList (applyTypeSubst subst t)
  Core.TypeSet t -> Core.TypeSet (applyTypeSubst subst t)
  Core.TypeMap mt -> Core.TypeMap $ mt {
    Core.mapTypeKeys = applyTypeSubst subst (Core.mapTypeKeys mt),
    Core.mapTypeValues = applyTypeSubst subst (Core.mapTypeValues mt) }
  Core.TypeForall fa -> Core.TypeForall $ fa {
    Core.forallTypeBody = applyTypeSubst subst (Core.forallTypeBody fa) }
  Core.TypeAnnotated at -> Core.TypeAnnotated $ at {
    Core.annotatedTypeBody = applyTypeSubst subst (Core.annotatedTypeBody at) }
  Core.TypeWrap wt -> Core.TypeWrap (applyTypeSubst subst wt)
  _ -> typ

-- | Infer type variable substitutions from actual argument types.
-- For each argument, try to determine its Hydra type and match against the
-- called function's parameter types to determine type variable mappings.
inferArgTypeSubst :: Graph -> Core.Term -> [Core.Term] -> GoState -> M.Map Core.Name Core.Type
inferArgTypeSubst g funTerm argTerms st =
  case stripTermWrappers funTerm of
    Core.TermVariable funName ->
      case M.lookup funName (graphBoundTypes g) of
        Just tscheme ->
          let schemeVars = case Core.typeSchemeVariables tscheme of
                [] -> let (fv, _) = unpackForallType (Core.typeSchemeType tscheme) in fv
                vs -> vs
              (_, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
              (paramTypes, _) = unpackFunctionType innerTyp
              -- For each arg, try to infer its type and unify with the param type
              argTypes = fmap (\arg -> inferTermType g arg st) argTerms
              -- Match param types against arg types to find substitutions
              pairs = concatMap (uncurry (matchTypes g schemeVars))
                        (zip paramTypes (fmap (orDefault (Core.TypeVariable $ Core.Name "_")) argTypes))
          in M.fromList pairs
        Nothing -> M.empty
    _ -> M.empty
  where
    orDefault def Nothing = def
    orDefault _ (Just x) = x

-- | Unify ALL type variables between two types.
-- Maps variables from the first type to the corresponding positions in the second type.
-- E.g., unifyTypeVars (FunctionType t5 Term) (FunctionType t0 Term) → [(t5, TypeVariable t0)]
unifyTypeVars :: Core.Type -> Core.Type -> [(Core.Name, Core.Type)]
unifyTypeVars t1 t2 = case (deannotateType t1, deannotateType t2) of
  (Core.TypeVariable v1, _) -> [(v1, t2)]
  (Core.TypeFunction ft1, Core.TypeFunction ft2) ->
    unifyTypeVars (Core.functionTypeDomain ft1) (Core.functionTypeDomain ft2)
    ++ unifyTypeVars (Core.functionTypeCodomain ft1) (Core.functionTypeCodomain ft2)
  (Core.TypeApplication at1, Core.TypeApplication at2) ->
    unifyTypeVars (Core.applicationTypeFunction at1) (Core.applicationTypeFunction at2)
    ++ unifyTypeVars (Core.applicationTypeArgument at1) (Core.applicationTypeArgument at2)
  (Core.TypeList a, Core.TypeList b) -> unifyTypeVars a b
  (Core.TypeSet a, Core.TypeSet b) -> unifyTypeVars a b
  (Core.TypeMap m1, Core.TypeMap m2) ->
    unifyTypeVars (Core.mapTypeKeys m1) (Core.mapTypeKeys m2)
    ++ unifyTypeVars (Core.mapTypeValues m1) (Core.mapTypeValues m2)
  (Core.TypeWrap a, Core.TypeWrap b) -> unifyTypeVars a b
  (Core.TypeForall fa1, Core.TypeForall fa2) ->
    unifyTypeVars (Core.forallTypeBody fa1) (Core.forallTypeBody fa2)
  _ -> []

-- | Unify two types to extract type variable substitutions.
-- Given a pattern type (from the function signature) and an actual type (from the argument),
-- extract mappings from the pattern's type variables to the actual type's components.
matchTypes :: Graph -> [Core.Name] -> Core.Type -> Core.Type -> [(Core.Name, Core.Type)]
matchTypes g schemeVars pattern actual = case (deannotateType pattern, deannotateType actual) of
  (Core.TypeVariable pv, _)
    | pv `elem` schemeVars -> [(pv, actual)]
  (Core.TypeApplication pat, Core.TypeApplication act) ->
    let (patBase, patArgs) = collectTypeArgs pattern
        (actBase, actArgs) = collectTypeArgs actual
    in case (deannotateType patBase, deannotateType actBase) of
      (Core.TypeVariable pn, Core.TypeVariable an)
        | pn == an -> concatMap (uncurry (matchTypes g schemeVars)) (zip patArgs actArgs)
      _ -> []
  (Core.TypeFunction pft, Core.TypeFunction aft) ->
    matchTypes g schemeVars (Core.functionTypeDomain pft) (Core.functionTypeDomain aft)
    ++ matchTypes g schemeVars (Core.functionTypeCodomain pft) (Core.functionTypeCodomain aft)
  _ -> []

-- | Infer type variable substitutions by matching a function's return type
-- against an expected return type.
inferRetTypeSubst :: Graph -> Core.Term -> [Core.Name] -> Core.Type -> M.Map Core.Name Core.Type
inferRetTypeSubst g funTerm schemeVars expectedRet =
  case stripTermWrappers funTerm of
    Core.TermVariable funName ->
      case M.lookup funName (graphBoundTypes g) of
        Just tscheme ->
          let (_, innerTyp) = unpackForallType (Core.typeSchemeType tscheme)
              (_, retTyp) = unpackFunctionType innerTyp
          in M.fromList $ matchTypes g schemeVars retTyp expectedRet
        Nothing -> M.empty
    _ -> M.empty

-- | Look up the type scheme variables of a called function.
lookupCalledSchemeVars :: Graph -> Core.Term -> [Core.Name]
lookupCalledSchemeVars g term =
  case stripTermWrappers term of
    Core.TermVariable funName ->
      case M.lookup funName (graphBoundTypes g) of
        Just tscheme -> case Core.typeSchemeVariables tscheme of
          [] -> let (fv, _) = unpackForallType (Core.typeSchemeType tscheme) in fv
          vs -> vs
        Nothing -> []
    _ -> []

-- | Encode arguments with coercion to expected parameter types.
encodeAndCoerceArgs :: Context -> Graph -> [Core.Term] -> [Core.Type] -> GoState
  -> GoResult [Go.Expression]
encodeAndCoerceArgs _ _ [] _ st = pure ([], st)
encodeAndCoerceArgs cx g (t:ts) paramTypes st = do
  -- Set expected type for the argument encoding (bidirectional type propagation)
  let stWithExpected = case paramTypes of
        (pt:_) -> st { goStateExpectedType = Just pt }
        [] -> st
  (e, st1) <- encodeTerm cx g t stWithExpected
  (coerced, st2) <- case paramTypes of
    (pt:_) -> coerceToType cx g e pt t st1
    [] -> pure (e, st1)
  let restTypes = case paramTypes of (_:pts) -> pts; [] -> []
  (rest, st3) <- encodeAndCoerceArgs cx g ts restTypes st2
  pure (coerced:rest, st3)

-- | Encode a list of types.
encodeTypes :: Context -> Graph -> [Core.Type] -> GoState -> GoResult [Go.Type]
encodeTypes _ _ [] st = pure ([], st)
encodeTypes cx g (t:ts) st = do
  (gt, st1) <- encodeType cx g t st
  (gts, st2) <- encodeTypes cx g ts st1
  pure (gt : gts, st2)

-- ============================================================================
-- Module assembly
-- ============================================================================

-- | Convert a Hydra module to a map of file paths to Go source code strings.
moduleToGo :: Module -> [Definition] -> Context -> Graph
  -> Either (InContext Error) (M.Map FilePath String)
moduleToGo mod_ defs cx g = do
  let (typeDefs, termDefs) = partitionDefinitions defs
      st0 = initState mod_
      pkgName = namespaceToGoPackage (moduleNamespace mod_)
  -- Encode type definitions (may produce multiple decls per type for unions)
  (typeDecls, st1) <- encodeTypeDefs cx g typeDefs st0
  -- Encode term definitions
  (termDecls, st2) <- encodeTermDefs cx g termDefs st1
  let allDecls = typeDecls ++ termDecls
      imports = buildImports (goStateImports st2)
      goModule = Go.Module
        (Go.PackageClause $ goIdent pkgName)
        imports
        allDecls
      code = printExpr $ parenthesize $ writeModule goModule
      filePath = goNamespaceToFilePath (moduleNamespace mod_)
  Right $ M.singleton filePath code

encodeTypeDefs :: Context -> Graph -> [TypeDefinition] -> GoState
  -> GoResult [Go.TopLevelDecl]
encodeTypeDefs _ _ [] st = pure ([], st)
encodeTypeDefs cx g (td:tds) st = do
  -- For union types, a single type definition may produce multiple Go declarations
  let typ = typeDefinitionType td
      (forallVars, innerTyp) = unpackForallType typ
  case deannotateType innerTyp of
    Core.TypeUnion rt -> do
      let name = toGoExported (goLocalName $ typeDefinitionName td)
          freeVars = filter isLocalVar $ S.toList (freeVariablesInType typ)
          allTypeVars = forallVars ++ freeVars
          tparams = if null allTypeVars then Nothing
                    else Just $ Go.TypeParameters
                      [Go.TypeParamDecl (fmap (goIdent . capitalize . unName) allTypeVars)
                        (Go.TypeConstraint $ Go.TypeElem [Go.TypeTerm False goAnyType])]
      (decls, st1) <- encodeUnionType cx g name tparams rt (st { goStateInTypeDef = True })
      (rest, st2) <- encodeTypeDefs cx g tds (st1 { goStateInTypeDef = False })
      pure (decls ++ rest, st2)
    _ -> do
      (d, st1) <- encodeTypeDefinition cx g td st
      (rest, st2) <- encodeTypeDefs cx g tds (st1 { goStateInTypeDef = False })
      pure (d : rest, st2)

encodeTermDefs :: Context -> Graph -> [TermDefinition] -> GoState
  -> GoResult [Go.TopLevelDecl]
encodeTermDefs _ _ [] st = pure ([], st)
encodeTermDefs cx g (td:tds) st = do
  (d, st1) <- encodeTermDefinition cx g td st
  -- Clear per-function state before encoding the next term definition
  let st1' = st1 { goStateFuncTypeParams = S.empty, goStateTypeSubst = M.empty,
                    goStateTermParamTypes = M.empty }
  (rest, st2) <- encodeTermDefs cx g tds st1'
  pure (d : rest, st2)

-- | Build Go import declarations from accumulated import paths.
-- | Build Go import declarations from accumulated import paths.
-- Adds import aliases when the Go package alias differs from the directory name.
buildImports :: S.Set String -> [Go.ImportDecl]
buildImports imps
  | S.null imps = []
  | otherwise = [Go.ImportDecl $ fmap toImportSpec $ L.sort $ S.toList imps]
  where
    toImportSpec path =
      let dirName = lastSeg path
          -- Reconstruct the namespace from the import path to get the Go alias
          ns = pathToNamespace path
          goAlias = namespaceToGoPackage ns
          -- Add alias if the Go alias differs from the directory name
          alias = if goAlias /= dirName
            then Just $ Go.ImportAliasName $ Go.Identifier goAlias
            else Nothing
      in Go.ImportSpec alias (Go.ImportPath $ Go.StringLitInterpreted $
          Go.InterpretedStringLit path)
    lastSeg = last . Strings.splitOn "/"
    -- Convert import path back to namespace.
    -- For hydra imports: "hydra.dev/hydra/encode/core" → Namespace "hydra.encode.core"
    -- For stdlib imports: "math/big" → Namespace "math.big" (no prefix to strip)
    pathToNamespace path =
      let segs = Strings.splitOn "/" path
          nsParts = if L.isPrefixOf "hydra.dev" (head segs)
            then drop 1 segs  -- Drop "hydra.dev" module prefix
            else segs          -- Standard library: keep as-is
      in Namespace $ L.intercalate "." nsParts

-- | Convert a Hydra namespace to a Go file path.
-- In Go, each package must be its own directory. So "hydra.core" becomes
-- "hydra/core/core.go" (the directory is the package, the file is named after the last segment).
goNamespaceToFilePath :: Namespace -> FilePath
goNamespaceToFilePath (Namespace ns) =
  let parts = Strings.splitOn "." ns
      dirPath = L.intercalate "/" parts
      fileName = case reverse parts of
        (p:_) -> p
        [] -> "main"
  in dirPath ++ "/" ++ fileName ++ ".go"

-- ============================================================================
-- Error helpers
-- ============================================================================

failGo :: Context -> String -> GoResult a
failGo cx msg = Left $ InContext {
  inContextObject = ErrorOther (OtherError msg),
  inContextContext = cx }

failGoSimple :: String -> GoResult a
failGoSimple msg = Left $ InContext {
  inContextObject = ErrorOther (OtherError msg),
  inContextContext = Context [] [] M.empty }

-- ============================================================================
-- Language definition
-- ============================================================================

goLanguage :: Language
goLanguage = GoLang.goLanguage
