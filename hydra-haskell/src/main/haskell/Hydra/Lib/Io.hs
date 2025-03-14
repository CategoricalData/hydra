-- | Haskell implementations of hydra.lib.io primitives.

module Hydra.Lib.Io (
  showElement,
  showGraph,
  showTerm,
  showType,
  showTypeConstraint,
  showTypeScheme,
  showTypeSubst,
) where

import Hydra.Core
import Hydra.Compute
import Hydra.Graph
--import Hydra.Ext.Json.Coder
--import Hydra.Dsl.Annotations
--import Hydra.Ext.Json.Serde
import Hydra.CoreEncoding
--import Hydra.Staging.Rewriting
--import Hydra.Annotations
import Hydra.Flows
import Hydra.Inference
import Hydra.Strip
import qualified Hydra.Json as Json
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


noGraph :: Graph
noGraph = Graph {
  graphElements = M.empty,
  graphEnvironment = M.empty,
  graphTypes = M.empty,
  graphBody = Terms.list [],
  graphPrimitives = M.empty,
  graphSchema = Nothing}


showElement :: Element -> String
showElement el = unName (elementName el) ++ " = " ++ showTerm (elementTerm el) ++ (case elementType el of
  Nothing -> ""
  Just ts -> " : " ++ showTypeScheme ts)

showGraph :: Graph -> String
showGraph graph = "{" ++ (L.intercalate ", " $ fmap showElement $ M.elems $ graphElements graph) ++ "}"

--showTerm :: Term -> String
----showTerm term = fromFlow "fail" noGraph (jsonValueToString <$> untypedTermToJson term)
--showTerm = show

showTerm :: Term -> String
showTerm term = case stripTerm term of
    TermApplication app -> "(" ++ (L.intercalate " @ " $ fmap showTerm $ gatherTerms [] app) ++ ")"
      where
        gatherTerms prev (Application lhs rhs) = case stripTerm lhs of
          TermApplication app2 -> gatherTerms (rhs:prev) app2
          t -> t:(rhs:prev)
    TermFunction f -> case f of
      FunctionElimination elm -> case elm of
        EliminationRecord (Projection tname fname) -> "project(" ++ unName tname ++ "){" ++ unName fname ++ "}"
        EliminationUnion (CaseStatement tname mdef cases) -> "case(" ++ unName tname ++ ")" ++ showFields fields
          where
            fields = cases ++ (Y.maybe [] (\d -> [Field (Name "[default]") d]) mdef)
            suffix = case mdef of
              Nothing -> ""
              Just def -> "[default] = " ++ showTerm def
        EliminationWrap tname -> "unwrap(" ++ unName tname ++ ")"
        _ -> show $ stripTerm term
      FunctionLambda (Lambda (Name v) mt body) -> "λ" ++ v ++ (Y.maybe "" (\t -> ":" ++ showType t) mt) ++ "." ++ showTerm body
      FunctionPrimitive (Name name) -> name ++ "!"
    TermLet (Let bindings env) -> "let " ++ (L.intercalate ", " $ fmap showBinding bindings) ++ " in " ++ showTerm env
      where
        showBinding (LetBinding (Name v) term mt) = v ++ "=" ++ showTerm term ++ (Y.maybe "" (\t -> ":" ++ showTypeScheme t) mt)
    TermList els -> "[" ++ (L.intercalate ", " $ fmap showTerm els) ++ "]"
    TermLiteral lit -> case lit of
      LiteralBinary _ -> "[binary]"
      LiteralBoolean b -> if b then "true" else "false"
      LiteralFloat fv -> case fv of
        FloatValueBigfloat v -> show v
        FloatValueFloat32 v -> show v
        FloatValueFloat64 v -> show v
      LiteralInteger iv -> case iv of
        IntegerValueBigint v -> show v
        IntegerValueInt8 v -> show v
        IntegerValueInt16 v -> show v
        IntegerValueInt32 v -> show v
        IntegerValueInt64 v -> show v
        IntegerValueUint8 v -> show v
        IntegerValueUint16 v -> show v
        IntegerValueUint32 v -> show v
        IntegerValueUint64 v -> show v
      LiteralString s -> show s
    TermOptional mt -> case mt of
      Nothing -> "nothing"
      Just t -> "just(" ++ showTerm t ++ ")"
    TermProduct els -> "(" ++ (L.intercalate ", " $ fmap showTerm els) ++ ")"
    TermRecord (Record tname fields) -> "record(" ++ unName tname ++ ")" ++ showFields fields
    TermTypeAbstraction (TypeAbstraction (Name param) body) -> "Λ" ++ param ++ "." ++ showTerm body
    TermTypeApplication (TypedTerm term typ) -> showTerm term ++ "⟨" ++ showType typ ++ "⟩"
    TermTyped (TypedTerm term typ) -> "(" ++ showTerm term ++ " : " ++ showType typ ++ ")"
    TermUnion (Injection tname f) -> "inject(" ++ unName tname ++ ")" ++ showFields [f]
    TermVariable (Name name) -> name
    TermWrap (WrappedTerm tname term1) -> "wrap(" ++ unName tname ++ "){" ++ showTerm term1 ++ "}"
    t -> show t
  where
    showField (Field fname term) = unName fname ++ "=" ++ showTerm term
    showFields fields = "{" ++ (L.intercalate ", " $ fmap showField fields) ++ "}"

--     coder <- termStringCoder
--     coderEncode coder encoded
--   where
--     --encoded = coreEncodeTerm $ rewriteTermMeta (const $ Kv M.empty) term
--     encoded = rewriteTermMeta (const $ Kv M.empty) term

--termStringCoder :: Flow Graph (Coder Graph Graph Term String)
--termStringCoder = do
--    termJsonCoder <- jsonCoder $ TypeVariable _Term
--    return $ Coder (mout termJsonCoder) (min termJsonCoder)
--  where
--    mout termJsonCoder term = jsonValueToString <$> coderEncode termJsonCoder term
--    min termJsonCoder s = case stringToJsonValue s of
--      Left msg -> fail $ "failed to parse JSON value: " ++ msg
--      Right v -> coderDecode termJsonCoder v

--showType :: Type -> String
--showType typ = fromFlow "fail" noGraph $ do
--    coder <- typeStringCoder
--    coderEncode coder encoded
--  where
--    encoded = coreEncodeType $ rewriteTypeMeta (const $ Kv M.empty) typ

-- TODO: for now, we are bypassing the complexity of TermAdapters because of issues yet to be resolved
--showType :: Type -> String
----showType = showTerm . coreEncodeType
--showType = show

--showType typ = case flowStateValue result of
--    Nothing -> "failed to encode type:\n" ++ show (traceMessages $ flowStateTrace result)
--    Just s -> s
--  where
--    result = unFlow (jsonValueToString <$> untypedTermToJson encoded) noGraph emptyTrace
--    encoded = stripTermRecursive $ coreEncodeType typ

--typeStringCoder :: Flow Graph (Coder Graph Graph Term String)
--typeStringCoder = do
--    typeJsonCoder <- jsonCoder $ TypeVariable _Type
--    return $ Coder (mout typeJsonCoder) (min typeJsonCoder)
--  where
--    mout typeJsonCoder term = jsonValueToString <$> coderEncode typeJsonCoder term
--    min typeJsonCoder s = case stringToJsonValue s of
--      Left msg -> fail $ "failed to parse as JSON value: " ++ msg
--      Right v -> coderDecode typeJsonCoder v

showType :: Type -> String
showType typ = case stripType typ of
    TypeApplication app -> "(" ++ (L.intercalate " @ " $ fmap showType $ gatherTypes [] app) ++ ")"
      where
        gatherTypes prev (ApplicationType lhs rhs) = case stripType lhs of
          TypeApplication app2 -> gatherTypes (rhs:prev) app2
          t -> t:(rhs:prev)
    TypeFunction t -> "(" ++ gatherTypes [] typ ++ ")"
      where
        gatherTypes prev t = case stripType t of
          TypeFunction (FunctionType dom cod) -> gatherTypes (dom:prev) cod
          t1 -> L.intercalate " → " $ fmap showType $ L.reverse (t1:prev)
    TypeList etyp -> "list<" ++ showType etyp ++ ">"
    TypeLambda (LambdaType (Name var) body) -> "λ" ++ var ++ "." ++ showType body
    TypeLiteral lt -> case lt of
      LiteralTypeBinary -> "binary"
      LiteralTypeBoolean -> "boolean"
      LiteralTypeFloat ft -> case ft of
        FloatTypeBigfloat -> "bigfloat"
        FloatTypeFloat32 -> "float32"
        FloatTypeFloat64 -> "float64"
      LiteralTypeInteger it -> case it of
        IntegerTypeBigint -> "bigint"
        IntegerTypeInt8 -> "int8"
        IntegerTypeInt16 -> "int16"
        IntegerTypeInt32 -> "int32"
        IntegerTypeInt64 -> "int64"
        IntegerTypeUint8 -> "uint8"
        IntegerTypeUint16 -> "uint16"
        IntegerTypeUint32 -> "uint32"
        IntegerTypeUint64 -> "uint64"
      LiteralTypeString -> "string"
    TypeMap (MapType keyTyp valTyp) -> "map<" ++ showType keyTyp ++ ", " ++ showType valTyp ++ ">"
    TypeOptional etyp -> "optional<" ++ showType etyp ++ ">"
    TypeProduct types -> L.intercalate "×" (fmap showType types)
    TypeRecord rt -> "record" ++ showRowType rt
    TypeSet etyp -> "set<" ++ showType etyp ++ ">"
    TypeUnion rt -> "union" ++ showRowType rt
    TypeVariable (Name name) -> name
    TypeWrap (WrappedType tname typ1) -> "wrap[" ++ unName tname ++ "](" ++ showType typ1 ++ ")"
    t -> show t
  where
    showFieldType (FieldType fname typ) = unName fname ++ " = " ++ showType typ
    showRowType (RowType _ fields) = "{" ++ (L.intercalate ", " $ fmap showFieldType fields) ++ "}"

showTypeConstraint :: TypeConstraint -> String
showTypeConstraint (TypeConstraint ltyp rtyp _) = showType ltyp ++ "≡" ++ showType rtyp

showTypeScheme :: TypeScheme -> String
showTypeScheme (TypeScheme vars body) = fa ++ showType body
  where
    fa = if L.null vars then "" else "∀[" ++ (L.intercalate "," (fmap (\(Name name) -> name) vars)) ++ "]."

showTypeSubst :: TypeSubst -> String
showTypeSubst (TypeSubst subst) = "{" ++ (L.intercalate "," (fmap (\(Name name, typ) -> name ++ "↦" ++ showType typ) (M.toList subst))) ++ "}"
