-- | A utility which instantiates a nonrecursive type with default values.

module Hydra.Staging.Templating where

import Hydra.Kernel

import qualified Data.Map as M
import qualified Data.Set as S


-- | Create a graph schema from a graph which contains nothing but encoded type definitions.
graphToSchema :: Graph -> Flow Graph (M.Map Name Type)
graphToSchema g = M.fromList <$> (mapM toPair $ M.toList $ graphElements g)
  where
    toPair (name, el) = do
      t <- coreDecodeType $ elementTerm el
      return (name, t)

-- | Given a graph schema and a nonrecursive type, instantiate it with default values.
--   If the minimal flag is set, the smallest possible term is produced; otherwise,
--   exactly one subterm is produced for constructors which do not otherwise require one, e.g. in lists and optionals.
insantiateTemplate :: Bool -> M.Map Name Type -> Type -> Flow s Term
insantiateTemplate minimal schema t = case t of
    TypeAnnotated (AnnotatedType t _) -> inst t
    TypeApplication _ -> noPoly
    TypeFunction _ -> noPoly
    TypeForall _ -> noPoly
    TypeList et -> if minimal
      then pure $ TermList []
      else do
        e <- inst et
        return $ TermList [e]
    TypeLiteral lt -> pure $ TermLiteral $ case lt of
      LiteralTypeBinary -> LiteralString ""
      LiteralTypeBoolean -> LiteralBoolean False
      LiteralTypeInteger it -> LiteralInteger $ case it of
        IntegerTypeBigint -> IntegerValueBigint 0
        IntegerTypeInt8 -> IntegerValueInt8 0
        IntegerTypeInt16 -> IntegerValueInt16 0
        IntegerTypeInt32 -> IntegerValueInt32 0
        IntegerTypeInt64 -> IntegerValueInt64 0
        IntegerTypeUint8 -> IntegerValueUint8 0
        IntegerTypeUint16 -> IntegerValueUint16 0
        IntegerTypeUint32 -> IntegerValueUint32 0
        IntegerTypeUint64 -> IntegerValueUint64 0
      LiteralTypeFloat ft -> LiteralFloat $ case ft of
        FloatTypeBigfloat -> FloatValueBigfloat 0
        FloatTypeFloat32 -> FloatValueFloat32 0
        FloatTypeFloat64 -> FloatValueFloat64 0
      LiteralTypeString -> LiteralString ""
    TypeMap (MapType kt vt) -> if minimal
      then return $ TermMap M.empty
      else do
        ke <- inst kt
        ve <- inst vt
        return $ TermMap $ M.singleton ke ve
    TypeOptional ot -> if minimal
      then return $ TermOptional Nothing
      else do
        e <- inst ot
        return $ TermOptional $ Just e
    TypeProduct types -> do
      es <- mapM inst types
      return $ TermProduct es
    TypeRecord (RowType tname fields) -> do
      dfields <- mapM toField fields
      return $ TermRecord $ Record tname dfields
      where
        toField ft = do
          e <- inst $ fieldTypeType ft
          return $ Field (fieldTypeName ft) e
    TypeSet et -> if minimal
        then return $ TermSet S.empty
        else do
          e <- inst et
          return $ TermSet $ S.fromList [e]
--     TypeStream et -> ...
--     TypeSum types -> ...
--     TypeUnion (RowType tname _ fields) -> ...
    TypeVariable tname -> case M.lookup tname schema of
      Just t' -> inst t'
      Nothing -> fail $ "Type variable " ++ show tname ++ " not found in schema"
    TypeWrap (WrappedType tname t') -> do
      e <- inst t'
      return $ TermWrap $ WrappedTerm tname e
  where
    inst = insantiateTemplate minimal schema
    noPoly = fail "Polymorphic and function types are not currently supported"


{-

-- Example of type-to-term instantiation which creates a YAML-based template out of the OpenCypher feature model.

import Hydra.Ext.Yaml.Model as Yaml
import Hydra.Flows
import Data.Map as M
import Data.Maybe as Y

ff = fromFlowIo bootstrapGraph

schema <- ff $ graphToSchema $ modulesToGraph [openCypherFeaturesModule]

typ <- ff $ inlineType schema $ Y.fromJust $ M.lookup _CypherFeatures schema
term <- ff $ insantiateTemplate False schema typ

encoder <- ff (coderEncode <$> yamlCoder typ)
yaml <- ff $ encoder term
putStrLn $ hydraYamlToString yaml

-}
