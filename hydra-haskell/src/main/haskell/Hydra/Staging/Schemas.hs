-- | Various functions for dereferencing and decoding schema types

module Hydra.Staging.Schemas where

import Hydra.Annotations
import Hydra.Variants
import Hydra.Qnames
import Hydra.Strip
import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.Staging.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Mantle
import Hydra.Module
import Hydra.Staging.Lexical
import Hydra.Staging.Rewriting
import Hydra.Flows
import Hydra.Rewriting
import Hydra.Errors
import Hydra.Lexical
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


-- TODO: use TypeScheme here instead of Type
data Definition = DefinitionType Name Type | DefinitionTerm Name Term Type

data Namespaces n = Namespaces {
  namespacesFocus :: (Namespace, n),
  namespacesMapping :: M.Map Namespace n} deriving Show

definitionDependencyNamespaces :: [Definition] -> S.Set Namespace
definitionDependencyNamespaces defs = S.fromList $ Y.catMaybes (namespaceOf <$> S.toList allNames)
  where
    allNames = S.unions (defNames <$> defs)
    defNames def = case def of
        DefinitionType _ typ -> typeDependencyNames typ
        DefinitionTerm _ term _ -> termDependencyNames True True True term

-- | Find dependency namespaces in all of a set of terms.
dependencyNamespaces :: Bool -> Bool -> Bool -> Bool -> [Term] -> Flow Graph (S.Set Namespace)
dependencyNamespaces withVars withPrims withNoms withSchema terms = do
    allNames <- S.unions <$> (CM.mapM depNames terms)
    return $ S.fromList $ Y.catMaybes (namespaceOf <$> S.toList allNames)
  where
    depNames term = do
      let dataNames = termDependencyNames withVars withPrims withNoms term

      schemaNames <- if withSchema
        then typeDependencyNames <$> requireTermType term
        else pure S.empty

      typeNames <- if isEncodedType (fullyStripTerm term)
        then typeDependencyNames <$> coreDecodeType term
        else pure S.empty

      return $ S.unions [dataNames, schemaNames, typeNames]

dereferenceType :: Name -> Flow Graph (Maybe Type)
dereferenceType name = do
  mel <- dereferenceElement name
  case mel of
    Nothing -> return Nothing
    Just el -> Just <$> coreDecodeType (elementTerm el)

elementAsTypedTerm :: Element -> Flow Graph TypedTerm
elementAsTypedTerm el = do
  typ <- requireTermType $ elementTerm el
  return $ TypedTerm (elementTerm el) typ

expectRecordType :: Name -> Type -> Flow s [FieldType]
expectRecordType ename typ = case stripType typ of
  TypeRecord (RowType tname fields) -> if tname == ename
    then pure fields
    else unexpected ("record of type " ++ unName ename) $ "record of type " ++ unName tname
  _ -> unexpected "record type" $ show typ

expectUnionType :: Name -> Type -> Flow s [FieldType]
expectUnionType ename typ = case stripType typ of
  TypeUnion (RowType tname fields) -> if tname == ename
    then pure fields
    else unexpected ("union of type " ++ unName ename) $ "union of type " ++ unName tname
  _ -> unexpected "union type" $ show typ

expectWrappedType :: Name -> Type -> Flow s Type
expectWrappedType ename typ = case stripType typ of
    TypeWrap (WrappedType tname t) -> if tname == ename
      then pure t
      else unexpected ("wrapped type " ++ unName ename) $ "wrapped type " ++ unName tname
    _ -> unexpected "wrapped type" $ show typ

findFieldType :: Name -> [FieldType] -> Flow s Type
findFieldType fname fields = case L.filter (\(FieldType fn _) -> fn == fname) fields of
  [] -> fail $ "No such field: " ++ unName fname
  [f] -> pure $ fieldTypeType f
  _ -> fail $ "Multiple fields named " ++ unName fname

-- TODO: a graph is a let-expression, so it should be trivial to get the type scheme of an element upon lookup. See issue #159.
lookupTypedTerm :: Graph -> Name -> Maybe TypedTerm
lookupTypedTerm g name = do
  el <- lookupElement g name
  typ <- getTermType $ elementTerm el
  return $ TypedTerm (elementTerm el) typ

fieldTypes :: Type -> Flow Graph (M.Map Name Type)
fieldTypes t = case stripType t of
    TypeLambda (LambdaType _ body) -> fieldTypes body
    TypeRecord rt -> pure $ toMap $ rowTypeFields rt
    TypeUnion rt -> pure $ toMap $ rowTypeFields rt
    TypeVariable name -> do
      withTrace ("field types of " ++ unName name) $ do
        el <- requireElement name
        coreDecodeType (elementTerm el) >>= fieldTypes
    _ -> unexpected "record or union type" $ show t
  where
    toMap fields = M.fromList (toPair <$> fields)
    toPair (FieldType fname ftype) = (fname, ftype)

-- | Checks whether the fields of a 'RowType' are all unit-typed.
--   A union with such a 'RowType' can be treated as an enum in languages that support enums.
isEnumType :: RowType -> Bool
isEnumType (RowType _ tfields) = L.foldl (\b f -> b && isUnitType (fieldTypeType f)) True tfields

isSerializable :: Element -> Flow Graph Bool
isSerializable el = do
    deps <- typeDependencies id (elementName el)
    let allVariants = S.fromList $ L.concat (variants <$> M.elems deps)
    return $ not $ S.member TypeVariantFunction allVariants
  where
    variants typ = typeVariant <$> foldOverType TraversalOrderPre (\m t -> t:m) [] typ

-- | Find dependency namespaces in all of the elements of a module, excluding the module's own namespace.
moduleDependencyNamespaces :: Bool -> Bool -> Bool -> Bool -> Module -> Flow Graph (S.Set Namespace)
moduleDependencyNamespaces withVars withPrims withNoms withSchema mod =
  S.delete (moduleNamespace mod) <$> (dependencyNamespaces withVars withPrims withNoms withSchema $
    (elementTerm <$> moduleElements mod))

namespacesForDefinitions :: (Namespace -> a) -> Namespace -> [Definition] -> Namespaces a
namespacesForDefinitions encodeNamespace focusNs defs = Namespaces (toPair focusNs) $ M.fromList (toPair <$> S.toList nss)
  where
    nss = S.delete focusNs $ definitionDependencyNamespaces defs
    toPair ns = (ns, encodeNamespace ns)

requireRecordType :: Name -> Flow Graph RowType
requireRecordType = requireRowType "record type" $ \t -> case t of
  TypeRecord rt -> Just rt
  _ -> Nothing

requireRowType :: String -> (Type -> Maybe RowType) -> Name -> Flow Graph RowType
requireRowType label getter name = do
  t <- requireType name
  case getter (rawType t) of
    Just rt -> return rt
    Nothing -> fail $ show name ++ " does not resolve to a " ++ label ++ " type: " ++ show t
  where
    rawType t = case t of
      TypeAnnotated (AnnotatedType t' _) -> rawType t'
      TypeLambda (LambdaType _ body) -> rawType body -- Note: throwing away quantification here
      _ -> t

requireType :: Name -> Flow Graph Type
requireType name = withTrace ("require type " ++ unName name) $
  (withSchemaContext $ requireElement name) >>= (coreDecodeType . elementTerm)

requireUnionType :: Name -> Flow Graph RowType
requireUnionType = requireRowType "union" $ \t -> case t of
  TypeUnion rt -> Just rt
  _ -> Nothing

requireWrappedType :: Name -> Flow Graph Type
requireWrappedType name = do
  typ <- requireType name
  case stripType typ of
    TypeWrap (WrappedType name t) -> return t
    _ -> return typ -- TODO: stop allowing this "slop" once typedefs are clearly separated from newtypes
--     _ -> fail $ "expected wrapped type for " ++ unName name ++ " but got " ++ show typ

resolveType :: Type -> Flow Graph (Maybe Type)
resolveType typ = case stripType typ of
    TypeVariable name -> withSchemaContext $ do
      mterm <- resolveTerm name
      case mterm of
        Nothing -> pure Nothing
        Just t -> Just <$> coreDecodeType t
    _ -> pure $ Just typ

schemaGraphToTypingEnvironment :: Graph -> Flow s (M.Map Name TypeScheme)
schemaGraphToTypingEnvironment g = withState g $ do
    mpairs <- CM.mapM toPair $ M.elems $ graphElements g
    return $ M.fromList $ Y.catMaybes mpairs
  where
    toPair el = do
      mts <- case elementType el of
        Just ts ->
          if ts == (TypeScheme [] $ TypeVariable _TypeScheme)
          then fmap Just $ coreDecodeTypeScheme $ elementTerm el
          -- TODO: temporary; all schema elements should be encoded as type schemes, not "lambda types"
          else if ts == (TypeScheme [] $ TypeVariable _Type)
          then fmap Just (toTypeScheme [] <$> coreDecodeType (elementTerm el))
          -- TODO: temporary; the following handles the "bootstrapping" case where we have not yet applied type inference to the schema graph
          else case fullyStripTerm (elementTerm el) of
            TermRecord (Record tname _) -> if tname == _TypeScheme
              then fmap Just $ coreDecodeTypeScheme $ elementTerm el
              else pure Nothing
            TermUnion (Injection tname _) -> if tname == _Type
              then fmap Just (toTypeScheme [] <$> coreDecodeType (elementTerm el))
              else pure Nothing
        Nothing -> pure Nothing
      return $ fmap (\ts -> (elementName el, ts)) mts
    toTypeScheme vars typ = case stripType typ of
      TypeLambda (LambdaType v body) -> toTypeScheme (v:vars) body
      _ -> TypeScheme (L.reverse vars) typ

typeDependencies :: (Type -> Type) -> Name -> Flow Graph (M.Map Name Type)
typeDependencies transform name = deps (S.fromList [name]) M.empty
  where
    deps seeds names = if S.null seeds
        then return names
        else do
          pairs <- CM.mapM toPair $ S.toList seeds
          let newNames = M.union names (M.fromList pairs)
          let refs = L.foldl S.union S.empty (typeDependencyNames <$> (snd <$> pairs))
          let visited = S.fromList $ M.keys names
          let newSeeds = S.difference refs visited
          deps newSeeds newNames
      where
        toPair name = do
          typ <- transform <$> requireType name
          return (name, typ)

    requireType name = do
      withTrace ("type dependencies of " ++ unName name) $ do
        el <- requireElement name
        coreDecodeType (elementTerm el)
