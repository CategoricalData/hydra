-- | Various functions for dereferencing and decoding schema types

module Hydra.Staging.Schemas where

import Hydra.Annotations
import Hydra.Variants
import Hydra.Qnames
import Hydra.Strip
import Hydra.Coders
import Hydra.Compute
import Hydra.Constants
import Hydra.Core
import Hydra.Staging.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Mantle
import Hydra.Module
import Hydra.Staging.Lexical
import Hydra.Staging.Rewriting
import Hydra.Staging.Sorting
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


definitionDependencyNamespaces :: Bool -> [Definition] -> S.Set Namespace
definitionDependencyNamespaces excludeUnit defs = S.fromList $ Y.catMaybes (namespaceOf <$> S.toList allNames)
  where
    allNames = S.unions (defNames <$> defs)
    defNames def = case def of
        DefinitionType (TypeDefinition _ typ) -> typeDependencyNames True excludeUnit typ
        DefinitionTerm (TermDefinition _ term _) -> termDependencyNames True True True term

-- | Find dependency namespaces in all of a set of terms.
dependencyNamespaces :: Bool -> Bool -> Bool -> Bool -> [Element] -> Flow Graph (S.Set Namespace)
dependencyNamespaces withVars withPrims withNoms withSchema els = do
    allNames <- S.delete placeholderName . S.unions <$> (CM.mapM depNames els)
    return $ S.fromList $ Y.catMaybes (namespaceOf <$> S.toList allNames)
  where
    depNames el = do
        typeNames <- if isEncodedType (fullyStripTerm term)
          then typeDependencyNames True True <$> coreDecodeType term
          else pure S.empty

        return $ S.unions [dataNames, schemaNames, typeNames]
      where
        term = elementTerm el
        dataNames = termDependencyNames withVars withPrims withNoms term
        schemaNames = if withSchema
          then case elementType el of
            Nothing -> S.empty
            Just ts -> typeDependencyNames True True (typeSchemeType ts)
          else S.empty

dereferenceType :: Name -> Flow Graph (Maybe Type)
dereferenceType name = do
  mel <- dereferenceElement name
  case mel of
    Nothing -> return Nothing
    Just el -> Just <$> coreDecodeType (elementTerm el)

elementAsTypedTerm :: Element -> Flow Graph TypedTerm
elementAsTypedTerm el = case elementType el of
  Nothing -> fail "missing element type"
  Just ts -> return $ TypedTerm (elementTerm el) (typeSchemeType ts)

findFieldType :: Name -> [FieldType] -> Flow s Type
findFieldType fname fields = case L.filter (\(FieldType fn _) -> fn == fname) fields of
  [] -> fail $ "No such field: " ++ unName fname
  [f] -> pure $ fieldTypeType f
  _ -> fail $ "Multiple fields named " ++ unName fname

fieldTypes :: Type -> Flow Graph (M.Map Name Type)
fieldTypes t = case stripType t of
    TypeForall (ForallType _ body) -> fieldTypes body
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

fullyStripType :: Type -> Type
fullyStripType typ = case stripType typ of
  TypeForall (ForallType _ body) -> fullyStripType body
  t -> t

-- | Checks whether the fields of a 'RowType' are all unit-typed.
--   A union with such a 'RowType' can be treated as an enum in languages that support enums.
isEnumRowType :: RowType -> Bool
isEnumRowType (RowType _ tfields) = L.foldl (\b f -> b && isUnitType (fieldTypeType f)) True tfields

isEnumType :: Type -> Bool
isEnumType typ = case stripType typ of
  TypeUnion rt -> isEnumRowType rt
  _ -> False

isSerializable :: Element -> Flow Graph Bool
isSerializable el = do
    deps <- typeDependencies False id (elementName el)
    let allVariants = S.fromList $ L.concat (variants <$> M.elems deps)
    return $ not $ S.member TypeVariantFunction allVariants
  where
    variants typ = typeVariant <$> foldOverType TraversalOrderPre (\m t -> t:m) [] typ

-- | Find dependency namespaces in all of the elements of a module, excluding the module's own namespace.
moduleDependencyNamespaces :: Bool -> Bool -> Bool -> Bool -> Module -> Flow Graph (S.Set Namespace)
moduleDependencyNamespaces withVars withPrims withNoms withSchema mod =
  S.delete (moduleNamespace mod) <$> (dependencyNamespaces withVars withPrims withNoms withSchema $
    (moduleElements mod))

namespacesForDefinitions :: Bool -> (Namespace -> a) -> Namespace -> [Definition] -> Namespaces a
namespacesForDefinitions excludeUnit encodeNamespace focusNs defs = Namespaces (toPair focusNs) $ M.fromList (toPair <$> S.toList nss)
  where
    nss = S.delete focusNs $ definitionDependencyNamespaces excludeUnit defs
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
      TypeForall (ForallType _ body) -> rawType body -- Note: throwing away quantification here
      _ -> t

requireType :: Name -> Flow Graph Type
requireType name = withTrace ("require type " ++ unName name) $
  (withSchemaContext $ requireElement name) >>= (coreDecodeType . elementTerm)

requireUnionType :: Name -> Flow Graph RowType
requireUnionType = requireRowType "union" $ \t -> case t of
  TypeUnion rt -> Just rt
  _ -> Nothing

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
      TypeForall (ForallType v body) -> toTypeScheme (v:vars) body
      _ -> TypeScheme (L.reverse vars) typ

topologicalSortTypeDefinitions :: [TypeDefinition] -> [[TypeDefinition]]
topologicalSortTypeDefinitions defs = fmap (Y.catMaybes . fmap (\n -> M.lookup n nameToDef)) sorted
  where
    sorted = topologicalSortComponents (toPair <$> defs)
    toPair def@(TypeDefinition name typ) = (typeDefinitionName def, S.toList $ typeDependencyNames False True typ)
    nameToDef = M.fromList $ L.map (\d -> (typeDefinitionName d, d)) defs

typeDependencies :: Bool -> (Type -> Type) -> Name -> Flow Graph (M.Map Name Type)
typeDependencies withSchema transform name = deps (S.fromList [name]) M.empty
  where
    deps seeds names = if S.null seeds
        then return names
        else do
          pairs <- CM.mapM toPair $ S.toList seeds
          let newNames = M.union names (M.fromList pairs)
          let refs = L.foldl S.union S.empty (typeDependencyNames withSchema True <$> (snd <$> pairs))
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
