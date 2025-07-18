module Hydra.Ext.Staging.Pg.TermsToElements (
  decodeValueSpec,
  parseValueSpec,
  termToElementsAdapter,
) where

import Hydra.Kernel
import Hydra.Pg.Mapping
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Maybe as Y


type PgAdapter s v = Adapter s s Type [PG.Label] Term [PG.Element v]

key_elements = Name "elements"

expectList :: (Term -> Flow s x) -> Term -> Flow s [x]
expectList f term = do
  s <- getState
  withEmptyGraph $ ExtractCore.list (\t -> withState s $ f t) term

termToElementsAdapter :: Schema s t v -> Type -> Flow s (PgAdapter s v)
termToElementsAdapter schema typ = do
    case getTypeAnnotation key_elements typ of
      Nothing -> pure trivialAdapter
      Just term -> do
        specs <- expectList decodeElementSpec term >>= CM.mapM (parseElementSpec schema)
        let labels = L.nub (fst <$> specs)
        let encoders = snd <$> specs
        let encode term = L.concat <$> CM.mapM (\e -> e term) encoders
        return $ Adapter lossy typ labels $ Coder encode (\els -> noDecoding "element")
  where
    trivialAdapter = Adapter False typ [] $ Coder (\term -> pure []) (\el -> fail "no corresponding element type")

-- TODO; infer lossiness
lossy = False

noDecoding :: String -> Flow s x
noDecoding cat = fail $ cat ++ " decoding is not yet supported"

parseEdgeIdPattern :: Schema s t v -> ValueSpec -> Flow s (Term -> Flow s [v])
parseEdgeIdPattern schema spec = do
  fun <- parseValueSpec spec
  return $ \term -> fun term >>= CM.mapM (coderEncode $ schemaEdgeIds schema)

parseEdgeSpec :: Schema s t v -> EdgeSpec -> Flow s (PG.Label, Term -> Flow s [PG.Element v])
parseEdgeSpec schema (EdgeSpec label id outV inV props) = do
  getId <- parseEdgeIdPattern schema id
  getOut <- parseVertexIdPattern schema outV
  getIn <- parseVertexIdPattern schema inV
  getProps <- CM.mapM (parsePropertySpec schema) props
  let encode term = withTrace "encode as edge" $ do
        tid <- requireUnique "edge id" getId term
        tout <- requireUnique "vertex id" getOut term
        tin <- requireUnique "edge id" getIn term
        tprops <- M.fromList <$> CM.mapM (\g -> requireUnique "property key" g term) getProps
        return [PG.ElementEdge $ PG.Edge label tid tout tin tprops]
  return (PG.LabelEdge label, encode)

parseElementSpec :: Schema s t v -> ElementSpec -> Flow s (PG.Label, Term -> Flow s [PG.Element v])
parseElementSpec schema spec = case spec of
  ElementSpecVertex vspec -> parseVertexSpec schema vspec
  ElementSpecEdge espec -> parseEdgeSpec schema espec

parsePattern :: String -> Flow s (Term -> Flow s [Term])
parsePattern pat = withTrace "parse path pattern" $ do
    (lits, paths) <- parsePattern [] [] "" pat
    return $ traverse lits paths
  where
    parsePattern lits paths cur s = case s of
      [] -> pure (L.reverse (nextLit:lits), L.reverse paths)
      ('$':'{':rest) -> parsePath (nextLit:lits) paths "" rest
      (c:rest) -> parsePattern lits paths (c:cur) rest
      where
        nextLit = L.reverse cur
        parsePath lits paths cur s = case s of
          [] -> fail $ "Unfinished path expression: " ++ pat
          ('}':rest) -> parsePattern lits (path:paths) "" rest
            where
              path = LS.splitOn "/" $ L.reverse cur
          (c:rest) -> parsePath lits paths (c:cur) rest
    traverse lits paths term = withTrace ("traverse pattern: " ++ pat) $ recurse [""] True lits paths
      where
        recurse values lp lits paths = if L.null values
            then pure []
            else if lp
            -- Try to apply a literal
            then case lits of
              -- All done. The last segment is always a literal.
              [] -> return $ Terms.string <$> values
              -- Append the literal and continue traversing.
              (l:rest) -> recurse (append l) False rest paths
            -- Try to apply a path
            else case paths of
              -- No more paths; continue with literals
              [] -> recurse values True lits []
              -- Apply the next path
              (path:rest) -> do
                  strings <- evalPath path term >>= CM.mapM toString
                  recurse (appendAll strings) True lits rest
          where
            append s = fmap (\v -> v ++ s) values
            appendAll strings = L.concat (append <$> strings)
    evalPath path term = case path of
        [] -> pure [term]
        (step:rest) -> do
          results <- evalStep step term
          L.concat <$> (CM.mapM (evalPath rest) results)
      where
        evalStep step term = if L.null step
          then pure [term]
          else case deannotateTerm term of
              TermList terms -> L.concat <$> CM.mapM (evalStep step) terms
              TermOptional mt -> case mt of
                Nothing -> pure []
                Just term' -> evalStep step term'
              TermRecord (Record _ fields) -> case M.lookup (Name step) (fieldMap fields) of
                Nothing -> fail $ "No such field " ++ step ++ " in record: " ++ show term
                Just term' -> pure [term']
              TermUnion (Injection _ field) -> if unName (fieldName field) == step
                then evalStep step $ fieldTerm field
                else pure [] -- Note: not checking the step against the union type; assuming it is correct but that it references a field unused by the injection
              TermWrap (WrappedTerm _ term') -> evalStep step term'
              _ -> fail $ "Can't traverse through term for step " ++ show step ++ ": " ++ show term

    -- TODO: replace this with a more standard function
    toString term = case deannotateTerm term of
      TermLiteral lit -> pure $ case lit of
        LiteralBinary b -> b
        LiteralBoolean b -> show b
        LiteralInteger i -> case i of
          IntegerValueBigint v -> show v
          IntegerValueInt8 v -> show v
          IntegerValueInt16 v -> show v
          IntegerValueInt32 v -> show v
          IntegerValueInt64 v -> show v
          IntegerValueUint8 v -> show v
          IntegerValueUint16 v -> show v
          IntegerValueUint32 v -> show v
          IntegerValueUint64 v -> show v
        LiteralFloat f -> case f of
          FloatValueBigfloat v -> show v
          FloatValueFloat32 v -> show v
          FloatValueFloat64 v -> show v
        LiteralString s -> s
      TermOptional mt -> case mt of
        Nothing -> pure "nothing"
        Just t -> toString t
      _ -> pure $ show term

parsePropertySpec :: Schema s t v -> PropertySpec -> Flow s (Term -> Flow s [(PG.PropertyKey, v)])
parsePropertySpec schema (PropertySpec key value) = withTrace "parse property spec" $ do
  fun <- parseValueSpec value
  return $ \term -> withTrace ("encode property " ++ PG.unPropertyKey key) $ do
    results <- fun term
    values <- CM.mapM (coderEncode $ schemaPropertyValues schema) results
    return $ fmap (\v -> (key, v)) values

parseValueSpec :: ValueSpec -> Flow s (Term -> Flow s [Term])
parseValueSpec spec = case spec of
  ValueSpecValue -> pure $ \term -> pure [term]
  ValueSpecPattern pat -> parsePattern pat

parseVertexIdPattern :: Schema s t v -> ValueSpec -> Flow s (Term -> Flow s [v])
parseVertexIdPattern schema spec = do
  fun <- parseValueSpec spec
  return $ \term -> fun term >>= CM.mapM (coderEncode $ schemaVertexIds schema)

parseVertexSpec :: Schema s t v -> VertexSpec -> Flow s (PG.Label, Term -> Flow s [PG.Element v])
parseVertexSpec schema (VertexSpec label id props) = do
  getId <- parseVertexIdPattern schema id
  getProps <- CM.mapM (parsePropertySpec schema) props
  let encode term = withTrace "encode as vertex" $ do
        tid <- requireUnique "vertex id" getId term
        tprops <- M.fromList <$> CM.mapM (\g -> requireUnique "property key" g term) getProps
        return [PG.ElementVertex $ PG.Vertex label tid tprops]
  return (PG.LabelVertex label, encode)

requireUnique :: String -> (Term -> Flow s [x]) -> Term -> Flow s x
requireUnique context fun term = do
  results <- fun term
  case results of
    [] -> fail $ "No value found: " ++ context
    [value] -> pure value
    _ -> fail $ "Multiple values found: " ++ context


-- Element spec decoding. TODO: this should code should really be generated rather than hand-written.

decodeEdgeLabel :: Term -> Flow s PG.EdgeLabel
decodeEdgeLabel t = PG.EdgeLabel <$> withEmptyGraph (ExtractCore.string t)

decodeEdgeSpec :: Term -> Flow s EdgeSpec
decodeEdgeSpec term = withTrace "decode edge spec" $ readRecord (\fields -> EdgeSpec
  <$> readField fields _EdgeSpec_label decodeEdgeLabel
  <*> readField fields _EdgeSpec_id decodeValueSpec
  <*> readField fields _EdgeSpec_out decodeValueSpec
  <*> readField fields _EdgeSpec_in decodeValueSpec
  <*> readField fields _EdgeSpec_properties (expectList decodePropertySpec)) term

decodeElementSpec :: Term -> Flow s ElementSpec
decodeElementSpec term = withTrace "decode element spec" $ readInjection [
  (_ElementSpec_vertex, \t -> ElementSpecVertex <$> decodeVertexSpec t),
  (_ElementSpec_edge, \t -> ElementSpecEdge <$> decodeEdgeSpec t)] term

decodePropertyKey :: Term -> Flow s PG.PropertyKey
decodePropertyKey t = PG.PropertyKey <$> withEmptyGraph (ExtractCore.string t)

decodePropertySpec :: Term -> Flow s PropertySpec
decodePropertySpec term = withTrace "decode property spec" $ readRecord (\fields -> PropertySpec
  <$> readField fields _PropertySpec_key decodePropertyKey
  <*> readField fields _PropertySpec_value decodeValueSpec) term

decodeValueSpec :: Term -> Flow s ValueSpec
decodeValueSpec term = withTrace "decode value spec" $ case deannotateTerm term of
  -- Allow an abbreviated specification consisting of only the pattern string
  TermLiteral (LiteralString s) -> pure $ ValueSpecPattern s
  _ -> readInjection [
    (_ValueSpec_value, \t -> pure ValueSpecValue),
    (_ValueSpec_pattern, \t -> ValueSpecPattern <$> withEmptyGraph (ExtractCore.string t))] term

decodeVertexLabel :: Term -> Flow s PG.VertexLabel
decodeVertexLabel t = PG.VertexLabel <$> withEmptyGraph (ExtractCore.string t)

decodeVertexSpec :: Term -> Flow s VertexSpec
decodeVertexSpec term = withTrace "decode vertex spec" $ readRecord (\fields -> VertexSpec
  <$> readField fields _VertexSpec_label decodeVertexLabel
  <*> readField fields _VertexSpec_id decodeValueSpec
  <*> readField fields _VertexSpec_properties (expectList decodePropertySpec)) term


-- General-purpose code for decoding

readInjection :: [(Name, Term -> Flow s x)] -> Term -> Flow s x
readInjection cases encoded = do
  mp <- withEmptyGraph (ExtractCore.map (\k -> Name <$> ExtractCore.string k) pure encoded)
  f <- case M.toList mp of
    [] -> fail "empty injection"
    [(k, v)] -> pure $ Field k v
    _ -> fail $ "invalid injection: " ++ show mp
  case snd <$> (L.filter (\c -> fst c == fieldName f) cases) of
    [] -> fail $ "unexpected field: " ++ unName (fieldName f)
    [fun] -> fun (fieldTerm f)
    _ -> fail "duplicate field name in cases"

readRecord :: (M.Map Name Term -> Flow s x) -> Term -> Flow s x
readRecord cons term = withEmptyGraph (ExtractCore.map (\k -> Name <$> ExtractCore.string k) pure term) >>= cons

readField fields fname fun = case M.lookup fname fields of
  Nothing -> fail $ "no such field: " ++ unName fname
  Just t -> fun t
