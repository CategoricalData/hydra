module Hydra.Langs.Tinkerpop.TermsToElements (
  decodeValueSpec,
  parseValueSpec,
  termToElementsAdapter,
) where

import Hydra.Kernel
import Hydra.Langs.Tinkerpop.Mappings
import qualified Hydra.Langs.Tinkerpop.PropertyGraph as PG
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


type PgAdapter s a v e p = Adapter s s (Type a) [PG.Label] (Term a) [PG.Element v e p]

termToElementsAdapter :: Schema s Kv t v e p -> Type Kv -> Flow s (PgAdapter s Kv v e p)
termToElementsAdapter schema typ = do
    case getTypeAnnotation "elements" typ of
      Nothing -> pure trivialAdapter
      Just term -> do
        specs <- Expect.list decodeElementSpec term >>= CM.mapM (parseElementSpec schema)
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

parseEdgeIdPattern :: Show a => Schema s a t v e p -> ValueSpec -> Flow s (Term a -> Flow s [e])
parseEdgeIdPattern schema spec = do
  fun <- parseValueSpec spec
  return $ \term -> fun term >>= CM.mapM (coderEncode $ schemaEdgeIds schema)

parseEdgeSpec :: Show a => Schema s a t v e p -> EdgeSpec -> Flow s (PG.Label, Term a -> Flow s [PG.Element v e p])
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

parsePattern :: Show a => String -> Flow s (Term a -> Flow s [Term a])
parsePattern pat = withTrace "parse path pattern" $ do
    (lits, paths) <- parseLit [] [] "" pat
    return $ traverseTop [""] True lits paths
  where
    parseLit lits paths cur s = case s of
      [] -> pure ((L.reverse cur):lits, L.reverse paths)
      ('$':'{':rest) -> parsePath (cur:lits) paths "" rest
      (c:rest) -> parseLit lits paths (c:cur) rest
    parsePath lits paths cur s = case s of
      [] -> fail $ "Unfinished path expression: " ++ pat
      ('}':rest) -> parseLit lits ((L.reverse cur):paths) "" rest
      (c:rest) -> parsePath lits paths (c:cur) rest
    traverseTop values lp lits paths term = withTrace ("traverse pattern: " ++ pat) $
      traverse values lp lits paths term
    traverse values lp lits paths term = if L.null values
        then pure []
        else if lp
        then case lits of
          [] -> return $ Terms.string <$> values
          (l:rest) -> traverse (append l) False rest paths term
        else case paths of
          [] -> traverse values True lits [] term
          (step:rest) -> do
              strings <- evalStep step term >>= CM.mapM toString
              traverse (appendAll strings) True lits rest term
            where
              toString = Expect.string
      where
        append s = fmap (\v -> v ++ s) values
        appendAll strings = L.concat (append <$> strings)
    evalStep step term = if L.null step
      then pure [term]
      else case stripTerm term of
          TermList terms -> L.concat <$> CM.mapM (evalStep step) terms
          TermRecord (Record _ fields) -> case M.lookup (FieldName step) (fieldMap fields) of
            Nothing -> fail $ "No such field " ++ step ++ " in record: " ++ show term
            Just term' -> pure [term']
          TermOptional mt -> case mt of
            Nothing -> pure []
            Just term' -> evalStep step term'
          TermUnion (Injection _ field) -> if unFieldName (fieldName field) == step
            then evalStep step $ fieldTerm field
            else pure [] -- Note: not checking the step against the union type; assuming it is correct but that it references a field unused by the injection
          TermWrap (Nominal _ term') -> evalStep step term'
          _ -> fail $ "Can't traverse through term for step " ++ show step ++ ": " ++ show term

parsePropertySpec :: Show a => Schema s a t v e p -> PropertySpec -> Flow s (Term a -> Flow s [(PG.PropertyKey, p)])
parsePropertySpec schema (PropertySpec key value) = withTrace "parse property spec" $ do
  fun <- parseValueSpec value
  return $ \term -> withTrace ("encode property " ++ PG.unPropertyKey key) $ do
    results <- fun term
    values <- CM.mapM (coderEncode $ schemaPropertyValues schema) results
    return $ fmap (\v -> (key, v)) values

parseElementSpec :: Show a => Schema s a t v e p -> ElementSpec -> Flow s (PG.Label, Term a -> Flow s [PG.Element v e p])
parseElementSpec schema spec = case spec of
  ElementSpecVertex vspec -> parseVertexSpec schema vspec
  ElementSpecEdge espec -> parseEdgeSpec schema espec

parseValueSpec :: Show a => ValueSpec -> Flow s (Term a -> Flow s [Term a])
parseValueSpec spec = case spec of
  ValueSpecValue -> pure $ \term -> pure [term]
  ValueSpecPattern pat -> parsePattern pat
--  _ -> fail $ "Unsupported value pattern: " ++ show spec

parseVertexIdPattern :: Show a => Schema s a t v e p -> ValueSpec -> Flow s (Term a -> Flow s [v])
parseVertexIdPattern schema spec = do
  fun <- parseValueSpec spec
  return $ \term -> fun term >>= CM.mapM (coderEncode $ schemaVertexIds schema)

parseVertexSpec :: Show a => Schema s a t v e p -> VertexSpec -> Flow s (PG.Label, Term a -> Flow s [PG.Element v e p])
parseVertexSpec schema (VertexSpec label id props) = do
  getId <- parseVertexIdPattern schema id
  getProps <- CM.mapM (parsePropertySpec schema) props
  let encode term = withTrace "encode as vertex" $ do
        tid <- requireUnique "vertex id" getId term
        tprops <- M.fromList <$> CM.mapM (\g -> requireUnique "property key" g term) getProps
        return [PG.ElementVertex $ PG.Vertex label tid tprops]
  return (PG.LabelVertex label, encode)

requireUnique :: String -> (Term a -> Flow s [x]) -> Term a -> Flow s x
requireUnique context fun term = do
  results <- fun term
  case results of
    [] -> fail $ "No value found: " ++ context
    [value] -> pure value
    _ -> fail $ "Multiple values found: " ++ context


-- Element spec decoding. TODO: this should code should really be generated rather than hand-written.

decodeEdgeLabel :: Show a => Term a -> Flow s PG.EdgeLabel
decodeEdgeLabel t = PG.EdgeLabel <$> Expect.string t

decodeEdgeSpec :: Show a => Term a -> Flow s EdgeSpec
decodeEdgeSpec term = withTrace "decode edge spec" $ matchRecord (\fields -> EdgeSpec
  <$> readField fields _EdgeSpec_label decodeEdgeLabel
  <*> readField fields _EdgeSpec_id decodeValueSpec
  <*> readField fields _EdgeSpec_out decodeValueSpec
  <*> readField fields _EdgeSpec_in decodeValueSpec
  <*> readField fields _EdgeSpec_properties (Expect.list decodePropertySpec)) term

decodeElementSpec :: Show a => Term a -> Flow s ElementSpec
decodeElementSpec term = withTrace "decode element spec" $ matchInjection [
  (_ElementSpec_vertex, \t -> ElementSpecVertex <$> decodeVertexSpec t),
  (_ElementSpec_edge, \t -> ElementSpecEdge <$> decodeEdgeSpec t)] term

decodePropertyKey :: Show a => Term a -> Flow s PG.PropertyKey
decodePropertyKey t = PG.PropertyKey <$> Expect.string t

decodePropertySpec :: Show a => Term a -> Flow s PropertySpec
decodePropertySpec term = withTrace "decode property spec" $ matchRecord (\fields -> PropertySpec
  <$> readField fields _PropertySpec_key decodePropertyKey
  <*> readField fields _PropertySpec_value decodeValueSpec) term

decodeValueSpec :: Show a => Term a -> Flow s ValueSpec
decodeValueSpec term = withTrace "decode value spec" $ case stripTerm term of
  -- Allow an abbreviated specification consisting of only the pattern string
  TermLiteral (LiteralString s) -> pure $ ValueSpecPattern s
  _ -> matchInjection [
    (_ValueSpec_value, \t -> pure ValueSpecValue),
    (_ValueSpec_pattern, \t -> ValueSpecPattern <$> Expect.string t)] term

decodeVertexLabel :: Show a => Term a -> Flow s PG.VertexLabel
decodeVertexLabel t = PG.VertexLabel <$> Expect.string t

decodeVertexSpec :: Show a => Term a -> Flow s VertexSpec
decodeVertexSpec term = withTrace "decode vertex spec" $ matchRecord (\fields -> VertexSpec
  <$> readField fields _VertexSpec_label decodeVertexLabel
  <*> readField fields _VertexSpec_id decodeValueSpec
  <*> readField fields _VertexSpec_properties (Expect.list decodePropertySpec)) term


-- General-purpose code for decoding

fieldMap :: [Field a] -> M.Map FieldName (Term a)
fieldMap fields = M.fromList (toPair <$> fields)
  where
    toPair f = (fieldName f, fieldTerm f)

matchInjection :: Show a => [(FieldName, Term a -> Flow s x)] -> Term a -> Flow s x
matchInjection cases encoded = do
  mp <- Expect.map (\k -> FieldName <$> Expect.string k) pure encoded
  f <- case M.toList mp of
    [] -> fail "empty injection"
    [(k, v)] -> pure $ Field k v
    _ -> fail $ "invalid injection: " ++ show mp
  case snd <$> (L.filter (\c -> fst c == fieldName f) cases) of
    [] -> fail $ "unexpected field: " ++ unFieldName (fieldName f)
    [fun] -> fun (fieldTerm f)
    _ -> fail "duplicate field name in cases"

matchRecord :: Show a => (M.Map FieldName (Term a) -> Flow s x) -> Term a -> Flow s x
matchRecord cons term = Expect.map (\k -> FieldName <$> Expect.string k) pure term >>= cons

readField fields fname fun = case M.lookup fname fields of
  Nothing -> fail $ "no such field: " ++ unFieldName fname
  Just t -> fun t
