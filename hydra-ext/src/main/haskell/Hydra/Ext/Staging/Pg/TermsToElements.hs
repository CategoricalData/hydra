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
import qualified Hydra.Lib.Literals as Literals

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Maybe as Y


type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

unexpectedE :: Context -> String -> String -> Result a
unexpectedE cx expected found = err cx $ "Expected " ++ expected ++ ", found: " ++ found

type PgAdapter v = Adapter Type [PG.Label] Term [PG.Element v]

key_elements = Name "elements"

expectList :: Context -> Graph -> (Context -> Graph -> Term -> Result x) -> Term -> Result [x]
expectList cx g f term = do
  elems <- ExtractCore.list cx g term
  CM.mapM (f cx g) elems

termToElementsAdapter :: Context -> Graph -> Schema s t v -> Type -> Result (PgAdapter v)
termToElementsAdapter cx g schema typ = do
    case getTypeAnnotation key_elements typ of
      Nothing -> pure trivialAdapter
      Just term -> do
        specs <- expectList cx g (\cx' g' t -> decodeElementSpec cx' g' t) term >>= CM.mapM (parseElementSpec cx g schema)
        let labels = L.nub (fst <$> specs)
        let encoders = snd <$> specs
        let encode cx' t = do
              results <- CM.mapM (\e -> e cx' t) encoders
              return $ L.concat results
        return $ Adapter lossy typ labels $ Coder (\cx' -> encode cx') (\cx' _els -> err cx' "element decoding is not yet supported")
  where
    trivialAdapter = Adapter False typ [] $ Coder (\_ _ -> Right []) (\cx' _ -> err cx' "no corresponding element type")

-- TODO; infer lossiness
lossy = False

parseEdgeIdPattern :: Context -> Graph -> Schema s t v -> ValueSpec -> Result (Context -> Term -> Result [v])
parseEdgeIdPattern cx g schema spec = do
  fun <- parseValueSpec cx g spec
  return $ \cx' term -> do
    terms <- fun cx' term
    CM.mapM (coderEncode (schemaEdgeIds schema) cx') terms

parseEdgeSpec :: Context -> Graph -> Schema s t v -> EdgeSpec -> Result (PG.Label, Context -> Term -> Result [PG.Element v])
parseEdgeSpec cx g schema (EdgeSpec label id outV inV props) = do
  getId <- parseEdgeIdPattern cx g schema id
  getOut <- parseVertexIdPattern cx g schema outV
  getIn <- parseVertexIdPattern cx g schema inV
  getProps <- CM.mapM (parsePropertySpec cx g schema) props
  let encode cx' term = do
        tid <- requireUnique cx' "edge id" (getId cx') term
        tout <- requireUnique cx' "vertex id" (getOut cx') term
        tin <- requireUnique cx' "edge id" (getIn cx') term
        tprops <- M.fromList <$> CM.mapM (\gf -> requireUnique cx' "property key" (gf cx') term) getProps
        return [PG.ElementEdge $ PG.Edge label tid tout tin tprops]
  return (PG.LabelEdge label, encode)

parseElementSpec :: Context -> Graph -> Schema s t v -> ElementSpec -> Result (PG.Label, Context -> Term -> Result [PG.Element v])
parseElementSpec cx g schema spec = case spec of
  ElementSpecVertex vspec -> parseVertexSpec cx g schema vspec
  ElementSpecEdge espec -> parseEdgeSpec cx g schema espec

parsePattern :: Context -> Graph -> String -> Result (Context -> Term -> Result [Term])
parsePattern cx _g pat = do
    (lits, paths) <- parsePatternInner [] [] "" pat
    return $ \cx' term -> traverse cx' lits paths term
  where
    parsePatternInner lits paths cur s = case s of
      [] -> pure (L.reverse (nextLit:lits), L.reverse paths)
      ('$':'{':rest) -> parsePath (nextLit:lits) paths "" rest
      (c:rest) -> parsePatternInner lits paths (c:cur) rest
      where
        nextLit = L.reverse cur
        parsePath lits' paths' cur' s' = case s' of
          [] -> err cx $ "Unfinished path expression: " ++ pat
          ('}':rest) -> parsePatternInner lits' (path:paths') "" rest
            where
              path = LS.splitOn "/" $ L.reverse cur'
          (c:rest) -> parsePath lits' paths' (c:cur') rest
    traverse cx' lits paths term = recurse [""] True lits paths
      where
        recurse values lp lits' paths' = if L.null values
            then pure []
            else if lp
            -- Try to apply a literal
            then case lits' of
              -- All done. The last segment is always a literal.
              [] -> return $ Terms.string <$> values
              -- Append the literal and continue traversing.
              (l:rest) -> recurse (append l) False rest paths'
            -- Try to apply a path
            else case paths' of
              -- No more paths; continue with literals
              [] -> recurse values True lits' []
              -- Apply the next path
              (path:rest) -> do
                  strings <- evalPath path term >>= CM.mapM toString
                  recurse (appendAll strings) True lits' rest
          where
            append s = fmap (\v -> v ++ s) values
            appendAll strings = L.concat (append <$> strings)
        evalPath path term' = case path of
            [] -> pure [term']
            (step:rest) -> do
              results <- evalStep step term'
              L.concat <$> (CM.mapM (evalPath rest) results)
          where
            evalStep step term'' = if L.null step
              then pure [term'']
              else case deannotateTerm term'' of
                  TermList terms -> L.concat <$> CM.mapM (evalStep step) terms
                  TermMaybe mt -> case mt of
                    Nothing -> pure []
                    Just t -> evalStep step t
                  TermRecord (Record _ fields) -> case M.lookup (Name step) (fieldMap fields) of
                    Nothing -> err cx' $ "No such field " ++ step ++ " in record: " ++ show term''
                    Just t -> pure [t]
                  TermUnion (Injection _ field) -> if unName (fieldName field) == step
                    then evalStep step $ fieldTerm field
                    else pure [] -- Note: not checking the step against the union type; assuming it is correct but that it references a field unused by the injection
                  TermWrap (WrappedTerm _ t) -> evalStep step t
                  _ -> err cx' $ "Can't traverse through term for step " ++ show step ++ ": " ++ show term''

        -- TODO: replace this with a more standard function
        toString term' = case deannotateTerm term' of
          TermLiteral lit -> pure $ case lit of
            LiteralBinary b -> Literals.binaryToStringBS b
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
          TermMaybe mt -> case mt of
            Nothing -> pure "nothing"
            Just t -> toString t
          _ -> pure $ show term'

parsePropertySpec :: Context -> Graph -> Schema s t v -> PropertySpec -> Result (Context -> Term -> Result [(PG.PropertyKey, v)])
parsePropertySpec cx g schema (PropertySpec key value) = do
  fun <- parseValueSpec cx g value
  return $ \cx' term -> do
    results <- fun cx' term
    values <- CM.mapM (coderEncode (schemaPropertyValues schema) cx') results
    return $ fmap (\v -> (key, v)) values

parseValueSpec :: Context -> Graph -> ValueSpec -> Result (Context -> Term -> Result [Term])
parseValueSpec cx g spec = case spec of
  ValueSpecValue -> pure $ \_ term -> pure [term]
  ValueSpecPattern pat -> parsePattern cx g pat

parseVertexIdPattern :: Context -> Graph -> Schema s t v -> ValueSpec -> Result (Context -> Term -> Result [v])
parseVertexIdPattern cx g schema spec = do
  fun <- parseValueSpec cx g spec
  return $ \cx' term -> do
    terms <- fun cx' term
    CM.mapM (coderEncode (schemaVertexIds schema) cx') terms

parseVertexSpec :: Context -> Graph -> Schema s t v -> VertexSpec -> Result (PG.Label, Context -> Term -> Result [PG.Element v])
parseVertexSpec cx g schema (VertexSpec label id props) = do
  getId <- parseVertexIdPattern cx g schema id
  getProps <- CM.mapM (parsePropertySpec cx g schema) props
  let encode cx' term = do
        tid <- requireUnique cx' "vertex id" (getId cx') term
        tprops <- M.fromList <$> CM.mapM (\gf -> requireUnique cx' "property key" (gf cx') term) getProps
        return [PG.ElementVertex $ PG.Vertex label tid tprops]
  return (PG.LabelVertex label, encode)

requireUnique :: Context -> String -> (Term -> Result [x]) -> Term -> Result x
requireUnique cx context fun term = do
  results <- fun term
  case results of
    [] -> err cx $ "No value found: " ++ context
    [value] -> pure value
    _ -> err cx $ "Multiple values found: " ++ context


-- Element spec decoding. TODO: this should code should really be generated rather than hand-written.

decodeEdgeLabel :: Context -> Graph -> Term -> Result PG.EdgeLabel
decodeEdgeLabel cx g t = PG.EdgeLabel <$> ExtractCore.string cx g t

decodeEdgeSpec :: Context -> Graph -> Term -> Result EdgeSpec
decodeEdgeSpec cx g term = readRecord cx g (\fields -> EdgeSpec
  <$> readField cx fields _EdgeSpec_label (decodeEdgeLabel cx g)
  <*> readField cx fields _EdgeSpec_id (decodeValueSpec cx g)
  <*> readField cx fields _EdgeSpec_out (decodeValueSpec cx g)
  <*> readField cx fields _EdgeSpec_in (decodeValueSpec cx g)
  <*> readField cx fields _EdgeSpec_properties (expectList cx g (\cx' g' t' -> decodePropertySpec cx' g' t'))) term

decodeElementSpec :: Context -> Graph -> Term -> Result ElementSpec
decodeElementSpec cx g term = readInjection cx g [
  (_ElementSpec_vertex, \t -> ElementSpecVertex <$> decodeVertexSpec cx g t),
  (_ElementSpec_edge, \t -> ElementSpecEdge <$> decodeEdgeSpec cx g t)] term

decodePropertyKey :: Context -> Graph -> Term -> Result PG.PropertyKey
decodePropertyKey cx g t = PG.PropertyKey <$> ExtractCore.string cx g t

decodePropertySpec :: Context -> Graph -> Term -> Result PropertySpec
decodePropertySpec cx g term = readRecord cx g (\fields -> PropertySpec
  <$> readField cx fields _PropertySpec_key (decodePropertyKey cx g)
  <*> readField cx fields _PropertySpec_value (decodeValueSpec cx g)) term

decodeValueSpec :: Context -> Graph -> Term -> Result ValueSpec
decodeValueSpec cx g term = case deannotateTerm term of
  -- Allow an abbreviated specification consisting of only the pattern string
  TermLiteral (LiteralString s) -> pure $ ValueSpecPattern s
  _ -> readInjection cx g [
    (_ValueSpec_value, \_ -> pure ValueSpecValue),
    (_ValueSpec_pattern, \t -> ValueSpecPattern <$> ExtractCore.string cx g t)] term

decodeVertexLabel :: Context -> Graph -> Term -> Result PG.VertexLabel
decodeVertexLabel cx g t = PG.VertexLabel <$> ExtractCore.string cx g t

decodeVertexSpec :: Context -> Graph -> Term -> Result VertexSpec
decodeVertexSpec cx g term = readRecord cx g (\fields -> VertexSpec
  <$> readField cx fields _VertexSpec_label (decodeVertexLabel cx g)
  <*> readField cx fields _VertexSpec_id (decodeValueSpec cx g)
  <*> readField cx fields _VertexSpec_properties (expectList cx g (\cx' g' t' -> decodePropertySpec cx' g' t'))) term


-- General-purpose code for decoding

readInjection :: Context -> Graph -> [(Name, Term -> Result x)] -> Term -> Result x
readInjection cx g cases encoded = do
  mp <- ExtractCore.map cx (\k -> Name <$> ExtractCore.string cx g k) Right g encoded
  f <- case M.toList mp of
    [] -> err cx "empty injection"
    [(k, v)] -> pure $ Field k v
    _ -> err cx $ "invalid injection: " ++ show mp
  case snd <$> (L.filter (\c -> fst c == fieldName f) cases) of
    [] -> err cx $ "unexpected field: " ++ unName (fieldName f)
    [fun] -> fun (fieldTerm f)
    _ -> err cx "duplicate field name in cases"

readRecord :: Context -> Graph -> (M.Map Name Term -> Result x) -> Term -> Result x
readRecord cx g cons term = do
  mp <- ExtractCore.map cx (\k -> Name <$> ExtractCore.string cx g k) Right g term
  cons mp

readField :: Context -> M.Map Name Term -> Name -> (Term -> Result a) -> Result a
readField cx fields fname fun = case M.lookup fname fields of
  Nothing -> err cx $ "no such field: " ++ unName fname
  Just t -> fun t
