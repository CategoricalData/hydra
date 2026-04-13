-- Note: this is an automatically generated file. Do not edit.

-- | Functions for mapping Hydra terms to property graph elements using mapping specifications

module Hydra.Pg.TermsToElements where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Mapping as Mapping
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Apply a parsed pattern to a term, producing string terms
applyPattern :: t0 -> String -> [([String], String)] -> Core.Term -> Either Errors.Error [Core.Term]
applyPattern cx firstLit pairs term =
    Logic.ifElse (Lists.null pairs) (Right [
      Core.TermLiteral (Core.LiteralString firstLit)]) (Eithers.bind (Eithers.mapList (\pp -> Eithers.map (\terms -> (Lists.map (\t -> termToString t) terms, (Pairs.second pp))) (evalPath cx (Pairs.first pp) term)) pairs) (\evaluated -> Right (Lists.map (\s -> Core.TermLiteral (Core.LiteralString s)) (Lists.foldl (\accum -> \ep ->
      let pStrs = Pairs.first ep
          litP = Pairs.second ep
      in (Lists.concat (Lists.map (\pStr -> Lists.map (\a -> Strings.cat2 (Strings.cat2 a pStr) litP) accum) pStrs))) [
      firstLit] evaluated))))

-- | Decode an edge label from a term
decodeEdgeLabel :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Model.EdgeLabel
decodeEdgeLabel cx g t = Eithers.map (\_x -> Model.EdgeLabel _x) (Core_.string g t)

-- | Decode an edge specification from a term
decodeEdgeSpec :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Mapping.EdgeSpec
decodeEdgeSpec cx g term =
    readRecord cx g (\fields -> Eithers.bind (readField cx fields (Core.Name "label") (decodeEdgeLabel cx g)) (\_a -> Eithers.bind (readField cx fields (Core.Name "id") (decodeValueSpec cx g)) (\_b -> Eithers.bind (readField cx fields (Core.Name "out") (decodeValueSpec cx g)) (\_c -> Eithers.bind (readField cx fields (Core.Name "in") (decodeValueSpec cx g)) (\_d -> Eithers.map (\_e -> Mapping.EdgeSpec {
      Mapping.edgeSpecLabel = _a,
      Mapping.edgeSpecId = _b,
      Mapping.edgeSpecOut = _c,
      Mapping.edgeSpecIn = _d,
      Mapping.edgeSpecProperties = _e}) (readField cx fields (Core.Name "properties") (expectList cx g decodePropertySpec))))))) term

-- | Decode an element specification from a term
decodeElementSpec :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Mapping.ElementSpec
decodeElementSpec cx g term =
    readInjection cx g [
      (Core.Name "vertex", (\t -> Eithers.map (\_x -> Mapping.ElementSpecVertex _x) (decodeVertexSpec cx g t))),
      (Core.Name "edge", (\t -> Eithers.map (\_x -> Mapping.ElementSpecEdge _x) (decodeEdgeSpec cx g t)))] term

-- | Decode a property key from a term
decodePropertyKey :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Model.PropertyKey
decodePropertyKey cx g t = Eithers.map (\_x -> Model.PropertyKey _x) (Core_.string g t)

-- | Decode a property specification from a term
decodePropertySpec :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Mapping.PropertySpec
decodePropertySpec cx g term =
    readRecord cx g (\fields -> Eithers.bind (readField cx fields (Core.Name "key") (decodePropertyKey cx g)) (\_a -> Eithers.map (\_b -> Mapping.PropertySpec {
      Mapping.propertySpecKey = _a,
      Mapping.propertySpecValue = _b}) (readField cx fields (Core.Name "value") (decodeValueSpec cx g)))) term

-- | Decode a value specification from a term
decodeValueSpec :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Mapping.ValueSpec
decodeValueSpec cx g term =
    case (Strip.deannotateTerm term) of
      Core.TermLiteral v0 -> case v0 of
        Core.LiteralString v1 -> Right (Mapping.ValueSpecPattern v1)
        _ -> readInjection cx g [
          (Core.Name "value", (\_ -> Right Mapping.ValueSpecValue)),
          (Core.Name "pattern", (\t -> Eithers.map (\_x -> Mapping.ValueSpecPattern _x) (Core_.string g t)))] term
      _ -> readInjection cx g [
        (Core.Name "value", (\_ -> Right Mapping.ValueSpecValue)),
        (Core.Name "pattern", (\t -> Eithers.map (\_x -> Mapping.ValueSpecPattern _x) (Core_.string g t)))] term

-- | Decode a vertex label from a term
decodeVertexLabel :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Model.VertexLabel
decodeVertexLabel cx g t = Eithers.map (\_x -> Model.VertexLabel _x) (Core_.string g t)

-- | Decode a vertex specification from a term
decodeVertexSpec :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Mapping.VertexSpec
decodeVertexSpec cx g term =
    readRecord cx g (\fields -> Eithers.bind (readField cx fields (Core.Name "label") (decodeVertexLabel cx g)) (\_a -> Eithers.bind (readField cx fields (Core.Name "id") (decodeValueSpec cx g)) (\_b -> Eithers.map (\_c -> Mapping.VertexSpec {
      Mapping.vertexSpecLabel = _a,
      Mapping.vertexSpecId = _b,
      Mapping.vertexSpecProperties = _c}) (readField cx fields (Core.Name "properties") (expectList cx g decodePropertySpec))))) term

-- | Evaluate a path (list of steps) on a term, returning all resulting terms
evalPath :: t0 -> [String] -> Core.Term -> Either Errors.Error [Core.Term]
evalPath cx path term =
    Logic.ifElse (Lists.null path) (Right [
      term]) (Eithers.bind (evalStep cx (Lists.head path) term) (\results -> Eithers.map (\xs -> Lists.concat xs) (Eithers.mapList (evalPath cx (Lists.tail path)) results)))

-- | Evaluate a single step of a path traversal on a term
evalStep :: t0 -> String -> Core.Term -> Either Errors.Error [Core.Term]
evalStep cx step term =
    Logic.ifElse (Strings.null step) (Right [
      term]) (case (Strip.deannotateTerm term) of
      Core.TermList v0 -> Eithers.map (\xs -> Lists.concat xs) (Eithers.mapList (evalStep cx step) v0)
      Core.TermMaybe v0 -> Maybes.maybe (Right []) (\t -> evalStep cx step t) v0
      Core.TermRecord v0 -> Maybes.maybe (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "No such field " step) " in record")))) (\t -> Right [
        t]) (Maps.lookup (Core.Name step) (Resolution.fieldMap (Core.recordFields v0)))
      Core.TermUnion v0 -> Logic.ifElse (Equality.equal (Core.unName (Core.fieldName (Core.injectionField v0))) step) (evalStep cx step (Core.fieldTerm (Core.injectionField v0))) (Right [])
      Core.TermWrap v0 -> evalStep cx step (Core.wrappedTermBody v0)
      _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Can't traverse through term for step " step))))

-- | Extract a list from a term and apply a decoder to each element
expectList :: t0 -> Graph.Graph -> (t0 -> Graph.Graph -> Core.Term -> Either Errors.Error t1) -> Core.Term -> Either Errors.Error [t1]
expectList cx g f term = Eithers.bind (Core_.list g term) (\elems -> Eithers.mapList (f cx g) elems)

-- | Parse an edge id pattern from a value spec and schema
parseEdgeIdPattern :: t0 -> t1 -> Mapping.Schema t2 t3 t4 -> Mapping.ValueSpec -> Either t5 (Context.Context -> Core.Term -> Either Errors.Error [t4])
parseEdgeIdPattern cx g schema spec =
    Eithers.bind (parseValueSpec cx g spec) (\fun -> Right (\cx_ -> \term -> Eithers.bind (fun cx_ term) (\terms -> Eithers.mapList (Coders.coderEncode (Mapping.schemaEdgeIds schema) cx_) terms)))

-- | Parse an edge specification into a label and encoder function
parseEdgeSpec :: t0 -> t1 -> Mapping.Schema t2 t3 t4 -> Mapping.EdgeSpec -> Either t5 (Model.Label, (Context.Context -> Core.Term -> Either Errors.Error [Model.Element t4]))
parseEdgeSpec cx g schema spec =

      let label = Mapping.edgeSpecLabel spec
          id = Mapping.edgeSpecId spec
          outV = Mapping.edgeSpecOut spec
          inV = Mapping.edgeSpecIn spec
          props = Mapping.edgeSpecProperties spec
      in (Eithers.bind (parseEdgeIdPattern cx g schema id) (\getId -> Eithers.bind (parseVertexIdPattern cx g schema outV) (\getOut -> Eithers.bind (parseVertexIdPattern cx g schema inV) (\getIn -> Eithers.bind (Eithers.mapList (parsePropertySpec cx g schema) props) (\getProps -> Right (Model.LabelEdge label, (\cx_ -> \term -> Eithers.bind (requireUnique cx_ "edge id" (getId cx_) term) (\tid -> Eithers.bind (requireUnique cx_ "vertex id" (getOut cx_) term) (\tout -> Eithers.bind (requireUnique cx_ "edge id" (getIn cx_) term) (\tin -> Eithers.bind (Eithers.map (\_xs -> Maps.fromList _xs) (Eithers.mapList (\gf -> requireUnique cx_ "property key" (gf cx_) term) getProps)) (\tprops -> Right [
        Model.ElementEdge (Model.Edge {
          Model.edgeLabel = label,
          Model.edgeId = tid,
          Model.edgeOut = tout,
          Model.edgeIn = tin,
          Model.edgeProperties = tprops})])))))))))))

-- | Parse an element specification into a label and encoder function
parseElementSpec :: t0 -> t1 -> Mapping.Schema t2 t3 t4 -> Mapping.ElementSpec -> Either t5 (Model.Label, (Context.Context -> Core.Term -> Either Errors.Error [Model.Element t4]))
parseElementSpec cx g schema spec =
    case spec of
      Mapping.ElementSpecVertex v0 -> parseVertexSpec cx g schema v0
      Mapping.ElementSpecEdge v0 -> parseEdgeSpec cx g schema v0

-- | Parse a string pattern into a function that traverses terms
parsePattern :: t0 -> t1 -> String -> Either t2 (t3 -> Core.Term -> Either Errors.Error [Core.Term])
parsePattern cx _g pat =

      let segments = Strings.splitOn "${" pat
          firstLit = Lists.head segments
          rest = Lists.tail segments
          parsed =
                  Lists.map (\seg ->
                    let parts = Strings.splitOn "}" seg
                        pathStr = Lists.head parts
                        litPart = Strings.intercalate "}" (Lists.tail parts)
                        pathSteps = Strings.splitOn "/" pathStr
                    in (pathSteps, litPart)) rest
      in (Right (\cx_ -> \term -> applyPattern cx_ firstLit parsed term))

-- | Parse a property specification into an encoder function
parsePropertySpec :: t0 -> t1 -> Mapping.Schema t2 t3 t4 -> Mapping.PropertySpec -> Either t5 (Context.Context -> Core.Term -> Either Errors.Error [(Model.PropertyKey, t4)])
parsePropertySpec cx g schema spec =

      let key = Mapping.propertySpecKey spec
          value = Mapping.propertySpecValue spec
      in (Eithers.bind (parseValueSpec cx g value) (\fun -> Right (\cx_ -> \term -> Eithers.bind (fun cx_ term) (\results -> Eithers.bind (Eithers.mapList (Coders.coderEncode (Mapping.schemaPropertyValues schema) cx_) results) (\values -> Right (Lists.map (\v -> (key, v)) values))))))

-- | Parse a value specification into a function that processes terms
parseValueSpec :: t0 -> t1 -> Mapping.ValueSpec -> Either t2 (t3 -> Core.Term -> Either Errors.Error [Core.Term])
parseValueSpec cx g spec =
    case spec of
      Mapping.ValueSpecValue -> Right (\_cx -> \term -> Right [
        term])
      Mapping.ValueSpecPattern v0 -> parsePattern cx g v0

-- | Parse a vertex id pattern from a value spec and schema
parseVertexIdPattern :: t0 -> t1 -> Mapping.Schema t2 t3 t4 -> Mapping.ValueSpec -> Either t5 (Context.Context -> Core.Term -> Either Errors.Error [t4])
parseVertexIdPattern cx g schema spec =
    Eithers.bind (parseValueSpec cx g spec) (\fun -> Right (\cx_ -> \term -> Eithers.bind (fun cx_ term) (\terms -> Eithers.mapList (Coders.coderEncode (Mapping.schemaVertexIds schema) cx_) terms)))

-- | Parse a vertex specification into a label and encoder function
parseVertexSpec :: t0 -> t1 -> Mapping.Schema t2 t3 t4 -> Mapping.VertexSpec -> Either t5 (Model.Label, (Context.Context -> Core.Term -> Either Errors.Error [Model.Element t4]))
parseVertexSpec cx g schema spec =

      let label = Mapping.vertexSpecLabel spec
          id = Mapping.vertexSpecId spec
          props = Mapping.vertexSpecProperties spec
      in (Eithers.bind (parseVertexIdPattern cx g schema id) (\getId -> Eithers.bind (Eithers.mapList (parsePropertySpec cx g schema) props) (\getProps -> Right (Model.LabelVertex label, (\cx_ -> \term -> Eithers.bind (requireUnique cx_ "vertex id" (getId cx_) term) (\tid -> Eithers.bind (Eithers.map (\_xs -> Maps.fromList _xs) (Eithers.mapList (\gf -> requireUnique cx_ "property key" (gf cx_) term) getProps)) (\tprops -> Right [
        Model.ElementVertex (Model.Vertex {
          Model.vertexLabel = label,
          Model.vertexId = tid,
          Model.vertexProperties = tprops})])))))))

-- | Read a field from a map of fields by name
readField :: t0 -> M.Map Core.Name t1 -> Core.Name -> (t1 -> Either Errors.Error t2) -> Either Errors.Error t2
readField cx fields fname fun =
    Maybes.maybe (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "no such field: " (Core.unName fname))))) fun (Maps.lookup fname fields)

-- | Read an injection (union value) from a term
readInjection :: t0 -> Graph.Graph -> [(Core.Name, (Core.Term -> Either Errors.Error t1))] -> Core.Term -> Either Errors.Error t1
readInjection cx g cases encoded =
    Eithers.bind (Core_.map (\k -> Eithers.map (\_n -> Core.Name _n) (Core_.string g k)) (\_v -> Right _v) g encoded) (\mp ->
      let entries = Maps.toList mp
      in (Logic.ifElse (Lists.null entries) (Left (Errors.ErrorOther (Errors.OtherError "empty injection"))) (
        let f = Lists.head entries
            key = Pairs.first f
            val = Pairs.second f
            matching = Lists.filter (\c -> Equality.equal (Pairs.first c) key) cases
        in (Logic.ifElse (Lists.null matching) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "unexpected field: " (Core.unName key))))) (Pairs.second (Lists.head matching) val)))))

-- | Read a record from a term as a map of field names to values
readRecord :: t0 -> Graph.Graph -> (M.Map Core.Name Core.Term -> Either Errors.Error t1) -> Core.Term -> Either Errors.Error t1
readRecord cx g cons term =
    Eithers.bind (Core_.map (\k -> Eithers.map (\_n -> Core.Name _n) (Core_.string g k)) (\_v -> Right _v) g term) cons

-- | Require exactly one result from a list-producing function
requireUnique :: t0 -> String -> (t1 -> Either Errors.Error [t2]) -> t1 -> Either Errors.Error t2
requireUnique cx context fun term =
    Eithers.bind (fun term) (\results -> Logic.ifElse (Lists.null results) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "No value found: " context)))) (Logic.ifElse (Equality.equal (Lists.length results) 1) (Right (Lists.head results)) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Multiple values found: " context))))))

-- | Create an adapter that maps terms to property graph elements using a mapping specification
termToElementsAdapter :: t0 -> Graph.Graph -> Mapping.Schema t1 t2 t3 -> Core.Type -> Either Errors.Error (Coders.Adapter Core.Type [Model.Label] Core.Term [Model.Element t3])
termToElementsAdapter cx g schema typ =

      let key_elements = Core.Name "elements"
      in (Maybes.maybe (Right (Coders.Adapter {
        Coders.adapterIsLossy = False,
        Coders.adapterSource = typ,
        Coders.adapterTarget = [],
        Coders.adapterCoder = Coders.Coder {
          Coders.coderEncode = (\_cx -> \_t -> Right []),
          Coders.coderDecode = (\cx_ -> \_els -> Left (Errors.ErrorOther (Errors.OtherError "no corresponding element type")))}})) (\term -> Eithers.bind (expectList cx g decodeElementSpec term) (\specTerms -> Eithers.bind (Eithers.mapList (parseElementSpec cx g schema) specTerms) (\specs ->
        let labels = Lists.nub (Lists.map (\_p -> Pairs.first _p) specs)
            encoders = Lists.map (\_p -> Pairs.second _p) specs
        in (Right (Coders.Adapter {
          Coders.adapterIsLossy = False,
          Coders.adapterSource = typ,
          Coders.adapterTarget = labels,
          Coders.adapterCoder = Coders.Coder {
            Coders.coderEncode = (\cx_ -> \t -> Eithers.map (\_xs -> Lists.concat _xs) (Eithers.mapList (\e -> e cx_ t) encoders)),
            Coders.coderDecode = (\cx_ -> \_els -> Left (Errors.ErrorOther (Errors.OtherError "element decoding is not yet supported")))}}))))) (Annotations.getTypeAnnotation key_elements typ))

-- | Convert a term to its string representation
termToString :: Core.Term -> String
termToString term =
    case (Strip.deannotateTerm term) of
      Core.TermLiteral v0 -> case v0 of
        Core.LiteralString v1 -> v1
        Core.LiteralBoolean v1 -> Logic.ifElse v1 "true" "false"
        Core.LiteralInteger v1 -> case v1 of
          Core.IntegerValueInt32 v2 -> Literals.showInt32 v2
          _ -> Core__.term term
        Core.LiteralFloat v1 -> case v1 of
          Core.FloatValueFloat64 v2 -> Literals.showFloat64 v2
          _ -> Core__.term term
        _ -> Core__.term term
      Core.TermMaybe v0 -> Maybes.maybe "nothing" (\t -> termToString t) v0
      _ -> Core__.term term
