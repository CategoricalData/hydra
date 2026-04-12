-- Note: this is an automatically generated file. Do not edit.

-- | Validation functions for property graphs

module Hydra.Validate.Pg where

import qualified Hydra.Error.Pg as Pg
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Pg.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

checkAll :: [Maybe t0] -> Maybe t0
checkAll checks =

      let errors = Maybes.cat checks
      in (Lists.safeHead errors)

validateEdge :: (t0 -> t1 -> Maybe Pg.InvalidValueError) -> Maybe (t1 -> Maybe Model.VertexLabel) -> Model.EdgeType t0 -> Model.Edge t1 -> Maybe Pg.InvalidEdgeError
validateEdge checkValue labelForVertexId typ el =

      let checkLabel =

                let expected = Model.edgeTypeLabel typ
                    actual = Model.edgeLabel el
                in (Logic.ifElse (Equality.equal (Model.unEdgeLabel actual) (Model.unEdgeLabel expected)) Nothing (Just (Pg.InvalidEdgeErrorLabel (Pg.NoSuchEdgeLabelError {
                  Pg.noSuchEdgeLabelErrorLabel = actual}))))
          checkId = Maybes.map (\err -> Pg.InvalidEdgeErrorId err) (checkValue (Model.edgeTypeId typ) (Model.edgeId el))
          checkProperties =
                  Maybes.map (\err -> Pg.InvalidEdgeErrorProperty err) (validateProperties checkValue (Model.edgeTypeProperties typ) (Model.edgeProperties el))
          checkOut =
                  Maybes.maybe Nothing (\f -> Maybes.maybe (Just Pg.InvalidEdgeErrorOutVertexNotFound) (\label -> Logic.ifElse (Equality.equal (Model.unVertexLabel label) (Model.unVertexLabel (Model.edgeTypeOut typ))) Nothing (Just (Pg.InvalidEdgeErrorOutVertexLabel (Pg.WrongVertexLabelError {
                    Pg.wrongVertexLabelErrorExpected = (Model.edgeTypeOut typ),
                    Pg.wrongVertexLabelErrorActual = label})))) (f (Model.edgeOut el))) labelForVertexId
          checkIn =
                  Maybes.maybe Nothing (\f -> Maybes.maybe (Just Pg.InvalidEdgeErrorInVertexNotFound) (\label -> Logic.ifElse (Equality.equal (Model.unVertexLabel label) (Model.unVertexLabel (Model.edgeTypeIn typ))) Nothing (Just (Pg.InvalidEdgeErrorInVertexLabel (Pg.WrongVertexLabelError {
                    Pg.wrongVertexLabelErrorExpected = (Model.edgeTypeIn typ),
                    Pg.wrongVertexLabelErrorActual = label})))) (f (Model.edgeIn el))) labelForVertexId
      in (checkAll [
        checkLabel,
        checkId,
        checkProperties,
        checkOut,
        checkIn])

validateGraph :: Ord t1 => ((t0 -> t1 -> Maybe Pg.InvalidValueError) -> Model.GraphSchema t0 -> Model.Graph t1 -> Maybe (Pg.InvalidGraphError t1))
validateGraph checkValue schema graph =

      let checkVertices =

                let checkVertex =
                        \el -> Maybes.maybe (Just (Pg.InvalidGraphErrorVertex (Pg.InvalidGraphVertexError {
                          Pg.invalidGraphVertexErrorId = (Model.vertexId el),
                          Pg.invalidGraphVertexErrorError = (Pg.InvalidVertexErrorLabel (Pg.NoSuchVertexLabelError {
                            Pg.noSuchVertexLabelErrorLabel = (Model.vertexLabel el)}))}))) (\t -> Maybes.map (\err -> Pg.InvalidGraphErrorVertex (Pg.InvalidGraphVertexError {
                          Pg.invalidGraphVertexErrorId = (Model.vertexId el),
                          Pg.invalidGraphVertexErrorError = err})) (validateVertex checkValue t el)) (Maps.lookup (Model.vertexLabel el) (Model.graphSchemaVertices schema))
                in (checkAll (Lists.map checkVertex (Maps.elems (Model.graphVertices graph))))
          checkEdges =

                    let checkEdge =
                            \el -> Maybes.maybe (Just (Pg.InvalidGraphErrorEdge (Pg.InvalidGraphEdgeError {
                              Pg.invalidGraphEdgeErrorId = (Model.edgeId el),
                              Pg.invalidGraphEdgeErrorError = (Pg.InvalidEdgeErrorLabel (Pg.NoSuchEdgeLabelError {
                                Pg.noSuchEdgeLabelErrorLabel = (Model.edgeLabel el)}))}))) (\t -> Maybes.map (\err -> Pg.InvalidGraphErrorEdge (Pg.InvalidGraphEdgeError {
                              Pg.invalidGraphEdgeErrorId = (Model.edgeId el),
                              Pg.invalidGraphEdgeErrorError = err})) (validateEdge checkValue labelForVertexId t el)) (Maps.lookup (Model.edgeLabel el) (Model.graphSchemaEdges schema))
                        labelForVertexId = Just (\i -> Maybes.map Model.vertexLabel (Maps.lookup i (Model.graphVertices graph)))
                    in (checkAll (Lists.map checkEdge (Maps.elems (Model.graphEdges graph))))
      in (checkAll [
        checkVertices,
        checkEdges])

validateProperties :: (t0 -> t1 -> Maybe Pg.InvalidValueError) -> [Model.PropertyType t0] -> M.Map Model.PropertyKey t1 -> Maybe Pg.InvalidElementPropertyError
validateProperties checkValue types props =

      let checkTypes = checkAll (Lists.map checkType types)
          checkType =
                  \t -> Logic.ifElse (Model.propertyTypeRequired t) (Maybes.maybe (Just (Pg.InvalidElementPropertyError {
                    Pg.invalidElementPropertyErrorKey = (Model.propertyTypeKey t),
                    Pg.invalidElementPropertyErrorError = (Pg.InvalidPropertyErrorMissingRequired (Model.propertyTypeKey t))})) (\_ -> Nothing) (Maps.lookup (Model.propertyTypeKey t) props)) Nothing
          checkValues =

                    let m = Maps.fromList (Lists.map (\p -> (Model.propertyTypeKey p, (Model.propertyTypeValue p))) types)
                        checkPair =
                                \pair ->
                                  let key = Pairs.first pair
                                      val = Pairs.second pair
                                  in (Maybes.maybe (Just (Pg.InvalidElementPropertyError {
                                    Pg.invalidElementPropertyErrorKey = key,
                                    Pg.invalidElementPropertyErrorError = (Pg.InvalidPropertyErrorUnexpectedKey key)})) (\typ -> Maybes.map (\err -> Pg.InvalidElementPropertyError {
                                    Pg.invalidElementPropertyErrorKey = key,
                                    Pg.invalidElementPropertyErrorError = (Pg.InvalidPropertyErrorInvalidValue err)}) (checkValue typ val)) (Maps.lookup key m))
                    in (checkAll (Lists.map checkPair (Maps.toList props)))
      in (checkAll [
        checkTypes,
        checkValues])

validateVertex :: (t0 -> t1 -> Maybe Pg.InvalidValueError) -> Model.VertexType t0 -> Model.Vertex t1 -> Maybe Pg.InvalidVertexError
validateVertex checkValue typ el =

      let checkLabel =

                let expected = Model.vertexTypeLabel typ
                    actual = Model.vertexLabel el
                in (Logic.ifElse (Equality.equal (Model.unVertexLabel actual) (Model.unVertexLabel expected)) Nothing (Just (Pg.InvalidVertexErrorLabel (Pg.NoSuchVertexLabelError {
                  Pg.noSuchVertexLabelErrorLabel = actual}))))
          checkId = Maybes.map (\err -> Pg.InvalidVertexErrorId err) (checkValue (Model.vertexTypeId typ) (Model.vertexId el))
          checkProperties =
                  Maybes.map (\err -> Pg.InvalidVertexErrorProperty err) (validateProperties checkValue (Model.vertexTypeProperties typ) (Model.vertexProperties el))
      in (checkAll [
        checkLabel,
        checkId,
        checkProperties])
