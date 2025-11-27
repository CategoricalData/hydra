-- Note: this is an automatically generated file. Do not edit.

-- | A utility which instantiates a nonrecursive type with default values

module Hydra.Templates where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create a graph schema from a graph which contains nothing but encoded type definitions
graphToSchema :: (Graph.Graph -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Type))
graphToSchema g =  
  let toPair = (\nameAndEl ->  
          let name = (Pairs.first nameAndEl)
          in  
            let el = (Pairs.second nameAndEl)
            in (Flows.bind (Monads.withTrace "graph to schema" (Core_.type_ (Core.bindingTerm el))) (\t -> Flows.pure (name, t))))
  in (Flows.bind (Flows.mapList toPair (Maps.toList (Graph.graphElements g))) (\pairs -> Flows.pure (Maps.fromList pairs)))

instantiateTemplate :: (Bool -> M.Map Core.Name Core.Type -> Core.Type -> Compute.Flow t0 Core.Term)
instantiateTemplate minimal schema t =  
  let inst = (instantiateTemplate minimal schema)
  in  
    let noPoly = (Flows.fail "Polymorphic and function types are not currently supported")
    in  
      let forFloat = (\ft -> (\x -> case x of
              Core.FloatTypeBigfloat -> (Core.FloatValueBigfloat 0.0)
              Core.FloatTypeFloat32 -> (Core.FloatValueFloat32 0.0)
              Core.FloatTypeFloat64 -> (Core.FloatValueFloat64 0.0)) ft)
      in  
        let forInteger = (\it -> (\x -> case x of
                Core.IntegerTypeBigint -> (Core.IntegerValueBigint 0)
                Core.IntegerTypeInt8 -> (Core.IntegerValueInt8 0)
                Core.IntegerTypeInt16 -> (Core.IntegerValueInt16 0)
                Core.IntegerTypeInt32 -> (Core.IntegerValueInt32 0)
                Core.IntegerTypeInt64 -> (Core.IntegerValueInt64 0)
                Core.IntegerTypeUint8 -> (Core.IntegerValueUint8 0)
                Core.IntegerTypeUint16 -> (Core.IntegerValueUint16 0)
                Core.IntegerTypeUint32 -> (Core.IntegerValueUint32 0)
                Core.IntegerTypeUint64 -> (Core.IntegerValueUint64 0)) it)
        in  
          let forLiteral = (\lt -> (\x -> case x of
                  Core.LiteralTypeBinary -> (Core.LiteralString "")
                  Core.LiteralTypeBoolean -> (Core.LiteralBoolean False)
                  Core.LiteralTypeInteger v1 -> (Core.LiteralInteger (forInteger v1))
                  Core.LiteralTypeFloat v1 -> (Core.LiteralFloat (forFloat v1))
                  Core.LiteralTypeString -> (Core.LiteralString "")) lt)
          in ((\x -> case x of
            Core.TypeAnnotated v1 -> (inst (Core.annotatedTypeBody v1))
            Core.TypeApplication _ -> noPoly
            Core.TypeFunction _ -> noPoly
            Core.TypeForall _ -> noPoly
            Core.TypeList v1 -> (Logic.ifElse minimal (Flows.pure (Core.TermList [])) (Flows.bind (inst v1) (\e -> Flows.pure (Core.TermList [
              e]))))
            Core.TypeLiteral v1 -> (Flows.pure (Core.TermLiteral (forLiteral v1)))
            Core.TypeMap v1 ->  
              let kt = (Core.mapTypeKeys v1)
              in  
                let vt = (Core.mapTypeValues v1)
                in (Logic.ifElse minimal (Flows.pure (Core.TermMap Maps.empty)) (Flows.bind (inst kt) (\ke -> Flows.bind (inst vt) (\ve -> Flows.pure (Core.TermMap (Maps.singleton ke ve))))))
            Core.TypeMaybe v1 -> (Logic.ifElse minimal (Flows.pure (Core.TermMaybe Nothing)) (Flows.bind (inst v1) (\e -> Flows.pure (Core.TermMaybe (Just e)))))
            Core.TypeProduct v1 -> (Flows.bind (Flows.mapList inst v1) (\es -> Flows.pure (Core.TermProduct es)))
            Core.TypeRecord v1 ->  
              let tname = (Core.rowTypeTypeName v1)
              in  
                let fields = (Core.rowTypeFields v1)
                in  
                  let toField = (\ft -> Flows.bind (inst (Core.fieldTypeType ft)) (\e -> Flows.pure (Core.Field {
                          Core.fieldName = (Core.fieldTypeName ft),
                          Core.fieldTerm = e})))
                  in (Flows.bind (Flows.mapList toField fields) (\dfields -> Flows.pure (Core.TermRecord (Core.Record {
                    Core.recordTypeName = tname,
                    Core.recordFields = dfields}))))
            Core.TypeSet v1 -> (Logic.ifElse minimal (Flows.pure (Core.TermSet Sets.empty)) (Flows.bind (inst v1) (\e -> Flows.pure (Core.TermSet (Sets.fromList [
              e])))))
            Core.TypeVariable v1 -> (Maybes.maybe (Flows.fail (Strings.cat2 "Type variable " (Strings.cat2 (Core__.term (Core.TermVariable v1)) " not found in schema"))) inst (Maps.lookup v1 schema))
            Core.TypeWrap v1 ->  
              let tname = (Core.wrappedTypeTypeName v1)
              in  
                let t_ = (Core.wrappedTypeBody v1)
                in (Flows.bind (inst t_) (\e -> Flows.pure (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = tname,
                  Core.wrappedTermBody = e}))))) t)
