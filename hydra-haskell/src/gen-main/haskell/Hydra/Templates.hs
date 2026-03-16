-- Note: this is an automatically generated file. Do not edit.

-- | A utility which instantiates a nonrecursive type with default values

module Hydra.Templates where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Error as Error
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Decode a list of type-encoding bindings into a map of named types
graphToSchema :: (Context.Context -> Graph.Graph -> [Core.Binding] -> Either (Context.InContext Error.DecodingError) (M.Map Core.Name Core.Type))
graphToSchema cx graph els =  
  let toPair = (\el ->  
          let name = (Core.bindingName el)
          in (Eithers.bind (Eithers.bimap (\_wc_e -> Context.InContext {
            Context.inContextObject = _wc_e,
            Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Core_.type_ graph (Core.bindingTerm el))) (\t -> Right (name, t))))
  in (Eithers.bind (Eithers.mapList toPair els) (\pairs -> Right (Maps.fromList pairs)))

-- | Given a graph schema and a nonrecursive type, instantiate it with default values. If the minimal flag is set, the smallest possible term is produced; otherwise, exactly one subterm is produced for constructors which do not otherwise require one, e.g. in lists and optionals. The name parameter provides the element name for nominal type construction.
instantiateTemplate :: (Context.Context -> Bool -> M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> Either (Context.InContext Error.Error) Core.Term)
instantiateTemplate cx minimal schema tname t =  
  let inst = (\tn -> instantiateTemplate cx minimal schema tn)
  in  
    let noPoly = (Left (Context.InContext {
            Context.inContextObject = (Error.ErrorOther (Error.OtherError "Polymorphic and function types are not currently supported")),
            Context.inContextContext = cx}))
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
                  Core.LiteralTypeInteger v0 -> (Core.LiteralInteger (forInteger v0))
                  Core.LiteralTypeFloat v0 -> (Core.LiteralFloat (forFloat v0))
                  Core.LiteralTypeString -> (Core.LiteralString "")) lt)
          in ((\x -> case x of
            Core.TypeAnnotated v0 -> (inst tname (Core.annotatedTypeBody v0))
            Core.TypeApplication _ -> noPoly
            Core.TypeFunction _ -> noPoly
            Core.TypeForall _ -> noPoly
            Core.TypeList v0 -> (Logic.ifElse minimal (Right (Core.TermList [])) (Eithers.bind (inst tname v0) (\e -> Right (Core.TermList [
              e]))))
            Core.TypeLiteral v0 -> (Right (Core.TermLiteral (forLiteral v0)))
            Core.TypeMap v0 ->  
              let kt = (Core.mapTypeKeys v0)
              in  
                let vt = (Core.mapTypeValues v0)
                in (Logic.ifElse minimal (Right (Core.TermMap Maps.empty)) (Eithers.bind (inst tname kt) (\ke -> Eithers.bind (inst tname vt) (\ve -> Right (Core.TermMap (Maps.singleton ke ve))))))
            Core.TypeMaybe v0 -> (Logic.ifElse minimal (Right (Core.TermMaybe Nothing)) (Eithers.bind (inst tname v0) (\e -> Right (Core.TermMaybe (Just e)))))
            Core.TypeRecord v0 ->  
              let toField = (\ft -> Eithers.bind (inst tname (Core.fieldTypeType ft)) (\e -> Right (Core.Field {
                      Core.fieldName = (Core.fieldTypeName ft),
                      Core.fieldTerm = e})))
              in (Eithers.bind (Eithers.mapList toField v0) (\dfields -> Right (Core.TermRecord (Core.Record {
                Core.recordTypeName = tname,
                Core.recordFields = dfields}))))
            Core.TypeSet v0 -> (Logic.ifElse minimal (Right (Core.TermSet Sets.empty)) (Eithers.bind (inst tname v0) (\e -> Right (Core.TermSet (Sets.fromList [
              e])))))
            Core.TypeVariable v0 -> (Maybes.maybe (Left (Context.InContext {
              Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "Type variable " (Strings.cat2 (Core__.term (Core.TermVariable v0)) " not found in schema")))),
              Context.inContextContext = cx})) (inst v0) (Maps.lookup v0 schema))
            Core.TypeWrap v0 -> (Eithers.bind (inst tname v0) (\e -> Right (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = tname,
              Core.wrappedTermBody = e}))))) t)
