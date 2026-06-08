-- Note: this is an automatically generated file. Do not edit.
-- | A utility which instantiates a nonrecursive type with default values

module Hydra.Templates where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Hydra.Haskell.Lib.Literals as Literals
-- | Decode a list of type-encoding bindings into a map of named types
graphToSchema :: t0 -> Graph.Graph -> [Core.Binding] -> Either Errors.DecodingError (M.Map Core.Name Core.Type)
graphToSchema cx graph els =

      let toPair =
              \el ->
                let name = Core.bindingName el
                in (Eithers.bind (DecodeCore.type_ graph (Core.bindingTerm el)) (\t -> Right (name, t)))
      in (Eithers.bind (Eithers.mapList toPair els) (\pairs -> Right (Maps.fromList pairs)))
-- | Given a graph schema and a nonrecursive type, instantiate it with default values. If the minimal flag is set, the smallest possible term is produced; otherwise, exactly one subterm is produced for constructors which do not otherwise require one, e.g. in lists and optionals. The name parameter provides the element name for nominal type construction.
instantiateTemplate :: t0 -> Bool -> M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> Either Errors.Error Core.Term
instantiateTemplate cx minimal schema tname t =

      let inst = \tn -> instantiateTemplate cx minimal schema tn
          noPoly =
                  Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
                    Errors.unexpectedShapeErrorExpected = "non-polymorphic type",
                    Errors.unexpectedShapeErrorActual = "polymorphic or function type"})))
          forFloat =
                  \ft -> case ft of
                    Core.FloatTypeFloat32 -> Core.FloatValueFloat32 0.0
                    Core.FloatTypeFloat64 -> Core.FloatValueFloat64 0.0
          forInteger =
                  \it -> case it of
                    Core.IntegerTypeBigint -> Core.IntegerValueBigint 0
                    Core.IntegerTypeInt8 -> Core.IntegerValueInt8 0
                    Core.IntegerTypeInt16 -> Core.IntegerValueInt16 0
                    Core.IntegerTypeInt32 -> Core.IntegerValueInt32 0
                    Core.IntegerTypeInt64 -> Core.IntegerValueInt64 0
                    Core.IntegerTypeUint8 -> Core.IntegerValueUint8 0
                    Core.IntegerTypeUint16 -> Core.IntegerValueUint16 0
                    Core.IntegerTypeUint32 -> Core.IntegerValueUint32 0
                    Core.IntegerTypeUint64 -> Core.IntegerValueUint64 0
          forLiteral =
                  \lt -> case lt of
                    Core.LiteralTypeBinary -> Core.LiteralString ""
                    Core.LiteralTypeBoolean -> Core.LiteralBoolean False
                    Core.LiteralTypeDecimal -> Core.LiteralDecimal (Literals.stringToDecimal "0.0")
                    Core.LiteralTypeInteger v0 -> Core.LiteralInteger (forInteger v0)
                    Core.LiteralTypeFloat v0 -> Core.LiteralFloat (forFloat v0)
                    Core.LiteralTypeString -> Core.LiteralString ""
      in case t of
        Core.TypeAnnotated v0 -> inst tname (Core.annotatedTypeBody v0)
        Core.TypeApplication _ -> noPoly
        Core.TypeFunction _ -> noPoly
        Core.TypeForall _ -> noPoly
        Core.TypeList v0 -> Logic.ifElse minimal (Right (Core.TermList [])) (Eithers.bind (inst tname v0) (\e -> Right (Core.TermList [
          e])))
        Core.TypeLiteral v0 -> Right (Core.TermLiteral (forLiteral v0))
        Core.TypeMap v0 ->
          let kt = Core.mapTypeKeys v0
              vt = Core.mapTypeValues v0
          in (Logic.ifElse minimal (Right (Core.TermMap Maps.empty)) (Eithers.bind (inst tname kt) (\ke -> Eithers.bind (inst tname vt) (\ve -> Right (Core.TermMap (Maps.singleton ke ve))))))
        Core.TypeOptional v0 -> Logic.ifElse minimal (Right (Core.TermOptional Nothing)) (Eithers.bind (inst tname v0) (\e -> Right (Core.TermOptional (Just e))))
        Core.TypeRecord v0 ->
          let toField =
                  \ft -> Eithers.bind (inst tname (Core.fieldTypeType ft)) (\e -> Right (Core.Field {
                    Core.fieldName = (Core.fieldTypeName ft),
                    Core.fieldTerm = e}))
          in (Eithers.bind (Eithers.mapList toField v0) (\dfields -> Right (Core.TermRecord (Core.Record {
            Core.recordTypeName = tname,
            Core.recordFields = dfields}))))
        Core.TypeSet v0 -> Logic.ifElse minimal (Right (Core.TermSet Sets.empty)) (Eithers.bind (inst tname v0) (\e -> Right (Core.TermSet (Sets.fromList [
          e]))))
        Core.TypeVariable v0 -> Optionals.cases (Maps.lookup v0 schema) (Left (Errors.ErrorResolution (Errors.ResolutionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "bound type variable",
          Errors.unexpectedShapeErrorActual = (Strings.cat2 "unbound variable " (Core.unName v0))})))) (inst v0)
        Core.TypeWrap v0 -> Eithers.bind (inst tname v0) (\e -> Right (Core.TermWrap (Core.WrappedTerm {
          Core.wrappedTermTypeName = tname,
          Core.wrappedTermBody = e})))
