-- | Abstractions for single- and bidirectional transformations

module Hydra.Compute where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A two-level bidirectional encoder which adapts types to types and terms to terms
data Adapter s1 s2 t1 t2 v1 v2 = 
  Adapter {
    adapterIsLossy :: Bool,
    adapterSource :: t1,
    adapterTarget :: t2,
    adapterCoder :: (Coder s1 s2 v1 v2)}

_Adapter = (Core.Name "hydra/compute.Adapter")

_Adapter_isLossy = (Core.Name "isLossy")

_Adapter_source = (Core.Name "source")

_Adapter_target = (Core.Name "target")

_Adapter_coder = (Core.Name "coder")

_Adapter_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "s1"),
  Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
    Core.lambdaTypeParameter = (Core.Name "s2"),
    Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
      Core.lambdaTypeParameter = (Core.Name "t1"),
      Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
        Core.lambdaTypeParameter = (Core.Name "t2"),
        Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
          Core.lambdaTypeParameter = (Core.Name "v1"),
          Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
            Core.lambdaTypeParameter = (Core.Name "v2"),
            Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
              Core.rowTypeTypeName = (Core.Name "hydra/compute.Adapter"),
              Core.rowTypeExtends = Nothing,
              Core.rowTypeFields = [
                Core.FieldType {
                  Core.fieldTypeName = (Core.Name "isLossy"),
                  Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
                Core.FieldType {
                  Core.fieldTypeName = (Core.Name "source"),
                  Core.fieldTypeType = (Core.TypeVariable (Core.Name "t1"))},
                Core.FieldType {
                  Core.fieldTypeName = (Core.Name "target"),
                  Core.fieldTypeType = (Core.TypeVariable (Core.Name "t2"))},
                Core.FieldType {
                  Core.fieldTypeName = (Core.Name "coder"),
                  Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = _Coder_type_,
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s1"))})),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s2"))})),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v1"))})),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v2"))}))}]}))}))}))}))}))}))}))

-- | A two-level encoder and decoder, operating both at a type level and an instance (data) level
data Bicoder s1 s2 t1 t2 v1 v2 = 
  Bicoder {
    bicoderEncode :: (t1 -> Adapter s1 s2 t1 t2 v1 v2),
    bicoderDecode :: (t2 -> Adapter s2 s1 t2 t1 v2 v1)}

_Bicoder = (Core.Name "hydra/compute.Bicoder")

_Bicoder_encode = (Core.Name "encode")

_Bicoder_decode = (Core.Name "decode")

_Bicoder_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "s1"),
  Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
    Core.lambdaTypeParameter = (Core.Name "s2"),
    Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
      Core.lambdaTypeParameter = (Core.Name "t1"),
      Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
        Core.lambdaTypeParameter = (Core.Name "t2"),
        Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
          Core.lambdaTypeParameter = (Core.Name "v1"),
          Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
            Core.lambdaTypeParameter = (Core.Name "v2"),
            Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
              Core.rowTypeTypeName = (Core.Name "hydra/compute.Bicoder"),
              Core.rowTypeExtends = Nothing,
              Core.rowTypeFields = [
                Core.FieldType {
                  Core.fieldTypeName = (Core.Name "encode"),
                  Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                Core.applicationTypeFunction = _Adapter_type_,
                                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s1"))})),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s2"))})),
                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t2"))})),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v1"))})),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v2"))}))}))},
                Core.FieldType {
                  Core.fieldTypeName = (Core.Name "decode"),
                  Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                    Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                Core.applicationTypeFunction = _Adapter_type_,
                                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s2"))})),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s1"))})),
                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t2"))})),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v2"))})),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v1"))}))}))}]}))}))}))}))}))}))}))

-- | An encoder and decoder; a bidirectional flow between two types
data Coder s1 s2 v1 v2 = 
  Coder {
    coderEncode :: (v1 -> Flow s1 v2),
    coderDecode :: (v2 -> Flow s2 v1)}

_Coder = (Core.Name "hydra/compute.Coder")

_Coder_encode = (Core.Name "encode")

_Coder_decode = (Core.Name "decode")

_Coder_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "s1"),
  Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
    Core.lambdaTypeParameter = (Core.Name "s2"),
    Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
      Core.lambdaTypeParameter = (Core.Name "v1"),
      Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
        Core.lambdaTypeParameter = (Core.Name "v2"),
        Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
          Core.rowTypeTypeName = (Core.Name "hydra/compute.Coder"),
          Core.rowTypeExtends = Nothing,
          Core.rowTypeFields = [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "encode"),
              Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v1")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = _Flow_type_,
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s1"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v2"))}))}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "decode"),
              Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v2")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = _Flow_type_,
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s2"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v1"))}))}))}]}))}))}))}))}))

-- | A variant of the State monad with built-in logging and error handling
newtype Flow s x = 
  Flow {
    unFlow :: (s -> Trace -> FlowState s x)}

_Flow = (Core.Name "hydra/compute.Flow")

_Flow_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "s"),
  Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
    Core.lambdaTypeParameter = (Core.Name "x"),
    Core.lambdaTypeBody = (Core.TypeFunction (Core.FunctionType {
      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "s")),
      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = _Trace_type_,
        Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = _FlowState_type_,
            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "x"))}))}))}))}))}))

-- | The result of evaluating a Flow
data FlowState s x = 
  FlowState {
    flowStateValue :: (Maybe x),
    flowStateState :: s,
    flowStateTrace :: Trace}
  deriving (Eq, Ord, Read, Show)

_FlowState = (Core.Name "hydra/compute.FlowState")

_FlowState_value = (Core.Name "value")

_FlowState_state = (Core.Name "state")

_FlowState_trace = (Core.Name "trace")

_FlowState_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "s"),
  Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
    Core.lambdaTypeParameter = (Core.Name "x"),
    Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
      Core.rowTypeTypeName = (Core.Name "hydra/compute.FlowState"),
      Core.rowTypeExtends = Nothing,
      Core.rowTypeFields = [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = (Core.TypeOptional (Core.TypeVariable (Core.Name "x")))},
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "state"),
          Core.fieldTypeType = (Core.TypeVariable (Core.Name "s"))},
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "trace"),
          Core.fieldTypeType = _Trace_type_}]}))}))}))

-- | A container for logging and error information
data Trace = 
  Trace {
    traceStack :: [String],
    traceMessages :: [String],
    -- | A map of string keys to arbitrary terms as values, for application-specific use
    traceOther :: (Map String Core.Term)}
  deriving (Eq, Ord, Read, Show)

_Trace = (Core.Name "hydra/compute.Trace")

_Trace_stack = (Core.Name "stack")

_Trace_messages = (Core.Name "messages")

_Trace_other = (Core.Name "other")

_Trace_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/compute.Trace"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "stack"),
      Core.fieldTypeType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "messages"),
      Core.fieldTypeType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "other"),
      Core.fieldTypeType = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
        Core.mapTypeValues = Core._Term_type_}))}]}))