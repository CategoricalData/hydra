-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.maps module.

module Hydra.Lib.Maps where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
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
alter :: Packaging.PrimitiveDefinition
alter =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.alter"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Alter a value at a key using a function which sees the optional current value."),
        Packaging.entityMetadataComments = [
          "alter(f, k, m) applies f to Just(v) when m contains key k with value v, or to Nothing when k is absent. If f returns Just(v'), the binding (k, v') is set in the result; if f returns Nothing, k is removed from the result.",
          "A single primitive that subsumes insert, delete, and adjust.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeOptional (Core.TypeVariable (Core.Name "v"))),
              Core.functionTypeCodomain = (Core.TypeOptional (Core.TypeVariable (Core.Name "v")))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "k")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "f"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "k"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "m"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.cases")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "k"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.delete")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "k"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "vNew"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.insert")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "k"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "vNew"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "alter, defined in terms of lookup/insert/delete via optionals.cases.")))]))})))}
bimap :: Packaging.PrimitiveDefinition
bimap =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.bimap"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Map functions over both the keys and values of a map."),
        Packaging.entityMetadataComments = [
          "bimap(fk, fv, m) returns a new map with key fk(k) and value fv(v) for each binding (k, v) in m.",
          "Key collisions after applying fk are resolved by keeping the last binding encountered (host may differ on collision policy if fk is not injective).",
          "Requires 'ordering' constraints on both the input and output key types.",
          "Total. Corresponds to a key-and-value lift of Haskell's Data.Map.fromList . map (\\(k,v) -> (fk k, fv v)) . toList."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k1"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k2"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v1"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v2"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "k1")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "k2"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v1")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "v2"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k1")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v1"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k2")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v2"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "fk"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "fv"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "m"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "p"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermPair (
                        Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "fk")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}),
                        (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "fv")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}))))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.toList")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "bimap on a map, defined via toList/fromList.")))]))})))}
delete :: Packaging.PrimitiveDefinition
delete =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.delete"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Remove a key from a map."),
        Packaging.entityMetadataComments = [
          "delete(k, m) returns m with the binding for k removed; if k is not present, m is returned unchanged.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.delete :: Ord k => k -> Map k v -> Map k v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "k")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
elems :: Packaging.PrimitiveDefinition
elems =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.elems"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Return the values of a map (in key order)."),
        Packaging.entityMetadataComments = [
          "elems(m) returns the values of m as a list, ordered by their keys' ascending order.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.elems :: Map k v -> [v]."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeVariable (Core.Name "v")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
empty :: Packaging.PrimitiveDefinition
empty =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.empty"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "The empty map."),
        Packaging.entityMetadataComments = [
          "empty is the map with no bindings.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.empty :: Map k v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
filter :: Packaging.PrimitiveDefinition
filter =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.filter"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Filter a map by value."),
        Packaging.entityMetadataComments = [
          "filter(p, m) returns the submap of m containing exactly the bindings (k, v) for which p(v) is true.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.filter :: (v -> Bool) -> Map k v -> Map k v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v")),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "p"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "m"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.filter")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "pr"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "p")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "pr"))}))}))}))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.toList")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "filter on a map, defined via toList/fromList.")))]))})))}
filterWithKey :: Packaging.PrimitiveDefinition
filterWithKey =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.filterWithKey"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Filter a map by key and value."),
        Packaging.entityMetadataComments = [
          "filterWithKey(p, m) returns the submap of m containing exactly the bindings (k, v) for which p(k, v) is true.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "k")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v")),
                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "p"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "m"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.filter")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "pr"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "p")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "pr"))}))})),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "pr"))}))}))}))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.toList")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "filterWithKey on a map, defined via toList/fromList.")))]))})))}
findWithDefault :: Packaging.PrimitiveDefinition
findWithDefault =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.findWithDefault"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Look up a value with a default if the key is absent."),
        Packaging.entityMetadataComments = [
          "findWithDefault(def, k, m) returns the value bound to k in m if k is present, or def otherwise. Equivalent to maybe(def, identity, lookup(k, m)).",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.findWithDefault :: Ord k => v -> k -> Map k v -> v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "v")),
            Typing.parameterIsLazy = True},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "k")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "v"))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "def"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "k"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "m"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.fromOptional")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "def"))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "k"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "findWithDefault, defined in terms of lookup + fromOptional.")))]))})))}
fromList :: Packaging.PrimitiveDefinition
fromList =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.fromList"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Build a map from a list of key-value pairs."),
        Packaging.entityMetadataComments = [
          "fromList(xs) returns the map containing exactly the bindings in xs. If xs contains multiple entries for the same key, the last one wins (matching Haskell's fromList behavior).",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.fromList :: Ord k => [(k, v)] -> Map k v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "k")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "v"))}))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
insert :: Packaging.PrimitiveDefinition
insert =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.insert"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Insert a key-value pair into a map."),
        Packaging.entityMetadataComments = [
          "insert(k, v, m) returns m with the binding (k, v) added or updated. If k is already present, its value is overwritten.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.insert :: Ord k => k -> v -> Map k v -> Map k v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "k")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "v")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
keys :: Packaging.PrimitiveDefinition
keys =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.keys"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Return the keys of a map (in key order)."),
        Packaging.entityMetadataComments = [
          "keys(m) returns the keys of m as a list, in ascending order.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.keys :: Map k v -> [k]."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeVariable (Core.Name "k")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
lookup :: Packaging.PrimitiveDefinition
lookup =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.lookup"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Look up a value in a map by key, returning Nothing if absent."),
        Packaging.entityMetadataComments = [
          "lookup(k, m) returns Just(v) where v is the value bound to k in m, or Nothing if k is not present.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.lookup :: Ord k => k -> Map k v -> Maybe v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "k")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeOptional (Core.TypeVariable (Core.Name "v")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
map :: Packaging.PrimitiveDefinition
map =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.map"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Map a function over the values of a map."),
        Packaging.entityMetadataComments = [
          "map(f, m) returns a map with the same keys as m and value f(v) for each binding (k, v).",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.map :: (v -> w) -> Map k v -> Map k w / fmap on Map."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v1"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v2"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v1")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "v2"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v1"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v2"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "f"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "m"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "p"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermPair (
                      Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}),
                      (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}))))}))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.toList")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "map over values, defined via toList/fromList.")))]))})))}
mapKeys :: Packaging.PrimitiveDefinition
mapKeys =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.mapKeys"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Map a function over the keys of a map."),
        Packaging.entityMetadataComments = [
          "mapKeys(f, m) returns a map where each binding (k, v) becomes (f(k), v). If f maps multiple keys to the same image, key collisions are resolved by keeping the binding with the greater original key (matching Haskell's mapKeys behavior).",
          "Requires 'ordering' constraints on both the input and output key types.",
          "Total. Corresponds to Haskell's Data.Map.mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> Map k1 v -> Map k2 v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k1"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k2"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "k1")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "k2"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k1")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k2")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "f"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "m"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "p"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermPair (
                      Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}),
                      (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))))}))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.toList")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "map over keys, defined via toList/fromList.")))]))})))}
member :: Packaging.PrimitiveDefinition
member =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.member"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Test whether a key is present in a map."),
        Packaging.entityMetadataComments = [
          "member(k, m) returns true iff k is a key in m.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.member :: Ord k => k -> Map k v -> Bool."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "k")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
null :: Packaging.PrimitiveDefinition
null =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.null"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Test whether a map is empty."),
        Packaging.entityMetadataComments = [
          "null(m) returns true iff m has no bindings.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.null :: Map k v -> Bool."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
singleton :: Packaging.PrimitiveDefinition
singleton =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.singleton"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Construct a map with a single key-value pair."),
        Packaging.entityMetadataComments = [
          "singleton(k, v) returns the map containing exactly the binding (k, v).",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.singleton :: k -> v -> Map k v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "k")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "v")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
size :: Packaging.PrimitiveDefinition
size =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.size"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Return the number of key-value pairs in a map."),
        Packaging.entityMetadataComments = [
          "size(m) returns the number of bindings in m as an int32.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.size :: Map k v -> Int (with narrowing to int32)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
toList :: Packaging.PrimitiveDefinition
toList =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.toList"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Convert a map to a list of key-value pairs (in key order)."),
        Packaging.entityMetadataComments = [
          "toList(m) returns the bindings of m as a list of (key, value) pairs, in ascending key order.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.toList :: Map k v -> [(k, v)]."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeVariable (Core.Name "k")),
            Core.pairTypeSecond = (Core.TypeVariable (Core.Name "v"))})))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
union :: Packaging.PrimitiveDefinition
union =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.union"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Compute the union of two maps; the first map's bindings take precedence on key collision."),
        Packaging.entityMetadataComments = [
          "union(m1, m2) returns the map containing all bindings from m1 plus the bindings of m2 whose keys are not in m1. On key collision, the binding from m1 is preferred.",
          "Requires an 'ordering' constraint on the key type.",
          "Total. Corresponds to Haskell's Data.Map.union :: Ord k => Map k v -> Map k v -> Map k v."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "k"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "v"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))}))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
