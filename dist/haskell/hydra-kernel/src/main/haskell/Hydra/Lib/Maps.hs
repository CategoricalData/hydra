-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.maps namespace.

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
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
alter :: Packaging.PrimitiveDefinition
alter =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.alter"),
      Packaging.primitiveDefinitionDescription = "Alter a value at a key using a function which sees the optional current value.",
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
              Core.functionTypeDomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "v"))),
              Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "v")))})),
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
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
bimap :: Packaging.PrimitiveDefinition
bimap =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.bimap"),
      Packaging.primitiveDefinitionDescription = "Map functions over both the keys and values of a map.",
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
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
delete :: Packaging.PrimitiveDefinition
delete =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.delete"),
      Packaging.primitiveDefinitionDescription = "Remove a key from a map.",
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
      Packaging.primitiveDefinitionDescription = "Return the values of a map (in key order).",
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
      Packaging.primitiveDefinitionDescription = "The empty map.",
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
      Packaging.primitiveDefinitionDescription = "Filter a map by value.",
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
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
filterWithKey :: Packaging.PrimitiveDefinition
filterWithKey =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.filterWithKey"),
      Packaging.primitiveDefinitionDescription = "Filter a map by key and value.",
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
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
findWithDefault :: Packaging.PrimitiveDefinition
findWithDefault =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.findWithDefault"),
      Packaging.primitiveDefinitionDescription = "Look up a value with a default if the key is absent.",
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
          Typing.resultType = (Core.TypeVariable (Core.Name "v"))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
fromList :: Packaging.PrimitiveDefinition
fromList =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.fromList"),
      Packaging.primitiveDefinitionDescription = "Build a map from a list of key-value pairs.",
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
      Packaging.primitiveDefinitionDescription = "Insert a key-value pair into a map.",
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
      Packaging.primitiveDefinitionDescription = "Return the keys of a map (in key order).",
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
      Packaging.primitiveDefinitionDescription = "Look up a value in a map by key, returning Nothing if absent.",
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
          Typing.resultType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "v")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
map :: Packaging.PrimitiveDefinition
map =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.map"),
      Packaging.primitiveDefinitionDescription = "Map a function over the values of a map.",
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
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
mapKeys :: Packaging.PrimitiveDefinition
mapKeys =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.mapKeys"),
      Packaging.primitiveDefinitionDescription = "Map a function over the keys of a map.",
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
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
member :: Packaging.PrimitiveDefinition
member =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maps.member"),
      Packaging.primitiveDefinitionDescription = "Test whether a key is present in a map.",
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
      Packaging.primitiveDefinitionDescription = "Test whether a map is empty.",
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
      Packaging.primitiveDefinitionDescription = "Construct a map with a single key-value pair.",
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
      Packaging.primitiveDefinitionDescription = "Return the number of key-value pairs in a map.",
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
      Packaging.primitiveDefinitionDescription = "Convert a map to a list of key-value pairs (in key order).",
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
      Packaging.primitiveDefinitionDescription = "Compute the union of two maps; the first map's bindings take precedence on key collision.",
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
