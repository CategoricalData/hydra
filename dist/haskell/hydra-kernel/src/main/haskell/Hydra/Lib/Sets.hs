-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.sets module.

module Hydra.Lib.Sets where
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
delete :: Packaging.PrimitiveDefinition
delete =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.delete"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Remove an element from a set."),
        Packaging.entityMetadataComments = [
          "delete(x, s) returns s with x removed; if x is not in s, s is returned unchanged.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.delete :: Ord a => a -> Set a -> Set a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
difference :: Packaging.PrimitiveDefinition
difference =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.difference"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Compute the difference of two sets: elements in the first that are not in the second."),
        Packaging.entityMetadataComments = [
          "difference(s1, s2) returns the set of elements that are in s1 but not in s2.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.difference :: Ord a => Set a -> Set a -> Set a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "s1"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "s2"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "acc"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "el"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.member")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "el"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "s2"))}))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "el"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))}))}))}))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.sets.empty"))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.toList")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "s1"))}))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Set difference, defined in terms of member and insert.")))])})))}
empty :: Packaging.PrimitiveDefinition
empty =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.empty"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "The empty set."),
        Packaging.entityMetadataComments = [
          "empty is the set with no elements.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.empty :: Set a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
fromList :: Packaging.PrimitiveDefinition
fromList =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.fromList"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Construct a set from a list of elements (duplicates removed)."),
        Packaging.entityMetadataComments = [
          "fromList(xs) returns the set containing exactly the distinct elements of xs; duplicates are discarded.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.fromList :: Ord a => [a] -> Set a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
insert :: Packaging.PrimitiveDefinition
insert =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.insert"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Add an element to a set."),
        Packaging.entityMetadataComments = [
          "insert(x, s) returns s with x added; if x is already in s, s is returned unchanged.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.insert :: Ord a => a -> Set a -> Set a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
intersection :: Packaging.PrimitiveDefinition
intersection =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.intersection"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Compute the intersection of two sets: elements present in both."),
        Packaging.entityMetadataComments = [
          "intersection(s1, s2) returns the set of elements present in both s1 and s2.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.intersection :: Ord a => Set a -> Set a -> Set a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "s1"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "s2"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "acc"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "el"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.member")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "el"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "s2"))}))})),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "el"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))}))}))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.sets.empty"))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.toList")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "s1"))}))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Set intersection, defined in terms of member and insert.")))])})))}
map :: Packaging.PrimitiveDefinition
map =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.map"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Map a function over a set."),
        Packaging.entityMetadataComments = [
          "map(f, s) returns the set of f(x) for each x in s. Elements that f maps to the same image are deduplicated by the result type's ordering.",
          "Requires 'ordering' constraints on both the input and output element types.",
          "Total. Corresponds to Haskell's Data.Set.map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "y")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "f"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "s"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.fromList")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.toList")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Map a function over a set, defined in terms of toList, lists.map and fromList.")))])})))}
member :: Packaging.PrimitiveDefinition
member =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.member"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Test whether an element is in a set."),
        Packaging.entityMetadataComments = [
          "member(x, s) returns true iff x is an element of s.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.member :: Ord a => a -> Set a -> Bool."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
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
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.null"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Test whether a set is empty."),
        Packaging.entityMetadataComments = [
          "null(s) returns true iff s has no elements.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.null :: Set a -> Bool."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
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
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.singleton"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Construct a set containing a single element."),
        Packaging.entityMetadataComments = [
          "singleton(x) returns the set containing exactly the element x.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.singleton :: a -> Set a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
size :: Packaging.PrimitiveDefinition
size =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.size"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Return the number of elements in a set."),
        Packaging.entityMetadataComments = [
          "size(s) returns the number of elements in s as an int32.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.size :: Set a -> Int (with narrowing to int32)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
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
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.toList"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Convert a set to a list (in ascending order)."),
        Packaging.entityMetadataComments = [
          "toList(s) returns the elements of s as a list, in ascending order under the element type's ordering.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.toAscList :: Set a -> [a]."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
union :: Packaging.PrimitiveDefinition
union =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.union"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Compute the union of two sets: elements in either."),
        Packaging.entityMetadataComments = [
          "union(s1, s2) returns the set of elements that are in s1 or in s2 (or both).",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.union :: Ord a => Set a -> Set a -> Set a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "s1"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "s2"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "acc"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "el"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "el"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))}))}))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "s2"))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.toList")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "s1"))}))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Set union, defined in terms of insert and toList.")))])})))}
unions :: Packaging.PrimitiveDefinition
unions =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.sets.unions"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Compute the union of a list of sets."),
        Packaging.entityMetadataComments = [
          "unions(ss) returns the union of every set in ss. Equivalent to folding union over ss starting from empty.",
          "Requires an 'ordering' constraint on the element type.",
          "Total. Corresponds to Haskell's Data.Set.unions :: Ord a => [Set a] -> Set a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = [
              Core.TypeClassConstraintSimple (Core.Name "ordering")]}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeSet (Core.TypeVariable (Core.Name "x")))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeSet (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "ss"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "acc"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "s"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.union")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}))}))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.sets.empty"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "ss"))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Union of a list of sets, defined in terms of foldl and union.")))])})))}
