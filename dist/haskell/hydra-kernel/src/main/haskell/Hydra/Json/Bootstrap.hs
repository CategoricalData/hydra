-- Note: this is an automatically generated file. Do not edit.
-- | A module which provides a minimal typing environment for decoding other modules from JSON. This avoids certain problems with generating entire source modules into target languages like Java, which is subject to method size limits for large modules like hydra.core.

module Hydra.Json.Bootstrap where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Paths as Paths
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | A bootstrap typing environment for decoding modules from JSON. Maps each kernel type name to its encoded type, used to seed JSON decoding before the full kernel graph is available.
typesByName :: M.Map Core.Name Core.Type
typesByName =
    M.fromList [
      (
        Core.Name "hydra.coders.Adapter",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t2"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "v1"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "v2"),
                  Core.forallTypeBody = (Core.TypeRecord [
                    Core.FieldType {
                      Core.fieldTypeName = (Core.Name "isLossy"),
                      Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                        Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeBoolean),
                        Core.annotatedTypeAnnotation = (M.fromList [
                          (
                            Core.Name "description",
                            (Core.TermLiteral (Core.LiteralString "Whether information may be lost in the course of this adaptation")))])}))},
                    Core.FieldType {
                      Core.fieldTypeName = (Core.Name "source"),
                      Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                        Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "t1")),
                        Core.annotatedTypeAnnotation = (M.fromList [
                          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The source type")))])}))},
                    Core.FieldType {
                      Core.fieldTypeName = (Core.Name "target"),
                      Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                        Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "t2")),
                        Core.annotatedTypeAnnotation = (M.fromList [
                          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The target type")))])}))},
                    Core.FieldType {
                      Core.fieldTypeName = (Core.Name "coder"),
                      Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                        Core.annotatedTypeBody = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.coders.Coder")),
                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v1"))})),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v2"))})),
                        Core.annotatedTypeAnnotation = (M.fromList [
                          (
                            Core.Name "description",
                            (Core.TermLiteral (Core.LiteralString "The coder for transforming instances of the source type to instances of the target type")))])}))}])}))}))}))})),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A two-level bidirectional encoder which adapts types to types and terms to terms")))])}))),
      (
        Core.Name "hydra.coders.AdapterContext",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "graph"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The underlying graph of elements and primitives")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "language"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.coders.Language")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The language being encoded or decoded")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "adapters"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.coders.Adapter")),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.core.Type"))})),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.core.Type"))})),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A map of type names to adapters for those types")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "An evaluation context together with a source language and a target language")))])}))),
      (
        Core.Name "hydra.coders.Bicoder",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t2"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "v1"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "v2"),
                  Core.forallTypeBody = (Core.TypeRecord [
                    Core.FieldType {
                      Core.fieldTypeName = (Core.Name "encode"),
                      Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                        Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                          Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.coders.Adapter")),
                                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t2"))})),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v1"))})),
                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v2"))}))})),
                        Core.annotatedTypeAnnotation = (M.fromList [
                          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function from source types to adapters")))])}))},
                    Core.FieldType {
                      Core.fieldTypeName = (Core.Name "decode"),
                      Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                        Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                          Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.coders.Adapter")),
                                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t2"))})),
                                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v2"))})),
                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v1"))}))})),
                        Core.annotatedTypeAnnotation = (M.fromList [
                          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function from target types to adapters")))])}))}])}))}))}))})),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A two-level encoder and decoder, operating both at a type level and an instance (data) level")))])}))),
      (
        Core.Name "hydra.coders.Coder",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "v1"),
            Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "v2"),
              Core.forallTypeBody = (Core.TypeRecord [
                Core.FieldType {
                  Core.fieldTypeName = (Core.Name "encode"),
                  Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                    Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.typing.InferenceContext")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v1")),
                        Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                          Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.Error")),
                          Core.eitherTypeRight = (Core.TypeVariable (Core.Name "v2"))}))}))})),
                    Core.annotatedTypeAnnotation = (M.fromList [
                      (
                        Core.Name "description",
                        (Core.TermLiteral (Core.LiteralString "A function which encodes source values as target values, given an InferenceContext for fresh-variable state and subterm-path tracing")))])}))},
                Core.FieldType {
                  Core.fieldTypeName = (Core.Name "decode"),
                  Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                    Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.typing.InferenceContext")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v2")),
                        Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                          Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.Error")),
                          Core.eitherTypeRight = (Core.TypeVariable (Core.Name "v1"))}))}))})),
                    Core.annotatedTypeAnnotation = (M.fromList [
                      (
                        Core.Name "description",
                        (Core.TermLiteral (Core.LiteralString "A function which decodes target values as source values, given an InferenceContext for fresh-variable state and subterm-path tracing")))])}))}])}))})),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "An encoder and decoder; a bidirectional transformation between two types")))])}))),
      (
        Core.Name "hydra.coders.CoderDirection",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "encode"),
              Core.fieldTypeType = Core.TypeUnit},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "decode"),
              Core.fieldTypeType = Core.TypeUnit}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "Indicates either the 'out' or the 'in' direction of a coder")))])}))),
      (
        Core.Name "hydra.coders.Language",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.coders.LanguageName")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The unique name of the language")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "constraints"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.coders.LanguageConstraints")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The constraints which characterize the language")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A named language together with language-specific constraints")))])}))),
      (
        Core.Name "hydra.coders.LanguageConstraints",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "literalVariants"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.variants.LiteralVariant"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "All supported literal variants")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "floatTypes"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.core.FloatType"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "All supported float types")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "integerTypes"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.core.IntegerType"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "All supported integer types")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "termVariants"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.variants.TermVariant"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "All supported term variants")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeVariants"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.variants.TypeVariant"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "All supported type variants")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "types"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A logical set of types, as a predicate which tests a type for inclusion")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A set of constraints on valid type and term expressions, characterizing a language")))])}))),
      (
        Core.Name "hydra.coders.LanguageName",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The unique name of a language")))])}))),
      (
        Core.Name "hydra.coders.SymmetricAdapter",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t"),
            Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "v"),
              Core.forallTypeBody = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.coders.Adapter")),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))})),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))})),
                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))}))}))})),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A bidirectional encoder which maps between the same type and term languages on either side")))])}))),
      (
        Core.Name "hydra.coders.TraversalOrder",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "pre"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Pre-order traversal")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "post"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Post-order traversal")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Specifies either a pre-order or post-order traversal")))])}))),
      (
        Core.Name "hydra.coders.TypeAdapter",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.coders.AdapterContext")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Type")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                Core.eitherTypeRight = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.coders.SymmetricAdapter")),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.core.Type"))})),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))}))}))})),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A function which maps a Hydra type to a symmetric adapter between types and terms")))])}))),
      (
        Core.Name "hydra.core.AnnotatedTerm",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "body"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The term being annotated")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "annotation"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The annotation as a map from keys to values")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A term together with an annotation")))])}))),
      (
        Core.Name "hydra.core.AnnotatedType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "body"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type being annotated")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "annotation"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The annotation as a map from keys to values")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type together with an annotation")))])}))),
      (
        Core.Name "hydra.core.Application",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "function"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The left-hand side of the application")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "argument"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The right-hand side of the application")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A term which applies a function to an argument")))])}))),
      (
        Core.Name "hydra.core.ApplicationType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "function"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The left-hand side of the application")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "argument"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The right-hand side of the application")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type-level analog of an application term")))])}))),
      (
        Core.Name "hydra.core.Binding",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the bound variable")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "term"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The term to which the variable is bound")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeScheme"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.core.TypeScheme"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The optional type scheme of the bound term")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A field with an optional type scheme, used to bind variables to terms in a 'let' expression")))])}))),
      (
        Core.Name "hydra.core.CaseStatement",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeName"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the union type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "default"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "An optional default case, used if none of the explicit cases match")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "cases"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Field"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A list of case alternatives, one per union field. Each Field's name is the variant tag being matched and term is the handler applied to the variant's payload.")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A union elimination; a case statement")))])}))),
      (
        Core.Name "hydra.core.EitherType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "left"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The 'left' alternative")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "right"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The 'right' alternative")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A type which provides a choice between a 'left' type and a 'right' type")))])}))),
      (
        Core.Name "hydra.core.Field",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the field")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "term"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The term value of the field")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A name/term pair")))])}))),
      (
        Core.Name "hydra.core.FieldType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the field")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "type"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of the field")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A name/type pair")))])}))),
      (
        Core.Name "hydra.core.FloatType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "float32"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 32-bit floating-point type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "float64"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 64-bit floating-point type")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A floating-point type")))])}))),
      (
        Core.Name "hydra.core.FloatValue",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "float32"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 32-bit floating-point value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "float64"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 64-bit floating-point value")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A floating-point literal value")))])}))),
      (
        Core.Name "hydra.core.ForallType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "parameter"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The variable which is bound by the lambda")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "body"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The body of the lambda")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term.")))])}))),
      (
        Core.Name "hydra.core.FunctionType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "domain"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The domain (input) type of the function")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "codomain"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The codomain (output) type of the function")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function type, also known as an arrow type")))])}))),
      (
        Core.Name "hydra.core.Injection",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeName"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the union type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "field"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Field")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The field being injected, including its name and value")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "An instance of a union type; i.e. a string-indexed generalization of inl() or inr()")))])}))),
      (
        Core.Name "hydra.core.IntegerType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "bigint"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An arbitrary-precision integer type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "int8"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An 8-bit signed integer type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "int16"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 16-bit signed integer type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "int32"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 32-bit signed integer type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "int64"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 64-bit signed integer type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "uint8"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An 8-bit unsigned integer type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "uint16"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 16-bit unsigned integer type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "uint32"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 32-bit unsigned integer type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "uint64"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 64-bit unsigned integer type")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An integer type")))])}))),
      (
        Core.Name "hydra.core.IntegerValue",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "bigint"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An arbitrary-precision integer value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "int8"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An 8-bit signed integer value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "int16"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 16-bit signed integer value (short value)")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "int32"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 32-bit signed integer value (int value)")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "int64"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 64-bit signed integer value (long value)")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "uint8"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An 8-bit unsigned integer value (byte)")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "uint16"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 16-bit unsigned integer value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "uint32"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint32)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 32-bit unsigned integer value (unsigned int)")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "uint64"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint64)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 64-bit unsigned integer value (unsigned long)")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An integer literal value")))])}))),
      (
        Core.Name "hydra.core.Lambda",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "parameter"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The parameter of the lambda")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "domain"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.core.Type"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An optional domain type for the lambda")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "body"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The body of the lambda")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function abstraction (lambda)")))])}))),
      (
        Core.Name "hydra.core.Let",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "bindings"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Binding"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The list of variable bindings")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "body"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The body term in which the variables are bound")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A set of (possibly recursive) 'let' bindings together with a body in which they are bound")))])}))),
      (
        Core.Name "hydra.core.Literal",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "binary"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeBinary),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A binary literal")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "boolean"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeBoolean),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A boolean literal")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "decimal"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeDecimal),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An arbitrary-precision decimal literal")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "float"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.FloatValue")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A floating-point literal")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "integer"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.IntegerValue")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An integer literal")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "string"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A string literal")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A term constant; an instance of a literal type")))])}))),
      (
        Core.Name "hydra.core.LiteralType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "binary"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of a binary (byte string) value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "boolean"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of a boolean (true/false) value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "decimal"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of an arbitrary-precision decimal value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "float"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.FloatType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of a floating-point value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "integer"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.IntegerType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of an integer value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "string"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of a string value")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants")))])}))),
      (
        Core.Name "hydra.core.MapType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "keys"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of keys in the map")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "values"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of values in the map")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A map type")))])}))),
      (
        Core.Name "hydra.core.Name",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A unique identifier in some context; a string-valued key")))])}))),
      (
        Core.Name "hydra.core.PairType",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "first"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The first component of the pair")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "second"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The second component of the pair")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type which pairs a 'first' type and a 'second' type")))])}))),
      (
        Core.Name "hydra.core.Projection",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeName"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the record type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "fieldName"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the projected field")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A record elimination; a projection")))])}))),
      (
        Core.Name "hydra.core.Record",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeName"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the record type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "fields"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Field"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The fields of the record, as a list of name/term pairs")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A record, or labeled tuple; a map of field names to terms")))])}))),
      (
        Core.Name "hydra.core.Term",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "annotated"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.AnnotatedTerm")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A term annotated with metadata")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "application"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Application")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function application")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "cases"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.CaseStatement")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A union elimination; a case statement")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "either"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An either value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "inject"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Injection")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An injection; an instance of a union type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "lambda"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Lambda")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function abstraction (lambda)")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "let"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Let")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A 'let' term, which binds variables to terms")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "list"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A list")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "literal"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Literal")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A literal value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "map"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A map of keys to values")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "maybe"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An optional value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "pair"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A pair (2-tuple)")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "project"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Projection")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A record elimination; a projection")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "record"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Record")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A record term")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "set"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A set of values")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeApplication"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.TypeApplicationTerm")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A System F type application term")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeLambda"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.TypeLambda")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A System F type abstraction term")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "unit"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A unit value; a term with no value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "unwrap"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "An unwrap elimination; the inverse of a wrap. Given the name of a wrapper type, unwraps an instance of that type to its underlying body value.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "variable"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A variable reference")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "wrap"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.WrappedTerm")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A wrapped term; an instance of a wrapper type (newtype)")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A data term")))])}))),
      (
        Core.Name "hydra.core.Type",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "annotated"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.AnnotatedType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An annotated type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "application"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.ApplicationType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type application")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "either"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.EitherType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An either (sum) type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "forall"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.ForallType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A universally quantified (polymorphic) type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "function"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.FunctionType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "list"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A list type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "literal"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.LiteralType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A literal type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "map"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.MapType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A map type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "maybe"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An optional type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "pair"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.PairType")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A pair (2-tuple) type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "record"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.FieldType"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A record type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "set"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A set type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "union"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.FieldType"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A union type with field names")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "unit"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The unit type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "variable"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type variable")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "void"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The void (uninhabited, or bottom) type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "wrap"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A wrapped type (newtype). There is no corresponding `unwrap` variant at the type level: wrap is the introduction form, and a wrapper type's underlying body type is given by the `wrap` variant's argument.")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A data type")))])}))),
      (
        Core.Name "hydra.core.TypeApplicationTerm",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "body"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The term being applied to a type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "type"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type argument")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A term applied to a type; a type application")))])}))),
      (
        Core.Name "hydra.core.TypeClassConstraint",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "simple"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A simple type class constraint, naming a single type class")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A type class constraint on a type variable. Currently has only one variant, but designed to be forward-compatible with multi-parameter type classes and constraints on type expressions.")))])}))),
      (
        Core.Name "hydra.core.TypeLambda",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "parameter"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type variable introduced by the abstraction")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "body"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The body of the abstraction")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A System F type abstraction term")))])}))),
      (
        Core.Name "hydra.core.TypeScheme",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "variables"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Name"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The free type variables")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "body"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type expression")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "constraints"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.TypeVariableConstraints"))}))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Optional metadata for type variables, including typeclass constraints. The map keys are type variable names.")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A type expression together with free type variables occurring in the expression")))])}))),
      (
        Core.Name "hydra.core.TypeVariableConstraints",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "classes"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.TypeClassConstraint"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The typeclass constraints on this type variable")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "Metadata associated with a type variable, including typeclass constraints")))])}))),
      (
        Core.Name "hydra.core.WrappedTerm",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeName"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the wrapper type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "body"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The wrapped term")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A term wrapped in a type name")))])}))),
      (
        Core.Name "hydra.errors.DecodingError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An error that occurred during decoding of a term")))])}))),
      (
        Core.Name "hydra.errors.EmptyListError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = Core.TypeUnit,
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "An empty list was encountered where a non-empty list was required")))])}))),
      (
        Core.Name "hydra.errors.Error",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "checking"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.checking.CheckingError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type checking error")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "decoding"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An error that occurred during decoding of a term")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "duplicateBinding"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateBindingError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A duplicate binding name error")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "duplicateField"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateFieldError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A duplicate field name error")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "extraction"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.ExtractionError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "An error that occurred while extracting a value from a term")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "inference"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.InferenceError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type inference error")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "invalidLiteral"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidLiteralError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A literal value validation error")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "other"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.OtherError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any other error")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "resolution"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.ResolutionError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A name-resolution error")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "undefinedField"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedFieldError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A reference to an undefined field")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "undefinedTermVariable"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTermVariableError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A reference to an undefined term variable")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "untypedTermVariable"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.core.UntypedTermVariableError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A term variable whose type is not known")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "unexpectedTermVariant"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.core.UnexpectedTermVariantError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An unexpected term variant")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "unexpectedTypeVariant"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.core.UnexpectedTypeVariantError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An unexpected type variant")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "unification"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.UnificationError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type unification error")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "An error of any kind, with kernel errors particularly differentiated")))])}))),
      (
        Core.Name "hydra.errors.ExtractionError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "emptyList"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.EmptyListError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "An empty list was encountered where a non-empty list was required")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "multipleBindings"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.MultipleBindingsError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Multiple let bindings were found with the same name")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "multipleFields"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.MultipleFieldsError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Multiple record fields were found with the same field name")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "noMatchingField"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.NoMatchingFieldError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "No field with the expected name was found in a record")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "noSuchBinding"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.NoSuchBindingError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "No let binding with the expected name was found")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "notEnoughCases"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.NotEnoughCasesError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A case statement did not contain enough cases to match the target")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "unexpectedShape"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.UnexpectedShapeError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A term, type, literal, or other value had an unexpected shape")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "An error that occurred while extracting a typed value from a term")))])}))),
      (
        Core.Name "hydra.errors.InferenceError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "checking"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.error.checking.CheckingError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type checking error encountered during inference")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "other"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.OtherInferenceError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A generic inference error carrying a message and a subterm path. Placeholder arm; sites should migrate to typed variants.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "unification"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.UnificationInferenceError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A unification failure encountered while inferring types")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An error that occurred during type inference")))])}))),
      (
        Core.Name "hydra.errors.MultipleBindingsError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The binding name which was duplicated")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Multiple let bindings with the same name were found")))])}))),
      (
        Core.Name "hydra.errors.MultipleFieldsError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "fieldName"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The field name which appeared more than once")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "Multiple fields with the same name were found in a record")))])}))),
      (
        Core.Name "hydra.errors.NoMatchingFieldError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "fieldName"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The field name which was not found")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "No field with the expected name was present")))])}))),
      (
        Core.Name "hydra.errors.NoSuchBindingError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The binding name which was not found")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "No let binding with the expected name was present")))])}))),
      (
        Core.Name "hydra.errors.NoSuchPrimitiveError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The primitive name which was not found")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "No primitive function with the expected name was registered in the graph")))])}))),
      (
        Core.Name "hydra.errors.NotEnoughCasesError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = Core.TypeUnit,
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A case statement was missing a case for the requested variant")))])}))),
      (
        Core.Name "hydra.errors.OtherError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any other error")))])}))),
      (
        Core.Name "hydra.errors.OtherInferenceError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "path"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.paths.SubtermPath")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The subterm path at which the error was observed")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "message"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A human-readable error message")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A generic inference error: message + subterm path")))])}))),
      (
        Core.Name "hydra.errors.OtherResolutionError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A generic resolution error: message")))])}))),
      (
        Core.Name "hydra.errors.ResolutionError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "noSuchBinding"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.NoSuchBindingError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "No binding with the expected name was found in the graph")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "noSuchPrimitive"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.NoSuchPrimitiveError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "No primitive function with the expected name was found in the graph")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "noMatchingField"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.NoMatchingFieldError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "No field with the expected name was present in a record or case statement")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "other"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.OtherResolutionError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A generic resolution error carrying a message")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "unexpectedShape"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.UnexpectedShapeError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A term had a shape other than the one expected (e.g. a record, an injection)")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "An error that occurred while resolving a name, primitive, or record/union shape in a graph")))])}))),
      (
        Core.Name "hydra.errors.UnexpectedShapeError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "expected"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A description of the expected shape")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "actual"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A description of the shape actually encountered")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A term, type, literal, or related value had a shape other than the one expected")))])}))),
      (
        Core.Name "hydra.errors.UnificationError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "leftType"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The left-hand type in the unification")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "rightType"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The right-hand type in the unification")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "message"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A human-readable error message")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An error that occurred during type unification")))])}))),
      (
        Core.Name "hydra.errors.UnificationInferenceError",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "path"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.paths.SubtermPath")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The subterm path at which the unification failure was observed")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "cause"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.UnificationError")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The underlying unification error")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A unification failure at a specific subterm locus during inference")))])}))),
      (
        Core.Name "hydra.graph.Graph",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "boundTerms"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The terms bound by all term variables in scope")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "boundTypes"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type schemes of all term variables in scope")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "classConstraints"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.TypeVariableConstraints"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "lambdaVariables"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.core.Name"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The set of term variables introduced by specifically by lambdas")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "metadata"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Any additional metadata bound to term variables in scope")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "primitives"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.graph.Primitive"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "All primitive functions and constants by name")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "schemaTypes"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "All schema types (type schemes) in scope")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeVariables"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.core.Name"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The set of type variables introduced specifically by type lambdas")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A graph, or lexical environment which binds names to terms, types, primitives, and metadata")))])}))),
      (
        Core.Name "hydra.graph.Library",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "namespace"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.ModuleName")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A common prefix for all primitive function names in the library")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "prefix"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A preferred namespace prefix for function names in the library")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "primitives"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.graph.Primitive"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The primitives defined in this library")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A library of primitive functions")))])}))),
      (
        Core.Name "hydra.graph.Primitive",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "definition"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.PrimitiveDefinition")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The host-independent declarative metadata for the primitive: name, description, signature, totality and purity flags, and an optional reference implementation.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "implementation"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.typing.InferenceContext")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                      Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                        Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.Error")),
                        Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))}))}))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A concrete implementation of the primitive function. The InferenceContext and Graph parameters are needed by higher-order primitives (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments via term reduction; the Graph provides variable and primitive bindings, while the InferenceContext supports subterm-path tracing for error reporting.")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A built-in function or constant, consisting of the host-independent PrimitiveDefinition (name, signature, metadata) plus a host-specific implementation.")))])}))),
      (
        Core.Name "hydra.graph.TermCoder",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "a"),
            Core.forallTypeBody = (Core.TypeRecord [
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "type"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The Hydra type of encoded terms")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "encode"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.typing.InferenceContext")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                        Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                          Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.Error")),
                          Core.eitherTypeRight = (Core.TypeVariable (Core.Name "a"))}))}))}))})),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An encode function from terms to native values")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "decode"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.typing.InferenceContext")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                        Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.Error")),
                        Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))}))})),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A decode function from native values to terms")))])}))}])})),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms.")))])}))),
      (
        Core.Name "hydra.packaging.Definition",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "term"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.TermDefinition")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A term definition")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "type"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.TypeDefinition")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type definition")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "primitive"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.PrimitiveDefinition")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A primitive definition")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A definition, which may be either a term, type, or primitive definition")))])}))),
      (
        Core.Name "hydra.packaging.DefinitionReference",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "type"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A reference to a type definition, by name")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "term"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A reference to a term definition, by name")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "primitive"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A reference to a primitive definition, by name")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A typed reference to a definition: a type, a term, or a primitive, identified by name")))])}))),
      (
        Core.Name "hydra.packaging.LifecycleInfo",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "availableSince"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.Version"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The version in which the entity was introduced, if known.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "deprecatedSince"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.Version"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The version in which the entity was deprecated, if applicable.")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "Version-lifecycle milestones for a packaging entity. Each milestone is independently optional; further milestones (e.g. stableSince, removedSince) may be added without changing dependent types.")))])}))),
      (
        Core.Name "hydra.packaging.EntityMetadata",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "description"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "An optional, concise one-line human-readable summary of the entity.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "comments"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Zero or more long-form prose paragraphs: cross-cutting semantic conventions, caveats, and references that would otherwise be repeated across the entity's constituents.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "seeAlso"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.EntityReference"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Typed cross-references to related entities, for navigation and documentation.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "lifecycle"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.LifecycleInfo"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Optional version-lifecycle milestones for the entity.")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "Documentation and lifecycle metadata attachable to a packaging entity (package, module, or definition). Bundling these fields in one type lets future metadata be added without changing the field shape of the entities that carry it.")))])}))),
      (
        Core.Name "hydra.packaging.EntityReference",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "package"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.PackageName")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A reference to a package, by name")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "module"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.ModuleName")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A reference to a module, by name")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "definition"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.DefinitionReference")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A reference to a definition (type, term, or primitive)")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A typed reference to a packaging entity: a package, a module, or a definition")))])}))),
      (
        Core.Name "hydra.util.FileExtension",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A file extension (without the dot), e.g. \"json\" or \"py\"")))])}))),
      (
        Core.Name "hydra.packaging.Module",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.ModuleName")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The name of the module, which is also the common prefix for all element names in the module")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "metadata"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.EntityMetadata"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Optional documentation and lifecycle metadata for the module")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "dependencies"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.ModuleDependency"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any modules which this module directly depends on")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "definitions"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.Definition"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The definitions in this module")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A logical collection of elements sharing a common module name, having dependencies on zero or more other modules")))])}))),
      (
        Core.Name "hydra.packaging.ModuleDependency",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "module"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.ModuleName")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the depended-on module")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "package"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.PackageName"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The package providing the depended-on module, if disambiguation is required")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A dependency on another module, identified by its name and (optionally) the package which provides it. When the package is omitted, the resolver searches all packages in scope; a duplicate module name across packages is a resolution error which can be disambiguated by naming the intended package explicitly.")))])}))),
      (
        Core.Name "hydra.packaging.ModuleName",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "The unique name of a module; a prefix for the names of elements defined in the module.")))])}))),
      (
        Core.Name "hydra.packaging.Package",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.PackageName")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the package")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "metadata"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.EntityMetadata"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Optional documentation and lifecycle metadata for the package")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "dependencies"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.PackageDependency"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The packages which this package depends on")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "modules"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.Module"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The modules in this package")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A package, which is a named collection of modules with metadata and dependencies")))])}))),
      (
        Core.Name "hydra.packaging.PackageDependency",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.PackageName")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the depended-on package")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "version"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.VersionSpecifier")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The version-range constraint on the depended-on package")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A dependency on another package, identified by name and constrained by an optional version specifier")))])}))),
      (
        Core.Name "hydra.packaging.PackageName",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "The unique name of a package, e.g. \"hydra-kernel\" or \"hydra-python\"")))])}))),
      (
        Core.Name "hydra.packaging.VersionSpecifier",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "any"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any version of the package satisfies the dependency")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A specifier constraining acceptable versions of a depended-on package. Currently only the `any` (unit) specifier is defined; future variants such as `exact`, `caret`, and `range` may be added without breaking consumers of the `any` form.")))])}))),
      (
        Core.Name "hydra.packaging.PrimitiveDefinition",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the primitive")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "signature"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.typing.TermSignature")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The signature of the primitive (always explicit, never inferred)")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "metadata"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.EntityMetadata"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Optional documentation and lifecycle metadata for the primitive (description, long-form comments, cross-references, version milestones).")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "isPure"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeBoolean),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Whether the primitive is pure (referentially transparent, no observable side effects). Defaults to true.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "isTotal"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeBoolean),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Whether the primitive is total (terminates on every input of its declared type). Defaults to true.")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "defaultImplementation"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "An optional cross-compilable reference implementation of the primitive, expressed as a Hydra term. Used by interpreters lacking a native implementation and as a proof-friendly reference. Distinct from the per-host Primitive.implementation.")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A primitive definition: the universal, host-independent declarative metadata for a primitive, including name, signature, documentation and lifecycle metadata, totality and purity flags, and an optional default implementation expressed as a Hydra term.")))])}))),
      (
        Core.Name "hydra.util.QualifiedName",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "moduleName"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.ModuleName"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The optional module name")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "local"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The local name")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A qualified name consisting of an optional module name together with a mandatory local name")))])}))),
      (
        Core.Name "hydra.packaging.TermDefinition",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the term")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "metadata"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.EntityMetadata"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Optional documentation and lifecycle metadata for the term definition")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "term"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The term being defined")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "signature"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.typing.TermSignature"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The optional signature of the term. When absent, the signature is to be inferred.")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A term-level definition, including a name, a term, and an optional signature")))])}))),
      (
        Core.Name "hydra.packaging.TypeDefinition",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the type")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "metadata"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.EntityMetadata"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Optional documentation and lifecycle metadata for the type definition")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeScheme"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type scheme being defined")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A type-level definition, including a name and the type scheme")))])}))),
      (
        Core.Name "hydra.packaging.Version",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A version string, e.g. \"0.15\" or \"1.0.0\".")))])}))),
      (
        Core.Name "hydra.typing.FunctionStructure",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "env"),
            Core.forallTypeBody = (Core.TypeRecord [
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "typeParams"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Name"))),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Type parameters (from type lambdas)")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "params"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Name"))),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Value parameters (from lambdas)")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "bindings"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Binding"))),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Let bindings accumulated from the term")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "body"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The body term after removing all lambdas, lets, etc.")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "domains"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Type"))),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Domain types of the value parameters")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "codomain"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.core.Type"))),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (
                      Core.Name "description",
                      (Core.TermLiteral (Core.LiteralString "The return type of the function (if type inference succeeded)")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "environment"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "env")),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Updated environment after processing all bindings")))])}))}])})),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A structured representation of a function term's components, replacing ad-hoc tuples. This captures all the information extracted from peeling lambdas, type lambdas, lets, and type applications from a term.")))])}))),
      (
        Core.Name "hydra.typing.InferenceContext",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "freshTypeVariableCount"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Counter used to generate distinct fresh type variables during inference")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "trace"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.paths.SubtermStep"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The current subterm-path trace, accumulated backwards (head = most-recently-pushed step, corresponding to the deepest point in the descent). At the moment an inference error is constructed, the list is reversed and wrapped into a SubtermPath (root-to-leaf order) and stamped onto the error.")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "State threaded through type inference: the fresh type variable counter and the current subterm-path trace.")))])}))),
      (
        Core.Name "hydra.typing.InferenceResult",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "term"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The term which was inferred")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "type"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The inferred type of the term")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "subst"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.typing.TypeSubst")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type substitution resulting from unification")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "classConstraints"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.TypeVariableConstraints"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Class constraints discovered during inference (e.g., Ord constraints from Map.lookup)")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "context"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.typing.InferenceContext")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "The updated InferenceContext after inference (carries fresh-variable counter and trace)")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The result of applying inference rules to a term.")))])}))),
      (
        Core.Name "hydra.typing.Parameter",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the parameter")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "description"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An optional human-readable description of the parameter")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "type"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of the parameter")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "isLazy"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeBoolean),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "Whether the parameter must be passed lazily (thunked) at call sites in hosts that distinguish strict from lazy evaluation")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A named, typed parameter of a term, with optional human-readable description and a flag indicating whether the parameter requires lazy evaluation by hosts which support it.")))])}))),
      (
        Core.Name "hydra.typing.Result",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "description"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An optional human-readable description of the result")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "type"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type of the result")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "The result of a term, consisting of a type and an optional human-readable description.")))])}))),
      (
        Core.Name "hydra.typing.TermSignature",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "typeParameters"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.typing.TypeParameter"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type parameters of the term, in order")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "parameters"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.typing.Parameter"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The value parameters of the term, in order")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "result"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.typing.Result")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The result of the term")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A structured signature for a term: an ordered list of type parameters (with optional class constraints), an ordered list of value parameters, and a result. TermSignature is a richer view of TypeScheme: every TermSignature can be converted to a TypeScheme by erasing parameter names, descriptions, and laziness flags.")))])}))),
      (
        Core.Name "hydra.typing.TermSubst",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A substitution of term variables for terms")))])}))),
      (
        Core.Name "hydra.typing.TypeClass",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "description"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A human-readable description of the type class")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A type class identifier together with a human-readable description. Type classes are referenced as bare names (e.g. the local name \"equality\") in TypeVariableConstraints.classes; the canonical definitions live as term bindings under hydra.classes.")))])}))),
      (
        Core.Name "hydra.typing.TypeConstraint",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "left"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The left-hand side of the constraint")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "right"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The right-hand side of the constraint")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "comment"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (
                    Core.Name "description",
                    (Core.TermLiteral (Core.LiteralString "A description of the type constraint which may be used for tracing or debugging")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "An assertion that two types can be unified into a single type")))])}))),
      (
        Core.Name "hydra.typing.TypeParameter",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "name"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the type parameter")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "constraints"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.TypeClassConstraint"))),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any type class constraints on the type parameter")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A type parameter of a term, with an optional list of type class constraints")))])}))),
      (
        Core.Name "hydra.typing.TypeSubst",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
            Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Type"))}))),
          Core.annotatedTypeAnnotation = (M.fromList [
            (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A substitution of type variables for types")))])}))),
      (
        Core.Name "hydra.util.CaseConvention",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "camel"),
              Core.fieldTypeType = Core.TypeUnit},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "pascal"),
              Core.fieldTypeType = Core.TypeUnit},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "lowerSnake"),
              Core.fieldTypeType = Core.TypeUnit},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "upperSnake"),
              Core.fieldTypeType = Core.TypeUnit}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A naming convention for symbols, such as camelCase or snake_case")))])}))),
      (
        Core.Name "hydra.util.Comparison",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "lessThan"),
              Core.fieldTypeType = Core.TypeUnit},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "equalTo"),
              Core.fieldTypeType = Core.TypeUnit},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "greaterThan"),
              Core.fieldTypeType = Core.TypeUnit}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "An equality judgement: less than, equal to, or greater than")))])}))),
      (
        Core.Name "hydra.util.ModuleNames",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "n"),
            Core.forallTypeBody = (Core.TypeRecord [
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "focus"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeVariable (Core.Name "hydra.packaging.ModuleName")),
                    Core.pairTypeSecond = (Core.TypeVariable (Core.Name "n"))})),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (
                      Core.Name "description",
                      (Core.TermLiteral (Core.LiteralString "The module name in focus, together with its associated value")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "mapping"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.packaging.ModuleName")),
                    Core.mapTypeValues = (Core.TypeVariable (Core.Name "n"))})),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A mapping of module names to values")))])}))}])})),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "A mapping from module names to values of type n, with a focus on one module name")))])}))),
      (
        Core.Name "hydra.util.Precision",
        (Core.TypeAnnotated (Core.AnnotatedType {
          Core.annotatedTypeBody = (Core.TypeUnion [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "arbitrary"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = Core.TypeUnit,
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Arbitrary precision")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "bits"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Precision to a specified number of bits")))])}))}]),
          Core.annotatedTypeAnnotation = (M.fromList [
            (
              Core.Name "description",
              (Core.TermLiteral (Core.LiteralString "Numeric precision: arbitrary precision, or precision to a specified number of bits")))])})))]
