-- Note: this is an automatically generated file. Do not edit.

-- | A module which provides a minimal typing environment for decoding other modules from JSON. This avoids certain problems with generating entire source modules into target languages like Java, which is subject to method size limits for large modules like hydra.core.

module Hydra.Json.Bootstrap where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

typesByName :: M.Map Core.Name Core.Type
typesByName =
    M.fromList [
      (Core.Name "hydra.coders.Adapter", (Core.TypeAnnotated (Core.AnnotatedType {
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
                        (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Whether information may be lost in the course of this adaptation")))])}))},
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
                        (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The coder for transforming instances of the source type to instances of the target type")))])}))}])}))}))}))})),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A two-level bidirectional encoder which adapts types to types and terms to terms")))])}))),
      (Core.Name "hydra.coders.AdapterContext", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An evaluation context together with a source language and a target language")))])}))),
      (Core.Name "hydra.coders.Bicoder", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A two-level encoder and decoder, operating both at a type level and an instance (data) level")))])}))),
      (Core.Name "hydra.coders.Coder", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "v1"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "v2"),
            Core.forallTypeBody = (Core.TypeRecord [
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "encode"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.context.Context")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v1")),
                      Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                        Core.eitherTypeLeft = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.context.InContext")),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.errors.Error"))})),
                        Core.eitherTypeRight = (Core.TypeVariable (Core.Name "v2"))}))}))})),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function which encodes source values as target values in a given context")))])}))},
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "decode"),
                Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.context.Context")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v2")),
                      Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                        Core.eitherTypeLeft = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.context.InContext")),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.errors.Error"))})),
                        Core.eitherTypeRight = (Core.TypeVariable (Core.Name "v1"))}))}))})),
                  Core.annotatedTypeAnnotation = (M.fromList [
                    (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function which decodes target values as source values in a given context")))])}))}])}))})),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An encoder and decoder; a bidirectional transformation between two types")))])}))),
      (Core.Name "hydra.coders.CoderDirection", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeUnion [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "encode"),
            Core.fieldTypeType = Core.TypeUnit},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "decode"),
            Core.fieldTypeType = Core.TypeUnit}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Indicates either the 'out' or the 'in' direction of a coder")))])}))),
      (Core.Name "hydra.coders.Language", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A named language together with language-specific constraints")))])}))),
      (Core.Name "hydra.coders.LanguageConstraints", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "eliminationVariants"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.variants.EliminationVariant"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "All supported elimination variants")))])}))},
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
            Core.fieldTypeName = (Core.Name "functionVariants"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.variants.FunctionVariant"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "All supported function variants")))])}))},
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
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A logical set of types, as a predicate which tests a type for inclusion")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A set of constraints on valid type and term expressions, characterizing a language")))])}))),
      (Core.Name "hydra.coders.LanguageName", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The unique name of a language")))])}))),
      (Core.Name "hydra.coders.SymmetricAdapter", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A bidirectional encoder which maps between the same type and term languages on either side")))])}))),
      (Core.Name "hydra.coders.TraversalOrder", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.coders.TypeAdapter", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function which maps a Hydra type to a symmetric adapter between types and terms")))])}))),
      (Core.Name "hydra.context.Context", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "trace"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A stack of context labels describing the current execution path")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "messages"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A log of warnings and/or info messages")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "other"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A map of string keys to arbitrary terms as values, for application-specific use")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An execution context for tracing and diagnostics, threaded through function calls")))])}))),
      (Core.Name "hydra.context.InContext", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "e"),
          Core.forallTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "object"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "e")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A domain object; typically an error")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "context"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.context.Context")),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The execution context at the point of capture")))])}))}])})),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A particular domain object (such as an error) together with an execution context")))])}))),
      (Core.Name "hydra.core.AnnotatedTerm", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.AnnotatedType", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.Application", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.ApplicationType", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.Binding", (Core.TypeAnnotated (Core.AnnotatedType {
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
            Core.fieldTypeName = (Core.Name "type"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.core.TypeScheme"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The optional type of the bound term")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A field with an optional type scheme, used to bind variables to terms in a 'let' expression")))])}))),
      (Core.Name "hydra.core.CaseStatement", (Core.TypeAnnotated (Core.AnnotatedType {
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
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An optional default case, used if none of the explicit cases match")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "cases"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Field"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A list of case alternatives, one per union field")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A union elimination; a case statement")))])}))),
      (Core.Name "hydra.core.EitherType", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type which provides a choice between a 'left' type and a 'right' type")))])}))),
      (Core.Name "hydra.core.Elimination", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeUnion [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "record"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Projection")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Eliminates a record by projecting a given field")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "union"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.CaseStatement")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Eliminates a union term by matching over the fields of the union. This is a case statement.")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "wrap"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Unwrap a wrapped term")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A corresponding elimination for an introduction term")))])}))),
      (Core.Name "hydra.core.Field", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.FieldType", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.FloatType", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeUnion [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "bigfloat"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = Core.TypeUnit,
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An arbitrary-precision floating-point type")))])}))},
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
      (Core.Name "hydra.core.FloatValue", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeUnion [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "bigfloat"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeBigfloat)),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An arbitrary-precision floating-point value")))])}))},
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
      (Core.Name "hydra.core.ForallType", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term.")))])}))),
      (Core.Name "hydra.core.Function", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeUnion [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "elimination"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Elimination")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An elimination for any of a few term variants")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "lambda"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Lambda")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function abstraction (lambda)")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function")))])}))),
      (Core.Name "hydra.core.FunctionType", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.Injection", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An instance of a union type; i.e. a string-indexed generalization of inl() or inr()")))])}))),
      (Core.Name "hydra.core.IntegerType", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.IntegerValue", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.Lambda", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.Let", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A set of (possibly recursive) 'let' bindings together with a body in which they are bound")))])}))),
      (Core.Name "hydra.core.Literal", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.LiteralType", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants")))])}))),
      (Core.Name "hydra.core.MapType", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.Name", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A unique identifier in some context; a string-valued key")))])}))),
      (Core.Name "hydra.core.PairType", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.Projection", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "typeName"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the record type")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "field"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the projected field")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A record elimination; a projection")))])}))),
      (Core.Name "hydra.core.Record", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A record, or labeled tuple; a map of field names to terms")))])}))),
      (Core.Name "hydra.core.Term", (Core.TypeAnnotated (Core.AnnotatedType {
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
            Core.fieldTypeName = (Core.Name "either"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An either value")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "function"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Function")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A function term")))])}))},
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
            Core.fieldTypeName = (Core.Name "union"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Injection")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An injection; an instance of a union type")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "unit"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = Core.TypeUnit,
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A unit value; a term with no value")))])}))},
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
      (Core.Name "hydra.core.Type", (Core.TypeAnnotated (Core.AnnotatedType {
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
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A wrapped type (newtype)")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A data type")))])}))),
      (Core.Name "hydra.core.TypeApplicationTerm", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.TypeLambda", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.core.TypeScheme", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "variables"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Name"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The free type variables")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "type"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type expression")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "constraints"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.TypeVariableMetadata"))}))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Optional metadata for type variables, including typeclass constraints. The map keys are type variable names.")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type expression together with free type variables occurring in the expression")))])}))),
      (Core.Name "hydra.core.TypeVariableMetadata", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "classes"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.core.Name"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The set of typeclass constraints on this type variable")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Metadata associated with a type variable, including typeclass constraints")))])}))),
      (Core.Name "hydra.core.WrappedTerm", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.errors.DecodingError", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An error that occurred during decoding of a term")))])}))),
      (Core.Name "hydra.errors.Error", (Core.TypeAnnotated (Core.AnnotatedType {
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
            Core.fieldTypeName = (Core.Name "other"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.errors.OtherError")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any other error")))])}))},
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An error of any kind, with kernel errors particularly differentiated")))])}))),
      (Core.Name "hydra.errors.OtherError", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any other error")))])}))),
      (Core.Name "hydra.errors.UnificationError", (Core.TypeAnnotated (Core.AnnotatedType {
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
      (Core.Name "hydra.graph.Graph", (Core.TypeAnnotated (Core.AnnotatedType {
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
                Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.TypeVariableMetadata"))})),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered.")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "lambdaVariables"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "hydra.core.Name"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The set of term variables introduced by specifically by lambdas")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "metadata"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any additional metadata bound to term variables in scope")))])}))},
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
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The set of type variables introduced specifically by type lambdas")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A graph, or lexical environment which binds names to terms, types, primitives, and metadata")))])}))),
      (Core.Name "hydra.graph.Primitive", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "name"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The unique name of the primitive function")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "type"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type signature of the primitive function")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "implementation"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.context.Context")),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                    Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.context.InContext")),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.errors.Error"))})),
                      Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))}))}))})),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A concrete implementation of the primitive function. The Context and Graph parameters are needed by higher-order primitives (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments via term reduction; the Graph provides variable and primitive bindings, while the Context supports tracing and error reporting.")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A built-in function or constant")))])}))),
      (Core.Name "hydra.graph.TermCoder", (Core.TypeAnnotated (Core.AnnotatedType {
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
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.context.Context")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                      Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                        Core.eitherTypeLeft = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.context.InContext")),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.errors.Error"))})),
                        Core.eitherTypeRight = (Core.TypeVariable (Core.Name "a"))}))}))}))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An encode function from terms to native values")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "decode"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.context.Context")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                    Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.context.InContext")),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.errors.Error"))})),
                      Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))}))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A decode function from native values to terms")))])}))}])})),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms.")))])}))),
      (Core.Name "hydra.packaging.Definition", (Core.TypeAnnotated (Core.AnnotatedType {
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
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type definition")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A definition, which may be either a term or type definition")))])}))),
      (Core.Name "hydra.packaging.FileExtension", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A file extension (without the dot), e.g. \"json\" or \"py\"")))])}))),
      (Core.Name "hydra.packaging.Library", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "namespace"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.Namespace")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A common prefix for all primitive function names in the library")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "prefix"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A preferred namespace prefix for function names in the library")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "primitives"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.graph.Primitive"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The primitives defined in this library")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A library of primitive functions")))])}))),
      (Core.Name "hydra.packaging.Module", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "namespace"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.Namespace")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A common prefix for all element names in the module")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "definitions"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.Definition"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The definitions in this module")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "termDependencies"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.Namespace"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any modules which the term expressions of this module directly depend upon")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "typeDependencies"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.Namespace"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Any modules which the type expressions of this module directly depend upon")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "description"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An optional human-readable description of the module")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A logical collection of elements in the same namespace, having dependencies on zero or more other modules")))])}))),
      (Core.Name "hydra.packaging.Namespace", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A prefix for element names")))])}))),
      (Core.Name "hydra.packaging.Namespaces", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "n"),
          Core.forallTypeBody = (Core.TypeRecord [
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "focus"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "hydra.packaging.Namespace")),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "n"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The namespace in focus, together with its associated value")))])}))},
            Core.FieldType {
              Core.fieldTypeName = (Core.Name "mapping"),
              Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.packaging.Namespace")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "n"))})),
                Core.annotatedTypeAnnotation = (M.fromList [
                  (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A mapping of namespaces to values")))])}))}])})),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A mapping from namespaces to values of type n, with a focus on one namespace")))])}))),
      (Core.Name "hydra.packaging.Package", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "name"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.packaging.PackageName")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the package")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "modules"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.Module"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The modules in this package")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "dependencies"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.packaging.PackageName"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The packages which this package depends on")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "description"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An optional human-readable description of the package")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A package, which is a named collection of modules with metadata and dependencies")))])}))),
      (Core.Name "hydra.packaging.PackageName", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeWrap (Core.TypeLiteral Core.LiteralTypeString)),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The unique name of a package, e.g. \"hydra-kernel\" or \"hydra-python\"")))])}))),
      (Core.Name "hydra.packaging.QualifiedName", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "namespace"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.packaging.Namespace"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The optional namespace")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "local"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The local name")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A qualified name consisting of an optional namespace together with a mandatory local name")))])}))),
      (Core.Name "hydra.packaging.TermDefinition", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "name"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the term")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "term"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The term being defined")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "type"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "hydra.core.TypeScheme"))),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type scheme of the term, including any class constraints")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A term-level definition, including a name, a term, and the type scheme of the term")))])}))),
      (Core.Name "hydra.packaging.TypeDefinition", (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = (Core.TypeRecord [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "name"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The name of the type")))])}))},
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "type"),
            Core.fieldTypeType = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme")),
              Core.annotatedTypeAnnotation = (M.fromList [
                (Core.Name "description", (Core.TermLiteral (Core.LiteralString "The type scheme being defined")))])}))}]),
        Core.annotatedTypeAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A type-level definition, including a name and the type scheme")))])}))),
      (Core.Name "hydra.util.CaseConvention", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "A naming convention for symbols, such as camelCase or snake_case")))])}))),
      (Core.Name "hydra.util.Comparison", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "An equality judgement: less than, equal to, or greater than")))])}))),
      (Core.Name "hydra.util.Precision", (Core.TypeAnnotated (Core.AnnotatedType {
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
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Numeric precision: arbitrary precision, or precision to a specified number of bits")))])})))]
