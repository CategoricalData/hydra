{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Typing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms                 as Terms
import           Hydra.Dsl.Types                 as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just ("Types supporting type inference and type reconstruction.")
  where
    ns = Namespace "hydra.typing"
    core = typeref $ moduleNamespace Core.module_
    typing = typeref ns
    def = datatype ns

    elements = [

      def "InferenceContext" $
        doc "The context provided to type inference, including various typing enviroments." $
        record [
          "schemaTypes">:
            doc "A fixed typing environment which is derived from the schema of the graph." $
            Types.map (core "Name") (core "TypeScheme"),
          "primitiveTypes">:
            doc "A fixed typing environment which is derived from the set of primitives in the graph." $
            Types.map (core "Name") (core "TypeScheme"),
          "dataTypes">:
            doc ("A mutable typing environment which is specific to the current graph being processed."
              ++ " This environment is (usually) smaller than the schema and primitive typing environments,"
              ++ " and is subject to global substitutions.") $
            Types.map (core "Name") (core "TypeScheme"),
          "debug">:
            doc "Whether to enable debug output during type inference" $
            boolean],

      def "InferenceResult" $
        doc "The result of applying inference rules to a term." $
        record [
          "term">:
            doc "The term which was inferred" $
            core "Term",
          "type">:
            doc "The inferred type of the term" $
            core "Type",
          "subst">:
            doc "The type substitution resulting from unification" $
            typing "TypeSubst"],

      def "TermSubst" $
        doc "A substitution of term variables for terms" $
        wrap $ Types.map (core "Name") (core "Term"),

      def "TypeConstraint" $
        doc "An assertion that two types can be unified into a single type" $
        record [
          "left">:
            doc "The left-hand side of the constraint" $
            core "Type",
          "right">:
            doc "The right-hand side of the constraint" $
            core "Type",
          "comment">:
            doc "A description of the type constraint which may be used for tracing or debugging" $
            string],

      def "TypeContext" $
        doc "A typing environment used for type reconstruction (typeOf) over System F terms" $
        record [
          "types">:
            doc "A mapping of lambda- and let-bound variables to their types" $
            Types.map (core "Name") (core "Type"),
          "variables">:
            doc "The set of type variables introduced by enclosing type lambdas" $
            Types.set (core "Name"),
          "inferenceContext">:
            doc "The schema types, primitive types, and data types of the graph" $
            typing "InferenceContext"],

      def "TypeSubst" $
        doc "A substitution of type variables for types" $
        wrap $ Types.map (core "Name") (core "Type")]
