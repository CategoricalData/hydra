{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier1.Typing where

-- Standard type-level Tier-1 imports
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Tier0.Core
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y


hydraTypingModule :: Module
hydraTypingModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just ("Types supporting type inference.")
  where
    ns = Namespace "hydra.typing"
    core = typeref $ moduleNamespace hydraCoreModule
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
          "debug">: boolean],

      def "InferenceResult" $
        doc "The result of applying inference rules to a term." $
        record [
          "term">: core "Term",
          "type">: core "Type",
          "subst">: typing "TypeSubst"],

      def "TermSubst" $
        doc "A substitution of term variables for terms" $
        wrap $ Types.map (core "Name") (core "Term"),

      def "TypeConstraint" $
        doc "An assertion that two types can be unified into a single type" $
        record [
          "left">: core "Type",
          "right">: core "Type",
          "comment">:
            doc "A description of the type constraint which may be used for tracing or debugging"
            string],

      def "TypeSubst" $
        doc "A substitution of type variables for types" $
        wrap $ Types.map (core "Name") (core "Type")]
