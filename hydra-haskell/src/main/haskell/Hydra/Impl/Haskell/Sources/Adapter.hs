module Hydra.Impl.Haskell.Sources.Adapter where

import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Evaluation

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraAdapterModule :: Module Meta
hydraAdapterModule = Module hydraAdapter [hydraEvaluationModule]

hydraAdapterName :: GraphName
hydraAdapterName = GraphName "hydra/adapter"

hydraAdapter :: Graph Meta
hydraAdapter = Graph hydraAdapterName elements (const True) hydraCoreName
  where
    def = datatype hydraAdapterName
    core = nsref hydraCoreName
    adapter = nsref hydraAdapterName
    evaluation = nsref hydraEvaluationName

    elements = [
      def "Adapter" $
        lambda "t" $ lambda "v" $ record [
          field "isLossy" boolean,
          field "source" $ variable "t",
          field "target" $ variable "t",
          field "coder" $ apply (apply (evaluation "Coder") (variable "v")) (variable "v")],

      def "AdapterContext" $
        lambda "m" $ record [
          field "evaluation" $ apply (evaluation "Context") (variable "m"),
          field "source" $ apply (adapter "Language") (variable "m"),
          field "target" $ apply (adapter "Language") (variable "m")],

      def "LanguageConstraints" $
        lambda "m" $ record [
          field "eliminationVariants" $ Types.set $ core "EliminationVariant",
          field "literalVariants" $ Types.set $ core "LiteralVariant",
          field "floatTypes" $ Types.set $ core "FloatType",
          field "functionVariants" $ Types.set $ core "FunctionVariant",
          field "integerTypes" $ Types.set $ core "IntegerType",
          field "termVariants" $ Types.set $ core "TermVariant",
          field "typeVariants" $ Types.set $ core "TypeVariant",
          field "types" $ function (apply (core "Type") (variable "m")) boolean],

      def "LanguageName" string,

      def "Language" $
        lambda "m" $ record [
          field "name" $ adapter "LanguageName",
          field "constraints" $ apply (adapter "LanguageConstraints") (variable "m")]]
