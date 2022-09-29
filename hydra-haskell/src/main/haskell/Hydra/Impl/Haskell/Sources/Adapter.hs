{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Adapter where

import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Compute

import Hydra.Core
import Hydra.Compute
import Hydra.Module
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraAdapterModule :: Module Meta
hydraAdapterModule = Module ns elements [hydraComputeModule] $
    Just "A model capturing basic abstractions for mappings between languages"
  where
    ns = Namespace "hydra/adapter"
    def = datatype ns
    core = nsref $ moduleNamespace hydraCoreModule
    adapter = nsref ns
    evaluation = nsref $ moduleNamespace hydraComputeModule

    elements = [
      def "Adapter" $
        lambda "s" $ lambda "t" $ lambda "v" $ record [
          "isLossy">: boolean,
          "source">: variable "t",
          "target">: variable "t",
          "coder">: evaluation "Coder" @@ "s" @@ "v" @@ "v"],

      def "AdapterContext" $
        lambda "m" $ record [
          "evaluation">: apply (evaluation "Context") (variable "m"),
          "source">: apply (adapter "Language") (variable "m"),
          "target">: apply (adapter "Language") (variable "m")],

      def "LanguageConstraints" $
        lambda "m" $ record [
          "eliminationVariants">: Types.set $ core "EliminationVariant",
          "literalVariants">: Types.set $ core "LiteralVariant",
          "floatTypes">: Types.set $ core "FloatType",
          "functionVariants">: Types.set $ core "FunctionVariant",
          "integerTypes">: Types.set $ core "IntegerType",
          "termVariants">: Types.set $ core "TermVariant",
          "typeVariants">: Types.set $ core "TypeVariant",
          "types">: core "Type" @@ "m" --> boolean],

      def "LanguageName" string,

      def "Language" $
        lambda "m" $ record [
          "name">: adapter "LanguageName",
          "constraints">: apply (adapter "LanguageConstraints") (variable "m")]]
