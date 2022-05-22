module Hydra.Impl.Haskell.Sources.Adapter where

import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Evaluation
import Hydra.Impl.Haskell.Sources.Graph

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraAdapterModule :: Module Meta
hydraAdapterModule = Module hydraAdapter [hydraCoreModule, hydraEvaluationModule, hydraGraphModule]

hydraAdapterName :: GraphName
hydraAdapterName = GraphName "hydra/adapter"

hydraAdapter :: Graph Meta
hydraAdapter = Graph hydraAdapterName elements (const True) hydraCoreName
  where
    def = datatype hydraAdapterName
    core = nominal . qualify hydraCoreName . Name
    adapter = nominal . qualify hydraAdapterName . Name
    evaluation = nominal . qualify hydraEvaluationName . Name

    elements = [
      
      {-
    
data Adapter t v
  = Adapter
    { adapterIsLossy :: Bool
    , adapterSource :: t
    , adapterTarget :: t
    , adapterStep :: Step v v }

data AdapterContext a
  = AdapterContext
    { adapterContextEvaluation :: Context a
    , adapterContextSource :: Language a
    , adapterContextTarget :: Language a }

data LanguageConstraints m
  = LanguageConstraints
    { languageConstraintsLiteralVariants :: (Set (LiteralVariant))
    , languageConstraintsFloatTypes :: (Set (FloatType))
    , languageConstraintsFunctionVariants :: (Set (FunctionVariant))
    , languageConstraintsIntegerTypes :: (Set (IntegerType))
    , languageConstraintsDataVariants :: (Set (DataVariant))
    , languageConstraintsTypeVariants :: (Set (TypeVariant))
    , languageConstraintsTypes :: Type m -> Bool }

type Language_Name = String

data Language m
  = Language
    { languageName :: Language_Name
    , languageConstraints :: LanguageConstraints m }


      -}
      
      def "Adapter" $
        universal "t" $ universal "v" $ record [
          field "isLossy" boolean,
          field "source" $ variable "t",
          field "target" $ variable "t",
          field "step" $ variable ("(Evaluation.Step v v)")], -- TODO: hack

      def "AdapterContext" $
        universal "a" $ record [
          field "evaluation" $ universal "a" $ evaluation "Context",
          field "source" $ universal "a" $ adapter "Language",
          field "target" $ universal "a" $ adapter "Language"],
          
      def "LanguageConstraints" $
        universal "m" $ record [
          field "literalVariants" $ Types.set $ core "LiteralVariant",
          field "floatTypes" $ Types.set $ core "FloatType",
          field "functionVariants" $ Types.set $ core "FunctionVariant",
          field "integerTypes" $ Types.set $ core "IntegerType",
          field "dataVariants" $ Types.set $ core "DataVariant",
          field "typeVariants" $ Types.set $ core "TypeVariant",
          field "types" $ function (universal "m" $ core "Type") boolean],
          
      def "LanguageName" string,
      
      def "Language" $
        universal "m" $ record [
          field "name" $ adapter "LanguageName",
          field "constraints" $ universal "m" $ adapter "LanguageConstraints"]]
