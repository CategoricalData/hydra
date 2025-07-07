module Hydra.Sources.Kernel.Types.Accessors where

-- Standard type-level Tier-1 imports
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Kernel.Types.Core
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y


hydraAccessorsModule :: Module
hydraAccessorsModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just "A model for term access patterns"
  where
    ns = Namespace "hydra.accessors"
    def = datatype ns
    accessors = typeref ns
    core = typeref $ moduleNamespace hydraCoreModule

    elements = [

      def "AccessorEdge" $
        record [
          "source">: accessors "AccessorNode",
          "path">: accessors "AccessorPath",
          "target">: accessors "AccessorNode"],

      def "AccessorGraph" $
        record [
          "nodes">: list $ accessors "AccessorNode",
          "edges">: list $ accessors "AccessorEdge"],

      def "AccessorNode" $
        record [
          "name">: core "Name",
          "label">: string,
          "id" >: string],

      def "AccessorPath" $
        wrap $ list $ accessors "TermAccessor",

      def "TermAccessor" $
        doc "A function which maps from a term to a particular immediate subterm" $
        union [
          "annotatedSubject">: unit,
          "applicationFunction">: unit,
          "applicationArgument">: unit,
          "lambdaBody">: unit,
          "unionCasesDefault">: unit,
          "unionCasesBranch">: core "Name",
          "letEnvironment">: unit,
          "letBinding">: core "Name",
          "listElement">: int32,
          "mapKey">: int32,
          "mapValue">: int32,
          "optionalTerm">: unit,
          "productTerm">: int32,
          "recordField">: core "Name",
          "setElement">: int32,
          "sumTerm">: unit,
          "typeAbstractionBody">: unit,
          "typeApplicationTerm">: unit,
          "injectionTerm">: unit,
          "wrappedTerm">: unit]]
