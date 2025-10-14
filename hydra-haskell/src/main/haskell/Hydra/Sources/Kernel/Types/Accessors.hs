module Hydra.Sources.Kernel.Types.Accessors where

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
    Just "A model for term access patterns"
  where
    ns = Namespace "hydra.accessors"
    def = datatype ns
    accessors = typeref ns
    core = typeref $ moduleNamespace Core.module_

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
          "letBody">: unit,
          "letBinding">: core "Name",
          "listElement">: int32,
          "mapKey">: int32,
          "mapValue">: int32,
          "optionalTerm">: unit,
          "productTerm">: int32,
          "recordField">: core "Name",
          "setElement">: int32,
          "sumTerm">: unit,
          "typeLambdaBody">: unit,
          "typeApplicationTerm">: unit,
          "injectionTerm">: unit,
          "wrappedTerm">: unit]]
