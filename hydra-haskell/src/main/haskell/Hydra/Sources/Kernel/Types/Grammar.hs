-- | A model for Hydra labeled-BNF grammars

module Hydra.Sources.Kernel.Types.Grammar where

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
module_ = Module ns elements [] [Core.module_] $
    Just "A common API for BNF-based grammars, specifying context-free languages"
  where
    ns = Namespace "hydra.grammar"
    grammar = typeref ns
    def = datatype ns

    elements = [

      def "Constant" $
        doc "A constant pattern" $
        wrap string,

      def "Grammar" $
        doc "An enhanced Backus-Naur form (BNF) grammar" $
        wrap $ list $ grammar "Production",

      def "Label" $
        doc "A name for a pattern" $
        wrap string,

      def "LabeledPattern" $
        doc "A pattern together with a name (label)" $
        record [
        "label">: grammar "Label",
        "pattern">: grammar "Pattern"],

      def "Pattern" $
        doc "A pattern which matches valid expressions in the language" $
        union [
          "alternatives">: list $ grammar "Pattern",
          "constant">: grammar "Constant",
          "ignored">: grammar "Pattern",
          "labeled">: grammar "LabeledPattern",
          "nil">: unit,
          "nonterminal">: grammar "Symbol",
          "option">: grammar "Pattern",
          "plus">: grammar "Pattern",
          "regex">: grammar "Regex",
          "sequence">: list $ grammar "Pattern",
          "star">: grammar "Pattern"],

      def "Production" $
        doc "A BNF production" $
        record [
          "symbol">: grammar "Symbol",
          "pattern">: grammar "Pattern"],

      def "Regex" $
        doc "A regular expression" $
        wrap string,

      def "Symbol" $
        doc "A nonterminal symbol" $
        wrap string]
