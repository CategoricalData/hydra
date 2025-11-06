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
        "label">:
          doc "The label for the pattern" $
          grammar "Label",
        "pattern">:
          doc "The pattern being labeled" $
          grammar "Pattern"],

      def "Pattern" $
        doc "A pattern which matches valid expressions in the language" $
        union [
          "alternatives">:
            doc "A choice between alternative patterns" $
            list $ grammar "Pattern",
          "constant">:
            doc "A constant (terminal) pattern" $
            grammar "Constant",
          "ignored">:
            doc "A pattern to be ignored (not captured)" $
            grammar "Pattern",
          "labeled">:
            doc "A labeled pattern" $
            grammar "LabeledPattern",
          "nil">:
            doc "An empty pattern" $
            unit,
          "nonterminal">:
            doc "A nonterminal symbol reference" $
            grammar "Symbol",
          "option">:
            doc "An optional pattern (zero or one occurrence)" $
            grammar "Pattern",
          "plus">:
            doc "One or more occurrences of a pattern" $
            grammar "Pattern",
          "regex">:
            doc "A regular expression pattern" $
            grammar "Regex",
          "sequence">:
            doc "A sequence of patterns" $
            list $ grammar "Pattern",
          "star">:
            doc "Zero or more occurrences of a pattern" $
            grammar "Pattern"],

      def "Production" $
        doc "A BNF production" $
        record [
          "symbol">:
            doc "The nonterminal symbol being defined" $
            grammar "Symbol",
          "pattern">:
            doc "The pattern which defines the symbol" $
            grammar "Pattern"],

      def "Regex" $
        doc "A regular expression" $
        wrap string,

      def "Symbol" $
        doc "A nonterminal symbol" $
        wrap string]
