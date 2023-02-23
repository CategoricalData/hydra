-- | A model for Hydra labeled-BNF grammars

module Hydra.Sources.Grammar where

import Hydra.Kernel
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Standard
import Hydra.Sources.Core


hydraGrammarModule :: Module Kv
hydraGrammarModule = Module ns elements [] $
    Just "A common API for BNF-based grammars, specifying context-free languages"
  where
    ns = Namespace "hydra/grammar"
    grammar = nsref ns
    def = datatype ns

    elements = [

      def "Constant" $
        doc "A constant pattern"
        string,

      def "Grammar" $
        doc "An enhanced Backus-Naur form (BNF) grammar" $
        list $ grammar "Production",

      def "Label" $
        doc "A name for a pattern"
        string,

      def "LabeledPattern" $
        doc "A pattern together with a name (label)" $
        record [
        "label">: grammar "Label",
        "pattern">: grammar "Pattern"],

      def "Pattern" $
        doc "A pattern which matches valid expressions in the language" $
        union [
          "nil">: unit,
          "ignored">: grammar "Pattern",
          "labeled">: grammar "LabeledPattern",
          "constant">: grammar "Constant",
          "regex">: grammar "Regex",
          "nonterminal">: grammar "Symbol",
          "sequence">: list $ grammar "Pattern",
          "alternatives">: list $ grammar "Pattern",
          "option">: grammar "Pattern",
          "star">: grammar "Pattern",
          "plus">: grammar "Pattern"],

      def "Production" $
        doc "A BNF production" $
        record [
          "symbol">: grammar "Symbol",
          "pattern">: grammar "Pattern"],

      def "Regex" $
        doc "A regular expression"
        string,

      def "Symbol" $
        doc "A nonterminal symbol"
        string]
