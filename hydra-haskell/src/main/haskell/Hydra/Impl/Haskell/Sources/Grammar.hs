-- | A model for Hydra labeled-BNF grammars

module Hydra.Impl.Haskell.Sources.Grammar where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraGrammarModule :: Module Meta
hydraGrammarModule = Module hydraGrammar [hydraCoreModule]

hydraGrammarName :: GraphName
hydraGrammarName = GraphName "hydra/grammar"

hydraGrammar :: Graph Meta
hydraGrammar = Graph hydraGrammarName elements (const True) hydraCoreName
  where
    grammar = nsref hydraGrammarName
    def = datatype hydraGrammarName
    
    elements = [

      def "Constant" string,
      
      def "Grammar" $ list $ grammar "Production",
      
      def "Label" string,
      
      def "LabeledPattern" $ record [
        field "label" $ grammar "Label",
        field "pattern" $ grammar "Pattern"],
        
      def "Pattern" $ union [
        field "nil" unit,
        field "ignored" $ grammar "Pattern",
        field "labeled" $ grammar "LabeledPattern",
        field "constant" $ grammar "Constant",
        field "regex" $ grammar "Regex",
        field "nonterminal" $ grammar "Symbol",
        field "sequence" $ list $ grammar "Pattern",
        field "alternatives" $ list $ grammar "Pattern",
        field "option" $ grammar "Pattern",
        field "star" $ grammar "Pattern",
        field "plus" $ grammar "Pattern"],
        
      def "Production" $ record [
        field "symbol" $ grammar "Symbol",
        field "pattern" $ grammar "Pattern"],
        
      def "Regex" string,
      
      def "Symbol" string]
