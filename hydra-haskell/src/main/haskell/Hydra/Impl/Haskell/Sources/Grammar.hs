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
        "label">: grammar "Label",
        "pattern">: grammar "Pattern"],
        
      def "Pattern" $ union [
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
        
      def "Production" $ record [
        "symbol">: grammar "Symbol",
        "pattern">: grammar "Pattern"],
        
      def "Regex" string,
      
      def "Symbol" string]
