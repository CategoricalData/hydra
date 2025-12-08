{-# LANGUAGE OverloadedStrings #-}

-- | A model for Hydra labeled-BNF grammars

module Hydra.Sources.Kernel.Types.Grammar where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.grammar"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just "A common API for BNF-based grammars, specifying context-free languages"
  where
    elements = [
      constant,
      grammar,
      label,
      labeledPattern,
      pattern,
      production,
      regex,
      symbol]

constant :: Binding
constant = define "Constant" $
  doc "A constant pattern" $
  T.wrap T.string

grammar :: Binding
grammar = define "Grammar" $
  doc "An enhanced Backus-Naur form (BNF) grammar" $
  T.wrap $ T.list $ use production

label :: Binding
label = define "Label" $
  doc "A name for a pattern" $
  T.wrap T.string

labeledPattern :: Binding
labeledPattern = define "LabeledPattern" $
  doc "A pattern together with a name (label)" $
  T.record [
    "label">:
      doc "The label for the pattern" $
      use label,
    "pattern">:
      doc "The pattern being labeled" $
      use pattern]

pattern :: Binding
pattern = define "Pattern" $
  doc "A pattern which matches valid expressions in the language" $
  T.union [
    "alternatives">:
      doc "A choice between alternative patterns" $
      T.list $ use pattern,
    "constant">:
      doc "A constant (terminal) pattern" $
      use constant,
    "ignored">:
      doc "A pattern to be ignored (not captured)" $
      use pattern,
    "labeled">:
      doc "A labeled pattern" $
      use labeledPattern,
    "nil">:
      doc "An empty pattern" $
      T.unit,
    "nonterminal">:
      doc "A nonterminal symbol reference" $
      use symbol,
    "option">:
      doc "An optional pattern (zero or one occurrence)" $
      use pattern,
    "plus">:
      doc "One or more occurrences of a pattern" $
      use pattern,
    "regex">:
      doc "A regular expression pattern" $
      use regex,
    "sequence">:
      doc "A sequence of patterns" $
      T.list $ use pattern,
    "star">:
      doc "Zero or more occurrences of a pattern" $
      use pattern]

production :: Binding
production = define "Production" $
  doc "A BNF production" $
  T.record [
    "symbol">:
      doc "The nonterminal symbol being defined" $
      use symbol,
    "pattern">:
      doc "The pattern which defines the symbol" $
      use pattern]

regex :: Binding
regex = define "Regex" $
  doc "A regular expression" $
  T.wrap T.string

symbol :: Binding
symbol = define "Symbol" $
  doc "A nonterminal symbol" $
  T.wrap T.string
