-- | A common API for BNF-based grammars

module Hydra.Grammar where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

newtype Constant 
  = Constant {
    unConstant :: String}
  deriving (Eq, Ord, Read, Show)

_Constant = (Core.Name "hydra/grammar.Constant")

newtype Grammar 
  = Grammar {
    unGrammar :: [Production]}
  deriving (Eq, Ord, Read, Show)

_Grammar = (Core.Name "hydra/grammar.Grammar")

newtype Label 
  = Label {
    unLabel :: String}
  deriving (Eq, Ord, Read, Show)

_Label = (Core.Name "hydra/grammar.Label")

data LabeledPattern 
  = LabeledPattern {
    labeledPatternLabel :: Label,
    labeledPatternPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_LabeledPattern = (Core.Name "hydra/grammar.LabeledPattern")

_LabeledPattern_label = (Core.FieldName "label")

_LabeledPattern_pattern = (Core.FieldName "pattern")

data Pattern 
  = PatternNil 
  | PatternIgnored Pattern
  | PatternLabeled LabeledPattern
  | PatternConstant Constant
  | PatternRegex Regex
  | PatternNonterminal Symbol
  | PatternSequence [Pattern]
  | PatternAlternatives [Pattern]
  | PatternOption Pattern
  | PatternStar Pattern
  | PatternPlus Pattern
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/grammar.Pattern")

_Pattern_nil = (Core.FieldName "nil")

_Pattern_ignored = (Core.FieldName "ignored")

_Pattern_labeled = (Core.FieldName "labeled")

_Pattern_constant = (Core.FieldName "constant")

_Pattern_regex = (Core.FieldName "regex")

_Pattern_nonterminal = (Core.FieldName "nonterminal")

_Pattern_sequence = (Core.FieldName "sequence")

_Pattern_alternatives = (Core.FieldName "alternatives")

_Pattern_option = (Core.FieldName "option")

_Pattern_star = (Core.FieldName "star")

_Pattern_plus = (Core.FieldName "plus")

data Production 
  = Production {
    productionSymbol :: Symbol,
    productionPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_Production = (Core.Name "hydra/grammar.Production")

_Production_symbol = (Core.FieldName "symbol")

_Production_pattern = (Core.FieldName "pattern")

newtype Regex 
  = Regex {
    unRegex :: String}
  deriving (Eq, Ord, Read, Show)

_Regex = (Core.Name "hydra/grammar.Regex")

newtype Symbol 
  = Symbol {
    unSymbol :: String}
  deriving (Eq, Ord, Read, Show)

_Symbol = (Core.Name "hydra/grammar.Symbol")