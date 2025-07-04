-- | A common API for BNF-based grammars, specifying context-free languages

module Hydra.Grammar where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A constant pattern
newtype Constant = 
  Constant {
    unConstant :: String}
  deriving (Eq, Ord, Read, Show)

_Constant = (Core.Name "hydra.grammar.Constant")

-- | An enhanced Backus-Naur form (BNF) grammar
newtype Grammar = 
  Grammar {
    unGrammar :: [Production]}
  deriving (Eq, Ord, Read, Show)

_Grammar = (Core.Name "hydra.grammar.Grammar")

-- | A name for a pattern
newtype Label = 
  Label {
    unLabel :: String}
  deriving (Eq, Ord, Read, Show)

_Label = (Core.Name "hydra.grammar.Label")

-- | A pattern together with a name (label)
data LabeledPattern = 
  LabeledPattern {
    labeledPatternLabel :: Label,
    labeledPatternPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_LabeledPattern = (Core.Name "hydra.grammar.LabeledPattern")

_LabeledPattern_label = (Core.Name "label")

_LabeledPattern_pattern = (Core.Name "pattern")

-- | A pattern which matches valid expressions in the language
data Pattern = 
  PatternAlternatives [Pattern] |
  PatternConstant Constant |
  PatternIgnored Pattern |
  PatternLabeled LabeledPattern |
  PatternNil  |
  PatternNonterminal Symbol |
  PatternOption Pattern |
  PatternPlus Pattern |
  PatternRegex Regex |
  PatternSequence [Pattern] |
  PatternStar Pattern
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra.grammar.Pattern")

_Pattern_alternatives = (Core.Name "alternatives")

_Pattern_constant = (Core.Name "constant")

_Pattern_ignored = (Core.Name "ignored")

_Pattern_labeled = (Core.Name "labeled")

_Pattern_nil = (Core.Name "nil")

_Pattern_nonterminal = (Core.Name "nonterminal")

_Pattern_option = (Core.Name "option")

_Pattern_plus = (Core.Name "plus")

_Pattern_regex = (Core.Name "regex")

_Pattern_sequence = (Core.Name "sequence")

_Pattern_star = (Core.Name "star")

-- | A BNF production
data Production = 
  Production {
    productionSymbol :: Symbol,
    productionPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_Production = (Core.Name "hydra.grammar.Production")

_Production_symbol = (Core.Name "symbol")

_Production_pattern = (Core.Name "pattern")

-- | A regular expression
newtype Regex = 
  Regex {
    unRegex :: String}
  deriving (Eq, Ord, Read, Show)

_Regex = (Core.Name "hydra.grammar.Regex")

-- | A nonterminal symbol
newtype Symbol = 
  Symbol {
    unSymbol :: String}
  deriving (Eq, Ord, Read, Show)

_Symbol = (Core.Name "hydra.grammar.Symbol")
