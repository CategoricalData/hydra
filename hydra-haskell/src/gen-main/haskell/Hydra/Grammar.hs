-- Note: this is an automatically generated file. Do not edit.

-- | A common API for BNF-based grammars, specifying context-free languages

module Hydra.Grammar where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
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
    -- | The label for the pattern
    labeledPatternLabel :: Label,
    -- | The pattern being labeled
    labeledPatternPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_LabeledPattern = (Core.Name "hydra.grammar.LabeledPattern")

_LabeledPattern_label = (Core.Name "label")

_LabeledPattern_pattern = (Core.Name "pattern")

-- | A pattern which matches valid expressions in the language
data Pattern = 
  -- | A choice between alternative patterns
  PatternAlternatives [Pattern] |
  -- | A constant (terminal) pattern
  PatternConstant Constant |
  -- | A pattern to be ignored (not captured)
  PatternIgnored Pattern |
  -- | A labeled pattern
  PatternLabeled LabeledPattern |
  -- | An empty pattern
  PatternNil  |
  -- | A nonterminal symbol reference
  PatternNonterminal Symbol |
  -- | An optional pattern (zero or one occurrence)
  PatternOption Pattern |
  -- | One or more occurrences of a pattern
  PatternPlus Pattern |
  -- | A regular expression pattern
  PatternRegex Regex |
  -- | A sequence of patterns
  PatternSequence [Pattern] |
  -- | Zero or more occurrences of a pattern
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
    -- | The nonterminal symbol being defined
    productionSymbol :: Symbol,
    -- | The pattern which defines the symbol
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
