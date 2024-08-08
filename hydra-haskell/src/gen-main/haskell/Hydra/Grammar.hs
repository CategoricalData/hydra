-- | A common API for BNF-based grammars, specifying context-free languages

module Hydra.Grammar where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A constant pattern
newtype Constant = 
  Constant {
    unConstant :: String}
  deriving (Eq, Ord, Read, Show)

_Constant = (Core.Name "hydra/grammar.Constant")

_Constant_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | An enhanced Backus-Naur form (BNF) grammar
newtype Grammar = 
  Grammar {
    unGrammar :: [Production]}
  deriving (Eq, Ord, Read, Show)

_Grammar = (Core.Name "hydra/grammar.Grammar")

_Grammar_type_ = (Core.TypeList _Production_type_)

-- | A name for a pattern
newtype Label = 
  Label {
    unLabel :: String}
  deriving (Eq, Ord, Read, Show)

_Label = (Core.Name "hydra/grammar.Label")

_Label_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | A pattern together with a name (label)
data LabeledPattern = 
  LabeledPattern {
    labeledPatternLabel :: Label,
    labeledPatternPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_LabeledPattern = (Core.Name "hydra/grammar.LabeledPattern")

_LabeledPattern_label = (Core.Name "label")

_LabeledPattern_pattern = (Core.Name "pattern")

_LabeledPattern_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/grammar.LabeledPattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "label"),
      Core.fieldTypeType = _Label_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _Pattern_type_}]}))

-- | A pattern which matches valid expressions in the language
data Pattern = 
  PatternNil  |
  PatternIgnored Pattern |
  PatternLabeled LabeledPattern |
  PatternConstant Constant |
  PatternRegex Regex |
  PatternNonterminal Symbol |
  PatternSequence [Pattern] |
  PatternAlternatives [Pattern] |
  PatternOption Pattern |
  PatternStar Pattern |
  PatternPlus Pattern
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/grammar.Pattern")

_Pattern_nil = (Core.Name "nil")

_Pattern_ignored = (Core.Name "ignored")

_Pattern_labeled = (Core.Name "labeled")

_Pattern_constant = (Core.Name "constant")

_Pattern_regex = (Core.Name "regex")

_Pattern_nonterminal = (Core.Name "nonterminal")

_Pattern_sequence = (Core.Name "sequence")

_Pattern_alternatives = (Core.Name "alternatives")

_Pattern_option = (Core.Name "option")

_Pattern_star = (Core.Name "star")

_Pattern_plus = (Core.Name "plus")

_Pattern_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/grammar.Pattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nil"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ignored"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "labeled"),
      Core.fieldTypeType = _LabeledPattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constant"),
      Core.fieldTypeType = _Constant_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "regex"),
      Core.fieldTypeType = _Regex_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nonterminal"),
      Core.fieldTypeType = _Symbol_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = (Core.TypeList _Pattern_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "alternatives"),
      Core.fieldTypeType = (Core.TypeList _Pattern_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "option"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "star"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "plus"),
      Core.fieldTypeType = _Pattern_type_}]}))

-- | A BNF production
data Production = 
  Production {
    productionSymbol :: Symbol,
    productionPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_Production = (Core.Name "hydra/grammar.Production")

_Production_symbol = (Core.Name "symbol")

_Production_pattern = (Core.Name "pattern")

_Production_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/grammar.Production"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "symbol"),
      Core.fieldTypeType = _Symbol_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _Pattern_type_}]}))

-- | A regular expression
newtype Regex = 
  Regex {
    unRegex :: String}
  deriving (Eq, Ord, Read, Show)

_Regex = (Core.Name "hydra/grammar.Regex")

_Regex_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | A nonterminal symbol
newtype Symbol = 
  Symbol {
    unSymbol :: String}
  deriving (Eq, Ord, Read, Show)

_Symbol = (Core.Name "hydra/grammar.Symbol")

_Symbol_type_ = (Core.TypeLiteral Core.LiteralTypeString)