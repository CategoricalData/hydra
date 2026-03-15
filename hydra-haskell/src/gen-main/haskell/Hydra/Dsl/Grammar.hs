-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.grammar

module Hydra.Dsl.Grammar where

import qualified Hydra.Grammar as Grammar
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

constant :: (String -> Grammar.Constant)
constant x = (Grammar.Constant x)

unConstant :: (Grammar.Constant -> String)
unConstant = Grammar.unConstant

grammar :: ([Grammar.Production] -> Grammar.Grammar)
grammar x = (Grammar.Grammar x)

unGrammar :: (Grammar.Grammar -> [Grammar.Production])
unGrammar = Grammar.unGrammar

label :: (String -> Grammar.Label)
label x = (Grammar.Label x)

unLabel :: (Grammar.Label -> String)
unLabel = Grammar.unLabel

labeledPattern :: (Grammar.Label -> Grammar.Pattern -> Grammar.LabeledPattern)
labeledPattern label pattern = Grammar.LabeledPattern {
  Grammar.labeledPatternLabel = label,
  Grammar.labeledPatternPattern = pattern}

labeledPatternLabel :: (Grammar.LabeledPattern -> Grammar.Label)
labeledPatternLabel = Grammar.labeledPatternLabel

labeledPatternPattern :: (Grammar.LabeledPattern -> Grammar.Pattern)
labeledPatternPattern = Grammar.labeledPatternPattern

labeledPatternWithLabel :: (Grammar.LabeledPattern -> Grammar.Label -> Grammar.LabeledPattern)
labeledPatternWithLabel original newVal = Grammar.LabeledPattern {
  Grammar.labeledPatternLabel = newVal,
  Grammar.labeledPatternPattern = (Grammar.labeledPatternPattern original)}

labeledPatternWithPattern :: (Grammar.LabeledPattern -> Grammar.Pattern -> Grammar.LabeledPattern)
labeledPatternWithPattern original newVal = Grammar.LabeledPattern {
  Grammar.labeledPatternLabel = (Grammar.labeledPatternLabel original),
  Grammar.labeledPatternPattern = newVal}

patternAlternatives :: ([Grammar.Pattern] -> Grammar.Pattern)
patternAlternatives x = (Grammar.PatternAlternatives x)

patternConstant :: (Grammar.Constant -> Grammar.Pattern)
patternConstant x = (Grammar.PatternConstant x)

patternIgnored :: (Grammar.Pattern -> Grammar.Pattern)
patternIgnored x = (Grammar.PatternIgnored x)

patternLabeled :: (Grammar.LabeledPattern -> Grammar.Pattern)
patternLabeled x = (Grammar.PatternLabeled x)

patternNil :: Grammar.Pattern
patternNil = Grammar.PatternNil

patternNonterminal :: (Grammar.Symbol -> Grammar.Pattern)
patternNonterminal x = (Grammar.PatternNonterminal x)

patternOption :: (Grammar.Pattern -> Grammar.Pattern)
patternOption x = (Grammar.PatternOption x)

patternPlus :: (Grammar.Pattern -> Grammar.Pattern)
patternPlus x = (Grammar.PatternPlus x)

patternRegex :: (Grammar.Regex -> Grammar.Pattern)
patternRegex x = (Grammar.PatternRegex x)

patternSequence :: ([Grammar.Pattern] -> Grammar.Pattern)
patternSequence x = (Grammar.PatternSequence x)

patternStar :: (Grammar.Pattern -> Grammar.Pattern)
patternStar x = (Grammar.PatternStar x)

production :: (Grammar.Symbol -> Grammar.Pattern -> Grammar.Production)
production symbol pattern = Grammar.Production {
  Grammar.productionSymbol = symbol,
  Grammar.productionPattern = pattern}

productionSymbol :: (Grammar.Production -> Grammar.Symbol)
productionSymbol = Grammar.productionSymbol

productionPattern :: (Grammar.Production -> Grammar.Pattern)
productionPattern = Grammar.productionPattern

productionWithSymbol :: (Grammar.Production -> Grammar.Symbol -> Grammar.Production)
productionWithSymbol original newVal = Grammar.Production {
  Grammar.productionSymbol = newVal,
  Grammar.productionPattern = (Grammar.productionPattern original)}

productionWithPattern :: (Grammar.Production -> Grammar.Pattern -> Grammar.Production)
productionWithPattern original newVal = Grammar.Production {
  Grammar.productionSymbol = (Grammar.productionSymbol original),
  Grammar.productionPattern = newVal}

regex :: (String -> Grammar.Regex)
regex x = (Grammar.Regex x)

unRegex :: (Grammar.Regex -> String)
unRegex = Grammar.unRegex

symbol :: (String -> Grammar.Symbol)
symbol x = (Grammar.Symbol x)

unSymbol :: (Grammar.Symbol -> String)
unSymbol = Grammar.unSymbol
