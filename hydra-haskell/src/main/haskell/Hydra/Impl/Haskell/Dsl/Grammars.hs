module Hydra.Impl.Haskell.Dsl.Grammars where

import Hydra.Grammar


infixr 0 -:-
(-:-) :: String -> Pattern -> Pattern
l -:- p = PatternLabeled $ LabeledPattern (Label l) p

alts :: [Pattern] -> Pattern
alts = PatternAlternatives

define :: String -> [Pattern] -> Production
define s pats = Production (Symbol s) pat
  where
    pat = case pats of
      [p] -> p
      _ -> alts pats

list :: [Pattern] -> Pattern
list = PatternSequence

nil :: Pattern
nil = PatternNil

opt :: Pattern -> Pattern
opt = PatternOption

plus :: Pattern -> Pattern
plus = PatternPlus

star :: Pattern -> Pattern
star = PatternStar

symbol :: String -> Pattern
symbol = PatternNonterminal . Symbol

terminal :: String -> Pattern
terminal = PatternConstant . Constant
