module Hydra.Dsl.Meta.Grammar where

import Hydra.Kernel hiding (Pattern)
import Hydra.Dsl.Meta.Phantoms
import Hydra.Grammar

import qualified Data.Map as M
import qualified Data.Maybe as Y


labeledPatternLabel :: TTerm LabeledPattern -> TTerm Label
labeledPatternLabel lp = project _LabeledPattern _LabeledPattern_label @@ lp

labeledPatternPattern :: TTerm LabeledPattern -> TTerm Pattern
labeledPatternPattern lp = project _LabeledPattern _LabeledPattern_pattern @@ lp

production :: TTerm Symbol -> TTerm Pattern -> TTerm Production
production sym pat = record _Production [
  _Production_symbol>>: sym,
  _Production_pattern>>: pat]

productionSymbol :: TTerm Production -> TTerm Symbol
productionSymbol p = project _Production _Production_symbol @@ p

productionPattern :: TTerm Production -> TTerm Pattern
productionPattern p = project _Production _Production_pattern @@ p

unConstant :: TTerm Constant -> TTerm String
unConstant c = unwrap _Constant @@ c

unGrammar :: TTerm Grammar -> TTerm [Production]
unGrammar g = unwrap _Grammar @@ g

unLabel :: TTerm Label -> TTerm String
unLabel l = unwrap _Label @@ l

unSymbol :: TTerm Symbol -> TTerm String
unSymbol s = unwrap _Symbol @@ s
