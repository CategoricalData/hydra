-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.grammar

module Hydra.Dsl.Grammar where

import qualified Hydra.Core as Core
import qualified Hydra.Grammar as Grammar
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

constant :: Phantoms.TTerm String -> Phantoms.TTerm Grammar.Constant
constant x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Constant"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unConstant :: Phantoms.TTerm Grammar.Constant -> Phantoms.TTerm String
unConstant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.grammar.Constant")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

grammar :: Phantoms.TTerm [Grammar.Production] -> Phantoms.TTerm Grammar.Grammar
grammar x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Grammar"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unGrammar :: Phantoms.TTerm Grammar.Grammar -> Phantoms.TTerm [Grammar.Production]
unGrammar x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.grammar.Grammar")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

label :: Phantoms.TTerm String -> Phantoms.TTerm Grammar.Label
label x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Label"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unLabel :: Phantoms.TTerm Grammar.Label -> Phantoms.TTerm String
unLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.grammar.Label")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledPattern :: Phantoms.TTerm Grammar.Label -> Phantoms.TTerm Grammar.Pattern -> Phantoms.TTerm Grammar.LabeledPattern
labeledPattern label pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.grammar.LabeledPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

labeledPatternLabel :: Phantoms.TTerm Grammar.LabeledPattern -> Phantoms.TTerm Grammar.Label
labeledPatternLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.grammar.LabeledPattern"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledPatternPattern :: Phantoms.TTerm Grammar.LabeledPattern -> Phantoms.TTerm Grammar.Pattern
labeledPatternPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.grammar.LabeledPattern"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledPatternWithLabel :: Phantoms.TTerm Grammar.LabeledPattern -> Phantoms.TTerm Grammar.Label -> Phantoms.TTerm Grammar.LabeledPattern
labeledPatternWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.grammar.LabeledPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.grammar.LabeledPattern"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

labeledPatternWithPattern :: Phantoms.TTerm Grammar.LabeledPattern -> Phantoms.TTerm Grammar.Pattern -> Phantoms.TTerm Grammar.LabeledPattern
labeledPatternWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.grammar.LabeledPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.grammar.LabeledPattern"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

patternAlternatives :: Phantoms.TTerm [Grammar.Pattern] -> Phantoms.TTerm Grammar.Pattern
patternAlternatives x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alternatives"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternConstant :: Phantoms.TTerm Grammar.Constant -> Phantoms.TTerm Grammar.Pattern
patternConstant x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternIgnored :: Phantoms.TTerm Grammar.Pattern -> Phantoms.TTerm Grammar.Pattern
patternIgnored x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ignored"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternLabeled :: Phantoms.TTerm Grammar.LabeledPattern -> Phantoms.TTerm Grammar.Pattern
patternLabeled x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternNil :: Phantoms.TTerm Grammar.Pattern
patternNil =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nil"),
        Core.fieldTerm = Core.TermUnit}}))

patternNonterminal :: Phantoms.TTerm Grammar.Symbol -> Phantoms.TTerm Grammar.Pattern
patternNonterminal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonterminal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternOption :: Phantoms.TTerm Grammar.Pattern -> Phantoms.TTerm Grammar.Pattern
patternOption x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "option"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternPlus :: Phantoms.TTerm Grammar.Pattern -> Phantoms.TTerm Grammar.Pattern
patternPlus x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternRegex :: Phantoms.TTerm Grammar.Regex -> Phantoms.TTerm Grammar.Pattern
patternRegex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternSequence :: Phantoms.TTerm [Grammar.Pattern] -> Phantoms.TTerm Grammar.Pattern
patternSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternStar :: Phantoms.TTerm Grammar.Pattern -> Phantoms.TTerm Grammar.Pattern
patternStar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

production :: Phantoms.TTerm Grammar.Symbol -> Phantoms.TTerm Grammar.Pattern -> Phantoms.TTerm Grammar.Production
production symbol pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.grammar.Production"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Phantoms.unTTerm symbol)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

productionSymbol :: Phantoms.TTerm Grammar.Production -> Phantoms.TTerm Grammar.Symbol
productionSymbol x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.grammar.Production"),
        Core.projectionField = (Core.Name "symbol")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

productionPattern :: Phantoms.TTerm Grammar.Production -> Phantoms.TTerm Grammar.Pattern
productionPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.grammar.Production"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

productionWithSymbol :: Phantoms.TTerm Grammar.Production -> Phantoms.TTerm Grammar.Symbol -> Phantoms.TTerm Grammar.Production
productionWithSymbol original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.grammar.Production"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.grammar.Production"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

productionWithPattern :: Phantoms.TTerm Grammar.Production -> Phantoms.TTerm Grammar.Pattern -> Phantoms.TTerm Grammar.Production
productionWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.grammar.Production"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.grammar.Production"),
              Core.projectionField = (Core.Name "symbol")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

regex :: Phantoms.TTerm String -> Phantoms.TTerm Grammar.Regex
regex x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Regex"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRegex :: Phantoms.TTerm Grammar.Regex -> Phantoms.TTerm String
unRegex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.grammar.Regex")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

symbol :: Phantoms.TTerm String -> Phantoms.TTerm Grammar.Symbol
symbol x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Symbol"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSymbol :: Phantoms.TTerm Grammar.Symbol -> Phantoms.TTerm String
unSymbol x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.grammar.Symbol")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
