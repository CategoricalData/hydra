-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.grammar

module Hydra.Encode.Grammar where

import qualified Hydra.Core as Core
import qualified Hydra.Grammar as Grammar
import qualified Hydra.Lib.Lists as Lists
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

constant :: (Grammar.Constant -> Core.Term)
constant x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Constant"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Grammar.unConstant x))}))

grammar :: (Grammar.Grammar -> Core.Term)
grammar x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Grammar"),
  Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map production xs)) (Grammar.unGrammar x))}))

label :: (Grammar.Label -> Core.Term)
label x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Label"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Grammar.unLabel x))}))

labeledPattern :: (Grammar.LabeledPattern -> Core.Term)
labeledPattern x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.grammar.LabeledPattern"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "label"),
      Core.fieldTerm = (label (Grammar.labeledPatternLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "pattern"),
      Core.fieldTerm = (pattern (Grammar.labeledPatternPattern x))}]}))

pattern :: (Grammar.Pattern -> Core.Term)
pattern x = case x of
  Grammar.PatternAlternatives v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "alternatives"),
      Core.fieldTerm = (Core.TermList (Lists.map pattern v1))}}))
  Grammar.PatternConstant v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "constant"),
      Core.fieldTerm = (constant v1)}}))
  Grammar.PatternIgnored v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "ignored"),
      Core.fieldTerm = (pattern v1)}}))
  Grammar.PatternLabeled v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "labeled"),
      Core.fieldTerm = (labeledPattern v1)}}))
  Grammar.PatternNil -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "nil"),
      Core.fieldTerm = Core.TermUnit}}))
  Grammar.PatternNonterminal v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "nonterminal"),
      Core.fieldTerm = (symbol v1)}}))
  Grammar.PatternOption v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "option"),
      Core.fieldTerm = (pattern v1)}}))
  Grammar.PatternPlus v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "plus"),
      Core.fieldTerm = (pattern v1)}}))
  Grammar.PatternRegex v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "regex"),
      Core.fieldTerm = (regex v1)}}))
  Grammar.PatternSequence v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "sequence"),
      Core.fieldTerm = (Core.TermList (Lists.map pattern v1))}}))
  Grammar.PatternStar v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.grammar.Pattern"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "star"),
      Core.fieldTerm = (pattern v1)}}))

production :: (Grammar.Production -> Core.Term)
production x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.grammar.Production"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "symbol"),
      Core.fieldTerm = (symbol (Grammar.productionSymbol x))},
    Core.Field {
      Core.fieldName = (Core.Name "pattern"),
      Core.fieldTerm = (pattern (Grammar.productionPattern x))}]}))

regex :: (Grammar.Regex -> Core.Term)
regex x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Regex"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Grammar.unRegex x))}))

symbol :: (Grammar.Symbol -> Core.Term)
symbol x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.grammar.Symbol"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Grammar.unSymbol x))}))
