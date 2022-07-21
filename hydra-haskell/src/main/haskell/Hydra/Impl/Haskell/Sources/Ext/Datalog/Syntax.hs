module Hydra.Impl.Haskell.Sources.Ext.Datalog.Syntax where

import Hydra.Core
import Hydra.Graph
import Hydra.Grammar
import Hydra.Impl.Haskell.Dsl.Grammars
import Hydra.Util.GrammarToModule
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard

comma = terminal ","
entail = terminal ":-"
lparen = terminal "("
period = terminal "."
rparen = terminal ")"

datalogSyntaxName :: GraphName
datalogSyntaxName = GraphName "hydra/ext/datalog/syntax"

datalogSyntaxModule :: Module Meta
datalogSyntaxModule = grammarToModule Standard.standardContext datalogGrammar datalogSyntaxName

datalogGrammar :: Grammar
datalogGrammar = Grammar [
  define "Constant" [
    regex "\\\".*\\\""],

  define "Relation" [
    regex "[a-z][a-zA-Z0-9]*"],
    
  define "Variable" [
    regex "[A-Z][a-zA-Z0-9]*"],

-- <program> ::= <fact> <program> | <rule> <program> | ɛ
  define "Program" [
    star (alts[
      symbol "Fact",
      symbol "Rule"])],
--    list[symbol "Fact", symbol "Program"],
--    list[symbol "Rule", symbol "Program"],
--    nil],

-- <fact> ::=  <relation> "(" <constant-list> ")." 
  define "Fact" [
    list[symbol "Relation", lparen, symbol "ConstantList", rparen, period]],

-- <rule> ::= <atom> ":-" <atom-list> "."
  define "Rule" [
    list[symbol "Atom", entail, symbol "AtomList", period]],

-- <atom> ::= <relation> "(" <term-list> ")"
  define "Atom" [
    list[symbol "Relation", lparen, symbol "TermList", rparen]],

-- <atom-list> ::= <atom> | <atom> "," <atom-list>
  define "AtomList" [
    "single">: symbol "Atom",
    "multiple">: list[symbol "Atom", comma, symbol "AtomList"]],

-- <term> ::= <constant> | <variable>
  define "Term" [
    symbol "Constant",
    symbol "Variable"],
  
-- <term-list> ::= <term> | <term> "," <term-list>
  define "TermList" [
    "single">: symbol "Term",
    "multiple">: list[symbol "Term", comma, symbol "TermList"]],
  
-- <constant-list> ::= <constant> | <constant> "," <constant-list>
  define "ConstantList" [
    "single">: symbol "Constant",
    "multiple">: list[symbol "Constant", comma, symbol "ConstantList"]]]
