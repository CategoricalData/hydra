{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Ext.Datalog.Syntax where

import Hydra.All
import Hydra.Impl.Haskell.Dsl.Grammars
import Hydra.Util.GrammarToModule
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard

comma = terminal ","
entail = terminal ":-"
lparen = terminal "("
period = terminal "."
rparen = terminal ")"


datalogSyntaxModule :: Module Meta
datalogSyntaxModule = grammarToModule ns datalogGrammar $
    Just "A basic Datalog model"
  where
    ns = Namespace "hydra/ext/datalog/syntax"

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
      "Fact",
      "Rule"])],
--    list["Fact", "Program"],
--    list["Rule", "Program"],
--    nil],

-- <fact> ::=  <relation> "(" <constant-list> ")."
  define "Fact" [
    list["Relation", lparen, "ConstantList", rparen, period]],

-- <rule> ::= <atom> ":-" <atom-list> "."
  define "Rule" [
    list["Atom", entail, "AtomList", period]],

-- <atom> ::= <relation> "(" <term-list> ")"
  define "Atom" [
    list["Relation", lparen, "TermList", rparen]],

-- <atom-list> ::= <atom> | <atom> "," <atom-list>
  define "AtomList" [
    "single">: "Atom",
    "multiple">: list["Atom", comma, "AtomList"]],

-- <term> ::= <constant> | <variable>
  define "Term" [
    "Constant",
    "Variable"],

-- <term-list> ::= <term> | <term> "," <term-list>
  define "TermList" [
    "single">: "Term",
    "multiple">: list["Term", comma, "TermList"]],

-- <constant-list> ::= <constant> | <constant> "," <constant-list>
  define "ConstantList" [
    "single">: "Constant",
    "multiple">: list["Constant", comma, "ConstantList"]]]
