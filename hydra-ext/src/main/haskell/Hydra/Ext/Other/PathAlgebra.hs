{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Other.PathAlgebra where

import Hydra.Sources.Tier2.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


pathAlgNs = Namespace "com.gdblab.pathalgebra"
pathAlg = typeref pathAlgNs

pathAlgebraModule :: Module
pathAlgebraModule = Module pathAlgNs elements [hydraCoreModule] [hydraCoreModule] $
    Just ("A syntax model for the path algebra grammar by Angles et al."
      ++ " See the paper \"Path-based Algebraic Foundations of Graph Query Languages\""
      ++ " and the ANTLR grammar at https://github.com/pathalgebra/AlgebraParser")
  where
    def = datatype pathAlgNs

    elements = terminals ++ nonterminals

    -- Terminals from the grammar
    terminals = [
      def "Number" bigint,      -- number in the grammar
      def "Text" string,        -- text in the grammar
      def "Label" string,       -- label in the grammar
      def "Variable" string,    -- var in the grammar
      def "PathName" string]     -- pathName in the grammar

    -- Nonterminal productions from the grammar
    nonterminals = [
-- pathQuery       :   'MATCH' projection restrictor_ext? pathPattern groupby? orderby?  EOF;
      def "PathQuery" $ record [
        "projection">: pathAlg "Projection",
        "restrictorExt">: optional $ pathAlg "RestrictorExt",
        "pathPattern">: pathAlg "PathPattern",
        "groupBy">: optional $ pathAlg "GroupBy",
        "orderBy">: optional $ pathAlg "OrderBy"],

-- projection      :   partProj groupProj pathProj;
      def "Projection" $ record [
        "partProj">: pathAlg "PartProj",
        "groupProj">: pathAlg "GroupProj",
        "pathProj">: pathAlg "PathProj"],

-- partProj        :   'ALL' 'PARTITIONS' | number 'PARTITIONS';
      def "PartProj" $ union [
        "all">: unit,
        "limited">: pathAlg "Number"],

-- groupProj       :   'ALL' 'GROUPS' | number 'GROUPS';
      def "GroupProj" $ union [
        "all">: unit,
        "limited">: pathAlg "Number"],

-- pathProj        :   'ALL' 'PATHS' | number 'PATHS';
      def "PathProj" $ union [
        "all">: unit,
        "limited">: pathAlg "Number"],

-- restrictor_ext  :   'WALK' # WALK | 'TRAIL' # TRAIL| 'SIMPLE' # SIMPLE| 'ACYCLIC' # ACYCLIC| 'SHORTEST' # SHORTEST;
      def "RestrictorExt" $ enum [
        "walk",
        "trail",
        "simple",
        "acyclic",
        "shortest"],

-- orderby         :   'ORDER BY' orderbyoption;
      def "OrderBy" $ pathAlg "OrderByOption",

-- groupby         :   'GROUP BY' groupbyoption;
      def "GroupBy" $ pathAlg "GroupByOption",

-- orderbyoption   :   'PARTITION' # PARTITION
--                |   'GROUP'     #GROUP
--                |   'PATH'     #PATH
--                |   'PARTITION GROUP' #PARTITIONGROUP
--                |   'PARTITION PATH' #PARTITIONPATH
--                |   'GROUP PATH' #GROUPPATH
--                |   'PARTITION GROUP PATH' #PARTITIONGROUPPATH;
      def "OrderByOption" $ enum [
        "partition",
        "group",
        "path",
        "partitionGroup",
        "partitionPath",
        "groupPath",
        "partitionGroupPath"],

-- groupbyoption   :   'SOURCE' # SOURCE
--                |   'TARGET' # TARGET
--                |   'LENGTH' # LENGTH
--                |   'SOURCE TARGET' # SOURCETARGET
--                |   'SOURCE LENGTH' # SOURCELENGTH
--                |   'TARGET LENGTH' # TARGETLENGTH
--                |   'SOURCE TARGET LENGTH'# SOURCETARGETLENGTH;
      def "GroupByOption" $ enum [
        "source",
        "target",
        "length",
        "sourceTarget",
        "sourceLength",
        "targetLength",
        "sourceTargetLength"],

-- pathPattern     :   pathName '=' nodePattern edgePattern nodePattern ('WHERE' complexCondition)? ;
      def "PathPattern" $ record [
        "pathName">: pathAlg "PathName",
        "startNode">: pathAlg "NodePattern",
        "edge">: pathAlg "EdgePattern",
        "endNode">: pathAlg "NodePattern",
        "condition">: optional $ pathAlg "ComplexCondition"],

-- nodePattern     :   '('var?')';
      def "NodePattern" $ record [
        "variable">: optional $ pathAlg "Variable"],

-- edgePattern     :   '-['rpq?']->' | '<-['rpq?']' | '~['rpq?']~';
      def "EdgePattern" $ record [
        "direction">: pathAlg "EdgeDirection",
        "rpq">: optional $ pathAlg "Rpq"],

      def "EdgeDirection" $ enum [
        "outgoing",    -- -[]->
        "incoming",    -- <-[]-
        "undirected"], -- ~[]~],

-- rpq             : '(' rpq ')'  # parenthesis
--                | label     # lbl
--                | '!' label # negated
--                | label '^' # reverse
--                | rpq '?'   # optional
--                | rpq '+' rpqrestrictor? # plus
--                | rpq '*' rpqrestrictor? # star
--                | rpq '/' rpq # concatenation
--                | rpq '|' rpq # alternation
      def "Rpq" $ union [
        "parenthesis">: pathAlg "Rpq",
        "label">: pathAlg "Label",
        "negated">: pathAlg "Label",
        "reverse">: pathAlg "Label",
        "optional">: pathAlg "Rpq",
        "plus">: pathAlg "Plus",
        "star">: pathAlg "Star",
        "concatenation">: pathAlg "Concatenation",
        "alternation">: pathAlg "Alternation"],

      def "Plus" $ record [
        "expression">: pathAlg "Rpq",
        "restrictor">: optional $ pathAlg "RpqRestrictor"],

      def "Star" $ record [
        "expression">: pathAlg "Rpq",
        "restrictor">: optional $ pathAlg "RpqRestrictor"],

      def "Concatenation" $ record [
        "left">: pathAlg "Rpq",
        "right">: pathAlg "Rpq"],

      def "Alternation" $ record [
        "left">: pathAlg "Rpq",
        "right">: pathAlg "Rpq"],

-- rpqrestrictor   : '{'restrictor_ext'}';
      def "RpqRestrictor" $ pathAlg "RestrictorExt",

-- var             :   '?'LETTER ( LETTER | DIGIT)*;
      -- Mapped to "Variable" in terminals

-- pathName        :   LETTER ( LETTER | DIGIT)*;
      -- Mapped to "PathName" in terminals

-- attribute       :   LETTER+;
      -- No associated Hydra definition needed as it's not used elsewhere

-- complexCondition:   condition | condition boolOp complexCondition;
      def "ComplexCondition" $ union [
        "simple">: pathAlg "Condition",
        "compound">: pathAlg "CompoundComplexCondition"],

      def "CompoundComplexCondition" $ record [
        "lhs">: pathAlg "Condition",
        "operator">: pathAlg "BoolOp",
        "rhs">: pathAlg "ComplexCondition"],

-- condition       :   function compareSym '\''text'\'' ;
      def "Condition" $ record [
        "function">: pathAlg "Function",
        "compareSym">: pathAlg "CompareSym",
        "value">: pathAlg "Text"],

-- compareSym      :   '=' | '!=' | '<' | '>' | '<=' | '>=';
      def "CompareSym" $ enum [
        "equal",
        "notEqual",
        "lessThan",
        "greaterThan",
        "lessThanOrEqual",
        "greaterThanOrEqual"],

-- function        :   text'('text')' | text'('function')' |  text'('function','text')';
      def "Function" $ union [
        "simple">: pathAlg "SimpleFunction",
        "nested">: pathAlg "NestedFunction",
        "complex">: pathAlg "ComplexFunction"],

      def "SimpleFunction" $ record [
        "name">: pathAlg "Text",
        "argument">: pathAlg "Text"],

      def "NestedFunction" $ record [
        "name">: pathAlg "Text",
        "innerFunction">: pathAlg "Function"],

      def "ComplexFunction" $ record [
        "name">: pathAlg "Text",
        "innerFunction">: pathAlg "Function",
        "additionalArg">: pathAlg "Text"],

-- boolOp          :   'AND' | 'OR';
      def "BoolOp" $ enum [
        "and",
        "or"]]

-- text            :   (LETTER | DIGIT)+;
      -- Mapped to "Text" in terminals

-- label           :   SIGN LETTER ( LETTER | DIGIT)*;
      -- Mapped to "Label" in terminals

-- number          :   (DIGIT)+;
      -- Mapped to "Number" in terminals

-- LETTER          :   [a-zA-Z];
-- DIGIT           :   [0-9];
-- SIGN            :   ':' ;
-- SYMBOL          :   [.,:'=] ;
-- WS: [ \t\r\n]+ -> skip;
      -- No associated Hydra definition needed as it's a lexer rule for whitespace
