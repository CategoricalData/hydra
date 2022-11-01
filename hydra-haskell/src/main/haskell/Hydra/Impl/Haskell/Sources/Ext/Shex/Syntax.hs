{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Ext.Shex.Syntax where

import Hydra.Core
import Hydra.Compute
import Hydra.Module
import Hydra.Grammar
import Hydra.Impl.Haskell.Dsl.Grammars
import Hydra.Util.GrammarToModule
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard


base_ = terminal "BASE"
prefix_ = terminal "PREFIX"
start_ = terminal "start"
equal_ = terminal "="
or_ = terminal "OR"
and_ = terminal "AND"
not_ = terminal "NOT"
true_ = terminal "true"
false_ = terminal "false"
iri_ = terminal "IRI"
bnode_ = terminal "BNODE"
literal_ = terminal "LITERAL"
nonLiteral_ = terminal "NONLITERAL"
length_ = terminal "LENGTH"
minLength_ = terminal "MINLENGTH"
maxLength_ = terminal "MAXLENGTH"
external_ = terminal "EXTERNAL"
percent_ = terminal "%"
at_ = terminal "@"
dollar_ = terminal "$"
ampersand_ = terminal "&"
colon_ = terminal ":"
period_ = terminal "."
coma_ = terminal ","
semicolon_ = terminal ";"
underscore_ = terminal "_"
dash_ = terminal "-"
parenOpen_ = terminal "("
parenClose_ = terminal ")"
braceOpen_ = terminal "{"
braceClose_ = terminal "}"
pipe_ = terminal "|"
star_ = terminal "*"
plus_ = terminal "+"
question_ = terminal "?"
tilde_ = terminal "~"
doubleFrwSlash_ = terminal "\\"
singleQuote_ = terminal "'"
doubleQuote_ = terminal "\""

minInclusive_ = terminal "MININCLUSIVE"
minExclusive_ = terminal "MINEXCLUSIVE"
maxInclusive_ = terminal "MAXINCLUSIVE"
maxExclusive_ = terminal "MAXEXCLUSIVE"
totalDigits_ = terminal "TOTALDIGITS"
fractionDigits_ = terminal "FRACTIONDIGITS"
extra_ = terminal "EXTRA"
closed_ = terminal "CLOSED"


shexSyntaxModule :: Module Meta
shexSyntaxModule = grammarToModule ns shexGrammar $
    Just ("A Shex model. Based on the BNF at:\n" ++
        "  https://github.com/shexSpec/grammar/blob/master/bnf")
    where
        ns = Namespace "hydra/ext/shex/syntax"

shexGrammar :: Grammar
shexGrammar = Grammar [

-- [1] shexDoc ::= directive* ((notStartAction | startActions) statement*)?
  define "shexDoc" [
    list[star"directive", opt(list[ alts["notStartAction", "startActions"], star "statement" ]), "prefixDecl"]],

-- [2] directive ::= baseDecl | prefixDecl
-- [3] baseDecl ::= "BASE" IRIREF
-- [4] prefixDecl ::= "PREFIX" PNAME_NS IRIREF
  define "directive" [
     "baseDecl" >: list[base_, "IRIREF"],
     "prefixDecl" >: list[prefix_, "PNAME_NS", "IRIREF"]],

-- [5] notStartAction ::= start | shapeExprDecl
-- [6] start ::= "start" '=' shapeExpression
-- [9] shapeExprDecl ::= shapeExprLabel (shapeExpression|"EXTERNAL")
  define "notStartAction" [
    "start">: list[start_, equal_, "shapeExpression"],
    "shapeExprDecl">: list["shapeExprLabel", alts["shapeExpression", external_]]],

-- [7] startActions ::= codeDecl+
  define "startActions" [
    plus("codeDecl")],

-- [8] statement ::= directive | notStartAction
  define "statement" [
    alts[ "directive", "notStartAction"]],

-- [10] shapeExpression ::= shapeOr
  define "shapeExpression" [
    "shapeOr"],

-- [11] inlineShapeExpression ::= inlineShapeOr
  define "inlineShapeExpression" [
    "inlineShapeOr"],

-- [12] shapeOr ::= shapeAnd ("OR" shapeAnd)*
  define "shapeOr" [
    list["shapeAnd", star(list[or_, "shapeAnd"])]],

-- [13] inlineShapeOr ::= inlineShapeAnd ("OR" inlineShapeAnd)*
  define "inlineShapeOr" [
    list["shapeAnd", star(list[or_, "inlineShapeAnd"])]],

-- [14] shapeAnd ::= shapeNot ("AND" shapeNot)*
  define "shapeAnd" [
    list["shapeNot", star(list[and_, "shapeNot"])]],

-- [15] inlineShapeAnd ::= inlineShapeNot ("AND" inlineShapeNot)*
  define "inlineShapeAnd" [
    list["inlineShapeNot", star(list[and_, "inlineShapeNot"])]],

-- [16] shapeNot ::= "NOT"? shapeAtom
  define "shapeNot" [
    list[opt (not_), "shapeAtom"]],

-- [17] inlineShapeNot ::= "NOT"? inlineShapeAtom
  define "inlineShapeNot" [
    list[opt(not_), "inlineShapeAtom"]],

-- [18] shapeAtom ::= nodeConstraint shapeOrRef?
--                                   | shapeOrRef
--                                   | "(" shapeExpression ")"
--                                   | '.'  # no constraint
  define "shapeAtom" [
    list["nodeConstraint", opt("shapeOrRef")],
    "shapeOrRef",
    list[parenOpen_, "shapeExpression", parenClose_],
    period_],

-- [19] inlineShapeAtom ::= nodeConstraint inlineShapeOrRef?
--                                   | inlineShapeOrRef nodeConstraint?
--                                   | "(" shapeExpression ")"
--                                   | '.'  # no constraint
  define "inlineShapeAtom" [
    list["nodeConstraint", opt("inlineShapeOrRef")],
    list["inlineShapeOrRef", opt("nodeConstraint")],
    list[parenOpen_, "shapeExpression", parenClose_],
    period_],

-- [20] shapeOrRef ::= shapeDefinition
--                                   | ATPNAME_LN | ATPNAME_NS | '@' shapeExprLabel
  define "shapeOrRef" [
    "shapeDefinition",
    "ATPNAME_LN",
    "ATPNAME_NS",
    list[at_,"shapeExprLabel"]],

-- [21] inlineShapeOrRef ::= inlineShapeDefinition
--                                   | ATPNAME_LN | ATPNAME_NS | '@' shapeExprLabel
  define "inlineShapeOrRef" [
    "inlineShapeDefinition",
    "ATPNAME_LN",
    "ATPNAME_NS",
    list[at_,"shapeExprLabel"]],

-- [22] nodeConstraint ::= "LITERAL" xsFacet*
--                                   | nonLiteralKind stringFacet*
--                                   | datatype xsFacet*
--                                   | valueSet xsFacet*
--                                   | xsFacet+
  define "nodeConstraint" [
    list[literal_, star("xsFacet")],
    list["nonLiteralKind", star("stringFacet")],
    list["datatype", star("xsFacet")],
    list["valueSet", star("xsFacet")],
    list["valueSet", star("xsFacet")],
    plus("xsFacet")],

-- [23] nonLiteralKind ::= "IRI" | "BNODE" | "NONLITERAL"
  define "nonLiteralKind" [iri_, bnode_, nonLiteral_],

-- [24] xsFacet ::= stringFacet | numericFacet
  define "xsFacet" ["stringFacet", "numericFacet"],

-- [25] stringFacet ::= stringLength INTEGER | REGEXP
  define "stringFacet" [
    list ["stringLength", "INTEGER"],
    "REGEXP" ],

-- [26] stringLength ::= "LENGTH" | "MINLENGTH" | "MAXLENGTH"
  define "stringLength" [
    length_, minLength_, maxLength_],

-- [27] numericFacet ::= numericRange numericLiteral
--                                   | numericLength INTEGER
  define "numericFacet" [
    list ["numericRange", "numericLiteral"],
    list ["numericLength", "INTEGER"]],

-- [28] numericRange ::= "MININCLUSIVE" | "MINEXCLUSIVE" | "MAXINCLUSIVE" | "MAXEXCLUSIVE"
  define "numericRange" [
    minInclusive_, minExclusive_, maxInclusive_, maxExclusive_],

-- [29] numericLength ::= "TOTALDIGITS" | "FRACTIONDIGITS"
  define "numericLength" [
    totalDigits_, fractionDigits_],

-- [30] shapeDefinition ::= (includeSet | extraPropertySet | "CLOSED")* '{' tripleExpression? '}' annotation* semanticActions
  define "shapeDefinition" [
    list[star(alts["includeSet", "extraPropertySet", closed_]),
        braceOpen_, opt("tripleExpression"), braceClose_,
        star("annotation"), "semanticActions"]],

-- [31] inlineShapeDefinition ::= (includeSet | extraPropertySet | "CLOSED")* '{' tripleExpression? '}'
  define "inlineShapeDefinition" [
    list[star(alts["includeSet", "extraPropertySet", closed_]), braceOpen_, opt("tripleExpression"), braceClose_]],

-- [32] extraPropertySet ::= "EXTRA" predicate+
  define "extraPropertySet" [
    list[extra_, plus"predicate"]],

-- [33] tripleExpression ::= oneOfTripleExpr
  define "tripleExpression" [
    "oneOfTripleExpr"],

-- [34] oneOfTripleExpr ::= groupTripleExpr | multiElementOneOf
  define "oneOfTripleExpr" [
    "groupTripleExpr",
    "multiElementOneOf"],

-- [35] multiElementOneOf ::= groupTripleExpr ('|' groupTripleExpr)+
  define "multiElementOneOf" [
    list["groupTripleExpr", plus(list[pipe_, "groupTripleExpr"])]],

-- [36] innerTripleExpr ::= multiElementGroup | multiElementOneOf
  define "innerTripleExpr" [
    "multiElementGroup",
    "multiElementOneOf"],

-- [37] groupTripleExpr ::= singleElementGroup | multiElementGroup
  define "groupTripleExpr" [
    "singleElementGroup",
    "multiElementGroup"],

-- [38] singleElementGroup ::= unaryTripleExpr ';'?
  define "singleElementGroup" [
    list["unaryTripleExpr", opt(semicolon_)]],

-- [39] multiElementGroup ::= unaryTripleExpr (';' unaryTripleExpr)+ ';'?
  define "multiElementGroup" [
    list["unaryTripleExpr", plus(list[semicolon_, "unaryTripleExpr"]), opt(semicolon_)]],

-- [40] unaryTripleExpr ::= ('$' tripleExprLabel)? (tripleConstraint | bracketedTripleExpr) | include
  define "unaryTripleExpr" [
    list[opt(list[dollar_, "tripleExprLabel"]), alts["tripleConstraint", "bracketedTripleExpr"]],
    "include"],

-- [41] bracketedTripleExpr ::= '(' innerTripleExpr ')' cardinality? annotation* semanticActions
  define "bracketedTripleExpr" [
    list[parenOpen_, "innerTripleExpr", parenClose_, opt"cardinality", star"annotation", "semanticActions"]],

-- [43] tripleConstraint ::= senseFlags? predicate inlineShapeExpression cardinality? annotation* semanticActions
  define "tripleConstraint" [
    list[opt"senseFlags", "predicate", "inlineShapeExpression", opt"cardinality", star"annotation", "semanticActions"]],

-- [44] cardinality ::= '*' | '+' | '?' | REPEAT_RANGE
  define "cardinality" [
    star_, plus_, question_, "REPEAT_RANGE"],


-- [45] senseFlags ::= '^'
  define "senseFlags" [
    terminal "^"],

-- [46] valueSet ::= '[' valueSetValue* ']'
  define "valueSet" [
    list[terminal "[", star"valueSetValue", terminal "]"]],

-- [47] valueSetValue ::= iriRange | literal
  define "valueSetValue" [
    "iriRange",
    "literal"],

-- [48] iriRange ::= iri ('~' exclusion*)? | '.' exclusion+
  define "iriRange" [
    list["iri", opt(list[tilde_, star"exclusion"])],
    list[period_, plus"exclusion"]],

-- [49] exclusion ::= '-' iri '~'?
  define "exclusion" [
    list[dash_, "iri", tilde_]],

-- [50] include ::= '&' tripleExprLabel
  define "include" [
    list[ampersand_, "tripleExprLabel"]],

-- [51] annotation ::= '//' predicate (iri | literal)
  define "annotation" [
    list[terminal "//", "predicate", alts["iri", "literal"]]],

-- [52] semanticActions ::= codeDecl*
  define "semanticActions" [
    star"codeDecl"],

-- [53] codeDecl ::= '%' iri (CODE | "%")
  define "codeDecl" [
    list[percent_, "iri", alts["CODE", percent_]]],

-- [13t] literal ::= rdfLiteral | numericLiteral | booleanLiteral
  define "literal" [
    "rdfLiteral",
    "numericLiteral",
    "booleanLiteral"],

-- [54] predicate ::= iri | RDF_TYPE
  define "predicate" [
    "iri",
    "RDF_TYPE"],

-- [55] datatype ::= iri
  define "datatype" [
    "iri"],

-- [56] shapeExprLabel ::= iri | blankNode
  define "shapeExprLabel" [
    "iri",
    "blankNode"],

-- [42] tripleExprLabel ::= '$' (iri | blankNode)
  define "tripleExprLabel" [
    list[dollar_, alts["iri", "blankNode"]]],

-- [16t] numericLiteral ::= INTEGER | DECIMAL | DOUBLE
  define "numericLiteral" [
    "INTEGER", "DECIMAL", "DOUBLE"],

-- [129s] rdfLiteral ::= string (LANGTAG | '^^' datatype)?
  define "rdfLiteral" [
    list["string", opt(alts["LANGTAG", list[terminal "^^", "datatype"]])]],

-- [134s] booleanLiteral ::= 'true' | 'false'
  define "booleanLiteral" [
    true_, false_],

-- [135s] string ::= STRING_LITERAL1 | STRING_LITERAL_LONG1
--                 | STRING_LITERAL2 | STRING_LITERAL_LONG2
  define "string" [
    "STRING_LITERAL1",
    "STRING_LITERAL_LONG1",
    "STRING_LITERAL2",
    "STRING_LITERAL_LONG2"],

-- [136s] iri ::= IRIREF | prefixedName
  define "iri" [
    "IRIREF",
    "prefixedName"],

-- [137s] prefixedName ::= PNAME_LN | PNAME_NS
  define "prefixedName" [
    "PNAME_LN",
    "PNAME_NS"],

-- [138s] blankNode ::= BLANK_NODE_LABEL
  define "blankNode" [
    "BLANK_NODE_LABEL"],

--   # Reserved for future use
-- [57] includeSet ::= '&' shapeExprLabel+
  define "includeSet" [
    list[ampersand_, plus"shapeExprLabel"]],

-- [58] CODE ::= '{' ([^%\\] | '\\' [%\\] | UCHAR)* '%' '}'
  define "CODE" [
    list[braceOpen_, star(alts[ regex "[^%\\]", list[doubleFrwSlash_, regex "[%\\]"], "UCHAR" ]), percent_, braceClose_]],

-- [59] REPEAT_RANGE ::= '{' INTEGER (',' (INTEGER | '*')?)? '}'
  define "REPEAT_RANGE" [
    list[braceOpen_, "INTEGER", opt(list[coma_, opt(opt(alts["INTEGER", star_]))]), braceClose_]],

-- [60] RDF_TYPE ::= 'a'
  define "RDF_TYPE" [
    terminal "a"],

-- [18t] IRIREF ::= '<' ([^#x00-#x20<>\"{}|^`\\] | UCHAR)* '>' /* #x00=NULL #01-#x1F=control codes #x20=space */
  define "IRIREF" [
    list[terminal "<", star(alts[ regex "[^#x00-#x20<>\"{}|^`\\]", "UCHAR"]), terminal ">"]],

-- [140s] PNAME_NS ::= PN_PREFIX? ':'
  define "PNAME_NS" [
    list[opt"PN_PREFIX", semicolon_]],

-- [141s] PNAME_LN ::= PNAME_NS PN_LOCAL
  define "PNAME_LN" [
    list["PNAME_NS", "PN_LOCAL"]],

-- [61] ATPNAME_NS ::= '@' PN_PREFIX? ':'
  define "ATPNAME_NS" [
    list[at_, opt"PN_PREFIX", colon_]],

-- [62] ATPNAME_LN ::= '@' PNAME_NS PN_LOCAL
  define "ATPNAME_LN" [
    list[at_, "PNAME_NS", "PN_LOCAL"]],

-- [63] REGEXP ::= '~/' ([^#x2f#x5C#xA#xD] | '\\' [tbnrf\\/] | UCHAR)* '/' [smix]*
  define "REGEXP" [
    list[terminal "~/", star(alts[
        regex "[^#x2f#x5C#xA#xD]",
        list[terminal "\\", regex "[tbnrf\\/]"], "UCHAR"]),
        terminal "/", star( regex "[smix]")
    ]],

-- [142s] BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
  define "BLANK_NODE_LABEL" [
    list[terminal "_:", alts["PN_CHARS_U", regex "[0-9]"], opt(star(alts["PN_CHARS", period_])),"PN_CHARS"]],

-- [145s] LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
  define "LANGTAG" [
    regex "@[a-zA-Z]+('-'[a-zA-Z0-9]+)*"],

-- [19t] INTEGER ::= [+-]? [0-9]+
  define "INTEGER" [
    regex "[+-]? [0-9]+"],

-- [20t] DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
  define "DECIMAL" [
    regex "[+-]? [0-9]*\\.[0-9]+"],

-- [21t] DOUBLE ::= [+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.'? [0-9]+ EXPONENT)
-- [155s] EXPONENT ::= [eE] [+-]? [0-9]+
  define "DOUBLE" [
    regex "([+-]?([0-9]+)?\\.[0-9]*[eE][+-]?[0-9]+"],

-- [156s] STRING_LITERAL1 ::= "'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'" /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
  define "STRING_LITERAL1" [
    list[singleQuote_, star(alts[regex "[^#x27#x5C#xA#xD]", "ECHAR", "UCHAR"]), singleQuote_]],

-- [157s] STRING_LITERAL2 ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"' /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
  define "STRING_LITERAL2" [
    list[doubleQuote_, star(alts[regex "[^#x22#x5C#xA#xD]", "ECHAR", "UCHAR"]), doubleQuote_]],

-- [158s] STRING_LITERAL_LONG1 ::= "'''" (("'" | "''")? ([^\'\\] | ECHAR | UCHAR))* "'''"
  define "STRING_LITERAL_LONG1" [
    list[singleQuote_, singleQuote_, singleQuote_,
        star(alts[list[ opt(alts[singleQuote_, list[singleQuote_,singleQuote_]]), regex "[^\'\\]"], "ECHAR", "UCHAR"]),
        singleQuote_, singleQuote_, singleQuote_]],

-- [159s] STRING_LITERAL_LONG2 ::= '"""' (('"' | '""')? ([^\"\\] | ECHAR | UCHAR))* '"""'
  define "STRING_LITERAL_LONG2" [
    list[doubleQuote_, doubleQuote_, doubleQuote_,
        star(alts[list[ opt(alts[doubleQuote_, list[doubleQuote_,doubleQuote_]]), regex "[^\"\\]"], "ECHAR", "UCHAR"]),
        doubleQuote_, doubleQuote_, doubleQuote_]],

-- [26t] UCHAR ::= '\\u' HEX HEX HEX HEX
--                                   | '\\U' HEX HEX HEX HEX HEX HEX HEX HEX
  define "UCHAR" [
    list["\\u", "HEX", "HEX", "HEX", "HEX"],
    list["\\U", "HEX", "HEX", "HEX", "HEX", "HEX", "HEX", "HEX", "HEX"]],

-- [160s] ECHAR ::= '\\' [tbnrf\\\"\']
  define "ECHAR" [
    list[doubleFrwSlash_, regex "[tbnrf\\\"\']"]],

-- [164s] PN_CHARS_BASE ::= [A-Z] | [a-z]
--                                   | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF]
--                                   | [#x0370-#x037D] | [#x037F-#x1FFF]
--                                   | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF]
--                                   | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
--                                   | [#x10000-#xEFFFF]
  define "PN_CHARS_BASE" [
    regex "[A-Z]",
    regex "[a-z]"], -- TODO other chars


-- [165s] PN_CHARS_U ::= PN_CHARS_BASE | '_'
  define "PN_CHARS_U" [
    "PN_CHARS_BASE",
    underscore_],

-- [167s] PN_CHARS ::= PN_CHARS_U | '-' | [0-9]
--                                   | [#x00B7] | [#x0300-#x036F] | [#x203F-#x2040]
  define "PN_CHARS" [
    "PN_CHARS_U",
    dash_,
    regex "0-9"], -- TODO other chars

-- [168s] PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
  define "PN_PREFIX" [
    list["PN_CHARS_BASE",
    opt(list[alts["PN_CHARS", period_],"PN_CHARS"])]],

-- [169s] PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
  define "PN_LOCAL" [
    list[
      alts["PN_CHARS_U", colon_, regex "0-9", "PLX"],
      opt(list[
        star(alts["PN_CHARS", period_, colon_, "PLX"]),
        alts["PN_CHARS", colon_, "PLX"]]
      )]],

-- [170s] PLX ::= PERCENT | PN_LOCAL_ESC
  define "PLX" [
    "PERCENT",
    "PN_LOCAL_ESC"],

-- [171s] PERCENT ::= '%' HEX HEX
  define "PERCENT" [
    list[percent_, "HEX", "HEX"]],

-- [172s] HEX ::= [0-9] | [A-F] | [a-f]
  define "HEX" [regex "[0-9][A-F][a-f]"],


-- [173s] PN_LOCAL_ESC ::= '\\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')
  define "PN_LOCAL_ESC" [
    list[doubleFrwSlash_, regex "[_~\\.-!$&'\\(\\)\\*+,;=/\\?#@%]"]]]

--   @pass ::= [ \t\r\n]+ -- TODO
--           | "#" [^\r\n]*

