{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Ext.Sql.Ansi where

import Hydra.Kernel
import Hydra.Impl.Haskell.Dsl.Grammars
import Hydra.Util.GrammarToModule
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard

import qualified Data.List as L


sqlModule :: Module Meta
sqlModule = grammarToModule ns sqlGrammar $
    Just ("A subset of ANSI SQL:2003, capturing selected productions of the BNF grammar provided at "
      ++ "https://ronsavage.github.io/SQL/sql-2003-2.bnf.html, which is based on "
      ++ "the Final Committee Draft (FCD) of ISO/IEC 9075-2:2003")
  where
    ns = Namespace "hydra/ext/sql/ansi"

sqlGrammar :: Grammar
sqlGrammar = Grammar $ tokens ++ productions

tokens :: [Production]
tokens = [
    -- <column name>    ::=   <identifier>
    define "ColumnName" [regex identifier],

    -- <domain name>    ::=   <schema qualified name>
    define "DomainName" [regex schemaQualifiedName],

    -- <path-resolved user-defined type name>    ::=   <user-defined type name>
    define "PathResolvedUserDefinedTypeName" [regex userDefinedTypeName],

    -- <table name>    ::=   <local or schema qualified name>
    define "TableName" [regex localOrSchemaQualifiedName],

    -- <unsigned integer>    ::=   <digit> ...
    define "UnsignedInteger" [regex $ plus digit]]
  where
    opt pat = par pat ++ "?"
    or pats = par $ L.intercalate "|" (par <$> pats)
    par pat = "(" ++ pat ++ ")"
    plus pat = par pat ++ "+"
    star pat = par pat ++ "*"

    -- <actual identifier>    ::=   <regular identifier> | <delimited identifier>
    actualIdentifier = regularIdentifier -- Note: this is a simplification

    -- <catalog name>    ::=   <identifier>
    catalogName = identifier

    digit = "[0-9]"

    -- <identifier>    ::=   <actual identifier>
    identifier = actualIdentifier

    -- <identifier body>    ::=   <identifier start> [ <identifier part> ... ]
    identifierBody = identifierStart ++ star identifierPart

    -- <identifier extend>    ::=   !! See the Syntax Rules.
    identifierExtend = "[0-9_]" -- TODO

    -- <identifier part>    ::=   <identifier start> | <identifier extend>
    identifierPart = or [identifierStart, identifierExtend]

    -- <identifier start>    ::=   !! See the Syntax Rules.
    identifierStart = "[A-Za-z]" -- TODO

    -- <local or schema qualifier>    ::=   <schema name> | MODULE
    localOrSchemaQualifier = or [schemaName, "MODULE"]

    -- <local or schema qualified name>    ::=   [ <local or schema qualifier> <period> ] <qualified identifier>
    localOrSchemaQualifiedName = opt (localOrSchemaQualifier ++ period) ++ qualifiedIdentifier

    period = "[.]"

    -- <qualified identifier>    ::=   <identifier>
    qualifiedIdentifier = identifier

    -- <regular identifier>    ::=   <identifier body>
    regularIdentifier = identifierBody

    -- <schema name>    ::=   [ <catalog name> <period> ] <unqualified schema name>
    schemaName = opt (catalogName ++ period) ++ unqualifiedSchemaName

    -- <schema qualified name>    ::=   [ <schema name> <period> ] <qualified identifier>
    schemaQualifiedName = opt (schemaName ++ period) ++ qualifiedIdentifier

    -- <schema qualified type name>    ::=   [ <schema name> <period> ] <qualified identifier>
    schemaQualifiedTypeName = opt (schemaName ++ period) ++ qualifiedIdentifier

    -- <unqualified schema name> ::= <schema name>
    unqualifiedSchemaName = schemaName

    -- <user-defined type name>    ::=   <schema qualified type name>
    userDefinedTypeName = schemaQualifiedTypeName

productions :: [Production]
productions = [
  -- <approximate numeric type>    ::=
       --         FLOAT [ <left paren> <precision> <right paren> ]
       --     |     REAL
       --     |     DOUBLE PRECISION
  define "ApproximateNumericType" [
    list[float_, opt(parens["Precision"])],
    real_,
    double_precision_],

  -- <array type>    ::=   <data type> ARRAY [ <left bracket or trigraph> <unsigned integer> <right bracket or trigraph> ]
  define "ArrayType" unsupported,

  -- <as subquery clause>    ::=   [ <left paren> <column name list> <right paren> ] AS <subquery> <with or without data>
  define "AsSubqueryClause" unsupported,

  -- <binary large object string type>    ::=
  --         BINARY LARGE OBJECT [ <left paren> <large object length> <right paren> ]
  --     |     BLOB [ <left paren> <large object length> <right paren> ]
  define "BinaryLargeObjectStringType" [
    list[binary_large_object_, opt(parens["LargeObjectLength"])],
    list[blob_, opt(parens["LargeObjectLength"])]],

  -- <boolean type>    ::=   BOOLEAN
  define "BooleanType" [
    boolean_],

  -- <character set specification>    ::=
  --         <standard character set name>
  --     |     <implementation-defined character set name>
  --     |     <user-defined character set name>
  define "CharacterSetSpecification" unsupported,

  -- <character string type>    ::=
  --         CHARACTER [ <left paren> <length> <right paren> ]
  --     |     CHAR [ <left paren> <length> <right paren> ]
  --     |     CHARACTER VARYING <left paren> <length> <right paren>
  --     |     CHAR VARYING <left paren> <length> <right paren>
  --     |     VARCHAR <left paren> <length> <right paren>
  --     |     CHARACTER LARGE OBJECT [ <left paren> <large object length> <right paren> ]
  --     |     CHAR LARGE OBJECT [ <left paren> <large object length> <right paren> ]
  --     |     CLOB [ <left paren> <large object length> <right paren> ]
  define "CharacterStringType" [
    list[character_, opt(parens["Length"])],
    list[char_, opt(parens["Length"])],
    list[character_varying_, left_paren_, "Length", right_paren_],
    list[char_varying_, left_paren_, "Length", right_paren_],
    list[varchar_, left_paren_, "Length", right_paren_],
    list[character_large_object_, opt(parens["LargeObjectLength"])],
    list[char_large_object_, opt(parens["LargeObjectLength"])],
    list[clob_, opt(parens["LargeObjectLength"])]],

  -- <collate clause>    ::=   COLLATE <collation name>
  define "CollateClause" unsupported,

  -- <collection type>    ::=   <array type> | <multiset type>
  define "CollectionType" [
    "ArrayType",
    "MultisetType"],

  -- <column constraint definition>    ::=   [ <constraint name definition> ] <column constraint> [ <constraint characteristics> ]
  define "ColumnConstraintDefinition" unsupported,

  -- <column definition>    ::=
  --         <column name> [ <data type> | <domain name> ] [ <reference scope check> ]
  --         [ <default clause> | <identity column specification> | <generation clause> ]
  --         [ <column constraint definition> ... ] [ <collate clause> ]
  define "ColumnDefinition" [list[
    "ColumnName",
    opt(alts["DataType", "DomainName"]),
    opt"ReferenceScopeCheck",
    opt(alts["DefaultClause", "IdentityColumnSpecification", "GenerationClause"]),
    star"ColumnConstraintDefinition",
    opt"CollateClause"]],

  -- <column options>    ::=   <column name> WITH OPTIONS <column option list>
  define "ColumnOptions" unsupported,

  -- <data type>    ::=
  --         <predefined type>
  --     |     <row type>
  --     |     <path-resolved user-defined type name>
  --     |     <reference type>
  --     |     <collection type>
  define "DataType" [
    "PredefinedType",
    "RowType",
    "PathResolvedUserDefinedTypeName",
    "ReferenceType",
    "CollectionType"],

  -- <datetime type>    ::=
  --         DATE
  --     |     TIME [ <left paren> <time precision> <right paren> ] [ <with or without time zone> ]
  --     |     TIMESTAMP [ <left paren> <timestamp precision> <right paren> ] [ <with or without time zone> ]
  define "DatetimeType" unsupported,

  -- <default clause>    ::=   DEFAULT <default option>
  define "DefaultClause" unsupported,

  -- <exact numeric type>    ::=
  --         NUMERIC [ <left paren> <precision> [ <comma> <scale> ] <right paren> ]
  --     |     DECIMAL [ <left paren> <precision> [ <comma> <scale> ] <right paren> ]
  --     |     DEC [ <left paren> <precision> [ <comma> <scale> ] <right paren> ]
  --     |     SMALLINT
  --     |     INTEGER
  --     |     INT
  --     |     BIGINT
  define "ExactNumericType" [
    list[numeric_, opt(parens["Precision", opt(list[comma_, "Scale"])])],
    list[decimal_, opt(parens["Precision", opt(list[comma_, "Scale"])])],
    list[dec_, opt(parens["Precision", opt(list[comma_, "Scale"])])],
    smallint_,
    integer_,
    int_,
    bigint_],

  -- <generation clause>    ::=   <generation rule> AS <generation expression>
  define "GenerationClause" unsupported,

  -- <global or local>    ::=   GLOBAL | LOCAL
  define "GlobalOrLocal" [
    global_,
    local_],

  -- <identity column specification>    ::=
  --         GENERATED { ALWAYS | BY DEFAULT } AS IDENTITY
  --         [ <left paren> <common sequence generator options> <right paren> ]
  define "IdentityColumnSpecification" unsupported,

  -- <interval type>    ::=   INTERVAL <interval qualifier>
  define "IntervalType" unsupported,

  -- <large object length>    ::=
  --         <unsigned integer> [ <multiplier> ] [ <char length units> ]
  --     |     <large object length token> [ <char length units> ]
  define "LargeObjectLength" unsupported,

  -- <length>    ::=   <unsigned integer>
  define "Length" [
      "UnsignedInteger"],

  -- <like clause>    ::=   LIKE <table name> [ <like options> ]
  define "LikeClause" unsupported,

  -- <multiset type>    ::=   <data type> MULTISET
  define "MultisetType" [
    list["DataType", multiset_]],

  -- <national character string type>    ::=
  --         NATIONAL CHARACTER [ <left paren> <length> <right paren> ]
  --     |     NATIONAL CHAR [ <left paren> <length> <right paren> ]
  --     |     NCHAR [ <left paren> <length> <right paren> ]
  --     |     NATIONAL CHARACTER VARYING <left paren> <length> <right paren>
  --     |     NATIONAL CHAR VARYING <left paren> <length> <right paren>
  --     |     NCHAR VARYING <left paren> <length> <right paren>
  --     |     NATIONAL CHARACTER LARGE OBJECT [ <left paren> <large object length> <right paren> ]
  --     |     NCHAR LARGE OBJECT [ <left paren> <large object length> <right paren> ]
  --     |     NCLOB [ <left paren> <large object length> <right paren> ]
  define "NationalCharacterStringType" unsupported,

  -- <numeric type>    ::=   <exact numeric type> | <approximate numeric type>
  define "NumericType" [
    "ExactNumericType",
    "ApproximateNumericType"],

  -- <precision>    ::=   <unsigned integer>
  define "Precision" [
    "UnsignedInteger"],

  -- <predefined type>    ::=
  --         <character string type> [ CHARACTER SET <character set specification> ] [ <collate clause> ]
  --     |     <national character string type> [ <collate clause> ]
  --     |     <binary large object string type>
  --     |     <numeric type>
  --     |     <boolean type>
  --     |     <datetime type>
  --     |     <interval type>
  define "PredefinedType" [
    list["CharacterStringType", opt(list[character_set_, "CharacterSetSpecification"]), opt"CollateClause"],
    list["NationalCharacterStringType", opt"CollateClause"],
    "BinaryLargeObjectStringType",
    "NumericType",
    "BooleanType",
    "DatetimeType",
    "IntervalType"],

  -- <reference scope check>    ::=   REFERENCES ARE [ NOT ] CHECKED [ ON DELETE <reference scope check action> ]
  define "ReferenceScopeCheck" unsupported,

  -- <reference type>    ::=   REF <left paren> <referenced type> <right paren> [ <scope clause> ]
  define "ReferenceType" unsupported,

  -- <row type>    ::=   ROW <row type body>
  define "RowType" unsupported,

  -- <scale>    ::=   <unsigned integer>
  define "Scale" [
    "UnsignedInteger"],

  -- <self-referencing column specification>    ::=   REF IS <self-referencing column name> <reference generation>
  define "SelfReferencingColumnSpecification" unsupported,

  -- <subtable clause>    ::=   UNDER <supertable clause>
  define "SubtableClause" unsupported,

  -- <table commit action>    ::=   PRESERVE | DELETE
  define "TableCommitAction" [
    preserve_,
    delete_],

  -- <table constraint definition>    ::=   [ <constraint name definition> ] <table constraint> [ <constraint characteristics> ]
  define "TableConstraintDefinition" unsupported,

  -- <table contents source>    ::=
  --         <table element list>
  --     |     OF <path-resolved user-defined type name> [ <subtable clause> ] [ <table element list> ]
  --     |     <as subquery clause>
  define "TableContentsSource" [
    "TableElementList",
    list [of_, "PathResolvedUserDefinedTypeName", opt"SubtableClause", opt"TableElementList"],
    "AsSubqueryClause"],

  -- <table definition>    ::=
  --          CREATE [ <table scope> ] TABLE <table name> <table contents source>
  --          [ ON COMMIT <table commit action> ROWS ]
  define "TableDefinition" [
    list[create_, opt"TableScope", table_, "TableName", "TableContentsSource",
      opt(list[on_commit_, "TableCommitAction", rows_])]],

  -- <table element>    ::=
  --         <column definition>
  --     |     <table constraint definition>
  --     |     <like clause>
  --     |     <self-referencing column specification>
  --     |     <column options>
  define "TableElement" [
    "ColumnDefinition",
    "TableConstraintDefinition",
    "LikeClause",
    "SelfReferencingColumnSpecification",
    "ColumnOptions"],

  -- <table element list>    ::=   <left paren> <table element> [ { <comma> <table element> }... ] <right paren>
  define "TableElementList" [
    parens["TableElement", star(list[comma_, "TableElement"])]],

  -- <table scope>    ::=   <global or local> TEMPORARY
  define "TableScope" [
    list["GlobalOrLocal", temporary_]]]

bigint_ = terminal "BIGINT"
binary_large_object_ = terminal "BINARY LARGE OBJECT"
blob_ = terminal "BLOB"
boolean_ = terminal "BOOLEAN"
char_ = terminal "CHAR"
char_large_object_ = terminal "CHAR LARGE OBJECT"
char_varying_ = terminal "CHAR VARYING"
character_ = terminal "CHARACTER"
character_large_object_ = terminal "CHARACTER LARGE OBJECT"
character_set_ = terminal "CHARACTER SET"
character_varying_ = terminal "CHARACTER VARYING"
clob_ = terminal "CLOB"
comma_ = terminal ","
create_ = terminal "CREATE"
dec_ = terminal "DEC"
decimal_ = terminal "DECIMAL"
delete_ = terminal "DELETE"
double_precision_ = terminal "DOUBLE PRECISION"
float_ = terminal "FLOAT"
global_ = terminal "GLOBAL"
int_ = terminal "INT"
integer_ = terminal "INTEGER"
left_paren_ = terminal "("
local_ = terminal "LOCAL"
multiset_ = terminal "MULTISET"
numeric_ = terminal "NUMERIC"
of_ = terminal "OF"
on_commit_ = terminal "ON COMMIT"
preserve_ = terminal "PRESERVE"
real_ = terminal "REAL"
right_paren_ = terminal ")"
rows_ = terminal "ROWS"
smallint_ = terminal "SMALLINT"
table_ = terminal "TABLE"
temporary_ = terminal "TEMPORARY"
varchar_ = terminal "VARCHAR"

parens ps = list $ [left_paren_] ++ ps ++ [right_paren_]
unsupported = []
