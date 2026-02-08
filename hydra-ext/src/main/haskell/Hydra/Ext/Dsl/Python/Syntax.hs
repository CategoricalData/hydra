-- | Meta-DSL for constructing Python syntax terms as first-class values.
-- Provides constructors and accessors for Python AST types.

module Hydra.Ext.Dsl.Python.Syntax where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Ext.Python.Syntax as Py

import Prelude hiding (map)


-- =============================================================================
-- Module (wrapper type)
-- =============================================================================

-- | Wrap statement groups into a Module
module_ :: TTerm [[Py.Statement]] -> TTerm Py.Module
module_ = wrap Py._Module

-- | Unwrap a Module to get its statement groups
unModule :: TTerm Py.Module -> TTerm [[Py.Statement]]
unModule m = unwrap Py._Module @@ m

-- =============================================================================
-- Name (wrapper type)
-- =============================================================================

-- | Wrap a string into a Name
name :: TTerm String -> TTerm Py.Name
name = wrap Py._Name

-- | Unwrap a Name to get its string value
unName :: TTerm Py.Name -> TTerm String
unName n = unwrap Py._Name @@ n

-- =============================================================================
-- DottedName (wrapper type)
-- =============================================================================

-- | Wrap names into a DottedName
dottedName :: TTerm [Py.Name] -> TTerm Py.DottedName
dottedName = wrap Py._DottedName

-- | Unwrap a DottedName
unDottedName :: TTerm Py.DottedName -> TTerm [Py.Name]
unDottedName dn = unwrap Py._DottedName @@ dn

-- =============================================================================
-- AnnotatedStatement (record type)
-- =============================================================================

-- | Construct an AnnotatedStatement
annotatedStatement :: TTerm String -> TTerm Py.Statement -> TTerm Py.AnnotatedStatement
annotatedStatement comment stmt = record Py._AnnotatedStatement [
  Py._AnnotatedStatement_comment>>: comment,
  Py._AnnotatedStatement_statement>>: stmt]

-- | Project the comment field
annotatedStatementComment :: TTerm Py.AnnotatedStatement -> TTerm String
annotatedStatementComment as = project Py._AnnotatedStatement Py._AnnotatedStatement_comment @@ as

-- | Project the statement field
annotatedStatementStatement :: TTerm Py.AnnotatedStatement -> TTerm Py.Statement
annotatedStatementStatement as = project Py._AnnotatedStatement Py._AnnotatedStatement_statement @@ as

-- =============================================================================
-- Statement (union type)
-- =============================================================================

-- | Inject CompoundStatement into Statement
statementCompound :: TTerm Py.CompoundStatement -> TTerm Py.Statement
statementCompound = inject Py._Statement Py._Statement_compound

-- | Inject SimpleStatements into Statement
statementSimple :: TTerm [Py.SimpleStatement] -> TTerm Py.Statement
statementSimple = inject Py._Statement Py._Statement_simple

-- | Inject AnnotatedStatement into Statement
statementAnnotated :: TTerm Py.AnnotatedStatement -> TTerm Py.Statement
statementAnnotated = inject Py._Statement Py._Statement_annotated

-- =============================================================================
-- SimpleStatement (union type)
-- =============================================================================

simpleStatementAssignment :: TTerm Py.Assignment -> TTerm Py.SimpleStatement
simpleStatementAssignment = inject Py._SimpleStatement Py._SimpleStatement_assignment

simpleStatementTypeAlias :: TTerm Py.TypeAlias -> TTerm Py.SimpleStatement
simpleStatementTypeAlias = inject Py._SimpleStatement Py._SimpleStatement_typeAlias

simpleStatementStarExpressions :: TTerm [Py.StarExpression] -> TTerm Py.SimpleStatement
simpleStatementStarExpressions = inject Py._SimpleStatement Py._SimpleStatement_starExpressions

simpleStatementReturn :: TTerm Py.ReturnStatement -> TTerm Py.SimpleStatement
simpleStatementReturn = inject Py._SimpleStatement Py._SimpleStatement_return

simpleStatementImport :: TTerm Py.ImportStatement -> TTerm Py.SimpleStatement
simpleStatementImport = inject Py._SimpleStatement Py._SimpleStatement_import

simpleStatementRaise :: TTerm Py.RaiseStatement -> TTerm Py.SimpleStatement
simpleStatementRaise = inject Py._SimpleStatement Py._SimpleStatement_raise

simpleStatementPass :: TTerm Py.SimpleStatement
simpleStatementPass = injectUnit Py._SimpleStatement Py._SimpleStatement_pass

-- =============================================================================
-- CompoundStatement (union type)
-- =============================================================================

compoundStatementFunction :: TTerm Py.FunctionDefinition -> TTerm Py.CompoundStatement
compoundStatementFunction = inject Py._CompoundStatement Py._CompoundStatement_function

compoundStatementClassDef :: TTerm Py.ClassDefinition -> TTerm Py.CompoundStatement
compoundStatementClassDef = inject Py._CompoundStatement Py._CompoundStatement_classDef

compoundStatementMatch :: TTerm Py.MatchStatement -> TTerm Py.CompoundStatement
compoundStatementMatch = inject Py._CompoundStatement Py._CompoundStatement_match

-- =============================================================================
-- FunctionDefinition (record type)
-- =============================================================================

-- | Construct a FunctionDefinition with optional decorators and raw definition
functionDefinition :: TTerm (Maybe Py.Decorators) -> TTerm Py.FunctionDefRaw -> TTerm Py.FunctionDefinition
functionDefinition decs raw = record Py._FunctionDefinition [
  Py._FunctionDefinition_decorators>>: decs,
  Py._FunctionDefinition_raw>>: raw]

-- | Construct a simple FunctionDefinition (no decorators)
functionDefinitionSimple :: TTerm Py.FunctionDefRaw -> TTerm Py.FunctionDefinition
functionDefinitionSimple raw = functionDefinition nothing raw

-- =============================================================================
-- FunctionDefRaw (record type)
-- =============================================================================

-- | Construct a FunctionDefRaw with all fields
functionDefRaw :: TTerm Bool -> TTerm Py.Name -> TTerm [Py.TypeParameter] -> TTerm (Maybe Py.Parameters) -> TTerm (Maybe Py.Expression) -> TTerm (Maybe Py.FuncTypeComment) -> TTerm Py.Block -> TTerm Py.FunctionDefRaw
functionDefRaw async name_ tparams params retType funcTypeComment block_ = record Py._FunctionDefRaw [
  Py._FunctionDefRaw_async>>: async,
  Py._FunctionDefRaw_name>>: name_,
  Py._FunctionDefRaw_typeParams>>: tparams,
  Py._FunctionDefRaw_params>>: params,
  Py._FunctionDefRaw_returnType>>: retType,
  Py._FunctionDefRaw_funcTypeComment>>: funcTypeComment,
  Py._FunctionDefRaw_block>>: block_]

-- | Construct a simple synchronous FunctionDefRaw (no async, no type params, no return type, no func type comment)
functionDefRawSimple :: TTerm Py.Name -> TTerm (Maybe Py.Parameters) -> TTerm Py.Block -> TTerm Py.FunctionDefRaw
functionDefRawSimple name_ params block_ = functionDefRaw false name_ (list ([] :: [TTerm Py.TypeParameter])) params nothing nothing block_

-- =============================================================================
-- Block (union type)
-- =============================================================================

blockIndented :: TTerm [[Py.Statement]] -> TTerm Py.Block
blockIndented = inject Py._Block Py._Block_indented

blockSimple :: TTerm [Py.SimpleStatement] -> TTerm Py.Block
blockSimple = inject Py._Block Py._Block_simple

-- =============================================================================
-- Expression (union type)
-- =============================================================================

expressionLambda :: TTerm Py.Lambda -> TTerm Py.Expression
expressionLambda = inject Py._Expression Py._Expression_lambda

expressionSimple :: TTerm Py.Disjunction -> TTerm Py.Expression
expressionSimple = inject Py._Expression Py._Expression_simple

-- =============================================================================
-- NamedExpression (union type)
-- =============================================================================

namedExpressionSimple :: TTerm Py.Expression -> TTerm Py.NamedExpression
namedExpressionSimple = inject Py._NamedExpression Py._NamedExpression_simple

namedExpressionAssignment :: TTerm Py.AssignmentExpression -> TTerm Py.NamedExpression
namedExpressionAssignment = inject Py._NamedExpression Py._NamedExpression_assignment

-- =============================================================================
-- AssignmentExpression (record type)
-- =============================================================================

assignmentExpression :: TTerm Py.Name -> TTerm Py.Expression -> TTerm Py.AssignmentExpression
assignmentExpression n expr = record Py._AssignmentExpression [
  Py._AssignmentExpression_name>>: n,
  Py._AssignmentExpression_expression>>: expr]

assignmentExpressionName :: TTerm Py.AssignmentExpression -> TTerm Py.Name
assignmentExpressionName ae = project Py._AssignmentExpression Py._AssignmentExpression_name @@ ae

assignmentExpressionExpression :: TTerm Py.AssignmentExpression -> TTerm Py.Expression
assignmentExpressionExpression ae = project Py._AssignmentExpression Py._AssignmentExpression_expression @@ ae

-- =============================================================================
-- Disjunction (wrapper type)
-- =============================================================================

disjunction :: TTerm [Py.Conjunction] -> TTerm Py.Disjunction
disjunction = wrap Py._Disjunction

unDisjunction :: TTerm Py.Disjunction -> TTerm [Py.Conjunction]
unDisjunction d = unwrap Py._Disjunction @@ d

-- =============================================================================
-- Conjunction (wrapper type)
-- =============================================================================

conjunction :: TTerm [Py.Inversion] -> TTerm Py.Conjunction
conjunction = wrap Py._Conjunction

unConjunction :: TTerm Py.Conjunction -> TTerm [Py.Inversion]
unConjunction c = unwrap Py._Conjunction @@ c

-- =============================================================================
-- Inversion (union type)
-- =============================================================================

inversionNot :: TTerm Py.Inversion -> TTerm Py.Inversion
inversionNot = inject Py._Inversion Py._Inversion_not

inversionSimple :: TTerm Py.Comparison -> TTerm Py.Inversion
inversionSimple = inject Py._Inversion Py._Inversion_simple

-- =============================================================================
-- Comparison (record type)
-- =============================================================================

comparison :: TTerm Py.BitwiseOr -> TTerm [Py.CompareOpBitwiseOrPair] -> TTerm Py.Comparison
comparison lhs rhs = record Py._Comparison [
  Py._Comparison_lhs>>: lhs,
  Py._Comparison_rhs>>: rhs]

comparisonLhs :: TTerm Py.Comparison -> TTerm Py.BitwiseOr
comparisonLhs c = project Py._Comparison Py._Comparison_lhs @@ c

comparisonRhs :: TTerm Py.Comparison -> TTerm [Py.CompareOpBitwiseOrPair]
comparisonRhs c = project Py._Comparison Py._Comparison_rhs @@ c

-- =============================================================================
-- BitwiseOr (record type)
-- =============================================================================

bitwiseOr :: TTerm (Maybe Py.BitwiseOr) -> TTerm Py.BitwiseXor -> TTerm Py.BitwiseOr
bitwiseOr lhs rhs = record Py._BitwiseOr [
  Py._BitwiseOr_lhs>>: lhs,
  Py._BitwiseOr_rhs>>: rhs]

bitwiseOrLhs :: TTerm Py.BitwiseOr -> TTerm (Maybe Py.BitwiseOr)
bitwiseOrLhs bo = project Py._BitwiseOr Py._BitwiseOr_lhs @@ bo

bitwiseOrRhs :: TTerm Py.BitwiseOr -> TTerm Py.BitwiseXor
bitwiseOrRhs bo = project Py._BitwiseOr Py._BitwiseOr_rhs @@ bo

-- =============================================================================
-- BitwiseXor (record type)
-- =============================================================================

bitwiseXorLhs :: TTerm Py.BitwiseXor -> TTerm (Maybe Py.BitwiseXor)
bitwiseXorLhs bx = project Py._BitwiseXor Py._BitwiseXor_lhs @@ bx

bitwiseXorRhs :: TTerm Py.BitwiseXor -> TTerm Py.BitwiseAnd
bitwiseXorRhs bx = project Py._BitwiseXor Py._BitwiseXor_rhs @@ bx

-- =============================================================================
-- BitwiseAnd (record type)
-- =============================================================================

bitwiseAndLhs :: TTerm Py.BitwiseAnd -> TTerm (Maybe Py.BitwiseAnd)
bitwiseAndLhs ba = project Py._BitwiseAnd Py._BitwiseAnd_lhs @@ ba

bitwiseAndRhs :: TTerm Py.BitwiseAnd -> TTerm Py.ShiftExpression
bitwiseAndRhs ba = project Py._BitwiseAnd Py._BitwiseAnd_rhs @@ ba

-- =============================================================================
-- ShiftExpression (record type)
-- =============================================================================

shiftExpressionRhs :: TTerm Py.ShiftExpression -> TTerm Py.Sum
shiftExpressionRhs se = project Py._ShiftExpression Py._ShiftExpression_rhs @@ se

-- =============================================================================
-- Sum (record type)
-- =============================================================================

sumRhs :: TTerm Py.Sum -> TTerm Py.Term
sumRhs s = project Py._Sum Py._Sum_rhs @@ s

-- =============================================================================
-- Term (record type)
-- =============================================================================

termRhs :: TTerm Py.Term -> TTerm Py.Factor
termRhs t = project Py._Term Py._Term_rhs @@ t

-- =============================================================================
-- Factor (union type)
-- =============================================================================

factorPositive :: TTerm Py.Factor -> TTerm Py.Factor
factorPositive = inject Py._Factor Py._Factor_positive

factorNegative :: TTerm Py.Factor -> TTerm Py.Factor
factorNegative = inject Py._Factor Py._Factor_negative

factorComplement :: TTerm Py.Factor -> TTerm Py.Factor
factorComplement = inject Py._Factor Py._Factor_complement

factorSimple :: TTerm Py.Power -> TTerm Py.Factor
factorSimple = inject Py._Factor Py._Factor_simple

-- =============================================================================
-- Power (record type)
-- =============================================================================

powerLhs :: TTerm Py.Power -> TTerm Py.AwaitPrimary
powerLhs p = project Py._Power Py._Power_lhs @@ p

powerRhs :: TTerm Py.Power -> TTerm (Maybe Py.Factor)
powerRhs p = project Py._Power Py._Power_rhs @@ p

-- =============================================================================
-- AwaitPrimary (record type)
-- =============================================================================

awaitPrimaryAwait :: TTerm Py.AwaitPrimary -> TTerm Bool
awaitPrimaryAwait ap = project Py._AwaitPrimary Py._AwaitPrimary_await @@ ap

awaitPrimaryPrimary :: TTerm Py.AwaitPrimary -> TTerm Py.Primary
awaitPrimaryPrimary ap = project Py._AwaitPrimary Py._AwaitPrimary_primary @@ ap

-- =============================================================================
-- Primary (union type)
-- =============================================================================

primarySimple :: TTerm Py.Atom -> TTerm Py.Primary
primarySimple = inject Py._Primary Py._Primary_simple

primaryCompound :: TTerm Py.PrimaryWithRhs -> TTerm Py.Primary
primaryCompound = inject Py._Primary Py._Primary_compound

-- =============================================================================
-- PrimaryWithRhs (record type)
-- =============================================================================

primaryWithRhsPrimary :: TTerm Py.PrimaryWithRhs -> TTerm Py.Primary
primaryWithRhsPrimary pwr = project Py._PrimaryWithRhs Py._PrimaryWithRhs_primary @@ pwr

primaryWithRhsRhs :: TTerm Py.PrimaryWithRhs -> TTerm Py.PrimaryRhs
primaryWithRhsRhs pwr = project Py._PrimaryWithRhs Py._PrimaryWithRhs_rhs @@ pwr

-- =============================================================================
-- PrimaryRhs (union type)
-- =============================================================================

primaryRhsCall :: TTerm Py.Args -> TTerm Py.PrimaryRhs
primaryRhsCall = inject Py._PrimaryRhs Py._PrimaryRhs_call

primaryRhsProject :: TTerm Py.Name -> TTerm Py.PrimaryRhs
primaryRhsProject = inject Py._PrimaryRhs Py._PrimaryRhs_project

primaryRhsSlices :: TTerm Py.Slices -> TTerm Py.PrimaryRhs
primaryRhsSlices = inject Py._PrimaryRhs Py._PrimaryRhs_slices

-- =============================================================================
-- Atom (union type)
-- =============================================================================

atomDict :: TTerm Py.Dict -> TTerm Py.Atom
atomDict = inject Py._Atom Py._Atom_dict

atomEllipsis :: TTerm Py.Atom
atomEllipsis = injectUnit Py._Atom Py._Atom_ellipsis

atomFalse :: TTerm Py.Atom
atomFalse = injectUnit Py._Atom Py._Atom_false

atomGroup :: TTerm Py.Group -> TTerm Py.Atom
atomGroup = inject Py._Atom Py._Atom_group

atomList :: TTerm Py.List -> TTerm Py.Atom
atomList = inject Py._Atom Py._Atom_list

atomName :: TTerm Py.Name -> TTerm Py.Atom
atomName = inject Py._Atom Py._Atom_name

atomNumber :: TTerm Py.Number -> TTerm Py.Atom
atomNumber = inject Py._Atom Py._Atom_number

atomSet :: TTerm Py.Set -> TTerm Py.Atom
atomSet = inject Py._Atom Py._Atom_set

atomString :: TTerm Py.String_ -> TTerm Py.Atom
atomString = inject Py._Atom Py._Atom_string

atomTrue :: TTerm Py.Atom
atomTrue = injectUnit Py._Atom Py._Atom_true

atomTuple :: TTerm Py.Tuple -> TTerm Py.Atom
atomTuple = inject Py._Atom Py._Atom_tuple

-- =============================================================================
-- Number (union type)
-- =============================================================================

numberInteger :: TTerm Integer -> TTerm Py.Number
numberInteger = inject Py._Number Py._Number_integer

numberFloat :: TTerm Double -> TTerm Py.Number
numberFloat = inject Py._Number Py._Number_float

-- =============================================================================
-- String_ (record type)
-- =============================================================================

string_ :: TTerm String -> TTerm Py.QuoteStyle -> TTerm Py.String_
string_ val style = record Py._String [
  Py._String_value>>: val,
  Py._String_quoteStyle>>: style]

stringValue :: TTerm Py.String_ -> TTerm String
stringValue s = project Py._String Py._String_value @@ s

stringQuoteStyle :: TTerm Py.String_ -> TTerm Py.QuoteStyle
stringQuoteStyle s = project Py._String Py._String_quoteStyle @@ s

-- =============================================================================
-- QuoteStyle (enum type)
-- =============================================================================

quoteStyleSingle :: TTerm Py.QuoteStyle
quoteStyleSingle = injectUnit Py._QuoteStyle Py._QuoteStyle_single

quoteStyleDouble :: TTerm Py.QuoteStyle
quoteStyleDouble = injectUnit Py._QuoteStyle Py._QuoteStyle_double

quoteStyleTriple :: TTerm Py.QuoteStyle
quoteStyleTriple = injectUnit Py._QuoteStyle Py._QuoteStyle_triple

-- =============================================================================
-- List (wrapper type)
-- =============================================================================

list_ :: TTerm [Py.StarNamedExpression] -> TTerm Py.List
list_ = wrap Py._List

unList :: TTerm Py.List -> TTerm [Py.StarNamedExpression]
unList l = unwrap Py._List @@ l

-- =============================================================================
-- Dict (wrapper type)
-- =============================================================================

dict :: TTerm [Py.DoubleStarredKvpair] -> TTerm Py.Dict
dict = wrap Py._Dict

unDict :: TTerm Py.Dict -> TTerm [Py.DoubleStarredKvpair]
unDict d = unwrap Py._Dict @@ d

-- =============================================================================
-- Tuple (wrapper type)
-- =============================================================================

tuple :: TTerm [Py.StarNamedExpression] -> TTerm Py.Tuple
tuple = wrap Py._Tuple

unTuple :: TTerm Py.Tuple -> TTerm [Py.StarNamedExpression]
unTuple t = unwrap Py._Tuple @@ t

-- =============================================================================
-- Set (wrapper type)
-- =============================================================================

set :: TTerm [Py.StarNamedExpression] -> TTerm Py.Set
set = wrap Py._Set

unSet :: TTerm Py.Set -> TTerm [Py.StarNamedExpression]
unSet s = unwrap Py._Set @@ s

-- =============================================================================
-- Kvpair (record type)
-- =============================================================================

kvpair :: TTerm Py.Expression -> TTerm Py.Expression -> TTerm Py.Kvpair
kvpair k v = record Py._Kvpair [
  Py._Kvpair_key>>: k,
  Py._Kvpair_value>>: v]

kvpairKey :: TTerm Py.Kvpair -> TTerm Py.Expression
kvpairKey kv = project Py._Kvpair Py._Kvpair_key @@ kv

kvpairValue :: TTerm Py.Kvpair -> TTerm Py.Expression
kvpairValue kv = project Py._Kvpair Py._Kvpair_value @@ kv

-- =============================================================================
-- DoubleStarredKvpair (union type)
-- =============================================================================

doubleStarredKvpairPair :: TTerm Py.Kvpair -> TTerm Py.DoubleStarredKvpair
doubleStarredKvpairPair = inject Py._DoubleStarredKvpair Py._DoubleStarredKvpair_pair

-- =============================================================================
-- Lambda (record type)
-- =============================================================================

-- | Construct a Lambda
lambda_ :: TTerm Py.LambdaParameters -> TTerm Py.Expression -> TTerm Py.Lambda
lambda_ params body = record Py._Lambda [
  Py._Lambda_params>>: params,
  Py._Lambda_body>>: body]

lambdaParameters :: TTerm Py.Lambda -> TTerm Py.LambdaParameters
lambdaParameters l = project Py._Lambda Py._Lambda_params @@ l

lambdaBody :: TTerm Py.Lambda -> TTerm Py.Expression
lambdaBody l = project Py._Lambda Py._Lambda_body @@ l

-- =============================================================================
-- LambdaParameters (record type)
-- =============================================================================

-- | Construct simple LambdaParameters with only paramNoDefault
lambdaParametersSimple :: TTerm [Py.LambdaParamNoDefault] -> TTerm Py.LambdaParameters
lambdaParametersSimple params = record Py._LambdaParameters [
  Py._LambdaParameters_slashNoDefault>>: nothing,
  Py._LambdaParameters_paramNoDefault>>: params,
  Py._LambdaParameters_paramWithDefault>>: list ([] :: [TTerm Py.LambdaParamWithDefault]),
  Py._LambdaParameters_starEtc>>: nothing]

-- | Construct empty LambdaParameters (for nullary lambdas)
lambdaParametersEmpty :: TTerm Py.LambdaParameters
lambdaParametersEmpty = record Py._LambdaParameters [
  Py._LambdaParameters_slashNoDefault>>: nothing,
  Py._LambdaParameters_paramNoDefault>>: list ([] :: [TTerm Py.LambdaParamNoDefault]),
  Py._LambdaParameters_paramWithDefault>>: list ([] :: [TTerm Py.LambdaParamWithDefault]),
  Py._LambdaParameters_starEtc>>: nothing]

lambdaParametersParamNoDefault :: TTerm Py.LambdaParameters -> TTerm [Py.LambdaParamNoDefault]
lambdaParametersParamNoDefault lp = project Py._LambdaParameters Py._LambdaParameters_paramNoDefault @@ lp

-- =============================================================================
-- LambdaParamNoDefault (wrapper type)
-- =============================================================================

-- | Wrap a Name into a LambdaParamNoDefault
lambdaParamNoDefault :: TTerm Py.Name -> TTerm Py.LambdaParamNoDefault
lambdaParamNoDefault = wrap Py._LambdaParamNoDefault

unLambdaParamNoDefault :: TTerm Py.LambdaParamNoDefault -> TTerm Py.Name
unLambdaParamNoDefault p = unwrap Py._LambdaParamNoDefault @@ p

-- =============================================================================
-- FunctionDefinition (record type)
-- =============================================================================

functionDefinitionDecorators :: TTerm Py.FunctionDefinition -> TTerm (Maybe Py.Decorators)
functionDefinitionDecorators fd = project Py._FunctionDefinition Py._FunctionDefinition_decorators @@ fd

functionDefinitionRaw :: TTerm Py.FunctionDefinition -> TTerm Py.FunctionDefRaw
functionDefinitionRaw fd = project Py._FunctionDefinition Py._FunctionDefinition_raw @@ fd

-- =============================================================================
-- FunctionDefRaw (record type)
-- =============================================================================

functionDefRawAsync :: TTerm Py.FunctionDefRaw -> TTerm Bool
functionDefRawAsync fdr = project Py._FunctionDefRaw Py._FunctionDefRaw_async @@ fdr

functionDefRawName :: TTerm Py.FunctionDefRaw -> TTerm Py.Name
functionDefRawName fdr = project Py._FunctionDefRaw Py._FunctionDefRaw_name @@ fdr

functionDefRawTypeParams :: TTerm Py.FunctionDefRaw -> TTerm [Py.TypeParameter]
functionDefRawTypeParams fdr = project Py._FunctionDefRaw Py._FunctionDefRaw_typeParams @@ fdr

functionDefRawParams :: TTerm Py.FunctionDefRaw -> TTerm (Maybe Py.Parameters)
functionDefRawParams fdr = project Py._FunctionDefRaw Py._FunctionDefRaw_params @@ fdr

functionDefRawReturnType :: TTerm Py.FunctionDefRaw -> TTerm (Maybe Py.Expression)
functionDefRawReturnType fdr = project Py._FunctionDefRaw Py._FunctionDefRaw_returnType @@ fdr

functionDefRawBlock :: TTerm Py.FunctionDefRaw -> TTerm Py.Block
functionDefRawBlock fdr = project Py._FunctionDefRaw Py._FunctionDefRaw_block @@ fdr

-- =============================================================================
-- Decorators (wrapper type)
-- =============================================================================

unDecorators :: TTerm Py.Decorators -> TTerm [Py.NamedExpression]
unDecorators d = unwrap Py._Decorators @@ d

-- =============================================================================
-- Parameters (union type)
-- =============================================================================

parametersParamNoDefault :: TTerm Py.ParamNoDefaultParameters -> TTerm Py.Parameters
parametersParamNoDefault = inject Py._Parameters Py._Parameters_paramNoDefault

-- =============================================================================
-- ParamNoDefaultParameters (record type)
-- =============================================================================

paramNoDefaultParametersParamNoDefault :: TTerm Py.ParamNoDefaultParameters -> TTerm [Py.ParamNoDefault]
paramNoDefaultParametersParamNoDefault p = project Py._ParamNoDefaultParameters Py._ParamNoDefaultParameters_paramNoDefault @@ p

-- =============================================================================
-- ParamNoDefault (record type)
-- =============================================================================

paramNoDefaultParam :: TTerm Py.ParamNoDefault -> TTerm Py.Param
paramNoDefaultParam p = project Py._ParamNoDefault Py._ParamNoDefault_param @@ p

-- =============================================================================
-- Param (record type)
-- =============================================================================

paramName :: TTerm Py.Param -> TTerm Py.Name
paramName p = project Py._Param Py._Param_name @@ p

paramAnnotation :: TTerm Py.Param -> TTerm (Maybe Py.Annotation)
paramAnnotation p = project Py._Param Py._Param_annotation @@ p

-- =============================================================================
-- Annotation (wrapper type)
-- =============================================================================

unAnnotation :: TTerm Py.Annotation -> TTerm Py.Expression
unAnnotation a = unwrap Py._Annotation @@ a

-- =============================================================================
-- Args (record type)
-- =============================================================================

argsPositional :: TTerm Py.Args -> TTerm [Py.PosArg]
argsPositional args = project Py._Args Py._Args_positional @@ args

-- =============================================================================
-- PosArg (union type)
-- =============================================================================

posArgExpression :: TTerm Py.Expression -> TTerm Py.PosArg
posArgExpression = inject Py._PosArg Py._PosArg_expression

-- =============================================================================
-- ClassDefinition (record type)
-- =============================================================================

classDefinitionDecorators :: TTerm Py.ClassDefinition -> TTerm (Maybe Py.Decorators)
classDefinitionDecorators cd = project Py._ClassDefinition Py._ClassDefinition_decorators @@ cd

classDefinitionName :: TTerm Py.ClassDefinition -> TTerm Py.Name
classDefinitionName cd = project Py._ClassDefinition Py._ClassDefinition_name @@ cd

classDefinitionArguments :: TTerm Py.ClassDefinition -> TTerm (Maybe Py.Args)
classDefinitionArguments cd = project Py._ClassDefinition Py._ClassDefinition_arguments @@ cd

classDefinitionBody :: TTerm Py.ClassDefinition -> TTerm Py.Block
classDefinitionBody cd = project Py._ClassDefinition Py._ClassDefinition_body @@ cd

-- =============================================================================
-- ImportStatement (union type)
-- =============================================================================

importStatementName :: TTerm Py.ImportName -> TTerm Py.ImportStatement
importStatementName = inject Py._ImportStatement Py._ImportStatement_name

importStatementFrom :: TTerm Py.ImportFrom -> TTerm Py.ImportStatement
importStatementFrom = inject Py._ImportStatement Py._ImportStatement_from

-- =============================================================================
-- ImportName (wrapper type)
-- =============================================================================

unImportName :: TTerm Py.ImportName -> TTerm [Py.DottedAsName]
unImportName n = unwrap Py._ImportName @@ n

-- =============================================================================
-- DottedAsName (record type)
-- =============================================================================

dottedAsNameName :: TTerm Py.DottedAsName -> TTerm Py.DottedName
dottedAsNameName dan = project Py._DottedAsName Py._DottedAsName_name @@ dan

-- =============================================================================
-- ImportFrom (record type)
-- =============================================================================

importFromDottedName :: TTerm Py.ImportFrom -> TTerm (Maybe Py.DottedName)
importFromDottedName if_ = project Py._ImportFrom Py._ImportFrom_dottedName @@ if_

importFromTargets :: TTerm Py.ImportFrom -> TTerm Py.ImportFromTargets
importFromTargets if_ = project Py._ImportFrom Py._ImportFrom_targets @@ if_

-- =============================================================================
-- ImportFromTargets (union type)
-- =============================================================================

importFromTargetsSimple :: TTerm [Py.ImportFromAsName] -> TTerm Py.ImportFromTargets
importFromTargetsSimple = inject Py._ImportFromTargets Py._ImportFromTargets_simple

importFromTargetsParens :: TTerm [Py.ImportFromAsName] -> TTerm Py.ImportFromTargets
importFromTargetsParens = inject Py._ImportFromTargets Py._ImportFromTargets_parens

importFromTargetsStar :: TTerm Py.ImportFromTargets
importFromTargetsStar = injectUnit Py._ImportFromTargets Py._ImportFromTargets_star

-- =============================================================================
-- ImportFromAsName (record type)
-- =============================================================================

importFromAsNameName :: TTerm Py.ImportFromAsName -> TTerm Py.Name
importFromAsNameName ifan = project Py._ImportFromAsName Py._ImportFromAsName_name @@ ifan

-- =============================================================================
-- Assignment (union type)
-- =============================================================================

assignmentTyped :: TTerm Py.TypedAssignment -> TTerm Py.Assignment
assignmentTyped = inject Py._Assignment Py._Assignment_typed

assignmentUntyped :: TTerm Py.UntypedAssignment -> TTerm Py.Assignment
assignmentUntyped = inject Py._Assignment Py._Assignment_untyped

-- =============================================================================
-- TypedAssignment (record type)
-- =============================================================================

typedAssignmentLhs :: TTerm Py.TypedAssignment -> TTerm Py.SingleTarget
typedAssignmentLhs ta = project Py._TypedAssignment Py._TypedAssignment_lhs @@ ta

typedAssignmentType :: TTerm Py.TypedAssignment -> TTerm Py.Expression
typedAssignmentType ta = project Py._TypedAssignment Py._TypedAssignment_type @@ ta

typedAssignmentRhs :: TTerm Py.TypedAssignment -> TTerm (Maybe Py.AnnotatedRhs)
typedAssignmentRhs ta = project Py._TypedAssignment Py._TypedAssignment_rhs @@ ta

-- =============================================================================
-- UntypedAssignment (record type)
-- =============================================================================

untypedAssignmentTargets :: TTerm Py.UntypedAssignment -> TTerm [Py.StarTarget]
untypedAssignmentTargets ua = project Py._UntypedAssignment Py._UntypedAssignment_targets @@ ua

untypedAssignmentRhs :: TTerm Py.UntypedAssignment -> TTerm Py.AnnotatedRhs
untypedAssignmentRhs ua = project Py._UntypedAssignment Py._UntypedAssignment_rhs @@ ua

-- =============================================================================
-- SingleTarget (union type)
-- =============================================================================

singleTargetName :: TTerm Py.Name -> TTerm Py.SingleTarget
singleTargetName = inject Py._SingleTarget Py._SingleTarget_name

-- =============================================================================
-- StarTarget (union type)
-- =============================================================================

starTargetUnstarred :: TTerm Py.TargetWithStarAtom -> TTerm Py.StarTarget
starTargetUnstarred = inject Py._StarTarget Py._StarTarget_unstarred

-- =============================================================================
-- TargetWithStarAtom (union type)
-- =============================================================================

targetWithStarAtomAtom :: TTerm Py.StarAtom -> TTerm Py.TargetWithStarAtom
targetWithStarAtomAtom = inject Py._TargetWithStarAtom Py._TargetWithStarAtom_atom

-- =============================================================================
-- StarAtom (union type)
-- =============================================================================

starAtomName :: TTerm Py.Name -> TTerm Py.StarAtom
starAtomName = inject Py._StarAtom Py._StarAtom_name

-- =============================================================================
-- AnnotatedRhs (union type)
-- =============================================================================

annotatedRhsStar :: TTerm [Py.StarExpression] -> TTerm Py.AnnotatedRhs
annotatedRhsStar = inject Py._AnnotatedRhs Py._AnnotatedRhs_star

-- =============================================================================
-- MatchStatement (record type)
-- =============================================================================

matchStatementSubject :: TTerm Py.MatchStatement -> TTerm Py.SubjectExpression
matchStatementSubject ms = project Py._MatchStatement Py._MatchStatement_subject @@ ms

matchStatementCases :: TTerm Py.MatchStatement -> TTerm [Py.CaseBlock]
matchStatementCases ms = project Py._MatchStatement Py._MatchStatement_cases @@ ms

-- =============================================================================
-- SubjectExpression (union type)
-- =============================================================================

subjectExpressionSimple :: TTerm Py.NamedExpression -> TTerm Py.SubjectExpression
subjectExpressionSimple = inject Py._SubjectExpression Py._SubjectExpression_simple

-- =============================================================================
-- CaseBlock (record type)
-- =============================================================================

caseBlockPatterns :: TTerm Py.CaseBlock -> TTerm Py.Patterns
caseBlockPatterns cb = project Py._CaseBlock Py._CaseBlock_patterns @@ cb

caseBlockGuard :: TTerm Py.CaseBlock -> TTerm (Maybe Py.Guard)
caseBlockGuard cb = project Py._CaseBlock Py._CaseBlock_guard @@ cb

caseBlockBody :: TTerm Py.CaseBlock -> TTerm Py.Block
caseBlockBody cb = project Py._CaseBlock Py._CaseBlock_body @@ cb

-- =============================================================================
-- Guard (wrapper type)
-- =============================================================================

guard :: TTerm Py.NamedExpression -> TTerm Py.Guard
guard = wrap Py._Guard

unGuard :: TTerm Py.Guard -> TTerm Py.NamedExpression
unGuard g = unwrap Py._Guard @@ g

-- =============================================================================
-- Patterns (union type)
-- =============================================================================

patternsPattern :: TTerm Py.Pattern -> TTerm Py.Patterns
patternsPattern = inject Py._Patterns Py._Patterns_pattern

-- =============================================================================
-- Pattern (union type)
-- =============================================================================

patternOr :: TTerm Py.OrPattern -> TTerm Py.Pattern
patternOr = inject Py._Pattern Py._Pattern_or

-- =============================================================================
-- OrPattern (wrapper type)
-- =============================================================================

unOrPattern :: TTerm Py.OrPattern -> TTerm [Py.ClosedPattern]
unOrPattern op = unwrap Py._OrPattern @@ op

-- =============================================================================
-- ClosedPattern (union type)
-- =============================================================================

closedPatternCapture :: TTerm Py.CapturePattern -> TTerm Py.ClosedPattern
closedPatternCapture = inject Py._ClosedPattern Py._ClosedPattern_capture

closedPatternWildcard :: TTerm Py.ClosedPattern
closedPatternWildcard = injectUnit Py._ClosedPattern Py._ClosedPattern_wildcard

closedPatternValue :: TTerm Py.ValuePattern -> TTerm Py.ClosedPattern
closedPatternValue = inject Py._ClosedPattern Py._ClosedPattern_value

closedPatternClass :: TTerm Py.ClassPattern -> TTerm Py.ClosedPattern
closedPatternClass = inject Py._ClosedPattern Py._ClosedPattern_class

-- =============================================================================
-- CapturePattern (wrapper type)
-- =============================================================================

capturePattern :: TTerm Py.PatternCaptureTarget -> TTerm Py.CapturePattern
capturePattern = wrap Py._CapturePattern

unCapturePattern :: TTerm Py.CapturePattern -> TTerm Py.PatternCaptureTarget
unCapturePattern cp = unwrap Py._CapturePattern @@ cp

-- =============================================================================
-- PatternCaptureTarget (wrapper type)
-- =============================================================================

patternCaptureTarget :: TTerm Py.Name -> TTerm Py.PatternCaptureTarget
patternCaptureTarget = wrap Py._PatternCaptureTarget

unPatternCaptureTarget :: TTerm Py.PatternCaptureTarget -> TTerm Py.Name
unPatternCaptureTarget pct = unwrap Py._PatternCaptureTarget @@ pct

-- =============================================================================
-- ValuePattern (wrapper type)
-- =============================================================================

valuePattern :: TTerm Py.Attribute -> TTerm Py.ValuePattern
valuePattern = wrap Py._ValuePattern

unValuePattern :: TTerm Py.ValuePattern -> TTerm Py.Attribute
unValuePattern vp = unwrap Py._ValuePattern @@ vp

-- =============================================================================
-- Attribute (wrapper type)
-- =============================================================================

attribute :: TTerm [Py.Name] -> TTerm Py.Attribute
attribute = wrap Py._Attribute

unAttribute :: TTerm Py.Attribute -> TTerm [Py.Name]
unAttribute a = unwrap Py._Attribute @@ a

-- =============================================================================
-- ClassPattern (record type)
-- =============================================================================

classPatternNameOrAttribute :: TTerm Py.ClassPattern -> TTerm Py.NameOrAttribute
classPatternNameOrAttribute cp = project Py._ClassPattern Py._ClassPattern_nameOrAttribute @@ cp

-- =============================================================================
-- NameOrAttribute (wrapper type)
-- =============================================================================

unNameOrAttribute :: TTerm Py.NameOrAttribute -> TTerm [Py.Name]
unNameOrAttribute noa = unwrap Py._NameOrAttribute @@ noa

-- =============================================================================
-- TypeAlias (record type)
-- =============================================================================

typeAliasName :: TTerm Py.TypeAlias -> TTerm Py.Name
typeAliasName ta = project Py._TypeAlias Py._TypeAlias_name @@ ta

typeAliasTypeParams :: TTerm Py.TypeAlias -> TTerm [Py.TypeParameter]
typeAliasTypeParams ta = project Py._TypeAlias Py._TypeAlias_typeParams @@ ta

typeAliasExpression :: TTerm Py.TypeAlias -> TTerm Py.Expression
typeAliasExpression ta = project Py._TypeAlias Py._TypeAlias_expression @@ ta

-- =============================================================================
-- TypeParameter (union type)
-- =============================================================================

typeParameterSimple :: TTerm Py.SimpleTypeParameter -> TTerm Py.TypeParameter
typeParameterSimple = inject Py._TypeParameter Py._TypeParameter_simple

-- =============================================================================
-- SimpleTypeParameter (record type)
-- =============================================================================

simpleTypeParameterName :: TTerm Py.SimpleTypeParameter -> TTerm Py.Name
simpleTypeParameterName stp = project Py._SimpleTypeParameter Py._SimpleTypeParameter_name @@ stp

-- =============================================================================
-- Slices (record type)
-- =============================================================================

slicesHead :: TTerm Py.Slices -> TTerm Py.Slice
slicesHead s = project Py._Slices Py._Slices_head @@ s

slicesTail :: TTerm Py.Slices -> TTerm [Py.SliceOrStarredExpression]
slicesTail s = project Py._Slices Py._Slices_tail @@ s

-- =============================================================================
-- Slice (union type)
-- =============================================================================

sliceNamed :: TTerm Py.NamedExpression -> TTerm Py.Slice
sliceNamed = inject Py._Slice Py._Slice_named

-- =============================================================================
-- SliceOrStarredExpression (union type)
-- =============================================================================

sliceOrStarredExpressionSlice :: TTerm Py.Slice -> TTerm Py.SliceOrStarredExpression
sliceOrStarredExpressionSlice = inject Py._SliceOrStarredExpression Py._SliceOrStarredExpression_slice

sliceOrStarredExpressionStarred :: TTerm Py.StarredExpression -> TTerm Py.SliceOrStarredExpression
sliceOrStarredExpressionStarred = inject Py._SliceOrStarredExpression Py._SliceOrStarredExpression_starred

-- =============================================================================
-- StarredExpression (wrapper type)
-- =============================================================================

starredExpression :: TTerm Py.Expression -> TTerm Py.StarredExpression
starredExpression = wrap Py._StarredExpression

unStarredExpression :: TTerm Py.StarredExpression -> TTerm Py.Expression
unStarredExpression se = unwrap Py._StarredExpression @@ se

-- =============================================================================
-- StarExpression (union type)
-- =============================================================================

starExpressionSimple :: TTerm Py.Expression -> TTerm Py.StarExpression
starExpressionSimple = inject Py._StarExpression Py._StarExpression_simple

-- =============================================================================
-- StarNamedExpression (union type)
-- =============================================================================

starNamedExpressionStar :: TTerm Py.BitwiseOr -> TTerm Py.StarNamedExpression
starNamedExpressionStar = inject Py._StarNamedExpression Py._StarNamedExpression_star

starNamedExpressionSimple :: TTerm Py.NamedExpression -> TTerm Py.StarNamedExpression
starNamedExpressionSimple = inject Py._StarNamedExpression Py._StarNamedExpression_simple

-- =============================================================================
-- Group (union type)
-- =============================================================================

groupExpression :: TTerm Py.NamedExpression -> TTerm Py.Group
groupExpression = inject Py._Group Py._Group_expression

-- =============================================================================
-- ReturnStatement (wrapper type)
-- =============================================================================

unReturnStatement :: TTerm Py.ReturnStatement -> TTerm [Py.StarExpression]
unReturnStatement rs = unwrap Py._ReturnStatement @@ rs

-- =============================================================================
-- RaiseStatement (wrapper type)
-- =============================================================================

raiseStatement :: TTerm (Maybe Py.RaiseExpression) -> TTerm Py.RaiseStatement
raiseStatement = wrap Py._RaiseStatement

unRaiseStatement :: TTerm Py.RaiseStatement -> TTerm (Maybe Py.RaiseExpression)
unRaiseStatement rs = unwrap Py._RaiseStatement @@ rs

-- =============================================================================
-- RaiseExpression (record type)
-- =============================================================================

raiseExpressionException :: TTerm Py.RaiseExpression -> TTerm Py.Expression
raiseExpressionException re = project Py._RaiseExpression Py._RaiseExpression_expression @@ re

-- =============================================================================
-- Conversion helpers (composing multiple constructors)
-- =============================================================================

-- | Convert a Python Name to a Primary (Name -> Atom -> Primary)
pyNameToPyPrimary :: TTerm Py.Name -> TTerm Py.Primary
pyNameToPyPrimary n = primarySimple (atomName n)

-- | Convert a Python Primary to an AwaitPrimary
pyPrimaryToPyAwaitPrimary :: TTerm Py.Primary -> TTerm Py.AwaitPrimary
pyPrimaryToPyAwaitPrimary p = record Py._AwaitPrimary [
  Py._AwaitPrimary_await>>: false,
  Py._AwaitPrimary_primary>>: p]

-- | Convert a Python Primary to a Power
pyPrimaryToPyPower :: TTerm Py.Primary -> TTerm Py.Power
pyPrimaryToPyPower p = record Py._Power [
  Py._Power_lhs>>: pyPrimaryToPyAwaitPrimary p,
  Py._Power_rhs>>: nothing]

-- | Convert a Python Primary to a Factor
pyPrimaryToPyFactor :: TTerm Py.Primary -> TTerm Py.Factor
pyPrimaryToPyFactor p = inject Py._Factor Py._Factor_simple (pyPrimaryToPyPower p)

-- | Convert a Python Primary to a Term
pyPrimaryToPyTerm :: TTerm Py.Primary -> TTerm Py.Term
pyPrimaryToPyTerm p = record Py._Term [
  Py._Term_lhs>>: nothing,
  Py._Term_rhs>>: pyPrimaryToPyFactor p]

-- | Convert a Python Primary to a Sum
pyPrimaryToPySum :: TTerm Py.Primary -> TTerm Py.Sum
pyPrimaryToPySum p = record Py._Sum [
  Py._Sum_lhs>>: nothing,
  Py._Sum_rhs>>: pyPrimaryToPyTerm p]

-- | Convert a Python Primary to a ShiftExpression
pyPrimaryToPyShiftExpression :: TTerm Py.Primary -> TTerm Py.ShiftExpression
pyPrimaryToPyShiftExpression p = record Py._ShiftExpression [
  Py._ShiftExpression_lhs>>: nothing,
  Py._ShiftExpression_rhs>>: pyPrimaryToPySum p]

-- | Convert a Python Primary to a BitwiseAnd
pyPrimaryToPyBitwiseAnd :: TTerm Py.Primary -> TTerm Py.BitwiseAnd
pyPrimaryToPyBitwiseAnd p = record Py._BitwiseAnd [
  Py._BitwiseAnd_lhs>>: nothing,
  Py._BitwiseAnd_rhs>>: pyPrimaryToPyShiftExpression p]

-- | Convert a Python Primary to a BitwiseXor
pyPrimaryToPyBitwiseXor :: TTerm Py.Primary -> TTerm Py.BitwiseXor
pyPrimaryToPyBitwiseXor p = record Py._BitwiseXor [
  Py._BitwiseXor_lhs>>: nothing,
  Py._BitwiseXor_rhs>>: pyPrimaryToPyBitwiseAnd p]

-- | Convert a Python Primary to a BitwiseOr
pyPrimaryToPyBitwiseOr :: TTerm Py.Primary -> TTerm Py.BitwiseOr
pyPrimaryToPyBitwiseOr p = record Py._BitwiseOr [
  Py._BitwiseOr_lhs>>: nothing,
  Py._BitwiseOr_rhs>>: pyPrimaryToPyBitwiseXor p]

-- | Convert a Python Primary to a Comparison
pyPrimaryToPyComparison :: TTerm Py.Primary -> TTerm Py.Comparison
pyPrimaryToPyComparison p = record Py._Comparison [
  Py._Comparison_lhs>>: pyPrimaryToPyBitwiseOr p,
  Py._Comparison_rhs>>: (list ([] :: [TTerm Py.CompareOpBitwiseOrPair]))]

-- | Convert a Python Primary to an Inversion
pyPrimaryToPyInversion :: TTerm Py.Primary -> TTerm Py.Inversion
pyPrimaryToPyInversion p = inject Py._Inversion Py._Inversion_simple (pyPrimaryToPyComparison p)

-- | Convert a Python Primary to a Conjunction
pyPrimaryToPyConjunction :: TTerm Py.Primary -> TTerm Py.Conjunction
pyPrimaryToPyConjunction p = wrap Py._Conjunction (list [pyPrimaryToPyInversion p])

-- | Convert a Python Primary to a Disjunction
pyPrimaryToPyDisjunction :: TTerm Py.Primary -> TTerm Py.Disjunction
pyPrimaryToPyDisjunction p = wrap Py._Disjunction (list [pyPrimaryToPyConjunction p])

-- | Convert a Python Primary to an Expression
pyPrimaryToPyExpression :: TTerm Py.Primary -> TTerm Py.Expression
pyPrimaryToPyExpression p = inject Py._Expression Py._Expression_simple (pyPrimaryToPyDisjunction p)

-- | Convert a Python Name to an Expression (Name -> Atom -> Primary -> ... -> Expression)
pyNameToPyExpression :: TTerm Py.Name -> TTerm Py.Expression
pyNameToPyExpression n = pyPrimaryToPyExpression (pyNameToPyPrimary n)

-- | Convert a Python String to an Expression
pyStringToPyExpression :: TTerm Py.String_ -> TTerm Py.Expression
pyStringToPyExpression s = pyPrimaryToPyExpression (primarySimple (atomString s))

-- | Create a double-quoted Python String_ from a string
doubleQuotedString :: TTerm String -> TTerm Py.String_
doubleQuotedString val = string_ val quoteStyleDouble

-- =============================================================================
-- ReturnStatement (wrapper type) - constructor
-- =============================================================================

-- | Wrap star expressions into a ReturnStatement
returnStatement :: TTerm [Py.StarExpression] -> TTerm Py.ReturnStatement
returnStatement = wrap Py._ReturnStatement

-- =============================================================================
-- Slices (record type) - constructor
-- =============================================================================

-- | Construct Slices
slices :: TTerm Py.Slice -> TTerm [Py.SliceOrStarredExpression] -> TTerm Py.Slices
slices hd tl = record Py._Slices [
  Py._Slices_head>>: hd,
  Py._Slices_tail>>: tl]

-- =============================================================================
-- Slice (union type) - additional constructors
-- =============================================================================

-- | Inject a SliceExpression into Slice
sliceSlice :: TTerm Py.SliceExpression -> TTerm Py.Slice
sliceSlice = inject Py._Slice Py._Slice_slice

-- =============================================================================
-- RaiseExpression (record type) - constructor
-- =============================================================================

-- | Construct a RaiseExpression
raiseExpression :: TTerm Py.Expression -> TTerm (Maybe Py.Expression) -> TTerm Py.RaiseExpression
raiseExpression expr from = record Py._RaiseExpression [
  Py._RaiseExpression_expression>>: expr,
  Py._RaiseExpression_from>>: from]

-- =============================================================================
-- TypeAlias (record type)
-- =============================================================================

-- | Construct a TypeAlias
typeAlias :: TTerm Py.Name -> TTerm [Py.TypeParameter] -> TTerm Py.Expression -> TTerm Py.TypeAlias
typeAlias n tparams expr = record Py._TypeAlias [
  Py._TypeAlias_name>>: n,
  Py._TypeAlias_typeParams>>: tparams,
  Py._TypeAlias_expression>>: expr]

-- =============================================================================
-- UntypedAssignment (record type) - constructor
-- =============================================================================

-- | Construct an UntypedAssignment
untypedAssignment :: TTerm [Py.StarTarget] -> TTerm Py.AnnotatedRhs -> TTerm (Maybe String) -> TTerm Py.UntypedAssignment
untypedAssignment targets rhs typeComment = record Py._UntypedAssignment [
  Py._UntypedAssignment_targets>>: targets,
  Py._UntypedAssignment_rhs>>: rhs,
  Py._UntypedAssignment_typeComment>>: typeComment]

-- | Construct a simple UntypedAssignment (no type comment)
untypedAssignmentSimple :: TTerm [Py.StarTarget] -> TTerm Py.AnnotatedRhs -> TTerm Py.UntypedAssignment
untypedAssignmentSimple targets rhs = untypedAssignment targets rhs nothing

-- =============================================================================
-- TypedAssignment (record type) - constructor
-- =============================================================================

-- | Construct a TypedAssignment
typedAssignment :: TTerm Py.SingleTarget -> TTerm Py.Expression -> TTerm (Maybe Py.AnnotatedRhs) -> TTerm Py.TypedAssignment
typedAssignment lhs typ rhs = record Py._TypedAssignment [
  Py._TypedAssignment_lhs>>: lhs,
  Py._TypedAssignment_type>>: typ,
  Py._TypedAssignment_rhs>>: rhs]

-- =============================================================================
-- Annotation (newtype wrapper)
-- =============================================================================

-- | Wrap an expression as an Annotation
annotation :: TTerm Py.Expression -> TTerm Py.Annotation
annotation = wrap Py._Annotation

-- =============================================================================
-- Kwarg (record type)
-- =============================================================================

-- | Construct a Kwarg
kwarg :: TTerm Py.Name -> TTerm Py.Expression -> TTerm Py.Kwarg
kwarg n expr = record Py._Kwarg [
  Py._Kwarg_name>>: n,
  Py._Kwarg_value>>: expr]

-- =============================================================================
-- KwargOrStarred (union type)
-- =============================================================================

-- | Inject a Kwarg into KwargOrStarred
kwargOrStarredKwarg :: TTerm Py.Kwarg -> TTerm Py.KwargOrStarred
kwargOrStarredKwarg = inject Py._KwargOrStarred Py._KwargOrStarred_kwarg

-- =============================================================================
-- PrimaryWithRhs (record type) - constructor
-- =============================================================================

-- | Construct a PrimaryWithRhs
primaryWithRhs :: TTerm Py.Primary -> TTerm Py.PrimaryRhs -> TTerm Py.PrimaryWithRhs
primaryWithRhs prim rhs = record Py._PrimaryWithRhs [
  Py._PrimaryWithRhs_primary>>: prim,
  Py._PrimaryWithRhs_rhs>>: rhs]

-- =============================================================================
-- ClassDefinition (record type) - constructor
-- =============================================================================

-- | Construct a ClassDefinition
classDefinition :: TTerm (Maybe Py.Decorators) -> TTerm Py.Name -> TTerm [Py.TypeParameter]
                -> TTerm (Maybe Py.Args) -> TTerm Py.Block -> TTerm Py.ClassDefinition
classDefinition decs n tparams args body = record Py._ClassDefinition [
  Py._ClassDefinition_decorators>>: decs,
  Py._ClassDefinition_name>>: n,
  Py._ClassDefinition_typeParams>>: tparams,
  Py._ClassDefinition_arguments>>: args,
  Py._ClassDefinition_body>>: body]

-- =============================================================================
-- Args (record type) - constructor
-- =============================================================================

-- | Construct Args with positional arguments only
argsPositionalOnly :: TTerm [Py.PosArg] -> TTerm Py.Args
argsPositionalOnly pos = record Py._Args [
  Py._Args_positional>>: pos,
  Py._Args_kwargOrStarred>>: list ([] :: [TTerm Py.KwargOrStarred]),
  Py._Args_kwargOrDoubleStarred>>: list ([] :: [TTerm Py.KwargOrDoubleStarred])]

-- | Construct Args with all fields
args :: TTerm [Py.PosArg] -> TTerm [Py.KwargOrStarred] -> TTerm [Py.KwargOrDoubleStarred] -> TTerm Py.Args
args pos kwargOrStarred kwargOrDoubleStarred = record Py._Args [
  Py._Args_positional>>: pos,
  Py._Args_kwargOrStarred>>: kwargOrStarred,
  Py._Args_kwargOrDoubleStarred>>: kwargOrDoubleStarred]

-- =============================================================================
-- ParamNoDefault (record type) - constructor
-- =============================================================================

-- | Construct a ParamNoDefault
-- | Construct a ParamNoDefault with param and optional type comment
paramNoDefault :: TTerm Py.Param -> TTerm (Maybe Py.TypeComment) -> TTerm Py.ParamNoDefault
paramNoDefault p tc = record Py._ParamNoDefault [
  Py._ParamNoDefault_param>>: p,
  Py._ParamNoDefault_typeComment>>: tc]

-- | Construct a simple ParamNoDefault (no type comment)
paramNoDefaultSimple :: TTerm Py.Param -> TTerm Py.ParamNoDefault
paramNoDefaultSimple p = paramNoDefault p nothing

-- =============================================================================
-- Param (record type) - constructor
-- =============================================================================

-- | Construct a Param with name and optional annotation
param :: TTerm Py.Name -> TTerm (Maybe Py.Annotation) -> TTerm Py.Param
param n ann = record Py._Param [
  Py._Param_name>>: n,
  Py._Param_annotation>>: ann]

-- | Construct a Param with just a name (no annotation)
paramSimple :: TTerm Py.Name -> TTerm Py.Param
paramSimple n = param n nothing

-- =============================================================================
-- ParamNoDefaultParameters (record type) - constructor
-- =============================================================================

-- | Construct ParamNoDefaultParameters
paramNoDefaultParameters :: TTerm [Py.ParamNoDefault] -> TTerm [Py.ParamWithDefault] -> TTerm (Maybe Py.StarEtc) -> TTerm Py.ParamNoDefaultParameters
paramNoDefaultParameters noDefault withDefault starEtc = record Py._ParamNoDefaultParameters [
  Py._ParamNoDefaultParameters_paramNoDefault>>: noDefault,
  Py._ParamNoDefaultParameters_paramWithDefault>>: withDefault,
  Py._ParamNoDefaultParameters_starEtc>>: starEtc]

-- | Construct simple ParamNoDefaultParameters (no defaults, no star)
paramNoDefaultParametersSimple :: TTerm [Py.ParamNoDefault] -> TTerm Py.ParamNoDefaultParameters
paramNoDefaultParametersSimple noDefault = paramNoDefaultParameters noDefault (list ([] :: [TTerm Py.ParamWithDefault])) nothing

-- =============================================================================
-- SimpleTypeParameter (record type) - constructor
-- =============================================================================

-- | Construct a SimpleTypeParameter
simpleTypeParameter :: TTerm Py.Name -> TTerm (Maybe Py.Expression) -> TTerm (Maybe Py.Expression) -> TTerm Py.SimpleTypeParameter
simpleTypeParameter n bound dflt = record Py._SimpleTypeParameter [
  Py._SimpleTypeParameter_name>>: n,
  Py._SimpleTypeParameter_bound>>: bound,
  Py._SimpleTypeParameter_default>>: dflt]

-- | Construct a simple SimpleTypeParameter (no bound or default)
simpleTypeParameterSimple :: TTerm Py.Name -> TTerm Py.SimpleTypeParameter
simpleTypeParameterSimple n = simpleTypeParameter n nothing nothing

-- =============================================================================
-- OrPattern (wrapper type) - constructor
-- =============================================================================

-- | Wrap closed patterns into an OrPattern
orPattern :: TTerm [Py.ClosedPattern] -> TTerm Py.OrPattern
orPattern = wrap Py._OrPattern

-- =============================================================================
-- CaseBlock (record type) - constructor
-- =============================================================================

-- | Construct a CaseBlock
caseBlock :: TTerm Py.Patterns -> TTerm (Maybe Py.Guard) -> TTerm Py.Block -> TTerm Py.CaseBlock
caseBlock patterns guard body = record Py._CaseBlock [
  Py._CaseBlock_patterns>>: patterns,
  Py._CaseBlock_guard>>: guard,
  Py._CaseBlock_body>>: body]

-- =============================================================================
-- ClassPattern (record type) - constructors
-- =============================================================================

-- | Construct a simple ClassPattern (no positional or keyword patterns)
classPatternSimple :: TTerm Py.NameOrAttribute -> TTerm Py.ClassPattern
classPatternSimple nameOrAttr = record Py._ClassPattern [
  Py._ClassPattern_nameOrAttribute>>: nameOrAttr,
  Py._ClassPattern_positionalPatterns>>: nothing,
  Py._ClassPattern_keywordPatterns>>: nothing]

-- | Construct a ClassPattern with keyword patterns
classPatternWithKeywords :: TTerm Py.NameOrAttribute -> TTerm Py.KeywordPatterns -> TTerm Py.ClassPattern
classPatternWithKeywords nameOrAttr kwPatterns = record Py._ClassPattern [
  Py._ClassPattern_nameOrAttribute>>: nameOrAttr,
  Py._ClassPattern_positionalPatterns>>: nothing,
  Py._ClassPattern_keywordPatterns>>: just kwPatterns]

-- =============================================================================
-- KeywordPattern (record type) - constructor
-- =============================================================================

-- | Construct a KeywordPattern
keywordPattern :: TTerm Py.Name -> TTerm Py.Pattern -> TTerm Py.KeywordPattern
keywordPattern n pattern = record Py._KeywordPattern [
  Py._KeywordPattern_name>>: n,
  Py._KeywordPattern_pattern>>: pattern]

-- =============================================================================
-- KeywordPatterns (wrapper type) - constructor
-- =============================================================================

-- | Wrap keyword patterns list into KeywordPatterns
keywordPatterns :: TTerm [Py.KeywordPattern] -> TTerm Py.KeywordPatterns
keywordPatterns = wrap Py._KeywordPatterns

-- =============================================================================
-- NameOrAttribute (wrapper type) - constructor
-- =============================================================================

-- | Wrap names into NameOrAttribute
nameOrAttribute :: TTerm [Py.Name] -> TTerm Py.NameOrAttribute
nameOrAttribute = wrap Py._NameOrAttribute

-- =============================================================================
-- Pattern (union type) - additional constructor
-- =============================================================================

-- | Inject a ClosedPattern into Pattern (as-pattern)
patternAs :: TTerm Py.AsPattern -> TTerm Py.Pattern
patternAs = inject Py._Pattern Py._Pattern_as

