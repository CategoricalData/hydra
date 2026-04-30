-- | Python serializer: converts Python AST to concrete syntax (source code).
-- Serializes the Python syntax model (Hydra.Python.Syntax) into properly formatted Python source code.

module Hydra.Sources.Python.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.Python.Syntax as Py
import qualified Hydra.Sources.Python.Syntax as PySyntax


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.python.serde"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Constants.ns, Serialization.ns],
            moduleTypeDependencies = (PySyntax.ns:KernelTypes.kernelTypesNamespaces),
            moduleDescription = Just "Python serializer: converts Python AST to concrete syntax"}
  where
    definitions = [
      toDefinition annotatedRhsToExpr,
      toDefinition annotatedStatementToExpr,
      toDefinition annotationToExpr,
      toDefinition argsToExpr,
      toDefinition assignmentToExpr,
      toDefinition assignmentExpressionToExpr,
      toDefinition atomToExpr,
      toDefinition attributeToExpr,
      toDefinition awaitPrimaryToExpr,
      toDefinition bitwiseAndToExpr,
      toDefinition bitwiseOrToExpr,
      toDefinition bitwiseXorToExpr,
      toDefinition blockToExpr,
      toDefinition capturePatternToExpr,
      toDefinition caseBlockToExpr,
      toDefinition classDefinitionToExpr,
      toDefinition classPatternToExpr,
      toDefinition closedPatternToExpr,
      toDefinition comparisonToExpr,
      toDefinition compoundStatementToExpr,
      toDefinition conditionalToExpr,
      toDefinition conjunctionToExpr,
      toDefinition decoratorsToExpr,
      toDefinition dictToExpr,
      toDefinition disjunctionToExpr,
      toDefinition dottedAsNameToExpr,
      toDefinition dottedNameToExpr,
      toDefinition doubleStarredKvpairToExpr,
      toDefinition expressionToExpr,
      toDefinition factorToExpr,
      toDefinition functionDefRawToExpr,
      toDefinition functionDefinitionToExpr,
      toDefinition groupToExpr,
      toDefinition guardToExpr,
      toDefinition importFromToExpr,
      toDefinition importFromAsNameToExpr,
      toDefinition importFromTargetsToExpr,
      toDefinition importNameToExpr,
      toDefinition importStatementToExpr,
      toDefinition inversionToExpr,
      toDefinition keywordPatternToExpr,
      toDefinition keywordPatternsToExpr,
      toDefinition kvpairToExpr,
      toDefinition kwargToExpr,
      toDefinition kwargOrDoubleStarredToExpr,
      toDefinition kwargOrStarredToExpr,
      toDefinition lambdaToExpr,
      toDefinition lambdaParamNoDefaultToExpr,
      toDefinition lambdaParametersToExpr,
      toDefinition lambdaStarEtcToExpr,
      toDefinition listToExpr,
      toDefinition matchStatementToExpr,
      toDefinition moduleToExpr,
      toDefinition nameToExpr,
      toDefinition nameOrAttributeToExpr,
      toDefinition namedExpressionToExpr,
      toDefinition numberToExpr,
      toDefinition orPatternToExpr,
      toDefinition paramToExpr,
      toDefinition paramNoDefaultToExpr,
      toDefinition paramNoDefaultParametersToExpr,
      toDefinition parametersToExpr,
      toDefinition patternToExpr,
      toDefinition patternCaptureTargetToExpr,
      toDefinition patternsToExpr,
      toDefinition posArgToExpr,
      toDefinition positionalPatternsToExpr,
      toDefinition powerToExpr,
      toDefinition primaryToExpr,
      toDefinition primaryRhsToExpr,
      toDefinition primaryWithRhsToExpr,
      toDefinition raiseExpressionToExpr,
      toDefinition raiseStatementToExpr,
      toDefinition relativeImportPrefixToExpr,
      toDefinition returnStatementToExpr,
      toDefinition setToExpr,
      toDefinition shiftExpressionToExpr,
      toDefinition simpleStatementToExpr,
      toDefinition simpleTypeParameterToExpr,
      toDefinition singleTargetToExpr,
      toDefinition sliceToExpr,
      toDefinition sliceOrStarredExpressionToExpr,
      toDefinition slicesToExpr,
      toDefinition starAtomToExpr,
      toDefinition starExpressionToExpr,
      toDefinition starNamedExpressionToExpr,
      toDefinition starTargetToExpr,
      toDefinition starredExpressionToExpr,
      toDefinition statementToExpr,
      toDefinition stringToExpr,
      toDefinition subjectExpressionToExpr,
      toDefinition sumToExpr,
      toDefinition tPrimaryToExpr,
      toDefinition tPrimaryAndNameToExpr,
      toDefinition targetWithStarAtomToExpr,
      toDefinition termToExpr,
      toDefinition tupleToExpr,
      toDefinition typeAliasToExpr,
      toDefinition typeParameterToExpr,
      toDefinition typedAssignmentToExpr,
      toDefinition untypedAssignmentToExpr,
      toDefinition valuePatternToExpr,
      toDefinition whileStatementToExpr,
      toDefinition escapePythonString,
      toDefinition pythonFloatLiteralText,
      toDefinition toPythonComments]


annotatedRhsToExpr :: TTermDefinition (Py.AnnotatedRhs -> Expr)
annotatedRhsToExpr = def "annotatedRhsToExpr" $
  doc "Serialize an annotated RHS" $
  lambda "arhs" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "=",
      cases Py._AnnotatedRhs (var "arhs") Nothing [
        Py._AnnotatedRhs_star>>: lambda "ses" $
          Serialization.commaSep @@ Serialization.inlineStyle @@
            Lists.map starExpressionToExpr (var "ses"),
        Py._AnnotatedRhs_yield>>: lambda "_" $ Serialization.cst @@ string "yield ..."]]

annotatedStatementToExpr :: TTermDefinition (Py.AnnotatedStatement -> Expr)
annotatedStatementToExpr = def "annotatedStatementToExpr" $
  doc "Serialize an annotated statement (with optional doc comment)" $
  lambda "as_" $ lets [
    "doc_">: project Py._AnnotatedStatement Py._AnnotatedStatement_comment @@ var "as_",
    "stmt">: project Py._AnnotatedStatement Py._AnnotatedStatement_statement @@ var "as_"] $
    Serialization.newlineSep @@ list [
      Serialization.cst @@ (toPythonComments @@ var "doc_"),
      statementToExpr @@ var "stmt"]

annotationToExpr :: TTermDefinition (Py.Annotation -> Expr)
annotationToExpr = def "annotationToExpr" $
  doc "Serialize a type annotation" $
  lambda "ann" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string ":",
      expressionToExpr @@ (unwrap Py._Annotation @@ var "ann")]

argsToExpr :: TTermDefinition (Py.Args -> Expr)
argsToExpr = def "argsToExpr" $
  doc "Serialize function arguments" $
  lambda "args" $ lets [
    "pos">: project Py._Args Py._Args_positional @@ var "args",
    "ks">: project Py._Args Py._Args_kwargOrStarred @@ var "args",
    "kss">: project Py._Args Py._Args_kwargOrDoubleStarred @@ var "args"] $
    Serialization.commaSepAdaptive @@ Lists.concat (list [
      Lists.map posArgToExpr (var "pos"),
      Lists.map kwargOrStarredToExpr (var "ks"),
      Lists.map kwargOrDoubleStarredToExpr (var "kss")])

assignmentToExpr :: TTermDefinition (Py.Assignment -> Expr)
assignmentToExpr = def "assignmentToExpr" $
  doc "Serialize an assignment" $
  lambda "a" $
    cases Py._Assignment (var "a") Nothing [
      Py._Assignment_typed>>: lambda "t" $ typedAssignmentToExpr @@ var "t",
      Py._Assignment_untyped>>: lambda "u" $ untypedAssignmentToExpr @@ var "u",
      Py._Assignment_aug>>: lambda "_" $ Serialization.cst @@ string "... += ..."]

assignmentExpressionToExpr :: TTermDefinition (Py.AssignmentExpression -> Expr)
assignmentExpressionToExpr = def "assignmentExpressionToExpr" $
  doc "Serialize an assignment expression (walrus operator)" $
  lambda "ae" $ lets [
    "name">: project Py._AssignmentExpression Py._AssignmentExpression_name @@ var "ae",
    "expr">: project Py._AssignmentExpression Py._AssignmentExpression_expression @@ var "ae"] $
    Serialization.spaceSep @@ list [
      nameToExpr @@ var "name",
      Serialization.cst @@ string ":=",
      expressionToExpr @@ var "expr"]

atomToExpr :: TTermDefinition (Py.Atom -> Expr)
atomToExpr = def "atomToExpr" $
  doc "Serialize a Python atom (literal or basic expression)" $
  lambda "atom" $
    cases Py._Atom (var "atom") Nothing [
      Py._Atom_dict>>: lambda "d" $ dictToExpr @@ var "d",
      Py._Atom_dictcomp>>: lambda "_" $ Serialization.cst @@ string "{...}",
      Py._Atom_ellipsis>>: constant $ Serialization.cst @@ string "...",
      Py._Atom_false>>: constant $ Serialization.cst @@ string "False",
      Py._Atom_genexp>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._Atom_group>>: lambda "g" $ groupToExpr @@ var "g",
      Py._Atom_list>>: lambda "l" $ listToExpr @@ var "l",
      Py._Atom_listcomp>>: lambda "_" $ Serialization.cst @@ string "[...]",
      Py._Atom_name>>: lambda "n" $ nameToExpr @@ var "n",
      Py._Atom_none>>: constant $ Serialization.cst @@ string "None",
      Py._Atom_number>>: lambda "n" $ numberToExpr @@ var "n",
      Py._Atom_set>>: lambda "s" $ setToExpr @@ var "s",
      Py._Atom_setcomp>>: lambda "_" $ Serialization.cst @@ string "{...}",
      Py._Atom_string>>: lambda "s" $ stringToExpr @@ var "s",
      Py._Atom_true>>: constant $ Serialization.cst @@ string "True",
      Py._Atom_tuple>>: lambda "t" $ tupleToExpr @@ var "t"]

attributeToExpr :: TTermDefinition (Py.Attribute -> Expr)
attributeToExpr = def "attributeToExpr" $
  doc "Serialize an attribute access" $
  lambda "attr" $
    Serialization.dotSep @@ Lists.map nameToExpr (unwrap Py._Attribute @@ var "attr")

awaitPrimaryToExpr :: TTermDefinition (Py.AwaitPrimary -> Expr)
awaitPrimaryToExpr = def "awaitPrimaryToExpr" $
  doc "Serialize an await primary expression" $
  lambda "ap" $ lets [
    "await_">: project Py._AwaitPrimary Py._AwaitPrimary_await @@ var "ap",
    "primary">: project Py._AwaitPrimary Py._AwaitPrimary_primary @@ var "ap"] $
    Logic.ifElse (var "await_")
      (Serialization.spaceSep @@ list [Serialization.cst @@ string "await", primaryToExpr @@ var "primary"])
      (primaryToExpr @@ var "primary")

bitwiseAndToExpr :: TTermDefinition (Py.BitwiseAnd -> Expr)
bitwiseAndToExpr = def "bitwiseAndToExpr" $
  doc "Serialize a bitwise AND expression" $
  lambda "band" $ lets [
    "lhs">: project Py._BitwiseAnd Py._BitwiseAnd_lhs @@ var "band",
    "rhs">: project Py._BitwiseAnd Py._BitwiseAnd_rhs @@ var "band"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Maybes.map (lambda "l" $
        Serialization.spaceSep @@ list [bitwiseAndToExpr @@ var "l", Serialization.cst @@ string "&"])
        (var "lhs"),
      just $ shiftExpressionToExpr @@ var "rhs"])

bitwiseOrToExpr :: TTermDefinition (Py.BitwiseOr -> Expr)
bitwiseOrToExpr = def "bitwiseOrToExpr" $
  doc "Serialize a bitwise OR expression" $
  lambda "bor" $ lets [
    "lhs">: project Py._BitwiseOr Py._BitwiseOr_lhs @@ var "bor",
    "rhs">: project Py._BitwiseOr Py._BitwiseOr_rhs @@ var "bor"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Maybes.map (lambda "l" $
        Serialization.spaceSep @@ list [bitwiseOrToExpr @@ var "l", Serialization.cst @@ string "|"])
        (var "lhs"),
      just $ bitwiseXorToExpr @@ var "rhs"])

bitwiseXorToExpr :: TTermDefinition (Py.BitwiseXor -> Expr)
bitwiseXorToExpr = def "bitwiseXorToExpr" $
  doc "Serialize a bitwise XOR expression" $
  lambda "bxor" $ lets [
    "lhs">: project Py._BitwiseXor Py._BitwiseXor_lhs @@ var "bxor",
    "rhs">: project Py._BitwiseXor Py._BitwiseXor_rhs @@ var "bxor"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Maybes.map (lambda "l" $
        Serialization.spaceSep @@ list [bitwiseXorToExpr @@ var "l", Serialization.cst @@ string "^"])
        (var "lhs"),
      just $ bitwiseAndToExpr @@ var "rhs"])

blockToExpr :: TTermDefinition (Py.Block -> Expr)
blockToExpr = def "blockToExpr" $
  doc "Serialize a block" $
  lambda "b" $
    cases Py._Block (var "b") Nothing [
      Py._Block_indented>>: lambda "groups" $
        Serialization.tabIndentDoubleSpace @@ Lists.map
          (lambda "stmts" $ Serialization.newlineSep @@ Lists.map statementToExpr (var "stmts"))
          (var "groups"),
      Py._Block_simple>>: lambda "ss" $
        Serialization.semicolonSep @@ Lists.map simpleStatementToExpr (var "ss")]

capturePatternToExpr :: TTermDefinition (Py.CapturePattern -> Expr)
capturePatternToExpr = def "capturePatternToExpr" $
  doc "Serialize a capture pattern" $
  lambda "cp" $
    patternCaptureTargetToExpr @@ (unwrap Py._CapturePattern @@ var "cp")

caseBlockToExpr :: TTermDefinition (Py.CaseBlock -> Expr)
caseBlockToExpr = def "caseBlockToExpr" $
  doc "Serialize a case block" $
  lambda "cb" $ lets [
    "patterns">: project Py._CaseBlock Py._CaseBlock_patterns @@ var "cb",
    "guard">: project Py._CaseBlock Py._CaseBlock_guard @@ var "cb",
    "body">: project Py._CaseBlock Py._CaseBlock_body @@ var "cb"] $
    Serialization.newlineSep @@ list [
      Serialization.noSep @@ list [
        Serialization.spaceSep @@ Maybes.cat (list [
          just $ Serialization.cst @@ string "case",
          just $ patternsToExpr @@ var "patterns",
          Maybes.map guardToExpr (var "guard")]),
        Serialization.cst @@ string ":"],
      blockToExpr @@ var "body"]

classDefinitionToExpr :: TTermDefinition (Py.ClassDefinition -> Expr)
classDefinitionToExpr = def "classDefinitionToExpr" $
  doc "Serialize a class definition" $
  lambda "cd" $ lets [
    "decs">: project Py._ClassDefinition Py._ClassDefinition_decorators @@ var "cd",
    "name">: project Py._ClassDefinition Py._ClassDefinition_name @@ var "cd",
    "args">: project Py._ClassDefinition Py._ClassDefinition_arguments @@ var "cd",
    "body">: project Py._ClassDefinition Py._ClassDefinition_body @@ var "cd",
    "argPart">: Maybes.map (lambda "a" $ Serialization.noSep @@ list [
      Serialization.cst @@ string "(",
      argsToExpr @@ var "a",
      Serialization.cst @@ string ")"]) (var "args")] $
    Serialization.newlineSep @@ Maybes.cat (list [
      Maybes.map decoratorsToExpr (var "decs"),
      just $ Serialization.noSep @@ Maybes.cat (list [
        just $ Serialization.spaceSep @@ list [Serialization.cst @@ string "class", nameToExpr @@ var "name"],
        var "argPart",
        just $ Serialization.cst @@ string ":"]),
      just $ blockToExpr @@ var "body"])

classPatternToExpr :: TTermDefinition (Py.ClassPattern -> Expr)
classPatternToExpr = def "classPatternToExpr" $
  doc "Serialize a class pattern" $
  lambda "cp" $ lets [
    "noa">: project Py._ClassPattern Py._ClassPattern_nameOrAttribute @@ var "cp",
    "pos">: project Py._ClassPattern Py._ClassPattern_positionalPatterns @@ var "cp",
    "kw">: project Py._ClassPattern Py._ClassPattern_keywordPatterns @@ var "cp"] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ nameOrAttributeToExpr @@ var "noa",
      just $ Serialization.cst @@ string "(",
      Maybes.map positionalPatternsToExpr (var "pos"),
      Maybes.map keywordPatternsToExpr (var "kw"),
      just $ Serialization.cst @@ string ")"])

closedPatternToExpr :: TTermDefinition (Py.ClosedPattern -> Expr)
closedPatternToExpr = def "closedPatternToExpr" $
  doc "Serialize a closed pattern" $
  lambda "cp" $
    cases Py._ClosedPattern (var "cp") Nothing [
      Py._ClosedPattern_literal>>: lambda "_" $ Serialization.cst @@ string "...",
      Py._ClosedPattern_capture>>: lambda "c" $ capturePatternToExpr @@ var "c",
      Py._ClosedPattern_wildcard>>: constant $ Serialization.cst @@ string "_",
      Py._ClosedPattern_value>>: lambda "v" $ valuePatternToExpr @@ var "v",
      Py._ClosedPattern_group>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._ClosedPattern_sequence>>: lambda "_" $ Serialization.cst @@ string "[...]",
      Py._ClosedPattern_mapping>>: lambda "_" $ Serialization.cst @@ string "{...}",
      Py._ClosedPattern_class>>: lambda "c" $ classPatternToExpr @@ var "c"]

comparisonToExpr :: TTermDefinition (Py.Comparison -> Expr)
comparisonToExpr = def "comparisonToExpr" $
  doc "Serialize a comparison expression" $
  lambda "cmp" $
    -- For now, just encode the LHS bitwise or; comparison operators are rarely used in generated code
    bitwiseOrToExpr @@ (project Py._Comparison Py._Comparison_lhs @@ var "cmp")

compoundStatementToExpr :: TTermDefinition (Py.CompoundStatement -> Expr)
compoundStatementToExpr = def "compoundStatementToExpr" $
  doc "Serialize a compound (multi-line) Python statement" $
  lambda "cs" $
    cases Py._CompoundStatement (var "cs") Nothing [
      Py._CompoundStatement_function>>: lambda "f" $ functionDefinitionToExpr @@ var "f",
      Py._CompoundStatement_if>>: lambda "_" $ Serialization.cst @@ string "if ...",
      Py._CompoundStatement_classDef>>: lambda "c" $ classDefinitionToExpr @@ var "c",
      Py._CompoundStatement_with>>: lambda "_" $ Serialization.cst @@ string "with ...",
      Py._CompoundStatement_for>>: lambda "_" $ Serialization.cst @@ string "for ...",
      Py._CompoundStatement_try>>: lambda "_" $ Serialization.cst @@ string "try ...",
      Py._CompoundStatement_while>>: lambda "w" $ whileStatementToExpr @@ var "w",
      Py._CompoundStatement_match>>: lambda "m" $ matchStatementToExpr @@ var "m"]

-- | Serialize a conditional expression: body if condition else elseExpr
conditionalToExpr :: TTermDefinition (Py.Conditional -> Expr)
conditionalToExpr = def "conditionalToExpr" $
  doc "Serialize a conditional expression (ternary)" $
  lambda "c" $ lets [
    "body">: project Py._Conditional Py._Conditional_body @@ var "c",
    "cond">: project Py._Conditional Py._Conditional_if @@ var "c",
    "elseExpr">: project Py._Conditional Py._Conditional_else @@ var "c"] $
    Serialization.spaceSep @@ list [
      disjunctionToExpr @@ var "body",
      Serialization.cst @@ string "if",
      disjunctionToExpr @@ var "cond",
      Serialization.cst @@ string "else",
      expressionToExpr @@ var "elseExpr"]

conjunctionToExpr :: TTermDefinition (Py.Conjunction -> Expr)
conjunctionToExpr = def "conjunctionToExpr" $
  doc "Serialize a conjunction (and expression)" $
  lambda "c" $
    Serialization.symbolSep @@ string "and" @@ Serialization.inlineStyle @@
      Lists.map inversionToExpr (unwrap Py._Conjunction @@ var "c")

decoratorsToExpr :: TTermDefinition (Py.Decorators -> Expr)
decoratorsToExpr = def "decoratorsToExpr" $
  doc "Serialize decorators" $
  lambda "decs" $
    Serialization.newlineSep @@ Lists.map
      (lambda "ne" $ Serialization.noSep @@ list [Serialization.cst @@ string "@", namedExpressionToExpr @@ var "ne"])
      (unwrap Py._Decorators @@ var "decs")

dictToExpr :: TTermDefinition (Py.Dict -> Expr)
dictToExpr = def "dictToExpr" $
  doc "Serialize a Python dictionary" $
  lambda "d" $
    Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
      Lists.map doubleStarredKvpairToExpr (unwrap Py._Dict @@ var "d")

disjunctionToExpr :: TTermDefinition (Py.Disjunction -> Expr)
disjunctionToExpr = def "disjunctionToExpr" $
  doc "Serialize a disjunction (or expression)" $
  lambda "d" $
    Serialization.symbolSep @@ string "or" @@ Serialization.inlineStyle @@
      Lists.map conjunctionToExpr (unwrap Py._Disjunction @@ var "d")

dottedAsNameToExpr :: TTermDefinition (Py.DottedAsName -> Expr)
dottedAsNameToExpr = def "dottedAsNameToExpr" $
  doc "Serialize a dotted as name" $
  lambda "dan" $ lets [
    "name">: project Py._DottedAsName Py._DottedAsName_name @@ var "dan",
    "alias">: project Py._DottedAsName Py._DottedAsName_as @@ var "dan"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ dottedNameToExpr @@ var "name",
      Maybes.map (lambda "a" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "as", nameToExpr @@ var "a"]) (var "alias")])

dottedNameToExpr :: TTermDefinition (Py.DottedName -> Expr)
dottedNameToExpr = def "dottedNameToExpr" $
  doc "Serialize a dotted name (e.g., module.submodule)" $
  lambda "dn" $
    Serialization.cst @@ Strings.intercalate (string ".") (Lists.map (lambda "n" $ unwrap Py._Name @@ var "n") (unwrap Py._DottedName @@ var "dn"))

doubleStarredKvpairToExpr :: TTermDefinition (Py.DoubleStarredKvpair -> Expr)
doubleStarredKvpairToExpr = def "doubleStarredKvpairToExpr" $
  doc "Serialize a double-starred key-value pair" $
  lambda "dskv" $
    cases Py._DoubleStarredKvpair (var "dskv") Nothing [
      Py._DoubleStarredKvpair_pair>>: lambda "p" $ kvpairToExpr @@ var "p",
      Py._DoubleStarredKvpair_starred>>: lambda "e" $
        Serialization.noSep @@ list [Serialization.cst @@ string "**", bitwiseOrToExpr @@ var "e"]]

expressionToExpr :: TTermDefinition (Py.Expression -> Expr)
expressionToExpr = def "expressionToExpr" $
  doc "Serialize a Python expression" $
  lambda "expr" $
    cases Py._Expression (var "expr") Nothing [
      Py._Expression_simple>>: lambda "d" $ disjunctionToExpr @@ var "d",
      Py._Expression_conditional>>: lambda "c" $ conditionalToExpr @@ var "c",
      Py._Expression_lambda>>: lambda "l" $ lambdaToExpr @@ var "l"]

factorToExpr :: TTermDefinition (Py.Factor -> Expr)
factorToExpr = def "factorToExpr" $
  doc "Serialize a factor expression" $
  lambda "f" $
    cases Py._Factor (var "f") Nothing [
      Py._Factor_positive>>: lambda "inner" $
        Serialization.noSep @@ list [Serialization.cst @@ string "+", factorToExpr @@ var "inner"],
      Py._Factor_negative>>: lambda "inner" $
        Serialization.noSep @@ list [Serialization.cst @@ string "-", factorToExpr @@ var "inner"],
      Py._Factor_complement>>: lambda "inner" $
        Serialization.noSep @@ list [Serialization.cst @@ string "~", factorToExpr @@ var "inner"],
      Py._Factor_simple>>: lambda "p" $ powerToExpr @@ var "p"]

functionDefRawToExpr :: TTermDefinition (Py.FunctionDefRaw -> Expr)
functionDefRawToExpr = def "functionDefRawToExpr" $
  doc "Serialize a raw function definition" $
  lambda "fdr" $ lets [
    "async_">: project Py._FunctionDefRaw Py._FunctionDefRaw_async @@ var "fdr",
    "name">: project Py._FunctionDefRaw Py._FunctionDefRaw_name @@ var "fdr",
    "tparams">: project Py._FunctionDefRaw Py._FunctionDefRaw_typeParams @@ var "fdr",
    "params">: project Py._FunctionDefRaw Py._FunctionDefRaw_params @@ var "fdr",
    "retType">: project Py._FunctionDefRaw Py._FunctionDefRaw_returnType @@ var "fdr",
    "block">: project Py._FunctionDefRaw Py._FunctionDefRaw_block @@ var "fdr",
    "asyncKw">: Logic.ifElse (var "async_") (just $ Serialization.cst @@ string "async") nothing,
    "tparamPart">: Logic.ifElse (Lists.null (var "tparams"))
      nothing
      (just $ Serialization.bracketList @@ Serialization.inlineStyle @@ Lists.map typeParameterToExpr (var "tparams")),
    "paramPart">: Maybes.map parametersToExpr (var "params"),
    "retPart">: Maybes.map (lambda "t" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "->", expressionToExpr @@ var "t"]) (var "retType")] $
    Serialization.newlineSep @@ list [
      Serialization.noSep @@ list [
        Serialization.spaceSep @@ Maybes.cat (list [
          var "asyncKw",
          just $ Serialization.cst @@ string "def",
          just $ Serialization.noSep @@ Maybes.cat (list [
            just $ nameToExpr @@ var "name",
            var "tparamPart",
            just $ Serialization.cst @@ string "(",
            var "paramPart",
            just $ Serialization.cst @@ string ")"]),
          var "retPart"]),
        Serialization.cst @@ string ":"],
      blockToExpr @@ var "block"]

functionDefinitionToExpr :: TTermDefinition (Py.FunctionDefinition -> Expr)
functionDefinitionToExpr = def "functionDefinitionToExpr" $
  doc "Serialize a function definition" $
  lambda "fd" $ lets [
    "decs">: project Py._FunctionDefinition Py._FunctionDefinition_decorators @@ var "fd",
    "raw">: project Py._FunctionDefinition Py._FunctionDefinition_raw @@ var "fd"] $
    Serialization.newlineSep @@ Maybes.cat (list [
      Maybes.map decoratorsToExpr (var "decs"),
      just $ functionDefRawToExpr @@ var "raw"])

groupToExpr :: TTermDefinition (Py.Group -> Expr)
groupToExpr = def "groupToExpr" $
  doc "Serialize a parenthesized group" $
  lambda "g" $
    cases Py._Group (var "g") Nothing [
      Py._Group_expression>>: lambda "ne" $ namedExpressionToExpr @@ var "ne",
      Py._Group_yield>>: lambda "_" $ Serialization.cst @@ string "(yield ...)"]

guardToExpr :: TTermDefinition (Py.Guard -> Expr)
guardToExpr = def "guardToExpr" $
  doc "Serialize a guard clause" $
  lambda "g" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "if",
      namedExpressionToExpr @@ (unwrap Py._Guard @@ var "g")]

importFromToExpr :: TTermDefinition (Py.ImportFrom -> Expr)
importFromToExpr = def "importFromToExpr" $
  doc "Serialize an import from statement" $
  lambda "if_" $ lets [
    "prefixes">: project Py._ImportFrom Py._ImportFrom_prefixes @@ var "if_",
    "name">: project Py._ImportFrom Py._ImportFrom_dottedName @@ var "if_",
    "targets">: project Py._ImportFrom Py._ImportFrom_targets @@ var "if_",
    "lhs">: Serialization.noSep @@ Maybes.cat (
      Lists.concat (list [
        Lists.map (lambda "p" $ just $ relativeImportPrefixToExpr @@ var "p") (var "prefixes"),
        list [Maybes.map dottedNameToExpr (var "name")]]))] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "from",
      var "lhs",
      Serialization.cst @@ string "import",
      importFromTargetsToExpr @@ var "targets"]

importFromAsNameToExpr :: TTermDefinition (Py.ImportFromAsName -> Expr)
importFromAsNameToExpr = def "importFromAsNameToExpr" $
  doc "Serialize an import from as name" $
  lambda "ifan" $ lets [
    "name">: project Py._ImportFromAsName Py._ImportFromAsName_name @@ var "ifan",
    "alias">: project Py._ImportFromAsName Py._ImportFromAsName_as @@ var "ifan"] $
    Maybes.maybe
      (nameToExpr @@ var "name")
      (lambda "a" $ Serialization.spaceSep @@ list [
        nameToExpr @@ var "name",
        Serialization.cst @@ string "as",
        nameToExpr @@ var "a"])
      (var "alias")

importFromTargetsToExpr :: TTermDefinition (Py.ImportFromTargets -> Expr)
importFromTargetsToExpr = def "importFromTargetsToExpr" $
  doc "Serialize import from targets" $
  lambda "t" $
    cases Py._ImportFromTargets (var "t") Nothing [
      Py._ImportFromTargets_simple>>: lambda "names" $
        Serialization.commaSep @@ Serialization.inlineStyle @@
          Lists.map importFromAsNameToExpr (var "names"),
      Py._ImportFromTargets_parens>>: lambda "names" $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "(",
          Serialization.commaSep @@ Serialization.inlineStyle @@
            Lists.map importFromAsNameToExpr (var "names"),
          Serialization.cst @@ string ")"],
      Py._ImportFromTargets_star>>: constant $ Serialization.cst @@ string "*"]

importNameToExpr :: TTermDefinition (Py.ImportName -> Expr)
importNameToExpr = def "importNameToExpr" $
  doc "Serialize an import name" $
  lambda "in_" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "import",
      Serialization.commaSep @@ Serialization.inlineStyle @@
        Lists.map dottedAsNameToExpr (unwrap Py._ImportName @@ var "in_")]

importStatementToExpr :: TTermDefinition (Py.ImportStatement -> Expr)
importStatementToExpr = def "importStatementToExpr" $
  doc "Serialize an import statement" $
  lambda "is_" $
    cases Py._ImportStatement (var "is_") Nothing [
      Py._ImportStatement_name>>: lambda "n" $ importNameToExpr @@ var "n",
      Py._ImportStatement_from>>: lambda "f" $ importFromToExpr @@ var "f"]

inversionToExpr :: TTermDefinition (Py.Inversion -> Expr)
inversionToExpr = def "inversionToExpr" $
  doc "Serialize an inversion (not expression)" $
  lambda "i" $
    cases Py._Inversion (var "i") Nothing [
      Py._Inversion_not>>: lambda "other" $
        Serialization.spaceSep @@ list [Serialization.cst @@ string "not", inversionToExpr @@ var "other"],
      Py._Inversion_simple>>: lambda "c" $ comparisonToExpr @@ var "c"]

keywordPatternToExpr :: TTermDefinition (Py.KeywordPattern -> Expr)
keywordPatternToExpr = def "keywordPatternToExpr" $
  doc "Serialize a keyword pattern" $
  lambda "kp" $ lets [
    "name">: project Py._KeywordPattern Py._KeywordPattern_name @@ var "kp",
    "pat">: project Py._KeywordPattern Py._KeywordPattern_pattern @@ var "kp"] $
    Serialization.noSep @@ list [
      nameToExpr @@ var "name",
      Serialization.cst @@ string "=",
      patternToExpr @@ var "pat"]

keywordPatternsToExpr :: TTermDefinition (Py.KeywordPatterns -> Expr)
keywordPatternsToExpr = def "keywordPatternsToExpr" $
  doc "Serialize keyword patterns" $
  lambda "kp" $
    Serialization.commaSep @@ Serialization.inlineStyle @@
      Lists.map keywordPatternToExpr (unwrap Py._KeywordPatterns @@ var "kp")

kvpairToExpr :: TTermDefinition (Py.Kvpair -> Expr)
kvpairToExpr = def "kvpairToExpr" $
  doc "Serialize a key-value pair" $
  lambda "kv" $ lets [
    "k">: project Py._Kvpair Py._Kvpair_key @@ var "kv",
    "v">: project Py._Kvpair Py._Kvpair_value @@ var "kv"] $
    Serialization.spaceSep @@ list [
      Serialization.noSep @@ list [expressionToExpr @@ var "k", Serialization.cst @@ string ":"],
      expressionToExpr @@ var "v"]

kwargToExpr :: TTermDefinition (Py.Kwarg -> Expr)
kwargToExpr = def "kwargToExpr" $
  doc "Serialize a keyword argument" $
  lambda "k" $ lets [
    "name">: project Py._Kwarg Py._Kwarg_name @@ var "k",
    "expr">: project Py._Kwarg Py._Kwarg_value @@ var "k"] $
    Serialization.noSep @@ list [
      nameToExpr @@ var "name",
      Serialization.cst @@ string "=",
      expressionToExpr @@ var "expr"]

kwargOrDoubleStarredToExpr :: TTermDefinition (Py.KwargOrDoubleStarred -> Expr)
kwargOrDoubleStarredToExpr = def "kwargOrDoubleStarredToExpr" $
  doc "Serialize a kwarg or double starred" $
  lambda "kds" $
    cases Py._KwargOrDoubleStarred (var "kds") Nothing [
      Py._KwargOrDoubleStarred_kwarg>>: lambda "k" $ kwargToExpr @@ var "k",
      Py._KwargOrDoubleStarred_doubleStarred>>: lambda "e" $
        Serialization.noSep @@ list [Serialization.cst @@ string "**", expressionToExpr @@ var "e"]]

kwargOrStarredToExpr :: TTermDefinition (Py.KwargOrStarred -> Expr)
kwargOrStarredToExpr = def "kwargOrStarredToExpr" $
  doc "Serialize a kwarg or starred" $
  lambda "ks" $
    cases Py._KwargOrStarred (var "ks") Nothing [
      Py._KwargOrStarred_kwarg>>: lambda "k" $ kwargToExpr @@ var "k",
      Py._KwargOrStarred_starred>>: lambda "se" $ starredExpressionToExpr @@ var "se"]

lambdaToExpr :: TTermDefinition (Py.Lambda -> Expr)
lambdaToExpr = def "lambdaToExpr" $
  doc "Serialize a lambda expression" $
  lambda "l" $ lets [
    "params">: project Py._Lambda Py._Lambda_params @@ var "l",
    "body">: project Py._Lambda Py._Lambda_body @@ var "l"] $
    Serialization.parens @@ (Serialization.spaceSep @@ list [
      Serialization.cst @@ string "lambda",
      Serialization.noSep @@ list [lambdaParametersToExpr @@ var "params", Serialization.cst @@ string ":"],
      expressionToExpr @@ var "body"])

lambdaParamNoDefaultToExpr :: TTermDefinition (Py.LambdaParamNoDefault -> Expr)
lambdaParamNoDefaultToExpr = def "lambdaParamNoDefaultToExpr" $
  doc "Serialize a lambda parameter without default" $
  lambda "p" $
    nameToExpr @@ (unwrap Py._LambdaParamNoDefault @@ var "p")

lambdaParametersToExpr :: TTermDefinition (Py.LambdaParameters -> Expr)
lambdaParametersToExpr = def "lambdaParametersToExpr" $
  doc "Serialize lambda parameters" $
  lambda "lp" $ lets [
    "nodef">: project Py._LambdaParameters Py._LambdaParameters_paramNoDefault @@ var "lp"] $
    Serialization.commaSep @@ Serialization.inlineStyle @@
      Lists.map lambdaParamNoDefaultToExpr (var "nodef")

lambdaStarEtcToExpr :: TTermDefinition (Py.LambdaStarEtc -> Expr)
lambdaStarEtcToExpr = def "lambdaStarEtcToExpr" $
  doc "Serialize lambda star etc" $
  lambda "lse" $
    cases Py._LambdaStarEtc (var "lse") Nothing [
      Py._LambdaStarEtc_paramNoDefault>>: lambda "p" $ lambdaParamNoDefaultToExpr @@ var "p",
      Py._LambdaStarEtc_star>>: lambda "_" $ Serialization.cst @@ string "*...",
      Py._LambdaStarEtc_paramMaybeDefault>>: lambda "_" $ Serialization.cst @@ string "...",
      Py._LambdaStarEtc_kwds>>: lambda "_" $ Serialization.cst @@ string "**..."]

listToExpr :: TTermDefinition (Py.List -> Expr)
listToExpr = def "listToExpr" $
  doc "Serialize a Python list" $
  lambda "l" $
    Serialization.bracketListAdaptive @@ Lists.map starNamedExpressionToExpr (unwrap Py._List @@ var "l")

matchStatementToExpr :: TTermDefinition (Py.MatchStatement -> Expr)
matchStatementToExpr = def "matchStatementToExpr" $
  doc "Serialize a match statement" $
  lambda "ms" $ lets [
    "subj">: project Py._MatchStatement Py._MatchStatement_subject @@ var "ms",
    "cases">: project Py._MatchStatement Py._MatchStatement_cases @@ var "ms"] $
    Serialization.newlineSep @@ list [
      Serialization.spaceSep @@ list [
        Serialization.cst @@ string "match",
        Serialization.noSep @@ list [subjectExpressionToExpr @@ var "subj", Serialization.cst @@ string ":"]],
      Serialization.tabIndentDoubleSpace @@ Lists.map caseBlockToExpr (var "cases")]

moduleToExpr :: TTermDefinition (Py.Module -> Expr)
moduleToExpr = def "moduleToExpr" $
  doc "Serialize a Python module to an AST expression" $
  lambda "mod" $ lets [
    "warning">: Serialization.cst @@ (toPythonComments @@ Constants.warningAutoGeneratedFile),
    "groups">: Lists.map
      (lambda "group" $ Serialization.newlineSep @@ Lists.map statementToExpr (var "group"))
      (unwrap Py._Module @@ var "mod")] $
    Serialization.doubleNewlineSep @@ Lists.cons (var "warning") (var "groups")

nameToExpr :: TTermDefinition (Py.Name -> Expr)
nameToExpr = def "nameToExpr" $
  doc "Serialize a Python name/identifier" $
  lambda "n" $
    Serialization.cst @@ (unwrap Py._Name @@ var "n")

nameOrAttributeToExpr :: TTermDefinition (Py.NameOrAttribute -> Expr)
nameOrAttributeToExpr = def "nameOrAttributeToExpr" $
  doc "Serialize a name or attribute" $
  lambda "noa" $
    Serialization.dotSep @@ Lists.map nameToExpr (unwrap Py._NameOrAttribute @@ var "noa")

namedExpressionToExpr :: TTermDefinition (Py.NamedExpression -> Expr)
namedExpressionToExpr = def "namedExpressionToExpr" $
  doc "Serialize a named expression" $
  lambda "ne" $
    cases Py._NamedExpression (var "ne") Nothing [
      Py._NamedExpression_simple>>: lambda "e" $ expressionToExpr @@ var "e",
      Py._NamedExpression_assignment>>: lambda "ae" $ assignmentExpressionToExpr @@ var "ae"]

numberToExpr :: TTermDefinition (Py.Number -> Expr)
numberToExpr = def "numberToExpr" $
  doc "Serialize a Python number literal" $
  lambda "num" $
    cases Py._Number (var "num") Nothing [
      Py._Number_float>>: lambda "f" $
        Serialization.cst @@ (pythonFloatLiteralText @@ Literals.showBigfloat (var "f")),
      Py._Number_integer>>: lambda "i" $ Serialization.cst @@ Literals.showBigint (var "i")]

orPatternToExpr :: TTermDefinition (Py.OrPattern -> Expr)
orPatternToExpr = def "orPatternToExpr" $
  doc "Serialize an or pattern" $
  lambda "op" $
    Serialization.symbolSep @@ string "|" @@ Serialization.inlineStyle @@
      Lists.map closedPatternToExpr (unwrap Py._OrPattern @@ var "op")

paramToExpr :: TTermDefinition (Py.Param -> Expr)
paramToExpr = def "paramToExpr" $
  doc "Serialize a parameter" $
  lambda "p" $ lets [
    "name">: project Py._Param Py._Param_name @@ var "p",
    "ann">: project Py._Param Py._Param_annotation @@ var "p"] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ nameToExpr @@ var "name",
      Maybes.map annotationToExpr (var "ann")])

paramNoDefaultToExpr :: TTermDefinition (Py.ParamNoDefault -> Expr)
paramNoDefaultToExpr = def "paramNoDefaultToExpr" $
  doc "Serialize a parameter without default" $
  lambda "pnd" $
    paramToExpr @@ (project Py._ParamNoDefault Py._ParamNoDefault_param @@ var "pnd")

paramNoDefaultParametersToExpr :: TTermDefinition (Py.ParamNoDefaultParameters -> Expr)
paramNoDefaultParametersToExpr = def "paramNoDefaultParametersToExpr" $
  doc "Serialize parameters without defaults" $
  lambda "pndp" $ lets [
    "nodef">: project Py._ParamNoDefaultParameters Py._ParamNoDefaultParameters_paramNoDefault @@ var "pndp"] $
    Serialization.commaSepAdaptive @@ Lists.map paramNoDefaultToExpr (var "nodef")

parametersToExpr :: TTermDefinition (Py.Parameters -> Expr)
parametersToExpr = def "parametersToExpr" $
  doc "Serialize function parameters" $
  lambda "p" $
    cases Py._Parameters (var "p") Nothing [
      Py._Parameters_paramNoDefault>>: lambda "pnd" $ paramNoDefaultParametersToExpr @@ var "pnd",
      Py._Parameters_slashNoDefault>>: lambda "_" $ Serialization.cst @@ string "...",
      Py._Parameters_slashWithDefault>>: lambda "_" $ Serialization.cst @@ string "..."]

patternToExpr :: TTermDefinition (Py.Pattern -> Expr)
patternToExpr = def "patternToExpr" $
  doc "Serialize a pattern" $
  lambda "p" $
    cases Py._Pattern (var "p") Nothing [
      Py._Pattern_or>>: lambda "op" $ orPatternToExpr @@ var "op",
      Py._Pattern_as>>: lambda "_" $ Serialization.cst @@ string "... as ..."]

patternCaptureTargetToExpr :: TTermDefinition (Py.PatternCaptureTarget -> Expr)
patternCaptureTargetToExpr = def "patternCaptureTargetToExpr" $
  doc "Serialize a pattern capture target" $
  lambda "pct" $
    nameToExpr @@ (unwrap Py._PatternCaptureTarget @@ var "pct")

patternsToExpr :: TTermDefinition (Py.Patterns -> Expr)
patternsToExpr = def "patternsToExpr" $
  doc "Serialize patterns" $
  lambda "ps" $
    cases Py._Patterns (var "ps") Nothing [
      Py._Patterns_pattern>>: lambda "p" $ patternToExpr @@ var "p",
      Py._Patterns_sequence>>: lambda "_" $ Serialization.cst @@ string "..."]

posArgToExpr :: TTermDefinition (Py.PosArg -> Expr)
posArgToExpr = def "posArgToExpr" $
  doc "Serialize a positional argument" $
  lambda "pa" $
    cases Py._PosArg (var "pa") Nothing [
      Py._PosArg_starred>>: lambda "se" $ starredExpressionToExpr @@ var "se",
      Py._PosArg_assignment>>: lambda "ae" $ assignmentExpressionToExpr @@ var "ae",
      Py._PosArg_expression>>: lambda "e" $ expressionToExpr @@ var "e"]

positionalPatternsToExpr :: TTermDefinition (Py.PositionalPatterns -> Expr)
positionalPatternsToExpr = def "positionalPatternsToExpr" $
  doc "Serialize positional patterns" $
  lambda "pp" $
    Serialization.commaSep @@ Serialization.inlineStyle @@
      Lists.map patternToExpr (unwrap Py._PositionalPatterns @@ var "pp")

powerToExpr :: TTermDefinition (Py.Power -> Expr)
powerToExpr = def "powerToExpr" $
  doc "Serialize a power expression" $
  lambda "p" $ lets [
    "lhs">: project Py._Power Py._Power_lhs @@ var "p",
    "rhs">: project Py._Power Py._Power_rhs @@ var "p"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ awaitPrimaryToExpr @@ var "lhs",
      Maybes.map (lambda "r" $
        Serialization.spaceSep @@ list [Serialization.cst @@ string "**", factorToExpr @@ var "r"])
        (var "rhs")])

primaryToExpr :: TTermDefinition (Py.Primary -> Expr)
primaryToExpr = def "primaryToExpr" $
  doc "Serialize a primary expression" $
  lambda "p" $
    cases Py._Primary (var "p") Nothing [
      Py._Primary_simple>>: lambda "a" $ atomToExpr @@ var "a",
      Py._Primary_compound>>: lambda "pwr" $ primaryWithRhsToExpr @@ var "pwr"]

primaryRhsToExpr :: TTermDefinition (Py.PrimaryRhs -> Expr)
primaryRhsToExpr = def "primaryRhsToExpr" $
  doc "Serialize a primary RHS" $
  lambda "rhs" $
    cases Py._PrimaryRhs (var "rhs") Nothing [
      Py._PrimaryRhs_call>>: lambda "args" $
        Serialization.noSep @@ list [Serialization.cst @@ string "(", argsToExpr @@ var "args", Serialization.cst @@ string ")"],
      Py._PrimaryRhs_project>>: lambda "name" $
        Serialization.noSep @@ list [Serialization.cst @@ string ".", nameToExpr @@ var "name"],
      Py._PrimaryRhs_slices>>: lambda "slices" $
        Serialization.noSep @@ list [Serialization.cst @@ string "[", slicesToExpr @@ var "slices", Serialization.cst @@ string "]"],
      Py._PrimaryRhs_genexp>>: lambda "_" $ Serialization.cst @@ string "[...]"]

primaryWithRhsToExpr :: TTermDefinition (Py.PrimaryWithRhs -> Expr)
primaryWithRhsToExpr = def "primaryWithRhsToExpr" $
  doc "Serialize a primary with RHS" $
  lambda "pwr" $ lets [
    "prim">: project Py._PrimaryWithRhs Py._PrimaryWithRhs_primary @@ var "pwr",
    "rhs">: project Py._PrimaryWithRhs Py._PrimaryWithRhs_rhs @@ var "pwr"] $
    Serialization.noSep @@ list [primaryToExpr @@ var "prim", primaryRhsToExpr @@ var "rhs"]

raiseExpressionToExpr :: TTermDefinition (Py.RaiseExpression -> Expr)
raiseExpressionToExpr = def "raiseExpressionToExpr" $
  doc "Serialize a raise expression" $
  lambda "re" $ lets [
    "expr">: project Py._RaiseExpression Py._RaiseExpression_expression @@ var "re",
    "from_">: project Py._RaiseExpression Py._RaiseExpression_from @@ var "re"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ expressionToExpr @@ var "expr",
      Maybes.map (lambda "f" $
        Serialization.spaceSep @@ list [Serialization.cst @@ string "from", expressionToExpr @@ var "f"])
        (var "from_")])

raiseStatementToExpr :: TTermDefinition (Py.RaiseStatement -> Expr)
raiseStatementToExpr = def "raiseStatementToExpr" $
  doc "Serialize a raise statement" $
  lambda "rs" $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "raise",
      Maybes.map raiseExpressionToExpr (unwrap Py._RaiseStatement @@ var "rs")])

relativeImportPrefixToExpr :: TTermDefinition (Py.RelativeImportPrefix -> Expr)
relativeImportPrefixToExpr = def "relativeImportPrefixToExpr" $
  doc "Serialize a relative import prefix" $
  lambda "p" $
    cases Py._RelativeImportPrefix (var "p") Nothing [
      Py._RelativeImportPrefix_dot>>: constant $ Serialization.cst @@ string ".",
      Py._RelativeImportPrefix_ellipsis>>: constant $ Serialization.cst @@ string "..."]

returnStatementToExpr :: TTermDefinition (Py.ReturnStatement -> Expr)
returnStatementToExpr = def "returnStatementToExpr" $
  doc "Serialize a return statement" $
  lambda "rs" $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "return",
      Serialization.commaSep @@ Serialization.inlineStyle @@
        Lists.map starExpressionToExpr (unwrap Py._ReturnStatement @@ var "rs")]

setToExpr :: TTermDefinition (Py.Set -> Expr)
setToExpr = def "setToExpr" $
  doc "Serialize a Python set" $
  lambda "s" $
    Serialization.bracesListAdaptive @@ Lists.map starNamedExpressionToExpr (unwrap Py._Set @@ var "s")

shiftExpressionToExpr :: TTermDefinition (Py.ShiftExpression -> Expr)
shiftExpressionToExpr = def "shiftExpressionToExpr" $
  doc "Serialize a shift expression" $
  lambda "se" $
    -- Shift operators are rarely used; just encode the sum
    sumToExpr @@ (project Py._ShiftExpression Py._ShiftExpression_rhs @@ var "se")

simpleStatementToExpr :: TTermDefinition (Py.SimpleStatement -> Expr)
simpleStatementToExpr = def "simpleStatementToExpr" $
  doc "Serialize a simple (single-line) Python statement" $
  lambda "ss" $
    cases Py._SimpleStatement (var "ss") Nothing [
      Py._SimpleStatement_assignment>>: lambda "a" $ assignmentToExpr @@ var "a",
      Py._SimpleStatement_starExpressions>>: lambda "es" $
        Serialization.newlineSep @@ Lists.map starExpressionToExpr (var "es"),
      Py._SimpleStatement_return>>: lambda "r" $ returnStatementToExpr @@ var "r",
      Py._SimpleStatement_raise>>: lambda "r" $ raiseStatementToExpr @@ var "r",
      Py._SimpleStatement_pass>>: constant $ Serialization.cst @@ string "pass",
      Py._SimpleStatement_break>>: constant $ Serialization.cst @@ string "break",
      Py._SimpleStatement_continue>>: constant $ Serialization.cst @@ string "continue",
      Py._SimpleStatement_import>>: lambda "i" $ importStatementToExpr @@ var "i",
      Py._SimpleStatement_typeAlias>>: lambda "t" $ typeAliasToExpr @@ var "t",
      Py._SimpleStatement_assert>>: lambda "_" $ Serialization.cst @@ string "assert ...",
      Py._SimpleStatement_global>>: lambda "_" $ Serialization.cst @@ string "global ...",
      Py._SimpleStatement_nonlocal>>: lambda "_" $ Serialization.cst @@ string "nonlocal ...",
      Py._SimpleStatement_del>>: lambda "_" $ Serialization.cst @@ string "del ..."]

simpleTypeParameterToExpr :: TTermDefinition (Py.SimpleTypeParameter -> Expr)
simpleTypeParameterToExpr = def "simpleTypeParameterToExpr" $
  doc "Serialize a simple type parameter" $
  lambda "stp" $
    nameToExpr @@ (project Py._SimpleTypeParameter Py._SimpleTypeParameter_name @@ var "stp")

singleTargetToExpr :: TTermDefinition (Py.SingleTarget -> Expr)
singleTargetToExpr = def "singleTargetToExpr" $
  doc "Serialize a single target" $
  lambda "st" $
    cases Py._SingleTarget (var "st") Nothing [
      Py._SingleTarget_name>>: lambda "n" $ nameToExpr @@ var "n",
      Py._SingleTarget_parens>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._SingleTarget_subscriptAttributeTarget>>: lambda "_" $ Serialization.cst @@ string "..."]

sliceToExpr :: TTermDefinition (Py.Slice -> Expr)
sliceToExpr = def "sliceToExpr" $
  doc "Serialize a slice" $
  lambda "s" $
    cases Py._Slice (var "s") Nothing [
      Py._Slice_named>>: lambda "ne" $ namedExpressionToExpr @@ var "ne",
      Py._Slice_slice_>>: lambda "_" $ Serialization.cst @@ string ":"]

sliceOrStarredExpressionToExpr :: TTermDefinition (Py.SliceOrStarredExpression -> Expr)
sliceOrStarredExpressionToExpr = def "sliceOrStarredExpressionToExpr" $
  doc "Serialize a slice or starred expression" $
  lambda "s" $
    cases Py._SliceOrStarredExpression (var "s") Nothing [
      Py._SliceOrStarredExpression_slice>>: lambda "sl" $ sliceToExpr @@ var "sl",
      Py._SliceOrStarredExpression_starred>>: lambda "se" $ starredExpressionToExpr @@ var "se"]

slicesToExpr :: TTermDefinition (Py.Slices -> Expr)
slicesToExpr = def "slicesToExpr" $
  doc "Serialize slices" $
  lambda "s" $ lets [
    "hd">: project Py._Slices Py._Slices_head @@ var "s",
    "tl">: project Py._Slices Py._Slices_tail @@ var "s"] $
    Serialization.commaSep @@ Serialization.inlineStyle @@
      Lists.cons (sliceToExpr @@ var "hd") (Lists.map sliceOrStarredExpressionToExpr (var "tl"))

starAtomToExpr :: TTermDefinition (Py.StarAtom -> Expr)
starAtomToExpr = def "starAtomToExpr" $
  doc "Serialize a star atom" $
  lambda "sa" $
    cases Py._StarAtom (var "sa") Nothing [
      Py._StarAtom_name>>: lambda "n" $ nameToExpr @@ var "n",
      Py._StarAtom_targetWithStarAtom>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._StarAtom_starTargetsTupleSeq>>: lambda "_" $ Serialization.cst @@ string "(...)",
      Py._StarAtom_starTargetsListSeq>>: lambda "_" $ Serialization.cst @@ string "[...]"]

starExpressionToExpr :: TTermDefinition (Py.StarExpression -> Expr)
starExpressionToExpr = def "starExpressionToExpr" $
  doc "Serialize a star expression" $
  lambda "se" $
    cases Py._StarExpression (var "se") Nothing [
      Py._StarExpression_star>>: lambda "bor" $
        Serialization.noSep @@ list [Serialization.cst @@ string "*", bitwiseOrToExpr @@ var "bor"],
      Py._StarExpression_simple>>: lambda "e" $ expressionToExpr @@ var "e"]

starNamedExpressionToExpr :: TTermDefinition (Py.StarNamedExpression -> Expr)
starNamedExpressionToExpr = def "starNamedExpressionToExpr" $
  doc "Serialize a star named expression" $
  lambda "sne" $
    cases Py._StarNamedExpression (var "sne") Nothing [
      Py._StarNamedExpression_star>>: lambda "bor" $
        Serialization.noSep @@ list [Serialization.cst @@ string "*", bitwiseOrToExpr @@ var "bor"],
      Py._StarNamedExpression_simple>>: lambda "ne" $ namedExpressionToExpr @@ var "ne"]

starTargetToExpr :: TTermDefinition (Py.StarTarget -> Expr)
starTargetToExpr = def "starTargetToExpr" $
  doc "Serialize a star target" $
  lambda "st" $
    cases Py._StarTarget (var "st") Nothing [
      Py._StarTarget_unstarred>>: lambda "t" $ targetWithStarAtomToExpr @@ var "t",
      Py._StarTarget_starred>>: lambda "inner" $
        Serialization.noSep @@ list [Serialization.cst @@ string "*", starTargetToExpr @@ var "inner"]]

starredExpressionToExpr :: TTermDefinition (Py.StarredExpression -> Expr)
starredExpressionToExpr = def "starredExpressionToExpr" $
  doc "Serialize a starred expression" $
  lambda "se" $
    Serialization.noSep @@ list [
      Serialization.cst @@ string "*",
      expressionToExpr @@ (unwrap Py._StarredExpression @@ var "se")]

statementToExpr :: TTermDefinition (Py.Statement -> Expr)
statementToExpr = def "statementToExpr" $
  doc "Serialize a Python statement" $
  lambda "stmt" $
    cases Py._Statement (var "stmt") Nothing [
      Py._Statement_annotated>>: lambda "a" $ annotatedStatementToExpr @@ var "a",
      Py._Statement_simple>>: lambda "ss" $
        Serialization.newlineSep @@ Lists.map simpleStatementToExpr (var "ss"),
      Py._Statement_compound>>: lambda "c" $ compoundStatementToExpr @@ var "c"]

stringToExpr :: TTermDefinition (Py.String_ -> Expr)
stringToExpr = def "stringToExpr" $
  doc "Serialize a Python string literal" $
  lambda "s" $ lets [
    "content">: project Py._String Py._String_value @@ var "s",
    "style">: project Py._String Py._String_quoteStyle @@ var "s"] $
    cases Py._QuoteStyle (var "style") Nothing [
      Py._QuoteStyle_single>>: constant $ Serialization.cst @@ (escapePythonString @@ false @@ var "content"),
      Py._QuoteStyle_double>>: constant $ Serialization.cst @@ (escapePythonString @@ true @@ var "content"),
      Py._QuoteStyle_triple>>: constant $ Serialization.noSep @@ list [
        Serialization.cst @@ string "r\"\"\"",
        Serialization.cst @@ var "content",
        Serialization.cst @@ string "\"\"\""]]

subjectExpressionToExpr :: TTermDefinition (Py.SubjectExpression -> Expr)
subjectExpressionToExpr = def "subjectExpressionToExpr" $
  doc "Serialize a subject expression" $
  lambda "se" $
    cases Py._SubjectExpression (var "se") Nothing [
      Py._SubjectExpression_simple>>: lambda "ne" $ namedExpressionToExpr @@ var "ne",
      Py._SubjectExpression_tuple>>: lambda "_" $ Serialization.cst @@ string "*..."]

sumToExpr :: TTermDefinition (Py.Sum -> Expr)
sumToExpr = def "sumToExpr" $
  doc "Serialize a sum expression" $
  lambda "s" $
    -- Just encode the term for now; sum operators (+/-) rarely used in generated code
    termToExpr @@ (project Py._Sum Py._Sum_rhs @@ var "s")

-- | Serialize a TPrimary (target-side primary expression)
tPrimaryToExpr :: TTermDefinition (Py.TPrimary -> Expr)
tPrimaryToExpr = def "tPrimaryToExpr" $
  doc "Serialize a target-side primary expression" $
  lambda "tp" $
    cases Py._TPrimary (var "tp") Nothing [
      Py._TPrimary_atom>>: lambda "a" $ atomToExpr @@ var "a",
      Py._TPrimary_primaryAndName>>: lambda "pn" $ tPrimaryAndNameToExpr @@ var "pn",
      Py._TPrimary_primaryAndSlices>>: lambda "_" $ Serialization.cst @@ string "...",
      Py._TPrimary_primaryAndGenexp>>: lambda "_" $ Serialization.cst @@ string "...",
      Py._TPrimary_primaryAndArguments>>: lambda "_" $ Serialization.cst @@ string "..."]

-- | Serialize a TPrimaryAndName (e.g., obj.attr)
tPrimaryAndNameToExpr :: TTermDefinition (Py.TPrimaryAndName -> Expr)
tPrimaryAndNameToExpr = def "tPrimaryAndNameToExpr" $
  doc "Serialize a TPrimaryAndName as primary.name" $
  lambda "pn" $ lets [
    "prim">: project Py._TPrimaryAndName Py._TPrimaryAndName_primary @@ var "pn",
    "name_">: project Py._TPrimaryAndName Py._TPrimaryAndName_name @@ var "pn"] $
    Serialization.noSep @@ list [tPrimaryToExpr @@ var "prim", Serialization.cst @@ string ".", nameToExpr @@ var "name_"]

targetWithStarAtomToExpr :: TTermDefinition (Py.TargetWithStarAtom -> Expr)
targetWithStarAtomToExpr = def "targetWithStarAtomToExpr" $
  doc "Serialize a target with star atom" $
  lambda "t" $
    cases Py._TargetWithStarAtom (var "t") Nothing [
      Py._TargetWithStarAtom_atom>>: lambda "a" $ starAtomToExpr @@ var "a",
      Py._TargetWithStarAtom_project>>: lambda "pn" $ tPrimaryAndNameToExpr @@ var "pn",
      Py._TargetWithStarAtom_slices>>: lambda "_" $ Serialization.cst @@ string "..."]

termToExpr :: TTermDefinition (Py.Term -> Expr)
termToExpr = def "termToExpr" $
  doc "Serialize a term expression" $
  lambda "t" $
    -- Just encode the factor; multiplication rarely used in generated code
    factorToExpr @@ (project Py._Term Py._Term_rhs @@ var "t")

tupleToExpr :: TTermDefinition (Py.Tuple -> Expr)
tupleToExpr = def "tupleToExpr" $
  doc "Serialize a Python tuple" $
  lambda "t" $ lets [
    "es">: unwrap Py._Tuple @@ var "t"] $
    Maybes.fromMaybe
      (Serialization.parenListAdaptive @@ Lists.map starNamedExpressionToExpr (var "es"))
      (Maybes.map
        (lambda "firstEs" $
          Logic.ifElse (Equality.equal (Lists.length (var "es")) (int32 1))
            (Serialization.parens @@ (Serialization.noSep @@ list [
              starNamedExpressionToExpr @@ var "firstEs",
              Serialization.cst @@ string ","]))
            (Serialization.parenListAdaptive @@ Lists.map starNamedExpressionToExpr (var "es")))
        (Lists.maybeHead (var "es")))

typeAliasToExpr :: TTermDefinition (Py.TypeAlias -> Expr)
typeAliasToExpr = def "typeAliasToExpr" $
  doc "Serialize a type alias" $
  lambda "ta" $ lets [
    "name">: project Py._TypeAlias Py._TypeAlias_name @@ var "ta",
    "tparams">: project Py._TypeAlias Py._TypeAlias_typeParams @@ var "ta",
    "expr">: project Py._TypeAlias Py._TypeAlias_expression @@ var "ta",
    "alias">: Serialization.noSep @@ Maybes.cat (list [
      just $ nameToExpr @@ var "name",
      Logic.ifElse (Lists.null (var "tparams"))
        nothing
        (just $ Serialization.bracketList @@ Serialization.inlineStyle @@ Lists.map typeParameterToExpr (var "tparams"))])] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "type",
      var "alias",
      Serialization.cst @@ string "=",
      expressionToExpr @@ var "expr"]

typeParameterToExpr :: TTermDefinition (Py.TypeParameter -> Expr)
typeParameterToExpr = def "typeParameterToExpr" $
  doc "Serialize a type parameter" $
  lambda "tp" $
    cases Py._TypeParameter (var "tp") Nothing [
      Py._TypeParameter_simple>>: lambda "s" $ simpleTypeParameterToExpr @@ var "s",
      Py._TypeParameter_star>>: lambda "_" $ Serialization.cst @@ string "*...",
      Py._TypeParameter_doubleStar>>: lambda "_" $ Serialization.cst @@ string "**..."]

typedAssignmentToExpr :: TTermDefinition (Py.TypedAssignment -> Expr)
typedAssignmentToExpr = def "typedAssignmentToExpr" $
  doc "Serialize a typed assignment" $
  lambda "ta" $ lets [
    "lhs">: project Py._TypedAssignment Py._TypedAssignment_lhs @@ var "ta",
    "typ">: project Py._TypedAssignment Py._TypedAssignment_type @@ var "ta",
    "rhs">: project Py._TypedAssignment Py._TypedAssignment_rhs @@ var "ta"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.noSep @@ list [singleTargetToExpr @@ var "lhs", Serialization.cst @@ string ":"],
      just $ expressionToExpr @@ var "typ",
      Maybes.map annotatedRhsToExpr (var "rhs")])

untypedAssignmentToExpr :: TTermDefinition (Py.UntypedAssignment -> Expr)
untypedAssignmentToExpr = def "untypedAssignmentToExpr" $
  doc "Serialize an untyped assignment" $
  lambda "ua" $ lets [
    "targets">: project Py._UntypedAssignment Py._UntypedAssignment_targets @@ var "ua",
    "rhs">: project Py._UntypedAssignment Py._UntypedAssignment_rhs @@ var "ua"] $
    Serialization.spaceSep @@ Lists.concat (list [
      Lists.map starTargetToExpr (var "targets"),
      list [annotatedRhsToExpr @@ var "rhs"]])

valuePatternToExpr :: TTermDefinition (Py.ValuePattern -> Expr)
valuePatternToExpr = def "valuePatternToExpr" $
  doc "Serialize a value pattern" $
  lambda "vp" $
    attributeToExpr @@ (unwrap Py._ValuePattern @@ var "vp")

whileStatementToExpr :: TTermDefinition (Py.WhileStatement -> Expr)
whileStatementToExpr = def "whileStatementToExpr" $
  doc "Serialize a while statement" $
  lambda "ws" $ lets [
    "cond">: project Py._WhileStatement Py._WhileStatement_condition @@ var "ws",
    "body">: project Py._WhileStatement Py._WhileStatement_body @@ var "ws",
    "else_">: project Py._WhileStatement Py._WhileStatement_else @@ var "ws"] $
    Serialization.newlineSep @@ Maybes.cat (list [
      just $ Serialization.newlineSep @@ list [
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "while",
          Serialization.noSep @@ list [
            namedExpressionToExpr @@ var "cond",
            Serialization.cst @@ string ":"]],
        blockToExpr @@ var "body"],
      Maybes.map (lambda "eb" $
        Serialization.newlineSep @@ list [
          Serialization.cst @@ string "else:",
          blockToExpr @@ var "eb"])
        (var "else_")])

escapePythonString :: TTermDefinition (Bool -> String -> String)
escapePythonString = def "escapePythonString" $
  doc "Escape special characters in a Python string and wrap in quotes" $
  lambda "doubleQuoted" $ lambda "s" $ lets [
    -- Helper to replace a substring
    "replace">: lambda "old" $ lambda "new" $ lambda "str" $
      Strings.intercalate (var "new") (Strings.splitOn (var "old") (var "str")),
    -- Escape backslashes first (must be first!)
    "s1">: var "replace" @@ string "\\" @@ string "\\\\" @@ var "s",
    -- Escape null bytes
    "s2">: var "replace" @@ string "\0" @@ string "\\x00" @@ var "s1",
    -- Escape newlines
    "s3">: var "replace" @@ string "\n" @@ string "\\n" @@ var "s2",
    -- Escape tabs
    "s4">: var "replace" @@ string "\t" @@ string "\\t" @@ var "s3",
    -- Escape carriage returns
    "s5">: var "replace" @@ string "\r" @@ string "\\r" @@ var "s4",
    -- Escape quotes based on quote style
    "escaped">: Logic.ifElse (var "doubleQuoted")
      (var "replace" @@ string "\"" @@ string "\\\"" @@ var "s5")
      (var "replace" @@ string "'" @@ string "\\'" @@ var "s5"),
    -- Add surrounding quotes
    "quote">: Logic.ifElse (var "doubleQuoted") (string "\"") (string "'")] $
    Strings.cat2 (var "quote") (Strings.cat2 (var "escaped") (var "quote"))

-- | Convert a showBigfloat result into valid Python source syntax, mapping
-- NaN and ±Infinity to float() constructor calls.
pythonFloatLiteralText :: TTermDefinition (String -> String)
pythonFloatLiteralText = def "pythonFloatLiteralText" $
  lambda "s" $
    Logic.ifElse (Equality.equal (var "s") (string "NaN")) (string "float('nan')") $
    Logic.ifElse (Equality.equal (var "s") (string "Infinity")) (string "float('inf')") $
    Logic.ifElse (Equality.equal (var "s") (string "-Infinity")) (string "float('-inf')")
      (var "s")

toPythonComments :: TTermDefinition (String -> String)
toPythonComments = def "toPythonComments" $
  doc ("Convert a doc string to Python comment format. Empty source lines"
    <> " emit `#` (no trailing space) so blank comment lines don't carry"
    <> " trailing whitespace into the generated file.") $
  lambda "doc_" $
    Logic.ifElse (Equality.equal (var "doc_") (string ""))
      (string "")
      (Strings.intercalate (string "\n") (Lists.map (lambda "line" $
          Logic.ifElse (Equality.equal (var "line") (string ""))
            (string "#")
            (Strings.cat2 (string "# ") (var "line")))
        (Strings.lines (var "doc_"))))
