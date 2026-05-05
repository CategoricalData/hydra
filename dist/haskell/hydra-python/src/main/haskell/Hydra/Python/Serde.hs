-- Note: this is an automatically generated file. Do not edit.
-- | Python serializer: converts Python AST to concrete syntax

module Hydra.Python.Serde where
import qualified Hydra.Ast as Ast
import qualified Hydra.Constants as Constants
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Python.Syntax as Syntax
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Serialize an annotated RHS
annotatedRhsToExpr :: Syntax.AnnotatedRhs -> Ast.Expr
annotatedRhsToExpr arhs =
    Serialization.spaceSep [
      Serialization.cst "=",
      case arhs of
        Syntax.AnnotatedRhsStar v0 -> Serialization.commaSep Serialization.inlineStyle (Lists.map starExpressionToExpr v0)
        Syntax.AnnotatedRhsYield _ -> Serialization.cst "yield ..."]
-- | Serialize an annotated statement (with optional doc comment)
annotatedStatementToExpr :: Syntax.AnnotatedStatement -> Ast.Expr
annotatedStatementToExpr as_ =

      let doc_ = Syntax.annotatedStatementComment as_
          stmt = Syntax.annotatedStatementStatement as_
      in (Serialization.newlineSep [
        Serialization.cst (toPythonComments doc_),
        (statementToExpr stmt)])
-- | Serialize a type annotation
annotationToExpr :: Syntax.Annotation -> Ast.Expr
annotationToExpr ann =
    Serialization.spaceSep [
      Serialization.cst ":",
      (expressionToExpr (Syntax.unAnnotation ann))]
-- | Serialize function arguments
argsToExpr :: Syntax.Args -> Ast.Expr
argsToExpr args =

      let pos = Syntax.argsPositional args
          ks = Syntax.argsKwargOrStarred args
          kss = Syntax.argsKwargOrDoubleStarred args
      in (Serialization.commaSepAdaptive (Lists.concat [
        Lists.map posArgToExpr pos,
        (Lists.map kwargOrStarredToExpr ks),
        (Lists.map kwargOrDoubleStarredToExpr kss)]))
-- | Serialize an assignment expression (walrus operator)
assignmentExpressionToExpr :: Syntax.AssignmentExpression -> Ast.Expr
assignmentExpressionToExpr ae =

      let name = Syntax.assignmentExpressionName ae
          expr = Syntax.assignmentExpressionExpression ae
      in (Serialization.spaceSep [
        nameToExpr name,
        (Serialization.cst ":="),
        (expressionToExpr expr)])
-- | Serialize an assignment
assignmentToExpr :: Syntax.Assignment -> Ast.Expr
assignmentToExpr a =
    case a of
      Syntax.AssignmentTyped v0 -> typedAssignmentToExpr v0
      Syntax.AssignmentUntyped v0 -> untypedAssignmentToExpr v0
      Syntax.AssignmentAug _ -> Serialization.cst "... += ..."
-- | Serialize a Python atom (literal or basic expression)
atomToExpr :: Syntax.Atom -> Ast.Expr
atomToExpr atom =
    case atom of
      Syntax.AtomDict v0 -> dictToExpr v0
      Syntax.AtomDictcomp _ -> Serialization.cst "{...}"
      Syntax.AtomEllipsis -> Serialization.cst "..."
      Syntax.AtomFalse -> Serialization.cst "False"
      Syntax.AtomGenexp _ -> Serialization.cst "(...)"
      Syntax.AtomGroup v0 -> groupToExpr v0
      Syntax.AtomList v0 -> listToExpr v0
      Syntax.AtomListcomp _ -> Serialization.cst "[...]"
      Syntax.AtomName v0 -> nameToExpr v0
      Syntax.AtomNone -> Serialization.cst "None"
      Syntax.AtomNumber v0 -> numberToExpr v0
      Syntax.AtomSet v0 -> setToExpr v0
      Syntax.AtomSetcomp _ -> Serialization.cst "{...}"
      Syntax.AtomString v0 -> stringToExpr v0
      Syntax.AtomTrue -> Serialization.cst "True"
      Syntax.AtomTuple v0 -> tupleToExpr v0
-- | Serialize an attribute access
attributeToExpr :: Syntax.Attribute -> Ast.Expr
attributeToExpr attr = Serialization.dotSep (Lists.map nameToExpr (Syntax.unAttribute attr))
-- | Serialize an await primary expression
awaitPrimaryToExpr :: Syntax.AwaitPrimary -> Ast.Expr
awaitPrimaryToExpr ap =

      let await_ = Syntax.awaitPrimaryAwait ap
          primary = Syntax.awaitPrimaryPrimary ap
      in (Logic.ifElse await_ (Serialization.spaceSep [
        Serialization.cst "await",
        (primaryToExpr primary)]) (primaryToExpr primary))
-- | Serialize a bitwise AND expression
bitwiseAndToExpr :: Syntax.BitwiseAnd -> Ast.Expr
bitwiseAndToExpr band =

      let lhs = Syntax.bitwiseAndLhs band
          rhs = Syntax.bitwiseAndRhs band
      in (Serialization.spaceSep (Maybes.cat [
        Maybes.map (\l -> Serialization.spaceSep [
          bitwiseAndToExpr l,
          (Serialization.cst "&")]) lhs,
        (Just (shiftExpressionToExpr rhs))]))
-- | Serialize a bitwise OR expression
bitwiseOrToExpr :: Syntax.BitwiseOr -> Ast.Expr
bitwiseOrToExpr bor =

      let lhs = Syntax.bitwiseOrLhs bor
          rhs = Syntax.bitwiseOrRhs bor
      in (Serialization.spaceSep (Maybes.cat [
        Maybes.map (\l -> Serialization.spaceSep [
          bitwiseOrToExpr l,
          (Serialization.cst "|")]) lhs,
        (Just (bitwiseXorToExpr rhs))]))
-- | Serialize a bitwise XOR expression
bitwiseXorToExpr :: Syntax.BitwiseXor -> Ast.Expr
bitwiseXorToExpr bxor =

      let lhs = Syntax.bitwiseXorLhs bxor
          rhs = Syntax.bitwiseXorRhs bxor
      in (Serialization.spaceSep (Maybes.cat [
        Maybes.map (\l -> Serialization.spaceSep [
          bitwiseXorToExpr l,
          (Serialization.cst "^")]) lhs,
        (Just (bitwiseAndToExpr rhs))]))
-- | Serialize a block
blockToExpr :: Syntax.Block -> Ast.Expr
blockToExpr b =
    case b of
      Syntax.BlockIndented v0 -> Serialization.tabIndentDoubleSpace (Lists.map (\stmts -> Serialization.newlineSep (Lists.map statementToExpr stmts)) v0)
      Syntax.BlockSimple v0 -> Serialization.semicolonSep (Lists.map simpleStatementToExpr v0)
-- | Serialize a capture pattern
capturePatternToExpr :: Syntax.CapturePattern -> Ast.Expr
capturePatternToExpr cp = patternCaptureTargetToExpr (Syntax.unCapturePattern cp)
-- | Serialize a case block
caseBlockToExpr :: Syntax.CaseBlock -> Ast.Expr
caseBlockToExpr cb =

      let patterns = Syntax.caseBlockPatterns cb
          guard = Syntax.caseBlockGuard cb
          body = Syntax.caseBlockBody cb
      in (Serialization.newlineSep [
        Serialization.noSep [
          Serialization.spaceSep (Maybes.cat [
            Just (Serialization.cst "case"),
            (Just (patternsToExpr patterns)),
            (Maybes.map guardToExpr guard)]),
          (Serialization.cst ":")],
        (blockToExpr body)])
-- | Serialize a class definition
classDefinitionToExpr :: Syntax.ClassDefinition -> Ast.Expr
classDefinitionToExpr cd =

      let decs = Syntax.classDefinitionDecorators cd
          name = Syntax.classDefinitionName cd
          args = Syntax.classDefinitionArguments cd
          body = Syntax.classDefinitionBody cd
          argPart =
                  Maybes.map (\a -> Serialization.noSep [
                    Serialization.cst "(",
                    (argsToExpr a),
                    (Serialization.cst ")")]) args
      in (Serialization.newlineSep (Maybes.cat [
        Maybes.map decoratorsToExpr decs,
        (Just (Serialization.noSep (Maybes.cat [
          Just (Serialization.spaceSep [
            Serialization.cst "class",
            (nameToExpr name)]),
          argPart,
          (Just (Serialization.cst ":"))]))),
        (Just (blockToExpr body))]))
-- | Serialize a class pattern
classPatternToExpr :: Syntax.ClassPattern -> Ast.Expr
classPatternToExpr cp =

      let noa = Syntax.classPatternNameOrAttribute cp
          pos = Syntax.classPatternPositionalPatterns cp
          kw = Syntax.classPatternKeywordPatterns cp
      in (Serialization.noSep (Maybes.cat [
        Just (nameOrAttributeToExpr noa),
        (Just (Serialization.cst "(")),
        (Maybes.map positionalPatternsToExpr pos),
        (Maybes.map keywordPatternsToExpr kw),
        (Just (Serialization.cst ")"))]))
-- | Serialize a closed pattern
closedPatternToExpr :: Syntax.ClosedPattern -> Ast.Expr
closedPatternToExpr cp =
    case cp of
      Syntax.ClosedPatternLiteral _ -> Serialization.cst "..."
      Syntax.ClosedPatternCapture v0 -> capturePatternToExpr v0
      Syntax.ClosedPatternWildcard -> Serialization.cst "_"
      Syntax.ClosedPatternValue v0 -> valuePatternToExpr v0
      Syntax.ClosedPatternGroup _ -> Serialization.cst "(...)"
      Syntax.ClosedPatternSequence _ -> Serialization.cst "[...]"
      Syntax.ClosedPatternMapping _ -> Serialization.cst "{...}"
      Syntax.ClosedPatternClass v0 -> classPatternToExpr v0
-- | Serialize a comparison expression
comparisonToExpr :: Syntax.Comparison -> Ast.Expr
comparisonToExpr cmp = bitwiseOrToExpr (Syntax.comparisonLhs cmp)
-- | Serialize a compound (multi-line) Python statement
compoundStatementToExpr :: Syntax.CompoundStatement -> Ast.Expr
compoundStatementToExpr cs =
    case cs of
      Syntax.CompoundStatementFunction v0 -> functionDefinitionToExpr v0
      Syntax.CompoundStatementIf _ -> Serialization.cst "if ..."
      Syntax.CompoundStatementClassDef v0 -> classDefinitionToExpr v0
      Syntax.CompoundStatementWith _ -> Serialization.cst "with ..."
      Syntax.CompoundStatementFor _ -> Serialization.cst "for ..."
      Syntax.CompoundStatementTry _ -> Serialization.cst "try ..."
      Syntax.CompoundStatementWhile v0 -> whileStatementToExpr v0
      Syntax.CompoundStatementMatch v0 -> matchStatementToExpr v0
-- | Serialize a conditional expression (ternary)
conditionalToExpr :: Syntax.Conditional -> Ast.Expr
conditionalToExpr c =

      let body = Syntax.conditionalBody c
          cond = Syntax.conditionalIf c
          elseExpr = Syntax.conditionalElse c
      in (Serialization.spaceSep [
        disjunctionToExpr body,
        (Serialization.cst "if"),
        (disjunctionToExpr cond),
        (Serialization.cst "else"),
        (expressionToExpr elseExpr)])
-- | Serialize a conjunction (and expression)
conjunctionToExpr :: Syntax.Conjunction -> Ast.Expr
conjunctionToExpr c =
    Serialization.symbolSep "and" Serialization.inlineStyle (Lists.map inversionToExpr (Syntax.unConjunction c))
-- | Serialize decorators
decoratorsToExpr :: Syntax.Decorators -> Ast.Expr
decoratorsToExpr decs =
    Serialization.newlineSep (Lists.map (\ne -> Serialization.noSep [
      Serialization.cst "@",
      (namedExpressionToExpr ne)]) (Syntax.unDecorators decs))
-- | Serialize a Python dictionary
dictToExpr :: Syntax.Dict -> Ast.Expr
dictToExpr d =
    Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map doubleStarredKvpairToExpr (Syntax.unDict d))
-- | Serialize a disjunction (or expression)
disjunctionToExpr :: Syntax.Disjunction -> Ast.Expr
disjunctionToExpr d =
    Serialization.symbolSep "or" Serialization.inlineStyle (Lists.map conjunctionToExpr (Syntax.unDisjunction d))
-- | Serialize a dotted as name
dottedAsNameToExpr :: Syntax.DottedAsName -> Ast.Expr
dottedAsNameToExpr dan =

      let name = Syntax.dottedAsNameName dan
          alias = Syntax.dottedAsNameAs dan
      in (Serialization.spaceSep (Maybes.cat [
        Just (dottedNameToExpr name),
        (Maybes.map (\a -> Serialization.spaceSep [
          Serialization.cst "as",
          (nameToExpr a)]) alias)]))
-- | Serialize a dotted name (e.g., module.submodule)
dottedNameToExpr :: Syntax.DottedName -> Ast.Expr
dottedNameToExpr dn = Serialization.cst (Strings.intercalate "." (Lists.map (\n -> Syntax.unName n) (Syntax.unDottedName dn)))
-- | Serialize a double-starred key-value pair
doubleStarredKvpairToExpr :: Syntax.DoubleStarredKvpair -> Ast.Expr
doubleStarredKvpairToExpr dskv =
    case dskv of
      Syntax.DoubleStarredKvpairPair v0 -> kvpairToExpr v0
      Syntax.DoubleStarredKvpairStarred v0 -> Serialization.noSep [
        Serialization.cst "**",
        (bitwiseOrToExpr v0)]
-- | Escape special characters in a Python string and wrap in quotes
escapePythonString :: Bool -> String -> String
escapePythonString doubleQuoted s =

      let replace = \old -> \new -> \str -> Strings.intercalate new (Strings.splitOn old str)
          s1 = replace "\\" "\\\\" s
          s2 = replace "\NUL" "\\x00" s1
          s3 = replace "\n" "\\n" s2
          s4 = replace "\t" "\\t" s3
          s5 = replace "\r" "\\r" s4
          escaped = Logic.ifElse doubleQuoted (replace "\"" "\\\"" s5) (replace "'" "\\'" s5)
          quote = Logic.ifElse doubleQuoted "\"" "'"
      in (Strings.cat2 quote (Strings.cat2 escaped quote))
-- | Serialize a Python expression
expressionToExpr :: Syntax.Expression -> Ast.Expr
expressionToExpr expr =
    case expr of
      Syntax.ExpressionSimple v0 -> disjunctionToExpr v0
      Syntax.ExpressionConditional v0 -> conditionalToExpr v0
      Syntax.ExpressionLambda v0 -> lambdaToExpr v0
-- | Serialize a factor expression
factorToExpr :: Syntax.Factor -> Ast.Expr
factorToExpr f =
    case f of
      Syntax.FactorPositive v0 -> Serialization.noSep [
        Serialization.cst "+",
        (factorToExpr v0)]
      Syntax.FactorNegative v0 -> Serialization.noSep [
        Serialization.cst "-",
        (factorToExpr v0)]
      Syntax.FactorComplement v0 -> Serialization.noSep [
        Serialization.cst "~",
        (factorToExpr v0)]
      Syntax.FactorSimple v0 -> powerToExpr v0
-- | Serialize a raw function definition
functionDefRawToExpr :: Syntax.FunctionDefRaw -> Ast.Expr
functionDefRawToExpr fdr =

      let async_ = Syntax.functionDefRawAsync fdr
          name = Syntax.functionDefRawName fdr
          tparams = Syntax.functionDefRawTypeParams fdr
          params = Syntax.functionDefRawParams fdr
          retType = Syntax.functionDefRawReturnType fdr
          block = Syntax.functionDefRawBlock fdr
          asyncKw = Logic.ifElse async_ (Just (Serialization.cst "async")) Nothing
          tparamPart =
                  Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.bracketList Serialization.inlineStyle (Lists.map typeParameterToExpr tparams)))
          paramPart = Maybes.map parametersToExpr params
          retPart =
                  Maybes.map (\t -> Serialization.spaceSep [
                    Serialization.cst "->",
                    (expressionToExpr t)]) retType
      in (Serialization.newlineSep [
        Serialization.noSep [
          Serialization.spaceSep (Maybes.cat [
            asyncKw,
            (Just (Serialization.cst "def")),
            (Just (Serialization.noSep (Maybes.cat [
              Just (nameToExpr name),
              tparamPart,
              (Just (Serialization.cst "(")),
              paramPart,
              (Just (Serialization.cst ")"))]))),
            retPart]),
          (Serialization.cst ":")],
        (blockToExpr block)])
-- | Serialize a function definition
functionDefinitionToExpr :: Syntax.FunctionDefinition -> Ast.Expr
functionDefinitionToExpr fd =

      let decs = Syntax.functionDefinitionDecorators fd
          raw = Syntax.functionDefinitionRaw fd
      in (Serialization.newlineSep (Maybes.cat [
        Maybes.map decoratorsToExpr decs,
        (Just (functionDefRawToExpr raw))]))
-- | Serialize a parenthesized group
groupToExpr :: Syntax.Group -> Ast.Expr
groupToExpr g =
    case g of
      Syntax.GroupExpression v0 -> namedExpressionToExpr v0
      Syntax.GroupYield _ -> Serialization.cst "(yield ...)"
-- | Serialize a guard clause
guardToExpr :: Syntax.Guard -> Ast.Expr
guardToExpr g =
    Serialization.spaceSep [
      Serialization.cst "if",
      (namedExpressionToExpr (Syntax.unGuard g))]
-- | Serialize an import from as name
importFromAsNameToExpr :: Syntax.ImportFromAsName -> Ast.Expr
importFromAsNameToExpr ifan =

      let name = Syntax.importFromAsNameName ifan
          alias = Syntax.importFromAsNameAs ifan
      in (Maybes.maybe (nameToExpr name) (\a -> Serialization.spaceSep [
        nameToExpr name,
        (Serialization.cst "as"),
        (nameToExpr a)]) alias)
-- | Serialize import from targets
importFromTargetsToExpr :: Syntax.ImportFromTargets -> Ast.Expr
importFromTargetsToExpr t =
    case t of
      Syntax.ImportFromTargetsSimple v0 -> Serialization.commaSep Serialization.inlineStyle (Lists.map importFromAsNameToExpr v0)
      Syntax.ImportFromTargetsParens v0 -> Serialization.noSep [
        Serialization.cst "(",
        (Serialization.commaSep Serialization.inlineStyle (Lists.map importFromAsNameToExpr v0)),
        (Serialization.cst ")")]
      Syntax.ImportFromTargetsStar -> Serialization.cst "*"
-- | Serialize an import from statement
importFromToExpr :: Syntax.ImportFrom -> Ast.Expr
importFromToExpr if_ =

      let prefixes = Syntax.importFromPrefixes if_
          name = Syntax.importFromDottedName if_
          targets = Syntax.importFromTargets if_
          lhs =
                  Serialization.noSep (Maybes.cat (Lists.concat [
                    Lists.map (\p -> Just (relativeImportPrefixToExpr p)) prefixes,
                    [
                      Maybes.map dottedNameToExpr name]]))
      in (Serialization.spaceSep [
        Serialization.cst "from",
        lhs,
        (Serialization.cst "import"),
        (importFromTargetsToExpr targets)])
-- | Serialize an import name
importNameToExpr :: Syntax.ImportName -> Ast.Expr
importNameToExpr in_ =
    Serialization.spaceSep [
      Serialization.cst "import",
      (Serialization.commaSep Serialization.inlineStyle (Lists.map dottedAsNameToExpr (Syntax.unImportName in_)))]
-- | Serialize an import statement
importStatementToExpr :: Syntax.ImportStatement -> Ast.Expr
importStatementToExpr is_ =
    case is_ of
      Syntax.ImportStatementName v0 -> importNameToExpr v0
      Syntax.ImportStatementFrom v0 -> importFromToExpr v0
-- | Serialize an inversion (not expression)
inversionToExpr :: Syntax.Inversion -> Ast.Expr
inversionToExpr i =
    case i of
      Syntax.InversionNot v0 -> Serialization.spaceSep [
        Serialization.cst "not",
        (inversionToExpr v0)]
      Syntax.InversionSimple v0 -> comparisonToExpr v0
-- | Serialize a keyword pattern
keywordPatternToExpr :: Syntax.KeywordPattern -> Ast.Expr
keywordPatternToExpr kp =

      let name = Syntax.keywordPatternName kp
          pat = Syntax.keywordPatternPattern kp
      in (Serialization.noSep [
        nameToExpr name,
        (Serialization.cst "="),
        (patternToExpr pat)])
-- | Serialize keyword patterns
keywordPatternsToExpr :: Syntax.KeywordPatterns -> Ast.Expr
keywordPatternsToExpr kp =
    Serialization.commaSep Serialization.inlineStyle (Lists.map keywordPatternToExpr (Syntax.unKeywordPatterns kp))
-- | Serialize a key-value pair
kvpairToExpr :: Syntax.Kvpair -> Ast.Expr
kvpairToExpr kv =

      let k = Syntax.kvpairKey kv
          v = Syntax.kvpairValue kv
      in (Serialization.spaceSep [
        Serialization.noSep [
          expressionToExpr k,
          (Serialization.cst ":")],
        (expressionToExpr v)])
-- | Serialize a kwarg or double starred
kwargOrDoubleStarredToExpr :: Syntax.KwargOrDoubleStarred -> Ast.Expr
kwargOrDoubleStarredToExpr kds =
    case kds of
      Syntax.KwargOrDoubleStarredKwarg v0 -> kwargToExpr v0
      Syntax.KwargOrDoubleStarredDoubleStarred v0 -> Serialization.noSep [
        Serialization.cst "**",
        (expressionToExpr v0)]
-- | Serialize a kwarg or starred
kwargOrStarredToExpr :: Syntax.KwargOrStarred -> Ast.Expr
kwargOrStarredToExpr ks =
    case ks of
      Syntax.KwargOrStarredKwarg v0 -> kwargToExpr v0
      Syntax.KwargOrStarredStarred v0 -> starredExpressionToExpr v0
-- | Serialize a keyword argument
kwargToExpr :: Syntax.Kwarg -> Ast.Expr
kwargToExpr k =

      let name = Syntax.kwargName k
          expr = Syntax.kwargValue k
      in (Serialization.noSep [
        nameToExpr name,
        (Serialization.cst "="),
        (expressionToExpr expr)])
-- | Serialize a lambda parameter without default
lambdaParamNoDefaultToExpr :: Syntax.LambdaParamNoDefault -> Ast.Expr
lambdaParamNoDefaultToExpr p = nameToExpr (Syntax.unLambdaParamNoDefault p)
-- | Serialize lambda parameters
lambdaParametersToExpr :: Syntax.LambdaParameters -> Ast.Expr
lambdaParametersToExpr lp =

      let nodef = Syntax.lambdaParametersParamNoDefault lp
      in (Serialization.commaSep Serialization.inlineStyle (Lists.map lambdaParamNoDefaultToExpr nodef))
-- | Serialize lambda star etc
lambdaStarEtcToExpr :: Syntax.LambdaStarEtc -> Ast.Expr
lambdaStarEtcToExpr lse =
    case lse of
      Syntax.LambdaStarEtcParamNoDefault v0 -> lambdaParamNoDefaultToExpr v0
      Syntax.LambdaStarEtcStar _ -> Serialization.cst "*..."
      Syntax.LambdaStarEtcParamMaybeDefault _ -> Serialization.cst "..."
      Syntax.LambdaStarEtcKwds _ -> Serialization.cst "**..."
-- | Serialize a lambda expression
lambdaToExpr :: Syntax.Lambda -> Ast.Expr
lambdaToExpr l =

      let params = Syntax.lambdaParams l
          body = Syntax.lambdaBody l
      in (Serialization.parens (Serialization.spaceSep [
        Serialization.cst "lambda",
        (Serialization.noSep [
          lambdaParametersToExpr params,
          (Serialization.cst ":")]),
        (expressionToExpr body)]))
-- | Serialize a Python list
listToExpr :: Syntax.List -> Ast.Expr
listToExpr l = Serialization.bracketListAdaptive (Lists.map starNamedExpressionToExpr (Syntax.unList l))
-- | Serialize a match statement
matchStatementToExpr :: Syntax.MatchStatement -> Ast.Expr
matchStatementToExpr ms =

      let subj = Syntax.matchStatementSubject ms
          cases = Syntax.matchStatementCases ms
      in (Serialization.newlineSep [
        Serialization.spaceSep [
          Serialization.cst "match",
          (Serialization.noSep [
            subjectExpressionToExpr subj,
            (Serialization.cst ":")])],
        (Serialization.tabIndentDoubleSpace (Lists.map caseBlockToExpr cases))])
-- | Serialize a Python module to an AST expression
moduleToExpr :: Syntax.Module -> Ast.Expr
moduleToExpr mod =

      let warning = Serialization.cst (toPythonComments Constants.warningAutoGeneratedFile)
          groups = Lists.map (\group -> Serialization.newlineSep (Lists.map statementToExpr group)) (Syntax.unModule mod)
      in (Serialization.doubleNewlineSep (Lists.cons warning groups))
-- | Serialize a name or attribute
nameOrAttributeToExpr :: Syntax.NameOrAttribute -> Ast.Expr
nameOrAttributeToExpr noa = Serialization.dotSep (Lists.map nameToExpr (Syntax.unNameOrAttribute noa))
-- | Serialize a Python name/identifier
nameToExpr :: Syntax.Name -> Ast.Expr
nameToExpr n = Serialization.cst (Syntax.unName n)
-- | Serialize a named expression
namedExpressionToExpr :: Syntax.NamedExpression -> Ast.Expr
namedExpressionToExpr ne =
    case ne of
      Syntax.NamedExpressionSimple v0 -> expressionToExpr v0
      Syntax.NamedExpressionAssignment v0 -> assignmentExpressionToExpr v0
-- | Serialize a Python number literal
numberToExpr :: Syntax.Number -> Ast.Expr
numberToExpr num =
    case num of
      Syntax.NumberFloat v0 -> Serialization.cst (pythonFloatLiteralText (Literals.showBigfloat v0))
      Syntax.NumberInteger v0 -> Serialization.cst (Literals.showBigint v0)
-- | Serialize an or pattern
orPatternToExpr :: Syntax.OrPattern -> Ast.Expr
orPatternToExpr op =
    Serialization.symbolSep "|" Serialization.inlineStyle (Lists.map closedPatternToExpr (Syntax.unOrPattern op))
-- | Serialize parameters without defaults
paramNoDefaultParametersToExpr :: Syntax.ParamNoDefaultParameters -> Ast.Expr
paramNoDefaultParametersToExpr pndp =

      let nodef = Syntax.paramNoDefaultParametersParamNoDefault pndp
      in (Serialization.commaSepAdaptive (Lists.map paramNoDefaultToExpr nodef))
-- | Serialize a parameter without default
paramNoDefaultToExpr :: Syntax.ParamNoDefault -> Ast.Expr
paramNoDefaultToExpr pnd = paramToExpr (Syntax.paramNoDefaultParam pnd)
-- | Serialize a parameter
paramToExpr :: Syntax.Param -> Ast.Expr
paramToExpr p =

      let name = Syntax.paramName p
          ann = Syntax.paramAnnotation p
      in (Serialization.noSep (Maybes.cat [
        Just (nameToExpr name),
        (Maybes.map annotationToExpr ann)]))
-- | Serialize function parameters
parametersToExpr :: Syntax.Parameters -> Ast.Expr
parametersToExpr p =
    case p of
      Syntax.ParametersParamNoDefault v0 -> paramNoDefaultParametersToExpr v0
      Syntax.ParametersSlashNoDefault _ -> Serialization.cst "..."
      Syntax.ParametersSlashWithDefault _ -> Serialization.cst "..."
-- | Serialize a pattern capture target
patternCaptureTargetToExpr :: Syntax.PatternCaptureTarget -> Ast.Expr
patternCaptureTargetToExpr pct = nameToExpr (Syntax.unPatternCaptureTarget pct)
-- | Serialize a pattern
patternToExpr :: Syntax.Pattern -> Ast.Expr
patternToExpr p =
    case p of
      Syntax.PatternOr v0 -> orPatternToExpr v0
      Syntax.PatternAs _ -> Serialization.cst "... as ..."
-- | Serialize patterns
patternsToExpr :: Syntax.Patterns -> Ast.Expr
patternsToExpr ps =
    case ps of
      Syntax.PatternsPattern v0 -> patternToExpr v0
      Syntax.PatternsSequence _ -> Serialization.cst "..."
-- | Serialize a positional argument
posArgToExpr :: Syntax.PosArg -> Ast.Expr
posArgToExpr pa =
    case pa of
      Syntax.PosArgStarred v0 -> starredExpressionToExpr v0
      Syntax.PosArgAssignment v0 -> assignmentExpressionToExpr v0
      Syntax.PosArgExpression v0 -> expressionToExpr v0
-- | Serialize positional patterns
positionalPatternsToExpr :: Syntax.PositionalPatterns -> Ast.Expr
positionalPatternsToExpr pp =
    Serialization.commaSep Serialization.inlineStyle (Lists.map patternToExpr (Syntax.unPositionalPatterns pp))
-- | Serialize a power expression
powerToExpr :: Syntax.Power -> Ast.Expr
powerToExpr p =

      let lhs = Syntax.powerLhs p
          rhs = Syntax.powerRhs p
      in (Serialization.spaceSep (Maybes.cat [
        Just (awaitPrimaryToExpr lhs),
        (Maybes.map (\r -> Serialization.spaceSep [
          Serialization.cst "**",
          (factorToExpr r)]) rhs)]))
-- | Serialize a primary RHS
primaryRhsToExpr :: Syntax.PrimaryRhs -> Ast.Expr
primaryRhsToExpr rhs =
    case rhs of
      Syntax.PrimaryRhsCall v0 -> Serialization.noSep [
        Serialization.cst "(",
        (argsToExpr v0),
        (Serialization.cst ")")]
      Syntax.PrimaryRhsProject v0 -> Serialization.noSep [
        Serialization.cst ".",
        (nameToExpr v0)]
      Syntax.PrimaryRhsSlices v0 -> Serialization.noSep [
        Serialization.cst "[",
        (slicesToExpr v0),
        (Serialization.cst "]")]
      Syntax.PrimaryRhsGenexp _ -> Serialization.cst "[...]"
-- | Serialize a primary expression
primaryToExpr :: Syntax.Primary -> Ast.Expr
primaryToExpr p =
    case p of
      Syntax.PrimarySimple v0 -> atomToExpr v0
      Syntax.PrimaryCompound v0 -> primaryWithRhsToExpr v0
-- | Serialize a primary with RHS
primaryWithRhsToExpr :: Syntax.PrimaryWithRhs -> Ast.Expr
primaryWithRhsToExpr pwr =

      let prim = Syntax.primaryWithRhsPrimary pwr
          rhs = Syntax.primaryWithRhsRhs pwr
      in (Serialization.noSep [
        primaryToExpr prim,
        (primaryRhsToExpr rhs)])
pythonFloatLiteralText :: String -> String
pythonFloatLiteralText s =
    Logic.ifElse (Equality.equal s "NaN") "float('nan')" (Logic.ifElse (Equality.equal s "Infinity") "float('inf')" (Logic.ifElse (Equality.equal s "-Infinity") "float('-inf')" s))
-- | Serialize a raise expression
raiseExpressionToExpr :: Syntax.RaiseExpression -> Ast.Expr
raiseExpressionToExpr re =

      let expr = Syntax.raiseExpressionExpression re
          from_ = Syntax.raiseExpressionFrom re
      in (Serialization.spaceSep (Maybes.cat [
        Just (expressionToExpr expr),
        (Maybes.map (\f -> Serialization.spaceSep [
          Serialization.cst "from",
          (expressionToExpr f)]) from_)]))
-- | Serialize a raise statement
raiseStatementToExpr :: Syntax.RaiseStatement -> Ast.Expr
raiseStatementToExpr rs =
    Serialization.spaceSep (Maybes.cat [
      Just (Serialization.cst "raise"),
      (Maybes.map raiseExpressionToExpr (Syntax.unRaiseStatement rs))])
-- | Serialize a relative import prefix
relativeImportPrefixToExpr :: Syntax.RelativeImportPrefix -> Ast.Expr
relativeImportPrefixToExpr p =
    case p of
      Syntax.RelativeImportPrefixDot -> Serialization.cst "."
      Syntax.RelativeImportPrefixEllipsis -> Serialization.cst "..."
-- | Serialize a return statement
returnStatementToExpr :: Syntax.ReturnStatement -> Ast.Expr
returnStatementToExpr rs =
    Serialization.spaceSep [
      Serialization.cst "return",
      (Serialization.commaSep Serialization.inlineStyle (Lists.map starExpressionToExpr (Syntax.unReturnStatement rs)))]
-- | Serialize a Python set
setToExpr :: Syntax.Set -> Ast.Expr
setToExpr s = Serialization.bracesListAdaptive (Lists.map starNamedExpressionToExpr (Syntax.unSet s))
-- | Serialize a shift expression
shiftExpressionToExpr :: Syntax.ShiftExpression -> Ast.Expr
shiftExpressionToExpr se = sumToExpr (Syntax.shiftExpressionRhs se)
-- | Serialize a simple (single-line) Python statement
simpleStatementToExpr :: Syntax.SimpleStatement -> Ast.Expr
simpleStatementToExpr ss =
    case ss of
      Syntax.SimpleStatementAssignment v0 -> assignmentToExpr v0
      Syntax.SimpleStatementStarExpressions v0 -> Serialization.newlineSep (Lists.map starExpressionToExpr v0)
      Syntax.SimpleStatementReturn v0 -> returnStatementToExpr v0
      Syntax.SimpleStatementRaise v0 -> raiseStatementToExpr v0
      Syntax.SimpleStatementPass -> Serialization.cst "pass"
      Syntax.SimpleStatementBreak -> Serialization.cst "break"
      Syntax.SimpleStatementContinue -> Serialization.cst "continue"
      Syntax.SimpleStatementImport v0 -> importStatementToExpr v0
      Syntax.SimpleStatementTypeAlias v0 -> typeAliasToExpr v0
      Syntax.SimpleStatementAssert _ -> Serialization.cst "assert ..."
      Syntax.SimpleStatementGlobal _ -> Serialization.cst "global ..."
      Syntax.SimpleStatementNonlocal _ -> Serialization.cst "nonlocal ..."
      Syntax.SimpleStatementDel _ -> Serialization.cst "del ..."
-- | Serialize a simple type parameter
simpleTypeParameterToExpr :: Syntax.SimpleTypeParameter -> Ast.Expr
simpleTypeParameterToExpr stp = nameToExpr (Syntax.simpleTypeParameterName stp)
-- | Serialize a single target
singleTargetToExpr :: Syntax.SingleTarget -> Ast.Expr
singleTargetToExpr st =
    case st of
      Syntax.SingleTargetName v0 -> nameToExpr v0
      Syntax.SingleTargetParens _ -> Serialization.cst "(...)"
      Syntax.SingleTargetSubscriptAttributeTarget _ -> Serialization.cst "..."
-- | Serialize a slice or starred expression
sliceOrStarredExpressionToExpr :: Syntax.SliceOrStarredExpression -> Ast.Expr
sliceOrStarredExpressionToExpr s =
    case s of
      Syntax.SliceOrStarredExpressionSlice v0 -> sliceToExpr v0
      Syntax.SliceOrStarredExpressionStarred v0 -> starredExpressionToExpr v0
-- | Serialize a slice
sliceToExpr :: Syntax.Slice -> Ast.Expr
sliceToExpr s =
    case s of
      Syntax.SliceNamed v0 -> namedExpressionToExpr v0
      Syntax.SliceSlice_ _ -> Serialization.cst ":"
-- | Serialize slices
slicesToExpr :: Syntax.Slices -> Ast.Expr
slicesToExpr s =

      let hd = Syntax.slicesHead s
          tl = Syntax.slicesTail s
      in (Serialization.commaSep Serialization.inlineStyle (Lists.cons (sliceToExpr hd) (Lists.map sliceOrStarredExpressionToExpr tl)))
-- | Serialize a star atom
starAtomToExpr :: Syntax.StarAtom -> Ast.Expr
starAtomToExpr sa =
    case sa of
      Syntax.StarAtomName v0 -> nameToExpr v0
      Syntax.StarAtomTargetWithStarAtom _ -> Serialization.cst "(...)"
      Syntax.StarAtomStarTargetsTupleSeq _ -> Serialization.cst "(...)"
      Syntax.StarAtomStarTargetsListSeq _ -> Serialization.cst "[...]"
-- | Serialize a star expression
starExpressionToExpr :: Syntax.StarExpression -> Ast.Expr
starExpressionToExpr se =
    case se of
      Syntax.StarExpressionStar v0 -> Serialization.noSep [
        Serialization.cst "*",
        (bitwiseOrToExpr v0)]
      Syntax.StarExpressionSimple v0 -> expressionToExpr v0
-- | Serialize a star named expression
starNamedExpressionToExpr :: Syntax.StarNamedExpression -> Ast.Expr
starNamedExpressionToExpr sne =
    case sne of
      Syntax.StarNamedExpressionStar v0 -> Serialization.noSep [
        Serialization.cst "*",
        (bitwiseOrToExpr v0)]
      Syntax.StarNamedExpressionSimple v0 -> namedExpressionToExpr v0
-- | Serialize a star target
starTargetToExpr :: Syntax.StarTarget -> Ast.Expr
starTargetToExpr st =
    case st of
      Syntax.StarTargetUnstarred v0 -> targetWithStarAtomToExpr v0
      Syntax.StarTargetStarred v0 -> Serialization.noSep [
        Serialization.cst "*",
        (starTargetToExpr v0)]
-- | Serialize a starred expression
starredExpressionToExpr :: Syntax.StarredExpression -> Ast.Expr
starredExpressionToExpr se =
    Serialization.noSep [
      Serialization.cst "*",
      (expressionToExpr (Syntax.unStarredExpression se))]
-- | Serialize a Python statement
statementToExpr :: Syntax.Statement -> Ast.Expr
statementToExpr stmt =
    case stmt of
      Syntax.StatementAnnotated v0 -> annotatedStatementToExpr v0
      Syntax.StatementSimple v0 -> Serialization.newlineSep (Lists.map simpleStatementToExpr v0)
      Syntax.StatementCompound v0 -> compoundStatementToExpr v0
-- | Serialize a Python string literal
stringToExpr :: Syntax.String_ -> Ast.Expr
stringToExpr s =

      let content = Syntax.stringValue s
          style = Syntax.stringQuoteStyle s
      in case style of
        Syntax.QuoteStyleSingle -> Serialization.cst (escapePythonString False content)
        Syntax.QuoteStyleDouble -> Serialization.cst (escapePythonString True content)
        Syntax.QuoteStyleTriple -> Serialization.noSep [
          Serialization.cst "r\"\"\"",
          (Serialization.cst content),
          (Serialization.cst "\"\"\"")]
-- | Serialize a subject expression
subjectExpressionToExpr :: Syntax.SubjectExpression -> Ast.Expr
subjectExpressionToExpr se =
    case se of
      Syntax.SubjectExpressionSimple v0 -> namedExpressionToExpr v0
      Syntax.SubjectExpressionTuple _ -> Serialization.cst "*..."
-- | Serialize a sum expression
sumToExpr :: Syntax.Sum -> Ast.Expr
sumToExpr s = termToExpr (Syntax.sumRhs s)
-- | Serialize a TPrimaryAndName as primary.name
tPrimaryAndNameToExpr :: Syntax.TPrimaryAndName -> Ast.Expr
tPrimaryAndNameToExpr pn =

      let prim = Syntax.tPrimaryAndNamePrimary pn
          name_ = Syntax.tPrimaryAndNameName pn
      in (Serialization.noSep [
        tPrimaryToExpr prim,
        (Serialization.cst "."),
        (nameToExpr name_)])
-- | Serialize a target-side primary expression
tPrimaryToExpr :: Syntax.TPrimary -> Ast.Expr
tPrimaryToExpr tp =
    case tp of
      Syntax.TPrimaryAtom v0 -> atomToExpr v0
      Syntax.TPrimaryPrimaryAndName v0 -> tPrimaryAndNameToExpr v0
      Syntax.TPrimaryPrimaryAndSlices _ -> Serialization.cst "..."
      Syntax.TPrimaryPrimaryAndGenexp _ -> Serialization.cst "..."
      Syntax.TPrimaryPrimaryAndArguments _ -> Serialization.cst "..."
-- | Serialize a target with star atom
targetWithStarAtomToExpr :: Syntax.TargetWithStarAtom -> Ast.Expr
targetWithStarAtomToExpr t =
    case t of
      Syntax.TargetWithStarAtomAtom v0 -> starAtomToExpr v0
      Syntax.TargetWithStarAtomProject v0 -> tPrimaryAndNameToExpr v0
      Syntax.TargetWithStarAtomSlices _ -> Serialization.cst "..."
-- | Serialize a term expression
termToExpr :: Syntax.Term -> Ast.Expr
termToExpr t = factorToExpr (Syntax.termRhs t)
-- | Convert a doc string to Python comment format. Empty source lines emit `#` (no trailing space) so blank comment lines don't carry trailing whitespace into the generated file.
toPythonComments :: String -> String
toPythonComments doc_ =
    Logic.ifElse (Equality.equal doc_ "") "" (Strings.intercalate "\n" (Lists.map (\line -> Logic.ifElse (Equality.equal line "") "#" (Strings.cat2 "# " line)) (Strings.lines doc_)))
-- | Serialize a Python tuple
tupleToExpr :: Syntax.Tuple -> Ast.Expr
tupleToExpr t =

      let es = Syntax.unTuple t
      in (Maybes.fromMaybe (Serialization.parenListAdaptive (Lists.map starNamedExpressionToExpr es)) (Maybes.map (\firstEs -> Logic.ifElse (Equality.equal (Lists.length es) 1) (Serialization.parens (Serialization.noSep [
        starNamedExpressionToExpr firstEs,
        (Serialization.cst ",")])) (Serialization.parenListAdaptive (Lists.map starNamedExpressionToExpr es))) (Lists.maybeHead es)))
-- | Serialize a type alias
typeAliasToExpr :: Syntax.TypeAlias -> Ast.Expr
typeAliasToExpr ta =

      let name = Syntax.typeAliasName ta
          tparams = Syntax.typeAliasTypeParams ta
          expr = Syntax.typeAliasExpression ta
          alias =
                  Serialization.noSep (Maybes.cat [
                    Just (nameToExpr name),
                    (Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.bracketList Serialization.inlineStyle (Lists.map typeParameterToExpr tparams))))])
      in (Serialization.spaceSep [
        Serialization.cst "type",
        alias,
        (Serialization.cst "="),
        (expressionToExpr expr)])
-- | Serialize a type parameter
typeParameterToExpr :: Syntax.TypeParameter -> Ast.Expr
typeParameterToExpr tp =
    case tp of
      Syntax.TypeParameterSimple v0 -> simpleTypeParameterToExpr v0
      Syntax.TypeParameterStar _ -> Serialization.cst "*..."
      Syntax.TypeParameterDoubleStar _ -> Serialization.cst "**..."
-- | Serialize a typed assignment
typedAssignmentToExpr :: Syntax.TypedAssignment -> Ast.Expr
typedAssignmentToExpr ta =

      let lhs = Syntax.typedAssignmentLhs ta
          typ = Syntax.typedAssignmentType ta
          rhs = Syntax.typedAssignmentRhs ta
      in (Serialization.spaceSep (Maybes.cat [
        Just (Serialization.noSep [
          singleTargetToExpr lhs,
          (Serialization.cst ":")]),
        (Just (expressionToExpr typ)),
        (Maybes.map annotatedRhsToExpr rhs)]))
-- | Serialize an untyped assignment
untypedAssignmentToExpr :: Syntax.UntypedAssignment -> Ast.Expr
untypedAssignmentToExpr ua =

      let targets = Syntax.untypedAssignmentTargets ua
          rhs = Syntax.untypedAssignmentRhs ua
      in (Serialization.spaceSep (Lists.concat [
        Lists.map starTargetToExpr targets,
        [
          annotatedRhsToExpr rhs]]))
-- | Serialize a value pattern
valuePatternToExpr :: Syntax.ValuePattern -> Ast.Expr
valuePatternToExpr vp = attributeToExpr (Syntax.unValuePattern vp)
-- | Serialize a while statement
whileStatementToExpr :: Syntax.WhileStatement -> Ast.Expr
whileStatementToExpr ws =

      let cond = Syntax.whileStatementCondition ws
          body = Syntax.whileStatementBody ws
          else_ = Syntax.whileStatementElse ws
      in (Serialization.newlineSep (Maybes.cat [
        Just (Serialization.newlineSep [
          Serialization.spaceSep [
            Serialization.cst "while",
            (Serialization.noSep [
              namedExpressionToExpr cond,
              (Serialization.cst ":")])],
          (blockToExpr body)]),
        (Maybes.map (\eb -> Serialization.newlineSep [
          Serialization.cst "else:",
          (blockToExpr eb)]) else_)]))
