-- Note: this is an automatically generated file. Do not edit.

-- | Python serializer: converts Python AST to concrete syntax

module Hydra.Ext.Python.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Constants as Constants
import qualified Hydra.Ext.Python.Syntax as Syntax
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Serialize an annotated RHS
encodeAnnotatedRhs :: (Syntax.AnnotatedRhs -> Ast.Expr)
encodeAnnotatedRhs arhs = (Serialization.spaceSep [
  Serialization.cst "=",
  ((\x -> case x of
    Syntax.AnnotatedRhsStar v0 -> (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeStarExpression v0))
    Syntax.AnnotatedRhsYield _ -> (Serialization.cst "yield ...")) arhs)])

-- | Serialize an annotated statement (with optional doc comment)
encodeAnnotatedStatement :: (Syntax.AnnotatedStatement -> Ast.Expr)
encodeAnnotatedStatement as_ =  
  let doc_ = (Syntax.annotatedStatementComment as_) 
      stmt = (Syntax.annotatedStatementStatement as_)
  in (Serialization.newlineSep [
    Serialization.cst (toPythonComments doc_),
    (encodeStatement stmt)])

-- | Serialize a type annotation
encodeAnnotation :: (Syntax.Annotation -> Ast.Expr)
encodeAnnotation ann = (Serialization.spaceSep [
  Serialization.cst ":",
  (encodeExpression (Syntax.unAnnotation ann))])

-- | Serialize function arguments
encodeArgs :: (Syntax.Args -> Ast.Expr)
encodeArgs args =  
  let pos = (Syntax.argsPositional args) 
      ks = (Syntax.argsKwargOrStarred args)
      kss = (Syntax.argsKwargOrDoubleStarred args)
  in (Serialization.commaSep Serialization.inlineStyle (Lists.concat [
    Lists.map encodePosArg pos,
    (Lists.map encodeKwargOrStarred ks),
    (Lists.map encodeKwargOrDoubleStarred kss)]))

-- | Serialize an assignment
encodeAssignment :: (Syntax.Assignment -> Ast.Expr)
encodeAssignment a = ((\x -> case x of
  Syntax.AssignmentTyped v0 -> (encodeTypedAssignment v0)
  Syntax.AssignmentUntyped v0 -> (encodeUntypedAssignment v0)
  Syntax.AssignmentAug _ -> (Serialization.cst "... += ...")) a)

-- | Serialize an assignment expression (walrus operator)
encodeAssignmentExpression :: (Syntax.AssignmentExpression -> Ast.Expr)
encodeAssignmentExpression ae =  
  let name = (Syntax.assignmentExpressionName ae) 
      expr = (Syntax.assignmentExpressionExpression ae)
  in (Serialization.spaceSep [
    encodeName name,
    (Serialization.cst ":="),
    (encodeExpression expr)])

-- | Serialize a Python atom (literal or basic expression)
encodeAtom :: (Syntax.Atom -> Ast.Expr)
encodeAtom atom = ((\x -> case x of
  Syntax.AtomDict v0 -> (encodeDict v0)
  Syntax.AtomDictcomp _ -> (Serialization.cst "{...}")
  Syntax.AtomEllipsis -> (Serialization.cst "...")
  Syntax.AtomFalse -> (Serialization.cst "False")
  Syntax.AtomGenexp _ -> (Serialization.cst "(...)")
  Syntax.AtomGroup v0 -> (encodeGroup v0)
  Syntax.AtomList v0 -> (encodeList v0)
  Syntax.AtomListcomp _ -> (Serialization.cst "[...]")
  Syntax.AtomName v0 -> (encodeName v0)
  Syntax.AtomNone -> (Serialization.cst "None")
  Syntax.AtomNumber v0 -> (encodeNumber v0)
  Syntax.AtomSet v0 -> (encodeSet v0)
  Syntax.AtomSetcomp _ -> (Serialization.cst "{...}")
  Syntax.AtomString v0 -> (encodeString v0)
  Syntax.AtomTrue -> (Serialization.cst "True")
  Syntax.AtomTuple v0 -> (encodeTuple v0)) atom)

-- | Serialize an attribute access
encodeAttribute :: (Syntax.Attribute -> Ast.Expr)
encodeAttribute attr = (Serialization.dotSep (Lists.map encodeName (Syntax.unAttribute attr)))

-- | Serialize an await primary expression
encodeAwaitPrimary :: (Syntax.AwaitPrimary -> Ast.Expr)
encodeAwaitPrimary ap =  
  let await_ = (Syntax.awaitPrimaryAwait ap) 
      primary = (Syntax.awaitPrimaryPrimary ap)
  in (Logic.ifElse await_ (Serialization.spaceSep [
    Serialization.cst "await",
    (encodePrimary primary)]) (encodePrimary primary))

-- | Serialize a bitwise AND expression
encodeBitwiseAnd :: (Syntax.BitwiseAnd -> Ast.Expr)
encodeBitwiseAnd band =  
  let lhs = (Syntax.bitwiseAndLhs band) 
      rhs = (Syntax.bitwiseAndRhs band)
  in (Serialization.spaceSep (Maybes.cat [
    Maybes.map (\l -> Serialization.spaceSep [
      encodeBitwiseAnd l,
      (Serialization.cst "&")]) lhs,
    (Just (encodeShiftExpression rhs))]))

-- | Serialize a bitwise OR expression
encodeBitwiseOr :: (Syntax.BitwiseOr -> Ast.Expr)
encodeBitwiseOr bor =  
  let lhs = (Syntax.bitwiseOrLhs bor) 
      rhs = (Syntax.bitwiseOrRhs bor)
  in (Serialization.spaceSep (Maybes.cat [
    Maybes.map (\l -> Serialization.spaceSep [
      encodeBitwiseOr l,
      (Serialization.cst "|")]) lhs,
    (Just (encodeBitwiseXor rhs))]))

-- | Serialize a bitwise XOR expression
encodeBitwiseXor :: (Syntax.BitwiseXor -> Ast.Expr)
encodeBitwiseXor bxor =  
  let lhs = (Syntax.bitwiseXorLhs bxor) 
      rhs = (Syntax.bitwiseXorRhs bxor)
  in (Serialization.spaceSep (Maybes.cat [
    Maybes.map (\l -> Serialization.spaceSep [
      encodeBitwiseXor l,
      (Serialization.cst "^")]) lhs,
    (Just (encodeBitwiseAnd rhs))]))

-- | Serialize a block
encodeBlock :: (Syntax.Block -> Ast.Expr)
encodeBlock b = ((\x -> case x of
  Syntax.BlockIndented v0 -> (Serialization.tabIndentDoubleSpace (Lists.map (\stmts -> Serialization.newlineSep (Lists.map encodeStatement stmts)) v0))
  Syntax.BlockSimple v0 -> (Serialization.semicolonSep (Lists.map encodeSimpleStatement v0))) b)

-- | Serialize a capture pattern
encodeCapturePattern :: (Syntax.CapturePattern -> Ast.Expr)
encodeCapturePattern cp = (encodePatternCaptureTarget (Syntax.unCapturePattern cp))

-- | Serialize a case block
encodeCaseBlock :: (Syntax.CaseBlock -> Ast.Expr)
encodeCaseBlock cb =  
  let patterns = (Syntax.caseBlockPatterns cb) 
      guard = (Syntax.caseBlockGuard cb)
      body = (Syntax.caseBlockBody cb)
  in (Serialization.newlineSep [
    Serialization.noSep [
      Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "case"),
        (Just (encodePatterns patterns)),
        (Maybes.map encodeGuard guard)]),
      (Serialization.cst ":")],
    (encodeBlock body)])

-- | Serialize a class definition
encodeClassDefinition :: (Syntax.ClassDefinition -> Ast.Expr)
encodeClassDefinition cd =  
  let decs = (Syntax.classDefinitionDecorators cd) 
      name = (Syntax.classDefinitionName cd)
      args = (Syntax.classDefinitionArguments cd)
      body = (Syntax.classDefinitionBody cd)
      argPart = (Maybes.map (\a -> Serialization.noSep [
              Serialization.cst "(",
              (encodeArgs a),
              (Serialization.cst ")")]) args)
  in (Serialization.newlineSep (Maybes.cat [
    Maybes.map encodeDecorators decs,
    (Just (Serialization.noSep (Maybes.cat [
      Just (Serialization.spaceSep [
        Serialization.cst "class",
        (encodeName name)]),
      argPart,
      (Just (Serialization.cst ":"))]))),
    (Just (encodeBlock body))]))

-- | Serialize a class pattern
encodeClassPattern :: (Syntax.ClassPattern -> Ast.Expr)
encodeClassPattern cp =  
  let noa = (Syntax.classPatternNameOrAttribute cp) 
      pos = (Syntax.classPatternPositionalPatterns cp)
      kw = (Syntax.classPatternKeywordPatterns cp)
  in (Serialization.noSep (Maybes.cat [
    Just (encodeNameOrAttribute noa),
    (Just (Serialization.cst "(")),
    (Maybes.map encodePositionalPatterns pos),
    (Maybes.map encodeKeywordPatterns kw),
    (Just (Serialization.cst ")"))]))

-- | Serialize a closed pattern
encodeClosedPattern :: (Syntax.ClosedPattern -> Ast.Expr)
encodeClosedPattern cp = ((\x -> case x of
  Syntax.ClosedPatternLiteral _ -> (Serialization.cst "...")
  Syntax.ClosedPatternCapture v0 -> (encodeCapturePattern v0)
  Syntax.ClosedPatternWildcard -> (Serialization.cst "_")
  Syntax.ClosedPatternValue v0 -> (encodeValuePattern v0)
  Syntax.ClosedPatternGroup _ -> (Serialization.cst "(...)")
  Syntax.ClosedPatternSequence _ -> (Serialization.cst "[...]")
  Syntax.ClosedPatternMapping _ -> (Serialization.cst "{...}")
  Syntax.ClosedPatternClass v0 -> (encodeClassPattern v0)) cp)

-- | Serialize a comparison expression
encodeComparison :: (Syntax.Comparison -> Ast.Expr)
encodeComparison cmp = (encodeBitwiseOr (Syntax.comparisonLhs cmp))

-- | Serialize a conditional expression (ternary)
encodeConditional :: (Syntax.Conditional -> Ast.Expr)
encodeConditional c =  
  let body = (Syntax.conditionalBody c) 
      cond = (Syntax.conditionalIf c)
      elseExpr = (Syntax.conditionalElse c)
  in (Serialization.spaceSep [
    encodeDisjunction body,
    (Serialization.cst "if"),
    (encodeDisjunction cond),
    (Serialization.cst "else"),
    (encodeExpression elseExpr)])

-- | Serialize a compound (multi-line) Python statement
encodeCompoundStatement :: (Syntax.CompoundStatement -> Ast.Expr)
encodeCompoundStatement cs = ((\x -> case x of
  Syntax.CompoundStatementFunction v0 -> (encodeFunctionDefinition v0)
  Syntax.CompoundStatementIf _ -> (Serialization.cst "if ...")
  Syntax.CompoundStatementClassDef v0 -> (encodeClassDefinition v0)
  Syntax.CompoundStatementWith _ -> (Serialization.cst "with ...")
  Syntax.CompoundStatementFor _ -> (Serialization.cst "for ...")
  Syntax.CompoundStatementTry _ -> (Serialization.cst "try ...")
  Syntax.CompoundStatementWhile v0 -> (encodeWhileStatement v0)
  Syntax.CompoundStatementMatch v0 -> (encodeMatchStatement v0)) cs)

-- | Serialize a conjunction (and expression)
encodeConjunction :: (Syntax.Conjunction -> Ast.Expr)
encodeConjunction c = (Serialization.symbolSep "and" Serialization.inlineStyle (Lists.map encodeInversion (Syntax.unConjunction c)))

-- | Serialize decorators
encodeDecorators :: (Syntax.Decorators -> Ast.Expr)
encodeDecorators decs = (Serialization.newlineSep (Lists.map (\ne -> Serialization.noSep [
  Serialization.cst "@",
  (encodeNamedExpression ne)]) (Syntax.unDecorators decs)))

-- | Serialize a Python dictionary
encodeDict :: (Syntax.Dict -> Ast.Expr)
encodeDict d = (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map encodeDoubleStarredKvpair (Syntax.unDict d)))

-- | Serialize a disjunction (or expression)
encodeDisjunction :: (Syntax.Disjunction -> Ast.Expr)
encodeDisjunction d = (Serialization.symbolSep "or" Serialization.inlineStyle (Lists.map encodeConjunction (Syntax.unDisjunction d)))

-- | Serialize a dotted as name
encodeDottedAsName :: (Syntax.DottedAsName -> Ast.Expr)
encodeDottedAsName dan =  
  let name = (Syntax.dottedAsNameName dan) 
      alias = (Syntax.dottedAsNameAs dan)
  in (Serialization.spaceSep (Maybes.cat [
    Just (encodeDottedName name),
    (Maybes.map (\a -> Serialization.spaceSep [
      Serialization.cst "as",
      (encodeName a)]) alias)]))

-- | Serialize a dotted name (e.g., module.submodule)
encodeDottedName :: (Syntax.DottedName -> Ast.Expr)
encodeDottedName dn = (Serialization.cst (Strings.intercalate "." (Lists.map (\n -> Syntax.unName n) (Syntax.unDottedName dn))))

-- | Serialize a double-starred key-value pair
encodeDoubleStarredKvpair :: (Syntax.DoubleStarredKvpair -> Ast.Expr)
encodeDoubleStarredKvpair dskv = ((\x -> case x of
  Syntax.DoubleStarredKvpairPair v0 -> (encodeKvpair v0)
  Syntax.DoubleStarredKvpairStarred v0 -> (Serialization.noSep [
    Serialization.cst "**",
    (encodeBitwiseOr v0)])) dskv)

-- | Serialize a Python expression
encodeExpression :: (Syntax.Expression -> Ast.Expr)
encodeExpression expr = ((\x -> case x of
  Syntax.ExpressionSimple v0 -> (encodeDisjunction v0)
  Syntax.ExpressionConditional v0 -> (encodeConditional v0)
  Syntax.ExpressionLambda v0 -> (encodeLambda v0)) expr)

-- | Serialize a factor expression
encodeFactor :: (Syntax.Factor -> Ast.Expr)
encodeFactor f = ((\x -> case x of
  Syntax.FactorPositive v0 -> (Serialization.noSep [
    Serialization.cst "+",
    (encodeFactor v0)])
  Syntax.FactorNegative v0 -> (Serialization.noSep [
    Serialization.cst "-",
    (encodeFactor v0)])
  Syntax.FactorComplement v0 -> (Serialization.noSep [
    Serialization.cst "~",
    (encodeFactor v0)])
  Syntax.FactorSimple v0 -> (encodePower v0)) f)

-- | Serialize a raw function definition
encodeFunctionDefRaw :: (Syntax.FunctionDefRaw -> Ast.Expr)
encodeFunctionDefRaw fdr =  
  let async_ = (Syntax.functionDefRawAsync fdr) 
      name = (Syntax.functionDefRawName fdr)
      tparams = (Syntax.functionDefRawTypeParams fdr)
      params = (Syntax.functionDefRawParams fdr)
      retType = (Syntax.functionDefRawReturnType fdr)
      block = (Syntax.functionDefRawBlock fdr)
      asyncKw = (Logic.ifElse async_ (Just (Serialization.cst "async")) Nothing)
      tparamPart = (Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.bracketList Serialization.inlineStyle (Lists.map encodeTypeParameter tparams))))
      paramPart = (Maybes.map encodeParameters params)
      retPart = (Maybes.map (\t -> Serialization.spaceSep [
              Serialization.cst "->",
              (encodeExpression t)]) retType)
  in (Serialization.newlineSep [
    Serialization.noSep [
      Serialization.spaceSep (Maybes.cat [
        asyncKw,
        (Just (Serialization.cst "def")),
        (Just (Serialization.noSep (Maybes.cat [
          Just (encodeName name),
          tparamPart,
          (Just (Serialization.cst "(")),
          paramPart,
          (Just (Serialization.cst ")"))]))),
        retPart]),
      (Serialization.cst ":")],
    (encodeBlock block)])

-- | Serialize a function definition
encodeFunctionDefinition :: (Syntax.FunctionDefinition -> Ast.Expr)
encodeFunctionDefinition fd =  
  let decs = (Syntax.functionDefinitionDecorators fd) 
      raw = (Syntax.functionDefinitionRaw fd)
  in (Serialization.newlineSep (Maybes.cat [
    Maybes.map encodeDecorators decs,
    (Just (encodeFunctionDefRaw raw))]))

-- | Serialize a parenthesized group
encodeGroup :: (Syntax.Group -> Ast.Expr)
encodeGroup g = ((\x -> case x of
  Syntax.GroupExpression v0 -> (encodeNamedExpression v0)
  Syntax.GroupYield _ -> (Serialization.cst "(yield ...)")) g)

-- | Serialize a guard clause
encodeGuard :: (Syntax.Guard -> Ast.Expr)
encodeGuard g = (Serialization.spaceSep [
  Serialization.cst "if",
  (encodeNamedExpression (Syntax.unGuard g))])

-- | Serialize an import from statement
encodeImportFrom :: (Syntax.ImportFrom -> Ast.Expr)
encodeImportFrom if_ =  
  let prefixes = (Syntax.importFromPrefixes if_) 
      name = (Syntax.importFromDottedName if_)
      targets = (Syntax.importFromTargets if_)
      lhs = (Serialization.noSep (Maybes.cat (Lists.concat [
              Lists.map (\p -> Just (encodeRelativeImportPrefix p)) prefixes,
              [
                Maybes.map encodeDottedName name]])))
  in (Serialization.spaceSep [
    Serialization.cst "from",
    lhs,
    (Serialization.cst "import"),
    (encodeImportFromTargets targets)])

-- | Serialize an import from as name
encodeImportFromAsName :: (Syntax.ImportFromAsName -> Ast.Expr)
encodeImportFromAsName ifan =  
  let name = (Syntax.importFromAsNameName ifan) 
      alias = (Syntax.importFromAsNameAs ifan)
  in (Maybes.maybe (encodeName name) (\a -> Serialization.spaceSep [
    encodeName name,
    (Serialization.cst "as"),
    (encodeName a)]) alias)

-- | Serialize import from targets
encodeImportFromTargets :: (Syntax.ImportFromTargets -> Ast.Expr)
encodeImportFromTargets t = ((\x -> case x of
  Syntax.ImportFromTargetsSimple v0 -> (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeImportFromAsName v0))
  Syntax.ImportFromTargetsParens v0 -> (Serialization.noSep [
    Serialization.cst "(",
    (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeImportFromAsName v0)),
    (Serialization.cst ")")])
  Syntax.ImportFromTargetsStar -> (Serialization.cst "*")) t)

-- | Serialize an import name
encodeImportName :: (Syntax.ImportName -> Ast.Expr)
encodeImportName in_ = (Serialization.spaceSep [
  Serialization.cst "import",
  (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeDottedAsName (Syntax.unImportName in_)))])

-- | Serialize an import statement
encodeImportStatement :: (Syntax.ImportStatement -> Ast.Expr)
encodeImportStatement is_ = ((\x -> case x of
  Syntax.ImportStatementName v0 -> (encodeImportName v0)
  Syntax.ImportStatementFrom v0 -> (encodeImportFrom v0)) is_)

-- | Serialize an inversion (not expression)
encodeInversion :: (Syntax.Inversion -> Ast.Expr)
encodeInversion i = ((\x -> case x of
  Syntax.InversionNot v0 -> (Serialization.spaceSep [
    Serialization.cst "not",
    (encodeInversion v0)])
  Syntax.InversionSimple v0 -> (encodeComparison v0)) i)

-- | Serialize a keyword pattern
encodeKeywordPattern :: (Syntax.KeywordPattern -> Ast.Expr)
encodeKeywordPattern kp =  
  let name = (Syntax.keywordPatternName kp) 
      pat = (Syntax.keywordPatternPattern kp)
  in (Serialization.noSep [
    encodeName name,
    (Serialization.cst "="),
    (encodePattern pat)])

-- | Serialize keyword patterns
encodeKeywordPatterns :: (Syntax.KeywordPatterns -> Ast.Expr)
encodeKeywordPatterns kp = (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeKeywordPattern (Syntax.unKeywordPatterns kp)))

-- | Serialize a key-value pair
encodeKvpair :: (Syntax.Kvpair -> Ast.Expr)
encodeKvpair kv =  
  let k = (Syntax.kvpairKey kv) 
      v = (Syntax.kvpairValue kv)
  in (Serialization.spaceSep [
    Serialization.noSep [
      encodeExpression k,
      (Serialization.cst ":")],
    (encodeExpression v)])

-- | Serialize a keyword argument
encodeKwarg :: (Syntax.Kwarg -> Ast.Expr)
encodeKwarg k =  
  let name = (Syntax.kwargName k) 
      expr = (Syntax.kwargValue k)
  in (Serialization.noSep [
    encodeName name,
    (Serialization.cst "="),
    (encodeExpression expr)])

-- | Serialize a kwarg or double starred
encodeKwargOrDoubleStarred :: (Syntax.KwargOrDoubleStarred -> Ast.Expr)
encodeKwargOrDoubleStarred kds = ((\x -> case x of
  Syntax.KwargOrDoubleStarredKwarg v0 -> (encodeKwarg v0)
  Syntax.KwargOrDoubleStarredDoubleStarred v0 -> (Serialization.noSep [
    Serialization.cst "**",
    (encodeExpression v0)])) kds)

-- | Serialize a kwarg or starred
encodeKwargOrStarred :: (Syntax.KwargOrStarred -> Ast.Expr)
encodeKwargOrStarred ks = ((\x -> case x of
  Syntax.KwargOrStarredKwarg v0 -> (encodeKwarg v0)
  Syntax.KwargOrStarredStarred v0 -> (encodeStarredExpression v0)) ks)

-- | Serialize a lambda expression
encodeLambda :: (Syntax.Lambda -> Ast.Expr)
encodeLambda l =  
  let params = (Syntax.lambdaParams l) 
      body = (Syntax.lambdaBody l)
  in (Serialization.parens (Serialization.spaceSep [
    Serialization.cst "lambda",
    (Serialization.noSep [
      encodeLambdaParameters params,
      (Serialization.cst ":")]),
    (encodeExpression body)]))

-- | Serialize a lambda parameter without default
encodeLambdaParamNoDefault :: (Syntax.LambdaParamNoDefault -> Ast.Expr)
encodeLambdaParamNoDefault p = (encodeName (Syntax.unLambdaParamNoDefault p))

-- | Serialize lambda parameters
encodeLambdaParameters :: (Syntax.LambdaParameters -> Ast.Expr)
encodeLambdaParameters lp =  
  let nodef = (Syntax.lambdaParametersParamNoDefault lp)
  in (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeLambdaParamNoDefault nodef))

-- | Serialize lambda star etc
encodeLambdaStarEtc :: (Syntax.LambdaStarEtc -> Ast.Expr)
encodeLambdaStarEtc lse = ((\x -> case x of
  Syntax.LambdaStarEtcParamNoDefault v0 -> (encodeLambdaParamNoDefault v0)
  Syntax.LambdaStarEtcStar _ -> (Serialization.cst "*...")
  Syntax.LambdaStarEtcParamMaybeDefault _ -> (Serialization.cst "...")
  Syntax.LambdaStarEtcKwds _ -> (Serialization.cst "**...")) lse)

-- | Serialize a Python list
encodeList :: (Syntax.List -> Ast.Expr)
encodeList l = (Serialization.bracketListAdaptive (Lists.map encodeStarNamedExpression (Syntax.unList l)))

-- | Serialize a match statement
encodeMatchStatement :: (Syntax.MatchStatement -> Ast.Expr)
encodeMatchStatement ms =  
  let subj = (Syntax.matchStatementSubject ms) 
      cases = (Syntax.matchStatementCases ms)
  in (Serialization.newlineSep [
    Serialization.spaceSep [
      Serialization.cst "match",
      (Serialization.noSep [
        encodeSubjectExpression subj,
        (Serialization.cst ":")])],
    (Serialization.tabIndentDoubleSpace (Lists.map encodeCaseBlock cases))])

-- | Serialize a Python module to an AST expression
encodeModule :: (Syntax.Module -> Ast.Expr)
encodeModule mod =  
  let warning = (Serialization.cst (toPythonComments Constants.warningAutoGeneratedFile)) 
      groups = (Lists.map (\group -> Serialization.newlineSep (Lists.map encodeStatement group)) (Syntax.unModule mod))
  in (Serialization.doubleNewlineSep (Lists.cons warning groups))

-- | Serialize a Python name/identifier
encodeName :: (Syntax.Name -> Ast.Expr)
encodeName n = (Serialization.cst (Syntax.unName n))

-- | Serialize a named expression
encodeNamedExpression :: (Syntax.NamedExpression -> Ast.Expr)
encodeNamedExpression ne = ((\x -> case x of
  Syntax.NamedExpressionSimple v0 -> (encodeExpression v0)
  Syntax.NamedExpressionAssignment v0 -> (encodeAssignmentExpression v0)) ne)

-- | Serialize a name or attribute
encodeNameOrAttribute :: (Syntax.NameOrAttribute -> Ast.Expr)
encodeNameOrAttribute noa = (Serialization.dotSep (Lists.map encodeName (Syntax.unNameOrAttribute noa)))

-- | Serialize a Python number literal
encodeNumber :: (Syntax.Number -> Ast.Expr)
encodeNumber num = ((\x -> case x of
  Syntax.NumberFloat v0 -> (Serialization.cst (Literals.showBigfloat v0))
  Syntax.NumberInteger v0 -> (Serialization.cst (Literals.showBigint v0))) num)

-- | Serialize an or pattern
encodeOrPattern :: (Syntax.OrPattern -> Ast.Expr)
encodeOrPattern op = (Serialization.symbolSep "|" Serialization.inlineStyle (Lists.map encodeClosedPattern (Syntax.unOrPattern op)))

-- | Serialize a parameter
encodeParam :: (Syntax.Param -> Ast.Expr)
encodeParam p =  
  let name = (Syntax.paramName p) 
      ann = (Syntax.paramAnnotation p)
  in (Serialization.noSep (Maybes.cat [
    Just (encodeName name),
    (Maybes.map encodeAnnotation ann)]))

-- | Serialize a parameter without default
encodeParamNoDefault :: (Syntax.ParamNoDefault -> Ast.Expr)
encodeParamNoDefault pnd = (encodeParam (Syntax.paramNoDefaultParam pnd))

-- | Serialize parameters without defaults
encodeParamNoDefaultParameters :: (Syntax.ParamNoDefaultParameters -> Ast.Expr)
encodeParamNoDefaultParameters pndp =  
  let nodef = (Syntax.paramNoDefaultParametersParamNoDefault pndp)
  in (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeParamNoDefault nodef))

-- | Serialize function parameters
encodeParameters :: (Syntax.Parameters -> Ast.Expr)
encodeParameters p = ((\x -> case x of
  Syntax.ParametersParamNoDefault v0 -> (encodeParamNoDefaultParameters v0)
  Syntax.ParametersSlashNoDefault _ -> (Serialization.cst "...")
  Syntax.ParametersSlashWithDefault _ -> (Serialization.cst "...")) p)

-- | Serialize a pattern
encodePattern :: (Syntax.Pattern -> Ast.Expr)
encodePattern p = ((\x -> case x of
  Syntax.PatternOr v0 -> (encodeOrPattern v0)
  Syntax.PatternAs _ -> (Serialization.cst "... as ...")) p)

-- | Serialize a pattern capture target
encodePatternCaptureTarget :: (Syntax.PatternCaptureTarget -> Ast.Expr)
encodePatternCaptureTarget pct = (encodeName (Syntax.unPatternCaptureTarget pct))

-- | Serialize patterns
encodePatterns :: (Syntax.Patterns -> Ast.Expr)
encodePatterns ps = ((\x -> case x of
  Syntax.PatternsPattern v0 -> (encodePattern v0)
  Syntax.PatternsSequence _ -> (Serialization.cst "...")) ps)

-- | Serialize a positional argument
encodePosArg :: (Syntax.PosArg -> Ast.Expr)
encodePosArg pa = ((\x -> case x of
  Syntax.PosArgStarred v0 -> (encodeStarredExpression v0)
  Syntax.PosArgAssignment v0 -> (encodeAssignmentExpression v0)
  Syntax.PosArgExpression v0 -> (encodeExpression v0)) pa)

-- | Serialize positional patterns
encodePositionalPatterns :: (Syntax.PositionalPatterns -> Ast.Expr)
encodePositionalPatterns pp = (Serialization.commaSep Serialization.inlineStyle (Lists.map encodePattern (Syntax.unPositionalPatterns pp)))

-- | Serialize a power expression
encodePower :: (Syntax.Power -> Ast.Expr)
encodePower p =  
  let lhs = (Syntax.powerLhs p) 
      rhs = (Syntax.powerRhs p)
  in (Serialization.spaceSep (Maybes.cat [
    Just (encodeAwaitPrimary lhs),
    (Maybes.map (\r -> Serialization.spaceSep [
      Serialization.cst "**",
      (encodeFactor r)]) rhs)]))

-- | Serialize a primary expression
encodePrimary :: (Syntax.Primary -> Ast.Expr)
encodePrimary p = ((\x -> case x of
  Syntax.PrimarySimple v0 -> (encodeAtom v0)
  Syntax.PrimaryCompound v0 -> (encodePrimaryWithRhs v0)) p)

-- | Serialize a primary RHS
encodePrimaryRhs :: (Syntax.PrimaryRhs -> Ast.Expr)
encodePrimaryRhs rhs = ((\x -> case x of
  Syntax.PrimaryRhsCall v0 -> (Serialization.noSep [
    Serialization.cst "(",
    (encodeArgs v0),
    (Serialization.cst ")")])
  Syntax.PrimaryRhsProject v0 -> (Serialization.noSep [
    Serialization.cst ".",
    (encodeName v0)])
  Syntax.PrimaryRhsSlices v0 -> (Serialization.noSep [
    Serialization.cst "[",
    (encodeSlices v0),
    (Serialization.cst "]")])
  Syntax.PrimaryRhsGenexp _ -> (Serialization.cst "[...]")) rhs)

-- | Serialize a primary with RHS
encodePrimaryWithRhs :: (Syntax.PrimaryWithRhs -> Ast.Expr)
encodePrimaryWithRhs pwr =  
  let prim = (Syntax.primaryWithRhsPrimary pwr) 
      rhs = (Syntax.primaryWithRhsRhs pwr)
  in (Serialization.noSep [
    encodePrimary prim,
    (encodePrimaryRhs rhs)])

-- | Serialize a raise expression
encodeRaiseExpression :: (Syntax.RaiseExpression -> Ast.Expr)
encodeRaiseExpression re =  
  let expr = (Syntax.raiseExpressionExpression re) 
      from_ = (Syntax.raiseExpressionFrom re)
  in (Serialization.spaceSep (Maybes.cat [
    Just (encodeExpression expr),
    (Maybes.map (\f -> Serialization.spaceSep [
      Serialization.cst "from",
      (encodeExpression f)]) from_)]))

-- | Serialize a raise statement
encodeRaiseStatement :: (Syntax.RaiseStatement -> Ast.Expr)
encodeRaiseStatement rs = (Serialization.spaceSep (Maybes.cat [
  Just (Serialization.cst "raise"),
  (Maybes.map encodeRaiseExpression (Syntax.unRaiseStatement rs))]))

-- | Serialize a relative import prefix
encodeRelativeImportPrefix :: (Syntax.RelativeImportPrefix -> Ast.Expr)
encodeRelativeImportPrefix p = ((\x -> case x of
  Syntax.RelativeImportPrefixDot -> (Serialization.cst ".")
  Syntax.RelativeImportPrefixEllipsis -> (Serialization.cst "...")) p)

-- | Serialize a return statement
encodeReturnStatement :: (Syntax.ReturnStatement -> Ast.Expr)
encodeReturnStatement rs = (Serialization.spaceSep [
  Serialization.cst "return",
  (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeStarExpression (Syntax.unReturnStatement rs)))])

-- | Serialize a Python set
encodeSet :: (Syntax.Set -> Ast.Expr)
encodeSet s = (Serialization.bracesListAdaptive (Lists.map encodeStarNamedExpression (Syntax.unSet s)))

-- | Serialize a shift expression
encodeShiftExpression :: (Syntax.ShiftExpression -> Ast.Expr)
encodeShiftExpression se = (encodeSum (Syntax.shiftExpressionRhs se))

-- | Serialize a simple (single-line) Python statement
encodeSimpleStatement :: (Syntax.SimpleStatement -> Ast.Expr)
encodeSimpleStatement ss = ((\x -> case x of
  Syntax.SimpleStatementAssignment v0 -> (encodeAssignment v0)
  Syntax.SimpleStatementStarExpressions v0 -> (Serialization.newlineSep (Lists.map encodeStarExpression v0))
  Syntax.SimpleStatementReturn v0 -> (encodeReturnStatement v0)
  Syntax.SimpleStatementRaise v0 -> (encodeRaiseStatement v0)
  Syntax.SimpleStatementPass -> (Serialization.cst "pass")
  Syntax.SimpleStatementBreak -> (Serialization.cst "break")
  Syntax.SimpleStatementContinue -> (Serialization.cst "continue")
  Syntax.SimpleStatementImport v0 -> (encodeImportStatement v0)
  Syntax.SimpleStatementTypeAlias v0 -> (encodeTypeAlias v0)
  Syntax.SimpleStatementAssert _ -> (Serialization.cst "assert ...")
  Syntax.SimpleStatementGlobal _ -> (Serialization.cst "global ...")
  Syntax.SimpleStatementNonlocal _ -> (Serialization.cst "nonlocal ...")
  Syntax.SimpleStatementDel _ -> (Serialization.cst "del ...")) ss)

-- | Serialize a simple type parameter
encodeSimpleTypeParameter :: (Syntax.SimpleTypeParameter -> Ast.Expr)
encodeSimpleTypeParameter stp = (encodeName (Syntax.simpleTypeParameterName stp))

-- | Serialize a single target
encodeSingleTarget :: (Syntax.SingleTarget -> Ast.Expr)
encodeSingleTarget st = ((\x -> case x of
  Syntax.SingleTargetName v0 -> (encodeName v0)
  Syntax.SingleTargetParens _ -> (Serialization.cst "(...)")
  Syntax.SingleTargetSubscriptAttributeTarget _ -> (Serialization.cst "...")) st)

-- | Serialize a slice
encodeSlice :: (Syntax.Slice -> Ast.Expr)
encodeSlice s = ((\x -> case x of
  Syntax.SliceNamed v0 -> (encodeNamedExpression v0)
  Syntax.SliceSlice_ _ -> (Serialization.cst ":")) s)

-- | Serialize a slice or starred expression
encodeSliceOrStarredExpression :: (Syntax.SliceOrStarredExpression -> Ast.Expr)
encodeSliceOrStarredExpression s = ((\x -> case x of
  Syntax.SliceOrStarredExpressionSlice v0 -> (encodeSlice v0)
  Syntax.SliceOrStarredExpressionStarred v0 -> (encodeStarredExpression v0)) s)

-- | Serialize slices
encodeSlices :: (Syntax.Slices -> Ast.Expr)
encodeSlices s =  
  let hd = (Syntax.slicesHead s) 
      tl = (Syntax.slicesTail s)
  in (Serialization.commaSep Serialization.inlineStyle (Lists.cons (encodeSlice hd) (Lists.map encodeSliceOrStarredExpression tl)))

-- | Serialize a star atom
encodeStarAtom :: (Syntax.StarAtom -> Ast.Expr)
encodeStarAtom sa = ((\x -> case x of
  Syntax.StarAtomName v0 -> (encodeName v0)
  Syntax.StarAtomTargetWithStarAtom _ -> (Serialization.cst "(...)")
  Syntax.StarAtomStarTargetsTupleSeq _ -> (Serialization.cst "(...)")
  Syntax.StarAtomStarTargetsListSeq _ -> (Serialization.cst "[...]")) sa)

-- | Serialize a star expression
encodeStarExpression :: (Syntax.StarExpression -> Ast.Expr)
encodeStarExpression se = ((\x -> case x of
  Syntax.StarExpressionStar v0 -> (Serialization.noSep [
    Serialization.cst "*",
    (encodeBitwiseOr v0)])
  Syntax.StarExpressionSimple v0 -> (encodeExpression v0)) se)

-- | Serialize a star named expression
encodeStarNamedExpression :: (Syntax.StarNamedExpression -> Ast.Expr)
encodeStarNamedExpression sne = ((\x -> case x of
  Syntax.StarNamedExpressionStar v0 -> (Serialization.noSep [
    Serialization.cst "*",
    (encodeBitwiseOr v0)])
  Syntax.StarNamedExpressionSimple v0 -> (encodeNamedExpression v0)) sne)

-- | Serialize a star target
encodeStarTarget :: (Syntax.StarTarget -> Ast.Expr)
encodeStarTarget st = ((\x -> case x of
  Syntax.StarTargetUnstarred v0 -> (encodeTargetWithStarAtom v0)
  Syntax.StarTargetStarred v0 -> (Serialization.noSep [
    Serialization.cst "*",
    (encodeStarTarget v0)])) st)

-- | Serialize a starred expression
encodeStarredExpression :: (Syntax.StarredExpression -> Ast.Expr)
encodeStarredExpression se = (Serialization.noSep [
  Serialization.cst "*",
  (encodeExpression (Syntax.unStarredExpression se))])

-- | Serialize a Python statement
encodeStatement :: (Syntax.Statement -> Ast.Expr)
encodeStatement stmt = ((\x -> case x of
  Syntax.StatementAnnotated v0 -> (encodeAnnotatedStatement v0)
  Syntax.StatementSimple v0 -> (Serialization.newlineSep (Lists.map encodeSimpleStatement v0))
  Syntax.StatementCompound v0 -> (encodeCompoundStatement v0)) stmt)

-- | Serialize a Python string literal
encodeString :: (Syntax.String_ -> Ast.Expr)
encodeString s =  
  let content = (Syntax.stringValue s) 
      style = (Syntax.stringQuoteStyle s)
  in ((\x -> case x of
    Syntax.QuoteStyleSingle -> (Serialization.cst (escapePythonString False content))
    Syntax.QuoteStyleDouble -> (Serialization.cst (escapePythonString True content))
    Syntax.QuoteStyleTriple -> (Serialization.noSep [
      Serialization.cst "r\"\"\"",
      (Serialization.cst content),
      (Serialization.cst "\"\"\"")])) style)

-- | Serialize a subject expression
encodeSubjectExpression :: (Syntax.SubjectExpression -> Ast.Expr)
encodeSubjectExpression se = ((\x -> case x of
  Syntax.SubjectExpressionSimple v0 -> (encodeNamedExpression v0)
  Syntax.SubjectExpressionTuple _ -> (Serialization.cst "*...")) se)

-- | Serialize a sum expression
encodeSum :: (Syntax.Sum -> Ast.Expr)
encodeSum s = (encodeTerm (Syntax.sumRhs s))

-- | Serialize a term expression
encodeTerm :: (Syntax.Term -> Ast.Expr)
encodeTerm t = (encodeFactor (Syntax.termRhs t))

-- | Serialize a target with star atom
encodeTargetWithStarAtom :: (Syntax.TargetWithStarAtom -> Ast.Expr)
encodeTargetWithStarAtom t = ((\x -> case x of
  Syntax.TargetWithStarAtomAtom v0 -> (encodeStarAtom v0)
  Syntax.TargetWithStarAtomProject v0 -> (encodeTPrimaryAndName v0)
  Syntax.TargetWithStarAtomSlices _ -> (Serialization.cst "...")) t)

-- | Serialize a TPrimaryAndName as primary.name
encodeTPrimaryAndName :: (Syntax.TPrimaryAndName -> Ast.Expr)
encodeTPrimaryAndName pn =  
  let prim = (Syntax.tPrimaryAndNamePrimary pn) 
      name_ = (Syntax.tPrimaryAndNameName pn)
  in (Serialization.noSep [
    encodeTPrimary prim,
    (Serialization.cst "."),
    (encodeName name_)])

-- | Serialize a target-side primary expression
encodeTPrimary :: (Syntax.TPrimary -> Ast.Expr)
encodeTPrimary tp = ((\x -> case x of
  Syntax.TPrimaryAtom v0 -> (encodeAtom v0)
  Syntax.TPrimaryPrimaryAndName v0 -> (encodeTPrimaryAndName v0)
  Syntax.TPrimaryPrimaryAndSlices _ -> (Serialization.cst "...")
  Syntax.TPrimaryPrimaryAndGenexp _ -> (Serialization.cst "...")
  Syntax.TPrimaryPrimaryAndArguments _ -> (Serialization.cst "...")) tp)

-- | Serialize a Python tuple
encodeTuple :: (Syntax.Tuple -> Ast.Expr)
encodeTuple t =  
  let es = (Syntax.unTuple t)
  in (Logic.ifElse (Equality.equal (Lists.length es) 1) (Serialization.parens (Serialization.noSep [
    encodeStarNamedExpression (Lists.head es),
    (Serialization.cst ",")])) (Serialization.parenList False (Lists.map encodeStarNamedExpression es)))

-- | Serialize a type alias
encodeTypeAlias :: (Syntax.TypeAlias -> Ast.Expr)
encodeTypeAlias ta =  
  let name = (Syntax.typeAliasName ta) 
      tparams = (Syntax.typeAliasTypeParams ta)
      expr = (Syntax.typeAliasExpression ta)
      alias = (Serialization.noSep (Maybes.cat [
              Just (encodeName name),
              (Logic.ifElse (Lists.null tparams) Nothing (Just (Serialization.bracketList Serialization.inlineStyle (Lists.map encodeTypeParameter tparams))))]))
  in (Serialization.spaceSep [
    Serialization.cst "type",
    alias,
    (Serialization.cst "="),
    (encodeExpression expr)])

-- | Serialize a type parameter
encodeTypeParameter :: (Syntax.TypeParameter -> Ast.Expr)
encodeTypeParameter tp = ((\x -> case x of
  Syntax.TypeParameterSimple v0 -> (encodeSimpleTypeParameter v0)
  Syntax.TypeParameterStar _ -> (Serialization.cst "*...")
  Syntax.TypeParameterDoubleStar _ -> (Serialization.cst "**...")) tp)

-- | Serialize a typed assignment
encodeTypedAssignment :: (Syntax.TypedAssignment -> Ast.Expr)
encodeTypedAssignment ta =  
  let lhs = (Syntax.typedAssignmentLhs ta) 
      typ = (Syntax.typedAssignmentType ta)
      rhs = (Syntax.typedAssignmentRhs ta)
  in (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.noSep [
      encodeSingleTarget lhs,
      (Serialization.cst ":")]),
    (Just (encodeExpression typ)),
    (Maybes.map encodeAnnotatedRhs rhs)]))

-- | Serialize an untyped assignment
encodeUntypedAssignment :: (Syntax.UntypedAssignment -> Ast.Expr)
encodeUntypedAssignment ua =  
  let targets = (Syntax.untypedAssignmentTargets ua) 
      rhs = (Syntax.untypedAssignmentRhs ua)
  in (Serialization.spaceSep (Lists.concat [
    Lists.map encodeStarTarget targets,
    [
      encodeAnnotatedRhs rhs]]))

-- | Serialize a value pattern
encodeValuePattern :: (Syntax.ValuePattern -> Ast.Expr)
encodeValuePattern vp = (encodeAttribute (Syntax.unValuePattern vp))

-- | Serialize a while statement
encodeWhileStatement :: (Syntax.WhileStatement -> Ast.Expr)
encodeWhileStatement ws =  
  let cond = (Syntax.whileStatementCondition ws) 
      body = (Syntax.whileStatementBody ws)
      else_ = (Syntax.whileStatementElse ws)
  in (Serialization.newlineSep (Maybes.cat [
    Just (Serialization.newlineSep [
      Serialization.spaceSep [
        Serialization.cst "while",
        (Serialization.noSep [
          encodeNamedExpression cond,
          (Serialization.cst ":")])],
      (encodeBlock body)]),
    (Maybes.map (\eb -> Serialization.newlineSep [
      Serialization.cst "else:",
      (encodeBlock eb)]) else_)]))

-- | Escape special characters in a Python string and wrap in quotes
escapePythonString :: (Bool -> String -> String)
escapePythonString doubleQuoted s =  
  let replace = (\old -> \new -> \str -> Strings.intercalate new (Strings.splitOn old str)) 
      s1 = (replace "\\" "\\\\" s)
      s2 = (replace "\NUL" "\\x00" s1)
      s3 = (replace "\n" "\\n" s2)
      s4 = (replace "\t" "\\t" s3)
      s5 = (replace "\r" "\\r" s4)
      escaped = (Logic.ifElse doubleQuoted (replace "\"" "\\\"" s5) (replace "'" "\\'" s5))
      quote = (Logic.ifElse doubleQuoted "\"" "'")
  in (Strings.cat2 quote (Strings.cat2 escaped quote))

-- | Convert a doc string to Python comment format
toPythonComments :: (String -> String)
toPythonComments doc_ = (Logic.ifElse (Equality.equal doc_ "") "" (Strings.intercalate "\n" (Lists.map (\line -> Strings.cat2 "# " line) (Strings.lines doc_))))
