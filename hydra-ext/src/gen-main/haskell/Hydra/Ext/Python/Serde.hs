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
    Syntax.AnnotatedRhsStar v1 -> (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeStarExpression v1))
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
  Syntax.AssignmentTyped v1 -> (encodeTypedAssignment v1)
  Syntax.AssignmentUntyped v1 -> (encodeUntypedAssignment v1)
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
  Syntax.AtomDict v1 -> (encodeDict v1)
  Syntax.AtomDictcomp _ -> (Serialization.cst "{...}")
  Syntax.AtomEllipsis -> (Serialization.cst "...")
  Syntax.AtomFalse -> (Serialization.cst "False")
  Syntax.AtomGenexp _ -> (Serialization.cst "(...)")
  Syntax.AtomGroup v1 -> (encodeGroup v1)
  Syntax.AtomList v1 -> (encodeList v1)
  Syntax.AtomListcomp _ -> (Serialization.cst "[...]")
  Syntax.AtomName v1 -> (encodeName v1)
  Syntax.AtomNone -> (Serialization.cst "None")
  Syntax.AtomNumber v1 -> (encodeNumber v1)
  Syntax.AtomSet v1 -> (encodeSet v1)
  Syntax.AtomSetcomp _ -> (Serialization.cst "{...}")
  Syntax.AtomString v1 -> (encodeString v1)
  Syntax.AtomTrue -> (Serialization.cst "True")
  Syntax.AtomTuple v1 -> (encodeTuple v1)) atom)

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
  Syntax.BlockIndented v1 -> (Serialization.tabIndentDoubleSpace (Lists.map (\stmts -> Serialization.newlineSep (Lists.map encodeStatement stmts)) v1))
  Syntax.BlockSimple v1 -> (Serialization.semicolonSep (Lists.map encodeSimpleStatement v1))) b)

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
  Syntax.ClosedPatternCapture v1 -> (encodeCapturePattern v1)
  Syntax.ClosedPatternWildcard -> (Serialization.cst "_")
  Syntax.ClosedPatternValue v1 -> (encodeValuePattern v1)
  Syntax.ClosedPatternGroup _ -> (Serialization.cst "(...)")
  Syntax.ClosedPatternSequence _ -> (Serialization.cst "[...]")
  Syntax.ClosedPatternMapping _ -> (Serialization.cst "{...}")
  Syntax.ClosedPatternClass v1 -> (encodeClassPattern v1)) cp)

-- | Serialize a comparison expression
encodeComparison :: (Syntax.Comparison -> Ast.Expr)
encodeComparison cmp = (encodeBitwiseOr (Syntax.comparisonLhs cmp))

-- | Serialize a compound (multi-line) Python statement
encodeCompoundStatement :: (Syntax.CompoundStatement -> Ast.Expr)
encodeCompoundStatement cs = ((\x -> case x of
  Syntax.CompoundStatementFunction v1 -> (encodeFunctionDefinition v1)
  Syntax.CompoundStatementIf _ -> (Serialization.cst "if ...")
  Syntax.CompoundStatementClassDef v1 -> (encodeClassDefinition v1)
  Syntax.CompoundStatementWith _ -> (Serialization.cst "with ...")
  Syntax.CompoundStatementFor _ -> (Serialization.cst "for ...")
  Syntax.CompoundStatementTry _ -> (Serialization.cst "try ...")
  Syntax.CompoundStatementWhile _ -> (Serialization.cst "while ...")
  Syntax.CompoundStatementMatch v1 -> (encodeMatchStatement v1)) cs)

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
  Syntax.DoubleStarredKvpairPair v1 -> (encodeKvpair v1)
  Syntax.DoubleStarredKvpairStarred v1 -> (Serialization.noSep [
    Serialization.cst "**",
    (encodeBitwiseOr v1)])) dskv)

-- | Serialize a Python expression
encodeExpression :: (Syntax.Expression -> Ast.Expr)
encodeExpression expr = ((\x -> case x of
  Syntax.ExpressionSimple v1 -> (encodeDisjunction v1)
  Syntax.ExpressionConditional _ -> (Serialization.cst "... if ... else ...")
  Syntax.ExpressionLambda v1 -> (encodeLambda v1)) expr)

-- | Serialize a factor expression
encodeFactor :: (Syntax.Factor -> Ast.Expr)
encodeFactor f = ((\x -> case x of
  Syntax.FactorPositive v1 -> (Serialization.noSep [
    Serialization.cst "+",
    (encodeFactor v1)])
  Syntax.FactorNegative v1 -> (Serialization.noSep [
    Serialization.cst "-",
    (encodeFactor v1)])
  Syntax.FactorComplement v1 -> (Serialization.noSep [
    Serialization.cst "~",
    (encodeFactor v1)])
  Syntax.FactorSimple v1 -> (encodePower v1)) f)

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
  Syntax.GroupExpression v1 -> (encodeNamedExpression v1)
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
  Syntax.ImportFromTargetsSimple v1 -> (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeImportFromAsName v1))
  Syntax.ImportFromTargetsParens v1 -> (Serialization.noSep [
    Serialization.cst "(",
    (Serialization.commaSep Serialization.inlineStyle (Lists.map encodeImportFromAsName v1)),
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
  Syntax.ImportStatementName v1 -> (encodeImportName v1)
  Syntax.ImportStatementFrom v1 -> (encodeImportFrom v1)) is_)

-- | Serialize an inversion (not expression)
encodeInversion :: (Syntax.Inversion -> Ast.Expr)
encodeInversion i = ((\x -> case x of
  Syntax.InversionNot v1 -> (Serialization.spaceSep [
    Serialization.cst "not",
    (encodeInversion v1)])
  Syntax.InversionSimple v1 -> (encodeComparison v1)) i)

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
  Syntax.KwargOrDoubleStarredKwarg v1 -> (encodeKwarg v1)
  Syntax.KwargOrDoubleStarredDoubleStarred v1 -> (Serialization.noSep [
    Serialization.cst "**",
    (encodeExpression v1)])) kds)

-- | Serialize a kwarg or starred
encodeKwargOrStarred :: (Syntax.KwargOrStarred -> Ast.Expr)
encodeKwargOrStarred ks = ((\x -> case x of
  Syntax.KwargOrStarredKwarg v1 -> (encodeKwarg v1)
  Syntax.KwargOrStarredStarred v1 -> (encodeStarredExpression v1)) ks)

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
  Syntax.LambdaStarEtcParamNoDefault v1 -> (encodeLambdaParamNoDefault v1)
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
  Syntax.NamedExpressionSimple v1 -> (encodeExpression v1)
  Syntax.NamedExpressionAssignment v1 -> (encodeAssignmentExpression v1)) ne)

-- | Serialize a name or attribute
encodeNameOrAttribute :: (Syntax.NameOrAttribute -> Ast.Expr)
encodeNameOrAttribute noa = (Serialization.dotSep (Lists.map encodeName (Syntax.unNameOrAttribute noa)))

-- | Serialize a Python number literal
encodeNumber :: (Syntax.Number -> Ast.Expr)
encodeNumber num = ((\x -> case x of
  Syntax.NumberFloat v1 -> (Serialization.cst (Literals.showBigfloat v1))
  Syntax.NumberInteger v1 -> (Serialization.cst (Literals.showBigint v1))) num)

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
  Syntax.ParametersParamNoDefault v1 -> (encodeParamNoDefaultParameters v1)
  Syntax.ParametersSlashNoDefault _ -> (Serialization.cst "...")
  Syntax.ParametersSlashWithDefault _ -> (Serialization.cst "...")) p)

-- | Serialize a pattern
encodePattern :: (Syntax.Pattern -> Ast.Expr)
encodePattern p = ((\x -> case x of
  Syntax.PatternOr v1 -> (encodeOrPattern v1)
  Syntax.PatternAs _ -> (Serialization.cst "... as ...")) p)

-- | Serialize a pattern capture target
encodePatternCaptureTarget :: (Syntax.PatternCaptureTarget -> Ast.Expr)
encodePatternCaptureTarget pct = (encodeName (Syntax.unPatternCaptureTarget pct))

-- | Serialize patterns
encodePatterns :: (Syntax.Patterns -> Ast.Expr)
encodePatterns ps = ((\x -> case x of
  Syntax.PatternsPattern v1 -> (encodePattern v1)
  Syntax.PatternsSequence _ -> (Serialization.cst "...")) ps)

-- | Serialize a positional argument
encodePosArg :: (Syntax.PosArg -> Ast.Expr)
encodePosArg pa = ((\x -> case x of
  Syntax.PosArgStarred v1 -> (encodeStarredExpression v1)
  Syntax.PosArgAssignment v1 -> (encodeAssignmentExpression v1)
  Syntax.PosArgExpression v1 -> (encodeExpression v1)) pa)

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
  Syntax.PrimarySimple v1 -> (encodeAtom v1)
  Syntax.PrimaryCompound v1 -> (encodePrimaryWithRhs v1)) p)

-- | Serialize a primary RHS
encodePrimaryRhs :: (Syntax.PrimaryRhs -> Ast.Expr)
encodePrimaryRhs rhs = ((\x -> case x of
  Syntax.PrimaryRhsCall v1 -> (Serialization.noSep [
    Serialization.cst "(",
    (encodeArgs v1),
    (Serialization.cst ")")])
  Syntax.PrimaryRhsProject v1 -> (Serialization.noSep [
    Serialization.cst ".",
    (encodeName v1)])
  Syntax.PrimaryRhsSlices v1 -> (Serialization.noSep [
    Serialization.cst "[",
    (encodeSlices v1),
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
  Syntax.SimpleStatementAssignment v1 -> (encodeAssignment v1)
  Syntax.SimpleStatementStarExpressions v1 -> (Serialization.newlineSep (Lists.map encodeStarExpression v1))
  Syntax.SimpleStatementReturn v1 -> (encodeReturnStatement v1)
  Syntax.SimpleStatementRaise v1 -> (encodeRaiseStatement v1)
  Syntax.SimpleStatementPass -> (Serialization.cst "pass")
  Syntax.SimpleStatementBreak -> (Serialization.cst "break")
  Syntax.SimpleStatementContinue -> (Serialization.cst "continue")
  Syntax.SimpleStatementImport v1 -> (encodeImportStatement v1)
  Syntax.SimpleStatementTypeAlias v1 -> (encodeTypeAlias v1)
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
  Syntax.SingleTargetName v1 -> (encodeName v1)
  Syntax.SingleTargetParens _ -> (Serialization.cst "(...)")
  Syntax.SingleTargetSubscriptAttributeTarget _ -> (Serialization.cst "...")) st)

-- | Serialize a slice
encodeSlice :: (Syntax.Slice -> Ast.Expr)
encodeSlice s = ((\x -> case x of
  Syntax.SliceNamed v1 -> (encodeNamedExpression v1)
  Syntax.SliceSlice_ _ -> (Serialization.cst ":")) s)

-- | Serialize a slice or starred expression
encodeSliceOrStarredExpression :: (Syntax.SliceOrStarredExpression -> Ast.Expr)
encodeSliceOrStarredExpression s = ((\x -> case x of
  Syntax.SliceOrStarredExpressionSlice v1 -> (encodeSlice v1)
  Syntax.SliceOrStarredExpressionStarred v1 -> (encodeStarredExpression v1)) s)

-- | Serialize slices
encodeSlices :: (Syntax.Slices -> Ast.Expr)
encodeSlices s =  
  let hd = (Syntax.slicesHead s) 
      tl = (Syntax.slicesTail s)
  in (Serialization.commaSep Serialization.inlineStyle (Lists.cons (encodeSlice hd) (Lists.map encodeSliceOrStarredExpression tl)))

-- | Serialize a star atom
encodeStarAtom :: (Syntax.StarAtom -> Ast.Expr)
encodeStarAtom sa = ((\x -> case x of
  Syntax.StarAtomName v1 -> (encodeName v1)
  Syntax.StarAtomTargetWithStarAtom _ -> (Serialization.cst "(...)")
  Syntax.StarAtomStarTargetsTupleSeq _ -> (Serialization.cst "(...)")
  Syntax.StarAtomStarTargetsListSeq _ -> (Serialization.cst "[...]")) sa)

-- | Serialize a star expression
encodeStarExpression :: (Syntax.StarExpression -> Ast.Expr)
encodeStarExpression se = ((\x -> case x of
  Syntax.StarExpressionStar v1 -> (Serialization.noSep [
    Serialization.cst "*",
    (encodeBitwiseOr v1)])
  Syntax.StarExpressionSimple v1 -> (encodeExpression v1)) se)

-- | Serialize a star named expression
encodeStarNamedExpression :: (Syntax.StarNamedExpression -> Ast.Expr)
encodeStarNamedExpression sne = ((\x -> case x of
  Syntax.StarNamedExpressionStar v1 -> (Serialization.noSep [
    Serialization.cst "*",
    (encodeBitwiseOr v1)])
  Syntax.StarNamedExpressionSimple v1 -> (encodeNamedExpression v1)) sne)

-- | Serialize a star target
encodeStarTarget :: (Syntax.StarTarget -> Ast.Expr)
encodeStarTarget st = ((\x -> case x of
  Syntax.StarTargetUnstarred v1 -> (encodeTargetWithStarAtom v1)
  Syntax.StarTargetStarred v1 -> (Serialization.noSep [
    Serialization.cst "*",
    (encodeStarTarget v1)])) st)

-- | Serialize a starred expression
encodeStarredExpression :: (Syntax.StarredExpression -> Ast.Expr)
encodeStarredExpression se = (Serialization.noSep [
  Serialization.cst "*",
  (encodeExpression (Syntax.unStarredExpression se))])

-- | Serialize a Python statement
encodeStatement :: (Syntax.Statement -> Ast.Expr)
encodeStatement stmt = ((\x -> case x of
  Syntax.StatementAnnotated v1 -> (encodeAnnotatedStatement v1)
  Syntax.StatementSimple v1 -> (Serialization.newlineSep (Lists.map encodeSimpleStatement v1))
  Syntax.StatementCompound v1 -> (encodeCompoundStatement v1)) stmt)

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
  Syntax.SubjectExpressionSimple v1 -> (encodeNamedExpression v1)
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
  Syntax.TargetWithStarAtomAtom v1 -> (encodeStarAtom v1)
  Syntax.TargetWithStarAtomProject _ -> (Serialization.cst "...")
  Syntax.TargetWithStarAtomSlices _ -> (Serialization.cst "...")) t)

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
  Syntax.TypeParameterSimple v1 -> (encodeSimpleTypeParameter v1)
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
