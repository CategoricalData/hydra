module Hydra.Ext.Python.Serde where

import qualified Hydra.Ext.Python.Syntax as Py
import Hydra.Serialization
import qualified Hydra.Ast as A

import qualified Data.List as L
import qualified Data.Maybe as Y


encodeAnnotatedRhs :: Py.AnnotatedRhs -> A.Expr
encodeAnnotatedRhs a = spaceSep [cst "=", case a of
  Py.AnnotatedRhsStar s -> commaSep inlineStyle (encodeStarExpression <$> s)
  _ -> unsupportedVariant "annotated rhs" a]

encodeAnnotatedStatement :: Py.AnnotatedStatement -> A.Expr
encodeAnnotatedStatement (Py.AnnotatedStatement doc stmt) = newlineSep [cst $ toPythonComments doc, encodeStatement stmt]

encodeAnnotation :: Py.Annotation -> A.Expr
encodeAnnotation (Py.Annotation expr) = spaceSep [cst ":", encodeExpression expr]

encodeArgs :: Py.Args -> A.Expr
encodeArgs (Py.Args positional kwargStarred kwargDoubleStarred) = commaSep inlineStyle $ ps ++ ks ++ kss
  where
    ps = encodePosArg <$> positional
    ks = encodeKwargOrStarred <$> kwargStarred
    kss = encodeKwargOrDoubleStarred <$> kwargDoubleStarred

encodeAssignment :: Py.Assignment -> A.Expr
encodeAssignment a = case a of
  Py.AssignmentTyped t -> encodeTypedAssignment t
  Py.AssignmentUntyped u -> encodeUntypedAssignment u
  _ -> unsupportedVariant "assignment" a

encodeAssignmentExpression :: Py.AssignmentExpression -> A.Expr
encodeAssignmentExpression (Py.AssignmentExpression name expr)
  = spaceSep [encodeName name, cst ":=", encodeExpression expr]

encodeAtom :: Py.Atom -> A.Expr
encodeAtom a = case a of
  Py.AtomDict d -> encodeDict d
  Py.AtomEllipsis -> cst "..."
  Py.AtomFalse -> cst "False"
  Py.AtomGroup g -> encodeGroup g
  Py.AtomList l -> encodeList l
  Py.AtomName n -> encodeName n
  Py.AtomNumber n -> encodeNumber n
  Py.AtomSet s -> encodeSet s
  Py.AtomString s -> encodeString s
  Py.AtomTrue -> cst "True"
  Py.AtomTuple t -> encodeTuple t
  _ -> unsupportedVariant "atom" a

encodeAttribute :: Py.Attribute -> A.Expr
encodeAttribute (Py.Attribute names) = dotSep $ encodeName <$> names

encodeAwaitPrimary :: Py.AwaitPrimary -> A.Expr
encodeAwaitPrimary (Py.AwaitPrimary await primary) = if await
  then spaceSep [cst "await", encodePrimary primary]
  else encodePrimary primary

encodeBitwiseAnd :: Py.BitwiseAnd -> A.Expr
encodeBitwiseAnd (Py.BitwiseAnd lhs rhs) = spaceSep $ Y.catMaybes [encodeLhs <$> lhs, Just $ encodeShiftExpression rhs]
  where
    encodeLhs l = spaceSep [encodeBitwiseAnd l, cst "&"]

encodeBitwiseOr :: Py.BitwiseOr -> A.Expr
encodeBitwiseOr (Py.BitwiseOr lhs rhs) = spaceSep $ Y.catMaybes [encodeLhs <$> lhs, Just $ encodeBitwiseXor rhs]
  where
    encodeLhs l = spaceSep [encodeBitwiseOr l, cst "|"]

encodeBitwiseXor :: Py.BitwiseXor -> A.Expr
encodeBitwiseXor (Py.BitwiseXor lhs rhs) = spaceSep $ Y.catMaybes [encodeLhs <$> lhs, Just $ encodeBitwiseAnd rhs]
  where
    encodeLhs l = spaceSep [encodeBitwiseXor l, cst "^"]

encodeBlock :: Py.Block -> A.Expr
encodeBlock b = case b of
  Py.BlockIndented sc -> tabIndentDoubleSpace (encodeGroup <$> sc)
    where
      encodeGroup ss = newlineSep (encodeStatement <$> ss)
  Py.BlockSimple ss -> semicolonSep (encodeSimpleStatement <$> ss)

encodeCapturePattern :: Py.CapturePattern -> A.Expr
encodeCapturePattern (Py.CapturePattern t) = encodePatternCaptureTarget t

encodeCaseBlock :: Py.CaseBlock -> A.Expr
encodeCaseBlock (Py.CaseBlock patterns guard body) = newlineSep [
  noSep [
    spaceSep $ Y.catMaybes [
      Just $ cst "case",
      Just $ encodePatterns patterns,
      encodeGuard <$> guard],
    cst ":"],
  encodeBlock body]

encodeClassDefinition :: Py.ClassDefinition -> A.Expr
encodeClassDefinition (Py.ClassDefinition mdecs name tparams args body) = newlineSep $
  Y.catMaybes [encodeDecorators <$> mdecs, Just classExpr]
  where
    header = noSep $ Y.catMaybes [
        Just $ spaceSep $ Y.catMaybes [Just $ cst "class", Just $ encodeName name],
        (argExp <$> args),
        Just $ cst ":"]
      where
        argExp a = noSep [cst "(", encodeArgs a, cst ")"]
    classExpr = case body of -- TODO: tparams
      Py.BlockSimple _ -> spaceSep [header, encodeBlock body]
      Py.BlockIndented _ -> newlineSep [header, encodeBlock body]

encodeClassPattern :: Py.ClassPattern -> A.Expr
encodeClassPattern (Py.ClassPattern named mpos mkw) = noSep $ Y.catMaybes [
    Just $ encodeNameOrAttribute named,
    Just $ cst "(",
    encodePositionalPatterns <$> mpos,
    -- TODO: keyword patterns
    Just $ cst ")"]

encodeClosedPattern :: Py.ClosedPattern -> A.Expr
encodeClosedPattern cp = case cp of
  Py.ClosedPatternCapture c -> encodeCapturePattern c
  Py.ClosedPatternClass p -> encodeClassPattern p
  Py.ClosedPatternValue v -> encodeValuePattern v
  Py.ClosedPatternWildcard -> cst "_"
  _ -> unsupportedVariant "closed pattern" cp

encodeCompareOpBitwiseOrPair :: Py.CompareOpBitwiseOrPair -> A.Expr
encodeCompareOpBitwiseOrPair p = unsupportedType "compare op bitwise or pair"

encodeComparison :: Py.Comparison -> A.Expr
encodeComparison (Py.Comparison lhs rhs) = spaceSep $ [encodeBitwiseOr lhs] ++ (encodeCompareOpBitwiseOrPair <$> rhs)

encodeCompoundStatement :: Py.CompoundStatement -> A.Expr
encodeCompoundStatement c = case c of
  Py.CompoundStatementClassDef d -> encodeClassDefinition d
  Py.CompoundStatementFunction f -> encodeFunctionDefinition f
  Py.CompoundStatementMatch m -> encodeMatchStatement m
  _ -> unsupportedVariant "compound statement" c

encodeConjunction :: Py.Conjunction -> A.Expr
encodeConjunction (Py.Conjunction is) = symbolSep "and" inlineStyle (encodeInversion <$> is)

encodeDecorators :: Py.Decorators -> A.Expr
encodeDecorators (Py.Decorators exprs) = newlineSep (encodeDec <$> exprs)
  where
    encodeDec ne = noSep [cst "@", encodeNamedExpression ne]

encodeDict :: Py.Dict -> A.Expr
encodeDict (Py.Dict items) = curlyBracesList Nothing halfBlockStyle (encodeDoubleStarredKvpair <$> items)

encodeDisjunction :: Py.Disjunction -> A.Expr
encodeDisjunction (Py.Disjunction cs) = symbolSep "or" inlineStyle (encodeConjunction <$> cs)

encodeDottedAsName :: Py.DottedAsName -> A.Expr
encodeDottedAsName (Py.DottedAsName name mas) = spaceSep $ Y.catMaybes [Just $ encodeDottedName name, encodeName <$> mas]
  where
    encodeAs a = spaceSep [cst "as", encodeName a]

encodeDottedName :: Py.DottedName -> A.Expr
encodeDottedName (Py.DottedName names) = cst $ L.intercalate "." (Py.unName <$> names)

encodeDoubleStarredKvpair :: Py.DoubleStarredKvpair -> A.Expr
encodeDoubleStarredKvpair d = case d of
  Py.DoubleStarredKvpairPair p -> encodeKvpair p
  _ -> unsupportedVariant "double starred kv pair" d

encodeExpression :: Py.Expression -> A.Expr
encodeExpression e = case e of
  Py.ExpressionLambda l -> encodeLambda l
  Py.ExpressionSimple d -> encodeDisjunction d
  _ -> unsupportedVariant "expression" e

encodeFactor :: Py.Factor -> A.Expr
encodeFactor f = case f of
  Py.FactorPositive f -> noSep [cst "+", encodeFactor f]
  Py.FactorNegative f -> noSep [cst "-", encodeFactor f]
  Py.FactorComplement f -> noSep [cst "~", encodeFactor f]
  Py.FactorSimple p -> encodePower p

encodeFunctionDefRaw :: Py.FunctionDefRaw -> A.Expr
encodeFunctionDefRaw (Py.FunctionDefRaw async name tparams params retType ftc block) = newlineSep [
    spaceSep $ Y.catMaybes [
      if async then Just $ cst "async" else Nothing,
      Just $ cst "def",
      Just $ noSep $ Y.catMaybes [
        Just $ spaceSep $ Y.catMaybes [
        Just $ noSep $ Y.catMaybes[
          Just $ encodeName name,
          Nothing, -- TODO: tparams
          Just $ cst "(",
          encodeParameters <$> params,
          Just $ cst ")"],
          retExpr],
        Just $ cst ":"],
      Nothing], -- TODO: ftc
    encodeBlock block]
  where
    retExpr = fmap (\t -> spaceSep[cst "->", encodeExpression t]) retType

encodeFunctionDefinition :: Py.FunctionDefinition -> A.Expr
encodeFunctionDefinition (Py.FunctionDefinition mdecs raw) = encodeFunctionDefRaw raw

encodeGroup :: Py.Group -> A.Expr
encodeGroup g = case g of
  Py.GroupExpression ne -> encodeNamedExpression ne
  _ -> unsupportedVariant "group" g

encodeGuard :: Py.Guard -> A.Expr
encodeGuard (Py.Guard ne) = spaceSep [cst "if", encodeNamedExpression ne]

encodeImportFrom :: Py.ImportFrom -> A.Expr
encodeImportFrom (Py.ImportFrom prefixes name targets) = spaceSep [cst "from", lhs, cst "import", rhs]
  where
    lhs = noSep $ Y.catMaybes $ (Just . encodeRelativeImportPrefix <$> prefixes) ++ [encodeDottedName <$> name]
    rhs = encodeImportFromTargets targets

encodeImportFromAsName :: Py.ImportFromAsName -> A.Expr
encodeImportFromAsName (Py.ImportFromAsName name malias) = case malias of
  Nothing -> encodeName name
  Just a -> spaceSep [encodeName name, cst "as", encodeName a]

encodeImportFromTargets :: Py.ImportFromTargets -> A.Expr
encodeImportFromTargets i = case i of
    Py.ImportFromTargetsSimple t -> names t
    Py.ImportFromTargetsParens t -> noSep [cst "(", names t, cst ")"]
    Py.ImportFromTargetsStar -> cst "*"
  where
    names :: [Py.ImportFromAsName] -> A.Expr
    names t = commaSep inlineStyle (encodeImportFromAsName <$> t)

encodeImportName :: Py.ImportName -> A.Expr
encodeImportName (Py.ImportName dans) = spaceSep [cst "import", commaSep inlineStyle (encodeDottedAsName <$> dans)]

encodeImportStatement :: Py.ImportStatement -> A.Expr
encodeImportStatement s = case s of
  Py.ImportStatementName n -> encodeImportName n
  Py.ImportStatementFrom i -> encodeImportFrom i

encodeInversion :: Py.Inversion -> A.Expr
encodeInversion i = case i of
  Py.InversionNot other -> spaceSep [cst "not", encodeInversion other]
  Py.InversionSimple c -> encodeComparison c

encodeKvpair :: Py.Kvpair -> A.Expr
encodeKvpair (Py.Kvpair k v) = spaceSep [noSep [encodeExpression k, cst ":"], encodeExpression v]

encodeKwarg :: Py.Kwarg -> A.Expr
encodeKwarg (Py.Kwarg name expr) = noSep [encodeName name, cst "=", encodeExpression expr]

encodeKwargOrDoubleStarred :: Py.KwargOrDoubleStarred -> A.Expr
encodeKwargOrDoubleStarred k = case k of
  Py.KwargOrDoubleStarredKwarg kw -> encodeKwarg kw
  Py.KwargOrDoubleStarredDoubleStarred e -> noSep [cst "**", encodeExpression e]

encodeKwargOrStarred :: Py.KwargOrStarred -> A.Expr
encodeKwargOrStarred k = case k of
  Py.KwargOrStarredKwarg kw -> encodeKwarg kw
  Py.KwargOrStarredStarred se -> encodeStarredExpression se

encodeLambda :: Py.Lambda -> A.Expr
encodeLambda (Py.Lambda params body) = spaceSep [cst "lambda", paramSec, encodeExpression body]
  where
    paramSec = noSep [encodeLambdaParameters params, cst ":"]

encodeLambdaParamNoDefault :: Py.LambdaParamNoDefault -> A.Expr
encodeLambdaParamNoDefault (Py.LambdaParamNoDefault name) = encodeName name

encodeLambdaParameters :: Py.LambdaParameters -> A.Expr
encodeLambdaParameters (Py.LambdaParameters slashNoDefault paramNoDefault paramWithDefault starEtc) =
  commaSep inlineStyle $ Y.catMaybes [
    Nothing, -- TODO: slashNoDefault
    Nothing, -- TODO: paramNoDefault
    Nothing, -- TODO: paramWithDefault
    encodeLambdaStarEtc <$> starEtc]

encodeLambdaStarEtc :: Py.LambdaStarEtc -> A.Expr
encodeLambdaStarEtc l = case l of
  Py.LambdaStarEtcParamNoDefault p -> encodeLambdaParamNoDefault p
  _ -> unsupportedVariant "lambda star etc" l

encodeList :: Py.List -> A.Expr
encodeList (Py.List es) = bracketListAdaptive (encodeStarNamedExpression <$> es)

encodeMatchStatement :: Py.MatchStatement -> A.Expr
encodeMatchStatement (Py.MatchStatement subj cases) = newlineSep [
  spaceSep [cst "match", noSep [encodeSubjectExpression subj, cst ":"]],
  tabIndentDoubleSpace (encodeCaseBlock <$> cases)]

encodeModule :: Py.Module -> A.Expr
encodeModule (Py.Module groups) = doubleNewlineSep (encodeGroup <$> groups)
  where
    encodeGroup ss = newlineSep (encodeStatement <$> ss)

encodeName :: Py.Name -> A.Expr
encodeName (Py.Name n) = cst n

encodeNamedExpression :: Py.NamedExpression -> A.Expr
encodeNamedExpression ne = case ne of
  Py.NamedExpressionSimple e -> encodeExpression e
  _ -> unsupportedVariant "named expression" ne

encodeNameOrAttribute :: Py.NameOrAttribute -> A.Expr
encodeNameOrAttribute (Py.NameOrAttribute names) = dotSep (encodeName <$> names)

encodeNumber :: Py.Number -> A.Expr
encodeNumber n = case n of
  Py.NumberFloat f -> cst $ show f
  Py.NumberInteger i -> cst $ show i

encodeOrPattern :: Py.OrPattern -> A.Expr
encodeOrPattern (Py.OrPattern p) = symbolSep "|" inlineStyle (encodeClosedPattern <$> p)

encodeParam :: Py.Param -> A.Expr
encodeParam (Py.Param name mann) = noSep $ Y.catMaybes [
  Just $ encodeName name,
  fmap encodeAnnotation mann]

encodeParamNoDefault :: Py.ParamNoDefault -> A.Expr
encodeParamNoDefault (Py.ParamNoDefault param tcomment) = encodeParam param

encodeParamNoDefaultParameters :: Py.ParamNoDefaultParameters -> A.Expr
encodeParamNoDefaultParameters (Py.ParamNoDefaultParameters nodef withdef staretc)
  = commaSep inlineStyle (encodeParamNoDefault <$> nodef) -- TODO: withdef, staretc

encodeParameters :: Py.Parameters -> A.Expr
encodeParameters p = case p of
  Py.ParametersParamNoDefault p -> encodeParamNoDefaultParameters p
  _ -> unsupportedVariant "parameters" p

encodePattern :: Py.Pattern -> A.Expr
encodePattern p = case p of
  Py.PatternOr op -> encodeOrPattern op
  _ -> unsupportedVariant "pattern" p

encodePatternCaptureTarget :: Py.PatternCaptureTarget -> A.Expr
encodePatternCaptureTarget (Py.PatternCaptureTarget name) = encodeName name

encodePatterns :: Py.Patterns -> A.Expr
encodePatterns ps = case ps of
  Py.PatternsPattern p -> encodePattern p
  _ -> unsupportedVariant "patterns" ps

encodePosArg :: Py.PosArg -> A.Expr
encodePosArg a = case a of
  Py.PosArgStarred se -> encodeStarredExpression se
  Py.PosArgAssignment ae -> encodeAssignmentExpression ae
  Py.PosArgExpression e -> encodeExpression e

encodePositionalPatterns :: Py.PositionalPatterns -> A.Expr
encodePositionalPatterns (Py.PositionalPatterns pats) = commaSep inlineStyle (encodePattern <$> pats)

encodePower :: Py.Power -> A.Expr
encodePower (Py.Power lhs rhs) = spaceSep $ Y.catMaybes [Just $ encodeAwaitPrimary lhs, encodeRhs <$> rhs]
  where
    encodeRhs r = spaceSep [cst "**", encodeFactor r]

encodePrimary :: Py.Primary -> A.Expr
encodePrimary p = case p of
  Py.PrimarySimple a -> encodeAtom a
  Py.PrimaryCompound p -> encodePrimaryWithRhs p

encodePrimaryRhs :: Py.PrimaryRhs -> A.Expr
encodePrimaryRhs p = case p of
  Py.PrimaryRhsCall args -> noSep [cst "(", encodeArgs args, cst ")"]
  Py.PrimaryRhsProject name -> noSep [cst ".", encodeName name]
  Py.PrimaryRhsSlices slices -> noSep [cst "[", encodeSlices slices, cst "]"]
  _ -> unsupportedVariant "primary rhs" p

encodePrimaryWithRhs :: Py.PrimaryWithRhs -> A.Expr
encodePrimaryWithRhs (Py.PrimaryWithRhs primary rhs) = noSep [encodePrimary primary, encodePrimaryRhs rhs]

encodeRaiseExpression :: Py.RaiseExpression -> A.Expr
encodeRaiseExpression (Py.RaiseExpression expr mfrom) = spaceSep $ Y.catMaybes [Just $ encodeExpression expr, fmap toFrom mfrom]
  where
    toFrom e = spaceSep [cst "from", encodeExpression e]

encodeRaiseStatement :: Py.RaiseStatement -> A.Expr
encodeRaiseStatement (Py.RaiseStatement me) = spaceSep $ Y.catMaybes [Just $ cst "raise", encodeRaiseExpression <$> me]

encodeRelativeImportPrefix :: Py.RelativeImportPrefix -> A.Expr
encodeRelativeImportPrefix p = cst $ case p of
  Py.RelativeImportPrefixDot -> "."
  Py.RelativeImportPrefixEllipsis -> "..."

encodeReturnStatement :: Py.ReturnStatement -> A.Expr
encodeReturnStatement (Py.ReturnStatement es) = spaceSep [cst "return", commaSep inlineStyle (encodeStarExpression <$> es)]

encodeSet :: Py.Set -> A.Expr
encodeSet (Py.Set es) = bracesListAdaptive (encodeStarNamedExpression <$> es)

encodeShiftExpression :: Py.ShiftExpression -> A.Expr
encodeShiftExpression (Py.ShiftExpression lhs rhs) = spaceSep $ Y.catMaybes [encodeShiftLhs <$> lhs, Just $ encodeSum rhs]

encodeShiftLhs :: Py.ShiftLhs -> A.Expr
encodeShiftLhs l = unsupportedType "shift lhs"

encodeSimpleStatement :: Py.SimpleStatement -> A.Expr
encodeSimpleStatement s = case s of
  Py.SimpleStatementAssignment a -> encodeAssignment a
  Py.SimpleStatementImport i -> encodeImportStatement i
  Py.SimpleStatementPass -> cst "pass"
  Py.SimpleStatementRaise r -> encodeRaiseStatement r
  Py.SimpleStatementReturn r -> encodeReturnStatement r
  Py.SimpleStatementTypeAlias t -> encodeTypeAlias t
  Py.SimpleStatementStarExpressions exprs -> newlineSep (encodeStarExpression <$> exprs)
  _ -> unsupportedVariant "simple statement" s

encodeSimpleTypeParameter :: Py.SimpleTypeParameter -> A.Expr
encodeSimpleTypeParameter (Py.SimpleTypeParameter name _ _) = encodeName name

encodeSingleTarget :: Py.SingleTarget -> A.Expr
encodeSingleTarget s = case s of
  Py.SingleTargetName n -> encodeName n
  _ -> unsupportedVariant "single target" s

encodeSlice :: Py.Slice -> A.Expr
encodeSlice s = case s of
  Py.SliceNamed n -> encodeNamedExpression n
  _ -> unsupportedVariant "slice" s

encodeSliceOrStarredExpression :: Py.SliceOrStarredExpression -> A.Expr
encodeSliceOrStarredExpression s = case s of
  Py.SliceOrStarredExpressionSlice s -> encodeSlice s
  Py.SliceOrStarredExpressionStarred e -> encodeStarredExpression e

encodeSlices :: Py.Slices -> A.Expr
encodeSlices (Py.Slices head tail) = commaSep inlineStyle (ehead:etail)
  where
    ehead = encodeSlice head
    etail = encodeSliceOrStarredExpression <$> tail

encodeStarAtom :: Py.StarAtom -> A.Expr
encodeStarAtom a = case a of
  Py.StarAtomName n -> encodeName n
  _ -> unsupportedVariant "star atom" a

encodeStarExpression :: Py.StarExpression -> A.Expr
encodeStarExpression s = case s of
  Py.StarExpressionSimple e -> encodeExpression e
  _ -> unsupportedVariant "star expression" s

encodeStarNamedExpression :: Py.StarNamedExpression -> A.Expr
encodeStarNamedExpression s = case s of
  Py.StarNamedExpressionStar bor -> noSep [cst "*", encodeBitwiseOr bor]
  Py.StarNamedExpressionSimple ne -> encodeNamedExpression ne

encodeStarTarget :: Py.StarTarget -> A.Expr
encodeStarTarget s = case s of
  Py.StarTargetUnstarred t -> encodeTargetWithStarAtom t
  _ -> unsupportedVariant "star target" s

encodeStarredExpression :: Py.StarredExpression -> A.Expr
encodeStarredExpression (Py.StarredExpression expr) = noSep [cst "*", encodeExpression expr]

encodeStatement :: Py.Statement -> A.Expr
encodeStatement s = case s of
  Py.StatementAnnotated a -> encodeAnnotatedStatement a
  Py.StatementCompound c -> encodeCompoundStatement c
  Py.StatementSimple stmts -> newlineSep (encodeSimpleStatement <$> stmts)

encodeString :: Py.String_ -> A.Expr
encodeString (Py.String_ s style) = case style of
  Py.QuoteStyleSingle -> cst $ escapePythonString False s
  Py.QuoteStyleDouble -> cst $ escapePythonString True s
  Py.QuoteStyleTriple -> tripleQuotes s

encodeSubjectExpression :: Py.SubjectExpression -> A.Expr
encodeSubjectExpression s = case s of
  Py.SubjectExpressionSimple e -> encodeNamedExpression e
  _ -> unsupportedVariant "subject expression" s

encodeSum :: Py.Sum -> A.Expr
encodeSum (Py.Sum lhs rhs) = spaceSep $ Y.catMaybes [encodeSumLhs <$> lhs, Just $ encodeTerm rhs]

encodeSumLhs :: Py.SumLhs -> A.Expr
encodeSumLhs l = unsupportedType "sum lhs"

encodeTerm :: Py.Term -> A.Expr
encodeTerm (Py.Term lhs rhs) = spaceSep $ Y.catMaybes [encodeTermLhs <$> lhs, Just $ encodeFactor rhs]

encodeTermLhs :: Py.TermLhs -> A.Expr
encodeTermLhs l = unsupportedType "term lhs"

encodeTargetWithStarAtom :: Py.TargetWithStarAtom -> A.Expr
encodeTargetWithStarAtom t = case t of
  Py.TargetWithStarAtomAtom a -> encodeStarAtom a
  _ -> unsupportedVariant "target with star atom" t

encodeTuple :: Py.Tuple -> A.Expr
encodeTuple (Py.Tuple es) = parenList False (encodeStarNamedExpression <$> es)

encodeTypeAlias :: Py.TypeAlias -> A.Expr
encodeTypeAlias (Py.TypeAlias name tparams body) = spaceSep [cst "type", alias, cst "=", encodeExpression body]
  where
    alias = noSep $ Y.catMaybes [
      Just $ encodeName name,
      if L.null tparams then Nothing else Just $ bracketList inlineStyle (encodeTypeParameter <$> tparams)]

encodeTypeParameter :: Py.TypeParameter -> A.Expr
encodeTypeParameter p = case p of
  Py.TypeParameterSimple s -> encodeSimpleTypeParameter s
  _ -> unsupportedVariant "type parameter" p

encodeTypedAssignment :: Py.TypedAssignment -> A.Expr
encodeTypedAssignment (Py.TypedAssignment lhs typ rhs) = spaceSep $ Y.catMaybes [
    Just head, Just $ encodeExpression typ, encodeAnnotatedRhs <$> rhs]
  where
    head = noSep [encodeSingleTarget lhs, cst ":"]

encodeUntypedAssignment :: Py.UntypedAssignment -> A.Expr
encodeUntypedAssignment (Py.UntypedAssignment targets rhs _) = spaceSep $ lefts ++ [right]
  where
    lefts = encodeStarTarget <$> targets
    right = encodeAnnotatedRhs rhs

encodeValuePattern :: Py.ValuePattern -> A.Expr
encodeValuePattern (Py.ValuePattern attr) = encodeAttribute attr

-- TODO: this is a partially ChatGPT-generated function which has not been thoroughly tested.
escapePythonString :: Bool -> String -> String
escapePythonString doubleQuoted str = encChar : L.concatMap escapeChar str ++ [encChar]
  where
    encChar = if doubleQuoted then '"' else '\''
    escapeChar '\'' = if doubleQuoted then  "'" else "\\'"  -- Escape single quote
    escapeChar '"'  = if doubleQuoted then "\\\"" else "\""  -- Escape double quote
    escapeChar '\\' = "\\\\" -- Escape backslash
    escapeChar '\n' = "\\n"  -- Escape newline
    escapeChar '\t' = "\\t"  -- Escape tab
    escapeChar '\r' = "\\r"  -- Escape carriage return
    escapeChar c
      | c < ' '   = "\\x" ++ toHex (fromEnum c) -- Escape other control chars as hex
      | otherwise = [c]  -- Leave normal characters as is

    toHex :: Int -> String
    toHex n = let hex = "0123456789abcdef"
              in [hex !! (n `div` 16), hex !! (n `mod` 16)]

toPythonComments :: String -> String
toPythonComments c = L.intercalate "\n" $ ("# " ++) <$> L.lines c

tripleQuotes :: String -> A.Expr
tripleQuotes s = cst $ "\"\"\"" ++ s ++ "\"\"\"" -- TODO: escaping

unsupportedVariant :: Show a => String -> a -> A.Expr
unsupportedVariant label obj = cst $ "[unsupported " ++ label ++ ": " ++ show obj ++ "]"
