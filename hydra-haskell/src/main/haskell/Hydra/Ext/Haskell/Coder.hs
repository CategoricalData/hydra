module Hydra.Ext.Haskell.Coder (printModule) where

import Hydra.Kernel
import Hydra.CoreDecoding
import Hydra.Adapters.Coders
import Hydra.Ext.Haskell.Language
import Hydra.Ext.Haskell.Utils
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.Terms
import Hydra.Util.Codetree.Script
import Hydra.Ext.Haskell.Serde
import Hydra.Ext.Haskell.Settings
import Hydra.Types.Inference

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


constantDecls :: Context m -> Namespaces -> Name -> Type m -> [H.DeclarationWithComments]
constantDecls cx namespaces name@(Name nm) typ = if useCoreImport
    then toDecl (Name "hydra/core.Name") nameDecl:(toDecl (Name "hydra/core.FieldName") <$> fieldDecls)
    else []
  where
    lname = localNameOfEager name
    toDecl n (k, v) = H.DeclarationWithComments decl Nothing
      where
        decl = H.DeclarationValueBinding $
          H.ValueBindingSimple $ H.ValueBinding_Simple pat rhs Nothing
        pat = H.PatternApplication $ H.Pattern_Application (simpleName k) []
        rhs = H.RightHandSide $ H.ExpressionApplication $ H.Expression_Application
          (H.ExpressionVariable $ elementReference namespaces n)
          (H.ExpressionLiteral $ H.LiteralString v)
    nameDecl = ("_" ++ lname, nm)
    fieldsOf t = case stripType t of
      TypeRecord rt -> rowTypeFields rt
      TypeUnion rt -> rowTypeFields rt
      _ -> []
    fieldDecls = toConstant <$> fieldsOf (snd $ unpackLambdaType cx typ)
    toConstant (FieldType (FieldName fname) _) = ("_" ++ lname ++ "_" ++ fname, fname)

constructModule :: (Ord m, Read m, Show m)
  => Module m
  -> M.Map (Type m) (Coder (Context m) (Context m) (Term m) H.Expression)
  -> [(Element m, TypedTerm m)] -> GraphFlow m H.Module
constructModule mod coders pairs = do
    cx <- getState
    decls <- L.concat <$> CM.mapM (createDeclarations cx) pairs
    let mc = moduleDescription mod
    return $ H.Module (Just $ H.ModuleHead mc (importName $ h $ moduleNamespace mod) []) imports decls
  where
    h (Namespace name) = name

    createDeclarations cx pair@(el, TypedTerm typ term) = if isType typ
      then toTypeDeclarations namespaces el term
      else do
        d <- toDataDeclaration coders namespaces pair
        return [d]

    namespaces = namespacesForModule mod
    importName name = H.ModuleName $ L.intercalate "." (capitalize <$> Strings.splitOn "/" name)
    imports = domainImports ++ standardImports
      where
        domainImports = toImport <$> M.toList (namespacesMapping namespaces)
          where
            toImport (Namespace name, alias) = H.Import True (importName name) (Just alias) Nothing
        standardImports = toImport . H.ModuleName <$> Y.catMaybes [
            Just "Data.List",
            Just "Data.Map",
            Just "Data.Set"{-,
            if useCoreImport && moduleNamespace g /= Namespace "hydra/core"
              then Just "Hydra.Core"
              else Nothing-}]
          where
            toImport name = H.Import False name Nothing Nothing

encodeAdaptedType :: (Ord m, Read m, Show m) => Namespaces -> Type m -> GraphFlow m H.Type
encodeAdaptedType namespaces typ = adaptType haskellLanguage typ >>= encodeType namespaces

encodeFunction :: (Eq m, Ord m, Read m, Show m) => Namespaces -> Function m -> GraphFlow m H.Expression
encodeFunction namespaces fun = case fun of
    FunctionElimination e -> case e of
      EliminationElement -> pure $ hsvar "id"
      EliminationList fun -> do
        let lhs = hsvar "foldl"
        rhs <- encodeTerm namespaces fun
        return $ hsapp lhs rhs
      EliminationWrap name -> pure $ H.ExpressionVariable $ elementReference namespaces $
        qname (namespaceOfEager name) $ newtypeAccessorName name
      EliminationOptional (OptionalCases nothing just) -> do
        nothingRhs <- H.CaseRhs <$> encodeTerm namespaces nothing
        let nothingAlt = H.Alternative (H.PatternName $ simpleName "Nothing") nothingRhs Nothing
        justAlt <- do
          -- Note: some of the following could be brought together with FunctionCases
          let v0 = "v"
          let rhsTerm = simplifyTerm $ apply just (variable v0)
          let v1 = if S.member (Name v0) $ freeVariablesInTerm rhsTerm then v0 else "_"
          let lhs = H.PatternApplication $ H.Pattern_Application (rawName "Just") [H.PatternName $ rawName v1]
          rhs <- H.CaseRhs <$> encodeTerm namespaces rhsTerm
          return $ H.Alternative lhs rhs Nothing
        return $ H.ExpressionCase $ H.Expression_Case (hsvar "x") [nothingAlt, justAlt]
      EliminationRecord (Projection dn fname) -> return $ H.ExpressionVariable $ recordFieldReference namespaces dn fname
      EliminationUnion (CaseStatement dn fields) -> hslambda "x" <$> caseExpr -- note: could use a lambda case here
        where
          caseExpr = do
            rt <- withSchemaContext $ requireUnionType False dn
            let fieldMap = M.fromList $ (\f -> (fieldTypeName f, f)) <$> rowTypeFields rt
            H.ExpressionCase <$> (H.Expression_Case (hsvar "x") <$> CM.mapM (toAlt fieldMap) fields)
          toAlt fieldMap (Field fn fun') = do
            let v0 = "v"
            let raw = apply fun' (variable v0)
            let rhsTerm = simplifyTerm raw
            let v1 = if isFreeIn (Name v0) rhsTerm then "_" else v0
            let hname = unionFieldReference namespaces dn fn
            args <- case M.lookup fn fieldMap of
              Just (FieldType _ ft) -> case stripType ft of
                TypeRecord (RowType _ Nothing []) -> pure []
                _ -> pure [H.PatternName $ rawName v1]
              Nothing -> fail $ "field " ++ show fn ++ " not found in " ++ show dn
            let lhs = H.PatternApplication $ H.Pattern_Application hname args
            rhs <- H.CaseRhs <$> encodeTerm namespaces rhsTerm
            return $ H.Alternative lhs rhs Nothing
    FunctionLambda (Lambda (Name v) body) -> hslambda v <$> encodeTerm namespaces body
    FunctionPrimitive name -> pure $ H.ExpressionVariable $ hsPrimitiveReference name
    _ -> fail $ "unexpected function: " ++ show fun

encodeLiteral :: Literal -> GraphFlow m H.Expression
encodeLiteral av = case av of
    LiteralBoolean b -> pure $ hsvar $ if b then "True" else "False"
    LiteralFloat fv -> case fv of
      FloatValueFloat32 f -> pure $ hslit $ H.LiteralFloat f
      FloatValueFloat64 f -> pure $ hslit $ H.LiteralDouble f
      _ -> unexpected "floating-point number" fv
    LiteralInteger iv -> case iv of
      IntegerValueBigint i -> pure $ hslit $ H.LiteralInteger i
      IntegerValueInt32 i -> pure $ hslit $ H.LiteralInt i
      _ -> unexpected "integer" iv
    LiteralString s -> pure $ hslit $ H.LiteralString s
    _ -> unexpected "literal value" av

encodeTerm :: (Eq m, Ord m, Read m, Show m) => Namespaces -> Term m -> GraphFlow m H.Expression
encodeTerm namespaces term = do
   case stripTerm term of
    TermApplication (Application fun arg) -> case stripTerm fun of
       TermFunction (FunctionElimination EliminationElement) -> encode arg
       _ -> hsapp <$> encode fun <*> encode arg
    TermElement name -> pure $ H.ExpressionVariable $ elementReference namespaces name
    TermFunction f -> encodeFunction namespaces f
    TermLet l -> fail $ "unexpected 'let' term nested within a non-let term"
    TermList els -> H.ExpressionList <$> CM.mapM encode els
    TermLiteral v -> encodeLiteral v
    TermWrap (Nominal tname term') -> if newtypesNotTypedefs
      then hsapp <$> pure (H.ExpressionVariable $ elementReference namespaces tname) <*> encode term'
      else encode term'
    TermOptional m -> case m of
      Nothing -> pure $ hsvar "Nothing"
      Just t -> hsapp (hsvar "Just") <$> encode t
    TermProduct terms -> H.ExpressionTuple <$> (CM.mapM encode terms)
    TermRecord (Record sname fields) -> do
      if L.null fields -- TODO: too permissive; not all empty record types are the unit type
        then pure $ H.ExpressionTuple []
        else do
            let typeName = typeNameForRecord sname
            updates <- CM.mapM toFieldUpdate fields
            return $ H.ExpressionConstructRecord $ H.Expression_ConstructRecord (rawName typeName) updates
          where
            toFieldUpdate (Field fn ft) = H.FieldUpdate (recordFieldReference namespaces sname fn) <$> encode ft
    TermUnion (Injection sname (Field fn ft)) -> do
      let lhs = H.ExpressionVariable $ unionFieldReference namespaces sname fn
      case stripTerm ft of
        TermRecord (Record _ []) -> pure lhs
        _ -> hsapp lhs <$> encode ft
    TermVariable (Name v) -> pure $ hsvar v
    _ -> fail $ "unexpected term: " ++ show term
  where
    encode = encodeTerm namespaces

encodeType :: Show m => Namespaces -> Type m -> GraphFlow m H.Type
encodeType namespaces typ = case stripType typ of
    TypeApplication (ApplicationType lhs rhs) -> toTypeApplication <$>
      CM.sequence [encode lhs, encode rhs]
    TypeElement et -> encode et
    TypeFunction (FunctionType dom cod) -> H.TypeFunction <$> (H.Type_Function <$> encode dom <*> encode cod)
    TypeLambda (LambdaType (Name v) body) -> toTypeApplication <$> CM.sequence [
      encode body,
      pure $ H.TypeVariable $ simpleName v]
    TypeList lt -> H.TypeList <$> encode lt
    TypeLiteral lt -> H.TypeVariable . rawName <$> case lt of
      LiteralTypeBoolean -> pure "Bool"
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat32 -> pure "Float"
        FloatTypeFloat64 -> pure "Double"
        _ -> fail $ "unexpected floating-point type: " ++ show ft
      LiteralTypeInteger it -> case it of
        IntegerTypeBigint -> pure "Integer"
        IntegerTypeInt32 -> pure "Int"
        _ -> fail $ "unexpected integer type: " ++ show it
      LiteralTypeString -> pure "String"
      _ -> fail $ "unexpected literal type: " ++ show lt
    TypeMap (MapType kt vt) -> toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Map",
      encode kt,
      encode vt]
    TypeWrap name -> wrap name
    TypeOptional ot -> toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Maybe",
      encode ot]
    TypeProduct types -> H.TypeTuple <$> (CM.mapM encode types)
    TypeRecord rt -> case rowTypeFields rt of
      [] -> pure $ H.TypeTuple []  -- TODO: too permissive; not all empty record types are the unit type
      _ -> wrap $ rowTypeTypeName rt
    TypeSet st -> toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Set",
      encode st]
    TypeUnion rt -> wrap $ rowTypeTypeName rt
    TypeVariable (Name v) -> pure $ H.TypeVariable $ simpleName v
    _ -> fail $ "unexpected type: " ++ show typ
  where
    encode = encodeType namespaces
    wrap name = pure $ H.TypeVariable $ elementReference namespaces name

moduleToHaskellModule :: (Ord m, Read m, Show m) => Module m -> GraphFlow m H.Module
moduleToHaskellModule mod = transformModule haskellLanguage (encodeTerm namespaces) constructModule mod
  where
    namespaces = namespacesForModule mod

printModule :: (Ord m, Read m, Show m) => Module m -> GraphFlow m (M.Map FilePath String)
printModule mod = do
  hsmod <- moduleToHaskellModule mod
  let s = printExpr $ parenthesize $ toTree hsmod
  return $ M.fromList [(namespaceToFilePath True (FileExtension "hs") $ moduleNamespace mod, s)]

toDataDeclaration :: (Ord m, Read m, Show m)
  => M.Map (Type m) (Coder (Context m) (Context m) (Term m) H.Expression) -> Namespaces
  -> (Element m, TypedTerm m) -> GraphFlow m H.DeclarationWithComments
toDataDeclaration coders namespaces (el, TypedTerm typ term) = toDecl hname term coder Nothing
  where
    coder = Y.fromJust $ M.lookup typ coders
    hname = simpleName $ localNameOfEager $ elementName el

    rewriteValueBinding vb = case vb of
      H.ValueBindingSimple (H.ValueBinding_Simple (H.PatternApplication (H.Pattern_Application name args)) rhs bindings) -> case rhs of
        H.RightHandSide (H.ExpressionLambda (H.Expression_Lambda vars body)) -> rewriteValueBinding $
          H.ValueBindingSimple $ H.ValueBinding_Simple
            (H.PatternApplication (H.Pattern_Application name (args ++ vars))) (H.RightHandSide body) bindings
        _ -> vb

    toDecl hname term coder bindings = case stripTerm term of
      TermLet (Let bindings env) -> do
          -- A "let" constant cannot be predicted in advance, so we infer its type and construct a coder on the fly
          -- This makes program code with "let" terms more expensive to transform than simple data.
          let bl = M.toList bindings
          ts <- fmap typeSchemeType <$> (CM.mapM inferType (snd <$> bl))
          coders <- CM.mapM (constructCoder haskellLanguage (encodeTerm namespaces)) ts
          let hnames = simpleName <$> (unName . fst <$> bl)
          hterms <- CM.zipWithM coderEncode coders (snd <$> bl)

          let bindings = L.zipWith toBinding hnames hterms
          toDecl hname env coder (Just $ H.LocalBindings bindings)
        where
          toBinding hname' hterm' = H.LocalBindingValue $ simpleValueBinding hname' hterm' Nothing
      _ -> do
        hterm <- coderEncode coder term
        let vb = simpleValueBinding hname hterm bindings
        htype <- encodeType namespaces typ
        let decl = H.DeclarationTypedBinding $ H.TypedBinding
                    (H.TypeSignature hname htype)
                    (rewriteValueBinding vb)
        cx <- getState
        comments <- annotationClassTermDescription (contextAnnotations cx) term
        return $ H.DeclarationWithComments decl comments

toTypeDeclarations :: (Ord m, Read m, Show m)
  => Namespaces -> Element m -> Term m -> GraphFlow m [H.DeclarationWithComments]
toTypeDeclarations namespaces el term = do
    cx <- getState
    let lname = localNameOfEager $ elementName el
    let hname = simpleName lname
    t <- decodeType term
    isSer <- isSerializable
    let deriv = H.Deriving $ if isSer
                  then rawName <$> ["Eq", "Ord", "Read", "Show"]
                  else []
    let (vars, t') = unpackLambdaType cx t
    let hd = declHead hname $ L.reverse vars
    decl <- case stripType t' of
      TypeRecord rt -> do
        cons <- recordCons lname $ rowTypeFields rt
        return $ H.DeclarationData $ H.DataDeclaration H.DataDeclaration_KeywordData [] hd [cons] [deriv]
      TypeUnion rt -> do
        cons <- CM.mapM (unionCons lname) $ rowTypeFields rt
        return $ H.DeclarationData $ H.DataDeclaration H.DataDeclaration_KeywordData [] hd cons [deriv]
      _ -> if newtypesNotTypedefs
        then do
          cons <- newtypeCons el t'
          return $ H.DeclarationData $ H.DataDeclaration H.DataDeclaration_KeywordNewtype [] hd [cons] [deriv]
        else do
          htype <- encodeAdaptedType namespaces t
          return $ H.DeclarationType (H.TypeDeclaration hd htype)
    comments <- annotationClassTermDescription (contextAnnotations cx) term
    return $ [H.DeclarationWithComments decl comments] ++ constantDecls cx namespaces (elementName el) t
  where
    isSerializable = do
        deps <- typeDependencies (elementName el)
        let allVariants = S.fromList $ L.concat (variants <$> M.elems deps)
        return $ not $ S.member TypeVariantFunction allVariants
      where
        variants typ = typeVariant <$> foldOverType TraversalOrderPre (\m t -> t:m) [] typ

    declHead name vars = case vars of
      [] -> H.DeclarationHeadSimple name
      ((Name h):rest) -> H.DeclarationHeadApplication $
        H.DeclarationHead_Application (declHead name rest) (H.Variable $ simpleName h)

    newtypeCons el typ = do
        cx <- getState
        let hname = simpleName $ newtypeAccessorName $ elementName el
        htype <- encodeAdaptedType namespaces typ
        comments <- annotationClassTypeDescription (contextAnnotations cx) typ
        let hfield = H.FieldWithComments (H.Field hname htype) comments
        return $ H.ConstructorWithComments
          (H.ConstructorRecord $ H.Constructor_Record (simpleName $ localNameOfEager $ elementName el) [hfield]) Nothing

    recordCons lname fields = do
        hFields <- CM.mapM toField fields
        return $ H.ConstructorWithComments (H.ConstructorRecord $ H.Constructor_Record (simpleName lname) hFields) Nothing
      where
        toField (FieldType (FieldName fname) ftype) = do
          let hname = simpleName $ decapitalize lname ++ capitalize fname
          htype <- encodeAdaptedType namespaces ftype
          cx <- getState
          comments <- annotationClassTypeDescription (contextAnnotations cx) ftype
          return $ H.FieldWithComments (H.Field hname htype) comments

    unionCons lname (FieldType (FieldName fname) ftype) = do
      cx <- getState
      comments <- annotationClassTypeDescription (contextAnnotations cx) ftype
      let nm = capitalize lname ++ capitalize fname
      typeList <- if stripType ftype == Types.unit
        then pure []
        else do
          htype <- encodeAdaptedType namespaces ftype
          return [htype]
      return $ H.ConstructorWithComments (H.ConstructorOrdinary $ H.Constructor_Ordinary (simpleName nm) typeList) comments
