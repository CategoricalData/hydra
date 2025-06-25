module Hydra.Ext.Haskell.Coder (moduleToHaskell) where

import Hydra.Kernel
import Hydra.Adapters
import Hydra.Ext.Haskell.Language
import Hydra.Ext.Haskell.Utils
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import Hydra.Dsl.Terms
import Hydra.Ext.Haskell.Serde
import Hydra.Ext.Haskell.Settings
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Decode as Decode

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data HaskellGenerationOptions = HaskellGenerationOptions {
  haskellGenerationOptionsIncludeTypeDefinitions :: Bool
}

key_haskellVar = Name "haskellVar"

-- TODO: make these settings configurable
defaultHaskellGenerationOptions = HaskellGenerationOptions False

adaptTypeToHaskellAndEncode :: HaskellNamespaces -> Type -> Flow Graph H.Type
adaptTypeToHaskellAndEncode namespaces = adaptAndEncodeType haskellLanguage (encodeType namespaces)

constantForFieldName :: Name -> Name -> String
constantForFieldName tname fname = "_" ++ localNameOf tname ++ "_" ++ unName fname

constantForTypeName :: Name -> String
constantForTypeName tname = "_" ++ localNameOf tname

constructModule :: HaskellNamespaces
  -> Module
  -> M.Map Type (Coder Graph Graph Term H.Expression)
  -> [(Element, TypedTerm)] -> Flow Graph H.Module
constructModule namespaces mod coders pairs = do
    g <- getState
    decls <- L.concat <$> CM.mapM (createDeclarations g) pairs
    let mc = moduleDescription mod
    return $ H.Module (Just $ H.ModuleHead mc (importName $ h $ moduleNamespace mod) []) imports decls
  where
    h (Namespace name) = name

    createDeclarations g pair@(el, tt@(TypedTerm term typ)) = do
      if isNativeType el
        then toTypeDeclarations namespaces el term
        else do
          d <- toDataDeclaration coders namespaces pair
          return [d]

    importName name = H.ModuleName $ L.intercalate "." (capitalize <$> Strings.splitOn "." name)
    imports = domainImports ++ standardImports
      where
        domainImports = toImport <$> M.toList (namespacesMapping namespaces)
          where
            toImport (Namespace name, alias) = H.Import True (importName name) (Just alias) Nothing
        standardImports = toImport <$> [
            ("Prelude", Nothing, ["Enum", "Ordering", "map", "pure", "sum"]),
            ("Data.Int", Just "I", []),
            ("Data.List", Just "L", []),
            ("Data.Map", Just "M", []),
            ("Data.Set", Just "S", [])]
          where
            toImport (name, malias, hidden) = H.Import (Y.isJust malias) (H.ModuleName name) (fmap H.ModuleName malias) spec
              where
                spec = if L.null hidden
                  then Nothing
                  else Just $ H.SpecImportHiding $ fmap (\n -> H.ImportExportSpec Nothing (simpleName n) Nothing) hidden

encodeFunction :: HaskellNamespaces -> Function -> Flow Graph H.Expression
encodeFunction namespaces fun = case fun of
    FunctionElimination e -> case e of
      EliminationWrap name -> pure $ H.ExpressionVariable $ elementReference namespaces $
        qname (Y.fromJust $ namespaceOf name) $ newtypeAccessorName name
      EliminationProduct (TupleProjection arity idx _) -> if arity == 2
        then return $ hsvar $ if idx == 0 then "fst" else "snd"
        else fail "Eliminations for tuples of arity > 2 are not supported yet in the Haskell coder"
      EliminationRecord (Projection dn fname) -> return $ H.ExpressionVariable $ recordFieldReference namespaces dn fname
      EliminationUnion (CaseStatement dn def fields) -> hslambda (rawName "x") <$> caseExpr -- note: could use a lambda case here
        where
          caseExpr = do
            rt <- withSchemaContext $ requireUnionType dn
            let fieldMap = M.fromList $ (\f -> (fieldTypeName f, f)) <$> rowTypeFields rt
            ecases <- CM.mapM (toAlt fieldMap) fields
            dcases <- case def of
              Nothing -> pure []
              Just d -> do
                cs <- H.CaseRhs <$> encodeTerm namespaces d
                let lhs = H.PatternName $ rawName ignoredVariable
                return [H.Alternative lhs cs Nothing]
            return $ H.ExpressionCase $ H.CaseExpression (hsvar "x") $ ecases ++ dcases
          toAlt fieldMap (Field fn fun') = withDepth key_haskellVar $ \depth -> do
            let v0 = "v" ++ show depth
            let raw = apply fun' (var v0)
            let rhsTerm = simplifyTerm raw
            let v1 = if isFreeVariableInTerm (Name v0) rhsTerm then ignoredVariable else v0
            let hname = unionFieldReference namespaces dn fn
            args <- case M.lookup fn fieldMap of
              Just (FieldType _ ft) -> case stripType ft of
                TypeRecord (RowType _ []) -> pure []
                _ -> pure [H.PatternName $ rawName v1]
              Nothing -> fail $ "field " ++ show fn ++ " not found in " ++ show dn
            let lhs = applicationPattern hname args
            rhs <- H.CaseRhs <$> encodeTerm namespaces rhsTerm
            return $ H.Alternative lhs rhs Nothing
    FunctionLambda (Lambda v _ body) -> hslambda (elementReference namespaces v) <$> encodeTerm namespaces body
    FunctionPrimitive name -> pure $ H.ExpressionVariable $ elementReference namespaces name

encodeLiteral :: Literal -> Flow Graph H.Expression
encodeLiteral av = case av of
    LiteralBoolean b -> pure $ hsvar $ if b then "True" else "False"
    LiteralFloat fv -> case fv of
      FloatValueFloat32 f -> pure $ hslit $ H.LiteralFloat f
      FloatValueFloat64 f -> pure $ hslit $ H.LiteralDouble f
      -- TODO: remove this variant; the fact that it is appearing here is a bug
      FloatValueBigfloat f -> pure $ hslit $ H.LiteralDouble f
--      _ -> unexpected "floating-point number" $ show fv
    LiteralInteger iv -> case iv of
      IntegerValueBigint i -> pure $ hslit $ H.LiteralInteger i
      IntegerValueInt8 i -> pure $ hslit $ H.LiteralInteger $ fromIntegral i
      IntegerValueInt16 i -> pure $ hslit $ H.LiteralInteger $ fromIntegral i
      IntegerValueInt32 i -> pure $ hslit $ H.LiteralInt i
      IntegerValueInt64 i -> pure $ hslit $ H.LiteralInteger $ fromIntegral i
      -- TODO: remove these variants; the fact that they are appearing here is a bug
      IntegerValueUint8 i -> pure $ hslit $ H.LiteralInteger $ fromIntegral i
      IntegerValueUint16 i -> pure $ hslit $ H.LiteralInteger $ fromIntegral i
      IntegerValueUint32 i -> pure $ hslit $ H.LiteralInteger $ fromIntegral i
      IntegerValueUint64 i -> pure $ hslit $ H.LiteralInteger $ fromIntegral i
--      _ -> unexpected "integer" $ show iv
    LiteralString s -> pure $ hslit $ H.LiteralString s
    _ -> unexpected "literal value" $ show av

encodeTerm :: HaskellNamespaces -> Term -> Flow Graph H.Expression
encodeTerm namespaces term = do
   case fullyStripTerm term of
    TermApplication (Application fun arg) -> hsapp <$> encode fun <*> encode arg
    TermFunction f -> encodeFunction namespaces f
    TermLet (Let bindings env) -> do
        hbindings <- CM.mapM encodeBinding bindings
        hinner <- encode env
        return $ H.ExpressionLet $ H.LetExpression hbindings hinner
      where
        encodeBinding (LetBinding name term _) = do
          let hname = simpleName $ unName name
          hexpr <- encode term
          return $ H.LocalBindingValue $ simpleValueBinding hname hexpr Nothing
    TermList els -> H.ExpressionList <$> CM.mapM encode els
    TermLiteral v -> encodeLiteral v
    TermMap m -> do
        let lhs = hsvar "M.fromList"
        rhs <- H.ExpressionList <$> CM.mapM encodePair (M.toList m)
        return $ hsapp lhs rhs
      where
        encodePair (k, v) = H.ExpressionTuple <$> sequence [encode k, encode v]
    TermWrap (WrappedTerm tname term') -> hsapp <$> pure (H.ExpressionVariable $ elementReference namespaces tname) <*> encode term'
    TermOptional m -> case m of
      Nothing -> pure $ hsvar "Nothing"
      Just t -> hsapp (hsvar "Just") <$> encode t
    TermProduct terms -> H.ExpressionTuple <$> (CM.mapM encode terms)
    TermRecord (Record sname fields) -> do
      if L.null fields -- TODO: too permissive; not all empty record types are the unit type
        then pure $ H.ExpressionTuple []
        else do
            let typeName = elementReference namespaces sname
            updates <- CM.mapM toFieldUpdate fields
            return $ H.ExpressionConstructRecord $ H.ConstructRecordExpression typeName updates
          where
            toFieldUpdate (Field fn ft) = H.FieldUpdate (recordFieldReference namespaces sname fn) <$> encode ft
    TermSet s -> do
      let lhs = hsvar "S.fromList"
      rhs <- encodeTerm namespaces $ TermList $ S.toList s
      return $ hsapp lhs rhs
    TermTypeAbstraction (TypeAbstraction _ term1) -> encode term1
    TermTypeApplication (TypedTerm term1 _) -> encode term1
    TermUnion (Injection sname (Field fn ft)) -> do
      let lhs = H.ExpressionVariable $ unionFieldReference namespaces sname fn
      case fullyStripTerm ft of
        TermRecord (Record _ []) -> pure lhs
        _ -> hsapp lhs <$> encode ft
    TermVariable name -> pure $ H.ExpressionVariable $ elementReference namespaces name
    t -> fail $ "unexpected term: " ++ show t
  where
    encode = encodeTerm namespaces

encodeType :: HaskellNamespaces -> Type -> Flow Graph H.Type
encodeType namespaces typ = withTrace "encode type" $ case stripType typ of
    TypeApplication (ApplicationType lhs rhs) -> toTypeApplication <$> CM.sequence [encode lhs, encode rhs]
    TypeFunction (FunctionType dom cod) -> H.TypeFunction <$> (H.FunctionType <$> encode dom <*> encode cod)
    TypeForall (ForallType (Name v) body) -> encode body
    TypeList lt -> H.TypeList <$> encode lt
    TypeLiteral lt -> H.TypeVariable . rawName <$> case lt of
      LiteralTypeBoolean -> pure "Bool"
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat32 -> pure "Float"
        FloatTypeFloat64 -> pure "Double"
        FloatTypeBigfloat -> pure "Double" -- TODO: type adapter should prevent this
--        _ -> fail $ "unexpected floating-point type: " ++ show ft
      LiteralTypeInteger it -> case it of
        IntegerTypeBigint -> pure "Integer"
        IntegerTypeInt8 -> pure "I.Int8"
        IntegerTypeInt16 -> pure "I.Int16"
        IntegerTypeInt32 -> pure "Int"
        IntegerTypeInt64 -> pure "I.Int64"
        _ -> fail $ "unexpected integer type: " ++ show it
      LiteralTypeString -> pure "String"
      _ -> fail $ "unexpected literal type: " ++ show lt
    TypeMap (MapType kt vt) -> toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "M.Map",
      encode kt,
      encode vt]
    TypeOptional ot -> toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Maybe",
      encode ot]
    TypeProduct types -> H.TypeTuple <$> (CM.mapM encode types)
    TypeRecord rt -> case rowTypeFields rt of
      [] -> if rowTypeTypeName rt == _Unit
        then pure $ unitTuple
        else ref $ rowTypeTypeName rt
      _ -> ref $ rowTypeTypeName rt
    TypeSet st -> toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "S.Set",
      encode st]
    TypeUnion rt -> ref $ rowTypeTypeName rt
    TypeVariable v -> if v == _Unit
      then pure unitTuple
      else ref v
    TypeWrap (WrappedType name _) -> ref name
    _ -> fail $ "unexpected type: " ++ show typ
  where
    encode = encodeType namespaces
    ref name = pure $ H.TypeVariable $ elementReference namespaces name
    unitTuple = H.TypeTuple []

encodeTypeWithClassAssertions :: HaskellNamespaces -> M.Map Name (S.Set TypeClass) -> Type -> Flow Graph H.Type
encodeTypeWithClassAssertions namespaces explicitClasses typ = withTrace "encode with assertions" $ do
    htyp <- adaptTypeToHaskellAndEncode namespaces typ
    if L.null assertPairs
      then pure htyp
      else do
        let encoded = encodeAssertion <$> assertPairs
        let hassert = if L.length encoded > 1 then L.head encoded else H.AssertionTuple encoded
        return $ H.TypeCtx $ H.ContextType hassert htyp
  where
    classes = M.unionWith S.union explicitClasses implicitClasses
    implicitClasses = getImplicitTypeClasses typ
    encodeAssertion (name, cls) = H.AssertionClass $ H.ClassAssertion hname [htype]
      where
        hname = rawName $ case cls of
          TypeClassEquality -> "Eq"
          TypeClassOrdering -> "Ord"
        htype = H.TypeVariable $ rawName $ unName name -- TODO: sanitization

    assertPairs = L.concat (toPairs <$> M.toList classes)
      where
        toPairs (name, cls) = toPair <$> S.toList cls
          where
            toPair c = (name, c)

findOrdVariables :: Type -> S.Set Name
findOrdVariables = foldOverType TraversalOrderPre fold S.empty
  where
    fold names typ = case typ of
      TypeMap (MapType kt _) -> tryType names kt
      TypeSet et -> tryType names et
      _ -> names
    isTypeVariable v = Y.isNothing (namespaceOf v) && L.head (unName v) == 't'
    tryType names t = case stripType t of
      TypeVariable v -> if isTypeVariable v
        then S.insert v names
        else names
      _ -> names

getImplicitTypeClasses :: Type -> M.Map Name (S.Set TypeClass)
getImplicitTypeClasses = M.fromList . fmap toPair . S.toList . findOrdVariables
  where
    toPair name = (name, S.fromList [TypeClassOrdering])

moduleToHaskellModule :: Module -> Flow Graph H.Module
moduleToHaskellModule mod = do
    namespaces <- namespacesForModule mod
    transformModule haskellLanguage (encodeTerm namespaces) (constructModule namespaces) mod

moduleToHaskell :: Module -> Flow Graph (M.Map FilePath String)
moduleToHaskell mod = do
  hsmod <- moduleToHaskellModule mod
  let s = printExpr $ parenthesize $ toTree hsmod
  return $ M.fromList [(namespaceToFilePath CaseConventionPascal (FileExtension "hs") $ moduleNamespace mod, s)]

nameDecls :: Graph -> HaskellNamespaces -> Name -> Type -> [H.DeclarationWithComments]
nameDecls g namespaces name@(Name nm) typ = if useCoreImport
    then (toDecl _Name nameDecl):(toDecl _Name <$> fieldDecls)
    else []
  where
    toDecl n (k, v) = H.DeclarationWithComments decl Nothing
      where
        decl = H.DeclarationValueBinding $ H.ValueBindingSimple $ H.SimpleValueBinding pat rhs Nothing
        pat = applicationPattern (simpleName k) []
        rhs = H.RightHandSide $ H.ExpressionApplication $ H.ApplicationExpression
          (H.ExpressionVariable $ elementReference namespaces n)
          (H.ExpressionLiteral $ H.LiteralString v)
    nameDecl = (constantForTypeName name, nm)
    fieldDecls = toConstant <$> fieldsOf typ
    toConstant (FieldType fname _) = (constantForFieldName name fname, unName fname)

toDataDeclaration :: M.Map Type (Coder Graph Graph Term H.Expression) -> HaskellNamespaces
  -> (Element, TypedTerm) -> Flow Graph H.DeclarationWithComments
toDataDeclaration coders namespaces (el, TypedTerm term typ) = do
    comments <- getTermDescription term
    toDecl comments hname term coder Nothing
  where
    coder = Y.fromJust $ M.lookup typ coders
    hname = simpleName $ localNameOf $ elementName el

    rewriteValueBinding vb = case vb of
      H.ValueBindingSimple (H.SimpleValueBinding (H.PatternApplication (H.ApplicationPattern name args)) rhs bindings) -> case rhs of
        H.RightHandSide (H.ExpressionLambda (H.LambdaExpression vars body)) -> rewriteValueBinding $
          H.ValueBindingSimple $ H.SimpleValueBinding
            (applicationPattern name (args ++ vars)) (H.RightHandSide body) bindings
        _ -> vb

    toDecl comments hname term coder bindings = case fullyStripTerm term of
      TermLet (Let lbindings env) -> do
          -- A "let" constant cannot be predicted in advance, so we construct a coder on the fly.
          -- This makes program code with "let" terms more expensive to transform than simple data.
          let ts = typeSchemeType . Y.fromJust . letBindingType <$> lbindings
          coders <- CM.mapM (constructCoder haskellLanguage (encodeTerm namespaces)) ts
          let hnames = simpleName <$> (unName . letBindingName <$> lbindings)
          hterms <- CM.zipWithM coderEncode coders (letBindingTerm <$> lbindings)

          let hbindings = L.zipWith toBinding hnames hterms
          toDecl comments hname env coder (Just $ H.LocalBindings hbindings)
        where
          toBinding hname' hterm' = H.LocalBindingValue $ simpleValueBinding hname' hterm' Nothing
      _ -> do
        hterm <- coderEncode coder term
        let vb = simpleValueBinding hname hterm bindings
        explicitClasses <- getTypeClasses $ stripTypesFromTerm $ elementTerm el
        htype <- encodeTypeWithClassAssertions namespaces explicitClasses typ
        let decl = H.DeclarationTypedBinding $ H.TypedBinding (H.TypeSignature hname htype) (rewriteValueBinding vb)
        return $ H.DeclarationWithComments decl comments

toTypeDeclarations :: HaskellNamespaces -> Element -> Term -> Flow Graph [H.DeclarationWithComments]
toTypeDeclarations namespaces el term = withTrace ("type element " ++ unName (elementName el)) $ do
    g <- getState
    let lname = localNameOf $ elementName el
    let hname = simpleName lname
    t <- DecodeCore.type_ term

    isSer <- isSerializable el
    let deriv = H.Deriving $ if isSer
                  then rawName <$> ["Eq", "Ord", "Read", "Show"]
                  else []
    let (vars, t') = unpackForallType g t
    let hd = declHead hname $ L.reverse vars
    decl <- case stripType t' of
      TypeRecord rt -> do
        cons <- recordCons lname $ rowTypeFields rt
        return $ H.DeclarationData $ H.DataDeclaration H.DataOrNewtypeData [] hd [cons] [deriv]
      TypeUnion rt -> do
        cons <- CM.mapM (unionCons g lname) $ rowTypeFields rt
        return $ H.DeclarationData $ H.DataDeclaration H.DataOrNewtypeData [] hd cons [deriv]
      TypeWrap (WrappedType tname wt) -> do
        cons <- newtypeCons el wt
        return $ H.DeclarationData $ H.DataDeclaration H.DataOrNewtypeNewtype [] hd [cons] [deriv]
      _ -> do
        htype <- adaptTypeToHaskellAndEncode namespaces t
        return $ H.DeclarationType (H.TypeDeclaration hd htype)
    comments <- getTermDescription term
    tdecls <- if haskellGenerationOptionsIncludeTypeDefinitions defaultHaskellGenerationOptions
       then do
         decl <- typeDecl namespaces (elementName el) t
         pure [decl]
       else pure []
    return $ [H.DeclarationWithComments decl comments]
      ++ nameDecls g namespaces (elementName el) t
      ++ tdecls
  where
    declHead name vars = case vars of
      [] -> H.DeclarationHeadSimple name
      ((Name h):rest) -> H.DeclarationHeadApplication $
        H.ApplicationDeclarationHead (declHead name rest) (H.Variable $ simpleName h)

    newtypeCons el typ = do
        let hname = simpleName $ newtypeAccessorName $ elementName el
        htype <- adaptTypeToHaskellAndEncode namespaces typ
        let hfield = H.FieldWithComments (H.Field hname htype) Nothing
        return $ H.ConstructorWithComments
          (H.ConstructorRecord $ H.RecordConstructor (simpleName $ localNameOf $ elementName el) [hfield]) Nothing

    recordCons lname fields = do
        hFields <- CM.mapM toField fields
        return $ H.ConstructorWithComments (H.ConstructorRecord $ H.RecordConstructor (simpleName lname) hFields) Nothing
      where
        toField (FieldType (Name fname) ftype) = do
          let hname = simpleName $ decapitalize lname ++ capitalize fname
          htype <- adaptTypeToHaskellAndEncode namespaces ftype
          comments <- getTypeDescription ftype
          return $ H.FieldWithComments (H.Field hname htype) comments

    unionCons g lname (FieldType (Name fname) ftype) = do
        comments <- getTypeDescription ftype
        let nm = deconflict $ capitalize lname ++ capitalize fname
        typeList <- if stripType ftype == Types.unit
          then pure []
          else do
            htype <- adaptTypeToHaskellAndEncode namespaces ftype
            return [htype]
        return $ H.ConstructorWithComments (H.ConstructorOrdinary $ H.OrdinaryConstructor (simpleName nm) typeList) comments
      where
        deconflict name = if Y.isJust (M.lookup tname $ graphElements g)
            then deconflict (name ++ "_")
            else name
          where
            tname = unqualifyName $ QualifiedName (Just $ fst $ namespacesFocus namespaces) name

-- | Constructs a Hydra Type definition which can be included along with the nativized Haskell type definition
typeDecl :: HaskellNamespaces -> Name -> Type -> Flow Graph H.DeclarationWithComments
typeDecl namespaces name typ = do
    -- Note: consider constructing this coder just once, then reusing it
    coder <- constructCoder haskellLanguage (encodeTerm namespaces) typeT
    expr <- coderEncode coder finalTerm
    let rhs = H.RightHandSide expr
    let hname = simpleName $ typeNameLocal name
    let pat = applicationPattern hname []
    let decl = H.DeclarationValueBinding $ H.ValueBindingSimple $ H.SimpleValueBinding pat rhs Nothing
    return $ H.DeclarationWithComments decl Nothing
  where
    typeName ns name = qname ns (typeNameLocal name)
    typeNameLocal name = "_" ++ localNameOf name ++ "_type_"
    rawTerm = EncodeCore.type_ typ
    finalTerm = rewriteTerm rewrite rawTerm
      where
        rewrite :: (Term -> Term) -> Term -> Term
        rewrite recurse term = Y.fromMaybe (recurse term) (Decode.variant _Type term >>= forType)
          where
            forType (Field fname fterm) = if fname == _Type_record
              then Nothing -- TODO
              else if fname == _Type_variable
              then Decode.name fterm >>= forVariableType
              else Nothing
            forVariableType name = (\ns -> TermVariable $ qname ns $ "_" ++ local ++ "_type_") <$> mns
              where
                (QualifiedName mns local) = qualifyName name
