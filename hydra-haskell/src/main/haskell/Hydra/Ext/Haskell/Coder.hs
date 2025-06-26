module Hydra.Ext.Haskell.Coder (moduleToHaskell) where

import qualified Hydra.Adapters as Adapters
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode as Decode
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Ext.Haskell.Language as Language
import qualified Hydra.Ext.Haskell.Serde as Serde
import qualified Hydra.Ext.Haskell.Settings as Settings
import qualified Hydra.Ext.Haskell.Utils as Utils
import qualified Hydra.Flows as Flows
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import qualified Hydra.Qnames as Qnames
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Strip as Strip

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data HaskellGenerationOptions = HaskellGenerationOptions {
  haskellGenerationOptionsIncludeTypeDefinitions :: Bool
}

key_haskellVar = Core.Name "haskellVar"

-- TODO: make these settings configurable
defaultHaskellGenerationOptions = HaskellGenerationOptions False

adaptTypeToHaskellAndEncode :: Utils.HaskellNamespaces -> Core.Type -> Compute.Flow Graph.Graph H.Type
adaptTypeToHaskellAndEncode namespaces = Adapters.adaptAndEncodeType Language.haskellLanguage (encodeType namespaces)

constantForFieldName :: Core.Name -> Core.Name -> String
constantForFieldName tname fname = "_" ++ Qnames.localNameOf tname ++ "_" ++ Core.unName fname

constantForTypeName :: Core.Name -> String
constantForTypeName tname = "_" ++ Qnames.localNameOf tname

constructModule :: Utils.HaskellNamespaces
  -> Module.Module
  -> M.Map Core.Type (Compute.Coder Graph.Graph Graph.Graph Core.Term H.Expression)
  -> [(Graph.Element, Core.TypedTerm)] -> Compute.Flow Graph.Graph H.Module
constructModule namespaces mod coders pairs = do
    g <- Errors.getState
    decls <- L.concat <$> CM.mapM (createDeclarations g) pairs
    let mc = Module.moduleDescription mod
    return $ H.Module (Just $ H.ModuleHead mc (importName $ h $ Module.moduleNamespace mod) []) imports decls
  where
    h (Module.Namespace name) = name

    createDeclarations g pair@(el, tt@(Core.TypedTerm term typ)) = do
      if Annotations.isNativeType el
        then toTypeDeclarations namespaces el term
        else do
          d <- toDataDeclaration coders namespaces pair
          return [d]

    importName name = H.ModuleName $ L.intercalate "." (Formatting.capitalize <$> Strings.splitOn "." name)
    imports = domainImports ++ standardImports
      where
        domainImports = toImport <$> M.toList (Module.namespacesMapping namespaces)
          where
            toImport (Module.Namespace name, alias) = H.Import True (importName name) (Just alias) Nothing
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
                  else Just $ H.SpecImportHiding $ fmap (\n -> H.ImportExportSpec Nothing (Utils.simpleName n) Nothing) hidden

encodeFunction :: Utils.HaskellNamespaces -> Core.Function -> Compute.Flow Graph.Graph H.Expression
encodeFunction namespaces fun = case fun of
    Core.FunctionElimination e -> case e of
      Core.EliminationWrap name -> pure $ H.ExpressionVariable $ Utils.elementReference namespaces $
        Qnames.qname (Y.fromJust $ Qnames.namespaceOf name) $ Utils.newtypeAccessorName name
      Core.EliminationProduct (Core.TupleProjection arity idx _) -> if arity == 2
        then return $ Utils.hsvar $ if idx == 0 then "fst" else "snd"
        else fail "Eliminations for tuples of arity > 2 are not supported yet in the Haskell coder"
      Core.EliminationRecord (Core.Projection dn fname) -> return $ H.ExpressionVariable $ Utils.recordFieldReference namespaces dn fname
      Core.EliminationUnion (Core.CaseStatement dn def fields) -> Utils.hslambda (Utils.rawName "x") <$> caseExpr -- note: could use a lambda case here
        where
          caseExpr = do
            rt <- Lexical.withSchemaContext $ Schemas.requireUnionType dn
            let fieldMap = M.fromList $ (\f -> (Core.fieldTypeName f, f)) <$> Core.rowTypeFields rt
            ecases <- CM.mapM (toAlt fieldMap) fields
            dcases <- case def of
              Nothing -> pure []
              Just d -> do
                cs <- H.CaseRhs <$> encodeTerm namespaces d
                let lhs = H.PatternName $ Utils.rawName Constants.ignoredVariable
                return [H.Alternative lhs cs Nothing]
            return $ H.ExpressionCase $ H.CaseExpression (Utils.hsvar "x") $ ecases ++ dcases
          toAlt fieldMap (Core.Field fn fun') = Annotations.withDepth key_haskellVar $ \depth -> do
            let v0 = "v" ++ show depth
            let raw = Terms.apply fun' (Terms.var v0)
            let rhsTerm = Rewriting.simplifyTerm raw
            let v1 = if Rewriting.isFreeVariableInTerm (Core.Name v0) rhsTerm then Constants.ignoredVariable else v0
            let hname = Utils.unionFieldReference namespaces dn fn
            args <- case M.lookup fn fieldMap of
              Just (Core.FieldType _ ft) -> case Strip.stripType ft of
                Core.TypeRecord (Core.RowType _ []) -> pure []
                _ -> pure [H.PatternName $ Utils.rawName v1]
              Nothing -> fail $ "field " ++ show fn ++ " not found in " ++ show dn
            let lhs = Utils.applicationPattern hname args
            rhs <- H.CaseRhs <$> encodeTerm namespaces rhsTerm
            return $ H.Alternative lhs rhs Nothing
    Core.FunctionLambda (Core.Lambda v _ body) -> Utils.hslambda (Utils.elementReference namespaces v) <$> encodeTerm namespaces body
    Core.FunctionPrimitive name -> pure $ H.ExpressionVariable $ Utils.elementReference namespaces name

encodeLiteral :: Core.Literal -> Compute.Flow Graph.Graph H.Expression
encodeLiteral av = case av of
    Core.LiteralBoolean b -> pure $ Utils.hsvar $ if b then "True" else "False"
    Core.LiteralFloat fv -> case fv of
      Core.FloatValueFloat32 f -> pure $ Utils.hslit $ H.LiteralFloat f
      Core.FloatValueFloat64 f -> pure $ Utils.hslit $ H.LiteralDouble f
      -- TODO: remove this variant; the fact that it is appearing here is a bug
      Core.FloatValueBigfloat f -> pure $ Utils.hslit $ H.LiteralDouble f
--      _ -> unexpected "floating-point number" $ show fv
    Core.LiteralInteger iv -> case iv of
      Core.IntegerValueBigint i -> pure $ Utils.hslit $ H.LiteralInteger i
      Core.IntegerValueInt8 i -> pure $ Utils.hslit $ H.LiteralInteger $ fromIntegral i
      Core.IntegerValueInt16 i -> pure $ Utils.hslit $ H.LiteralInteger $ fromIntegral i
      Core.IntegerValueInt32 i -> pure $ Utils.hslit $ H.LiteralInt i
      Core.IntegerValueInt64 i -> pure $ Utils.hslit $ H.LiteralInteger $ fromIntegral i
      -- TODO: remove these variants; the fact that they are appearing here is a bug
      Core.IntegerValueUint8 i -> pure $ Utils.hslit $ H.LiteralInteger $ fromIntegral i
      Core.IntegerValueUint16 i -> pure $ Utils.hslit $ H.LiteralInteger $ fromIntegral i
      Core.IntegerValueUint32 i -> pure $ Utils.hslit $ H.LiteralInteger $ fromIntegral i
      Core.IntegerValueUint64 i -> pure $ Utils.hslit $ H.LiteralInteger $ fromIntegral i
--      _ -> unexpected "integer" $ show iv
    Core.LiteralString s -> pure $ Utils.hslit $ H.LiteralString s
    _ -> Errors.unexpected "literal value" $ show av

encodeTerm :: Utils.HaskellNamespaces -> Core.Term -> Compute.Flow Graph.Graph H.Expression
encodeTerm namespaces term = do
   case Strip.fullyStripTerm term of
    Core.TermApplication (Core.Application fun arg) -> Utils.hsapp <$> encode fun <*> encode arg
    Core.TermFunction f -> encodeFunction namespaces f
    Core.TermLet (Core.Let bindings env) -> do
        hbindings <- CM.mapM encodeBinding bindings
        hinner <- encode env
        return $ H.ExpressionLet $ H.LetExpression hbindings hinner
      where
        encodeBinding (Core.LetBinding name term _) = do
          let hname = Utils.simpleName $ Core.unName name
          hexpr <- encode term
          return $ H.LocalBindingValue $ Utils.simpleValueBinding hname hexpr Nothing
    Core.TermList els -> H.ExpressionList <$> CM.mapM encode els
    Core.TermLiteral v -> encodeLiteral v
    Core.TermMap m -> do
        let lhs = Utils.hsvar "M.fromList"
        rhs <- H.ExpressionList <$> CM.mapM encodePair (M.toList m)
        return $ Utils.hsapp lhs rhs
      where
        encodePair (k, v) = H.ExpressionTuple <$> sequence [encode k, encode v]
    Core.TermWrap (Core.WrappedTerm tname term') -> Utils.hsapp <$> pure (H.ExpressionVariable $ Utils.elementReference namespaces tname) <*> encode term'
    Core.TermOptional m -> case m of
      Nothing -> pure $ Utils.hsvar "Nothing"
      Just t -> Utils.hsapp (Utils.hsvar "Just") <$> encode t
    Core.TermProduct terms -> H.ExpressionTuple <$> (CM.mapM encode terms)
    Core.TermRecord (Core.Record sname fields) -> do
      if L.null fields -- TODO: too permissive; not all empty record types are the unit type
        then pure $ H.ExpressionTuple []
        else do
            let typeName = Utils.elementReference namespaces sname
            updates <- CM.mapM toFieldUpdate fields
            return $ H.ExpressionConstructRecord $ H.ConstructRecordExpression typeName updates
          where
            toFieldUpdate (Core.Field fn ft) = H.FieldUpdate (Utils.recordFieldReference namespaces sname fn) <$> encode ft
    Core.TermSet s -> do
      let lhs = Utils.hsvar "S.fromList"
      rhs <- encodeTerm namespaces $ Core.TermList $ S.toList s
      return $ Utils.hsapp lhs rhs
    Core.TermTypeAbstraction (Core.TypeAbstraction _ term1) -> encode term1
    Core.TermTypeApplication (Core.TypedTerm term1 _) -> encode term1
    Core.TermUnion (Core.Injection sname (Core.Field fn ft)) -> do
      let lhs = H.ExpressionVariable $ Utils.unionFieldReference namespaces sname fn
      case Strip.fullyStripTerm ft of
        Core.TermRecord (Core.Record _ []) -> pure lhs
        _ -> Utils.hsapp lhs <$> encode ft
    Core.TermVariable name -> pure $ H.ExpressionVariable $ Utils.elementReference namespaces name
    t -> fail $ "unexpected term: " ++ show t
  where
    encode = encodeTerm namespaces

encodeType :: Utils.HaskellNamespaces -> Core.Type -> Compute.Flow Graph.Graph H.Type
encodeType namespaces typ = Flows.withTrace "encode type" $ case Strip.stripType typ of
    Core.TypeApplication (Core.ApplicationType lhs rhs) -> Utils.toTypeApplication <$> CM.sequence [encode lhs, encode rhs]
    Core.TypeFunction (Core.FunctionType dom cod) -> H.TypeFunction <$> (H.FunctionType <$> encode dom <*> encode cod)
    Core.TypeForall (Core.ForallType (Core.Name v) body) -> encode body
    Core.TypeList lt -> H.TypeList <$> encode lt
    Core.TypeLiteral lt -> H.TypeVariable . Utils.rawName <$> case lt of
      Core.LiteralTypeBoolean -> pure "Bool"
      Core.LiteralTypeFloat ft -> case ft of
        Core.FloatTypeFloat32 -> pure "Float"
        Core.FloatTypeFloat64 -> pure "Double"
        Core.FloatTypeBigfloat -> pure "Double" -- TODO: type adapter should prevent this
--        _ -> fail $ "unexpected floating-point type: " ++ show ft
      Core.LiteralTypeInteger it -> case it of
        Core.IntegerTypeBigint -> pure "Integer"
        Core.IntegerTypeInt8 -> pure "I.Int8"
        Core.IntegerTypeInt16 -> pure "I.Int16"
        Core.IntegerTypeInt32 -> pure "Int"
        Core.IntegerTypeInt64 -> pure "I.Int64"
        _ -> fail $ "unexpected integer type: " ++ show it
      Core.LiteralTypeString -> pure "String"
      _ -> fail $ "unexpected literal type: " ++ show lt
    Core.TypeMap (Core.MapType kt vt) -> Utils.toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ Utils.rawName "M.Map",
      encode kt,
      encode vt]
    Core.TypeOptional ot -> Utils.toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ Utils.rawName "Maybe",
      encode ot]
    Core.TypeProduct types -> H.TypeTuple <$> (CM.mapM encode types)
    Core.TypeRecord rt -> case Core.rowTypeFields rt of
      [] -> if Core.rowTypeTypeName rt == Core._Unit
        then pure $ unitTuple
        else ref $ Core.rowTypeTypeName rt
      _ -> ref $ Core.rowTypeTypeName rt
    Core.TypeSet st -> Utils.toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ Utils.rawName "S.Set",
      encode st]
    Core.TypeUnion rt -> ref $ Core.rowTypeTypeName rt
    Core.TypeVariable v -> if v == Core._Unit
      then pure unitTuple
      else ref v
    Core.TypeWrap (Core.WrappedType name _) -> ref name
    _ -> fail $ "unexpected type: " ++ show typ
  where
    encode = encodeType namespaces
    ref name = pure $ H.TypeVariable $ Utils.elementReference namespaces name
    unitTuple = H.TypeTuple []

encodeTypeWithClassAssertions :: Utils.HaskellNamespaces -> M.Map Core.Name (S.Set Graph.TypeClass) -> Core.Type -> Compute.Flow Graph.Graph H.Type
encodeTypeWithClassAssertions namespaces explicitClasses typ = Flows.withTrace "encode with assertions" $ do
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
        hname = Utils.rawName $ case cls of
          Graph.TypeClassEquality -> "Eq"
          Graph.TypeClassOrdering -> "Ord"
        htype = H.TypeVariable $ Utils.rawName $ Core.unName name -- TODO: sanitization

    assertPairs = L.concat (toPairs <$> M.toList classes)
      where
        toPairs (name, cls) = toPair <$> S.toList cls
          where
            toPair c = (name, c)

findOrdVariables :: Core.Type -> S.Set Core.Name
findOrdVariables = Rewriting.foldOverType Coders.TraversalOrderPre fold S.empty
  where
    fold names typ = case typ of
      Core.TypeMap (Core.MapType kt _) -> tryType names kt
      Core.TypeSet et -> tryType names et
      _ -> names
    isTypeVariable v = Y.isNothing (Qnames.namespaceOf v) && L.head (Core.unName v) == 't'
    tryType names t = case Strip.stripType t of
      Core.TypeVariable v -> if isTypeVariable v
        then S.insert v names
        else names
      _ -> names

getImplicitTypeClasses :: Core.Type -> M.Map Core.Name (S.Set Graph.TypeClass)
getImplicitTypeClasses = M.fromList . fmap toPair . S.toList . findOrdVariables
  where
    toPair name = (name, S.fromList [Graph.TypeClassOrdering])

moduleToHaskellModule :: Module.Module -> Compute.Flow Graph.Graph H.Module
moduleToHaskellModule mod = do
    namespaces <- Utils.namespacesForModule mod
    Adapters.transformModule Language.haskellLanguage (encodeTerm namespaces) (constructModule namespaces) mod

moduleToHaskell :: Module.Module -> Compute.Flow Graph.Graph (M.Map String String)
moduleToHaskell mod = do
  hsmod <- moduleToHaskellModule mod
  let s = Serialization.printExpr $ Serialization.parenthesize $ Serde.moduleToExpr hsmod
  return $ M.fromList [(Qnames.namespaceToFilePath Mantle.CaseConventionPascal (Module.FileExtension "hs") $ Module.moduleNamespace mod, s)]

nameDecls :: Graph.Graph -> Utils.HaskellNamespaces -> Core.Name -> Core.Type -> [H.DeclarationWithComments]
nameDecls g namespaces name@(Core.Name nm) typ = if Settings.useCoreImport
    then (toDecl Core._Name nameDecl):(toDecl Core._Name <$> fieldDecls)
    else []
  where
    toDecl n (k, v) = H.DeclarationWithComments decl Nothing
      where
        decl = H.DeclarationValueBinding $ H.ValueBindingSimple $ H.SimpleValueBinding pat rhs Nothing
        pat = Utils.applicationPattern (Utils.simpleName k) []
        rhs = H.RightHandSide $ H.ExpressionApplication $ H.ApplicationExpression
          (H.ExpressionVariable $ Utils.elementReference namespaces n)
          (H.ExpressionLiteral $ H.LiteralString v)
    nameDecl = (constantForTypeName name, nm)
    fieldDecls = toConstant <$> Lexical.fieldsOf typ
    toConstant (Core.FieldType fname _) = (constantForFieldName name fname, Core.unName fname)

toDataDeclaration :: M.Map Core.Type (Compute.Coder Graph.Graph Graph.Graph Core.Term H.Expression) -> Utils.HaskellNamespaces
  -> (Graph.Element, Core.TypedTerm) -> Compute.Flow Graph.Graph H.DeclarationWithComments
toDataDeclaration coders namespaces (el, Core.TypedTerm term typ) = do
    comments <- Annotations.getTermDescription term
    toDecl comments hname term coder Nothing
  where
    coder = Y.fromJust $ M.lookup typ coders
    hname = Utils.simpleName $ Qnames.localNameOf $ Graph.elementName el

    rewriteValueBinding vb = case vb of
      H.ValueBindingSimple (H.SimpleValueBinding (H.PatternApplication (H.ApplicationPattern name args)) rhs bindings) -> case rhs of
        H.RightHandSide (H.ExpressionLambda (H.LambdaExpression vars body)) -> rewriteValueBinding $
          H.ValueBindingSimple $ H.SimpleValueBinding
            (Utils.applicationPattern name (args ++ vars)) (H.RightHandSide body) bindings
        _ -> vb

    toDecl comments hname term coder bindings = case Strip.fullyStripTerm term of
      Core.TermLet (Core.Let lbindings env) -> do
          -- A "let" constant cannot be predicted in advance, so we construct a coder on the fly.
          -- This makes program code with "let" terms more expensive to transform than simple data.
          let ts = Core.typeSchemeType . Y.fromJust . Core.letBindingType <$> lbindings
          coders <- CM.mapM (Adapters.constructCoder Language.haskellLanguage (encodeTerm namespaces)) ts
          let hnames = Utils.simpleName <$> (Core.unName . Core.letBindingName <$> lbindings)
          hterms <- CM.zipWithM Compute.coderEncode coders (Core.letBindingTerm <$> lbindings)

          let hbindings = L.zipWith toBinding hnames hterms
          toDecl comments hname env coder (Just $ H.LocalBindings hbindings)
        where
          toBinding hname' hterm' = H.LocalBindingValue $ Utils.simpleValueBinding hname' hterm' Nothing
      _ -> do
        hterm <- Compute.coderEncode coder term
        let vb = Utils.simpleValueBinding hname hterm bindings
        explicitClasses <- Annotations.getTypeClasses $ Rewriting.stripTypesFromTerm $ Graph.elementTerm el
        htype <- encodeTypeWithClassAssertions namespaces explicitClasses typ
        let decl = H.DeclarationTypedBinding $ H.TypedBinding (H.TypeSignature hname htype) (rewriteValueBinding vb)
        return $ H.DeclarationWithComments decl comments

toTypeDeclarations :: Utils.HaskellNamespaces -> Graph.Element -> Core.Term -> Compute.Flow Graph.Graph [H.DeclarationWithComments]
toTypeDeclarations namespaces el term = Flows.withTrace ("type element " ++ Core.unName (Graph.elementName el)) $ do
    g <- Errors.getState
    let lname = Qnames.localNameOf $ Graph.elementName el
    let hname = Utils.simpleName lname
    t <- DecodeCore.type_ term

    isSer <- Schemas.isSerializable el
    let deriv = H.Deriving $ if isSer
                  then Utils.rawName <$> ["Eq", "Ord", "Read", "Show"]
                  else []
    let (vars, t') = Utils.unpackForallType g t
    let hd = declHead hname $ L.reverse vars
    decl <- case Strip.stripType t' of
      Core.TypeRecord rt -> do
        cons <- recordCons lname $ Core.rowTypeFields rt
        return $ H.DeclarationData $ H.DataDeclaration H.DataOrNewtypeData [] hd [cons] [deriv]
      Core.TypeUnion rt -> do
        cons <- CM.mapM (unionCons g lname) $ Core.rowTypeFields rt
        return $ H.DeclarationData $ H.DataDeclaration H.DataOrNewtypeData [] hd cons [deriv]
      Core.TypeWrap (Core.WrappedType tname wt) -> do
        cons <- newtypeCons el wt
        return $ H.DeclarationData $ H.DataDeclaration H.DataOrNewtypeNewtype [] hd [cons] [deriv]
      _ -> do
        htype <- adaptTypeToHaskellAndEncode namespaces t
        return $ H.DeclarationType (H.TypeDeclaration hd htype)
    comments <- Annotations.getTermDescription term
    tdecls <- if haskellGenerationOptionsIncludeTypeDefinitions defaultHaskellGenerationOptions
       then do
         decl <- typeDecl namespaces (Graph.elementName el) t
         pure [decl]
       else pure []
    return $ [H.DeclarationWithComments decl comments]
      ++ nameDecls g namespaces (Graph.elementName el) t
      ++ tdecls
  where
    declHead name vars = case vars of
      [] -> H.DeclarationHeadSimple name
      ((Core.Name h):rest) -> H.DeclarationHeadApplication $
        H.ApplicationDeclarationHead (declHead name rest) (H.Variable $ Utils.simpleName h)

    newtypeCons el typ = do
        let hname = Utils.simpleName $ Utils.newtypeAccessorName $ Graph.elementName el
        htype <- adaptTypeToHaskellAndEncode namespaces typ
        let hfield = H.FieldWithComments (H.Field hname htype) Nothing
        return $ H.ConstructorWithComments
          (H.ConstructorRecord $ H.RecordConstructor (Utils.simpleName $ Qnames.localNameOf $ Graph.elementName el) [hfield]) Nothing

    recordCons lname fields = do
        hFields <- CM.mapM toField fields
        return $ H.ConstructorWithComments (H.ConstructorRecord $ H.RecordConstructor (Utils.simpleName lname) hFields) Nothing
      where
        toField (Core.FieldType (Core.Name fname) ftype) = do
          let hname = Utils.simpleName $ Formatting.decapitalize lname ++ Formatting.capitalize fname
          htype <- adaptTypeToHaskellAndEncode namespaces ftype
          comments <- Annotations.getTypeDescription ftype
          return $ H.FieldWithComments (H.Field hname htype) comments

    unionCons g lname (Core.FieldType (Core.Name fname) ftype) = do
        comments <- Annotations.getTypeDescription ftype
        let nm = deconflict $ Formatting.capitalize lname ++ Formatting.capitalize fname
        typeList <- if Strip.stripType ftype == Types.unit
          then pure []
          else do
            htype <- adaptTypeToHaskellAndEncode namespaces ftype
            return [htype]
        return $ H.ConstructorWithComments (H.ConstructorOrdinary $ H.OrdinaryConstructor (Utils.simpleName nm) typeList) comments
      where
        deconflict name = if Y.isJust (M.lookup tname $ Graph.graphElements g)
            then deconflict (name ++ "_")
            else name
          where
            tname = Qnames.unqualifyName $ Module.QualifiedName (Just $ fst $ Module.namespacesFocus namespaces) name

-- | Constructs a Hydra Type definition which can be included along with the nativized Haskell type definition
typeDecl :: Utils.HaskellNamespaces -> Core.Name -> Core.Type -> Compute.Flow Graph.Graph H.DeclarationWithComments
typeDecl namespaces name typ = do
    -- Note: consider constructing this coder just once, then reusing it
    coder <- Adapters.constructCoder Language.haskellLanguage (encodeTerm namespaces) $ Core.TypeVariable Core._Type
    expr <- Compute.coderEncode coder finalTerm
    let rhs = H.RightHandSide expr
    let hname = Utils.simpleName $ typeNameLocal name
    let pat = Utils.applicationPattern hname []
    let decl = H.DeclarationValueBinding $ H.ValueBindingSimple $ H.SimpleValueBinding pat rhs Nothing
    return $ H.DeclarationWithComments decl Nothing
  where
    typeName ns name = Qnames.qname ns (typeNameLocal name)
    typeNameLocal name = "_" ++ Qnames.localNameOf name ++ "_type_"
    rawTerm = EncodeCore.type_ typ
    finalTerm = Rewriting.rewriteTerm rewrite rawTerm
      where
        rewrite :: (Core.Term -> Core.Term) -> Core.Term -> Core.Term
        rewrite recurse term = Y.fromMaybe (recurse term) (Decode.variant Core._Type term >>= forType)
          where
            forType (Core.Field fname fterm) = if fname == Core._Type_record
              then Nothing -- TODO
              else if fname == Core._Type_variable
              then Decode.name fterm >>= forVariableType
              else Nothing
            forVariableType name = (\ns -> Core.TermVariable $ Qnames.qname ns $ "_" ++ local ++ "_type_") <$> mns
              where
                (Module.QualifiedName mns local) = Qnames.qualifyName name
