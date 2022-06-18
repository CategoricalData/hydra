module Hydra.Ext.Haskell.Coder (
  moduleToHaskellModule,
  moduleToHaskellString,
  haskellLanguage,
) where

import Hydra.Basics
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.Primitives
import Hydra.Rewriting
import Hydra.Util.Coders
import Hydra.Util.Formatting
import Hydra.Ext.Haskell.Language
import Hydra.Ext.Haskell.Utils
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Util.Codetree.Script
import Hydra.Ext.Haskell.Serde

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


newtypesNotTypedefs :: Bool
newtypesNotTypedefs = True

useCoreImport :: Bool
useCoreImport = True

moduleToHaskellModule :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified H.Module
moduleToHaskellModule cx g = graphToExternalModule haskellLanguage (encodeTerm namespaces) constructModule cx g
  where
    namespaces = namespacesForGraph g

moduleToHaskellString :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified String
moduleToHaskellString cx g = do
  hsmod <- moduleToHaskellModule cx g
  return $ printExpr $ parenthesize $ toTree hsmod

constantDecls :: Namespaces -> Name -> Type m -> [H.DeclarationWithComments]
constantDecls namespaces name@(Name nm) typ = if useCoreImport
    then toDecl (Name "hydra/core.Name") nameDecl:(toDecl (Name "hydra/core.FieldName") <$> fieldDecls)
    else []
  where
    lname = localNameOf name
    toDecl n (k, v) = H.DeclarationWithComments decl Nothing
      where
        decl = H.DeclarationValueBinding $
          H.ValueBindingSimple $ H.ValueBinding_Simple pat rhs Nothing
        pat = H.PatternApplication $ H.Pattern_Application (simpleName k) []
        rhs = H.RightHandSide $ H.ExpressionApplication $ H.Expression_Application
          (H.ExpressionVariable $ elementReference namespaces n)
          (H.ExpressionLiteral $ H.LiteralString v)
    nameDecl = ("_" ++ lname, nm)
    fieldsOf t = case typeExpr t of
      TypeExprRecord fields -> fields
      TypeExprUnion fields -> fields
      _ -> []
    fieldDecls = toConstant <$> fieldsOf (snd $ unpackUniversalType typ)
    toConstant (FieldType (FieldName fname) _) = ("_" ++ lname ++ "_" ++ fname, fname)

constructModule :: (Default m, Ord m, Read m, Show m)
  => Context m -> Graph m -> M.Map (Type m) (Step (Term m) H.Expression) -> [(Element m, TypedTerm m)] -> Result H.Module
constructModule cx g coders pairs = do
    decls <- L.concat <$> CM.mapM createDeclarations pairs
    return $ H.Module (Just $ H.ModuleHead (importName $ h $ graphName g) []) imports decls
  where
    h (GraphName name) = name

    createDeclarations pair@(el, TypedTerm typ term) = if isType typ
      then toTypeDeclarations namespaces cx el term
      else toDataDeclarations coders namespaces cx pair

    namespaces = namespacesForGraph g
    importName name = H.ModuleName $ L.intercalate "." (capitalize <$> Strings.splitOn "/" name)
    imports = domainImports ++ standardImports
      where
        domainImports = toImport <$> M.toList (namespacesMapping namespaces)
          where
            toImport (GraphName name, alias) = H.Import True (importName name) (Just alias) Nothing
        standardImports = toImport . H.ModuleName <$> Y.catMaybes [
            Just "Data.Map",
            Just "Data.Set"{-,
            if useCoreImport && graphName g /= GraphName "hydra/core"
              then Just "Hydra.Core"
              else Nothing-}]
          where
            toImport name = H.Import False name Nothing Nothing

toDataDeclarations :: (Ord m, Show m)
  => M.Map (Type m) (Step (Term m) H.Expression) -> Namespaces -> Context m
  -> (Element m, TypedTerm m) -> Result [H.DeclarationWithComments]
toDataDeclarations coders namespaces cx (el, TypedTerm typ term) = do
    let coder = Y.fromJust $ M.lookup typ coders
    rhs <- H.RightHandSide <$> stepOut coder term
    let hname = simpleName $ localNameOf $ elementName el
    let pat = H.PatternApplication $ H.Pattern_Application hname []
    htype <- encodeType namespaces typ
    let decl = H.DeclarationTypedBinding $ H.TypedBinding
                (H.TypeSignature hname htype)
                (H.ValueBindingSimple $ rewriteValueBinding $ H.ValueBinding_Simple pat rhs Nothing)
    comments <- contextDescriptionOf cx $ termMeta term
    return [H.DeclarationWithComments decl comments]
  where
    rewriteValueBinding vb = case vb of
      H.ValueBinding_Simple (H.PatternApplication (H.Pattern_Application name args)) rhs bindings -> case rhs of
        H.RightHandSide (H.ExpressionLambda (H.Expression_Lambda vars body)) -> rewriteValueBinding $
          H.ValueBinding_Simple
            (H.PatternApplication (H.Pattern_Application name (args ++ vars))) (H.RightHandSide body) bindings
        _ -> vb

toTypeDeclarations :: (Default m, Ord m, Read m, Show m)
  => Namespaces -> Context m -> Element m -> Term m -> Result [H.DeclarationWithComments]
toTypeDeclarations namespaces cx el term = do
    let lname = localNameOf $ elementName el
    let hname = simpleName lname
    t <- decodeType cx term
    isSer <- isSerializable
    let deriv = H.Deriving $ if isSer
                  then rawName <$> ["Eq", "Ord", "Read", "Show"]
                  else []
    let (vars, t') = unpackUniversalType t
    let hd = declHead hname $ L.reverse vars
    decl <- case typeExpr t' of
      TypeExprRecord fields -> do
        cons <- recordCons lname fields
        return $ H.DeclarationData (H.DataDeclaration H.DataDeclaration_KeywordData [] hd [cons] [deriv])
      TypeExprUnion fields -> do
        cons <- CM.mapM (unionCons lname) fields
        return $ H.DeclarationData (H.DataDeclaration H.DataDeclaration_KeywordData [] hd cons [deriv])
      _ -> if newtypesNotTypedefs
        then do
          cons <- newtypeCons el t'
          return $ H.DeclarationData (H.DataDeclaration H.DataDeclaration_KeywordNewtype [] hd [cons] [deriv])
        else do
          htype <- encodeAdaptedType namespaces cx t
          return $ H.DeclarationType (H.TypeDeclaration hd htype)
    comments <- contextDescriptionOf cx $ termMeta term
    return $ [H.DeclarationWithComments decl comments] ++ constantDecls namespaces (elementName el) t
  where
    isSerializable = do
        deps <- typeDependencies cx (elementName el)
        let allVariants = S.fromList $ L.concat (variants <$> M.elems deps)
        return $ not $ S.member TypeVariantFunction allVariants
      where
        variants typ = typeVariant <$> foldOverType TraversalOrderPre (\m t -> t:m) [] typ

    declHead name vars = case vars of
      [] -> H.DeclarationHeadSimple name
      ((TypeVariable h):rest) -> H.DeclarationHeadApplication $
        H.DeclarationHead_Application (declHead name rest) (H.Variable $ simpleName h)

    newtypeCons el typ = do
        let hname = simpleName $ newtypeAccessorName $ elementName el
        htype <- encodeAdaptedType namespaces cx typ
        let hfield = H.Field hname htype
        return $ H.ConstructorRecord $ H.Constructor_Record (simpleName $ localNameOf $ elementName el) [hfield]

    recordCons lname fields = do
        hFields <- CM.mapM toField fields
        return $ H.ConstructorRecord $ H.Constructor_Record (simpleName lname) hFields
      where
        toField (FieldType (FieldName fname) ftype) = do
          let hname = simpleName $ decapitalize lname ++ capitalize fname
          htype <- encodeAdaptedType namespaces cx ftype
          return $ H.Field hname htype

    unionCons lname (FieldType (FieldName fname) ftype) = do
      let nm = capitalize lname ++ capitalize fname
      typeList <- if ftype {typeMeta = dflt} == Types.unit
        then pure []
        else do
          htype <- encodeAdaptedType namespaces cx ftype
          return [htype]
      return $ H.ConstructorOrdinary $ H.Constructor_Ordinary (simpleName nm) typeList

encodeAdaptedType :: (Default m, Ord m, Read m, Show m) => Namespaces -> Context m -> Type m -> Result H.Type
encodeAdaptedType namespaces cx typ = adaptType cx haskellLanguage typ >>= encodeType namespaces

encodeFunction :: (Default m, Eq m, Ord m, Read m, Show m) => Namespaces -> Context m -> m -> Function m -> Result H.Expression
encodeFunction namespaces cx meta fun = case fun of
    FunctionElimination e -> case e of
      EliminationElement -> pure $ hsvar "id"
      EliminationNominal name -> pure $ H.ExpressionVariable $ elementReference namespaces $
        qname (graphNameOf name) $ newtypeAccessorName name
      EliminationOptional (OptionalCases nothing just) -> do
        nothingRhs <- H.CaseRhs <$> encodeTerm namespaces cx nothing
        let nothingAlt = H.Alternative (H.PatternName $ simpleName "Nothing") nothingRhs Nothing
        justAlt <- do
          -- Note: some of the following could be brought together with FunctionCases
          let v0 = "v"
          let rhsTerm = simplifyTerm $ apply just (variable v0)
          let v1 = if S.member (Variable v0) $ freeVariablesInTerm rhsTerm then v0 else "_"
          let lhs = H.PatternApplication $ H.Pattern_Application (rawName "Just") [H.PatternName $ rawName v1]
          rhs <- H.CaseRhs <$> encodeTerm namespaces cx rhsTerm
          return $ H.Alternative lhs rhs Nothing
        return $ H.ExpressionCase $ H.Expression_Case (hsvar "x") [nothingAlt, justAlt]
      EliminationRecord fname -> do
        dn <- domName
        case dn of
          Just n -> pure $ H.ExpressionVariable $ recordFieldReference namespaces n fname
          Nothing -> fail "unqualified record"
      EliminationUnion fields -> hslambda "x" <$> caseExpr -- note: could use a lambda case here
        where
          caseExpr = do
            fieldMap <- fieldMapOf <$> findDomain
            H.ExpressionCase <$> (H.Expression_Case (hsvar "x") <$> CM.mapM (toAlt fieldMap) fields)
          toAlt fieldMap (Field fn fun') = do
            let v0 = "v"
            let raw = apply fun' (variable v0)
            let rhsTerm = simplifyTerm raw
            let v1 = if isFreeIn (Variable v0) rhsTerm then "_" else v0
            dn <- domName
            hname <- case dn of
              Just n -> pure $ unionFieldReference namespaces n fn
              Nothing -> fail "unqualified field name"
            args <- case fieldMap >>= M.lookup fn of
              Just (FieldType _ (Type (TypeExprRecord []) _)) -> pure []
              Just _ -> pure [H.PatternName $ rawName v1]
              Nothing -> fail $ "field " ++ show fn ++ " not found in " ++ show dn
            let lhs = H.PatternApplication $ H.Pattern_Application hname args
            rhs <- H.CaseRhs <$> encodeTerm namespaces cx rhsTerm
            return $ H.Alternative lhs rhs Nothing
    FunctionLambda (Lambda (Variable v) body) -> hslambda v <$> encodeTerm namespaces cx body
    FunctionPrimitive name -> pure $ H.ExpressionVariable $ hsPrimitiveReference name
    _ -> fail $ "unexpected function: " ++ show fun
  where
    fieldMapOf typ = case typeExpr <$> typ of
      Just (TypeExprUnion tfields) -> Just $ M.fromList $ (\f -> (fieldTypeName f, f)) <$> tfields
      Just (TypeExprUniversal (UniversalType _ tbody)) -> fieldMapOf $ Just tbody
      _ -> Nothing
    findDomain = do
      dn <- domName
      case dn of
        Nothing -> pure Nothing
        Just name -> do
          scx <- schemaContext cx -- TODO: cache this
          typ <- requireType scx name
          return $ Just typ
    domName = do
        t <- contextTypeOf cx meta
        return $ case t of
          Just typ -> case typeExpr typ of
            TypeExprFunction (FunctionType dom _) -> nomName dom
            _ -> Nothing
          Nothing -> Nothing
      where
        nomName typ = case typeExpr typ of
          TypeExprNominal name -> Just name
          TypeExprUniversal (UniversalType _ body) -> nomName body
          _ -> Nothing

encodeLiteral :: Literal -> Result H.Expression
encodeLiteral av = case av of
    LiteralBoolean b -> pure $ hsvar $ case b of
      BooleanValueTrue -> "True"
      _ -> "False"
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

encodeTerm :: (Default m, Eq m, Ord m, Read m, Show m) => Namespaces -> Context m -> Term m -> Result H.Expression
encodeTerm namespaces cx term@(Term expr meta) = do
   case expr of
    TermExprApplication (Application fun arg) -> case termExpr fun of
       TermExprFunction (FunctionElimination EliminationElement) -> encode cx arg
       _ -> hsapp <$> encode cx fun <*> encode cx arg
    TermExprElement name -> pure $ H.ExpressionVariable $ elementReference namespaces name
    TermExprFunction f -> encodeFunction namespaces cx (termMeta term) f
    TermExprList els -> H.ExpressionList <$> CM.mapM (encode cx) els
    TermExprLiteral v -> encodeLiteral v
    TermExprNominal (Named tname term') -> if newtypesNotTypedefs
      then hsapp <$> pure (H.ExpressionVariable $ elementReference namespaces tname) <*> encode cx term'
      else encode cx term'
    TermExprOptional m -> case m of
      Nothing -> pure $ hsvar "Nothing"
      Just t -> hsapp (hsvar "Just") <$> encode cx t
    TermExprRecord fields -> do
      sname <- findSname
      case sname of
        Nothing -> case fields of
            [] -> pure $ H.ExpressionTuple []
            _ -> fail $ "unexpected anonymous record: " ++ show term
        Just name -> do
            let typeName = typeNameForRecord name
            updates <- CM.mapM toFieldUpdate fields
            return $ H.ExpressionConstructRecord $ H.Expression_ConstructRecord (rawName typeName) updates
          where
            toFieldUpdate (Field fn ft) = H.FieldUpdate (recordFieldReference namespaces name fn) <$> encode cx ft
    TermExprUnion (Field fn ft) -> do
      sname <- findSname
      lhs <- case sname of
        Just n -> pure $ H.ExpressionVariable $ unionFieldReference namespaces n fn
        Nothing -> fail "unqualified field"
      case termExpr ft of
        TermExprRecord [] -> pure lhs
        _ -> hsapp lhs <$> encode cx ft
    TermExprVariable (Variable v) -> pure $ hsvar v
    _ -> fail $ "unexpected term: " ++ show term
  where
    encode = encodeTerm namespaces
    findSname = do
      r <- contextTypeOf cx meta
      return $ case typeExpr <$> r of
        Just (TypeExprNominal name) -> Just name
        Nothing -> Nothing

encodeType :: Show m => Namespaces -> Type m -> Result H.Type
encodeType namespaces typ = case typeExpr typ of
    TypeExprElement et -> encode et
    TypeExprFunction (FunctionType dom cod) -> H.TypeFunction <$> (H.Type_Function <$> encode dom <*> encode cod)
    TypeExprList lt -> H.TypeList <$> encode lt
    TypeExprLiteral lt -> H.TypeVariable . rawName <$> case lt of
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
    TypeExprMap (MapType kt vt) -> toApplicationType <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Map",
      encode kt,
      encode vt]
    TypeExprNominal name -> pure $ H.TypeVariable $ elementReference namespaces name
    TypeExprOptional ot -> toApplicationType <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Maybe",
      encode ot]
    TypeExprSet st -> toApplicationType <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Set",
      encode st]
    TypeExprUniversal (UniversalType (TypeVariable v) body) -> toApplicationType <$> CM.sequence [
      encode body,
      pure $ H.TypeVariable $ simpleName v]
    TypeExprVariable (TypeVariable v) -> pure $ H.TypeVariable $ simpleName v
    TypeExprRecord [] -> pure $ H.TypeTuple []
    _ -> fail $ "unexpected type: " ++ show typ
  where
    encode = encodeType namespaces
