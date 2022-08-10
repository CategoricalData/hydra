module Hydra.Ext.Haskell.Coder (printGraph) where

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
import Hydra.Ext.Haskell.Settings

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
    fieldsOf t = case typeExpr cx t of
      TypeRecord rt -> rowTypeFields rt
      TypeUnion rt -> rowTypeFields rt
      _ -> []
    fieldDecls = toConstant <$> fieldsOf (snd $ unpackLambdaType cx typ)
    toConstant (FieldType (FieldName fname) _) = ("_" ++ lname ++ "_" ++ fname, fname)

constructModule :: (Ord m, Read m, Show m)
  => Context m -> Graph m -> M.Map (Type m) (Coder (Term m) H.Expression) -> [(Element m, TypedTerm m)] -> Result H.Module
constructModule cx g coders pairs = do
    decls <- L.concat <$> CM.mapM createDeclarations pairs
    return $ H.Module (Just $ H.ModuleHead (importName $ h $ graphName g) []) imports decls
  where
    h (GraphName name) = name

    createDeclarations pair@(el, TypedTerm typ term) = if isType cx typ
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

encodeAdaptedType :: (Ord m, Read m, Show m) => Namespaces -> Context m -> Type m -> Result H.Type
encodeAdaptedType namespaces cx typ = adaptType cx language typ >>= encodeType cx namespaces

encodeFunction :: (Eq m, Ord m, Read m, Show m) => Namespaces -> Context m -> Function m -> Result H.Expression
encodeFunction namespaces cx fun = case fun of
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
      EliminationRecord (Projection dn fname) -> return $ H.ExpressionVariable $ recordFieldReference namespaces dn fname
      EliminationUnion (CaseStatement dn fields) -> hslambda "x" <$> caseExpr -- note: could use a lambda case here
        where
          caseExpr = do
            scx <- schemaContext cx -- TODO: cache this
            rt <- requireUnionType scx dn
            let fieldMap = M.fromList $ (\f -> (fieldTypeName f, f)) <$> rowTypeFields rt
            H.ExpressionCase <$> (H.Expression_Case (hsvar "x") <$> CM.mapM (toAlt fieldMap) fields)
          toAlt fieldMap (Field fn fun') = do
            let v0 = "v"
            let raw = apply fun' (variable v0)
            let rhsTerm = simplifyTerm raw
            let v1 = if isFreeIn (Variable v0) rhsTerm then "_" else v0
            let hname = unionFieldReference namespaces dn fn
            args <- case M.lookup fn fieldMap of
              Just (FieldType _ ft) -> case typeExpr cx ft of
                TypeRecord (RowType _ []) -> pure []
                _ -> pure [H.PatternName $ rawName v1]
              Nothing -> failWithTrace cx $ "field " ++ show fn ++ " not found in " ++ show dn
            let lhs = H.PatternApplication $ H.Pattern_Application hname args
            rhs <- H.CaseRhs <$> encodeTerm namespaces cx rhsTerm
            return $ H.Alternative lhs rhs Nothing
    FunctionLambda (Lambda (Variable v) body) -> hslambda v <$> encodeTerm namespaces cx body
    FunctionPrimitive name -> pure $ H.ExpressionVariable $ hsPrimitiveReference name
    _ -> failWithTrace cx $ "unexpected function: " ++ show fun

encodeLiteral :: Literal -> Result H.Expression
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

encodeTerm :: (Eq m, Ord m, Read m, Show m) => Namespaces -> Context m -> Term m -> Result H.Expression
encodeTerm namespaces cx term = do
   case termExpr cx term of
    TermApplication (Application fun arg) -> case termExpr cx fun of
       TermFunction (FunctionElimination EliminationElement) -> encode cx arg
       _ -> hsapp <$> encode cx fun <*> encode cx arg
    TermElement name -> pure $ H.ExpressionVariable $ elementReference namespaces name
    TermFunction f -> encodeFunction namespaces cx f
    TermList els -> H.ExpressionList <$> CM.mapM (encode cx) els
    TermLiteral v -> encodeLiteral v
    TermNominal (Named tname term') -> if newtypesNotTypedefs
      then hsapp <$> pure (H.ExpressionVariable $ elementReference namespaces tname) <*> encode cx term'
      else encode cx term'
    TermOptional m -> case m of
      Nothing -> pure $ hsvar "Nothing"
      Just t -> hsapp (hsvar "Just") <$> encode cx t
    TermRecord (Record sname fields) -> do
      if L.null fields -- TODO: too permissive; not all empty record types are the unit type
        then pure $ H.ExpressionTuple []
        else do
            let typeName = typeNameForRecord sname
            updates <- CM.mapM toFieldUpdate fields
            return $ H.ExpressionConstructRecord $ H.Expression_ConstructRecord (rawName typeName) updates
          where
            toFieldUpdate (Field fn ft) = H.FieldUpdate (recordFieldReference namespaces sname fn) <$> encode cx ft
    TermUnion (Union sname (Field fn ft)) -> do
      let lhs = H.ExpressionVariable $ unionFieldReference namespaces sname fn
      case termExpr cx ft of
        TermRecord (Record _ []) -> pure lhs
        _ -> hsapp lhs <$> encode cx ft
    TermVariable (Variable v) -> pure $ hsvar v
    _ -> failWithTrace cx $ "unexpected term: " ++ show term
  where
    encode = encodeTerm namespaces

encodeType :: Show m => Context m -> Namespaces -> Type m -> Result H.Type
encodeType cx namespaces typ = case typeExpr cx typ of
    TypeApplication (ApplicationType lhs rhs) -> toTypeApplication <$>
      CM.sequence [encode lhs, encode rhs]
    TypeElement et -> encode et
    TypeFunction (FunctionType dom cod) -> H.TypeFunction <$> (H.Type_Function <$> encode dom <*> encode cod)
    TypeLambda (LambdaType (VariableType v) body) -> toTypeApplication <$> CM.sequence [
      encode body,
      pure $ H.TypeVariable $ simpleName v]
    TypeList lt -> H.TypeList <$> encode lt
    TypeLiteral lt -> H.TypeVariable . rawName <$> case lt of
      LiteralTypeBoolean -> pure "Bool"
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat32 -> pure "Float"
        FloatTypeFloat64 -> pure "Double"
        _ -> failWithTrace cx $ "unexpected floating-point type: " ++ show ft
      LiteralTypeInteger it -> case it of
        IntegerTypeBigint -> pure "Integer"
        IntegerTypeInt32 -> pure "Int"
        _ -> failWithTrace cx $ "unexpected integer type: " ++ show it
      LiteralTypeString -> pure "String"
      _ -> failWithTrace cx $ "unexpected literal type: " ++ show lt
    TypeMap (MapType kt vt) -> toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Map",
      encode kt,
      encode vt]
    TypeNominal name -> nominal name
    TypeOptional ot -> toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Maybe",
      encode ot]
    TypeRecord (RowType _ []) -> pure $ H.TypeTuple []  -- TODO: too permissive; not all empty record types are the unit type
    TypeRecord (RowType n _) -> nominal n
    TypeSet st -> toTypeApplication <$> CM.sequence [
      pure $ H.TypeVariable $ rawName "Set",
      encode st]
    TypeUnion (RowType n _) -> nominal n
    TypeVariable (VariableType v) -> pure $ H.TypeVariable $ simpleName v
    _ -> failWithTrace cx $ "unexpected type: " ++ show typ
  where
    encode = encodeType cx namespaces
    nominal name = pure $ H.TypeVariable $ elementReference namespaces name

moduleToHaskellModule :: (Ord m, Read m, Show m) => Context m -> Graph m -> Qualified H.Module
moduleToHaskellModule cx g = graphToExternalModule language (encodeTerm namespaces) constructModule cx g
  where
    namespaces = namespacesForGraph g

printGraph :: (Ord m, Read m, Show m) => Context m -> Graph m -> Qualified (M.Map FilePath String)
printGraph cx g = do
  hsmod <- moduleToHaskellModule cx g
  let s = printExpr $ parenthesize $ toTree hsmod
  return $ M.fromList [(graphNameToFilePath True (FileExtension "hs") $ graphName g, s)]

toDataDeclarations :: (Ord m, Show m)
  => M.Map (Type m) (Coder (Term m) H.Expression) -> Namespaces -> Context m
  -> (Element m, TypedTerm m) -> Result [H.DeclarationWithComments]
toDataDeclarations coders namespaces cx (el, TypedTerm typ term) = do
    let coder = Y.fromJust $ M.lookup typ coders
    rhs <- H.RightHandSide <$> coderEncode coder term
    let hname = simpleName $ localNameOf $ elementName el
    let pat = H.PatternApplication $ H.Pattern_Application hname []
    htype <- encodeType cx namespaces typ
    let decl = H.DeclarationTypedBinding $ H.TypedBinding
                (H.TypeSignature hname htype)
                (H.ValueBindingSimple $ rewriteValueBinding $ H.ValueBinding_Simple pat rhs Nothing)
    comments <- annotationClassTermDescription (contextAnnotations cx) cx term
    return [H.DeclarationWithComments decl comments]
  where
    rewriteValueBinding vb = case vb of
      H.ValueBinding_Simple (H.PatternApplication (H.Pattern_Application name args)) rhs bindings -> case rhs of
        H.RightHandSide (H.ExpressionLambda (H.Expression_Lambda vars body)) -> rewriteValueBinding $
          H.ValueBinding_Simple
            (H.PatternApplication (H.Pattern_Application name (args ++ vars))) (H.RightHandSide body) bindings
        _ -> vb

toTypeDeclarations :: (Ord m, Read m, Show m)
  => Namespaces -> Context m -> Element m -> Term m -> Result [H.DeclarationWithComments]
toTypeDeclarations namespaces cx el term = do
    let lname = localNameOf $ elementName el
    let hname = simpleName lname
    t <- decodeType cx term
    isSer <- isSerializable
    let deriv = H.Deriving $ if isSer
                  then rawName <$> ["Eq", "Ord", "Read", "Show"]
                  else []
    let (vars, t') = unpackLambdaType cx t
    let hd = declHead hname $ L.reverse vars
    decl <- case typeExpr cx t' of
      TypeRecord (RowType _ fields) -> do
        cons <- recordCons lname fields
        return $ H.DeclarationData (H.DataDeclaration H.DataDeclaration_KeywordData [] hd [cons] [deriv])
      TypeUnion (RowType _ fields) -> do
        cons <- CM.mapM (unionCons lname) fields
        return $ H.DeclarationData (H.DataDeclaration H.DataDeclaration_KeywordData [] hd cons [deriv])
      _ -> if newtypesNotTypedefs
        then do
          cons <- newtypeCons el t'
          return $ H.DeclarationData (H.DataDeclaration H.DataDeclaration_KeywordNewtype [] hd [cons] [deriv])
        else do
          htype <- encodeAdaptedType namespaces cx t
          return $ H.DeclarationType (H.TypeDeclaration hd htype)
    comments <- annotationClassTermDescription (contextAnnotations cx) cx term
    return $ [H.DeclarationWithComments decl comments] ++ constantDecls cx namespaces (elementName el) t
  where
    isSerializable = do
        deps <- typeDependencies cx (elementName el)
        let allVariants = S.fromList $ L.concat (variants <$> M.elems deps)
        return $ not $ S.member TypeVariantFunction allVariants
      where
        variants typ = typeVariant <$> foldOverType TraversalOrderPre (\m t -> t:m) [] typ

    declHead name vars = case vars of
      [] -> H.DeclarationHeadSimple name
      ((VariableType h):rest) -> H.DeclarationHeadApplication $
        H.DeclarationHead_Application (declHead name rest) (H.Variable $ simpleName h)

    newtypeCons el typ = do
        let hname = simpleName $ newtypeAccessorName $ elementName el
        htype <- encodeAdaptedType namespaces cx typ
        comments <- annotationClassTypeDescription (contextAnnotations cx) cx typ
        let hfield = H.FieldWithComments (H.Field hname htype) comments
        return $ H.ConstructorRecord $ H.Constructor_Record (simpleName $ localNameOf $ elementName el) [hfield]

    recordCons lname fields = do
        hFields <- CM.mapM toField fields
        return $ H.ConstructorRecord $ H.Constructor_Record (simpleName lname) hFields
      where
        toField (FieldType (FieldName fname) ftype) = do
          let hname = simpleName $ decapitalize lname ++ capitalize fname
          htype <- encodeAdaptedType namespaces cx ftype
          comments <- annotationClassTypeDescription (contextAnnotations cx) cx ftype
          return $ H.FieldWithComments (H.Field hname htype) comments

    unionCons lname (FieldType (FieldName fname) ftype) = do
      let nm = capitalize lname ++ capitalize fname
      typeList <- if typeExpr cx ftype == Types.unit
        then pure []
        else do
          htype <- encodeAdaptedType namespaces cx ftype
          return [htype]
      return $ H.ConstructorOrdinary $ H.Constructor_Ordinary (simpleName nm) typeList
