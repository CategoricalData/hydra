-- | Wrapper for @wisnesky's Algorithm W implementation which makes it into an alternative inferencer for Hydra

module Hydra.Inference.AlgorithmWBridge where

import Hydra.Inference.AlgorithmW

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Dsl.Literals as Literals
import qualified Hydra.Dsl.LiteralTypes as LiteralTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Libraries
import Hydra.Basics
import Hydra.Strip
import Hydra.Tier1
import Hydra.Coders
import Hydra.Inference.Substitution
import Hydra.Rewriting

import qualified Data.List as L
import qualified Data.Map as M
import qualified Control.Monad as CM

import Control.Monad.Except
import Control.Monad.State


-- A minimal Hydra graph container for use in these translation functions
data HydraContext = HydraContext (M.Map Core.Name Graph.Primitive)

----------------------------------------

-- | Find all of the bound variables in the type annotations within a System F term.
--   This function considers the types in "typed terms" (term:type), domain types on lambdas (\v:type.term),
--   and also type abstractions (/\v.term) to provide bound type variables.
boundTypeVariablesInSystemFTerm :: Core.Term -> [Core.Name]
boundTypeVariablesInSystemFTerm = L.nub . foldOverTerm TraversalOrderPost addTypeVars []
  where
    addTypeVars vars term = typeVarsIn term ++ vars
    typeVarsIn term = case term of
      Core.TermFunction (Core.FunctionLambda (Core.Lambda _ (Just typ) _)) -> boundVariablesInTypeOrdered typ
      Core.TermTypeAbstraction (Core.TypeAbstraction v _) -> [v]
      Core.TermTyped (Core.TypedTerm term typ) -> boundVariablesInTypeOrdered typ
      _ -> []

boundTypeVariablesInTermOrdered :: Core.Term -> [Core.Name]
boundTypeVariablesInTermOrdered = L.nub . foldOverTerm TraversalOrderPre fld []
  where
    fld vars term = case term of
      Core.TermTyped (Core.TypedTerm _ typ) -> variablesInTypeOrdered False typ ++ vars
      _ -> vars

-- | Finds all of the universal type variables in a type expression, in the order in which they appear.
-- Note: this function assumes that there are no shadowed type variables, as in (forall a. forall a. a)
-- TODO: redundant with variablesInTypeOrdered
boundVariablesInTypeOrdered :: Core.Type -> [Core.Name]
boundVariablesInTypeOrdered typ = case typ of
  Core.TypeLambda (Core.LambdaType var body) -> var:(boundVariablesInTypeOrdered body)
  t -> L.concat (boundVariablesInTypeOrdered <$> subtypes t)

-- | Replace arbitrary bound type variables like v, a, v_12 with the systematic type variables t0, t1, t2, ...
--   following a canonical ordering in the term.
--   This function assumes that the bound variables do not also appear free in the type expressions of the term,
--   which in Hydra is made less likely by using the unusual naming convention tv_0, tv_1, etc. for temporary variables.
normalizeBoundTypeVariablesInSystemFTerm :: Core.Term -> Core.Term
normalizeBoundTypeVariablesInSystemFTerm term = replaceTypeVariablesInSystemFTerm subst term
  where
    actualVars = boundTypeVariablesInSystemFTerm term
    subst = M.fromList $ L.zip actualVars normalVariables

replaceTypeVariables :: M.Map Core.Name Core.Name -> Core.Type -> Core.Type
replaceTypeVariables subst = rewriteType $ \recurse t -> case recurse t of
    Core.TypeVariable v -> Core.TypeVariable $ replace v
    Core.TypeLambda (Core.LambdaType v body) -> Core.TypeLambda $ Core.LambdaType (replace v) body
    t1 -> t1
  where
    replace v = M.findWithDefault v v subst

-- Note: this will replace all occurrences, regardless of boundness or shadowing
replaceTypeVariablesInSystemFTerm :: M.Map Core.Name Core.Name -> Core.Term -> Core.Term
replaceTypeVariablesInSystemFTerm subst = rewriteTerm $ \recurse term ->
  case recurse term of
    Core.TermFunction (Core.FunctionLambda (Core.Lambda v (Just mt) body)) ->
        Core.TermFunction $ Core.FunctionLambda $ Core.Lambda v (Just mt2) body
      where
        mt2 = replaceTypeVariables subst mt
    Core.TermTypeAbstraction (Core.TypeAbstraction v body) -> Core.TermTypeAbstraction $ Core.TypeAbstraction v2 body
      where
        v2 = M.findWithDefault v v subst
    Core.TermTyped (Core.TypedTerm term typ) -> Core.TermTyped $ Core.TypedTerm term (replaceTypeVariables subst typ)
    t -> t

-- | Find the variables (both bound and free) in a type expression, following a preorder traversal of the expression.
variablesInTypeOrdered :: Bool -> Core.Type -> [Core.Name]
variablesInTypeOrdered onlyBound = L.nub . vars -- Note: we rely on the fact that 'nub' keeps the first occurrence
  where
    vars t = case t of
      Core.TypeLambda (Core.LambdaType v body) -> v:(vars body)
      Core.TypeVariable v -> if onlyBound then [] else [v]
      _ -> L.concat (vars <$> subtypes t)

----------------------------------------

-- Note: no support for @wisnesky's Prim constructors other than PrimStr, PrimNat, Cons, and Nil
hydraTermToStlc :: HydraContext -> Core.Term -> Either String Expr
hydraTermToStlc context term = case term of
    Core.TermApplication (Core.Application t1 t2) -> App <$> toStlc t1 <*> toStlc t2
    Core.TermFunction f -> case f of
      Core.FunctionLambda (Core.Lambda (Core.Name v) _ body) -> Abs <$> pure v <*> toStlc body
      Core.FunctionPrimitive name -> do
        prim <- case M.lookup name prims of
          Nothing -> Left $ "no such primitive: " ++ Core.unName name
          Just p -> Right p
        ts <- hydraTypeSchemeToStlc $ Graph.primitiveType prim
        return $ Const $ TypedPrim $ TypedPrimitive name ts
    Core.TermLet (Core.Let bindings env) -> Letrec <$> CM.mapM bindingToStlc bindings <*> toStlc env
      where
        bindingToStlc (Core.LetBinding (Core.Name v) term _) = do
          s <- toStlc term
          return (v, s)
    Core.TermList els -> do
      sels <- CM.mapM toStlc els
      return $ foldr (\el acc -> App (App (Const Cons) el) acc) (Const Nil) sels
    Core.TermLiteral lit -> pure $ Const $ Lit lit
    Core.TermProduct els -> Tuple <$> (CM.mapM toStlc els)
    Core.TermVariable (Core.Name v) -> pure $ Var v
    _ -> Left $ "Unsupported term: " ++ show term
  where
    HydraContext prims = context
    toStlc = hydraTermToStlc context
    pair a b = App (App (Const Pair) a) b

hydraTypeSchemeToStlc :: Core.TypeScheme -> Either String TypSch
hydraTypeSchemeToStlc (Core.TypeScheme vars body) = do
    sbody <- toStlc body
    return $ Forall (Core.unName <$> vars) sbody
  where
    toStlc typ = case stripType typ of
      Core.TypeFunction (Core.FunctionType dom cod) -> TyFn <$> toStlc dom <*> toStlc cod
      Core.TypeList et -> TyList <$> toStlc et
      Core.TypeLiteral lt -> pure $ TyLit lt
--      TypeMap MapType |
--      TypeOptional Type |
      Core.TypeProduct types -> TyTuple <$> (CM.mapM toStlc types)
--      TypeRecord RowType |
--      TypeSet Type |
--      TypeStream Type |
      Core.TypeSum types -> if L.length types == 0
        then pure TyVoid
        else if L.length types == 1
          then Left $ "unary sums are not yet supported"
          else do
            stypes <- CM.mapM toStlc types
            let rev = L.reverse stypes
            return $ L.foldl (\a e -> TySum e a) (TySum (rev !! 1) (rev !! 0)) $ L.drop 2 rev
--      TypeUnion RowType |
      Core.TypeVariable name -> pure $ TyVar $ Core.unName name
--      TypeWrap (Nominal Type)
      _ -> Left $ "unsupported type: " ++ show typ


-- hydraTypeToTypeScheme :: Core.Type -> Either String TypSch
-- hydraTypeToTypeScheme typ = do
--     let (boundVars, baseType) = splitBoundVars [] typ
--     ty <- toStlc baseType
--     return $ Forall (Core.unName <$> boundVars) ty
--   where
--     splitBoundVars vars typ = case stripType typ of
--       Core.TypeLambda (Core.LambdaType v body) -> (v:vars', typ')
--         where
--           (vars', typ') = splitBoundVars vars body
--       _ -> (vars, typ)

systemFExprToHydra :: FExpr -> Either String Core.Term
systemFExprToHydra expr = case expr of
  FConst prim -> case prim of
    Lit lit -> pure $ Core.TermLiteral lit
    TypedPrim (TypedPrimitive name _) -> pure $ Core.TermFunction $ Core.FunctionPrimitive name
    Nil -> pure $ Core.TermList []
    _ -> Left $ "Unsupported primitive: " ++ show prim
    -- Note: other prims are unsupported
  FVar v -> pure $ Core.TermVariable $ Core.Name v
  FApp e1 e2 -> case e1 of
    FApp (FTyApp (FConst Cons) _) hd -> do
        els <- CM.mapM systemFExprToHydra (hd:(gather e2))
        return $ Core.TermList els -- TODO: include inferred type
      where
        gather e = case e of
          FTyApp (FConst Nil) _ -> []
          FApp (FApp (FTyApp (FConst Cons) _) hd) tl -> hd:(gather tl)
    FTyApp (FConst Pair) _ -> do
--        els <- CM.mapM systemFExprToHydra (gather expr)
        els <- pure []
        return $ Core.TermProduct els -- TODO: include inferred type
      where
        gather e = case e of
          FApp (FApp (FTyApp (FConst Pair) _) el) arg -> el:(gather arg)
          _ -> [e]
    _ -> Core.TermApplication <$> (Core.Application <$> systemFExprToHydra e1 <*> systemFExprToHydra e2)
  FAbs v dom e -> do
    term <- systemFExprToHydra e
    hdom <- Core.typeSchemeType <$> systemFTypeToHydra dom
    return $ Core.TermFunction $ Core.FunctionLambda (Core.Lambda (Core.Name v) (Just hdom) term)
  FTyAbs params body -> do
    hbody <- systemFExprToHydra body
    return $ L.foldl (\t v -> Core.TermTypeAbstraction $ Core.TypeAbstraction (Core.Name v) t) hbody $ L.reverse params
  FTyApp fun args -> do
    hfun <- systemFExprToHydra fun
    hargs <- CM.mapM (\t -> Core.typeSchemeType <$> systemFTypeToHydra t) args
    return $ L.foldl (\t a -> Core.TermTypeApplication $ Core.TypedTerm t a) hfun $ L.reverse hargs
  FLetrec bindings env -> Core.TermLet <$>
      (Core.Let <$> CM.mapM bindingToHydra bindings <*> systemFExprToHydra env)
    where
      bindingToHydra (v, ty, term) = do
        hterm <- systemFExprToHydra term
        hts <- systemFTypeToHydra ty
        return $ Core.LetBinding (Core.Name v) hterm $ Just hts
  FTuple els -> Core.TermProduct <$> (CM.mapM systemFExprToHydra els)
  FInj i types e -> Core.TermSum <$> (Core.Sum i (L.length types) <$> systemFExprToHydra e)

systemFTypeToHydra :: FTy -> Either String Core.TypeScheme
systemFTypeToHydra ty = case ty of
    FForall vars body -> Core.TypeScheme (Core.Name <$> vars) <$> toHydra body
    _ -> Core.TypeScheme [] <$> toHydra ty
  where
    toHydra ty = case ty of
      FTyVar v -> pure $ Core.TypeVariable $ Core.Name v
      FTyLit lt -> pure $ Core.TypeLiteral lt
      FTyList lt -> Core.TypeList <$> toHydra lt
      FTyFn dom cod -> Core.TypeFunction <$> (Core.FunctionType <$> toHydra dom <*> toHydra cod)
      FTyProd t1 t2 -> Core.TypeProduct <$> CM.mapM toHydra (t1:(componentsTypesOf t2))
        where
          componentsTypesOf t = case t of
            FTyProd t1 t2 -> t1:(componentsTypesOf t2)
            _ -> [t]
      FTySum t1 t2 -> Core.TypeSum <$> CM.mapM toHydra (t1:(componentsTypesOf t2))
        where
          componentsTypesOf t = case t of
            FTySum t1 t2 -> t1:(componentsTypesOf t2)
            _ -> [t]
      FTyUnit -> pure $ Core.TypeProduct []
      FTyVoid -> pure $ Core.TypeSum []
      FTyTuple tys -> Core.TypeProduct <$> (CM.mapM toHydra tys)
      FTyVariant tys -> Core.TypeSum <$> (CM.mapM toHydra tys)

inferWithAlgorithmW :: HydraContext -> Core.Term -> IO Core.Term
inferWithAlgorithmW context term = do
    stlc <- case hydraTermToStlc context (wrap term) of
       Left err -> fail err
       Right t -> return t
    (fexpr, _) <- inferExpr stlc
    case systemFExprToHydra fexpr of
      Left err -> fail err
      Right t -> normalizeBoundTypeVariablesInSystemFTerm <$> unwrap t
  where
    sFieldName = Core.Name "tempVar"
    wrap term = Core.TermLet $ Core.Let ([Core.LetBinding sFieldName term Nothing]) $
      Core.TermLiteral $ Core.LiteralString "tempEnvironment"
    unwrap term = case term of
      Core.TermLet (Core.Let bindings _) -> case bindings of
        [(Core.LetBinding fname t _)] -> if fname == sFieldName
          then pure t
          else fail "expected let binding matching input"
        _ -> fail "expected let bindings"

inferExpr :: Expr -> IO (FExpr, FTy)
inferExpr t = case (fst $ runState (runExceptT (w 0 [] [] t)) ([],0)) of
  Left e -> fail $ "inference error: " ++ e
  Right (_, (ty, f)) -> case (typeOf [] [] [] f) of
    Left err -> fail $ "type error: " ++ err
    Right tt -> if tt == mTyToFTy ty
      then return (f, tt)
      else fail "no match"
