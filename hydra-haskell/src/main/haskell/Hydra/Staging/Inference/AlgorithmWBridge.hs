-- | Wrapper for @wisnesky's Algorithm W implementation which makes it into an alternative inferencer for Hydra

module Hydra.Staging.Inference.AlgorithmWBridge where

import Hydra.Staging.Inference.AlgorithmW

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Dsl.Literals as Literals
import qualified Hydra.Dsl.LiteralTypes as LiteralTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Libraries
import Hydra.Strip
import Hydra.Rewriting
import Hydra.Coders
import Hydra.Staging.Inference.Substitution
import Hydra.Staging.Rewriting

import qualified Data.List as L
import qualified Data.Map as M
import qualified Control.Monad as CM

import Control.Monad.Except
import Control.Monad.State


-- A minimal Hydra graph container for use in these translation functions
data HydraContext = HydraContext (M.Map Core.Name Graph.Primitive)

----------------------------------------

-- Note: no support for @wisnesky's Prim constructors other than PrimStr, PrimNat, PrimCons, and PrimNil
hydraTermToStlc :: HydraContext -> Core.Term -> Either String Expr
hydraTermToStlc context term = case term of
    Core.TermApplication (Core.Application t1 t2) -> ExprApp <$> toStlc t1 <*> toStlc t2
    Core.TermFunction f -> case f of
--      Core.FunctionElimination elm -> case elm of
--        EliminationRecord (Projection tname fname) -> Right $ ExprProj
      Core.FunctionLambda (Core.Lambda (Core.Name v) _ body) -> ExprAbs <$> pure v <*> toStlc body
      Core.FunctionPrimitive name -> do
        prim <- case M.lookup name prims of
          Nothing -> Left $ "no such primitive: " ++ Core.unName name
          Just p -> Right p
        ts <- hydraTypeSchemeToStlc $ Graph.primitiveType prim
        return $ ExprConst $ PrimTyped $ TypedPrimitive name ts
    Core.TermLet (Core.Let bindings env) -> ExprLetrec <$> CM.mapM bindingToStlc bindings <*> toStlc env
      where
        bindingToStlc (Core.LetBinding (Core.Name v) term _) = do
          s <- toStlc term
          return (v, s)
    Core.TermList els -> do
      sels <- CM.mapM toStlc els
      return $ foldr (\el acc -> ExprApp (ExprApp (ExprConst PrimCons) el) acc) (ExprConst PrimNil) sels
    Core.TermLiteral lit -> pure $ ExprConst $ PrimLiteral lit
    Core.TermProduct els -> ExprTuple <$> (CM.mapM toStlc els)
    Core.TermVariable (Core.Name v) -> pure $ ExprVar v
    _ -> Left $ "Unsupported term: " ++ show term
  where
    HydraContext prims = context
    toStlc = hydraTermToStlc context
    pair a b = ExprApp (ExprApp (ExprConst PrimPair) a) b

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

-- | Convert a System F term expression to a Hydra term
toTerm :: FExpr -> Core.Term
toTerm expr = case expr of
  FConst prim -> case prim of
    PrimLiteral lit -> Core.TermLiteral lit
    PrimTyped (TypedPrimitive name _) -> Core.TermFunction $ Core.FunctionPrimitive name
    PrimNil -> Core.TermList []
    -- Note: other prims are unsupported; they can be added here as needed
  FVar v -> Core.TermVariable $ Core.Name v
  FTuple els -> Core.TermProduct $ (fmap toTerm els)
  -- FProj i e -> ... TODO
  FInj i types e -> Core.TermSum $ Core.Sum i (L.length types) $ toTerm e
  -- FCase... -> ... TODO
  FApp e1 e2 -> case e1 of
    FApp (FTyApp (FConst PrimCons) _) hd -> Core.TermList $
        fmap toTerm (hd:(gather e2)) -- TODO: include inferred type
      where
        gather e = case e of
          FTyApp (FConst PrimNil) _ -> []
          FApp (FApp (FTyApp (FConst PrimCons) _) hd) tl -> hd:(gather tl)
    FTyApp (FConst PrimPair) _ -> Core.TermProduct els -- TODO: include inferred type
      where
--        els = fmap toTerm (gather expr)
        els = []
        gather e = case e of
          FApp (FApp (FTyApp (FConst PrimPair) _) el) arg -> el:(gather arg)
          _ -> [e]
    _ -> Core.TermApplication $ Core.Application (toTerm e1) (toTerm e2)
  FAbs v dom e -> Core.TermFunction $ Core.FunctionLambda (Core.Lambda (Core.Name v) (Just hdom) (toTerm e))
    where
      hdom = Core.typeSchemeType $ toTypeScheme dom
  FTyApp fun args -> L.foldl (\t a -> Core.TermTypeApplication $ Core.TypedTerm t a) (toTerm fun) $ L.reverse hargs
    where
      hargs = fmap (\t -> Core.typeSchemeType $ toTypeScheme t) args
  FTyAbs params body -> L.foldl (\t v -> Core.TermTypeAbstraction $ Core.TypeAbstraction (Core.Name v) t) (toTerm body) $ L.reverse params
  FLetrec bindings env -> Core.TermLet $ Core.Let (fmap bindingToHydra bindings) (toTerm env)
    where
      bindingToHydra (v, ty, term) = Core.LetBinding (Core.Name v) (toTerm term) $ Just $ toTypeScheme ty

toType :: FTy -> Core.Type
toType ty = case ty of
  FTyVar v -> Core.TypeVariable $ Core.Name v
  FTyLit lt -> Core.TypeLiteral lt
  FTyList lt -> Core.TypeList $ toType lt
  FTyFn dom cod -> Core.TypeFunction $ Core.FunctionType (toType dom) (toType cod)
  FTyProd t1 t2 -> Core.TypeProduct (toType <$> (t1:(componentsTypesOf t2)))
    where
      componentsTypesOf t = case t of
        FTyProd t1 t2 -> t1:(componentsTypesOf t2)
        _ -> [t]
  FTySum t1 t2 -> Core.TypeSum (toType <$> (t1:(componentsTypesOf t2)))
    where
      componentsTypesOf t = case t of
        FTySum t1 t2 -> t1:(componentsTypesOf t2)
        _ -> [t]
  FTyUnit -> Core.TypeProduct []
  FTyVoid -> Core.TypeSum []
  FTyTuple tys -> Core.TypeProduct (toType <$> tys)
  FTyVariant tys -> Core.TypeSum (toType <$> tys)

-- | Convert a System F type expression to a Hydra type scheme
toTypeScheme :: FTy -> Core.TypeScheme
toTypeScheme ty = case ty of
  FTyForall vars body -> Core.TypeScheme (Core.Name <$> vars) $ toType body
  _ -> Core.TypeScheme [] $ toType ty

termToInferredFExpr :: HydraContext -> Core.Term -> IO (FExpr, FTy)
termToInferredFExpr context term = do
  stlc <- case hydraTermToStlc context (wrapTerm term) of
     Left err -> fail err
     Right t -> return t
  inferExpr stlc

termToInferredTerm :: HydraContext -> Core.Term -> IO (Core.Term, Core.TypeScheme)
termToInferredTerm context term = do
  fexpr <- fst <$> termToInferredFExpr context term
  unwrapTerm (normalizeTypeVariablesInTerm $ toTerm fexpr)

sFieldName = Core.Name "tempVar"

-- Wrap a term inside a let-term; the Algorithm W implementation only produces "forall" types for let-bindings.
wrapTerm :: Core.Term -> Core.Term
wrapTerm term = Core.TermLet $ Core.Let ([Core.LetBinding sFieldName term Nothing]) $
  Core.TermLiteral $ Core.LiteralString "tempEnvironment"

unwrapTerm :: Core.Term -> IO (Core.Term, Core.TypeScheme)
unwrapTerm term = case term of
  Core.TermLet (Core.Let bindings _) -> case bindings of
    [(Core.LetBinding fname t mts)] -> if fname == sFieldName
      then case mts of
         Nothing -> fail "no type scheme in inferred let binding"
         Just ts -> pure (t, ts)
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
