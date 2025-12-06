-- | Wrapper for @wisnesky's Algorithm W implementation which makes it into an alternative inferencer for Hydra

module Hydra.Reference.AlgorithmWBridge where

import Hydra.Reference.AlgorithmW

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Dsl.Literals as Literals
import qualified Hydra.Dsl.LiteralTypes as LiteralTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Libraries
import Hydra.Coders
import Hydra.Rewriting

import qualified Data.List as L
import qualified Data.Map as M
import qualified Control.Monad as CM

import Control.Monad.Except
import Control.Monad.State


-- A minimal Hydra graph container for use in these translation functions
data HydraContext = HydraContext (M.Map Core.Name Graph.Primitive)

----------------------------------------

-- Note: no support for @wisnesky's Prim constructors other than Str, Nat, Cons, and Nil
hydraTermToStlc :: HydraContext -> Core.Term -> Either String Expr
hydraTermToStlc context term = case term of
    Core.TermApplication (Core.Application t1 t2) -> App <$> toStlc t1 <*> toStlc t2
    Core.TermFunction f -> case f of
--      Core.FunctionElimination elm -> case elm of
--        EliminationRecord (Projection tname fname) -> Right $ ExprProj
      Core.FunctionLambda (Core.Lambda (Core.Name v) _ body) -> Abs <$> pure v <*> toStlc body
      Core.FunctionPrimitive name -> do
        prim <- case M.lookup name prims of
          Nothing -> Left $ "no such primitive: " ++ Core.unName name
          Just p -> Right p
        ts <- hydraTypeSchemeToStlc $ Graph.primitiveType prim
        return $ Const $ PrimTyped $ TypedPrimitive name ts
    Core.TermLet (Core.Let bindings env) -> Letrec <$> CM.mapM bindingToStlc bindings <*> toStlc env
      where
        bindingToStlc (Core.Binding (Core.Name v) term _) = do
          s <- toStlc term
          return (v, s)
    Core.TermList els -> do
      sels <- CM.mapM toStlc els
      return $ foldr (\el acc -> App (App (Const Cons) el) acc) (Const Nil) sels
    Core.TermLiteral lit -> pure $ Const $ PrimLiteral lit
    Core.TermPair (t1, t2) -> pair <$> toStlc t1 <*> toStlc t2
    Core.TermProduct els -> case els of
      [] -> pure $ Const TT
      [t1, t2] -> pair <$> toStlc t1 <*> toStlc t2
      _ -> Left $ "products with arity other than 0 or 2 are not supported"
    Core.TermEither et -> case et of
      Left l -> App (Const Inl) <$> toStlc l
      Right r -> App (Const Inr) <$> toStlc r
    Core.TermUnit -> pure $ Const TT
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
    toStlc typ = case deannotateType typ of
      Core.TypeFunction (Core.FunctionType dom cod) -> TyFn <$> toStlc dom <*> toStlc cod
      Core.TypeList et -> TyList <$> toStlc et
      Core.TypeLiteral lt -> pure $ TyLit lt
--      TypeMap MapType |
--      TypeMaybe Type |
      Core.TypePair (Core.PairType first second) -> TyProd <$> toStlc first <*> toStlc second
--      TypeRecord RowType |
--      TypeSet Type |
      Core.TypeEither (Core.EitherType left right) -> TyEither <$> toStlc left <*> toStlc right
--      TypeUnion RowType |
      Core.TypeVariable name -> pure $ TyVar $ Core.unName name
--      TypeWrap (Nominal Type)
      _ -> Left $ "unsupported type: " ++ show typ

-- | Convert a System F term expression to a Hydra term
toTerm :: FExpr -> Core.Term
toTerm expr = case expr of
  FAbs v dom e -> Core.TermFunction $ Core.FunctionLambda (Core.Lambda (Core.Name v) (Just hdom) (toTerm e))
    where
      hdom = Core.typeSchemeType $ toTypeScheme dom
  -- App (App (Const Pair) (nat 0)) (nat 1)
  --   e1 = App (Const Pair) (nat 0)
  --   e2 = nat 1

  -- FApp (FApp (FTyApp (FConst pair) [Int32,Int32]) (FConst 0)) (FConst 1)
  --   e1 = FApp (FTyApp (FConst pair) [Int32,Int32]) (FConst 0)
  --   e2 = FConst 1
  FApp e1 e2 -> case e1 of
    FApp (FTyApp (FConst Cons) _) hd -> Core.TermList $
        fmap toTerm (hd:(gather e2)) -- TODO: include inferred type
      where
        gather e = case e of
          FTyApp (FConst Nil) _ -> []
          FApp (FApp (FTyApp (FConst Cons) _) hd) tl -> hd:(gather tl)
    FApp (FTyApp (FConst Pair) _) lhs -> Core.TermPair (toTerm lhs, toTerm e2)
    FTyApp (FConst Inl) _ -> Core.TermEither $ Left $ toTerm e2
    FTyApp (FConst Inr) _ -> Core.TermEither $ Right $ toTerm e2
    _ -> Core.TermApplication $ Core.Application (toTerm e1) (toTerm e2)
  FConst prim -> case prim of
    PrimLiteral lit -> Core.TermLiteral lit
    PrimTyped (TypedPrimitive name _) -> Core.TermFunction $ Core.FunctionPrimitive name
    Nil -> Core.TermList []
    Pair -> Terms.lambdas ["a", "b"] $ Terms.pair (Terms.var "a") (Terms.var "b")
    TT -> Core.TermProduct []
    _ -> Terms.string $ "unexpected primitive: " ++ show prim
    -- Note: other prims are unsupported; they can be added here as needed
  FLetrec bindings env -> Core.TermLet $ Core.Let (fmap bindingToHydra bindings) (toTerm env)
    where
      bindingToHydra (v, ty, term) = Core.Binding (Core.Name v) (toTerm term) $ Just $ toTypeScheme ty
  FTyAbs params body -> L.foldl (\t v -> Core.TermTypeLambda $ Core.TypeLambda (Core.Name v) t) (toTerm body) $ L.reverse params
  FTyApp fun args -> L.foldl (\t a -> Core.TermTypeApplication $ Core.TypeApplicationTerm t a) (toTerm fun) $ L.reverse hargs
    where
      hargs = fmap (\t -> Core.typeSchemeType $ toTypeScheme t) args
  FVar v -> Core.TermVariable $ Core.Name v

toType :: FTy -> Core.Type
toType ty = case ty of
  FTyVar v -> Core.TypeVariable $ Core.Name v
  FTyLit lt -> Core.TypeLiteral lt
  FTyList lt -> Core.TypeList $ toType lt
  FTyFn dom cod -> Core.TypeFunction $ Core.FunctionType (toType dom) (toType cod)
  FTyProd t1 t2 -> Core.TypePair $ Core.PairType (toType t1) (toType t2)
  FTySum t1 t2 -> Core.TypeEither $ Core.EitherType (toType t1) (toType t2)
  FTyEither t1 t2 -> Core.TypeEither $ Core.EitherType (toType t1) (toType t2)
  FTyUnit -> Core.TypeProduct []
  FTyVoid -> Core.TypeUnit

-- | Convert a System F type expression to a Hydra type scheme
toTypeScheme :: FTy -> Core.TypeScheme
toTypeScheme ty = case ty of
  FForall vars body -> Core.TypeScheme (Core.Name <$> vars) $ toType body
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
wrapTerm term = Core.TermLet $ Core.Let ([Core.Binding sFieldName term Nothing]) $
  Core.TermLiteral $ Core.LiteralString "tempEnvironment"

unwrapTerm :: Core.Term -> IO (Core.Term, Core.TypeScheme)
unwrapTerm term = case term of
  Core.TermLet (Core.Let bindings _) -> case bindings of
    [(Core.Binding fname t mts)] -> if fname == sFieldName
      then case mts of
         Nothing -> fail "no type scheme in inferred let binding"
         Just ts -> pure (t, ts)
      else fail "expected let binding matching input"
    _ -> fail "expected let bindings"

inferExpr :: Expr -> IO (FExpr, FTy)
inferExpr t = case (fst $ runState (runExceptT (w [] t)) 0) of
  Left e -> fail $ "inference error: " ++ e
  Right (_, (ty, f)) -> case (typeOf [] [] f) of
    Left err -> fail $ "type error: " ++ err
    Right tt -> if tt == mTyToFTy ty
      then return (f, tt)
      else fail "no match"
