module Hydra.Staging.Inference.AltInference where

import Hydra.Variants
import Hydra.Coders
import Hydra.Core
import Hydra.Compute
import Hydra.Mantle
import Hydra.Flows
import Hydra.Strip
import Hydra.Rewriting
import Hydra.Staging.Rewriting
import Hydra.Lib.Flows as Flows
import qualified Hydra.Tools.Monads as TM
import qualified Hydra.Dsl.Types as Types

import qualified Data.List as L
import qualified Data.Map as M


--------------------------------------------------------------------------------
-- Graphs

showType :: Type -> String
showType (TypeFunction (FunctionType dom cod)) = "(" ++ showType dom ++ "→" ++ showType cod ++ ")"
showType (TypeList etyp) = "[" ++ showType etyp ++ "]"
showType (TypeLiteral lit) = show lit
showType (TypeMap (MapType keyTyp valTyp)) = "map<" ++ showType keyTyp ++ "," ++ showType valTyp ++ ">"
showType (TypeProduct types) = "(" ++ (L.intercalate "," (fmap showType types)) ++ ")"
showType (TypeVariable (Name name)) = name

showTypeScheme :: TypeScheme -> String
showTypeScheme (TypeScheme vars body) = "∀[" ++ (L.intercalate "," (fmap (\(Name name) -> name) vars)) ++ "]." ++ showType body

showConstraint :: TypeConstraint -> String
showConstraint (TypeConstraint ltyp rtyp _) = showType ltyp ++ "≡" ++ showType rtyp

--------------------------------------------------------------------------------
-- Unification

-- TODO: eliminate lambda types before unification
-- TODO: reduce application types before unification

data SUnificationError
  = SUnificationErrorCannotUnify Type Type (Maybe String)
  | SUnificationErrorOccursCheckFailed Name Type (Maybe String)
  deriving (Eq, Ord, Show)

-- | Determine whether a type variable appears within a type expression.
--   No distinction is made between free and bound type variables.
sOccursIn :: Name -> Type -> Bool
sOccursIn var = foldOverType TraversalOrderPre tryType False
  where
    tryType b typ = case typ of
      TypeVariable v -> b || v == var
      _ -> b

{-
Robinson's algorithm, following https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf
Specifically this is an implementation of the following rules:
 * Unify({(x, t)} ∪ E) = {t/x} Unify(E{t/x}) if x ∉ FV(t)
 * Unify(∅) = I (the identity substitution x ↦ x)
 * Unify({(x, x)} ∪ E) = Unify(E)
 * Unify({(f(s1, ..., sn), f(t1, ..., tn))} ∪ E) = Unify({(s1, t1), ..., (sn, tn)} ∪ E)
-}
uUnify :: [TypeConstraint] -> Either SUnificationError SSubst
uUnify [] = Right sEmptySubst
uUnify ((TypeConstraint left right comment):rest) = case sleft of
    TypeVariable name -> case sright of
        TypeVariable name2 -> if name == name2
          then uUnify rest
          else bind name sright
        _ -> tryBinding name sright
    _ -> case sright of
      TypeVariable name -> tryBinding name sleft
      _ -> do
        constraints2 <- uJoin sleft sright comment
        uUnify $ constraints2 ++ rest
  where
    sleft = stripType left
    sright = stripType right
    -- TODO: this occurrence check is expensive; consider delaying it until the time of substitution
    tryBinding v t = if sOccursIn v t
      then Left $ SUnificationErrorOccursCheckFailed v t comment
      else bind v t
    bind v t = sComposeSubst subst <$> uUnify (uSubstInConstraints subst rest)
      where
        subst = sSingletonSubst v t

uJoin :: Type -> Type -> Maybe String -> Either SUnificationError [TypeConstraint]
uJoin left right comment = case sleft of
    TypeFunction (FunctionType domleft codleft) -> case sright of
      TypeFunction (FunctionType domright codright) -> Right [
        joinOne domleft domright,
        joinOne codleft codright]
    TypeList eleft -> case sright of
      TypeList eright -> Right [joinOne eleft eright]
      _ -> cannotUnify
    TypeLiteral ltleft -> assertEqual
    TypeMap (MapType kleft vleft) -> case sright of
      TypeMap (MapType kright vright) -> Right [
        joinOne kleft kright,
        joinOne vleft vright]
      _ -> cannotUnify
    TypeOptional eleft -> case sright of
      TypeOptional eright -> Right [joinOne eleft eright]
      _ -> cannotUnify
    TypeProduct lefts -> case sright of
      TypeProduct rights -> joinList lefts rights
      _ -> cannotUnify
    TypeRecord rtleft -> case sright of
      TypeRecord rtright -> joinRowTypes rtleft rtright
      _ -> cannotUnify
    TypeSet eleft -> case sright of
      TypeSet eright -> Right [joinOne eleft eright]
      _ -> cannotUnify
    TypeSum lefts -> case sright of
      TypeSum rights -> joinList lefts rights
      _ -> cannotUnify
    TypeUnion rtleft -> case sright of
      TypeUnion rtright -> joinRowTypes rtleft rtright
      _ -> cannotUnify
    TypeWrap (WrappedType nameLeft eleft) -> case sright of
      TypeWrap (WrappedType nameRight eright) -> if nameLeft /= nameRight
        then cannotUnify
        else Right [joinOne eleft eright]
      _ -> cannotUnify
    -- TypeAnnotated, TypeApplication, TypeLambda, TypeVariable should not appear here
    _ -> cannotUnify
  where
    sleft = stripType left
    sright = stripType right
    joinOne l r = TypeConstraint l r Nothing
    cannotUnify = Left $ SUnificationErrorCannotUnify sleft sright comment
    assertEqual = if sleft == sright
      then Right []
      else cannotUnify
    joinList lefts rights = if L.length lefts == L.length rights
      then Right $ L.zipWith joinOne lefts rights
      else cannotUnify
    fieldNames = fmap fieldTypeName
    fieldTypes = fmap fieldTypeType
    joinRowTypes (RowType tnameLeft fieldsLeft) (RowType tnameRight fieldsRight) = if tnameLeft /= tnameRight
      then cannotUnify
      else if (fieldNames fieldsLeft) /= (fieldNames fieldsRight)
      then cannotUnify
      else joinList (fieldTypes fieldsLeft) (fieldTypes fieldsRight)

uSubstInConstraint :: SSubst -> TypeConstraint -> TypeConstraint
uSubstInConstraint subst (TypeConstraint t1 t2 ctx) = TypeConstraint (sSubstituteTypeVariables subst t1) (sSubstituteTypeVariables subst t2) ctx

uSubstInConstraints :: SSubst -> [TypeConstraint] -> [TypeConstraint]
uSubstInConstraints subst = fmap (uSubstInConstraint subst)

--------------------------------------------------------------------------------
-- Substitution

data SSubst = SSubst { sUnSubst :: M.Map Name Type }

instance Show SSubst where
  show (SSubst subst) = "{" ++ L.intercalate ", " (fmap (\((Name k), v) -> k ++ ": " ++ showType v) $ M.toList subst) ++ "}"

sEmptySubst = SSubst M.empty

sSingletonSubst :: Name -> Type -> SSubst
sSingletonSubst v t = SSubst $ M.singleton v t

sSubstituteTypeVariables :: SSubst -> Type -> Type
sSubstituteTypeVariables subst = rewriteType rewrite
  where
    rewrite recurse typ = case recurse typ of
      TypeVariable name -> case M.lookup name (sUnSubst subst) of
        Just styp -> styp
        Nothing -> typ
      t -> t

-- TODO: remove unused bound type variables
sSubstituteTypeVariablesInScheme :: SSubst -> TypeScheme -> TypeScheme
sSubstituteTypeVariablesInScheme subst (TypeScheme vars typ) = TypeScheme vars $ sSubstituteTypeVariables subst typ

sComposeSubst :: SSubst -> SSubst -> SSubst
sComposeSubst (SSubst firstMap) second@(SSubst secondMap) = SSubst $
  M.union (sSubstituteTypeVariables second <$> firstMap) secondMap

--------------------------------------------------------------------------------
-- Inference

data SInferenceContext
  = SInferenceContext {
    sInferenceContextLexicon :: M.Map Name TypeScheme,
    sInferenceContextVariableCount :: Int,
    sInferenceContextTypingEnvironment :: M.Map Name TypeScheme}
  deriving (Eq, Ord, Show)

data SInferenceResult
  = SInferenceResult {
    sInferenceResultScheme :: TypeScheme,
    sInferenceResultConstraints :: [TypeConstraint]}
  deriving (Eq, Ord)
instance Show SInferenceResult where
  show (SInferenceResult scheme constraints) = "{type= " ++ showTypeScheme scheme ++ ", constraints= " ++ show constraints ++ "}"




sInferFromApplication :: Application -> Flow SInferenceContext SInferenceResult
sInferFromApplication (Application lterm rterm) = bind4 sNewVar sNewVar (sInferTypeInternal lterm) (sInferTypeInternal rterm) withVars
  where
    withVars dom cod lresult rresult = Flows.pure $ SInferenceResult (TypeScheme tvars $ TypeVariable cod) $ [
        TypeConstraint (Types.function (TypeVariable dom) (TypeVariable cod)) ltyp ctx,
        TypeConstraint (TypeVariable dom) rtyp ctx]
        ++ sInferenceResultConstraints lresult ++ sInferenceResultConstraints rresult
      where
        ctx = Just "application"
        ltyp = typeSchemeType $ sInferenceResultScheme lresult
        rtyp = typeSchemeType $ sInferenceResultScheme rresult
        tvars = typeSchemeVariables (sInferenceResultScheme lresult) ++ typeSchemeVariables (sInferenceResultScheme rresult)

sInferFromFunction :: Function -> Flow SInferenceContext SInferenceResult
sInferFromFunction f = case f of
  FunctionLambda (Lambda var _ body) -> Flows.bind sNewVar withVar
    where
      withVar tvar = sWithTypeBinding var (Types.mono $ TypeVariable tvar) $ Flows.map withBodyType (sInferTypeInternal body)
        where
          -- TODO: prove that tvar will never appear in vars
          withBodyType (SInferenceResult (TypeScheme vars t) constraints)
            = SInferenceResult (TypeScheme (tvar:vars) $ Types.function (TypeVariable tvar) t) constraints
  FunctionPrimitive name -> Flow $ \ctx t -> case M.lookup name (sInferenceContextLexicon ctx) of
    Just scheme -> unFlow (Flows.map withoutConstraints $ sInstantiate scheme) ctx t
    Nothing -> unFlow (Flows.fail $ "No such primitive: " ++ unName name) ctx t

-- TODO: propagate rawValueVars and envVars into the final result, possibly after substitution
-- TODO: recursive and mutually recursive let
sInferFromLet :: Let -> Flow SInferenceContext SInferenceResult
sInferFromLet (Let bindings env) = if L.length bindings > 2
    then sInferTypeInternal $ TermLet (Let [L.head bindings] $ TermLet $ Let (L.tail bindings) env)
    else forSingleBinding $ L.head bindings
  where
    forSingleBinding (LetBinding key value _) = Flows.bind sNewVar withVar
      where
        -- Create a temporary type variable for the binding
        withVar var = sWithTypeBinding key (Types.mono $ TypeVariable var) $
            Flows.bind (sInferTypeInternal value) withValueType
          where
            -- Unify and substitute over the value constraints
            -- TODO: save the substitution and pass it along, instead of the original set of constraints
            withValueType (SInferenceResult rawValueScheme valueConstraints) = Flows.bind (TM.fromEither $ uUnify kvConstraints) afterUnification
              where
                rawValueVars = typeSchemeVariables rawValueScheme
                kvConstraints = keyConstraint:valueConstraints
                keyConstraint = TypeConstraint (TypeVariable var) (typeSchemeType rawValueScheme) $ Just "let binding"
                -- Now update the type binding to use the inferred type
                afterUnification subst = sWithTypeBinding key valueScheme
                    $ Flows.map withEnvType (sInferTypeInternal env)
                  where
                    valueScheme = sSubstituteTypeVariablesInScheme subst rawValueScheme
                    withEnvType (SInferenceResult envScheme envConstraints) = SInferenceResult envScheme constraints
                      where
                        constraints = kvConstraints ++ envConstraints
                        envVars = typeSchemeVariables envScheme

sInferFromList :: [Term] -> Flow SInferenceContext SInferenceResult
sInferFromList els = Flows.bind sNewVar withVar
  where
    withVar tvar = if L.null els
        then Flows.pure $ yield (TypeScheme [tvar] $ Types.list $ TypeVariable tvar) []
        else Flows.map fromResults (Flows.sequence (sInferTypeInternal <$> els))
      where
        fromResults results = yield (TypeScheme vars $ Types.list $ TypeVariable tvar) constraints
          where
            uctx = Just "list element"
            constraints = cinner ++ couter
            cinner = L.concat (sInferenceResultConstraints <$> results)
            couter = fmap (\t -> TypeConstraint (TypeVariable tvar) t uctx) types
            types = typeSchemeType . sInferenceResultScheme <$> results
            vars = L.concat (typeSchemeVariables . sInferenceResultScheme <$> results)

sInferFromLiteral :: Literal -> Flow SInferenceContext SInferenceResult
sInferFromLiteral lit = Flows.pure $ yieldWithoutConstraints $ Types.mono $ TypeLiteral $ literalType lit

sInferFromMap :: M.Map Term Term -> Flow SInferenceContext SInferenceResult
sInferFromMap m = Flows.bind sNewVar withKeyVar
  where
    withKeyVar kvar = Flows.bind sNewVar withValueVar
      where
        withValueVar vvar = if M.null m
           then Flows.pure $ yield (TypeScheme [kvar, vvar] $ Types.map (TypeVariable kvar) (TypeVariable vvar)) []
           else Flows.map withResults (Flows.sequence $ fmap fromPair $ M.toList m)
          where
            fromPair (k, v) = Flows.bind (sInferTypeInternal k) withKeyType
              where
                withKeyType (SInferenceResult (TypeScheme kvars kt) kconstraints) = Flows.map withValueType (sInferTypeInternal v)
                  where
                    withValueType (SInferenceResult (TypeScheme vvars vt) vconstraints)
                      = (kvars ++ vvars,
                         [TypeConstraint (TypeVariable kvar) kt $ Just "map key",
                          TypeConstraint (TypeVariable vvar) vt $ Just "map value"]
                          ++ kconstraints ++ vconstraints)
            withResults pairs = yield (TypeScheme (L.concat (fst <$> pairs)) $ Types.map (TypeVariable kvar) (TypeVariable vvar)) $
              L.concat (snd <$> pairs)

sInferFromProduct :: [Term] -> Flow SInferenceContext SInferenceResult
sInferFromProduct els = if L.null els
    then Flows.pure $ yield (Types.mono $ Types.product []) []
    else Flows.map fromResults (Flows.sequence (sInferTypeInternal <$> els))
  where
    fromResults results = yield (TypeScheme tvars $ TypeProduct tbodies) constraints
      where
        tvars = L.concat $ typeSchemeVariables . sInferenceResultScheme <$> results
        tbodies = typeSchemeType . sInferenceResultScheme <$> results
        constraints = L.concat $ sInferenceResultConstraints <$> results

sInferFromVariable :: Name -> Flow SInferenceContext SInferenceResult
sInferFromVariable var = Flow $ \ctx t -> case M.lookup var (sInferenceContextTypingEnvironment ctx) of
  Just scheme -> unFlow (Flows.map withoutConstraints $ sInstantiate scheme) ctx t
  Nothing -> unFlow (Flows.fail $ "Variable not bound to type: " ++ unName var) ctx t

sInferType :: Term -> Flow SInferenceContext TypeScheme
sInferType term = Flows.bind (sInferTypeInternal term) unifyAndSubst
  where
    unifyAndSubst :: SInferenceResult -> Flow SInferenceContext TypeScheme
    unifyAndSubst result = Flows.bind (TM.fromEither $ uUnify $ sInferenceResultConstraints result) doSubst
      where
        doSubst :: SSubst -> Flow SInferenceContext TypeScheme
        doSubst subst = sInstantiateAndNormalize $ sSubstituteTypeVariablesInScheme subst $ sInferenceResultScheme result

sInferTypeInternal :: Term -> Flow SInferenceContext SInferenceResult
sInferTypeInternal term = case term of
--  TermAnnotated ...
  TermApplication a -> sInferFromApplication a
  TermFunction f -> sInferFromFunction f
  TermLet l -> sInferFromLet l
  TermList els -> sInferFromList els
  TermLiteral l -> sInferFromLiteral l
  TermMap m -> sInferFromMap m
--  TermOptional ...
  TermProduct els -> sInferFromProduct els
--  TermRecord ...
--  TermSet ...
--  TermSum ...
--  TermTyped ...
--  TermUnion ...
  TermVariable name -> sInferFromVariable name
--  TermWrap ...

sInstantiate :: TypeScheme -> Flow SInferenceContext TypeScheme
sInstantiate scheme = Flows.map doSubst (sNewVars $ L.length oldVars)
    where
      doSubst newVars = TypeScheme newVars $ sSubstituteTypeVariables subst $ typeSchemeType scheme
        where
          subst = SSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)
      oldVars = L.intersect (L.nub $ typeSchemeVariables scheme) (sFreeTypeVariables $ typeSchemeType scheme)

sInstantiateAndNormalize :: TypeScheme -> Flow SInferenceContext TypeScheme
sInstantiateAndNormalize scheme = Flows.map sNormalizeTypeVariables (sInstantiate scheme)

sFreeTypeVariables :: Type -> [Name]
sFreeTypeVariables typ = case typ of
  TypeFunction (FunctionType dom cod) -> L.nub $ sFreeTypeVariables dom ++ sFreeTypeVariables cod
  TypeList t -> sFreeTypeVariables t
  TypeLiteral _ -> []
  TypeMap (MapType k v) -> L.nub $ sFreeTypeVariables k ++ sFreeTypeVariables v
  TypeProduct types -> L.nub $ L.concat $ sFreeTypeVariables <$> types
  TypeVariable name -> [name]

sNormalizeTypeVariables :: TypeScheme -> TypeScheme
sNormalizeTypeVariables scheme = TypeScheme newVars $ sSubstituteTypeVariables subst $ typeSchemeType scheme
  where
    normalVariables = (\n -> Name $ "t" ++ show n) <$> [0..]
    oldVars = typeSchemeVariables scheme
    newVars = L.take (L.length oldVars) normalVariables
    subst =SSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)

sNewVar :: Flow SInferenceContext Name
sNewVar = Flows.map L.head (sNewVars 1)

sNewVars :: Int -> Flow SInferenceContext [Name]
sNewVars n = Flow helper
  where
    helper ctx t = FlowState value ctx' t
      where
        value = Just ((\n -> Name $ "t" ++ show n) <$> (L.take n [(sInferenceContextVariableCount ctx)..]))
        ctx' = ctx {sInferenceContextVariableCount = n + sInferenceContextVariableCount ctx}

sVarScheme :: Name -> TypeScheme
sVarScheme v = TypeScheme [v] $ TypeVariable v

-- | Temporarily add a (term variable, type scheme) to the typing environment
sWithTypeBinding :: Name -> TypeScheme -> Flow SInferenceContext a -> Flow SInferenceContext a
sWithTypeBinding name scheme f = Flow helper
  where
    helper ctx0 t0 = FlowState e ctx3 t1
      where
        env = sInferenceContextTypingEnvironment ctx0
        ctx1 = ctx0 {sInferenceContextTypingEnvironment = M.insert name scheme env}
        FlowState e ctx2 t1 = unFlow f ctx1 t0
        ctx3 = ctx2 {sInferenceContextTypingEnvironment = env}

unsupported = Flows.fail "Not yet supported"
withoutConstraints scheme = SInferenceResult scheme []
yield = SInferenceResult
yieldWithoutConstraints scheme = yield scheme []

--------------------------------------------------------------------------------
-- Testing


{-

----------------------------------------
-- Unification

_unify (Types.var "a") (Types.var "b")
_unify (Types.var "a") (Types.list $ Types.var "b")

_unify (Types.var "a") Types.int32

_unify (Types.list Types.string) (Types.list $ Types.var "a")

_unify (Types.map (Types.var "a") Types.int32) (Types.map Types.string (Types.var "b"))

sUnifyAll [Types.var "t1" === Types.var "t0", Types.var "t1" === Types.int32]

-- Failure cases

_unify Types.string Types.int32

_unify (Types.var "a") (Types.list $ Types.var "a")

ctx = Just "ctx"
uUnify [(TypeConstraint (Types.var "t1") (Types.var "t0") ctx), (TypeConstraint (Types.var "t1") Types.int32 ctx)]


----------------------------------------
-- Inference

_infer $ _str "hello"

_infer _add

_infer $ _list [_add]

_infer $ _lambda "x" $ _int 42

_infer $ _list [_int 42]




_inferRaw (_list [_int 42])



:module
import Hydra.NewInference

-- System F cases
_inferRaw (lambda "x" $ var "x")                                                           -- (Types.poly ["t0"] $ Types.function (Types.var "t0") (Types.var "t0"))
_inferRaw (int32 32 `with` ["foo">: lambda "x" $ var "x"])                                 -- (Types.mono Types.int32)
_inferRaw ((var "f" @@ int32 0) `with` ["f">: lambda "x" $ var "x"])                       -- (Types.mono Types.int32)
_inferRaw (var "f" `with` ["f">: (lambda "x" $ var "x") @@ int32 0])                       -- (Types.mono Types.int32)
_inferRaw (lambda "x" $ list [var "x"])                                                    -- (Types.poly ["t0"] $ Types.function (Types.var "t0") (Types.list (Types.var "t0")))
_inferRaw (var "sng" `with` ["sng">: lambda "x" $ list [var "x"]])                         -- (Types.poly ["t0"] $ Types.function (Types.var "t0") (Types.list (Types.var "t0")))
_inferRaw ((var "+" @@ (primSucc @@ (primSucc @@ int32 0)) @@ (primSucc @@ int32 0)) `with` ["+">: lambda "x" $ lambda "y" (primSucc @@ (var "+" @@ (primPred @@ var "x") @@ var "y"))]) -- (Types.mono Types.int32)
_inferRaw (var "f" `with` ["f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x")]) -- (Types.poly ["t0"] $ Types.function Types.int32 (Types.function Types.int32 (Types.var "t0")))



_inferRaw (var "f" `with` ["f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x")]) -- (Types.poly ["t0"] $ Types.function Types.int32 (Types.function Types.int32 (Types.var "t0")))



:set +m

constraints = [
  _con (Types.function (Types.var "t5") (Types.var "t6")) (Types.var "t0"),
  _con (Types.var "t5") Types.int32,
  _con (Types.function (Types.var "t3") (Types.var "t4")) (Types.var "t6"),
  _con (Types.var "t3") (Types.var "t1"),
  _con (Types.var "t0") (Types.function (Types.var "t1") (Types.function (Types.var "t2") (Types.var "t4")))]

uUnify constraints




_inferRaw (_lambda "x" $ _list [_var "x", _int 42])

_inferRaw (_lambda "y" (_a (_lambda "x" $ _list [_var "x"]) (_var "y")))




_inferRaw (var "sng" `with` ["sng">: lambda "x" $ list [var "x"]])


----------------------------------------
-- Instantiation

sInstantiate (Types.poly ["t0", "t1"] $ Types.function (Types.var "t1") (Types.var "t1")) sInitialContext



-}
