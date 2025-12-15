-- | Common utilities for language coders, providing shared patterns for term decomposition,
-- environment management, and other cross-language concerns.

module Hydra.Ext.Staging.CoderUtils where

import Hydra.Kernel
import Hydra.Typing
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.List as L
import qualified Data.Map as M


-- | A structured representation of a function term's components, replacing ad-hoc tuples.
-- This captures all the information extracted from peeling lambdas, type lambdas, lets, and
-- type applications from a term.
data FunctionStructure env = FunctionStructure {
  -- | Type parameters (from type lambdas)
  functionStructureTypeParams :: [Name],
  -- | Value parameters (from lambdas)
  functionStructureParams :: [Name],
  -- | Let bindings accumulated from the term
  functionStructureBindings :: [Binding],
  -- | The body term after removing all lambdas, lets, etc.
  functionStructureBody :: Term,
  -- | Domain types of the value parameters
  functionStructureDomains :: [Type],
  -- | The return type of the function (if type inference succeeded)
  functionStructureCodomain :: Maybe Type,
  -- | Updated environment after processing all bindings
  functionStructureEnvironment :: env
}

-- | Analyze a function term by recursively peeling off lambdas, type lambdas, lets, and type applications.
-- This is a common pattern across all language coders: we need to understand the structure of a function
-- to properly encode it in the target language.
--
-- The function:
-- - Collects type parameters from type lambdas
-- - Collects value parameters from lambdas (with their domain types)
-- - Accumulates let bindings
-- - Handles type applications
-- - Returns the simplified body plus all accumulated structure
--
-- This replaces ad-hoc 7-tuple returns with a structured type.
--
-- Parameters:
-- - getTC: Function to get the TypeContext from an environment
-- - setTC: Function to set the TypeContext in an environment
-- - env: The initial environment
-- - term: The term to analyze
analyzeFunctionTerm :: (env -> TypeContext) -> (TypeContext -> env -> env) -> env -> Term -> Flow s (FunctionStructure env)
analyzeFunctionTerm getTC setTC env term = gather True env [] [] [] [] [] term
  where
    gather argMode env tparams args bindings doms tapps term = case deannotateTerm term of
        -- Lambda with typed domain: collect parameter and domain, extend environment
        TermFunction (FunctionLambda lam@(Lambda var (Just dom) body)) -> if argMode
            then gather argMode env2 tparams (var:args) bindings (dom:doms) tapps body
            else finish term
          where
            env2 = setTC (extendTypeContextForLambda (getTC env) lam) env

        -- Lambda with untyped domain: collect parameter but use unit type as placeholder
        -- This is critical for Python test generation where lambdas may lack type annotations
        TermFunction (FunctionLambda lam@(Lambda var Nothing body)) -> if argMode
            then gather argMode env2 tparams (var:args) bindings (TypeVariable (Name "_"):doms) tapps body
            else finish term
          where
            env2 = setTC (extendTypeContextForLambda (getTC env) lam) env

        -- Let: accumulate bindings, extend environment
        TermLet lt@(Let bindings2 body) -> gather False env2 tparams args (bindings ++ bindings2) doms tapps body
          where
            env2 = setTC (extendTypeContextForLet bindingMetadata (getTC env) lt) env

        -- Type application: accumulate type arguments
        TermTypeApplication (TypeApplicationTerm e t) -> gather argMode env tparams args bindings doms (t:tapps) e

        -- Type lambda: collect type parameter, extend environment
        TermTypeLambda tlam@(TypeLambda tvar body) -> gather argMode env2 (tvar:tparams) args bindings doms tapps body
          where
            env2 = setTC (extendTypeContextForTypeLambda (getTC env) tlam) env

        -- Base case: we've reached the body
        t -> finish t
      where
        finish t = do
          -- Reapply type applications to the body
          let t2 = L.foldl (\trm typ -> TermTypeApplication $ TypeApplicationTerm trm typ) t tapps
          -- Try to infer the return type; if it fails, continue without it
          -- This allows encoding of untyped lambdas in dynamically-typed target languages like Python
          mtyp <- tryTypeOf (getTC env) t2
          return $ FunctionStructure {
            functionStructureTypeParams = L.reverse tparams,
            functionStructureParams = L.reverse args,
            functionStructureBindings = bindings,
            functionStructureBody = t2,
            functionStructureDomains = L.reverse doms,
            functionStructureCodomain = mtyp,
            functionStructureEnvironment = env}

-- | Analyze a function term without inferring the return type.
-- This is a performance optimization for dynamically-typed target languages (like Python)
-- where the codomain type is not needed and type inference is expensive.
-- Use this variant when generating test code or when types are not required.
analyzeFunctionTermNoInfer :: (env -> TypeContext) -> (TypeContext -> env -> env) -> env -> Term -> Flow s (FunctionStructure env)
analyzeFunctionTermNoInfer getTC setTC env term = gather True env [] [] [] [] [] term
  where
    gather argMode env tparams args bindings doms tapps term = case deannotateTerm term of
        -- Lambda with typed domain
        TermFunction (FunctionLambda lam@(Lambda var (Just dom) body)) -> if argMode
            then gather argMode env2 tparams (var:args) bindings (dom:doms) tapps body
            else finish term
          where
            env2 = setTC (extendTypeContextForLambda (getTC env) lam) env
        -- Lambda with untyped domain (critical for Python test generation)
        TermFunction (FunctionLambda lam@(Lambda var Nothing body)) -> if argMode
            then gather argMode env2 tparams (var:args) bindings (TypeVariable (Name "_"):doms) tapps body
            else finish term
          where
            env2 = setTC (extendTypeContextForLambda (getTC env) lam) env
        TermLet lt@(Let bindings2 body) -> gather False env2 tparams args (bindings ++ bindings2) doms tapps body
          where
            env2 = setTC (extendTypeContextForLet bindingMetadata (getTC env) lt) env
        TermTypeApplication (TypeApplicationTerm e t) -> gather argMode env tparams args bindings doms (t:tapps) e
        TermTypeLambda tlam@(TypeLambda tvar body) -> gather argMode env2 (tvar:tparams) args bindings doms tapps body
          where
            env2 = setTC (extendTypeContextForTypeLambda (getTC env) tlam) env
        t -> finish t
      where
        finish t = do
          let t2 = L.foldl (\trm typ -> TermTypeApplication $ TypeApplicationTerm trm typ) t tapps
          -- Skip type inference - return Nothing for codomain
          return $ FunctionStructure {
            functionStructureTypeParams = L.reverse tparams,
            functionStructureParams = L.reverse args,
            functionStructureBindings = bindings,
            functionStructureBody = t2,
            functionStructureDomains = L.reverse doms,
            functionStructureCodomain = Nothing,
            functionStructureEnvironment = env}

-- | Try to infer the type of a term, returning Nothing if type inference fails.
-- This is useful for generating code in dynamically-typed languages where
-- type information is optional (e.g., Python).
tryTypeOf :: TypeContext -> Term -> Flow s (Maybe Type)
tryTypeOf tc term = Flow $ \s t ->
  let FlowState mResult s' t' = unFlow (typeOf tc [] term) s t
  in FlowState (Just mResult) s' t'

-- | Produces a simple 'true' value if the binding is complex (needs to be treated as a function)
bindingMetadata :: TypeContext -> Binding -> Maybe Term
bindingMetadata tc b = if isComplexBinding tc b
  then Just Terms.true
  else Nothing

-- | Recursively gather applications from a term, returning the list of arguments
-- and the base term. Applications are traversed left-to-right, with arguments
-- collected in the order they appear (leftmost first).
--
-- For example, given a term representing @f a b c@, this returns @([a, b, c], f)@.
gatherApplications :: Term -> ([Term], Term)
gatherApplications = go []
  where
    go args term = case deannotateTerm term of
      TermApplication (Application l r) -> go (r:args) l
      _ -> (args, term)

-- | Recursively gather applications, type lambdas, and type applications from a term.
-- This is a more comprehensive version of 'gatherApplications' that also strips away
-- type-level constructs.
--
-- Returns a pair of (base term, argument list) where the base term has all applications,
-- type lambdas, and type applications removed, and the argument list contains the
-- collected term arguments.
--
-- For example:
-- - @(f a b)@ returns @(f, [a, b])@
-- - @((\T -> f) a)@ returns @(f, [a])@ (type lambda stripped)
-- - @(f[T] a)@ returns @(f, [a])@ (type application stripped)
gatherArgs :: Term -> [Term] -> (Term, [Term])
gatherArgs term args = case deannotateTerm term of
  TermApplication (Application lhs rhs) -> gatherArgs lhs (rhs:args)
  TermTypeLambda (TypeLambda _ body) -> gatherArgs body args
  TermTypeApplication (TypeApplicationTerm t _) -> gatherArgs t args
  _ -> (term, args)

isComplexBinding :: TypeContext -> Binding -> Bool
isComplexBinding tc b@(Binding _ term (Just ts)) = isPolymorphic || isNonNullary || isComplexTerm tc (bindingTerm b)
  where
    isPolymorphic = not $ L.null $ typeSchemeVariables ts
    isNonNullary = typeArity (typeSchemeType ts) > 0

-- | Determine whether a given term requires needs to be treated as a (possibly nullary) function,
--   rather than a simple value. The term might be an actual function, or it may have type parameters
--   or internal let bindings, or it may reference complex variables.
--   Treating the term as a function enables lazy evaluation as well as encapsulation of complex expressions.
isComplexTerm :: TypeContext -> Term -> Bool
isComplexTerm tc t = case t of
--  TermFunction _ -> True
  TermLet _ -> True
  TermTypeApplication _ -> True
  TermTypeLambda _ -> True
  TermVariable name -> isComplexVariable tc name
  _ -> L.foldl (\b t -> b || isComplexTerm tc t) False $ subterms t

-- | Look up a variable to see if it is bound to a complex term
isComplexVariable :: TypeContext -> Name -> Bool
isComplexVariable tc name = case M.lookup name (typeContextMetadata tc) of
  Just _ -> True
  -- If a variable is not defined in the type context, assume mutual recursion, therefore complex
  _ -> case M.lookup name (typeContextTypes tc) of
    Nothing -> True
    _ -> False

-- | Determines whether a binding represents a function call that needs to be invoked.
-- This heuristic is used to decide whether to use function call syntax in the target language.
--
-- A binding is considered a function call if:
-- - It has arity 0 (no parameters), AND
-- - It was NOT encoded as a simple assignment (i.e., it became a def/function)
--
-- This ensures that bindings encoded as `def x(): ...` are called as `x()` when referenced,
-- while bindings encoded as `x = expr` are referenced as just `x`.
--
-- Note: A binding becomes a function (requiring call syntax) if either:
-- 1. It's not a "simple assignment" (has lambdas, lets, case expressions, etc.), OR
-- 2. It's a non-trivial term (has side effects or could fail at runtime)
isFunctionCall :: Binding -> Bool
isFunctionCall (Binding _ term (Just ts)) =
  let isArityZero = typeSchemeArity ts == 0
      -- A binding needs to be called if it wasn't a simple assignment
      -- (i.e., it was encoded as a function definition)
      notSimple = not (isSimpleAssignment term)
      -- Or if it's not trivial (function applications that could fail)
      notTrivial = not (isTrivialTerm term)
  in isArityZero && (notSimple || notTrivial)
isFunctionCall _ = False

-- | Determines whether a term can be encoded as a simple assignment (without type annotation).
-- This heuristic helps decide whether we need explicit type signatures in the target language.
--
-- A term is considered a simple assignment if:
-- - It's not a lambda, let, or type lambda
-- - It's not a type application (which introduces polymorphism requiring type signatures)
-- - When peeled of applications, it's not a case statement
--
-- Terms that are NOT simple assignments will need type annotations to properly introduce
-- type variables that may be used in cast expressions.
isSimpleAssignment :: Term -> Bool
isSimpleAssignment term = case term of
  TermAnnotated (AnnotatedTerm body _) -> isSimpleAssignment body
  TermFunction (FunctionLambda _) -> False
  TermLet _ -> False
  TermTypeLambda _ -> False
  -- Polymorphic terms should not use simple assignment; they need a type signature for the sake of introducing
  -- type variables which may be used in cast expressions.
  TermTypeApplication (TypeApplicationTerm body _) -> isSimpleAssignment body
  t -> case (fst (gatherArgs t [])) of
    TermFunction (FunctionElimination (EliminationUnion _)) -> False
    _ -> True

-- | Determines whether a term is "trivial" and safe to evaluate eagerly in a let binding.
-- Trivial terms cannot fail at runtime and have no side effects that depend on evaluation order.
--
-- A term is considered trivial if it's:
-- - A literal (int, float, string, bool)
-- - A variable reference
-- - A unit value
-- - A field projection (record.field)
-- - An optional value (Just/Nothing) containing trivial terms
-- - A list literal containing trivial terms
-- - A record literal containing trivial terms
-- - An annotated term wrapping a trivial term
--
-- Non-trivial terms (function applications like head/tail, case expressions) must be wrapped
-- in thunks to preserve lazy evaluation semantics when generating Python code.
isTrivialTerm :: Term -> Bool
isTrivialTerm term = case deannotateTerm term of
  -- Literals are always safe
  TermLiteral _ -> True
  -- Variable references are safe (just name lookups)
  TermVariable _ -> True
  -- Unit is safe
  TermUnit -> True
  -- Wrapped terms (newtypes) - check inner term
  TermWrap (WrappedTerm _ inner) -> isTrivialTerm inner
  -- Optional values - check inner term if present
  TermMaybe Nothing -> True
  TermMaybe (Just inner) -> isTrivialTerm inner
  -- Either values - check inner term
  TermEither (Left inner) -> isTrivialTerm inner
  TermEither (Right inner) -> isTrivialTerm inner
  -- List literals - check all elements
  TermList terms -> all isTrivialTerm terms
  -- Record literals - check all field values
  TermRecord (Record _ fields) -> all (isTrivialTerm . fieldTerm) fields
  -- Injection (union constructor) - check inner term
  TermUnion (Injection _ (Field _ inner)) -> isTrivialTerm inner
  -- Field projection is safe (just accessing a property)
  TermApplication (Application (TermFunction (FunctionElimination (EliminationRecord (Projection _ _)))) arg) ->
    isTrivialTerm arg
  -- All other applications (function calls) are NOT trivial - they could fail
  TermApplication _ -> False
  -- Functions themselves are safe (not evaluated until called)
  TermFunction _ -> True
  -- Let expressions need their own handling
  TermLet _ -> False
  -- Type applications - check inner term
  TermTypeApplication (TypeApplicationTerm inner _) -> isTrivialTerm inner
  -- Type lambdas - check inner term
  TermTypeLambda (TypeLambda _ inner) -> isTrivialTerm inner
  -- Everything else is considered non-trivial to be safe
  _ -> False

-- | Update the metadata portion of a coder state.
-- This is useful for tracking language-specific metadata during code generation.
--
-- The function takes three parameters:
-- - getMeta: Extract metadata from state
-- - makeCoder: Construct state from Graph and metadata
-- - getGraph: Extract Graph from state
-- - f: Transformation to apply to metadata
--
-- Example usage in Python coder:
-- @updateCoderMetadata pyGraphMetadata PyGraph pyGraphGraph f@
updateCoderMetadata :: (state -> metadata) -> (Graph -> metadata -> state) -> (state -> Graph)
                    -> (metadata -> metadata) -> Flow state ()
updateCoderMetadata getMeta makeCoder getGraph f = do
  st <- getState
  putState $ makeCoder (getGraph st) (f $ getMeta st)

-- | Temporarily extend the graph with additional bindings for a computation.
-- The bindings are only visible within the provided flow action.
--
-- This is commonly used when encoding terms that introduce local bindings
-- (like let expressions), where we need those bindings available in the graph
-- for type checking and term resolution.
--
-- Example usage in Python coder:
-- @withGraphBindings pyGraphGraph PyGraph pyGraphMetadata bindings action@
withGraphBindings :: (state -> Graph) -> (Graph -> metadata -> state) -> (state -> metadata)
                  -> [Binding] -> Flow state a -> Flow state a
withGraphBindings getGraph makeCoder getMeta bindings =
  withUpdatedCoderGraph getGraph getMeta makeCoder (extendGraphWithBindings bindings)

-- | Temporarily update the graph for a computation, then restore it.
-- The metadata remains mutable throughout the flow (any changes are preserved).
--
-- This pattern is useful when you need to:
-- - Try encoding with an extended graph but not commit the extensions
-- - Test something in a modified graph context
-- - Keep metadata changes but discard graph changes
--
-- Example usage in Python coder:
-- @withUpdatedCoderGraph pyGraphGraph pyGraphMetadata PyGraph f action@
--
-- Note: This does not use withState, as we want the metadata to be mutable
-- throughout the flow, even though we update the graph temporarily.
withUpdatedCoderGraph :: (state -> Graph) -> (state -> metadata) -> (Graph -> metadata -> state)
                      -> (Graph -> Graph) -> Flow state a -> Flow state a
withUpdatedCoderGraph getGraph getMeta makeCoder f flow = do
  st <- getState
  putState $ makeCoder (f $ getGraph st) (getMeta st)
  r <- flow
  st2 <- getState
  putState $ makeCoder (getGraph st) (getMeta st2)
  return r

-- | Run a Flow Graph computation within a Flow state computation.
-- This allows you to execute graph-level operations (like type inference,
-- term lookups, etc.) while maintaining a richer coder state with metadata.
--
-- The graph changes from the inner computation are preserved, but run in
-- the context of the coder's graph.
--
-- Example usage in Python coder:
-- @inCoderGraphContext pyGraphGraph pyGraphMetadata PyGraph graphAction@
inCoderGraphContext :: (state -> Graph) -> (state -> metadata) -> (Graph -> metadata -> state)
                    -> Flow Graph a -> Flow state a
inCoderGraphContext getGraph getMeta makeCoder f = do
  st <- getState
  (ret, g2) <- withState (getGraph st) $ do
    ret <- f
    g2 <- getState
    return (ret, g2)
  putState $ makeCoder g2 (getMeta st)
  return ret
