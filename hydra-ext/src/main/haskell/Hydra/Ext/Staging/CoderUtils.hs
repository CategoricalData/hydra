-- | Common utilities for language coders, providing shared patterns for term decomposition,
-- environment management, and other cross-language concerns.

module Hydra.Ext.Staging.CoderUtils (
  -- * Function structure
  FunctionStructure(..),
  analyzeFunctionTerm,

  -- * Term decomposition
  gatherApplications,
  gatherArgs,

  -- * Term classification
  isFunctionCall,
  isSimpleAssignment,

  -- * State management
  updateCoderMetadata,
  withGraphBindings,
  withUpdatedCoderGraph,
  inCoderGraphContext,

  -- * TypeContext management
  withLambdaContext,
  withLetContext,
  withTypeLambdaContext,

  -- * Definitions
  partitionDefinitions,
) where

import Hydra.Kernel
import Hydra.Typing
import qualified Hydra.Show.Core as ShowCore

import qualified Data.List as L


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
  -- | The return type of the function
  functionStructureCodomain :: Type,
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
analyzeFunctionTerm getTC setTC env term = withTrace ("analyzeFunctionTerm for " ++ ShowCore.term term) $
  gather True env [] [] [] [] [] term
  where
    gather argMode env tparams args bindings doms tapps term = case deannotateTerm term of
        -- Lambda: collect parameter and domain, extend environment
        TermFunction (FunctionLambda lam@(Lambda var (Just dom) body)) -> if argMode
            then gather argMode env2 tparams (var:args) bindings (dom:doms) tapps body
            else finish term
          where
            env2 = setTC (extendTypeContextForLambda (getTC env) lam) env

        -- Let: accumulate bindings, extend environment
        TermLet lt@(Let bindings2 body) -> gather False env2 tparams args (bindings ++ bindings2) doms tapps body
          where
            env2 = setTC (extendTypeContextForLet (getTC env) lt) env

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
            -- Infer the return type
            typ <- withTrace ("inferring type for body") $ typeOf (getTC env) [] t2
            return $ FunctionStructure {
              functionStructureTypeParams = L.reverse tparams,
              functionStructureParams = L.reverse args,
              functionStructureBindings = bindings,
              functionStructureBody = t2,
              functionStructureDomains = L.reverse doms,
              functionStructureCodomain = typ,
              functionStructureEnvironment = env
            }

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

-- | Determines whether a binding represents a function call or case statement.
-- This heuristic is used to decide whether to use function call syntax in the target language.
--
-- A binding is considered a function call if:
-- - Its body is a let expression, OR
-- - Its body is a case statement (union elimination)
--
-- Note: This also checks if the type has arity > 0, but that check is currently commented out.
isFunctionCall :: Binding -> Bool
isFunctionCall (Binding name term (Just ts)) =
  let term1 = deannotateAndDetypeTerm term
      -- Only apply these checks for arity-0 functions
      isArityZero = typeSchemeArity ts == 0
      -- Check if it's a wrapped term (e.g., Flow monad) - these are thunks that need to be called
      isWrapped = case term1 of
        TermWrap _ -> True
        _ -> False
      result = isArityZero && isWrapped
  in result
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

partitionDefinitions :: [Definition] -> ([TypeDefinition], [TermDefinition])
partitionDefinitions defs = (L.reverse typeDefsRev, L.reverse termDefsRev)
  where
    (typeDefsRev, termDefsRev) = L.foldl part ([], []) defs
    part (types, terms) def = case def of
      DefinitionType tdef -> (tdef:types, terms)
      DefinitionTerm tdef -> (types, tdef:terms)

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

-- | Process a term within a Lambda's extended TypeContext.
-- This helper abstracts the common pattern of:
-- 1. Extracting the TypeContext from the environment
-- 2. Extending it for the lambda
-- 3. Updating the environment with the new TypeContext
-- 4. Continuing processing with the updated environment
--
-- This pattern is needed by both inline and multiline encoders in Python,
-- and will be needed by the Java coder as well.
--
-- Parameters:
-- - getTC: Extract TypeContext from environment
-- - setTC: Update TypeContext in environment
-- - env: Current environment
-- - lam: Lambda to process
-- - continuation: Action to run with the extended environment
--
-- Example usage in Python coder:
-- @withLambdaContext pythonEnvironmentTypeContext (\\tc e -> e{pythonEnvironmentTypeContext=tc}) env lam $ \\env' -> ...@
withLambdaContext :: (env -> TypeContext) -> (TypeContext -> env -> env)
                  -> env -> Lambda -> (env -> Flow s a) -> Flow s a
withLambdaContext getTC setTC env lam continuation =
  let env' = setTC (extendTypeContextForLambda (getTC env) lam) env
  in continuation env'

-- | Process a term within a Let's extended TypeContext.
-- Similar to 'withLambdaContext', but for Let expressions.
--
-- Example usage in Python coder:
-- @withLetContext pythonEnvironmentTypeContext (\\tc e -> e{pythonEnvironmentTypeContext=tc}) env letrec $ \\env' -> ...@
withLetContext :: (env -> TypeContext) -> (TypeContext -> env -> env)
               -> env -> Let -> (env -> Flow s a) -> Flow s a
withLetContext getTC setTC env letrec continuation =
  let env' = setTC (extendTypeContextForLet (getTC env) letrec) env
  in continuation env'

-- | Process a term within a TypeLambda's extended TypeContext.
-- Similar to 'withLambdaContext', but for TypeLambda expressions.
--
-- Example usage in Python coder:
-- @withTypeLambdaContext pythonEnvironmentTypeContext (\\tc e -> e{pythonEnvironmentTypeContext=tc}) env tlam $ \\env' -> ...@
withTypeLambdaContext :: (env -> TypeContext) -> (TypeContext -> env -> env)
                      -> env -> TypeLambda -> (env -> Flow s a) -> Flow s a
withTypeLambdaContext getTC setTC env tlam continuation =
  let env' = setTC (extendTypeContextForTypeLambda (getTC env) tlam) env
  in continuation env'
