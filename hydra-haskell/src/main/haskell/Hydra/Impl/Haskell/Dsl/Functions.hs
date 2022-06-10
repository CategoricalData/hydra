module Hydra.Impl.Haskell.Dsl.Functions where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.PhantomsOld
import qualified Hydra.Impl.Haskell.Dsl.Types as Types


caseField :: FieldName -> Program (a -> b) -> Case (c -> b)
caseField fname (Program term) = Case (Field fname term)

-- Note: Haskell cannot check that the provided cases agree with a particular union type
cases :: Context Meta -> Type Meta -> Type Meta -> [Case (a -> b)] -> Program (a -> b)
cases cx dom cod cs = typed cx (Types.function dom cod) $
    program $ TermExprFunction $ FunctionElimination $ EliminationUnion $ toField <$> cs
  where
    toField (Case field) = field

compareTo :: Program a -> Program (a -> Bool)
compareTo (Program other) = program $ TermExprFunction $ FunctionCompareTo other

deref :: Program (Ref a -> a)
deref = program $ TermExprFunction $ FunctionElimination EliminationElement

lambda :: Var a -> Program b -> Program (a -> b)
lambda (Var v) (Program body) = program $ TermExprFunction $ FunctionLambda $ Lambda (Variable v) body

optionalCases :: Program b -> Program (a -> b) -> Program (Maybe a -> b)
optionalCases (Program nothing) (Program just)
  = program $ TermExprFunction $ FunctionElimination $ EliminationOptional $ OptionalCases nothing just

prim :: Name -> Program a
prim name = program $ TermExprFunction $ FunctionPrimitive name

-- Note: Haskell cannot check that the given field name actually maps a to b
project :: FieldName -> Program (a -> b)
project fname = program $ TermExprFunction $ FunctionElimination $ EliminationRecord fname
