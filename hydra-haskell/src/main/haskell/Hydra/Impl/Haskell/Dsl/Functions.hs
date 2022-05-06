module Hydra.Impl.Haskell.Dsl.Functions where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types


caseField :: FieldName -> Program (a -> b) -> Case (c -> b)
caseField fname (Program term) = Case (Field fname term)

-- Note: Haskell cannot check that the provided cases agree with a particular union type
cases :: Type Meta -> Type Meta -> [Case (a -> b)] -> Program (a -> b)
cases dom cod cs = typed (Types.function dom cod) $
    program $ ExpressionFunction $ FunctionCases $ toField <$> cs
  where
    toField (Case field) = field

compareTo :: Program a -> Program (a -> Bool)
compareTo (Program other) = program $ ExpressionFunction $ FunctionCompareTo other

deref :: Program (Ref a -> a)
deref = program $ ExpressionFunction $ FunctionData

lambda :: Var a -> Program b -> Program (a -> b)
lambda (Var v) (Program body) = program $ ExpressionFunction $ FunctionLambda $ Lambda v body

optionalCases :: Program b -> Program (a -> b) -> Program (Maybe a -> b)
optionalCases (Program nothing) (Program just)
  = program $ ExpressionFunction $ FunctionOptionalCases $ OptionalCases nothing just

prim :: Name -> Program a
prim name = program $ ExpressionFunction $ FunctionPrimitive name

-- Note: Haskell cannot check that the given field name actually maps a to b
project :: FieldName -> Program (a -> b)
project fname = program $ ExpressionFunction $ FunctionProjection fname
