module Hydra.Prototyping.Helpers (
  apply,
  cases,
  compose,
  constFunction,
  deref,
  funcRef,
  function,
  functionType,
  lambda,
  match,
  matchWithVariants,
  nominalType,
  stringTerm,
  stringType,
  unitTerm,
  unitVariant,
  variable,
  variant,
  withFunction,
  withVariant,
) where

import Hydra.Core
import Hydra.Graph


apply func arg = TermApplication $ Application func arg

cases :: [Field] -> Term
cases = TermCases

compose :: Term -> Term -> Term
compose f2 f1 = lambda var $ apply f2 $ (apply f1 (variable var))
  where var = "x"

constFunction :: Term -> Term
constFunction term = lambda "_" term

deref :: Name -> Term
deref name = apply TermData $ TermElement name
 
funcRef :: Element -> Term
funcRef el = apply TermData $ TermElement $ elementName el

function :: Name -> Term
function = TermFunction

functionType :: Type -> Type -> Type
functionType dom cod = TypeFunction $ FunctionType dom cod

lambda :: Variable -> Term -> Term
lambda param body = TermLambda $ Lambda param body

match :: [(FieldName, Term)] -> Term
match = cases . fmap toField
  where
    toField (name, term) = Field name term

matchWithVariants :: [(FieldName, FieldName)] -> Term
matchWithVariants = cases . fmap toField
  where
    toField (from, to) = Field from $ constFunction $ unitVariant to

nominalType :: Name -> Type
nominalType = TypeNominal

stringTerm :: String -> Term
stringTerm = TermAtomic . AtomicValueString

stringType :: Type
stringType = TypeAtomic AtomicTypeString

unitTerm :: Term
unitTerm = TermRecord []

unitVariant :: FieldName -> Term
unitVariant fname = variant fname unitTerm

variable :: Variable -> Term
variable = TermVariable

variant :: FieldName -> Term -> Term
variant fname term = TermUnion (Field fname term)

withFunction :: FieldName -> Element -> Term
withFunction name el = lambda var $ variant name $ apply (funcRef el) (variable var)
  where var = "x"

withVariant :: FieldName -> Term
withVariant name = constFunction $ unitVariant name
