module Hydra.Prototyping.Helpers (
  apply,
  cases,
  compose,
  constFunction,
  funcRef,
  functionType,
  lambda,
  match,
  matchWithVariants,
  nominalType,
  stringTerm,
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

funcRef :: Element -> Term
funcRef el = apply TermData $ TermElement $ elementName el

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

stringTerm = TermAtomic . AtomicValueString

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


