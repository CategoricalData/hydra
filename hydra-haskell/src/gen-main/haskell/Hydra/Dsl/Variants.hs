-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.variants

module Hydra.Dsl.Variants where

import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

eliminationVariantRecord :: Variants.EliminationVariant
eliminationVariantRecord = Variants.EliminationVariantRecord

eliminationVariantUnion :: Variants.EliminationVariant
eliminationVariantUnion = Variants.EliminationVariantUnion

eliminationVariantWrap :: Variants.EliminationVariant
eliminationVariantWrap = Variants.EliminationVariantWrap

functionVariantElimination :: Variants.FunctionVariant
functionVariantElimination = Variants.FunctionVariantElimination

functionVariantLambda :: Variants.FunctionVariant
functionVariantLambda = Variants.FunctionVariantLambda

functionVariantPrimitive :: Variants.FunctionVariant
functionVariantPrimitive = Variants.FunctionVariantPrimitive

literalVariantBinary :: Variants.LiteralVariant
literalVariantBinary = Variants.LiteralVariantBinary

literalVariantBoolean :: Variants.LiteralVariant
literalVariantBoolean = Variants.LiteralVariantBoolean

literalVariantFloat :: Variants.LiteralVariant
literalVariantFloat = Variants.LiteralVariantFloat

literalVariantInteger :: Variants.LiteralVariant
literalVariantInteger = Variants.LiteralVariantInteger

literalVariantString :: Variants.LiteralVariant
literalVariantString = Variants.LiteralVariantString

termVariantAnnotated :: Variants.TermVariant
termVariantAnnotated = Variants.TermVariantAnnotated

termVariantApplication :: Variants.TermVariant
termVariantApplication = Variants.TermVariantApplication

termVariantEither :: Variants.TermVariant
termVariantEither = Variants.TermVariantEither

termVariantFunction :: Variants.TermVariant
termVariantFunction = Variants.TermVariantFunction

termVariantLet :: Variants.TermVariant
termVariantLet = Variants.TermVariantLet

termVariantList :: Variants.TermVariant
termVariantList = Variants.TermVariantList

termVariantLiteral :: Variants.TermVariant
termVariantLiteral = Variants.TermVariantLiteral

termVariantMap :: Variants.TermVariant
termVariantMap = Variants.TermVariantMap

termVariantMaybe :: Variants.TermVariant
termVariantMaybe = Variants.TermVariantMaybe

termVariantPair :: Variants.TermVariant
termVariantPair = Variants.TermVariantPair

termVariantRecord :: Variants.TermVariant
termVariantRecord = Variants.TermVariantRecord

termVariantSet :: Variants.TermVariant
termVariantSet = Variants.TermVariantSet

termVariantTypeApplication :: Variants.TermVariant
termVariantTypeApplication = Variants.TermVariantTypeApplication

termVariantTypeLambda :: Variants.TermVariant
termVariantTypeLambda = Variants.TermVariantTypeLambda

termVariantUnion :: Variants.TermVariant
termVariantUnion = Variants.TermVariantUnion

termVariantUnit :: Variants.TermVariant
termVariantUnit = Variants.TermVariantUnit

termVariantVariable :: Variants.TermVariant
termVariantVariable = Variants.TermVariantVariable

termVariantWrap :: Variants.TermVariant
termVariantWrap = Variants.TermVariantWrap

typeVariantAnnotated :: Variants.TypeVariant
typeVariantAnnotated = Variants.TypeVariantAnnotated

typeVariantApplication :: Variants.TypeVariant
typeVariantApplication = Variants.TypeVariantApplication

typeVariantEither :: Variants.TypeVariant
typeVariantEither = Variants.TypeVariantEither

typeVariantForall :: Variants.TypeVariant
typeVariantForall = Variants.TypeVariantForall

typeVariantFunction :: Variants.TypeVariant
typeVariantFunction = Variants.TypeVariantFunction

typeVariantList :: Variants.TypeVariant
typeVariantList = Variants.TypeVariantList

typeVariantLiteral :: Variants.TypeVariant
typeVariantLiteral = Variants.TypeVariantLiteral

typeVariantMap :: Variants.TypeVariant
typeVariantMap = Variants.TypeVariantMap

typeVariantMaybe :: Variants.TypeVariant
typeVariantMaybe = Variants.TypeVariantMaybe

typeVariantPair :: Variants.TypeVariant
typeVariantPair = Variants.TypeVariantPair

typeVariantRecord :: Variants.TypeVariant
typeVariantRecord = Variants.TypeVariantRecord

typeVariantSet :: Variants.TypeVariant
typeVariantSet = Variants.TypeVariantSet

typeVariantUnion :: Variants.TypeVariant
typeVariantUnion = Variants.TypeVariantUnion

typeVariantUnit :: Variants.TypeVariant
typeVariantUnit = Variants.TypeVariantUnit

typeVariantVariable :: Variants.TypeVariant
typeVariantVariable = Variants.TypeVariantVariable

typeVariantWrap :: Variants.TypeVariant
typeVariantWrap = Variants.TypeVariantWrap
