module Hydra.Dsl.Mantle where

import Hydra.Kernel
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Core

import qualified Data.Map as M
import qualified Data.Maybe as Y


termAccessorAnnotatedSubject :: TTerm TermAccessor
termAccessorAnnotatedSubject = unitVariant _TermAccessor _TermAccessor_annotatedSubject

termAccessorApplicationFunction :: TTerm TermAccessor
termAccessorApplicationFunction = unitVariant _TermAccessor _TermAccessor_applicationFunction

termAccessorApplicationArgument :: TTerm TermAccessor
termAccessorApplicationArgument = unitVariant _TermAccessor _TermAccessor_applicationArgument

termAccessorLambdaBody :: TTerm TermAccessor
termAccessorLambdaBody = unitVariant _TermAccessor _TermAccessor_lambdaBody

termAccessorListFold :: TTerm TermAccessor
termAccessorListFold = unitVariant _TermAccessor _TermAccessor_listFold

termAccessorOptionalCasesNothing :: TTerm TermAccessor
termAccessorOptionalCasesNothing = unitVariant _TermAccessor _TermAccessor_optionalCasesNothing

termAccessorOptionalCasesJust :: TTerm TermAccessor
termAccessorOptionalCasesJust = unitVariant _TermAccessor _TermAccessor_optionalCasesJust

termAccessorUnionCasesDefault :: TTerm TermAccessor
termAccessorUnionCasesDefault = unitVariant _TermAccessor _TermAccessor_unionCasesDefault

termAccessorUnionCasesBranch :: TTerm Name -> TTerm TermAccessor
termAccessorUnionCasesBranch = variant _TermAccessor _TermAccessor_unionCasesBranch

termAccessorLetEnvironment :: TTerm TermAccessor
termAccessorLetEnvironment = unitVariant _TermAccessor _TermAccessor_letEnvironment

termAccessorLetBinding :: TTerm Name -> TTerm TermAccessor
termAccessorLetBinding = variant _TermAccessor _TermAccessor_letBinding

termAccessorListElement :: TTerm Int -> TTerm TermAccessor
termAccessorListElement = variant _TermAccessor _TermAccessor_listElement

termAccessorMapKey :: TTerm Int -> TTerm TermAccessor
termAccessorMapKey = variant _TermAccessor _TermAccessor_mapKey

termAccessorMapValue :: TTerm Int -> TTerm TermAccessor
termAccessorMapValue = variant _TermAccessor _TermAccessor_mapValue

termAccessorOptionalTerm :: TTerm TermAccessor
termAccessorOptionalTerm = unitVariant _TermAccessor _TermAccessor_optionalTerm

termAccessorProductTerm :: TTerm Int -> TTerm TermAccessor
termAccessorProductTerm = variant _TermAccessor _TermAccessor_productTerm

termAccessorRecordField :: TTerm Name -> TTerm TermAccessor
termAccessorRecordField = variant _TermAccessor _TermAccessor_recordField

termAccessorSetElement :: TTerm Int -> TTerm TermAccessor
termAccessorSetElement = variant _TermAccessor _TermAccessor_setElement

termAccessorSumTerm :: TTerm TermAccessor
termAccessorSumTerm = unitVariant _TermAccessor _TermAccessor_sumTerm

termAccessorTypeAbstractionBody :: TTerm TermAccessor
termAccessorTypeAbstractionBody = unitVariant _TermAccessor _TermAccessor_typeAbstractionBody

termAccessorTypeApplicationTerm :: TTerm TermAccessor
termAccessorTypeApplicationTerm = unitVariant _TermAccessor _TermAccessor_typeApplicationTerm

termAccessorTypedTerm :: TTerm TermAccessor
termAccessorTypedTerm = unitVariant _TermAccessor _TermAccessor_typedTerm

termAccessorInjectionTerm :: TTerm TermAccessor
termAccessorInjectionTerm = unitVariant _TermAccessor _TermAccessor_injectionTerm

termAccessorWrappedTerm :: TTerm TermAccessor
termAccessorWrappedTerm = unitVariant _TermAccessor _TermAccessor_wrappedTerm
