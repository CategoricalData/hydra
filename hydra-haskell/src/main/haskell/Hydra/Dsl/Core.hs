module Hydra.Dsl.Core where

import Hydra.Kernel
import Hydra.Dsl.Base as Base

import qualified Data.Map as M


annotated :: Datum x -> Datum a -> Datum (Annotated x a)
annotated subject annotation = record _Annotated [
    _Annotated_subject>>: subject,
    _Annotated_annotation>>: annotation]

annotatedSubject :: Datum (Annotated x a -> x)
annotatedSubject = project _Annotated _Annotated_subject

annotatedAnnotation :: Datum (Annotated x a -> a)
annotatedAnnotation = project _Annotated _Annotated_annotation

applicationType :: Datum (Type a) -> Datum (Type a) -> Datum (ApplicationType a)
applicationType function argument = record _ApplicationType [
    _ApplicationType_function>>: function,
    _ApplicationType_argument>>: argument]

applicationTypeFunction :: Datum (ApplicationType a -> Type a)
applicationTypeFunction = project _ApplicationType _ApplicationType_function

applicationTypeArgument :: Datum (ApplicationType a -> Type a)
applicationTypeArgument = project _ApplicationType _ApplicationType_argument

functionType :: Datum (Type a) -> Datum (Type a) -> Datum (FunctionType a)
functionType domain codomain = record _FunctionType [
    _FunctionType_domain>>: domain,
    _FunctionType_codomain>>: codomain]

functionTypeDomain :: Datum (FunctionType a -> Type a)
functionTypeDomain = project _FunctionType _FunctionType_domain

functionTypeCodomain :: Datum (FunctionType a -> Type a)
functionTypeCodomain = project _FunctionType _FunctionType_codomain

lambdaType :: Datum Name -> Datum (Type a) -> Datum (LambdaType a)
lambdaType parameter body = record _LambdaType [
    _LambdaType_parameter>>: parameter,
    _LambdaType_body>>: body]

lambdaTypeParameter :: Datum (LambdaType a -> Name)
lambdaTypeParameter = project _LambdaType _LambdaType_parameter

lambdaTypeBody :: Datum (LambdaType a -> Type a)
lambdaTypeBody = project _LambdaType _LambdaType_body
