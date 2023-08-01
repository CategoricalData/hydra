module Hydra.Sources.Tier1 where

import Hydra.Kernel
import Hydra.Sources.Compute
import Hydra.Sources.Graph
import Hydra.Sources.Mantle
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Lib.Equality as Equality
import Hydra.Dsl.Lib.Maps as Maps
import Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import Prelude hiding ((++))
import qualified Data.Map as M
import qualified Data.Set as S


tier1Definition :: String -> Datum a -> Definition a
tier1Definition = definitionInModule hydraTier1Module

hydraTier1Module :: Module Kv
hydraTier1Module = Module (Namespace "hydra/tier1") elements [hydraGraphModule, hydraMantleModule, hydraComputeModule] $
    Just ("A module for all tier-1 functions and constants. "
      <> "These are generated functions and constants which DSL functions and the implementations of primitive functions are allowed to depend upon. "
      <> "Higher tiers of generated code may not be depended upon, as these tiers may themselves need to depend on DSL functions or primitive functions.")
  where
   elements = [
     el ignoredVariableDef,
     el placeholderNameDef,
     el skipAnnotationsDef,
     el stripTermDef,
     el stripTypeDef,
     el unqualifyNameDef]

eqA = (M.fromList [(Name "a", S.fromList [TypeClassEquality])])
elementA = Types.apply (TypeVariable _Element) (Types.var "a") :: Type a
fieldA = Types.apply (TypeVariable _Field) (Types.var "a") :: Type a
fieldTypeA = Types.apply (TypeVariable _FieldType) (Types.var "a") :: Type a
graphA = Types.apply (TypeVariable _Graph) (Types.var "a") :: Type a
termA = Types.apply (TypeVariable _Term) (Types.var "a") :: Type a
typeA = Types.apply (TypeVariable _Type) (Types.var "a") :: Type a

ignoredVariableDef :: Definition String
ignoredVariableDef = tier1Definition "ignoredVariable" $
  string "_"

-- localNameOfLazy :: Name -> String
-- localNameOfLazy = qualifiedNameLocal . qualifyNameLazy
--
-- localNameOfEager :: Name -> String
-- localNameOfEager = qualifiedNameLocal . qualifyNameEager
--
-- namespaceOfLazy :: Name -> Maybe Namespace
-- namespaceOfLazy = qualifiedNameNamespace . qualifyNameLazy
--
-- namespaceOfEager :: Name -> Maybe Namespace
-- namespaceOfEager = qualifiedNameNamespace . qualifyNameEager

placeholderNameDef :: Definition Name
placeholderNameDef = tier1Definition "placeholderName" $
  doc "A placeholder name for row types as they are being constructed" $
  wrap _Name $ string "Placeholder"

skipAnnotationsDef :: Definition ((a -> Maybe (Annotated a m)) -> a -> a)
skipAnnotationsDef = tier1Definition "skipAnnotations" $
  function getAnnType (Types.function (Types.var "x") (Types.var "x")) $
  lambda "getAnn" $ lambda "t" $
    (var "skip" @@ var "t") `with` [
      "skip">:
        function (Types.var "x") (Types.var "x") $
        lambda "t1" $
          (matchOpt
            (var "t1")
            (lambda "ann" $ var "skip" @@ (project _Annotated _Annotated_subject @@ var "ann")))
          @@ (var "getAnn" @@ var "t1")]
  where
    getAnnType = (Types.function
      (Types.var "x")
      (Types.optional $ Types.apply (Types.apply (TypeVariable _Annotated) (Types.var "x")) (Types.var "a")))

stripTermDef :: Definition (Term a -> Term a)
stripTermDef = tier1Definition "stripTerm" $
    doc "Strip all annotations from a term" $
    function termA termA $
      lambda "x" (ref skipAnnotationsDef @@ (match _Term (Just nothing) [
        Case _Term_annotated --> lambda "ann" (just $ var "ann")]) @@ var "x")

stripTypeDef :: Definition (Type a -> Type a)
stripTypeDef = tier1Definition "stripType" $
    doc "Strip all annotations from a type" $
    function typeA typeA $
      lambda "x" (ref skipAnnotationsDef @@ (match _Type (Just nothing) [
        Case _Type_annotated --> lambda "ann" (just $ var "ann")]) @@ var "x")
  where
    typeA = Types.apply (TypeVariable _Type) (Types.var "a")

unqualifyNameDef :: Definition (QualifiedName -> Name)
unqualifyNameDef = tier1Definition "unqualifyName" $
  doc "Convert a qualified name to a dot-separated name" $
  lambda "qname" $ (wrap _Name $ var "prefix" ++ (project _QualifiedName _QualifiedName_local @@ var "qname"))
    `with` [
      "prefix">: matchOpt (string "") (lambda "n" $ (unwrap _Namespace @@ var "n") ++ string ".")
        @@ (project _QualifiedName _QualifiedName_namespace @@ var "qname")]
