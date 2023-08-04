module Hydra.Sources.Strip where

import Hydra.Core
import Hydra.Compute
import Hydra.Module
import Hydra.Phantoms
import Hydra.Sources.Core
import Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Set as S


stripDefinition :: String -> Datum a -> Definition a
stripDefinition = definitionInModule hydraStripModule

hydraStripModule :: Module Kv
hydraStripModule = Module (Namespace "hydra/strip") elements [hydraCoreModule] $
    Just "Several functions for stripping annotations from types and terms."
  where
   elements = [
     el skipAnnotationsDef,
     el stripTermDef,
     el stripTypeDef]

skipAnnotationsDef :: Definition ((a -> Maybe (Annotated a m)) -> a -> a)
skipAnnotationsDef = stripDefinition "skipAnnotations" $
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
stripTermDef = stripDefinition "stripTerm" $
    doc "Strip all annotations from a term" $
    function termA termA $
      lambda "x" (ref skipAnnotationsDef @@ (match _Term (Just nothing) [
        Case _Term_annotated --> lambda "ann" (just $ var "ann")]) @@ var "x")

stripTypeDef :: Definition (Type a -> Type a)
stripTypeDef = stripDefinition "stripType" $
    doc "Strip all annotations from a type" $
    function typeA typeA $
      lambda "x" (ref skipAnnotationsDef @@ (match _Type (Just nothing) [
        Case _Type_annotated --> lambda "ann" (just $ var "ann")]) @@ var "x")
  where
    typeA = Types.apply (TypeVariable _Type) (Types.var "a")
