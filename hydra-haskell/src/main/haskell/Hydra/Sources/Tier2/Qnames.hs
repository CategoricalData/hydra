{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Qnames where

-- Standard Tier-2 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All

import qualified Hydra.Dsl.Mantle as Mantle


qnamesDefinition :: String -> TTerm a -> TElement a
qnamesDefinition = definitionInModule hydraQnamesModule

hydraQnamesModule :: Module
hydraQnamesModule = Module (Namespace "hydra.qnames") elements [] [hydraMantleModule, hydraModuleModule] $
    Just ("Functions for working with qualified names.")
  where
   elements = [
     el localNameOfDef,
     el namespaceOfDef,
     el namespaceToFilePathDef,
     el qnameDef,
     el qualifyNameDef,
     el unqualifyNameDef]

localNameOfDef :: TElement (Name -> String)
localNameOfDef = qnamesDefinition "localNameOf" $
  function nameT tString $
  Module.qualifiedNameLocal <.> ref qualifyNameDef

namespaceOfDef :: TElement (Name -> Maybe Namespace)
namespaceOfDef = qnamesDefinition "namespaceOf" $
  function nameT (tOpt namespaceT) $
  Module.qualifiedNameNamespace <.> ref qualifyNameDef

namespaceToFilePathDef :: TElement (CaseConvention -> FileExtension -> Namespace -> String)
namespaceToFilePathDef = qnamesDefinition "namespaceToFilePath" $
  function caseConventionT (tFun fileExtensionT (tFun namespaceT tString)) $
  lambda "caseConv" $ lambda "ext" $ lambda "ns" $
    (((Strings.intercalate "/" $ var "parts") ++ "." ++ (Module.unFileExtension @@ var "ext"))
    `with` [
      "parts">: Lists.map
        (ref convertCaseDef @@ Mantle.caseConventionCamel @@ var "caseConv")
        (Strings.splitOn "." (Core.unNamespace @@ var "ns"))])

qnameDef :: TElement (Namespace -> String -> Name)
qnameDef = qnamesDefinition "qname" $
  doc "Construct a qualified (dot-separated) name" $
  functionN [namespaceT, tString, nameT] $
  lambda "ns" $ lambda "name" $
    nom _Name $
      Strings.cat $
        list [apply (unwrap _Namespace) (var "ns"), string ".", var "name"]

qualifyNameDef :: TElement (Name -> QualifiedName)
qualifyNameDef = qnamesDefinition "qualifyName" $
  function nameT qualifiedNameT $
  lambda "name" $ (Logic.ifElse (Equality.equalInt32 (int32 1) (Lists.length $ var "parts"))
      (Module.qualifiedName nothing (Core.unName @@ var "name"))
      (Module.qualifiedName
        (just $ wrap _Namespace (Strings.intercalate "." (Lists.reverse (Lists.tail $ var "parts"))))
        (Lists.head $ var "parts")))
    `with` [
      "parts">: Lists.reverse (Strings.splitOn "." (Core.unName @@ var "name"))]

unqualifyNameDef :: TElement (QualifiedName -> Name)
unqualifyNameDef = qnamesDefinition "unqualifyName" $
  doc "Convert a qualified name to a dot-separated name" $
  function qualifiedNameT nameT $
  lambda "qname" $ (wrap _Name $ var "prefix" ++ (project _QualifiedName _QualifiedName_local @@ var "qname"))
    `with` [
      "prefix">: matchOpt (string "") (lambda "n" $ (unwrap _Namespace @@ var "n") ++ string ".")
        @@ (project _QualifiedName _QualifiedName_namespace @@ var "qname")]
