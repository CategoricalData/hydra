{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Qnames where

-- Standard Tier-2 imports
import qualified Hydra.Dsl.Coders          as Coders
import qualified Hydra.Dsl.Compute         as Compute
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Dsl.Phantoms        as Phantoms
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.TTerms          as TTerms
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

import qualified Hydra.Dsl.Mantle as Mantle


qnamesDefinition :: String -> TTerm a -> TElement a
qnamesDefinition = definitionInModule hydraQnamesModule

hydraQnamesModule :: Module
hydraQnamesModule = Module (Namespace "hydra.qnames") elements
    [hydraFormattingModule] [hydraMantleModule, hydraModuleModule] $
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
  asFunction Module.qualifiedNameLocal <.> ref qualifyNameDef

namespaceOfDef :: TElement (Name -> Maybe Namespace)
namespaceOfDef = qnamesDefinition "namespaceOf" $
  asFunction Module.qualifiedNameNamespace <.> ref qualifyNameDef

namespaceToFilePathDef :: TElement (CaseConvention -> FileExtension -> Namespace -> String)
namespaceToFilePathDef = qnamesDefinition "namespaceToFilePath" $
  lambda "caseConv" $ lambda "ext" $ lambda "ns" $ lets [
    "parts">: Lists.map
      (ref convertCaseDef @@ Mantle.caseConventionCamel @@ var "caseConv")
      (Strings.splitOn "." (Core.unNamespace $ var "ns"))]
    $ (Strings.intercalate "/" $ var "parts") ++ "." ++ (Module.unFileExtension $ var "ext")

qnameDef :: TElement (Namespace -> String -> Name)
qnameDef = qnamesDefinition "qname" $
  doc "Construct a qualified (dot-separated) name" $
  lambda "ns" $ lambda "name" $
    wrap _Name $
      Strings.cat $
        list [apply (unwrap _Namespace) (var "ns"), string ".", var "name"]

qualifyNameDef :: TElement (Name -> QualifiedName)
qualifyNameDef = qnamesDefinition "qualifyName" $
  lambda "name" $ lets [
    "parts">: Lists.reverse (Strings.splitOn "." (Core.unName $ var "name"))]
    $ Logic.ifElse
      (Equality.equalInt32 (int32 1) (Lists.length $ var "parts"))
      (Module.qualifiedName nothing (Core.unName $ var "name"))
      (Module.qualifiedName
        (just $ wrap _Namespace (Strings.intercalate "." (Lists.reverse (Lists.tail $ var "parts"))))
        (Lists.head $ var "parts"))

unqualifyNameDef :: TElement (QualifiedName -> Name)
unqualifyNameDef = qnamesDefinition "unqualifyName" $
  doc "Convert a qualified name to a dot-separated name" $
  lambda "qname" $ lets [
    "prefix">: Optionals.maybe
      (string "")
      (lambda "n" $ (unwrap _Namespace @@ var "n") ++ string ".")
      (project _QualifiedName _QualifiedName_namespace @@ var "qname")]
    $ wrap _Name $ var "prefix" ++ (project _QualifiedName _QualifiedName_local @@ var "qname")
