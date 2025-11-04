{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Names where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes as Maybes
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting


module_ :: Module
module_ = Module (Namespace "hydra.names") elements
    [Formatting.module_]
    kernelTypesModules $
    Just ("Functions for working with qualified names.")
  where
   elements = [
     el compactNameDef,
     el localNameOfDef,
     el namespaceOfDef,
     el namespaceToFilePathDef,
     el qnameDef,
     el qualifyNameDef,
     el uniqueLabelDef,
     el unqualifyNameDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

compactNameDef :: TBinding (M.Map Namespace String -> Name -> String)
compactNameDef = define "compactName" $
  doc "Given a mapping of namespaces to prefixes, convert a name to a compact string representation" $
  lambda "namespaces" $ lambda "name" $ lets [
    "qualName">: ref qualifyNameDef @@ var "name",
    "mns">: Module.qualifiedNameNamespace $ var "qualName",
    "local">: Module.qualifiedNameLocal $ var "qualName"]
    $ Maybes.maybe
        (Core.unName $ var "name")
        (lambda "ns" $
          Maybes.maybe (var "local")
            (lambda "pre" $ Strings.cat $ list [var "pre", string ":", var "local"])
            (Maps.lookup (var "ns") (var "namespaces")))
        (var "mns")

localNameOfDef :: TBinding (Name -> String)
localNameOfDef = define "localNameOf" $
  unaryFunction Module.qualifiedNameLocal <.> ref qualifyNameDef

namespaceOfDef :: TBinding (Name -> Maybe Namespace)
namespaceOfDef = define "namespaceOf" $
  unaryFunction Module.qualifiedNameNamespace <.> ref qualifyNameDef

namespaceToFilePathDef :: TBinding (CaseConvention -> FileExtension -> Namespace -> String)
namespaceToFilePathDef = define "namespaceToFilePath" $
  lambda "caseConv" $ lambda "ext" $ lambda "ns" $ lets [
    "parts">: Lists.map
      (ref Formatting.convertCaseDef @@ Mantle.caseConventionCamel @@ var "caseConv")
      (Strings.splitOn "." (Core.unNamespace $ var "ns"))]
    $ (Strings.intercalate "/" $ var "parts") ++ "." ++ (Module.unFileExtension $ var "ext")

qnameDef :: TBinding (Namespace -> String -> Name)
qnameDef = define "qname" $
  doc "Construct a qualified (dot-separated) name" $
  lambda "ns" $ lambda "name" $
    wrap _Name $
      Strings.cat $
        list [apply (unwrap _Namespace) (var "ns"), string ".", var "name"]

qualifyNameDef :: TBinding (Name -> QualifiedName)
qualifyNameDef = define "qualifyName" $
  lambda "name" $ lets [
    "parts">: Lists.reverse (Strings.splitOn "." (Core.unName $ var "name"))]
    $ Logic.ifElse
      (Equality.equal (int32 1) (Lists.length $ var "parts"))
      (Module.qualifiedName nothing (Core.unName $ var "name"))
      (Module.qualifiedName
        (just $ wrap _Namespace (Strings.intercalate "." (Lists.reverse (Lists.tail $ var "parts"))))
        (Lists.head $ var "parts"))

uniqueLabelDef :: TBinding (S.Set String -> String -> String)
uniqueLabelDef = define "uniqueLabel" $
  doc "Generate a unique label by appending a suffix if the label is already in use" $
  lambda "visited" $ lambda "l" $
  Logic.ifElse (Sets.member (var "l") (var "visited"))
    (ref uniqueLabelDef @@ var "visited" @@ Strings.cat2 (var "l") (string "'"))
    (var "l")

unqualifyNameDef :: TBinding (QualifiedName -> Name)
unqualifyNameDef = define "unqualifyName" $
  doc "Convert a qualified name to a dot-separated name" $
  lambda "qname" $ lets [
    "prefix">: Maybes.maybe
      (string "")
      (lambda "n" $ (unwrap _Namespace @@ var "n") ++ string ".")
      (project _QualifiedName _QualifiedName_namespace @@ var "qname")]
    $ wrap _Name $ var "prefix" ++ (project _QualifiedName _QualifiedName_local @@ var "qname")
