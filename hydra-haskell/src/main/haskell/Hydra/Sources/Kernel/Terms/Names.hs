
module Hydra.Sources.Kernel.Terms.Names where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  compactName, freshName, freshNames, localNameOf, nameToFilePath, namespaceOf, namespaceToFilePath,
  normalTypeVariable, qname, qualifyName,
  uniqueLabel, unqualifyName)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants  as Constants
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting


ns :: Namespace
ns = Namespace "hydra.names"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Constants.ns, Formatting.ns]
    kernelTypesNamespaces $
    Just ("Functions for working with qualified names.")
  where
   elements = [
     toDefinition compactName,
     toDefinition freshName,
     toDefinition freshNames,
     toDefinition localNameOf,
     toDefinition nameToFilePath,
     toDefinition namespaceOf,
     toDefinition namespaceToFilePath,
     toDefinition normalTypeVariable,
     toDefinition qname,
     toDefinition qualifyName,
     toDefinition uniqueLabel,
     toDefinition unqualifyName]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

compactName :: TTermDefinition (M.Map Namespace String -> Name -> String)
compactName = define "compactName" $
  doc "Given a mapping of namespaces to prefixes, convert a name to a compact string representation" $
  lambda "namespaces" $ lambda "name" $ lets [
    "qualName">: qualifyName @@ var "name",
    "mns">: Module.qualifiedNameNamespace $ var "qualName",
    "local">: Module.qualifiedNameLocal $ var "qualName"]
    $ Maybes.maybe
        (Core.unName $ var "name")
        (lambda "ns" $
          Maybes.maybe (var "local")
            (lambda "pre" $ Strings.cat $ list [var "pre", string ":", var "local"])
            (Maps.lookup (var "ns") (var "namespaces")))
        (var "mns")

localNameOf :: TTermDefinition (Name -> String)
localNameOf = define "localNameOf" $
  doc "Extract the local part of a name" $
  unaryFunction Module.qualifiedNameLocal <.> qualifyName

nameToFilePath :: TTermDefinition (CaseConvention -> CaseConvention -> FileExtension -> Name -> FilePath)
nameToFilePath = define "nameToFilePath" $
  doc "Convert a name to file path, given case conventions for namespaces and local names, and assuming '/' as the file path separator" $
  "nsConv" ~> "localConv" ~> "ext" ~> "name" ~>
  "qualName" <~ qualifyName @@ var "name" $
  "ns" <~ Module.qualifiedNameNamespace (var "qualName") $
  "local" <~ Module.qualifiedNameLocal (var "qualName") $
  "nsToFilePath" <~ ("ns" ~>
    Strings.intercalate (string "/") (Lists.map
      ("part" ~> Formatting.convertCase @@ Util.caseConventionCamel @@ var "nsConv" @@ var "part")
      (Strings.splitOn (string ".") (Module.unNamespace (var "ns"))))) $
  "prefix" <~ Maybes.maybe (string "")
    ("n" ~> Strings.cat2 (var "nsToFilePath" @@ var "n") (string "/"))
    (var "ns") $
  "suffix" <~ Formatting.convertCase @@ Util.caseConventionPascal @@ var "localConv" @@ var "local" $
  Strings.cat (list [var "prefix", var "suffix", string ".", Module.unFileExtension (var "ext")])

namespaceOf :: TTermDefinition (Name -> Maybe Namespace)
namespaceOf = define "namespaceOf" $
  doc "Extract the namespace of a name, if any" $
  unaryFunction Module.qualifiedNameNamespace <.> qualifyName

namespaceToFilePath :: TTermDefinition (CaseConvention -> FileExtension -> Namespace -> String)
namespaceToFilePath = define "namespaceToFilePath" $
  doc "Convert a namespace to a file path with the given case convention and file extension" $
  lambda "caseConv" $ lambda "ext" $ lambda "ns" $ lets [
    "parts">: Lists.map
      (Formatting.convertCase @@ Util.caseConventionCamel @@ var "caseConv")
      (Strings.splitOn (string ".") (Module.unNamespace $ var "ns"))]
    $ (Strings.intercalate (string "/") $ var "parts") ++ string "." ++ (Module.unFileExtension $ var "ext")

qname :: TTermDefinition (Namespace -> String -> Name)
qname = define "qname" $
  doc "Construct a qualified (dot-separated) name" $
  lambda "ns" $ lambda "name" $
    wrap _Name $
      Strings.cat $
        list [apply (unwrap _Namespace) (var "ns"), string ".", var "name"]

qualifyName :: TTermDefinition (Name -> QualifiedName)
qualifyName = define "qualifyName" $
  doc "Split a dot-separated name into a namespace and local name" $
  lambda "name" $ lets [
    "parts">: Lists.reverse (Strings.splitOn (string ".") (Core.unName $ var "name"))]
    $ Logic.ifElse
      (Equality.equal (int32 1) (Lists.length $ var "parts"))
      (Module.qualifiedName nothing (Core.unName $ var "name"))
      (Module.qualifiedName
        (just $ wrap _Namespace (Strings.intercalate (string ".") (Lists.reverse (Lists.tail $ var "parts"))))
        (Lists.head $ var "parts"))

uniqueLabel :: TTermDefinition (S.Set String -> String -> String)
uniqueLabel = define "uniqueLabel" $
  doc "Generate a unique label by appending a suffix if the label is already in use" $
  lambda "visited" $ lambda "l" $
  Logic.ifElse (Sets.member (var "l") (var "visited"))
    (uniqueLabel @@ var "visited" @@ Strings.cat2 (var "l") (string "'"))
    (var "l")

unqualifyName :: TTermDefinition (QualifiedName -> Name)
unqualifyName = define "unqualifyName" $
  doc "Convert a qualified name to a dot-separated name" $
  lambda "qname" $ lets [
    "prefix">: Maybes.maybe
      (string "")
      (lambda "n" $ (unwrap _Namespace @@ var "n") ++ string ".")
      (project _QualifiedName _QualifiedName_namespace @@ var "qname")]
    $ wrap _Name $ var "prefix" ++ (project _QualifiedName _QualifiedName_local @@ var "qname")

freshName :: TTermDefinition (Context -> (Name, Context))
freshName = define "freshName" $
  doc "Generate a fresh type variable name, threading Context" $
  "cx" ~>
  "count" <~ Annotations.getCount @@ Constants.key_freshTypeVariableCount @@ var "cx" $
  pair
    (normalTypeVariable @@ var "count")
    (Annotations.putCount @@ Constants.key_freshTypeVariableCount @@ Math.add (var "count") (int32 1) @@ var "cx")

freshNames :: TTermDefinition (Int -> Context -> ([Name], Context))
freshNames = define "freshNames" $
  doc "Generate multiple fresh type variable names, threading Context" $
  "n" ~> "cx" ~>
  -- Fold over n units, accumulating names and threading context
  "go" <~ ("acc" ~> "_" ~>
    "names" <~ Pairs.first (var "acc") $
    "cx0" <~ Pairs.second (var "acc") $
    "result" <~ freshName @@ var "cx0" $
    "name" <~ Pairs.first (var "result") $
    "cx1" <~ Pairs.second (var "result") $
    pair (Lists.concat2 (var "names") (Lists.pure (var "name"))) (var "cx1")) $
  Lists.foldl (var "go") (pair (list ([] :: [TTerm Name])) (var "cx")) (Lists.replicate (var "n") unit)

normalTypeVariable :: TTermDefinition (Int -> Name)
normalTypeVariable = define "normalTypeVariable" $
  doc "Type variable naming convention follows Haskell: t0, t1, etc." $
  "i" ~> Core.name (Strings.cat2 (string "t") (Literals.showInt32 $ var "i"))
