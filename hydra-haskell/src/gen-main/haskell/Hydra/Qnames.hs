-- | Functions for working with qualified names.

module Hydra.Qnames where

import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

localNameOfEager :: (Core.Name -> String)
localNameOfEager x = (Module.qualifiedNameLocal (qualifyNameEager x))

localNameOfLazy :: (Core.Name -> String)
localNameOfLazy x = (Module.qualifiedNameLocal (qualifyNameLazy x))

namespaceOfEager :: (Core.Name -> Maybe Module.Namespace)
namespaceOfEager x = (Module.qualifiedNameNamespace (qualifyNameEager x))

namespaceOfLazy :: (Core.Name -> Maybe Module.Namespace)
namespaceOfLazy x = (Module.qualifiedNameNamespace (qualifyNameLazy x))

namespaceToFilePath :: (Mantle.CaseConvention -> Module.FileExtension -> Module.Namespace -> String)
namespaceToFilePath caseConv ext ns =  
  let parts = (Lists.map (Formatting.convertCase Mantle.CaseConventionCamel caseConv) (Strings.splitOn "/" (Module.unNamespace ns)))
  in (Strings.cat [
    Strings.cat [
      Strings.intercalate "/" parts,
      "."],
    (Module.unFileExtension ext)])

-- | Construct a qualified (dot-separated) name
qname :: (Module.Namespace -> String -> Core.Name)
qname ns name = (Core.Name (Strings.cat [
  Module.unNamespace ns,
  ".",
  name]))

qualifyNameEager :: (Core.Name -> Module.QualifiedName)
qualifyNameEager name =  
  let parts = (Strings.splitOn "." (Core.unName name))
  in (Logic.ifElse (Module.QualifiedName {
    Module.qualifiedNameNamespace = Nothing,
    Module.qualifiedNameLocal = (Core.unName name)}) (Module.QualifiedName {
    Module.qualifiedNameNamespace = (Just (Module.Namespace (Lists.head parts))),
    Module.qualifiedNameLocal = (Strings.intercalate "." (Lists.tail parts))}) (Equality.equalInt32 1 (Lists.length parts)))

qualifyNameLazy :: (Core.Name -> Module.QualifiedName)
qualifyNameLazy name =  
  let parts = (Lists.reverse (Strings.splitOn "." (Core.unName name)))
  in (Logic.ifElse (Module.QualifiedName {
    Module.qualifiedNameNamespace = Nothing,
    Module.qualifiedNameLocal = (Core.unName name)}) (Module.QualifiedName {
    Module.qualifiedNameNamespace = (Just (Module.Namespace (Strings.intercalate "." (Lists.reverse (Lists.tail parts))))),
    Module.qualifiedNameLocal = (Lists.head parts)}) (Equality.equalInt32 1 (Lists.length parts)))

-- | Convert a qualified name to a dot-separated name
unqualifyName :: (Module.QualifiedName -> Core.Name)
unqualifyName qname =  
  let prefix = ((\x -> case x of
          Nothing -> ""
          Just v1 -> (Strings.cat [
            Module.unNamespace v1,
            "."])) (Module.qualifiedNameNamespace qname))
  in (Core.Name (Strings.cat [
    prefix,
    (Module.qualifiedNameLocal qname)]))