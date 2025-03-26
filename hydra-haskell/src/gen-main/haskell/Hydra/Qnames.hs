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
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

localNameOf :: (Core.Name -> String)
localNameOf arg_ = (Module.qualifiedNameLocal (qualifyName arg_))

namespaceOf :: (Core.Name -> Maybe Module.Namespace)
namespaceOf arg_ = (Module.qualifiedNameNamespace (qualifyName arg_))

namespaceToFilePath :: (Mantle.CaseConvention -> Module.FileExtension -> Module.Namespace -> String)
namespaceToFilePath caseConv ext ns =  
  let parts = (Lists.map (Formatting.convertCase Mantle.CaseConventionCamel caseConv) (Strings.splitOn "." (Module.unNamespace ns)))
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

qualifyName :: (Core.Name -> Module.QualifiedName)
qualifyName name =  
  let parts = (Lists.reverse (Strings.splitOn "." (Core.unName name)))
  in (Logic.ifElse (Equality.equalInt32 1 (Lists.length parts)) (Module.QualifiedName {
    Module.qualifiedNameNamespace = Nothing,
    Module.qualifiedNameLocal = (Core.unName name)}) (Module.QualifiedName {
    Module.qualifiedNameNamespace = (Just (Module.Namespace (Strings.intercalate "." (Lists.reverse (Lists.tail parts))))),
    Module.qualifiedNameLocal = (Lists.head parts)}))

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
