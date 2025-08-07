-- | Functions for working with qualified names.

module Hydra.Names where

import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Given a mapping of namespaces to prefixes, convert a name to a compact string representation
compactName :: (M.Map Module.Namespace String -> Core.Name -> String)
compactName namespaces name =  
  let qualName = (qualifyName name)
  in  
    let mns = (Module.qualifiedNameNamespace qualName)
    in  
      let local = (Module.qualifiedNameLocal qualName)
      in (Optionals.maybe (Core.unName name) (\ns -> Optionals.maybe local (\pre -> Strings.cat [
        pre,
        ":",
        local]) (Maps.lookup ns namespaces)) mns)

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
  in (Logic.ifElse (Equality.equal 1 (Lists.length parts)) (Module.QualifiedName {
    Module.qualifiedNameNamespace = Nothing,
    Module.qualifiedNameLocal = (Core.unName name)}) (Module.QualifiedName {
    Module.qualifiedNameNamespace = (Just (Module.Namespace (Strings.intercalate "." (Lists.reverse (Lists.tail parts))))),
    Module.qualifiedNameLocal = (Lists.head parts)}))

-- | Generate a unique label by appending a suffix if the label is already in use
uniqueLabel :: (S.Set String -> String -> String)
uniqueLabel visited l = (Logic.ifElse (Sets.member l visited) (uniqueLabel visited (Strings.cat2 l "'")) l)

-- | Convert a qualified name to a dot-separated name
unqualifyName :: (Module.QualifiedName -> Core.Name)
unqualifyName qname =  
  let prefix = (Optionals.maybe "" (\n -> Strings.cat [
          Module.unNamespace n,
          "."]) (Module.qualifiedNameNamespace qname))
  in (Core.Name (Strings.cat [
    prefix,
    (Module.qualifiedNameLocal qname)]))
