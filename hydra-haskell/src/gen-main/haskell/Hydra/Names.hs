-- Note: this is an automatically generated file. Do not edit.

-- | Functions for working with qualified names.

module Hydra.Names where

import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Given a mapping of namespaces to prefixes, convert a name to a compact string representation
compactName :: (M.Map Module.Namespace String -> Core.Name -> String)
compactName namespaces name =  
  let qualName = (qualifyName name) 
      mns = (Module.qualifiedNameNamespace qualName)
      local = (Module.qualifiedNameLocal qualName)
  in (Maybes.maybe (Core.unName name) (\ns -> Maybes.maybe local (\pre -> Strings.cat [
    pre,
    ":",
    local]) (Maps.lookup ns namespaces)) mns)

-- | Extract the local part of a name
localNameOf :: (Core.Name -> String)
localNameOf arg_ = (Module.qualifiedNameLocal (qualifyName arg_))

-- | Extract the namespace of a name, if any
namespaceOf :: (Core.Name -> Maybe Module.Namespace)
namespaceOf arg_ = (Module.qualifiedNameNamespace (qualifyName arg_))

-- | Convert a namespace to a file path with the given case convention and file extension
namespaceToFilePath :: (Util.CaseConvention -> Module.FileExtension -> Module.Namespace -> String)
namespaceToFilePath caseConv ext ns =  
  let parts = (Lists.map (Formatting.convertCase Util.CaseConventionCamel caseConv) (Strings.splitOn "." (Module.unNamespace ns)))
  in (Strings.cat2 (Strings.cat2 (Strings.intercalate "/" parts) ".") (Module.unFileExtension ext))

-- | Construct a qualified (dot-separated) name
qname :: (Module.Namespace -> String -> Core.Name)
qname ns name = (Core.Name (Strings.cat [
  Module.unNamespace ns,
  ".",
  name]))

-- | Split a dot-separated name into a namespace and local name
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
  let prefix = (Maybes.maybe "" (\n -> Strings.cat2 (Module.unNamespace n) ".") (Module.qualifiedNameNamespace qname))
  in (Core.Name (Strings.cat2 prefix (Module.qualifiedNameLocal qname)))
