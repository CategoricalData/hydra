-- Note: this is an automatically generated file. Do not edit.

-- | Java naming constants and package name utilities

module Hydra.Ext.Java.Names where

import qualified Hydra.Ext.Java.Syntax as Syntax
import qualified Hydra.Lib.Lists as Lists
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

acceptMethodName :: String
acceptMethodName = "accept"

applyMethodName :: String
applyMethodName = "apply"

compareToMethodName :: String
compareToMethodName = "compareTo"

equalsMethodName :: String
equalsMethodName = "equals"

getMethodName :: String
getMethodName = "get"

hashCodeMethodName :: String
hashCodeMethodName = "hashCode"

instanceName :: String
instanceName = "instance"

otherInstanceName :: String
otherInstanceName = "other"

otherwiseMethodName :: String
otherwiseMethodName = "otherwise"

partialVisitorName :: String
partialVisitorName = "PartialVisitor"

setMethodName :: String
setMethodName = "set"

valueFieldName :: String
valueFieldName = "value"

visitMethodName :: String
visitMethodName = "visit"

visitorName :: String
visitorName = "Visitor"

visitorReturnParameter :: String
visitorReturnParameter = "R"

-- | Construct a Java package name from a list of string parts
javaPackageName :: ([String] -> Syntax.PackageName)
javaPackageName parts = (Syntax.PackageName (Lists.map (\p -> Syntax.Identifier p) parts))

-- | The hydra.core package name
hydraCorePackageName :: (Maybe Syntax.PackageName)
hydraCorePackageName = (Just (javaPackageName [
  "hydra",
  "core"]))

-- | The hydra.util package name
hydraUtilPackageName :: (Maybe Syntax.PackageName)
hydraUtilPackageName = (Just (javaPackageName [
  "hydra",
  "util"]))

-- | The java.lang package name
javaLangPackageName :: (Maybe Syntax.PackageName)
javaLangPackageName = (Just (javaPackageName [
  "java",
  "lang"]))

-- | The java.util.function package name
javaUtilFunctionPackageName :: (Maybe Syntax.PackageName)
javaUtilFunctionPackageName = (Just (javaPackageName [
  "java",
  "util",
  "function"]))

-- | The java.util package name
javaUtilPackageName :: (Maybe Syntax.PackageName)
javaUtilPackageName = (Just (javaPackageName [
  "java",
  "util"]))
