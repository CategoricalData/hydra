-- Note: this is an automatically generated file. Do not edit.
-- | Java naming constants and package name utilities

module Hydra.Java.Names where
import qualified Hydra.Ast as Ast
import qualified Hydra.Classes as Classes
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Java.Syntax as Syntax
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
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
-- | The hydra.core package name
hydraCorePackageName :: Maybe Syntax.PackageName
hydraCorePackageName =
    Just (javaPackageName [
      "hydra",
      "core"])
-- | The hydra.util package name
hydraUtilPackageName :: Maybe Syntax.PackageName
hydraUtilPackageName =
    Just (javaPackageName [
      "hydra",
      "util"])
instanceName :: String
instanceName = "instance"
-- | The java.lang package name
javaLangPackageName :: Maybe Syntax.PackageName
javaLangPackageName =
    Just (javaPackageName [
      "java",
      "lang"])
-- | Construct a Java package name from a list of string parts
javaPackageName :: [String] -> Syntax.PackageName
javaPackageName parts = Syntax.PackageName (Lists.map (\p -> Syntax.Identifier p) parts)
-- | The java.util.function package name
javaUtilFunctionPackageName :: Maybe Syntax.PackageName
javaUtilFunctionPackageName =
    Just (javaPackageName [
      "java",
      "util",
      "function"])
-- | The java.util package name
javaUtilPackageName :: Maybe Syntax.PackageName
javaUtilPackageName =
    Just (javaPackageName [
      "java",
      "util"])
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
