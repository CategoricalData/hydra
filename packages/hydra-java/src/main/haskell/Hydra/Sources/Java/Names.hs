-- | Java naming constants and package name utilities.
-- Provides string constants for Java method names, field names, and visitor patterns,
-- as well as package name constructors.

module Hydra.Sources.Java.Names where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Java.Syntax as Java
import qualified Hydra.Sources.Java.Syntax as JavaSyntax


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.java.names"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [],
            moduleTypeDependencies = (JavaSyntax.ns:KernelTypes.kernelTypesNamespaces),
            moduleDescription = Just "Java naming constants and package name utilities"}
  where
    definitions = [
      toDefinition acceptMethodName,
      toDefinition applyMethodName,
      toDefinition compareToMethodName,
      toDefinition equalsMethodName,
      toDefinition getMethodName,
      toDefinition hashCodeMethodName,
      toDefinition hydraCorePackageName,
      toDefinition hydraUtilPackageName,
      toDefinition instanceName,
      toDefinition javaLangPackageName,
      toDefinition javaPackageName,
      toDefinition javaUtilFunctionPackageName,
      toDefinition javaUtilPackageName,
      toDefinition otherInstanceName,
      toDefinition otherwiseMethodName,
      toDefinition partialVisitorName,
      toDefinition setMethodName,
      toDefinition valueFieldName,
      toDefinition visitMethodName,
      toDefinition visitorName,
      toDefinition visitorReturnParameter]

acceptMethodName :: TTermDefinition String
acceptMethodName = def "acceptMethodName" $ string "accept"

applyMethodName :: TTermDefinition String
applyMethodName = def "applyMethodName" $ string "apply"

compareToMethodName :: TTermDefinition String
compareToMethodName = def "compareToMethodName" $ string "compareTo"

equalsMethodName :: TTermDefinition String
equalsMethodName = def "equalsMethodName" $ string "equals"

getMethodName :: TTermDefinition String
getMethodName = def "getMethodName" $ string "get"

hashCodeMethodName :: TTermDefinition String
hashCodeMethodName = def "hashCodeMethodName" $ string "hashCode"

hydraCorePackageName :: TTermDefinition (Maybe Java.PackageName)
hydraCorePackageName = def "hydraCorePackageName" $
  doc "The hydra.core package name" $
  just (javaPackageName @@ list [string "hydra", string "core"])

hydraUtilPackageName :: TTermDefinition (Maybe Java.PackageName)
hydraUtilPackageName = def "hydraUtilPackageName" $
  doc "The hydra.util package name" $
  just (javaPackageName @@ list [string "hydra", string "util"])

instanceName :: TTermDefinition String
instanceName = def "instanceName" $ string "instance"

javaLangPackageName :: TTermDefinition (Maybe Java.PackageName)
javaLangPackageName = def "javaLangPackageName" $
  doc "The java.lang package name" $
  just (javaPackageName @@ list [string "java", string "lang"])

javaPackageName :: TTermDefinition ([String] -> Java.PackageName)
javaPackageName = def "javaPackageName" $
  doc "Construct a Java package name from a list of string parts" $
  "parts" ~>
    wrap Java._PackageName $
      Lists.map ("p" ~> wrap Java._Identifier (var "p")) (var "parts")

javaUtilFunctionPackageName :: TTermDefinition (Maybe Java.PackageName)
javaUtilFunctionPackageName = def "javaUtilFunctionPackageName" $
  doc "The java.util.function package name" $
  just (javaPackageName @@ list [string "java", string "util", string "function"])

javaUtilPackageName :: TTermDefinition (Maybe Java.PackageName)
javaUtilPackageName = def "javaUtilPackageName" $
  doc "The java.util package name" $
  just (javaPackageName @@ list [string "java", string "util"])

otherInstanceName :: TTermDefinition String
otherInstanceName = def "otherInstanceName" $ string "other"

otherwiseMethodName :: TTermDefinition String
otherwiseMethodName = def "otherwiseMethodName" $ string "otherwise"

partialVisitorName :: TTermDefinition String
partialVisitorName = def "partialVisitorName" $ string "PartialVisitor"

setMethodName :: TTermDefinition String
setMethodName = def "setMethodName" $ string "set"

valueFieldName :: TTermDefinition String
valueFieldName = def "valueFieldName" $ string "value"

visitMethodName :: TTermDefinition String
visitMethodName = def "visitMethodName" $ string "visit"

visitorName :: TTermDefinition String
visitorName = def "visitorName" $ string "Visitor"

visitorReturnParameter :: TTermDefinition String
visitorReturnParameter = def "visitorReturnParameter" $ string "R"
