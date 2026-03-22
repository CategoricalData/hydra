-- | C++ naming utilities: encoding Hydra names as C++ names.
-- Provides functions for converting Hydra names to C++ naming conventions.

module Hydra.Ext.Sources.Cpp.Names where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
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
import qualified Hydra.Dsl.Module                     as Module
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
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
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
import qualified Hydra.Ext.Cpp.Syntax as Cpp
import qualified Hydra.Ext.Sources.Cpp.Syntax as CppSyntax
import qualified Hydra.Ext.Sources.Cpp.Utils as CppUtils
import qualified Hydra.Ext.Sources.Cpp.Environment as CppEnvironment
import qualified Hydra.Ext.Sources.Cpp.Language as CppLanguage


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.cpp.names"

cppLanguageNs :: Namespace
cppLanguageNs = Namespace "hydra.ext.cpp.language"

module_ :: Module
module_ = Module ns elements
    [Names.ns, Formatting.ns, cppLanguageNs, CppUtils.ns]
    (CppEnvironment.ns:CppSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "C++ naming utilities: encoding Hydra names as C++ names"
  where
    elements = [
      toBinding className,
      toBinding createTypeReference,
      toBinding encodeEnumValue,
      toBinding encodeFieldName,
      toBinding encodeName,
      toBinding encodeNameQualified,
      toBinding encodeNamespace,
      toBinding encodeTypeVariable,
      toBinding fwdHeaderName,
      toBinding namespaceDecl,
      toBinding partialVisitorName,
      toBinding sanitizeCppName,
      toBinding termVariableReference,
      toBinding typeVariableReference,
      toBinding variableReference,
      toBinding variantName,
      toBinding visitorName]

-- | Get the C++ class name from a Hydra Name
className :: TBinding (Name -> String)
className = def "className" $
  doc "Get the C++ class name from a Hydra Name" $
  lambda "name" $
    sanitizeCppName @@ (Names.localNameOf @@ var "name")

-- | Create a type reference, optionally wrapped in shared_ptr
createTypeReference :: TBinding (Bool -> CppUtils.CppEnvironment -> Name -> Cpp.TypeExpression)
createTypeReference = def "createTypeReference" $
  doc "Create a type reference, optionally wrapped in shared_ptr" $
  lambdas ["isPointer", "env", "name"] $ lets [
    "baseType">:
      inject Cpp._TypeExpression Cpp._TypeExpression_basic $
        inject Cpp._BasicType Cpp._BasicType_named $
          encodeName @@ true @@ Util.caseConventionPascal @@ var "env" @@ var "name"] $
    Logic.ifElse (var "isPointer")
      (CppUtils.toConstType @@ var "baseType")
      (var "baseType")

-- | Encode an enum value with appropriate naming convention
encodeEnumValue :: TBinding (CppUtils.CppEnvironment -> Name -> String)
encodeEnumValue = def "encodeEnumValue" $
  doc "Encode an enum value with appropriate naming convention" $
  encodeName @@ false @@ Util.caseConventionUpperSnake

-- | Encode a field name with appropriate naming convention
encodeFieldName :: TBinding (CppUtils.CppEnvironment -> Name -> String)
encodeFieldName = def "encodeFieldName" $
  doc "Encode a field name with appropriate naming convention" $
  lambdas ["env", "fname"] $
    encodeName @@ false @@ Util.caseConventionLowerSnake @@ var "env" @@ var "fname"

-- | Encode a qualified name with namespace
encodeNameQualified :: TBinding (CppUtils.CppEnvironment -> Name -> String)
encodeNameQualified = def "encodeNameQualified" $
  doc "Encode a qualified name with namespace" $
  lambdas ["env", "name"] $ lets [
    "boundVars">: Pairs.second $
      project CppUtils._CppEnvironment CppUtils._CppEnvironment_boundTypeVariables @@ var "env",
    "focusNs">: Pairs.first $
      Module.namespacesFocus
        (project CppUtils._CppEnvironment CppUtils._CppEnvironment_namespaces @@ var "env"),
    "qualName">: Names.qualifyName @@ var "name",
    "mns">: Module.qualifiedNameNamespace $ var "qualName",
    "local">: Module.qualifiedNameLocal $ var "qualName"] $
    Maybes.maybe
      (Logic.ifElse (Equality.equal (var "mns") (just $ var "focusNs"))
        (sanitizeCppName @@ var "local")
        (Strings.intercalate (string "::")
          (Lists.map sanitizeCppName
            (Strings.splitOn (string ".") (Core.unName $ var "name")))))
      (lambda "n" $ var "n")
      (Maps.lookup (var "name") (var "boundVars"))

-- | Encode a name with specified convention
encodeName :: TBinding (Bool -> CaseConvention -> CppUtils.CppEnvironment -> Name -> String)
encodeName = def "encodeName" $
  doc "Encode a name with specified convention" $
  lambdas ["isQualified", "conv", "env", "name"] $ lets [
    "focusNs">: Pairs.first $
      Module.namespacesFocus
        (project CppUtils._CppEnvironment CppUtils._CppEnvironment_namespaces @@ var "env"),
    "boundVars">: Pairs.second $
      project CppUtils._CppEnvironment CppUtils._CppEnvironment_boundTypeVariables @@ var "env",
    "qualName">: Names.qualifyName @@ var "name",
    "mns">: Module.qualifiedNameNamespace $ var "qualName",
    "local">: Module.qualifiedNameLocal $ var "qualName",
    "cppLocal">: sanitizeCppName @@ (Formatting.convertCase @@ Util.caseConventionCamel @@ var "conv" @@ var "local"),
    "cppNs">: lambda "nsVal" $ Strings.intercalate (string "::")
      (Lists.map (Formatting.convertCase @@ Util.caseConventionCamel @@ Util.caseConventionLowerSnake)
        (Strings.splitOn (string ".") (Module.unNamespace $ var "nsVal")))] $
    Logic.ifElse (var "isQualified")
      (Maybes.maybe
        (Maybes.maybe
          (var "cppLocal")
          (lambda "nsVal" $ Strings.cat2 (var "cppNs" @@ var "nsVal") (Strings.cat2 (string "::") (var "cppLocal")))
          (var "mns"))
        (lambda "n" $ var "n")
        (Maps.lookup (var "name") (var "boundVars")))
      (var "cppLocal")

-- | Encode a namespace as a C++ namespace string
encodeNamespace :: TBinding (Namespace -> String)
encodeNamespace = def "encodeNamespace" $
  doc "Encode a namespace as a C++ namespace string" $
  lambda "nsVal" $
    Strings.intercalate (string "::")
      (Lists.map (Formatting.convertCase @@ Util.caseConventionCamel @@ Util.caseConventionLowerSnake)
        (Strings.splitOn (string ".") (Module.unNamespace $ var "nsVal")))

-- | Encode a type variable name
encodeTypeVariable :: TBinding (Name -> String)
encodeTypeVariable = def "encodeTypeVariable" $
  doc "Encode a type variable name" $
  lambda "name" $
    Formatting.capitalize @@ (Core.unName $ var "name")

-- | Get the forward header name for a namespace
fwdHeaderName :: TBinding (Namespace -> Name)
fwdHeaderName = def "fwdHeaderName" $
  doc "Get the forward header name for a namespace" $
  lambda "nsVal" $
    Names.unqualifyName @@
      (Module.qualifiedName (just $ var "nsVal") (string "Fwd"))

-- | Create a namespace declaration wrapping inner declarations
namespaceDecl :: TBinding (Namespace -> [Cpp.Declaration] -> Cpp.Declaration)
namespaceDecl = def "namespaceDecl" $
  doc "Create a namespace declaration wrapping inner declarations" $
  lambdas ["nsVal", "decls"] $
    inject Cpp._Declaration Cpp._Declaration_namespace $
      record Cpp._NamespaceDeclaration [
        Cpp._NamespaceDeclaration_name>>: encodeNamespace @@ var "nsVal",
        Cpp._NamespaceDeclaration_declarations>>: var "decls"]

-- | Get the partial visitor name for a type
partialVisitorName :: TBinding (Name -> String)
partialVisitorName = def "partialVisitorName" $
  doc "Get the partial visitor name for a type" $
  lambda "name" $
    sanitizeCppName @@ (Strings.cat2 (Names.localNameOf @@ var "name") (string "PartialVisitor"))

-- | Sanitize a name to be valid in C++
sanitizeCppName :: TBinding (String -> String)
sanitizeCppName = def "sanitizeCppName" $
  doc "Sanitize a name to be valid in C++" $
  Formatting.sanitizeWithUnderscores @@ CppLanguage.cppReservedWords

-- | Create a reference to a term variable
termVariableReference :: TBinding (CppUtils.CppEnvironment -> Name -> Cpp.Expression)
termVariableReference = def "termVariableReference" $
  doc "Create a reference to a term variable" $
  variableReference @@ Util.caseConventionLowerSnake

-- | Create a reference to a type variable
typeVariableReference :: TBinding (CppUtils.CppEnvironment -> Name -> Cpp.TypeExpression)
typeVariableReference = def "typeVariableReference" $
  doc "Create a reference to a type variable" $
  lambdas ["env", "name"] $
    inject Cpp._TypeExpression Cpp._TypeExpression_basic $
      inject Cpp._BasicType Cpp._BasicType_named $
        encodeName @@ true @@ Util.caseConventionPascal @@ var "env" @@ var "name"

-- | Create a variable reference expression
variableReference :: TBinding (CaseConvention -> CppUtils.CppEnvironment -> Name -> Cpp.Expression)
variableReference = def "variableReference" $
  doc "Create a variable reference expression" $
  lambdas ["conv", "env", "name"] $
    CppUtils.createIdentifierExpr @@ (encodeName @@ true @@ var "conv" @@ var "env" @@ var "name")

-- | Get the variant name by combining type name and field name
variantName :: TBinding (Name -> Name -> String)
variantName = def "variantName" $
  doc "Get the variant name by combining type name and field name" $
  lambdas ["tname", "fname"] $
    sanitizeCppName @@
      (Strings.cat2 (Names.localNameOf @@ var "tname") (Formatting.capitalize @@ (Core.unName $ var "fname")))

-- | Get the visitor name for a type
visitorName :: TBinding (Name -> String)
visitorName = def "visitorName" $
  doc "Get the visitor name for a type" $
  lambda "name" $
    sanitizeCppName @@ (Strings.cat2 (Names.localNameOf @@ var "name") (string "Visitor"))
