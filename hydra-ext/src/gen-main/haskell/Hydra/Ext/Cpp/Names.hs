-- Note: this is an automatically generated file. Do not edit.

-- | C++ naming utilities: encoding Hydra names as C++ names

module Hydra.Ext.Cpp.Names where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Cpp.Environment as Environment
import qualified Hydra.Ext.Cpp.Language as Language
import qualified Hydra.Ext.Cpp.Syntax as Syntax
import qualified Hydra.Ext.Cpp.Utils as Utils
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Get the C++ class name from a Hydra Name
className :: Core.Name -> String
className name = sanitizeCppName (Names.localNameOf name)

-- | Create a type reference, optionally wrapped in shared_ptr
createTypeReference :: Bool -> Environment.CppEnvironment -> Core.Name -> Syntax.TypeExpression
createTypeReference isPointer env name =

      let baseType = Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (encodeName True Util.CaseConventionPascal env name))
      in (Logic.ifElse isPointer (Utils.toConstType baseType) baseType)

-- | Encode an enum value with appropriate naming convention
encodeEnumValue :: Environment.CppEnvironment -> Core.Name -> String
encodeEnumValue = encodeName False Util.CaseConventionUpperSnake

-- | Encode a field name with appropriate naming convention
encodeFieldName :: Environment.CppEnvironment -> Core.Name -> String
encodeFieldName env fname = encodeName False Util.CaseConventionLowerSnake env fname

-- | Encode a name with specified convention
encodeName :: Bool -> Util.CaseConvention -> Environment.CppEnvironment -> Core.Name -> String
encodeName isQualified conv env name =

      let focusNs = Pairs.first (Module.namespacesFocus (Environment.cppEnvironmentNamespaces env))
          boundVars = Pairs.second (Environment.cppEnvironmentBoundTypeVariables env)
          qualName = Names.qualifyName name
          mns = Module.qualifiedNameNamespace qualName
          local = Module.qualifiedNameLocal qualName
          cppLocal = sanitizeCppName (Formatting.convertCase Util.CaseConventionCamel conv local)
          cppNs =
                  \nsVal -> Strings.intercalate "::" (Lists.map (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake) (Strings.splitOn "." (Module.unNamespace nsVal)))
      in (Logic.ifElse isQualified (Maybes.maybe (Maybes.maybe cppLocal (\nsVal -> Strings.cat2 (cppNs nsVal) (Strings.cat2 "::" cppLocal)) mns) (\n -> n) (Maps.lookup name boundVars)) cppLocal)

-- | Encode a qualified name with namespace
encodeNameQualified :: Environment.CppEnvironment -> Core.Name -> String
encodeNameQualified env name =

      let boundVars = Pairs.second (Environment.cppEnvironmentBoundTypeVariables env)
          focusNs = Pairs.first (Module.namespacesFocus (Environment.cppEnvironmentNamespaces env))
          qualName = Names.qualifyName name
          mns = Module.qualifiedNameNamespace qualName
          local = Module.qualifiedNameLocal qualName
      in (Maybes.maybe (Logic.ifElse (Equality.equal mns (Just focusNs)) (sanitizeCppName local) (Strings.intercalate "::" (Lists.map sanitizeCppName (Strings.splitOn "." (Core.unName name))))) (\n -> n) (Maps.lookup name boundVars))

-- | Encode a namespace as a C++ namespace string
encodeNamespace :: Module.Namespace -> String
encodeNamespace nsVal =
    Strings.intercalate "::" (Lists.map (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake) (Strings.splitOn "." (Module.unNamespace nsVal)))

-- | Encode a type variable name
encodeTypeVariable :: Core.Name -> String
encodeTypeVariable name = Formatting.capitalize (Core.unName name)

-- | Get the forward header name for a namespace
fwdHeaderName :: Module.Namespace -> Core.Name
fwdHeaderName nsVal =
    Names.unqualifyName (Module.QualifiedName {
      Module.qualifiedNameNamespace = (Just nsVal),
      Module.qualifiedNameLocal = "Fwd"})

-- | Create a namespace declaration wrapping inner declarations
namespaceDecl :: Module.Namespace -> [Syntax.Declaration] -> Syntax.Declaration
namespaceDecl nsVal decls =
    Syntax.DeclarationNamespace (Syntax.NamespaceDeclaration {
      Syntax.namespaceDeclarationName = (encodeNamespace nsVal),
      Syntax.namespaceDeclarationDeclarations = decls})

-- | Get the partial visitor name for a type
partialVisitorName :: Core.Name -> String
partialVisitorName name = sanitizeCppName (Strings.cat2 (Names.localNameOf name) "PartialVisitor")

-- | Sanitize a name to be valid in C++
sanitizeCppName :: String -> String
sanitizeCppName = Formatting.sanitizeWithUnderscores Language.cppReservedWords

-- | Create a reference to a term variable
termVariableReference :: Environment.CppEnvironment -> Core.Name -> Syntax.Expression
termVariableReference = variableReference Util.CaseConventionLowerSnake

-- | Create a reference to a type variable
typeVariableReference :: Environment.CppEnvironment -> Core.Name -> Syntax.TypeExpression
typeVariableReference env name =
    Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (encodeName True Util.CaseConventionPascal env name))

-- | Create a variable reference expression
variableReference :: Util.CaseConvention -> Environment.CppEnvironment -> Core.Name -> Syntax.Expression
variableReference conv env name = Utils.createIdentifierExpr (encodeName True conv env name)

-- | Get the variant name by combining type name and field name
variantName :: Core.Name -> Core.Name -> String
variantName tname fname = sanitizeCppName (Strings.cat2 (Names.localNameOf tname) (Formatting.capitalize (Core.unName fname)))

-- | Get the visitor name for a type
visitorName :: Core.Name -> String
visitorName name = sanitizeCppName (Strings.cat2 (Names.localNameOf name) "Visitor")
