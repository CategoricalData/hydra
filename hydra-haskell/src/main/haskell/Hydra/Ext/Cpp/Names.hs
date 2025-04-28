-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Cpp.Names where

import Hydra.Kernel
import qualified Hydra.Ext.Cpp.Syntax as Cpp
import qualified Hydra.Ext.Cpp.Serde as CppSer
import qualified Hydra.Lib.Strings as Strings
import Hydra.Ext.Cpp.Utils
import Hydra.Ext.Cpp.Language

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Maybe as Y


className :: Name -> String
className = sanitizeCppName . localNameOf

createTypeReference :: Bool -> CppEnvironment -> Name -> Cpp.TypeExpression
createTypeReference isPointer env name = if isPointer
    then createTemplateType "std::shared_ptr" [toConstType baseType]
    else baseType
  where
    baseType = Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $ encodeName True CaseConventionPascal env name

--createTypeReference :: Bool -> CppEnvironment -> Name -> Cpp.TypeExpression
--createTypeReference isClass env name =
--  Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $ encodeName False CaseConventionPascal env name

-- | Encode an enum value with appropriate naming convention
encodeEnumValue :: CppEnvironment -> Name -> String
encodeEnumValue = encodeName False CaseConventionUpperSnake

-- | Encode a field name with appropriate naming convention
encodeFieldName :: CppEnvironment -> Name -> String
encodeFieldName env fname = encodeName False CaseConventionLowerSnake env fname

-- | Encode a qualified name with namespace
encodeNameQualified :: CppEnvironment -> Name -> String
encodeNameQualified env name = case M.lookup name (snd $ cppEnvironmentBoundTypeVariables env) of
    Just n -> n
    Nothing -> if ns == Just focusNs
      then sanitizeCppName local
      else L.intercalate "::" (sanitizeCppName <$> (Strings.splitOn "." $ unName name))
  where
    focusNs = fst $ namespacesFocus $ cppEnvironmentNamespaces env
    QualifiedName ns local = qualifyName name

-- | Encode a name with specified convention
encodeName :: Bool -> CaseConvention -> CppEnvironment -> Name -> String
encodeName isQualified conv env name = if isQualified
    then case M.lookup name (snd $ cppEnvironmentBoundTypeVariables env) of
      Just n -> n
--      Nothing -> if mns == Just focusNs
--        then cppLocal
--        else case mns of
--          Nothing -> cppLocal
--          Just ns -> cppNs ns ++ "::" ++ cppLocal
      Nothing -> case mns of
          Nothing -> cppLocal
          Just ns -> cppNs ns ++ "::" ++ cppLocal
    else cppLocal
  where
    focusNs = fst $ namespacesFocus $ cppEnvironmentNamespaces env
    QualifiedName mns local = qualifyName name
    cppLocal = sanitizeCppName $ convertCase CaseConventionCamel conv local
    cppNs ns = L.intercalate "::" $ fmap (convertCase CaseConventionCamel CaseConventionLowerSnake) $ Strings.splitOn "." $ unNamespace ns

-- | Encode a namespace as a C++ namespace string
encodeNamespace :: Namespace -> String
encodeNamespace ns = L.intercalate "::" $ (convertCase CaseConventionCamel CaseConventionLowerSnake) <$> (Strings.splitOn "." $ unNamespace ns)

-- | Encode a type variable name
encodeTypeVariable :: Name -> String
encodeTypeVariable = capitalize . unName

partialVisitorName :: Name -> String
partialVisitorName name = sanitizeCppName $ localNameOf name ++ "PartialVisitor"

-- | Sanitize a name to be valid in C++
sanitizeCppName :: String -> String
sanitizeCppName = sanitizeWithUnderscores cppReservedWords

-- | Create a reference to a term variable
termVariableReference :: CppEnvironment -> Name -> Cpp.Expression
termVariableReference = variableReference CaseConventionLowerSnake

-- | Create a reference to a type variable
typeVariableReference :: CppEnvironment -> Name -> Cpp.TypeExpression
typeVariableReference env name = Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $ encodeName True CaseConventionPascal env name

-- | Create a variable reference expression
variableReference :: CaseConvention -> CppEnvironment -> Name -> Cpp.Expression
variableReference conv env name = createIdentifierExpr $ encodeName True conv env name

variantName :: Name -> Name -> String
variantName tname fname = sanitizeCppName $ localNameOf tname ++ capitalize (unName fname)

visitorName :: Name -> String
visitorName name = sanitizeCppName $ localNameOf name ++ "Visitor"
