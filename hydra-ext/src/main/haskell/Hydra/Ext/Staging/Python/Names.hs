module Hydra.Ext.Staging.Python.Names where

import Hydra.Kernel
import qualified Hydra.Ext.Python.Syntax as Py
import qualified Hydra.Ext.Staging.Python.Serde as PySer
import qualified Hydra.Lib.Strings as Strings
import Hydra.Ext.Staging.Python.Utils
import Hydra.Ext.Python.Language
import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Maybe as Y


-- Temporary macros for Python code generation
_useFutureAnnotations_ = True

-- | Target Python version for code generation.
--   Python310 generates code compatible with Python 3.10+ (e.g., for PyPy).
--   Python312 uses PEP 695 type alias syntax (type X = ...).
data PythonVersion = Python310 | Python312
  deriving (Eq, Show)

-- | Current target Python version. Change this to Python310 for PyPy compatibility.
--   Python312 generates the original PEP 695 syntax that passed most tests.
--   Python310 generates backward-compatible syntax for PyPy.
targetPythonVersion :: PythonVersion
targetPythonVersion = Python310

data PythonEnvironment = PythonEnvironment {
  pythonEnvironmentNamespaces :: Namespaces Py.DottedName,
  pythonEnvironmentBoundTypeVariables :: ([Name], M.Map Name Py.Name),
  pythonEnvironmentTypeContext :: TypeContext,
  pythonEnvironmentNUllaryBindings :: S.Set Name,
  pythonEnvironmentVersion :: PythonVersion}

encodeConstantForFieldName :: PythonEnvironment -> Name -> Name -> Py.Name
encodeConstantForFieldName _ tname fname = Py.Name $
  convertCase CaseConventionPascal CaseConventionUpperSnake (localNameOf tname) ++ "__"
  ++ convertCase CaseConventionCamel CaseConventionUpperSnake (unName fname) ++ "__NAME"

encodeConstantForTypeName :: PythonEnvironment -> Name -> Py.Name
encodeConstantForTypeName _ tname = Py.Name $ (convertCase CaseConventionPascal CaseConventionUpperSnake $ localNameOf tname) ++ "__NAME"

encodeEnumValue :: PythonEnvironment -> Name -> Py.Name
encodeEnumValue = encodeName False CaseConventionUpperSnake

encodeFieldName :: PythonEnvironment -> Name -> Py.Name
encodeFieldName env fname = encodeName False CaseConventionLowerSnake env fname

encodeName :: Bool -> CaseConvention -> PythonEnvironment -> Name -> Py.Name
encodeName isQualified conv env name = if isQualified
    then case M.lookup name (snd $ pythonEnvironmentBoundTypeVariables env) of
      Just n -> n
      Nothing -> if mns == Just focusNs
        then Py.Name $ if _useFutureAnnotations_ then pyLocal else PySer.escapePythonString True pyLocal
        else case mns of
          Nothing -> Py.Name pyLocal
          Just ns -> Py.Name $ pyNs ns ++ "." ++ pyLocal
    else Py.Name pyLocal
  where
    focusNs = fst $ namespacesFocus $ pythonEnvironmentNamespaces env
    QualifiedName mns local = qualifyName name
    pyLocal = sanitizePythonName $ convertCase CaseConventionCamel conv local
    pyNs ns = L.intercalate "." $ fmap (convertCase CaseConventionCamel CaseConventionLowerSnake) $ Strings.splitOn "." $ unNamespace ns

encodeNameQualified :: PythonEnvironment -> Name -> Py.Name
encodeNameQualified env name = case M.lookup name (snd $ pythonEnvironmentBoundTypeVariables env) of
    Just n -> n
    Nothing -> if ns == Just focusNs
      then Py.Name $ if _useFutureAnnotations_ then local else PySer.escapePythonString True local
      else Py.Name $ L.intercalate "." (sanitizePythonName <$> (Strings.splitOn "." $ unName name))
  where
    focusNs = fst $ namespacesFocus $ pythonEnvironmentNamespaces env
    QualifiedName ns local = qualifyName name

encodeNamespace :: Namespace -> Py.DottedName
encodeNamespace ns = Py.DottedName (Py.Name . (convertCase CaseConventionCamel CaseConventionLowerSnake) <$> (Strings.splitOn "." $ unNamespace ns))

encodeTypeVariable :: Name -> Py.Name
encodeTypeVariable = Py.Name . capitalize . unName

findNamespaces :: Namespace -> [Definition] -> Namespaces Py.DottedName
findNamespaces focusNs defs = if fst (namespacesFocus namespaces) == coreNs
    then namespaces
    else namespaces {namespacesMapping = M.insert coreNs (encodeNamespace coreNs) $ namespacesMapping namespaces}
  where
    coreNs = Namespace "hydra.core"
    namespaces = namespacesForDefinitions encodeNamespace focusNs defs

sanitizePythonName :: String -> String
sanitizePythonName = sanitizeWithUnderscores pythonReservedWords

termVariableReference :: PythonEnvironment -> Name -> Py.Expression
termVariableReference = variableReference CaseConventionLowerSnake False

typeVariableReference :: PythonEnvironment -> Name -> Py.Expression
typeVariableReference = variableReference CaseConventionPascal False

variantName :: Bool -> PythonEnvironment -> Name -> Name -> Py.Name
variantName isQualified env tname fname = encodeName isQualified CaseConventionPascal env
  $ Name $ unName tname ++ capitalize (unName fname)

variableReference :: CaseConvention -> Bool -> PythonEnvironment -> Name -> Py.Expression
variableReference conv quoted env name = if quoted && sameNamespace
    then doubleQuotedString $ Py.unName pyName
    else unquoted
  where
    pyName = encodeName True conv env name
    unquoted = pyNameToPyExpression pyName
    sameNamespace = case namespaceOf name of
      Nothing -> False
      Just ns -> ns == fst (namespacesFocus $ pythonEnvironmentNamespaces env)
