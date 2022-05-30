module Hydra.Ext.Java.CoderUtils where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Lib.Strings as Strings
import Hydra.Util.Coders
import Hydra.Ext.Java.Language

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


importAliasesForGraph :: Graph m -> M.Map GraphName Java.PackageName
importAliasesForGraph g = L.foldl addName M.empty $ S.toList deps
  where
    deps = dataGraphDependencies True True True g
    addName m name = M.insert name (graphNameToPackageName name) m
    graphNameToPackageName (GraphName n) = javaPackageName $ Strings.splitOn "/" n

javaPackageDeclaration :: GraphName -> Java.PackageDeclaration
javaPackageDeclaration (GraphName name) = Java.PackageDeclaration [] (Java.Identifier <$> Strings.splitOn "/" name)

javaPackageName :: [String] -> Java.PackageName
javaPackageName parts = Java.PackageName (Java.Identifier <$> parts)

javaClassType :: [Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.ClassType
javaClassType args pkg id = Java.ClassType [] qual (javaTypeIdentifier id) targs
  where
    qual = maybe Java.ClassTypeQualifierNone Java.ClassTypeQualifierPackage pkg
    targs = Java.TypeArgumentReference <$> args

javaConstructorName :: Name -> Java.ClassOrInterfaceTypeToInstantiate
javaConstructorName name = Java.ClassOrInterfaceTypeToInstantiate [id] Nothing
  where
    local = localNameOf name
    id = Java.AnnotatedIdentifier [] $ Java.Identifier $ if S.member local javaReservedWords
      then local ++ "_"
      else local

fieldNameToJavaVariableDeclaratorId :: FieldName -> Java.VariableDeclaratorId
fieldNameToJavaVariableDeclaratorId (FieldName n) = Java.VariableDeclaratorId id Nothing
  where
    id = Java.Identifier $ if S.member n javaReservedWords
      then n ++ "_"
      else n

javaRefType :: [Java.ReferenceType] -> Maybe Java.PackageName -> String -> Java.Type
javaRefType args pkg id = Java.TypeReference $ Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
  javaClassType args pkg id

javaStatementsToBlock :: [Java.Statement] -> Java.Block
javaStatementsToBlock stmts = Java.Block (Java.BlockStatementStatement <$> stmts)

javaEmptyStatement :: Java.Statement
javaEmptyStatement = Java.StatementWithoutTrailing $ Java.StatementWithoutTrailingSubstatementEmpty Java.EmptyStatement

javaReturnStatement :: Y.Maybe (Java.Expression) -> Java.Statement
javaReturnStatement mex = Java.StatementWithoutTrailing $ Java.StatementWithoutTrailingSubstatementReturn $
  Java.ReturnStatement mex

javaTypeIdentifier :: String -> Java.TypeIdentifier
javaTypeIdentifier = Java.TypeIdentifier . Java.Identifier

javaTypeVariable :: String -> Java.ReferenceType
javaTypeVariable v = Java.ReferenceTypeVariable $ Java.TypeVariable [] $ javaTypeIdentifier v

javaUtilPackageName :: Maybe Java.PackageName
javaUtilPackageName = Just $ javaPackageName ["java", "util"]

asJavaReferenceType :: Java.Type -> Result Java.ReferenceType
asJavaReferenceType t = case t of
  Java.TypeReference rt -> pure rt
  _ -> fail $ "expected a Java reference type. Found: " ++ show t

addJavaTypeParameter :: Java.ReferenceType -> Java.Type -> Result Java.Type
addJavaTypeParameter rt t = case t of
  Java.TypeReference (Java.ReferenceTypeClassOrInterface cit) -> case cit of
    Java.ClassOrInterfaceTypeClass (Java.ClassType anns qual id args) -> pure $
      Java.TypeReference $ Java.ReferenceTypeClassOrInterface $
        Java.ClassOrInterfaceTypeClass $ Java.ClassType anns qual id (args ++ [Java.TypeArgumentReference rt])
    _ -> fail $ "expected a Java class type. Found: " ++ show cit
  _ -> fail $ "expected a Java class or interface type. Found: " ++ show t

toJavaArrayType :: Java.Type -> Result Java.Type
toJavaArrayType t = Java.TypeReference . Java.ReferenceTypeArray <$> case t of
  Java.TypeReference rt -> case rt of
    Java.ReferenceTypeClassOrInterface cit -> pure $
      Java.ArrayType (Java.Dims [[]]) $ Java.ArrayType_VariantClassOrInterface cit
    Java.ReferenceTypeArray (Java.ArrayType (Java.Dims d) v) -> pure $
      Java.ArrayType (Java.Dims $ d ++ [[]]) v
    _ -> fail $ "don't know how to make Java reference type into array type: " ++ show rt
  _ -> fail $ "don't know how to make Java type into array type: " ++ show t

nameToJavaReferenceType :: M.Map GraphName Java.PackageName -> Bool -> Name -> Java.ReferenceType
nameToJavaReferenceType aliases qualify name = Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
  nameToJavaClassType aliases qualify name

nameToJavaClassType :: M.Map GraphName Java.PackageName -> Bool -> Name -> Java.ClassType
nameToJavaClassType aliases qualify name = Java.ClassType [] pkg (javaTypeIdentifier local) []
  where
    (gname, local) = toQname name
    pkg = if qualify || S.member local javaReservedWords
      then Y.maybe none Java.ClassTypeQualifierPackage $ M.lookup gname aliases
      else none
    none = Java.ClassTypeQualifierNone
