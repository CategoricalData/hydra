-- | Java serializer: converts Java AST to concrete syntax (source code).

module Hydra.Ext.Sources.Java.Serde where

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
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as LibLiterals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules  as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms    as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
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
import qualified Hydra.Sources.Kernel.Terms.Monads         as Monads
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
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
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
import Hydra.Ast
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Ext.Sources.Java.Syntax as JavaSyntax


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.java.serde"

module_ :: Module
module_ = Module ns elements
    [Constants.ns, Serialization.ns]
    (JavaSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Java serializer: converts Java AST to concrete syntax"
  where
    elements = [
      -- Unicode escaping helpers
      toBinding hexDigit,
      toBinding padHex4,
      toBinding javaUnicodeEscape,
      toBinding escapeJavaChar,
      toBinding escapeJavaString,
      -- Serialization functions
      toBinding writeAdditionalBound,
      toBinding writeAdditiveExpression,
      toBinding writeAmbiguousName,
      toBinding writeAndExpression,
      toBinding writeAnnotatedIdentifier,
      toBinding writeAnnotation,
      toBinding writeAnnotationTypeDeclaration,
      toBinding writeArrayAccess,
      toBinding writeArrayCreationExpression,
      toBinding writeArrayInitializer,
      toBinding writeArrayType,
      toBinding writeAssertStatement,
      toBinding writeAssignment,
      toBinding writeAssignmentExpression,
      toBinding writeBlock,
      toBinding writeBlockStatement,
      toBinding writeBreakStatement,
      toBinding writeCastExpression,
      toBinding writeCastExpression_Lambda,
      toBinding writeCastExpression_NotPlusMinus,
      toBinding writeCastExpression_RefAndBounds,
      toBinding writeCastExpression_Primitive,
      toBinding writeClassBody,
      toBinding writeClassBodyDeclaration,
      toBinding writeClassBodyDeclarationWithComments,
      toBinding writeClassDeclaration,
      toBinding writeClassInstanceCreationExpression,
      toBinding writeClassInstanceCreationExpression_Qualifier,
      toBinding writeClassLiteral,
      toBinding writeClassMemberDeclaration,
      toBinding writeClassModifier,
      toBinding writeClassOrInterfaceType,
      toBinding writeClassOrInterfaceTypeToInstantiate,
      toBinding writeClassType,
      toBinding writeCompilationUnit,
      toBinding writeConditionalAndExpression,
      toBinding writeConditionalExpression,
      toBinding writeConditionalExpression_TernaryCond,
      toBinding writeConditionalExpression_TernaryLambda,
      toBinding writeConditionalOrExpression,
      toBinding writeConstantDeclaration,
      toBinding writeConstantModifier,
      toBinding writeConstructorBody,
      toBinding writeConstructorDeclaration,
      toBinding writeConstructorDeclarator,
      toBinding writeConstructorModifier,
      toBinding writeContinueStatement,
      toBinding writeDims,
      toBinding writeDoStatement,
      toBinding writeElementValue,
      toBinding writeElementValuePair,
      toBinding writeEnumDeclaration,
      toBinding writeEqualityExpression,
      toBinding writeExclusiveOrExpression,
      toBinding writeExplicitConstructorInvocation,
      toBinding writeExpression,
      toBinding writeExpressionName,
      toBinding writeExpressionStatement,
      toBinding writeFieldAccess,
      toBinding writeFieldDeclaration,
      toBinding writeFieldModifier,
      toBinding writeFloatingPointLiteral,
      toBinding writeFloatingPointType,
      toBinding writeForStatement,
      toBinding writeFormalParameter,
      toBinding writeFormalParameter_Simple,
      toBinding writeIdentifier,
      toBinding writeIfThenStatement,
      toBinding writeIfThenElseStatement,
      toBinding writeImportDeclaration,
      toBinding writeInclusiveOrExpression,
      toBinding writeInstanceInitializer,
      toBinding writeIntegerLiteral,
      toBinding writeIntegralType,
      toBinding writeInterfaceBody,
      toBinding writeInterfaceDeclaration,
      toBinding writeInterfaceMemberDeclaration,
      toBinding writeInterfaceMethodDeclaration,
      toBinding writeInterfaceMethodModifier,
      toBinding writeInterfaceModifier,
      toBinding writeInterfaceType,
      toBinding writeLabeledStatement,
      toBinding writeLambdaBody,
      toBinding writeLambdaExpression,
      toBinding writeLambdaParameters,
      toBinding writeLeftHandSide,
      toBinding writeLiteral,
      toBinding writeLocalVariableDeclaration,
      toBinding writeLocalVariableDeclarationStatement,
      toBinding writeLocalName,
      toBinding writeMarkerAnnotation,
      toBinding writeMethodBody,
      toBinding writeMethodDeclaration,
      toBinding writeMethodDeclarator,
      toBinding writeMethodHeader,
      toBinding writeMethodInvocation,
      toBinding writeMethodModifier,
      toBinding writeMethodName,
      toBinding writeMethodReference,
      toBinding writeMultiplicativeExpression,
      toBinding writeNormalAnnotation,
      toBinding writeNormalClassDeclaration,
      toBinding writeNormalInterfaceDeclaration,
      toBinding writeNumericType,
      toBinding writePackageDeclaration,
      toBinding writePackageName,
      toBinding writePackageOrTypeName,
      toBinding writePackageModifier,
      toBinding writePostDecrementExpression,
      toBinding writePostIncrementExpression,
      toBinding writePostfixExpression,
      toBinding writePreDecrementExpression,
      toBinding writePreIncrementExpression,
      toBinding writePrimary,
      toBinding writePrimaryNoNewArray,
      toBinding writePrimitiveType,
      toBinding writePrimitiveTypeWithAnnotations,
      toBinding writeReceiverParameter,
      toBinding writeReferenceType,
      toBinding writeRelationalExpression,
      toBinding writeRelationalExpression_GreaterThan,
      toBinding writeRelationalExpression_GreaterThanEqual,
      toBinding writeRelationalExpression_InstanceOf,
      toBinding writeRelationalExpression_LessThan,
      toBinding writeRelationalExpression_LessThanEqual,
      toBinding writeResult,
      toBinding writeReturnStatement,
      toBinding writeShiftExpression,
      toBinding writeSimpleTypeName,
      toBinding writeSingleElementAnnotation,
      toBinding writeStatement,
      toBinding writeStatementExpression,
      toBinding writeStatementWithoutTrailingSubstatement,
      toBinding writeStaticInitializer,
      toBinding writeStringLiteral,
      toBinding writeSwitchStatement,
      toBinding writeSynchronizedStatement,
      toBinding writeThrowStatement,
      toBinding writeThrows,
      toBinding writeTryStatement,
      toBinding writeType,
      toBinding writeTypeArgument,
      toBinding writeTypeArgumentsOrDiamond,
      toBinding writeTypeBound,
      toBinding writeTypeDeclaration,
      toBinding writeTypeDeclarationWithComments,
      toBinding writeTypeIdentifier,
      toBinding writeTypeName,
      toBinding writeTypeParameter,
      toBinding writeTypeParameterModifier,
      toBinding writeTypeVariable,
      toBinding writeUnannType,
      toBinding writeUnaryExpression,
      toBinding writeUnaryExpressionNotPlusMinus,
      toBinding writeUnqualifiedClassInstanceCreationExpression,
      toBinding writeVariableArityParameter,
      toBinding writeVariableDeclarator,
      toBinding writeVariableDeclaratorId,
      toBinding writeVariableInitializer,
      toBinding writeVariableModifier,
      toBinding writeWhileStatement,
      toBinding writeWildcard,
      toBinding writeWildcardBounds,
      toBinding sanitizeJavaComment,
      toBinding singleLineComment,
      toBinding withComments]

-- =============================================================================
-- Unicode escaping helpers
-- =============================================================================

-- | Convert a value 0-15 to an uppercase hex digit character code.
hexDigit :: TBinding (Int -> Int)
hexDigit = def "hexDigit" $
  lambda "n" $
    Logic.ifElse (Equality.lt (var "n") (int32 10))
      (Math.add (var "n") (int32 48))   -- '0' = 48
      (Math.add (Math.sub (var "n") (int32 10)) (int32 65))  -- 'A' = 65

-- | Convert an integer to a 4-digit uppercase hex string (zero-padded).
padHex4 :: TBinding (Int -> String)
padHex4 = def "padHex4" $
  lambda "n" $
    "d3" <~ Math.div (var "n") (int32 4096) $     -- n / 16^3
    "r3" <~ Math.mod (var "n") (int32 4096) $
    "d2" <~ Math.div (var "r3") (int32 256) $      -- remainder / 16^2
    "r2" <~ Math.mod (var "r3") (int32 256) $
    "d1" <~ Math.div (var "r2") (int32 16) $       -- remainder / 16
    "d0" <~ Math.mod (var "r2") (int32 16) $        -- remainder
    Strings.fromList (list [hexDigit @@ var "d3", hexDigit @@ var "d2",
                            hexDigit @@ var "d1", hexDigit @@ var "d0"])

-- | Convert an integer to a \\uXXXX escape sequence.
-- For supplementary plane characters (> 0xFFFF), produces a UTF-16 surrogate pair.
javaUnicodeEscape :: TBinding (Int -> String)
javaUnicodeEscape = def "javaUnicodeEscape" $
  lambda "n" $
    Logic.ifElse (Equality.gt (var "n") (int32 65535))
      -- Supplementary plane: UTF-16 surrogate pair
      ("n'" <~ Math.sub (var "n") (int32 65536) $
       "hi" <~ Math.add (int32 55296) (Math.div (var "n'") (int32 1024)) $  -- 0xD800
       "lo" <~ Math.add (int32 56320) (Math.mod (var "n'") (int32 1024)) $  -- 0xDC00
       Strings.cat2 (Strings.cat2 (string "\\u") (padHex4 @@ var "hi"))
                     (Strings.cat2 (string "\\u") (padHex4 @@ var "lo")))
      -- Basic multilingual plane
      (Strings.cat2 (string "\\u") (padHex4 @@ var "n"))

-- | Escape a single character (given as its code point) for use in a Java string or char literal.
escapeJavaChar :: TBinding (Int -> String)
escapeJavaChar = def "escapeJavaChar" $
  lambda "c" $
    Logic.ifElse (Equality.equal (var "c") (int32 34))   -- '"'
      (string "\\\"")
      (Logic.ifElse (Equality.equal (var "c") (int32 92))  -- '\\'
        (string "\\\\")
        (Logic.ifElse (Equality.equal (var "c") (int32 10))  -- '\n'
          (string "\\n")
          (Logic.ifElse (Equality.equal (var "c") (int32 13))  -- '\r'
            (string "\\r")
            (Logic.ifElse (Equality.equal (var "c") (int32 9))  -- '\t'
              (string "\\t")
              (Logic.ifElse (Equality.equal (var "c") (int32 8))  -- '\b'
                (string "\\b")
                (Logic.ifElse (Equality.equal (var "c") (int32 12))  -- '\f'
                  (string "\\f")
                  (Logic.ifElse (Logic.and (Equality.gte (var "c") (int32 32))
                                           (Equality.lt (var "c") (int32 127)))
                    (Strings.fromList (list [var "c"]))
                    (javaUnicodeEscape @@ var "c"))))))))

-- | Escape a string for use in a Java string literal.
escapeJavaString :: TBinding (String -> String)
escapeJavaString = def "escapeJavaString" $
  lambda "s" $
    Strings.cat (Lists.map (lambda "c" $ escapeJavaChar @@ var "c") (Strings.toList (var "s")))

-- =============================================================================
-- Helper functions
-- =============================================================================

sanitizeJavaComment :: TBinding (String -> String)
sanitizeJavaComment = def "sanitizeJavaComment" $
  doc "Sanitize a string for use in a Java comment" $
  lambda "s" $
    Strings.intercalate (string "&gt;")
      (Strings.splitOn (string ">")
        (Strings.intercalate (string "&lt;")
          (Strings.splitOn (string "<") (var "s"))))

singleLineComment :: TBinding (String -> Expr)
singleLineComment = def "singleLineComment" $
  doc "Create a single-line Java comment" $
  lambda "c" $
    Serialization.cst @@ (Strings.cat2 (string "// ") (sanitizeJavaComment @@ var "c"))

withComments :: TBinding (Maybe String -> Expr -> Expr)
withComments = def "withComments" $
  doc "Wrap an expression with optional Javadoc comments" $
  lambda "mc" $ lambda "expr" $
    Maybes.maybe
      (var "expr")
      (lambda "c" $ Serialization.newlineSep @@ list [
        Serialization.cst @@ (Strings.cat2 (string "/**\n")
          (Strings.cat2
            (Strings.intercalate (string "\n")
              (Lists.map (lambda "l" $ Strings.cat2 (string " * ") (var "l"))
                (Strings.lines (sanitizeJavaComment @@ var "c"))))
            (string "\n */"))),
        var "expr"])
      (var "mc")

-- =============================================================================
-- Leaf functions (simple unwrap + cst)
-- =============================================================================

writeIdentifier :: TBinding (Java.Identifier -> Expr)
writeIdentifier = def "writeIdentifier" $
  lambda "id" $
    Serialization.cst @@ (unwrap Java._Identifier @@ var "id")

writeTypeIdentifier :: TBinding (Java.TypeIdentifier -> Expr)
writeTypeIdentifier = def "writeTypeIdentifier" $
  lambda "tid" $
    writeIdentifier @@ (unwrap Java._TypeIdentifier @@ var "tid")

writeSimpleTypeName :: TBinding (Java.SimpleTypeName -> Expr)
writeSimpleTypeName = def "writeSimpleTypeName" $
  lambda "stn" $
    writeTypeIdentifier @@ (unwrap Java._SimpleTypeName @@ var "stn")

writeMethodName :: TBinding (Java.MethodName -> Expr)
writeMethodName = def "writeMethodName" $
  lambda "mn" $
    writeIdentifier @@ (unwrap Java._MethodName @@ var "mn")

writeUnannType :: TBinding (Java.UnannType -> Expr)
writeUnannType = def "writeUnannType" $
  lambda "ut" $
    writeType @@ (unwrap Java._UnannType @@ var "ut")

writeInterfaceType :: TBinding (Java.InterfaceType -> Expr)
writeInterfaceType = def "writeInterfaceType" $
  lambda "it" $
    writeClassType @@ (unwrap Java._InterfaceType @@ var "it")

writePackageModifier :: TBinding (Java.PackageModifier -> Expr)
writePackageModifier = def "writePackageModifier" $
  lambda "pm" $
    writeAnnotation @@ (unwrap Java._PackageModifier @@ var "pm")

writeTypeParameterModifier :: TBinding (Java.TypeParameterModifier -> Expr)
writeTypeParameterModifier = def "writeTypeParameterModifier" $
  lambda "tpm" $
    writeAnnotation @@ (unwrap Java._TypeParameterModifier @@ var "tpm")

-- =============================================================================
-- Stub functions
-- =============================================================================

writeAnnotationTypeDeclaration :: TBinding (Java.AnnotationTypeDeclaration -> Expr)
writeAnnotationTypeDeclaration = def "writeAnnotationTypeDeclaration" $
  lambda "_" $ Serialization.cst @@ string "STUB:AnnotationTypeDeclaration"

writeArrayAccess :: TBinding (Java.ArrayAccess -> Expr)
writeArrayAccess = def "writeArrayAccess" $
  lambda "_" $ Serialization.cst @@ string "STUB:ArrayAccess"

writeAssertStatement :: TBinding (Java.AssertStatement -> Expr)
writeAssertStatement = def "writeAssertStatement" $
  lambda "_" $ Serialization.cst @@ string "STUB:AssertStatement"

writeCastExpression_Lambda :: TBinding (Java.CastExpression_Lambda -> Expr)
writeCastExpression_Lambda = def "writeCastExpression_Lambda" $
  lambda "_" $ Serialization.cst @@ string "STUB:CastExpression_Lambda"

writeClassLiteral :: TBinding (Java.ClassLiteral -> Expr)
writeClassLiteral = def "writeClassLiteral" $
  lambda "_" $ Serialization.cst @@ string "STUB:ClassLiteral"

writeConditionalExpression_TernaryCond :: TBinding (Java.ConditionalExpression_TernaryCond -> Expr)
writeConditionalExpression_TernaryCond = def "writeConditionalExpression_TernaryCond" $
  lambda "_" $ Serialization.cst @@ string "STUB:ConditionalExpression_TernaryCond"

writeConditionalExpression_TernaryLambda :: TBinding (Java.ConditionalExpression_TernaryLambda -> Expr)
writeConditionalExpression_TernaryLambda = def "writeConditionalExpression_TernaryLambda" $
  lambda "_" $ Serialization.cst @@ string "STUB:ConditionalExpression_TernaryLambda"

writeConstantModifier :: TBinding (Java.ConstantModifier -> Expr)
writeConstantModifier = def "writeConstantModifier" $
  lambda "_" $ Serialization.cst @@ string "STUB:ConstantModifier"

writeDoStatement :: TBinding (Java.DoStatement -> Expr)
writeDoStatement = def "writeDoStatement" $
  lambda "_" $ Serialization.cst @@ string "STUB:DoStatement"

writeEnumDeclaration :: TBinding (Java.EnumDeclaration -> Expr)
writeEnumDeclaration = def "writeEnumDeclaration" $
  lambda "_" $ Serialization.cst @@ string "STUB:EnumDeclaration"

writeExplicitConstructorInvocation :: TBinding (Java.ExplicitConstructorInvocation -> Expr)
writeExplicitConstructorInvocation = def "writeExplicitConstructorInvocation" $
  lambda "_" $ Serialization.cst @@ string "STUB:ExplicitConstructorInvocation"

writeForStatement :: TBinding (Java.ForStatement -> Expr)
writeForStatement = def "writeForStatement" $
  lambda "_" $ Serialization.cst @@ string "STUB:ForStatement"

writeIfThenElseStatement :: TBinding (Java.IfThenElseStatement -> Expr)
writeIfThenElseStatement = def "writeIfThenElseStatement" $
  lambda "_" $ Serialization.cst @@ string "STUB:IfThenElseStatement"

writeInstanceInitializer :: TBinding (Java.InstanceInitializer -> Expr)
writeInstanceInitializer = def "writeInstanceInitializer" $
  lambda "_" $ Serialization.cst @@ string "STUB:InstanceInitializer"

writeLabeledStatement :: TBinding (Java.LabeledStatement -> Expr)
writeLabeledStatement = def "writeLabeledStatement" $
  lambda "_" $ Serialization.cst @@ string "STUB:LabeledStatement"

writeMethodReference :: TBinding (Java.MethodReference -> Expr)
writeMethodReference = def "writeMethodReference" $
  lambda "_" $ Serialization.cst @@ string "STUB:MethodReference"

writePostDecrementExpression :: TBinding (Java.PostDecrementExpression -> Expr)
writePostDecrementExpression = def "writePostDecrementExpression" $
  lambda "_" $ Serialization.cst @@ string "STUB:PostDecrementExpression"

writePostIncrementExpression :: TBinding (Java.PostIncrementExpression -> Expr)
writePostIncrementExpression = def "writePostIncrementExpression" $
  lambda "_" $ Serialization.cst @@ string "STUB:PostIncrementExpression"

writePreDecrementExpression :: TBinding (Java.PreDecrementExpression -> Expr)
writePreDecrementExpression = def "writePreDecrementExpression" $
  lambda "_" $ Serialization.cst @@ string "STUB:PreDecrementExpression"

writePreIncrementExpression :: TBinding (Java.PreIncrementExpression -> Expr)
writePreIncrementExpression = def "writePreIncrementExpression" $
  lambda "_" $ Serialization.cst @@ string "STUB:PreIncrementExpression"

writeReceiverParameter :: TBinding (Java.ReceiverParameter -> Expr)
writeReceiverParameter = def "writeReceiverParameter" $
  lambda "_" $ Serialization.cst @@ string "STUB:ReceiverParameter"

writeStaticInitializer :: TBinding (Java.StaticInitializer -> Expr)
writeStaticInitializer = def "writeStaticInitializer" $
  lambda "_" $ Serialization.cst @@ string "STUB:StaticInitializer"

writeSwitchStatement :: TBinding (Java.SwitchStatement -> Expr)
writeSwitchStatement = def "writeSwitchStatement" $
  lambda "_" $ Serialization.cst @@ string "STUB:SwitchStatement"

writeSynchronizedStatement :: TBinding (Java.SynchronizedStatement -> Expr)
writeSynchronizedStatement = def "writeSynchronizedStatement" $
  lambda "_" $ Serialization.cst @@ string "STUB:SynchronizedStatement"

writeThrows :: TBinding (Java.Throws -> Expr)
writeThrows = def "writeThrows" $
  lambda "_" $ Serialization.cst @@ string "STUB:Throws"

writeTryStatement :: TBinding (Java.TryStatement -> Expr)
writeTryStatement = def "writeTryStatement" $
  lambda "_" $ Serialization.cst @@ string "STUB:TryStatement"

writeVariableArityParameter :: TBinding (Java.VariableArityParameter -> Expr)
writeVariableArityParameter = def "writeVariableArityParameter" $
  lambda "_" $ Serialization.cst @@ string "STUB:VariableArityParameter"

writeWhileStatement :: TBinding (Java.WhileStatement -> Expr)
writeWhileStatement = def "writeWhileStatement" $
  lambda "_" $ Serialization.cst @@ string "STUB:WhileStatement"

-- =============================================================================
-- Simple union dispatch functions
-- =============================================================================

writeType :: TBinding (Java.Type -> Expr)
writeType = def "writeType" $
  lambda "t" $
    cases Java._Type (var "t") Nothing [
      Java._Type_primitive>>: lambda "pt" $ writePrimitiveTypeWithAnnotations @@ var "pt",
      Java._Type_reference>>: lambda "rt" $ writeReferenceType @@ var "rt"]

writeAnnotation :: TBinding (Java.Annotation -> Expr)
writeAnnotation = def "writeAnnotation" $
  lambda "ann" $
    cases Java._Annotation (var "ann") Nothing [
      Java._Annotation_normal>>: lambda "n" $ writeNormalAnnotation @@ var "n",
      Java._Annotation_marker>>: lambda "m" $ writeMarkerAnnotation @@ var "m",
      Java._Annotation_singleElement>>: lambda "s" $ writeSingleElementAnnotation @@ var "s"]

writePrimitiveType :: TBinding (Java.PrimitiveType -> Expr)
writePrimitiveType = def "writePrimitiveType" $
  lambda "pt" $
    cases Java._PrimitiveType (var "pt") Nothing [
      Java._PrimitiveType_numeric>>: lambda "nt" $ writeNumericType @@ var "nt",
      Java._PrimitiveType_boolean>>: constant $ Serialization.cst @@ string "boolean"]

writeNumericType :: TBinding (Java.NumericType -> Expr)
writeNumericType = def "writeNumericType" $
  lambda "nt" $
    cases Java._NumericType (var "nt") Nothing [
      Java._NumericType_integral>>: lambda "it" $ writeIntegralType @@ var "it",
      Java._NumericType_floatingPoint>>: lambda "ft" $ writeFloatingPointType @@ var "ft"]

writeIntegralType :: TBinding (Java.IntegralType -> Expr)
writeIntegralType = def "writeIntegralType" $
  lambda "t" $
    cases Java._IntegralType (var "t") Nothing [
      Java._IntegralType_byte>>: constant $ Serialization.cst @@ string "byte",
      Java._IntegralType_short>>: constant $ Serialization.cst @@ string "short",
      Java._IntegralType_int>>: constant $ Serialization.cst @@ string "int",
      Java._IntegralType_long>>: constant $ Serialization.cst @@ string "long",
      Java._IntegralType_char>>: constant $ Serialization.cst @@ string "char"]

writeFloatingPointType :: TBinding (Java.FloatingPointType -> Expr)
writeFloatingPointType = def "writeFloatingPointType" $
  lambda "ft" $
    cases Java._FloatingPointType (var "ft") Nothing [
      Java._FloatingPointType_float>>: constant $ Serialization.cst @@ string "float",
      Java._FloatingPointType_double>>: constant $ Serialization.cst @@ string "double"]

writeReferenceType :: TBinding (Java.ReferenceType -> Expr)
writeReferenceType = def "writeReferenceType" $
  lambda "rt" $
    cases Java._ReferenceType (var "rt") Nothing [
      Java._ReferenceType_classOrInterface>>: lambda "cit" $ writeClassOrInterfaceType @@ var "cit",
      Java._ReferenceType_variable>>: lambda "v" $ writeTypeVariable @@ var "v",
      Java._ReferenceType_array>>: lambda "at" $ writeArrayType @@ var "at"]

writeClassOrInterfaceType :: TBinding (Java.ClassOrInterfaceType -> Expr)
writeClassOrInterfaceType = def "writeClassOrInterfaceType" $
  lambda "cit" $
    cases Java._ClassOrInterfaceType (var "cit") Nothing [
      Java._ClassOrInterfaceType_class>>: lambda "ct" $ writeClassType @@ var "ct",
      Java._ClassOrInterfaceType_interface>>: lambda "it" $ writeInterfaceType @@ var "it"]

writeExpression :: TBinding (Java.Expression -> Expr)
writeExpression = def "writeExpression" $
  lambda "e" $
    cases Java._Expression (var "e") Nothing [
      Java._Expression_lambda>>: lambda "l" $ writeLambdaExpression @@ var "l",
      Java._Expression_assignment>>: lambda "a" $ writeAssignmentExpression @@ var "a"]

writeAssignmentExpression :: TBinding (Java.AssignmentExpression -> Expr)
writeAssignmentExpression = def "writeAssignmentExpression" $
  lambda "e" $
    cases Java._AssignmentExpression (var "e") Nothing [
      Java._AssignmentExpression_conditional>>: lambda "c" $ writeConditionalExpression @@ var "c",
      Java._AssignmentExpression_assignment>>: lambda "a" $ writeAssignment @@ var "a"]

writeConditionalExpression :: TBinding (Java.ConditionalExpression -> Expr)
writeConditionalExpression = def "writeConditionalExpression" $
  lambda "c" $
    cases Java._ConditionalExpression (var "c") Nothing [
      Java._ConditionalExpression_simple>>: lambda "co" $ writeConditionalOrExpression @@ var "co",
      Java._ConditionalExpression_ternaryCond>>: lambda "tc" $ writeConditionalExpression_TernaryCond @@ var "tc",
      Java._ConditionalExpression_ternaryLambda>>: lambda "tl" $ writeConditionalExpression_TernaryLambda @@ var "tl"]

writeResult :: TBinding (Java.Result -> Expr)
writeResult = def "writeResult" $
  lambda "r" $
    cases Java._Result (var "r") Nothing [
      Java._Result_type>>: lambda "t" $ writeUnannType @@ var "t",
      Java._Result_void>>: constant $ Serialization.cst @@ string "void"]

writeMethodBody :: TBinding (Java.MethodBody -> Expr)
writeMethodBody = def "writeMethodBody" $
  lambda "b" $
    cases Java._MethodBody (var "b") Nothing [
      Java._MethodBody_block>>: lambda "block" $ writeBlock @@ var "block",
      Java._MethodBody_none>>: constant $ Serialization.cst @@ string ";"]

writeClassDeclaration :: TBinding (Java.ClassDeclaration -> Expr)
writeClassDeclaration = def "writeClassDeclaration" $
  lambda "d" $
    cases Java._ClassDeclaration (var "d") Nothing [
      Java._ClassDeclaration_normal>>: lambda "nd" $ writeNormalClassDeclaration @@ var "nd",
      Java._ClassDeclaration_enum>>: lambda "ed" $ writeEnumDeclaration @@ var "ed"]

writeInterfaceDeclaration :: TBinding (Java.InterfaceDeclaration -> Expr)
writeInterfaceDeclaration = def "writeInterfaceDeclaration" $
  lambda "d" $
    cases Java._InterfaceDeclaration (var "d") Nothing [
      Java._InterfaceDeclaration_normalInterface>>: lambda "n" $ writeNormalInterfaceDeclaration @@ var "n",
      Java._InterfaceDeclaration_annotationType>>: lambda "a" $ writeAnnotationTypeDeclaration @@ var "a"]

writeCastExpression :: TBinding (Java.CastExpression -> Expr)
writeCastExpression = def "writeCastExpression" $
  lambda "e" $
    cases Java._CastExpression (var "e") Nothing [
      Java._CastExpression_primitive>>: lambda "p" $ writeCastExpression_Primitive @@ var "p",
      Java._CastExpression_notPlusMinus>>: lambda "npm" $ writeCastExpression_NotPlusMinus @@ var "npm",
      Java._CastExpression_lambda>>: lambda "l" $ writeCastExpression_Lambda @@ var "l"]

writeFormalParameter :: TBinding (Java.FormalParameter -> Expr)
writeFormalParameter = def "writeFormalParameter" $
  lambda "p" $
    cases Java._FormalParameter (var "p") Nothing [
      Java._FormalParameter_simple>>: lambda "s" $ writeFormalParameter_Simple @@ var "s",
      Java._FormalParameter_variableArity>>: lambda "v" $ writeVariableArityParameter @@ var "v"]

writeLocalName :: TBinding (Java.LocalVariableType -> Expr)
writeLocalName = def "writeLocalName" $
  lambda "t" $
    cases Java._LocalVariableType (var "t") Nothing [
      Java._LocalVariableType_type>>: lambda "ut" $ writeUnannType @@ var "ut",
      Java._LocalVariableType_var>>: constant $ Serialization.cst @@ string "var"]

writeVariableInitializer :: TBinding (Java.VariableInitializer -> Expr)
writeVariableInitializer = def "writeVariableInitializer" $
  lambda "i" $
    cases Java._VariableInitializer (var "i") Nothing [
      Java._VariableInitializer_expression>>: lambda "e" $ writeExpression @@ var "e",
      Java._VariableInitializer_arrayInitializer>>: lambda "ai" $ writeArrayInitializer @@ var "ai"]

writeVariableModifier :: TBinding (Java.VariableModifier -> Expr)
writeVariableModifier = def "writeVariableModifier" $
  lambda "m" $
    cases Java._VariableModifier (var "m") Nothing [
      Java._VariableModifier_annotation>>: lambda "ann" $ writeAnnotation @@ var "ann",
      Java._VariableModifier_final>>: constant $ Serialization.cst @@ string "final"]

writeLeftHandSide :: TBinding (Java.LeftHandSide -> Expr)
writeLeftHandSide = def "writeLeftHandSide" $
  lambda "lhs" $
    cases Java._LeftHandSide (var "lhs") Nothing [
      Java._LeftHandSide_expressionName>>: lambda "en" $ writeExpressionName @@ var "en",
      Java._LeftHandSide_fieldAccess>>: lambda "fa" $ writeFieldAccess @@ var "fa",
      Java._LeftHandSide_arrayAccess>>: lambda "aa" $ writeArrayAccess @@ var "aa"]

writeLambdaBody :: TBinding (Java.LambdaBody -> Expr)
writeLambdaBody = def "writeLambdaBody" $
  lambda "b" $
    cases Java._LambdaBody (var "b") Nothing [
      Java._LambdaBody_expression>>: lambda "e" $ writeExpression @@ var "e",
      Java._LambdaBody_block>>: lambda "b" $ writeBlock @@ var "b"]

writeLambdaParameters :: TBinding (Java.LambdaParameters -> Expr)
writeLambdaParameters = def "writeLambdaParameters" $
  lambda "p" $
    cases Java._LambdaParameters (var "p") Nothing [
      Java._LambdaParameters_tuple>>: lambda "l" $ Serialization.parenList @@ false @@ Lists.map writeLambdaParameters (var "l"),
      Java._LambdaParameters_single>>: lambda "id" $ writeIdentifier @@ var "id"]

writeTypeArgument :: TBinding (Java.TypeArgument -> Expr)
writeTypeArgument = def "writeTypeArgument" $
  lambda "a" $
    cases Java._TypeArgument (var "a") Nothing [
      Java._TypeArgument_reference>>: lambda "rt" $ writeReferenceType @@ var "rt",
      Java._TypeArgument_wildcard>>: lambda "w" $ writeWildcard @@ var "w"]

writeTypeDeclaration :: TBinding (Java.TypeDeclaration -> Expr)
writeTypeDeclaration = def "writeTypeDeclaration" $
  lambda "d" $
    cases Java._TypeDeclaration (var "d") Nothing [
      Java._TypeDeclaration_class>>: lambda "d" $ writeClassDeclaration @@ var "d",
      Java._TypeDeclaration_interface>>: lambda "d" $ writeInterfaceDeclaration @@ var "d",
      Java._TypeDeclaration_none>>: constant $ Serialization.cst @@ string ";"]

writePrimary :: TBinding (Java.Primary -> Expr)
writePrimary = def "writePrimary" $
  lambda "p" $
    cases Java._Primary (var "p") Nothing [
      Java._Primary_noNewArray>>: lambda "n" $ writePrimaryNoNewArray @@ var "n",
      Java._Primary_arrayCreation>>: lambda "a" $ writeArrayCreationExpression @@ var "a"]

writeWildcardBounds :: TBinding (Java.WildcardBounds -> Expr)
writeWildcardBounds = def "writeWildcardBounds" $
  lambda "b" $
    cases Java._WildcardBounds (var "b") Nothing [
      Java._WildcardBounds_extends>>: lambda "rt" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "extends", writeReferenceType @@ var "rt"],
      Java._WildcardBounds_super>>: lambda "rt" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "super", writeReferenceType @@ var "rt"]]

writeTypeArgumentsOrDiamond :: TBinding (Java.TypeArgumentsOrDiamond -> Expr)
writeTypeArgumentsOrDiamond = def "writeTypeArgumentsOrDiamond" $
  lambda "targs" $
    cases Java._TypeArgumentsOrDiamond (var "targs") Nothing [
      Java._TypeArgumentsOrDiamond_arguments>>: lambda "args" $
        Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map writeTypeArgument (var "args"),
      Java._TypeArgumentsOrDiamond_diamond>>: constant $ Serialization.cst @@ string "<>"]

-- =============================================================================
-- Literals
-- =============================================================================

writeFloatingPointLiteral :: TBinding (Java.FloatingPointLiteral -> Expr)
writeFloatingPointLiteral = def "writeFloatingPointLiteral" $
  lambda "fl" $
    Serialization.cst @@ LibLiterals.showBigfloat (unwrap Java._FloatingPointLiteral @@ var "fl")

writeIntegerLiteral :: TBinding (Java.IntegerLiteral -> Expr)
writeIntegerLiteral = def "writeIntegerLiteral" $
  lambda "il" $ lets [
    "i">: unwrap Java._IntegerLiteral @@ var "il",
    "suffix">: Logic.ifElse (Logic.or
        (Equality.gt (var "i") (bigint 2147483647))
        (Equality.lt (var "i") (bigint (-2147483648))))
      (string "L")
      (string "")] $
    Serialization.cst @@ Strings.cat2 (LibLiterals.showBigint (var "i")) (var "suffix")

writeStringLiteral :: TBinding (Java.StringLiteral -> Expr)
writeStringLiteral = def "writeStringLiteral" $
  doc "Serialize a Java string literal with proper Unicode escaping." $
  lambda "sl" $
    "s" <~ unwrap Java._StringLiteral @@ var "sl" $
    Serialization.cst @@ Strings.cat2 (string "\"") (Strings.cat2 (escapeJavaString @@ var "s") (string "\""))

writeLiteral :: TBinding (Java.Literal -> Expr)
writeLiteral = def "writeLiteral" $
  lambda "l" $
    cases Java._Literal (var "l") Nothing [
      Java._Literal_null>>: constant $ Serialization.cst @@ string "null",
      Java._Literal_integer>>: lambda "il" $ writeIntegerLiteral @@ var "il",
      Java._Literal_floatingPoint>>: lambda "fl" $ writeFloatingPointLiteral @@ var "fl",
      Java._Literal_boolean>>: lambda "b" $ Serialization.cst @@ Logic.ifElse (var "b") (string "true") (string "false"),
      Java._Literal_character>>: lambda "c" $
        -- Convert uint16 char to int32 for comparison and string operations
        "ci" <~ LibLiterals.bigintToInt32 (LibLiterals.uint16ToBigint $ var "c") $
        -- Escape the character code for a Java char literal
        Serialization.cst @@ Strings.cat2 (string "'")
          (Strings.cat2
            (Logic.ifElse (Equality.equal (var "ci") (int32 39))  -- '\''
              (string "\\'")
              (Logic.ifElse (Equality.equal (var "ci") (int32 92))  -- '\\'
                (string "\\\\")
                (Logic.ifElse (Equality.equal (var "ci") (int32 10))  -- '\n'
                  (string "\\n")
                  (Logic.ifElse (Equality.equal (var "ci") (int32 13))  -- '\r'
                    (string "\\r")
                    (Logic.ifElse (Equality.equal (var "ci") (int32 9))  -- '\t'
                      (string "\\t")
                      (Logic.ifElse (Logic.and (Equality.gte (var "ci") (int32 32))
                                               (Equality.lt (var "ci") (int32 127)))
                        (Strings.fromList (list [var "ci"]))
                        (javaUnicodeEscape @@ var "ci")))))))
            (string "'")),
      Java._Literal_string>>: lambda "sl" $ writeStringLiteral @@ var "sl"]

-- =============================================================================
-- Names and identifiers
-- =============================================================================

writeAmbiguousName :: TBinding (Java.AmbiguousName -> Expr)
writeAmbiguousName = def "writeAmbiguousName" $
  lambda "an" $
    Serialization.dotSep @@ Lists.map writeIdentifier (unwrap Java._AmbiguousName @@ var "an")

writeExpressionName :: TBinding (Java.ExpressionName -> Expr)
writeExpressionName = def "writeExpressionName" $
  lambda "en" $ lets [
    "mqual">: project Java._ExpressionName Java._ExpressionName_qualifier @@ var "en",
    "id">: project Java._ExpressionName Java._ExpressionName_identifier @@ var "en"] $
    Serialization.dotSep @@ Maybes.cat (list [
      Maybes.map writeAmbiguousName (var "mqual"),
      just $ writeIdentifier @@ var "id"])

writeTypeName :: TBinding (Java.TypeName -> Expr)
writeTypeName = def "writeTypeName" $
  lambda "tn" $ lets [
    "id">: project Java._TypeName Java._TypeName_identifier @@ var "tn",
    "mqual">: project Java._TypeName Java._TypeName_qualifier @@ var "tn"] $
    Serialization.dotSep @@ Maybes.cat (list [
      Maybes.map writePackageOrTypeName (var "mqual"),
      just $ writeTypeIdentifier @@ var "id"])

writePackageName :: TBinding (Java.PackageName -> Expr)
writePackageName = def "writePackageName" $
  lambda "pn" $
    Serialization.dotSep @@ Lists.map writeIdentifier (unwrap Java._PackageName @@ var "pn")

writePackageOrTypeName :: TBinding (Java.PackageOrTypeName -> Expr)
writePackageOrTypeName = def "writePackageOrTypeName" $
  lambda "potn" $
    Serialization.dotSep @@ Lists.map writeIdentifier (unwrap Java._PackageOrTypeName @@ var "potn")

writeAnnotatedIdentifier :: TBinding (Java.AnnotatedIdentifier -> Expr)
writeAnnotatedIdentifier = def "writeAnnotatedIdentifier" $
  lambda "ai" $
    writeIdentifier @@ (project Java._AnnotatedIdentifier Java._AnnotatedIdentifier_identifier @@ var "ai")

writeTypeVariable :: TBinding (Java.TypeVariable -> Expr)
writeTypeVariable = def "writeTypeVariable" $
  lambda "tv" $ lets [
    "anns">: project Java._TypeVariable Java._TypeVariable_annotations @@ var "tv",
    "id">: project Java._TypeVariable Java._TypeVariable_identifier @@ var "tv"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "anns"))
        nothing
        (just $ Serialization.spaceSep @@ Lists.map writeAnnotation (var "anns")),
      just $ writeTypeIdentifier @@ var "id"])

-- =============================================================================
-- Modifiers
-- =============================================================================

writeClassModifier :: TBinding (Java.ClassModifier -> Expr)
writeClassModifier = def "writeClassModifier" $
  lambda "m" $
    cases Java._ClassModifier (var "m") Nothing [
      Java._ClassModifier_annotation>>: lambda "ann" $ writeAnnotation @@ var "ann",
      Java._ClassModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._ClassModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._ClassModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._ClassModifier_abstract>>: constant $ Serialization.cst @@ string "abstract",
      Java._ClassModifier_static>>: constant $ Serialization.cst @@ string "static",
      Java._ClassModifier_final>>: constant $ Serialization.cst @@ string "final",
      Java._ClassModifier_strictfp>>: constant $ Serialization.cst @@ string "strictfp"]

writeFieldModifier :: TBinding (Java.FieldModifier -> Expr)
writeFieldModifier = def "writeFieldModifier" $
  lambda "m" $
    cases Java._FieldModifier (var "m") Nothing [
      Java._FieldModifier_annotation>>: lambda "ann" $ writeAnnotation @@ var "ann",
      Java._FieldModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._FieldModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._FieldModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._FieldModifier_static>>: constant $ Serialization.cst @@ string "static",
      Java._FieldModifier_final>>: constant $ Serialization.cst @@ string "final",
      Java._FieldModifier_transient>>: constant $ Serialization.cst @@ string "transient",
      Java._FieldModifier_volatile>>: constant $ Serialization.cst @@ string "volatile"]

writeMethodModifier :: TBinding (Java.MethodModifier -> Expr)
writeMethodModifier = def "writeMethodModifier" $
  lambda "m" $
    cases Java._MethodModifier (var "m") Nothing [
      Java._MethodModifier_annotation>>: lambda "ann" $ writeAnnotation @@ var "ann",
      Java._MethodModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._MethodModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._MethodModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._MethodModifier_abstract>>: constant $ Serialization.cst @@ string "abstract",
      Java._MethodModifier_final>>: constant $ Serialization.cst @@ string "final",
      Java._MethodModifier_synchronized>>: constant $ Serialization.cst @@ string "synchronized",
      Java._MethodModifier_native>>: constant $ Serialization.cst @@ string "native",
      Java._MethodModifier_strictfb>>: constant $ Serialization.cst @@ string "strictfb"]

writeConstructorModifier :: TBinding (Java.ConstructorModifier -> Expr)
writeConstructorModifier = def "writeConstructorModifier" $
  lambda "m" $
    cases Java._ConstructorModifier (var "m") Nothing [
      Java._ConstructorModifier_annotation>>: lambda "ann" $ writeAnnotation @@ var "ann",
      Java._ConstructorModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._ConstructorModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._ConstructorModifier_private>>: constant $ Serialization.cst @@ string "private"]

writeInterfaceModifier :: TBinding (Java.InterfaceModifier -> Expr)
writeInterfaceModifier = def "writeInterfaceModifier" $
  lambda "m" $
    cases Java._InterfaceModifier (var "m") Nothing [
      Java._InterfaceModifier_annotation>>: lambda "a" $ writeAnnotation @@ var "a",
      Java._InterfaceModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._InterfaceModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._InterfaceModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._InterfaceModifier_abstract>>: constant $ Serialization.cst @@ string "abstract",
      Java._InterfaceModifier_static>>: constant $ Serialization.cst @@ string "static",
      Java._InterfaceModifier_strictfb>>: constant $ Serialization.cst @@ string "strictfb"]

writeInterfaceMethodModifier :: TBinding (Java.InterfaceMethodModifier -> Expr)
writeInterfaceMethodModifier = def "writeInterfaceMethodModifier" $
  lambda "m" $
    cases Java._InterfaceMethodModifier (var "m") Nothing [
      Java._InterfaceMethodModifier_annotation>>: lambda "a" $ writeAnnotation @@ var "a",
      Java._InterfaceMethodModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._InterfaceMethodModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._InterfaceMethodModifier_abstract>>: constant $ Serialization.cst @@ string "abstract",
      Java._InterfaceMethodModifier_default>>: constant $ Serialization.cst @@ string "default",
      Java._InterfaceMethodModifier_static>>: constant $ Serialization.cst @@ string "static",
      Java._InterfaceMethodModifier_strictfp>>: constant $ Serialization.cst @@ string "strictfp"]

-- =============================================================================
-- Expression hierarchy
-- =============================================================================

writeAdditionalBound :: TBinding (Java.AdditionalBound -> Expr)
writeAdditionalBound = def "writeAdditionalBound" $
  lambda "ab" $
    Serialization.spaceSep @@ list [Serialization.cst @@ string "&", writeInterfaceType @@ (unwrap Java._AdditionalBound @@ var "ab")]

writeAdditiveExpression :: TBinding (Java.AdditiveExpression -> Expr)
writeAdditiveExpression = def "writeAdditiveExpression" $
  lambda "e" $
    cases Java._AdditiveExpression (var "e") Nothing [
      Java._AdditiveExpression_unary>>: lambda "m" $ writeMultiplicativeExpression @@ var "m",
      Java._AdditiveExpression_plus>>: lambda "b" $
        Serialization.infixWs @@ string "+" @@
          (writeAdditiveExpression @@ (project Java._AdditiveExpression_Binary Java._AdditiveExpression_Binary_lhs @@ var "b")) @@
          (writeMultiplicativeExpression @@ (project Java._AdditiveExpression_Binary Java._AdditiveExpression_Binary_rhs @@ var "b")),
      Java._AdditiveExpression_minus>>: lambda "b" $
        Serialization.infixWs @@ string "-" @@
          (writeAdditiveExpression @@ (project Java._AdditiveExpression_Binary Java._AdditiveExpression_Binary_lhs @@ var "b")) @@
          (writeMultiplicativeExpression @@ (project Java._AdditiveExpression_Binary Java._AdditiveExpression_Binary_rhs @@ var "b"))]

writeMultiplicativeExpression :: TBinding (Java.MultiplicativeExpression -> Expr)
writeMultiplicativeExpression = def "writeMultiplicativeExpression" $
  lambda "e" $
    cases Java._MultiplicativeExpression (var "e") Nothing [
      Java._MultiplicativeExpression_unary>>: lambda "u" $ writeUnaryExpression @@ var "u",
      Java._MultiplicativeExpression_times>>: lambda "b" $
        Serialization.infixWs @@ string "*" @@
          (writeMultiplicativeExpression @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_lhs @@ var "b")) @@
          (writeUnaryExpression @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_rhs @@ var "b")),
      Java._MultiplicativeExpression_divide>>: lambda "b" $
        Serialization.infixWs @@ string "/" @@
          (writeMultiplicativeExpression @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_lhs @@ var "b")) @@
          (writeUnaryExpression @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_rhs @@ var "b")),
      Java._MultiplicativeExpression_mod>>: lambda "b" $
        Serialization.infixWs @@ string "%" @@
          (writeMultiplicativeExpression @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_lhs @@ var "b")) @@
          (writeUnaryExpression @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_rhs @@ var "b"))]

writeUnaryExpression :: TBinding (Java.UnaryExpression -> Expr)
writeUnaryExpression = def "writeUnaryExpression" $
  lambda "e" $
    cases Java._UnaryExpression (var "e") Nothing [
      Java._UnaryExpression_preIncrement>>: lambda "pi" $ writePreIncrementExpression @@ var "pi",
      Java._UnaryExpression_preDecrement>>: lambda "pd" $ writePreDecrementExpression @@ var "pd",
      Java._UnaryExpression_plus>>: lambda "p" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "+", writeUnaryExpression @@ var "p"],
      Java._UnaryExpression_minus>>: lambda "m" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "-", writeUnaryExpression @@ var "m"],
      Java._UnaryExpression_other>>: lambda "o" $ writeUnaryExpressionNotPlusMinus @@ var "o"]

writeUnaryExpressionNotPlusMinus :: TBinding (Java.UnaryExpressionNotPlusMinus -> Expr)
writeUnaryExpressionNotPlusMinus = def "writeUnaryExpressionNotPlusMinus" $
  lambda "e" $
    cases Java._UnaryExpressionNotPlusMinus (var "e") Nothing [
      Java._UnaryExpressionNotPlusMinus_postfix>>: lambda "p" $ writePostfixExpression @@ var "p",
      Java._UnaryExpressionNotPlusMinus_tilde>>: lambda "u" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "~", writeUnaryExpression @@ var "u"],
      Java._UnaryExpressionNotPlusMinus_not>>: lambda "u" $ Serialization.noSep @@ list [Serialization.cst @@ string "!", writeUnaryExpression @@ var "u"],
      Java._UnaryExpressionNotPlusMinus_cast>>: lambda "c" $ writeCastExpression @@ var "c"]

writePostfixExpression :: TBinding (Java.PostfixExpression -> Expr)
writePostfixExpression = def "writePostfixExpression" $
  lambda "e" $
    cases Java._PostfixExpression (var "e") Nothing [
      Java._PostfixExpression_primary>>: lambda "p" $ writePrimary @@ var "p",
      Java._PostfixExpression_name>>: lambda "en" $ writeExpressionName @@ var "en",
      Java._PostfixExpression_postIncrement>>: lambda "pi" $ writePostIncrementExpression @@ var "pi",
      Java._PostfixExpression_postDecrement>>: lambda "pd" $ writePostDecrementExpression @@ var "pd"]

writeAndExpression :: TBinding (Java.AndExpression -> Expr)
writeAndExpression = def "writeAndExpression" $
  lambda "ae" $
    Serialization.infixWsList @@ string "&" @@ Lists.map writeEqualityExpression (unwrap Java._AndExpression @@ var "ae")

writeExclusiveOrExpression :: TBinding (Java.ExclusiveOrExpression -> Expr)
writeExclusiveOrExpression = def "writeExclusiveOrExpression" $
  lambda "eoe" $
    Serialization.infixWsList @@ string "^" @@ Lists.map writeAndExpression (unwrap Java._ExclusiveOrExpression @@ var "eoe")

writeInclusiveOrExpression :: TBinding (Java.InclusiveOrExpression -> Expr)
writeInclusiveOrExpression = def "writeInclusiveOrExpression" $
  lambda "ioe" $
    Serialization.infixWsList @@ string "|" @@ Lists.map writeExclusiveOrExpression (unwrap Java._InclusiveOrExpression @@ var "ioe")

writeConditionalAndExpression :: TBinding (Java.ConditionalAndExpression -> Expr)
writeConditionalAndExpression = def "writeConditionalAndExpression" $
  lambda "cae" $
    Serialization.infixWsList @@ string "&&" @@ Lists.map writeInclusiveOrExpression (unwrap Java._ConditionalAndExpression @@ var "cae")

writeConditionalOrExpression :: TBinding (Java.ConditionalOrExpression -> Expr)
writeConditionalOrExpression = def "writeConditionalOrExpression" $
  lambda "coe" $
    Serialization.infixWsList @@ string "||" @@ Lists.map writeConditionalAndExpression (unwrap Java._ConditionalOrExpression @@ var "coe")

writeEqualityExpression :: TBinding (Java.EqualityExpression -> Expr)
writeEqualityExpression = def "writeEqualityExpression" $
  lambda "e" $
    cases Java._EqualityExpression (var "e") Nothing [
      Java._EqualityExpression_unary>>: lambda "r" $ writeRelationalExpression @@ var "r",
      Java._EqualityExpression_equal>>: lambda "b" $
        Serialization.infixWs @@ string "==" @@
          (writeEqualityExpression @@ (project Java._EqualityExpression_Binary Java._EqualityExpression_Binary_lhs @@ var "b")) @@
          (writeRelationalExpression @@ (project Java._EqualityExpression_Binary Java._EqualityExpression_Binary_rhs @@ var "b")),
      Java._EqualityExpression_notEqual>>: lambda "b" $
        Serialization.infixWs @@ string "!=" @@
          (writeEqualityExpression @@ (project Java._EqualityExpression_Binary Java._EqualityExpression_Binary_lhs @@ var "b")) @@
          (writeRelationalExpression @@ (project Java._EqualityExpression_Binary Java._EqualityExpression_Binary_rhs @@ var "b"))]

writeRelationalExpression :: TBinding (Java.RelationalExpression -> Expr)
writeRelationalExpression = def "writeRelationalExpression" $
  lambda "e" $
    cases Java._RelationalExpression (var "e") Nothing [
      Java._RelationalExpression_simple>>: lambda "s" $ writeShiftExpression @@ var "s",
      Java._RelationalExpression_lessThan>>: lambda "lt" $ writeRelationalExpression_LessThan @@ var "lt",
      Java._RelationalExpression_greaterThan>>: lambda "gt" $ writeRelationalExpression_GreaterThan @@ var "gt",
      Java._RelationalExpression_lessThanEqual>>: lambda "lte" $ writeRelationalExpression_LessThanEqual @@ var "lte",
      Java._RelationalExpression_greaterThanEqual>>: lambda "gte" $ writeRelationalExpression_GreaterThanEqual @@ var "gte",
      Java._RelationalExpression_instanceof>>: lambda "i" $ writeRelationalExpression_InstanceOf @@ var "i"]

writeRelationalExpression_LessThan :: TBinding (Java.RelationalExpression_LessThan -> Expr)
writeRelationalExpression_LessThan = def "writeRelationalExpression_LessThan" $
  lambda "lt" $
    Serialization.infixWs @@ string "<" @@
      (writeRelationalExpression @@ (project Java._RelationalExpression_LessThan Java._RelationalExpression_LessThan_lhs @@ var "lt")) @@
      (writeShiftExpression @@ (project Java._RelationalExpression_LessThan Java._RelationalExpression_LessThan_rhs @@ var "lt"))

writeRelationalExpression_GreaterThan :: TBinding (Java.RelationalExpression_GreaterThan -> Expr)
writeRelationalExpression_GreaterThan = def "writeRelationalExpression_GreaterThan" $
  lambda "gt" $
    Serialization.infixWs @@ string ">" @@
      (writeRelationalExpression @@ (project Java._RelationalExpression_GreaterThan Java._RelationalExpression_GreaterThan_lhs @@ var "gt")) @@
      (writeShiftExpression @@ (project Java._RelationalExpression_GreaterThan Java._RelationalExpression_GreaterThan_rhs @@ var "gt"))

writeRelationalExpression_LessThanEqual :: TBinding (Java.RelationalExpression_LessThanEqual -> Expr)
writeRelationalExpression_LessThanEqual = def "writeRelationalExpression_LessThanEqual" $
  lambda "lte" $
    Serialization.infixWs @@ string "<=" @@
      (writeRelationalExpression @@ (project Java._RelationalExpression_LessThanEqual Java._RelationalExpression_LessThanEqual_lhs @@ var "lte")) @@
      (writeShiftExpression @@ (project Java._RelationalExpression_LessThanEqual Java._RelationalExpression_LessThanEqual_rhs @@ var "lte"))

writeRelationalExpression_GreaterThanEqual :: TBinding (Java.RelationalExpression_GreaterThanEqual -> Expr)
writeRelationalExpression_GreaterThanEqual = def "writeRelationalExpression_GreaterThanEqual" $
  lambda "gte" $
    Serialization.infixWs @@ string ">=" @@
      (writeRelationalExpression @@ (project Java._RelationalExpression_GreaterThanEqual Java._RelationalExpression_GreaterThanEqual_lhs @@ var "gte")) @@
      (writeShiftExpression @@ (project Java._RelationalExpression_GreaterThanEqual Java._RelationalExpression_GreaterThanEqual_rhs @@ var "gte"))

writeRelationalExpression_InstanceOf :: TBinding (Java.RelationalExpression_InstanceOf -> Expr)
writeRelationalExpression_InstanceOf = def "writeRelationalExpression_InstanceOf" $
  lambda "io" $
    Serialization.infixWs @@ string "instanceof" @@
      (writeRelationalExpression @@ (project Java._RelationalExpression_InstanceOf Java._RelationalExpression_InstanceOf_lhs @@ var "io")) @@
      (writeReferenceType @@ (project Java._RelationalExpression_InstanceOf Java._RelationalExpression_InstanceOf_rhs @@ var "io"))

writeShiftExpression :: TBinding (Java.ShiftExpression -> Expr)
writeShiftExpression = def "writeShiftExpression" $
  lambda "e" $
    cases Java._ShiftExpression (var "e") Nothing [
      Java._ShiftExpression_unary>>: lambda "a" $ writeAdditiveExpression @@ var "a",
      Java._ShiftExpression_shiftLeft>>: lambda "b" $
        Serialization.infixWs @@ string "<<" @@
          (writeShiftExpression @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_lhs @@ var "b")) @@
          (writeAdditiveExpression @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_rhs @@ var "b")),
      Java._ShiftExpression_shiftRight>>: lambda "b" $
        Serialization.infixWs @@ string ">>" @@
          (writeShiftExpression @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_lhs @@ var "b")) @@
          (writeAdditiveExpression @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_rhs @@ var "b")),
      Java._ShiftExpression_shiftRightZeroFill>>: lambda "b" $
        Serialization.infixWs @@ string ">>>" @@
          (writeShiftExpression @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_lhs @@ var "b")) @@
          (writeAdditiveExpression @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_rhs @@ var "b"))]

writeLambdaExpression :: TBinding (Java.LambdaExpression -> Expr)
writeLambdaExpression = def "writeLambdaExpression" $
  lambda "le" $ lets [
    "params">: project Java._LambdaExpression Java._LambdaExpression_parameters @@ var "le",
    "body">: project Java._LambdaExpression Java._LambdaExpression_body @@ var "le"] $
    Serialization.infixWs @@ string "->" @@ (writeLambdaParameters @@ var "params") @@ (writeLambdaBody @@ var "body")

writeAssignment :: TBinding (Java.Assignment -> Expr)
writeAssignment = def "writeAssignment" $
  lambda "a" $ lets [
    "lhs">: project Java._Assignment Java._Assignment_lhs @@ var "a",
    "op">: project Java._Assignment Java._Assignment_op @@ var "a",
    "rhs">: project Java._Assignment Java._Assignment_expression @@ var "a",
    "ctop">: cases Java._AssignmentOperator (var "op") Nothing [
      Java._AssignmentOperator_simple>>: constant $ string "=",
      Java._AssignmentOperator_times>>: constant $ string "*=",
      Java._AssignmentOperator_div>>: constant $ string "/=",
      Java._AssignmentOperator_mod>>: constant $ string "%=",
      Java._AssignmentOperator_plus>>: constant $ string "+=",
      Java._AssignmentOperator_minus>>: constant $ string "-=",
      Java._AssignmentOperator_shiftLeft>>: constant $ string "<<=",
      Java._AssignmentOperator_shiftRight>>: constant $ string ">>=",
      Java._AssignmentOperator_shiftRightZeroFill>>: constant $ string ">>>=",
      Java._AssignmentOperator_and>>: constant $ string "&=",
      Java._AssignmentOperator_xor>>: constant $ string "^=",
      Java._AssignmentOperator_or>>: constant $ string "|="]] $
    Serialization.infixWs @@ var "ctop" @@ (writeLeftHandSide @@ var "lhs") @@ (writeExpression @@ var "rhs")

-- =============================================================================
-- Statements
-- =============================================================================

writeStatement :: TBinding (Java.Statement -> Expr)
writeStatement = def "writeStatement" $
  lambda "s" $
    cases Java._Statement (var "s") Nothing [
      Java._Statement_withoutTrailing>>: lambda "s" $ writeStatementWithoutTrailingSubstatement @@ var "s",
      Java._Statement_labeled>>: lambda "l" $ writeLabeledStatement @@ var "l",
      Java._Statement_ifThen>>: lambda "it" $ writeIfThenStatement @@ var "it",
      Java._Statement_ifThenElse>>: lambda "ite" $ writeIfThenElseStatement @@ var "ite",
      Java._Statement_while>>: lambda "w" $ writeWhileStatement @@ var "w",
      Java._Statement_for>>: lambda "f" $ writeForStatement @@ var "f"]

writeStatementWithoutTrailingSubstatement :: TBinding (Java.StatementWithoutTrailingSubstatement -> Expr)
writeStatementWithoutTrailingSubstatement = def "writeStatementWithoutTrailingSubstatement" $
  lambda "s" $
    cases Java._StatementWithoutTrailingSubstatement (var "s") Nothing [
      Java._StatementWithoutTrailingSubstatement_block>>: lambda "b" $ writeBlock @@ var "b",
      Java._StatementWithoutTrailingSubstatement_empty>>: constant $ Serialization.cst @@ string ";",
      Java._StatementWithoutTrailingSubstatement_expression>>: lambda "e" $ writeExpressionStatement @@ var "e",
      Java._StatementWithoutTrailingSubstatement_assert>>: lambda "a" $ writeAssertStatement @@ var "a",
      Java._StatementWithoutTrailingSubstatement_switch>>: lambda "s" $ writeSwitchStatement @@ var "s",
      Java._StatementWithoutTrailingSubstatement_do>>: lambda "d" $ writeDoStatement @@ var "d",
      Java._StatementWithoutTrailingSubstatement_break>>: lambda "b" $ writeBreakStatement @@ var "b",
      Java._StatementWithoutTrailingSubstatement_continue>>: lambda "c" $ writeContinueStatement @@ var "c",
      Java._StatementWithoutTrailingSubstatement_return>>: lambda "r" $ writeReturnStatement @@ var "r",
      Java._StatementWithoutTrailingSubstatement_synchronized>>: lambda "s" $ writeSynchronizedStatement @@ var "s",
      Java._StatementWithoutTrailingSubstatement_throw>>: lambda "t" $ writeThrowStatement @@ var "t",
      Java._StatementWithoutTrailingSubstatement_try>>: lambda "t" $ writeTryStatement @@ var "t"]

writeStatementExpression :: TBinding (Java.StatementExpression -> Expr)
writeStatementExpression = def "writeStatementExpression" $
  lambda "e" $
    cases Java._StatementExpression (var "e") Nothing [
      Java._StatementExpression_assignment>>: lambda "a" $ writeAssignment @@ var "a",
      Java._StatementExpression_preIncrement>>: lambda "pi" $ writePreIncrementExpression @@ var "pi",
      Java._StatementExpression_preDecrement>>: lambda "pd" $ writePreDecrementExpression @@ var "pd",
      Java._StatementExpression_postIncrement>>: lambda "pi" $ writePostIncrementExpression @@ var "pi",
      Java._StatementExpression_postDecrement>>: lambda "pd" $ writePostDecrementExpression @@ var "pd",
      Java._StatementExpression_methodInvocation>>: lambda "m" $ writeMethodInvocation @@ var "m",
      Java._StatementExpression_classInstanceCreation>>: lambda "cic" $ writeClassInstanceCreationExpression @@ var "cic"]

writeExpressionStatement :: TBinding (Java.ExpressionStatement -> Expr)
writeExpressionStatement = def "writeExpressionStatement" $
  lambda "es" $
    Serialization.withSemi @@ (writeStatementExpression @@ (unwrap Java._ExpressionStatement @@ var "es"))

writeBlock :: TBinding (Java.Block -> Expr)
writeBlock = def "writeBlock" $
  lambda "b" $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@ (Serialization.newlineSep @@ Lists.map writeBlockStatement (unwrap Java._Block @@ var "b"))

writeBlockStatement :: TBinding (Java.BlockStatement -> Expr)
writeBlockStatement = def "writeBlockStatement" $
  lambda "s" $
    cases Java._BlockStatement (var "s") Nothing [
      Java._BlockStatement_localVariableDeclaration>>: lambda "d" $ writeLocalVariableDeclarationStatement @@ var "d",
      Java._BlockStatement_class>>: lambda "cd" $ writeClassDeclaration @@ var "cd",
      Java._BlockStatement_statement>>: lambda "s" $ writeStatement @@ var "s"]

writeLocalVariableDeclarationStatement :: TBinding (Java.LocalVariableDeclarationStatement -> Expr)
writeLocalVariableDeclarationStatement = def "writeLocalVariableDeclarationStatement" $
  lambda "lvds" $
    Serialization.withSemi @@ (writeLocalVariableDeclaration @@ (unwrap Java._LocalVariableDeclarationStatement @@ var "lvds"))

writeLocalVariableDeclaration :: TBinding (Java.LocalVariableDeclaration -> Expr)
writeLocalVariableDeclaration = def "writeLocalVariableDeclaration" $
  lambda "lvd" $ lets [
    "mods">: project Java._LocalVariableDeclaration Java._LocalVariableDeclaration_modifiers @@ var "lvd",
    "t">: project Java._LocalVariableDeclaration Java._LocalVariableDeclaration_type @@ var "lvd",
    "decls">: project Java._LocalVariableDeclaration Java._LocalVariableDeclaration_declarators @@ var "lvd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeVariableModifier (var "mods")),
      just $ writeLocalName @@ var "t",
      just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeVariableDeclarator (var "decls")])

writeReturnStatement :: TBinding (Java.ReturnStatement -> Expr)
writeReturnStatement = def "writeReturnStatement" $
  lambda "rs" $ lets [
    "mex">: unwrap Java._ReturnStatement @@ var "rs"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "return",
      Maybes.map writeExpression (var "mex")]))

writeThrowStatement :: TBinding (Java.ThrowStatement -> Expr)
writeThrowStatement = def "writeThrowStatement" $
  lambda "ts" $
    Serialization.withSemi @@ (Serialization.spaceSep @@ list [
      Serialization.cst @@ string "throw",
      writeExpression @@ (unwrap Java._ThrowStatement @@ var "ts")])

writeBreakStatement :: TBinding (Java.BreakStatement -> Expr)
writeBreakStatement = def "writeBreakStatement" $
  lambda "bs" $ lets [
    "mlabel">: unwrap Java._BreakStatement @@ var "bs"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "break",
      Maybes.map writeIdentifier (var "mlabel")]))

writeContinueStatement :: TBinding (Java.ContinueStatement -> Expr)
writeContinueStatement = def "writeContinueStatement" $
  lambda "cs" $ lets [
    "mlabel">: unwrap Java._ContinueStatement @@ var "cs"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "continue",
      Maybes.map writeIdentifier (var "mlabel")]))

writeIfThenStatement :: TBinding (Java.IfThenStatement -> Expr)
writeIfThenStatement = def "writeIfThenStatement" $
  lambda "its" $ lets [
    "cond">: project Java._IfThenStatement Java._IfThenStatement_expression @@ var "its",
    "thn">: project Java._IfThenStatement Java._IfThenStatement_statement @@ var "its"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "if",
      Serialization.parenList @@ false @@ list [writeExpression @@ var "cond"],
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@ (writeStatement @@ var "thn")]

-- =============================================================================
-- Primary expressions
-- =============================================================================

writePrimaryNoNewArray :: TBinding (Java.PrimaryNoNewArray -> Expr)
writePrimaryNoNewArray = def "writePrimaryNoNewArray" $
  lambda "p" $
    cases Java._PrimaryNoNewArray (var "p") Nothing [
      Java._PrimaryNoNewArray_literal>>: lambda "l" $ writeLiteral @@ var "l",
      Java._PrimaryNoNewArray_classLiteral>>: lambda "cl" $ writeClassLiteral @@ var "cl",
      Java._PrimaryNoNewArray_this>>: constant $ Serialization.cst @@ string "this",
      Java._PrimaryNoNewArray_dotThis>>: lambda "n" $ Serialization.dotSep @@ list [writeTypeName @@ var "n", Serialization.cst @@ string "this"],
      Java._PrimaryNoNewArray_parens>>: lambda "e" $ Serialization.parenList @@ false @@ list [writeExpression @@ var "e"],
      Java._PrimaryNoNewArray_classInstance>>: lambda "ci" $ writeClassInstanceCreationExpression @@ var "ci",
      Java._PrimaryNoNewArray_fieldAccess>>: lambda "fa" $ writeFieldAccess @@ var "fa",
      Java._PrimaryNoNewArray_arrayAccess>>: lambda "aa" $ writeArrayAccess @@ var "aa",
      Java._PrimaryNoNewArray_methodInvocation>>: lambda "mi" $ writeMethodInvocation @@ var "mi",
      Java._PrimaryNoNewArray_methodReference>>: lambda "mr" $ writeMethodReference @@ var "mr"]

writeFieldAccess :: TBinding (Java.FieldAccess -> Expr)
writeFieldAccess = def "writeFieldAccess" $
  lambda "fa" $ lets [
    "qual">: project Java._FieldAccess Java._FieldAccess_qualifier @@ var "fa",
    "id">: project Java._FieldAccess Java._FieldAccess_identifier @@ var "fa"] $
    cases Java._FieldAccess_Qualifier (var "qual") Nothing [
      Java._FieldAccess_Qualifier_primary>>: lambda "p" $ Serialization.dotSep @@ list [writePrimary @@ var "p", writeIdentifier @@ var "id"],
      Java._FieldAccess_Qualifier_super>>: constant $ Serialization.dotSep @@ list [Serialization.cst @@ string "super", writeIdentifier @@ var "id"],
      Java._FieldAccess_Qualifier_typed>>: lambda "tn" $ Serialization.dotSep @@ list [writeTypeName @@ var "tn", Serialization.cst @@ string "super", writeIdentifier @@ var "id"]]

-- =============================================================================
-- Annotations
-- =============================================================================

writeMarkerAnnotation :: TBinding (Java.MarkerAnnotation -> Expr)
writeMarkerAnnotation = def "writeMarkerAnnotation" $
  lambda "ma" $
    Serialization.prefix @@ string "@" @@ (writeTypeName @@ (unwrap Java._MarkerAnnotation @@ var "ma"))

writeNormalAnnotation :: TBinding (Java.NormalAnnotation -> Expr)
writeNormalAnnotation = def "writeNormalAnnotation" $
  lambda "na" $ lets [
    "tname">: project Java._NormalAnnotation Java._NormalAnnotation_typeName @@ var "na",
    "pairs">: project Java._NormalAnnotation Java._NormalAnnotation_pairs @@ var "na"] $
    Serialization.prefix @@ string "@" @@ (Serialization.noSep @@ list [
      writeTypeName @@ var "tname",
      Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeElementValuePair (var "pairs")])

writeSingleElementAnnotation :: TBinding (Java.SingleElementAnnotation -> Expr)
writeSingleElementAnnotation = def "writeSingleElementAnnotation" $
  lambda "sea" $ lets [
    "tname">: project Java._SingleElementAnnotation Java._SingleElementAnnotation_name @@ var "sea",
    "mv">: project Java._SingleElementAnnotation Java._SingleElementAnnotation_value @@ var "sea"] $
    Maybes.maybe
      (writeMarkerAnnotation @@ (wrap Java._MarkerAnnotation (var "tname")))
      (lambda "v" $ Serialization.prefix @@ string "@" @@ (Serialization.noSep @@ list [
        writeTypeName @@ var "tname",
        Serialization.parenList @@ false @@ list [writeElementValue @@ var "v"]]))
      (var "mv")

writeElementValue :: TBinding (Java.ElementValue -> Expr)
writeElementValue = def "writeElementValue" $
  lambda "ev" $
    cases Java._ElementValue (var "ev") Nothing [
      Java._ElementValue_conditionalExpression>>: lambda "c" $ writeConditionalExpression @@ var "c",
      Java._ElementValue_elementValueArrayInitializer>>: lambda "evai" $
        Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeElementValue (unwrap Java._ElementValueArrayInitializer @@ var "evai"),
      Java._ElementValue_annotation>>: lambda "ann" $ writeAnnotation @@ var "ann"]

writeElementValuePair :: TBinding (Java.ElementValuePair -> Expr)
writeElementValuePair = def "writeElementValuePair" $
  lambda "evp" $ lets [
    "k">: project Java._ElementValuePair Java._ElementValuePair_key @@ var "evp",
    "v">: project Java._ElementValuePair Java._ElementValuePair_value @@ var "evp"] $
    Serialization.infixWs @@ string "=" @@ (writeIdentifier @@ var "k") @@ (writeElementValue @@ var "v")

-- =============================================================================
-- Type structures
-- =============================================================================

writePrimitiveTypeWithAnnotations :: TBinding (Java.PrimitiveTypeWithAnnotations -> Expr)
writePrimitiveTypeWithAnnotations = def "writePrimitiveTypeWithAnnotations" $
  lambda "ptwa" $ lets [
    "pt">: project Java._PrimitiveTypeWithAnnotations Java._PrimitiveTypeWithAnnotations_type @@ var "ptwa",
    "anns">: project Java._PrimitiveTypeWithAnnotations Java._PrimitiveTypeWithAnnotations_annotations @@ var "ptwa"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "anns")) nothing (just $ Serialization.spaceSep @@ Lists.map writeAnnotation (var "anns")),
      just $ writePrimitiveType @@ var "pt"])

writeClassType :: TBinding (Java.ClassType -> Expr)
writeClassType = def "writeClassType" $
  lambda "ct" $ lets [
    "anns">: project Java._ClassType Java._ClassType_annotations @@ var "ct",
    "qual">: project Java._ClassType Java._ClassType_qualifier @@ var "ct",
    "id">: project Java._ClassType Java._ClassType_identifier @@ var "ct",
    "args">: project Java._ClassType Java._ClassType_arguments @@ var "ct",
    "qualifiedId">: cases Java._ClassTypeQualifier (var "qual") Nothing [
      Java._ClassTypeQualifier_none>>: constant $ writeTypeIdentifier @@ var "id",
      Java._ClassTypeQualifier_package>>: lambda "pkg" $ Serialization.dotSep @@ list [writePackageName @@ var "pkg", writeTypeIdentifier @@ var "id"],
      Java._ClassTypeQualifier_parent>>: lambda "cit" $ Serialization.dotSep @@ list [writeClassOrInterfaceType @@ var "cit", writeTypeIdentifier @@ var "id"]]] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ Serialization.spaceSep @@ Maybes.cat (list [
        Logic.ifElse (Lists.null (var "anns")) nothing (just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeAnnotation (var "anns")),
        just $ var "qualifiedId"]),
      Logic.ifElse (Lists.null (var "args")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map writeTypeArgument (var "args"))])

writeArrayType :: TBinding (Java.ArrayType -> Expr)
writeArrayType = def "writeArrayType" $
  lambda "at" $ lets [
    "dims">: project Java._ArrayType Java._ArrayType_dims @@ var "at",
    "variant">: project Java._ArrayType Java._ArrayType_variant @@ var "at",
    "varExpr">: cases Java._ArrayType_Variant (var "variant") Nothing [
      Java._ArrayType_Variant_primitive>>: lambda "pt" $ writePrimitiveTypeWithAnnotations @@ var "pt",
      Java._ArrayType_Variant_classOrInterface>>: lambda "cit" $ writeClassOrInterfaceType @@ var "cit",
      Java._ArrayType_Variant_variable>>: lambda "tv" $ writeTypeVariable @@ var "tv"]] $
    Serialization.noSep @@ list [var "varExpr", writeDims @@ var "dims"]

writeDims :: TBinding (Java.Dims -> Expr)
writeDims = def "writeDims" $
  lambda "d" $
    Serialization.noSep @@ Lists.map (lambda "_" $ Serialization.cst @@ string "[]") (unwrap Java._Dims @@ var "d")

writeTypeBound :: TBinding (Java.TypeBound -> Expr)
writeTypeBound = def "writeTypeBound" $
  lambda "b" $
    cases Java._TypeBound (var "b") Nothing [
      Java._TypeBound_variable>>: lambda "tv" $ writeTypeVariable @@ var "tv",
      Java._TypeBound_classOrInterface>>: lambda "ci" $ lets [
        "cit">: project Java._TypeBound_ClassOrInterface Java._TypeBound_ClassOrInterface_type @@ var "ci",
        "additional">: project Java._TypeBound_ClassOrInterface Java._TypeBound_ClassOrInterface_additional @@ var "ci"] $
        Logic.ifElse (Lists.null (var "additional"))
          (writeClassOrInterfaceType @@ var "cit")
          (Serialization.spaceSep @@ Lists.cons (writeClassOrInterfaceType @@ var "cit") (Lists.map writeAdditionalBound (var "additional")))]

writeTypeParameter :: TBinding (Java.TypeParameter -> Expr)
writeTypeParameter = def "writeTypeParameter" $
  lambda "tp" $ lets [
    "mods">: project Java._TypeParameter Java._TypeParameter_modifiers @@ var "tp",
    "id">: project Java._TypeParameter Java._TypeParameter_identifier @@ var "tp",
    "bound">: project Java._TypeParameter Java._TypeParameter_bound @@ var "tp"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeTypeParameterModifier (var "mods")),
      just $ writeTypeIdentifier @@ var "id",
      Maybes.map (lambda "b" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "extends", writeTypeBound @@ var "b"]) (var "bound")])

writeWildcard :: TBinding (Java.Wildcard -> Expr)
writeWildcard = def "writeWildcard" $
  lambda "w" $ lets [
    "anns">: project Java._Wildcard Java._Wildcard_annotations @@ var "w",
    "mbounds">: project Java._Wildcard Java._Wildcard_wildcard @@ var "w"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "anns")) nothing (just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeAnnotation (var "anns")),
      just $ Serialization.cst @@ string "*",
      Maybes.map writeWildcardBounds (var "mbounds")])

-- =============================================================================
-- Declarations
-- =============================================================================

writeClassBody :: TBinding (Java.ClassBody -> Expr)
writeClassBody = def "writeClassBody" $
  lambda "cb" $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
      (Serialization.doubleNewlineSep @@ Lists.map writeClassBodyDeclarationWithComments (unwrap Java._ClassBody @@ var "cb"))

writeClassBodyDeclaration :: TBinding (Java.ClassBodyDeclaration -> Expr)
writeClassBodyDeclaration = def "writeClassBodyDeclaration" $
  lambda "d" $
    cases Java._ClassBodyDeclaration (var "d") Nothing [
      Java._ClassBodyDeclaration_classMember>>: lambda "d" $ writeClassMemberDeclaration @@ var "d",
      Java._ClassBodyDeclaration_instanceInitializer>>: lambda "i" $ writeInstanceInitializer @@ var "i",
      Java._ClassBodyDeclaration_staticInitializer>>: lambda "i" $ writeStaticInitializer @@ var "i",
      Java._ClassBodyDeclaration_constructorDeclaration>>: lambda "d" $ writeConstructorDeclaration @@ var "d"]

writeClassBodyDeclarationWithComments :: TBinding (Java.ClassBodyDeclarationWithComments -> Expr)
writeClassBodyDeclarationWithComments = def "writeClassBodyDeclarationWithComments" $
  lambda "cbdwc" $ lets [
    "d">: project Java._ClassBodyDeclarationWithComments Java._ClassBodyDeclarationWithComments_value @@ var "cbdwc",
    "mc">: project Java._ClassBodyDeclarationWithComments Java._ClassBodyDeclarationWithComments_comments @@ var "cbdwc"] $
    withComments @@ var "mc" @@ (writeClassBodyDeclaration @@ var "d")

writeClassMemberDeclaration :: TBinding (Java.ClassMemberDeclaration -> Expr)
writeClassMemberDeclaration = def "writeClassMemberDeclaration" $
  lambda "d" $
    cases Java._ClassMemberDeclaration (var "d") Nothing [
      Java._ClassMemberDeclaration_field>>: lambda "fd" $ writeFieldDeclaration @@ var "fd",
      Java._ClassMemberDeclaration_method>>: lambda "md" $ writeMethodDeclaration @@ var "md",
      Java._ClassMemberDeclaration_class>>: lambda "cd" $ writeClassDeclaration @@ var "cd",
      Java._ClassMemberDeclaration_interface>>: lambda "id" $ writeInterfaceDeclaration @@ var "id",
      Java._ClassMemberDeclaration_none>>: constant $ Serialization.cst @@ string ";"]

writeFieldDeclaration :: TBinding (Java.FieldDeclaration -> Expr)
writeFieldDeclaration = def "writeFieldDeclaration" $
  lambda "fd" $ lets [
    "mods">: project Java._FieldDeclaration Java._FieldDeclaration_modifiers @@ var "fd",
    "typ">: project Java._FieldDeclaration Java._FieldDeclaration_unannType @@ var "fd",
    "vars">: project Java._FieldDeclaration Java._FieldDeclaration_variableDeclarators @@ var "fd"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeFieldModifier (var "mods")),
      just $ writeUnannType @@ var "typ",
      just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeVariableDeclarator (var "vars")]))

writeVariableDeclarator :: TBinding (Java.VariableDeclarator -> Expr)
writeVariableDeclarator = def "writeVariableDeclarator" $
  lambda "vd" $ lets [
    "id">: project Java._VariableDeclarator Java._VariableDeclarator_id @@ var "vd",
    "minit">: project Java._VariableDeclarator Java._VariableDeclarator_initializer @@ var "vd",
    "idSec">: writeVariableDeclaratorId @@ var "id"] $
    Maybes.maybe (var "idSec")
      (lambda "init" $ Serialization.infixWs @@ string "=" @@ var "idSec" @@ (writeVariableInitializer @@ var "init"))
      (var "minit")

writeVariableDeclaratorId :: TBinding (Java.VariableDeclaratorId -> Expr)
writeVariableDeclaratorId = def "writeVariableDeclaratorId" $
  lambda "vdi" $ lets [
    "id">: project Java._VariableDeclaratorId Java._VariableDeclaratorId_identifier @@ var "vdi",
    "mdims">: project Java._VariableDeclaratorId Java._VariableDeclaratorId_dims @@ var "vdi"] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ writeIdentifier @@ var "id",
      Maybes.map writeDims (var "mdims")])

writeMethodDeclaration :: TBinding (Java.MethodDeclaration -> Expr)
writeMethodDeclaration = def "writeMethodDeclaration" $
  lambda "md" $ lets [
    "anns">: project Java._MethodDeclaration Java._MethodDeclaration_annotations @@ var "md",
    "mods">: project Java._MethodDeclaration Java._MethodDeclaration_modifiers @@ var "md",
    "header">: project Java._MethodDeclaration Java._MethodDeclaration_header @@ var "md",
    "body">: project Java._MethodDeclaration Java._MethodDeclaration_body @@ var "md",
    "headerAndBody">: Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeMethodModifier (var "mods")),
      just $ writeMethodHeader @@ var "header",
      just $ writeMethodBody @@ var "body"])] $
    Serialization.newlineSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "anns")) nothing (just $ Serialization.newlineSep @@ Lists.map writeAnnotation (var "anns")),
      just $ var "headerAndBody"])

writeMethodHeader :: TBinding (Java.MethodHeader -> Expr)
writeMethodHeader = def "writeMethodHeader" $
  lambda "mh" $ lets [
    "params">: project Java._MethodHeader Java._MethodHeader_parameters @@ var "mh",
    "result">: project Java._MethodHeader Java._MethodHeader_result @@ var "mh",
    "decl">: project Java._MethodHeader Java._MethodHeader_declarator @@ var "mh",
    "mthrows">: project Java._MethodHeader Java._MethodHeader_throws @@ var "mh"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "params")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map writeTypeParameter (var "params")),
      just $ writeResult @@ var "result",
      just $ writeMethodDeclarator @@ var "decl",
      Maybes.map writeThrows (var "mthrows")])

writeMethodDeclarator :: TBinding (Java.MethodDeclarator -> Expr)
writeMethodDeclarator = def "writeMethodDeclarator" $
  lambda "md" $ lets [
    "id">: project Java._MethodDeclarator Java._MethodDeclarator_identifier @@ var "md",
    "params">: project Java._MethodDeclarator Java._MethodDeclarator_formalParameters @@ var "md"] $
    Serialization.noSep @@ list [
      writeIdentifier @@ var "id",
      Serialization.parenList @@ false @@ Lists.map writeFormalParameter (var "params")]

writeFormalParameter_Simple :: TBinding (Java.FormalParameter_Simple -> Expr)
writeFormalParameter_Simple = def "writeFormalParameter_Simple" $
  lambda "fps" $ lets [
    "mods">: project Java._FormalParameter_Simple Java._FormalParameter_Simple_modifiers @@ var "fps",
    "typ">: project Java._FormalParameter_Simple Java._FormalParameter_Simple_type @@ var "fps",
    "id">: project Java._FormalParameter_Simple Java._FormalParameter_Simple_id @@ var "fps"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeVariableModifier (var "mods")),
      just $ writeUnannType @@ var "typ",
      just $ writeVariableDeclaratorId @@ var "id"])

writeConstructorDeclaration :: TBinding (Java.ConstructorDeclaration -> Expr)
writeConstructorDeclaration = def "writeConstructorDeclaration" $
  lambda "cd" $ lets [
    "mods">: project Java._ConstructorDeclaration Java._ConstructorDeclaration_modifiers @@ var "cd",
    "cons">: project Java._ConstructorDeclaration Java._ConstructorDeclaration_constructor @@ var "cd",
    "mthrows">: project Java._ConstructorDeclaration Java._ConstructorDeclaration_throws @@ var "cd",
    "body">: project Java._ConstructorDeclaration Java._ConstructorDeclaration_body @@ var "cd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeConstructorModifier (var "mods")),
      just $ writeConstructorDeclarator @@ var "cons",
      Maybes.map writeThrows (var "mthrows"),
      just $ writeConstructorBody @@ var "body"])

writeConstructorDeclarator :: TBinding (Java.ConstructorDeclarator -> Expr)
writeConstructorDeclarator = def "writeConstructorDeclarator" $
  lambda "cd" $ lets [
    "tparams">: project Java._ConstructorDeclarator Java._ConstructorDeclarator_parameters @@ var "cd",
    "name">: project Java._ConstructorDeclarator Java._ConstructorDeclarator_name @@ var "cd",
    "fparams">: project Java._ConstructorDeclarator Java._ConstructorDeclarator_formalParameters @@ var "cd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "tparams")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map writeTypeParameter (var "tparams")),
      just $ writeSimpleTypeName @@ var "name",
      just $ Serialization.parenList @@ false @@ Lists.map writeFormalParameter (var "fparams")])

writeConstructorBody :: TBinding (Java.ConstructorBody -> Expr)
writeConstructorBody = def "writeConstructorBody" $
  lambda "cb" $ lets [
    "minvoc">: project Java._ConstructorBody Java._ConstructorBody_invocation @@ var "cb",
    "stmts">: project Java._ConstructorBody Java._ConstructorBody_statements @@ var "cb"] $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@ (Serialization.doubleNewlineSep @@ Maybes.cat (list [
      Maybes.map writeExplicitConstructorInvocation (var "minvoc"),
      just $ Serialization.newlineSep @@ Lists.map writeBlockStatement (var "stmts")]))

-- =============================================================================
-- Interface declarations
-- =============================================================================

writeInterfaceBody :: TBinding (Java.InterfaceBody -> Expr)
writeInterfaceBody = def "writeInterfaceBody" $
  lambda "ib" $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
      (Serialization.doubleNewlineSep @@ Lists.map writeInterfaceMemberDeclaration (unwrap Java._InterfaceBody @@ var "ib"))

writeInterfaceMemberDeclaration :: TBinding (Java.InterfaceMemberDeclaration -> Expr)
writeInterfaceMemberDeclaration = def "writeInterfaceMemberDeclaration" $
  lambda "d" $
    cases Java._InterfaceMemberDeclaration (var "d") Nothing [
      Java._InterfaceMemberDeclaration_constant>>: lambda "c" $ writeConstantDeclaration @@ var "c",
      Java._InterfaceMemberDeclaration_interfaceMethod>>: lambda "im" $ writeInterfaceMethodDeclaration @@ var "im",
      Java._InterfaceMemberDeclaration_class>>: lambda "cd" $ writeClassDeclaration @@ var "cd",
      Java._InterfaceMemberDeclaration_interface>>: lambda "id" $ writeInterfaceDeclaration @@ var "id"]

writeInterfaceMethodDeclaration :: TBinding (Java.InterfaceMethodDeclaration -> Expr)
writeInterfaceMethodDeclaration = def "writeInterfaceMethodDeclaration" $
  lambda "imd" $ lets [
    "mods">: project Java._InterfaceMethodDeclaration Java._InterfaceMethodDeclaration_modifiers @@ var "imd",
    "header">: project Java._InterfaceMethodDeclaration Java._InterfaceMethodDeclaration_header @@ var "imd",
    "body">: project Java._InterfaceMethodDeclaration Java._InterfaceMethodDeclaration_body @@ var "imd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeInterfaceMethodModifier (var "mods")),
      just $ writeMethodHeader @@ var "header",
      just $ writeMethodBody @@ var "body"])

writeConstantDeclaration :: TBinding (Java.ConstantDeclaration -> Expr)
writeConstantDeclaration = def "writeConstantDeclaration" $
  lambda "cd" $ lets [
    "mods">: project Java._ConstantDeclaration Java._ConstantDeclaration_modifiers @@ var "cd",
    "typ">: project Java._ConstantDeclaration Java._ConstantDeclaration_type @@ var "cd",
    "vars">: project Java._ConstantDeclaration Java._ConstantDeclaration_variables @@ var "cd"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeConstantModifier (var "mods")),
      just $ writeUnannType @@ var "typ",
      just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeVariableDeclarator (var "vars")]))

-- =============================================================================
-- Normal class and interface declarations
-- =============================================================================

writeNormalClassDeclaration :: TBinding (Java.NormalClassDeclaration -> Expr)
writeNormalClassDeclaration = def "writeNormalClassDeclaration" $
  lambda "ncd" $ lets [
    "mods">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_modifiers @@ var "ncd",
    "id">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_identifier @@ var "ncd",
    "tparams">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_parameters @@ var "ncd",
    "msuperc">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_extends @@ var "ncd",
    "superi">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_implements @@ var "ncd",
    "body">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_body @@ var "ncd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeClassModifier (var "mods")),
      just $ Serialization.cst @@ string "class",
      just $ Serialization.noSep @@ Maybes.cat (list [
        just $ writeTypeIdentifier @@ var "id",
        Logic.ifElse (Lists.null (var "tparams")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map writeTypeParameter (var "tparams"))]),
      Maybes.map (lambda "c" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "extends", writeClassType @@ var "c"]) (var "msuperc"),
      Logic.ifElse (Lists.null (var "superi")) nothing
        (just $ Serialization.spaceSep @@ list [Serialization.cst @@ string "implements", Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeInterfaceType (var "superi")]),
      just $ writeClassBody @@ var "body"])

writeNormalInterfaceDeclaration :: TBinding (Java.NormalInterfaceDeclaration -> Expr)
writeNormalInterfaceDeclaration = def "writeNormalInterfaceDeclaration" $
  lambda "nid" $ lets [
    "mods">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_modifiers @@ var "nid",
    "id">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_identifier @@ var "nid",
    "tparams">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_parameters @@ var "nid",
    "extends">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_extends @@ var "nid",
    "body">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_body @@ var "nid"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writeInterfaceModifier (var "mods")),
      just $ Serialization.cst @@ string "interface",
      just $ Serialization.noSep @@ Maybes.cat (list [
        just $ writeTypeIdentifier @@ var "id",
        Logic.ifElse (Lists.null (var "tparams")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map writeTypeParameter (var "tparams"))]),
      Logic.ifElse (Lists.null (var "extends")) nothing
        (just $ Serialization.spaceSep @@ list [Serialization.cst @@ string "extends", Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeInterfaceType (var "extends")]),
      just $ writeInterfaceBody @@ var "body"])

writeTypeDeclarationWithComments :: TBinding (Java.TypeDeclarationWithComments -> Expr)
writeTypeDeclarationWithComments = def "writeTypeDeclarationWithComments" $
  lambda "tdwc" $ lets [
    "d">: project Java._TypeDeclarationWithComments Java._TypeDeclarationWithComments_value @@ var "tdwc",
    "mc">: project Java._TypeDeclarationWithComments Java._TypeDeclarationWithComments_comments @@ var "tdwc"] $
    withComments @@ var "mc" @@ (writeTypeDeclaration @@ var "d")

-- =============================================================================
-- Imports and compilation unit
-- =============================================================================

writePackageDeclaration :: TBinding (Java.PackageDeclaration -> Expr)
writePackageDeclaration = def "writePackageDeclaration" $
  lambda "pd" $ lets [
    "mods">: project Java._PackageDeclaration Java._PackageDeclaration_modifiers @@ var "pd",
    "ids">: project Java._PackageDeclaration Java._PackageDeclaration_identifiers @@ var "pd"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map writePackageModifier (var "mods")),
      just $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "package",
        Serialization.cst @@ (Strings.intercalate (string ".") (Lists.map (lambda "id" $ unwrap Java._Identifier @@ var "id") (var "ids")))]]))

writeImportDeclaration :: TBinding (Java.ImportDeclaration -> Expr)
writeImportDeclaration = def "writeImportDeclaration" $
  lambda "imp" $
    cases Java._ImportDeclaration (var "imp") Nothing [
      Java._ImportDeclaration_singleType>>: lambda "st" $
        Serialization.withSemi @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "import",
          writeTypeName @@ (unwrap Java._SingleTypeImportDeclaration @@ var "st")]),
      Java._ImportDeclaration_typeImportOnDemand>>: lambda "_" $ Serialization.cst @@ string "STUB:ImportDeclarationTypeImportOnDemand",
      Java._ImportDeclaration_singleStaticImport>>: lambda "_" $ Serialization.cst @@ string "STUB:ImportDeclarationSingleStaticImport",
      Java._ImportDeclaration_staticImportOnDemand>>: lambda "_" $ Serialization.cst @@ string "STUB:ImportDeclarationStaticImportOnDemand"]

writeCompilationUnit :: TBinding (Java.CompilationUnit -> Expr)
writeCompilationUnit = def "writeCompilationUnit" $
  lambda "u" $
    cases Java._CompilationUnit (var "u") Nothing [
      Java._CompilationUnit_ordinary>>: lambda "ocu" $ lets [
        "mpkg">: project Java._OrdinaryCompilationUnit Java._OrdinaryCompilationUnit_package @@ var "ocu",
        "imports">: project Java._OrdinaryCompilationUnit Java._OrdinaryCompilationUnit_imports @@ var "ocu",
        "types">: project Java._OrdinaryCompilationUnit Java._OrdinaryCompilationUnit_types @@ var "ocu",
        "warning">: just $ singleLineComment @@ Constants.warningAutoGeneratedFile,
        "pkgSec">: Maybes.map writePackageDeclaration (var "mpkg"),
        "importsSec">: Logic.ifElse (Lists.null (var "imports")) nothing
          (just $ Serialization.newlineSep @@ Lists.map writeImportDeclaration (var "imports")),
        "typesSec">: Logic.ifElse (Lists.null (var "types")) nothing
          (just $ Serialization.doubleNewlineSep @@ Lists.map writeTypeDeclarationWithComments (var "types"))] $
        Serialization.doubleNewlineSep @@ Maybes.cat (list [var "warning", var "pkgSec", var "importsSec", var "typesSec"])]

-- =============================================================================
-- Method invocation
-- =============================================================================

writeMethodInvocation :: TBinding (Java.MethodInvocation -> Expr)
writeMethodInvocation = def "writeMethodInvocation" $
  lambda "mi" $ lets [
    "header">: project Java._MethodInvocation Java._MethodInvocation_header @@ var "mi",
    "args">: project Java._MethodInvocation Java._MethodInvocation_arguments @@ var "mi",
    "argSec">: Serialization.parenList @@ true @@ Lists.map writeExpression (var "args"),
    "headerSec">: cases Java._MethodInvocation_Header (var "header") Nothing [
      Java._MethodInvocation_Header_simple>>: lambda "mname" $ writeMethodName @@ var "mname",
      Java._MethodInvocation_Header_complex>>: lambda "cx" $ lets [
        "cvar">: project Java._MethodInvocation_Complex Java._MethodInvocation_Complex_variant @@ var "cx",
        "targs">: project Java._MethodInvocation_Complex Java._MethodInvocation_Complex_typeArguments @@ var "cx",
        "cid">: project Java._MethodInvocation_Complex Java._MethodInvocation_Complex_identifier @@ var "cx",
        "idSec">: Serialization.noSep @@ Maybes.cat (list [
          Logic.ifElse (Lists.null (var "targs")) nothing
            (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map writeTypeArgument (var "targs")),
          just $ writeIdentifier @@ var "cid"])] $
        cases Java._MethodInvocation_Variant (var "cvar") Nothing [
          Java._MethodInvocation_Variant_type>>: lambda "tname" $ Serialization.dotSep @@ list [writeTypeName @@ var "tname", var "idSec"],
          Java._MethodInvocation_Variant_expression>>: lambda "en" $ Serialization.dotSep @@ list [writeExpressionName @@ var "en", var "idSec"],
          Java._MethodInvocation_Variant_primary>>: lambda "p" $ Serialization.dotSep @@ list [writePrimary @@ var "p", var "idSec"],
          Java._MethodInvocation_Variant_super>>: constant $ Serialization.dotSep @@ list [Serialization.cst @@ string "super", var "idSec"],
          Java._MethodInvocation_Variant_typeSuper>>: lambda "tname" $ Serialization.dotSep @@ list [writeTypeName @@ var "tname", Serialization.cst @@ string "super", var "idSec"]]]] $
    Serialization.noSep @@ list [var "headerSec", var "argSec"]

-- =============================================================================
-- Cast expressions
-- =============================================================================

writeCastExpression_NotPlusMinus :: TBinding (Java.CastExpression_NotPlusMinus -> Expr)
writeCastExpression_NotPlusMinus = def "writeCastExpression_NotPlusMinus" $
  lambda "npm" $ lets [
    "rb">: project Java._CastExpression_NotPlusMinus Java._CastExpression_NotPlusMinus_refAndBounds @@ var "npm",
    "ex">: project Java._CastExpression_NotPlusMinus Java._CastExpression_NotPlusMinus_expression @@ var "npm"] $
    Serialization.spaceSep @@ list [writeCastExpression_RefAndBounds @@ var "rb", writeUnaryExpression @@ var "ex"]

writeCastExpression_RefAndBounds :: TBinding (Java.CastExpression_RefAndBounds -> Expr)
writeCastExpression_RefAndBounds = def "writeCastExpression_RefAndBounds" $
  lambda "rab" $ lets [
    "rt">: project Java._CastExpression_RefAndBounds Java._CastExpression_RefAndBounds_type @@ var "rab",
    "adds">: project Java._CastExpression_RefAndBounds Java._CastExpression_RefAndBounds_bounds @@ var "rab"] $
    Serialization.parenList @@ false @@ list [Serialization.spaceSep @@ Maybes.cat (list [
      just $ writeReferenceType @@ var "rt",
      Logic.ifElse (Lists.null (var "adds")) nothing (just $ Serialization.spaceSep @@ Lists.map writeAdditionalBound (var "adds"))])]

writeCastExpression_Primitive :: TBinding (Java.CastExpression_Primitive -> Expr)
writeCastExpression_Primitive = def "writeCastExpression_Primitive" $
  lambda "cp" $ lets [
    "pt">: project Java._CastExpression_Primitive Java._CastExpression_Primitive_type @@ var "cp",
    "ex">: project Java._CastExpression_Primitive Java._CastExpression_Primitive_expression @@ var "cp"] $
    Serialization.spaceSep @@ list [
      Serialization.parenList @@ false @@ list [writePrimitiveTypeWithAnnotations @@ var "pt"],
      writeUnaryExpression @@ var "ex"]

-- =============================================================================
-- Class instance creation
-- =============================================================================

writeClassInstanceCreationExpression :: TBinding (Java.ClassInstanceCreationExpression -> Expr)
writeClassInstanceCreationExpression = def "writeClassInstanceCreationExpression" $
  lambda "cice" $ lets [
    "mqual">: project Java._ClassInstanceCreationExpression Java._ClassInstanceCreationExpression_qualifier @@ var "cice",
    "e">: project Java._ClassInstanceCreationExpression Java._ClassInstanceCreationExpression_expression @@ var "cice"] $
    Maybes.maybe
      (writeUnqualifiedClassInstanceCreationExpression @@ var "e")
      (lambda "q" $ Serialization.dotSep @@ list [writeClassInstanceCreationExpression_Qualifier @@ var "q", writeUnqualifiedClassInstanceCreationExpression @@ var "e"])
      (var "mqual")

writeClassInstanceCreationExpression_Qualifier :: TBinding (Java.ClassInstanceCreationExpression_Qualifier -> Expr)
writeClassInstanceCreationExpression_Qualifier = def "writeClassInstanceCreationExpression_Qualifier" $
  lambda "q" $
    cases Java._ClassInstanceCreationExpression_Qualifier (var "q") Nothing [
      Java._ClassInstanceCreationExpression_Qualifier_expression>>: lambda "en" $ writeExpressionName @@ var "en",
      Java._ClassInstanceCreationExpression_Qualifier_primary>>: lambda "p" $ writePrimary @@ var "p"]

writeUnqualifiedClassInstanceCreationExpression :: TBinding (Java.UnqualifiedClassInstanceCreationExpression -> Expr)
writeUnqualifiedClassInstanceCreationExpression = def "writeUnqualifiedClassInstanceCreationExpression" $
  lambda "ucice" $ lets [
    "targs">: project Java._UnqualifiedClassInstanceCreationExpression Java._UnqualifiedClassInstanceCreationExpression_typeArguments @@ var "ucice",
    "cit">: project Java._UnqualifiedClassInstanceCreationExpression Java._UnqualifiedClassInstanceCreationExpression_classOrInterface @@ var "ucice",
    "args">: project Java._UnqualifiedClassInstanceCreationExpression Java._UnqualifiedClassInstanceCreationExpression_arguments @@ var "ucice",
    "mbody">: project Java._UnqualifiedClassInstanceCreationExpression Java._UnqualifiedClassInstanceCreationExpression_body @@ var "ucice"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "new",
      Logic.ifElse (Lists.null (var "targs")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map writeTypeArgument (var "targs")),
      just $ Serialization.noSep @@ list [writeClassOrInterfaceTypeToInstantiate @@ var "cit", Serialization.parenList @@ false @@ Lists.map writeExpression (var "args")],
      Maybes.map writeClassBody (var "mbody")])

writeClassOrInterfaceTypeToInstantiate :: TBinding (Java.ClassOrInterfaceTypeToInstantiate -> Expr)
writeClassOrInterfaceTypeToInstantiate = def "writeClassOrInterfaceTypeToInstantiate" $
  lambda "coitti" $ lets [
    "ids">: project Java._ClassOrInterfaceTypeToInstantiate Java._ClassOrInterfaceTypeToInstantiate_identifiers @@ var "coitti",
    "margs">: project Java._ClassOrInterfaceTypeToInstantiate Java._ClassOrInterfaceTypeToInstantiate_typeArguments @@ var "coitti"] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ Serialization.dotSep @@ Lists.map writeAnnotatedIdentifier (var "ids"),
      Maybes.map writeTypeArgumentsOrDiamond (var "margs")])

-- =============================================================================
-- Array expressions
-- =============================================================================

writeArrayCreationExpression :: TBinding (Java.ArrayCreationExpression -> Expr)
writeArrayCreationExpression = def "writeArrayCreationExpression" $
  lambda "ace" $
    cases Java._ArrayCreationExpression (var "ace") Nothing [
      Java._ArrayCreationExpression_primitiveArray>>: lambda "pa" $ lets [
        "pt">: project Java._ArrayCreationExpression_PrimitiveArray Java._ArrayCreationExpression_PrimitiveArray_type @@ var "pa",
        "ai">: project Java._ArrayCreationExpression_PrimitiveArray Java._ArrayCreationExpression_PrimitiveArray_array @@ var "pa"] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "new",
          Serialization.noSep @@ list [writePrimitiveTypeWithAnnotations @@ var "pt", Serialization.cst @@ string "[]"],
          writeArrayInitializer @@ var "ai"],
      Java._ArrayCreationExpression_classOrInterfaceArray>>: lambda "_" $ Serialization.cst @@ string "STUB:ArrayCreationExpression",
      Java._ArrayCreationExpression_primitive>>: lambda "_" $ Serialization.cst @@ string "STUB:ArrayCreationExpression",
      Java._ArrayCreationExpression_classOrInterface>>: lambda "_" $ Serialization.cst @@ string "STUB:ArrayCreationExpression"]

writeArrayInitializer :: TBinding (Java.ArrayInitializer -> Expr)
writeArrayInitializer = def "writeArrayInitializer" $
  lambda "ai" $ lets [
    "groups">: unwrap Java._ArrayInitializer @@ var "ai"] $
    Logic.ifElse (Equality.equal (Lists.length (var "groups")) (int32 1))
      (Serialization.noSep @@ list [
        Serialization.cst @@ string "{",
        Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map writeVariableInitializer (Lists.head (var "groups")),
        Serialization.cst @@ string "}"])
      (Serialization.cst @@ string "{}")
