-- | Java serializer: converts Java AST to concrete syntax (source code).

module Hydra.Sources.Java.Serde where

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
import qualified Hydra.Dsl.Meta.Lib.Literals               as LibLiterals
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
import Hydra.Ast
import qualified Hydra.Java.Syntax as Java
import qualified Hydra.Sources.Java.Syntax as JavaSyntax


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.java.serde"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Constants.ns, Serialization.ns],
            moduleTypeDependencies = (JavaSyntax.ns:KernelTypes.kernelTypesNamespaces),
            moduleDescription = Just "Java serializer: converts Java AST to concrete syntax"}
  where
    definitions = [
      toDefinition additionalBoundToExpr,
      toDefinition additiveExpressionToExpr,
      toDefinition ambiguousNameToExpr,
      toDefinition andExpressionToExpr,
      toDefinition annotatedIdentifierToExpr,
      toDefinition annotationToExpr,
      toDefinition annotationTypeDeclarationToExpr,
      toDefinition arrayAccessToExpr,
      toDefinition arrayCreationExpressionToExpr,
      toDefinition arrayInitializerToExpr,
      toDefinition arrayTypeToExpr,
      toDefinition assertStatementToExpr,
      toDefinition assignmentExpressionToExpr,
      toDefinition assignmentToExpr,
      toDefinition blockStatementToExpr,
      toDefinition blockToExpr,
      toDefinition breakStatementToExpr,
      toDefinition castExpressionLambdaToExpr,
      toDefinition castExpressionNotPlusMinusToExpr,
      toDefinition castExpressionPrimitiveToExpr,
      toDefinition castExpressionRefAndBoundsToExpr,
      toDefinition castExpressionToExpr,
      toDefinition classBodyDeclarationToExpr,
      toDefinition classBodyDeclarationWithCommentsToExpr,
      toDefinition classBodyToExpr,
      toDefinition classDeclarationToExpr,
      toDefinition classInstanceCreationExpressionQualifierToExpr,
      toDefinition classInstanceCreationExpressionToExpr,
      toDefinition classLiteralToExpr,
      toDefinition classMemberDeclarationToExpr,
      toDefinition classModifierToExpr,
      toDefinition classOrInterfaceTypeToExpr,
      toDefinition classOrInterfaceTypeToInstantiateToExpr,
      toDefinition classTypeToExpr,
      toDefinition compilationUnitToExpr,
      toDefinition conditionalAndExpressionToExpr,
      toDefinition conditionalExpressionTernaryCondToExpr,
      toDefinition conditionalExpressionTernaryLambdaToExpr,
      toDefinition conditionalExpressionToExpr,
      toDefinition conditionalOrExpressionToExpr,
      toDefinition constantDeclarationToExpr,
      toDefinition constantModifierToExpr,
      toDefinition constructorBodyToExpr,
      toDefinition constructorDeclarationToExpr,
      toDefinition constructorDeclaratorToExpr,
      toDefinition constructorModifierToExpr,
      toDefinition continueStatementToExpr,
      toDefinition dimsToExpr,
      toDefinition doStatementToExpr,
      toDefinition elementValuePairToExpr,
      toDefinition elementValueToExpr,
      toDefinition enumDeclarationToExpr,
      toDefinition equalityExpressionToExpr,
      toDefinition escapeJavaChar,
      toDefinition escapeJavaString,
      toDefinition exclusiveOrExpressionToExpr,
      toDefinition explicitConstructorInvocationToExpr,
      toDefinition expressionNameToExpr,
      toDefinition expressionStatementToExpr,
      toDefinition expressionToExpr,
      toDefinition fieldAccessToExpr,
      toDefinition fieldDeclarationToExpr,
      toDefinition fieldModifierToExpr,
      toDefinition floatingPointLiteralToExpr,
      toDefinition floatingPointTypeToExpr,
      toDefinition forStatementToExpr,
      toDefinition formalParameterSimpleToExpr,
      toDefinition formalParameterToExpr,
      toDefinition hexDigit,
      toDefinition identifierToExpr,
      toDefinition ifThenElseStatementToExpr,
      toDefinition ifThenStatementToExpr,
      toDefinition importDeclarationToExpr,
      toDefinition inclusiveOrExpressionToExpr,
      toDefinition instanceInitializerToExpr,
      toDefinition integerLiteralToExpr,
      toDefinition integralTypeToExpr,
      toDefinition interfaceBodyToExpr,
      toDefinition interfaceDeclarationToExpr,
      toDefinition interfaceMemberDeclarationToExpr,
      toDefinition interfaceMethodDeclarationToExpr,
      toDefinition interfaceMethodModifierToExpr,
      toDefinition interfaceModifierToExpr,
      toDefinition interfaceTypeToExpr,
      toDefinition javaFloatLiteralText,
      toDefinition javaUnicodeEscape,
      toDefinition labeledStatementToExpr,
      toDefinition lambdaBodyToExpr,
      toDefinition lambdaExpressionToExpr,
      toDefinition lambdaParametersToExpr,
      toDefinition leftHandSideToExpr,
      toDefinition literalToExpr,
      toDefinition localNameToExpr,
      toDefinition localVariableDeclarationStatementToExpr,
      toDefinition localVariableDeclarationToExpr,
      toDefinition markerAnnotationToExpr,
      toDefinition methodBodyToExpr,
      toDefinition methodDeclarationToExpr,
      toDefinition methodDeclaratorToExpr,
      toDefinition methodHeaderToExpr,
      toDefinition methodInvocationToExpr,
      toDefinition methodModifierToExpr,
      toDefinition methodNameToExpr,
      toDefinition methodReferenceToExpr,
      toDefinition multiplicativeExpressionToExpr,
      toDefinition normalAnnotationToExpr,
      toDefinition normalClassDeclarationToExpr,
      toDefinition normalInterfaceDeclarationToExpr,
      toDefinition numericTypeToExpr,
      toDefinition packageDeclarationToExpr,
      toDefinition packageModifierToExpr,
      toDefinition packageNameToExpr,
      toDefinition packageOrTypeNameToExpr,
      toDefinition padHex4,
      toDefinition postDecrementExpressionToExpr,
      toDefinition postIncrementExpressionToExpr,
      toDefinition postfixExpressionToExpr,
      toDefinition preDecrementExpressionToExpr,
      toDefinition preIncrementExpressionToExpr,
      toDefinition primaryNoNewArrayExpressionExpressionToExpr,
      toDefinition primaryToExpr,
      toDefinition primitiveTypeToExpr,
      toDefinition primitiveTypeWithAnnotationsToExpr,
      toDefinition receiverParameterToExpr,
      toDefinition referenceTypeToExpr,
      toDefinition relationalExpressionGreaterThanEqualToExpr,
      toDefinition relationalExpressionGreaterThanToExpr,
      toDefinition relationalExpressionInstanceOfToExpr,
      toDefinition relationalExpressionLessThanEqualToExpr,
      toDefinition relationalExpressionLessThanToExpr,
      toDefinition relationalExpressionToExpr,
      toDefinition resultToExpr,
      toDefinition returnStatementToExpr,
      toDefinition sanitizeJavaComment,
      toDefinition shiftExpressionToExpr,
      toDefinition simpleTypeNameToExpr,
      toDefinition singleElementAnnotationToExpr,
      toDefinition singleLineComment,
      toDefinition statementExpressionToExpr,
      toDefinition statementToExpr,
      toDefinition statementWithoutTrailingSubstatementToExpr,
      toDefinition staticInitializerToExpr,
      toDefinition stringLiteralToExpr,
      toDefinition switchStatementToExpr,
      toDefinition synchronizedStatementToExpr,
      toDefinition throwStatementToExpr,
      toDefinition throwsToExpr,
      toDefinition tryStatementToExpr,
      toDefinition typeArgumentToExpr,
      toDefinition typeArgumentsOrDiamondToExpr,
      toDefinition typeBoundToExpr,
      toDefinition typeDeclarationToExpr,
      toDefinition typeDeclarationWithCommentsToExpr,
      toDefinition typeIdentifierToExpr,
      toDefinition typeNameToExpr,
      toDefinition typeParameterModifierToExpr,
      toDefinition typeParameterToExpr,
      toDefinition typeToExpr,
      toDefinition typeVariableToExpr,
      toDefinition unannTypeToExpr,
      toDefinition unaryExpressionNotPlusMinusToExpr,
      toDefinition unaryExpressionToExpr,
      toDefinition unqualifiedClassInstanceCreationExpressionToExpr,
      toDefinition variableArityParameterToExpr,
      toDefinition variableDeclaratorIdToExpr,
      toDefinition variableDeclaratorToExpr,
      toDefinition variableInitializerToExpr,
      toDefinition variableModifierToExpr,
      toDefinition whileStatementToExpr,
      toDefinition wildcardBoundsToExpr,
      toDefinition wildcardToExpr,
      toDefinition withComments]

-- | Escape a single character (given as its code point) for use in a Java string or char literal.
escapeJavaChar :: TTermDefinition (Int -> String)
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
escapeJavaString :: TTermDefinition (String -> String)
escapeJavaString = def "escapeJavaString" $
  lambda "s" $
    Strings.cat (Lists.map (lambda "c" $ escapeJavaChar @@ var "c") (Strings.toList (var "s")))

-- | Convert a value 0-15 to an uppercase hex digit character code.
hexDigit :: TTermDefinition (Int -> Int)
hexDigit = def "hexDigit" $
  lambda "n" $
    Logic.ifElse (Equality.lt (var "n") (int32 10))
      (Math.add (var "n") (int32 48))   -- '0' = 48
      (Math.add (Math.sub (var "n") (int32 10)) (int32 65))  -- 'A' = 65

-- | Convert an integer to a 4-digit uppercase hex string (zero-padded).
padHex4 :: TTermDefinition (Int -> String)
padHex4 = def "padHex4" $
  lambda "n" $
    "d3" <~ Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "n") (int32 4096)) $     -- n / 16^3
    "r3" <~ Maybes.fromMaybe (int32 0) (Math.maybeMod (var "n") (int32 4096)) $
    "d2" <~ Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "r3") (int32 256)) $      -- remainder / 16^2
    "r2" <~ Maybes.fromMaybe (int32 0) (Math.maybeMod (var "r3") (int32 256)) $
    "d1" <~ Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "r2") (int32 16)) $       -- remainder / 16
    "d0" <~ Maybes.fromMaybe (int32 0) (Math.maybeMod (var "r2") (int32 16)) $        -- remainder
    Strings.fromList (list [hexDigit @@ var "d3", hexDigit @@ var "d2",
                            hexDigit @@ var "d1", hexDigit @@ var "d0"])

-- | Convert an integer to a \\uXXXX escape sequence.
-- For supplementary plane characters (> 0xFFFF), produces a UTF-16 surrogate pair.
javaUnicodeEscape :: TTermDefinition (Int -> String)
javaUnicodeEscape = def "javaUnicodeEscape" $
  lambda "n" $
    Logic.ifElse (Equality.gt (var "n") (int32 65535))
      -- Supplementary plane: UTF-16 surrogate pair
      ("n'" <~ Math.sub (var "n") (int32 65536) $
       "hi" <~ Math.add (int32 55296) (Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "n'") (int32 1024))) $  -- 0xD800
       "lo" <~ Math.add (int32 56320) (Maybes.fromMaybe (int32 0) (Math.maybeMod (var "n'") (int32 1024))) $  -- 0xDC00
       Strings.cat2 (Strings.cat2 (string "\\u") (padHex4 @@ var "hi"))
                     (Strings.cat2 (string "\\u") (padHex4 @@ var "lo")))
      -- Basic multilingual plane
      (Strings.cat2 (string "\\u") (padHex4 @@ var "n"))

sanitizeJavaComment :: TTermDefinition (String -> String)
sanitizeJavaComment = def "sanitizeJavaComment" $
  doc "Sanitize a string for use in a Java comment" $
  lambda "s" $
    Strings.intercalate (string "&gt;")
      (Strings.splitOn (string ">")
        (Strings.intercalate (string "&lt;")
          (Strings.splitOn (string "<") (var "s"))))

singleLineComment :: TTermDefinition (String -> Expr)
singleLineComment = def "singleLineComment" $
  doc ("Create a single-line Java comment. Empty text emits `//` (no"
    <> " trailing space) so blank line comments don't carry trailing whitespace.") $
  lambda "c" $ lets [
    "sanitized">: sanitizeJavaComment @@ var "c"] $
    Serialization.cst @@ Logic.ifElse (Equality.equal (var "sanitized") (string ""))
      (string "//")
      (Strings.cat2 (string "// ") (var "sanitized"))

withComments :: TTermDefinition (Maybe String -> Expr -> Expr)
withComments = def "withComments" $
  doc ("Wrap an expression with optional Javadoc comments. Blank lines"
    <> " inside the doc body emit ` *` (no trailing space) instead of ` * `.") $
  lambda "mc" $ lambda "expr" $
    Maybes.maybe
      (var "expr")
      (lambda "c" $ Serialization.newlineSep @@ list [
        Serialization.cst @@ (Strings.cat2 (string "/**\n")
          (Strings.cat2
            (Strings.intercalate (string "\n")
              (Lists.map (lambda "l" $ Logic.ifElse (Equality.equal (var "l") (string ""))
                  (string " *")
                  (Strings.cat2 (string " * ") (var "l")))
                (Strings.lines (sanitizeJavaComment @@ var "c"))))
            (string "\n */"))),
        var "expr"])
      (var "mc")

identifierToExpr :: TTermDefinition (Java.Identifier -> Expr)
identifierToExpr = def "identifierToExpr" $
  lambda "id" $
    Serialization.cst @@ (unwrap Java._Identifier @@ var "id")

typeIdentifierToExpr :: TTermDefinition (Java.TypeIdentifier -> Expr)
typeIdentifierToExpr = def "typeIdentifierToExpr" $
  lambda "tid" $
    identifierToExpr @@ (unwrap Java._TypeIdentifier @@ var "tid")

simpleTypeNameToExpr :: TTermDefinition (Java.SimpleTypeName -> Expr)
simpleTypeNameToExpr = def "simpleTypeNameToExpr" $
  lambda "stn" $
    typeIdentifierToExpr @@ (unwrap Java._SimpleTypeName @@ var "stn")

methodNameToExpr :: TTermDefinition (Java.MethodName -> Expr)
methodNameToExpr = def "methodNameToExpr" $
  lambda "mn" $
    identifierToExpr @@ (unwrap Java._MethodName @@ var "mn")

unannTypeToExpr :: TTermDefinition (Java.UnannType -> Expr)
unannTypeToExpr = def "unannTypeToExpr" $
  lambda "ut" $
    typeToExpr @@ (unwrap Java._UnannType @@ var "ut")

interfaceTypeToExpr :: TTermDefinition (Java.InterfaceType -> Expr)
interfaceTypeToExpr = def "interfaceTypeToExpr" $
  lambda "it" $
    classTypeToExpr @@ (unwrap Java._InterfaceType @@ var "it")

packageModifierToExpr :: TTermDefinition (Java.PackageModifier -> Expr)
packageModifierToExpr = def "packageModifierToExpr" $
  lambda "pm" $
    annotationToExpr @@ (unwrap Java._PackageModifier @@ var "pm")

typeParameterModifierToExpr :: TTermDefinition (Java.TypeParameterModifier -> Expr)
typeParameterModifierToExpr = def "typeParameterModifierToExpr" $
  lambda "tpm" $
    annotationToExpr @@ (unwrap Java._TypeParameterModifier @@ var "tpm")


annotationTypeDeclarationToExpr :: TTermDefinition (Java.AnnotationTypeDeclaration -> Expr)
annotationTypeDeclarationToExpr = def "annotationTypeDeclarationToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:AnnotationTypeDeclaration"

arrayAccessToExpr :: TTermDefinition (Java.ArrayAccess -> Expr)
arrayAccessToExpr = def "arrayAccessToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:ArrayAccess"

assertStatementToExpr :: TTermDefinition (Java.AssertStatement -> Expr)
assertStatementToExpr = def "assertStatementToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:AssertStatement"

castExpressionLambdaToExpr :: TTermDefinition (Java.CastExpression_Lambda -> Expr)
castExpressionLambdaToExpr = def "castExpressionLambdaToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:CastExpression_Lambda"

classLiteralToExpr :: TTermDefinition (Java.ClassLiteral -> Expr)
classLiteralToExpr = def "classLiteralToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:ClassLiteral"

conditionalExpressionTernaryCondToExpr :: TTermDefinition (Java.ConditionalExpression_TernaryCond -> Expr)
conditionalExpressionTernaryCondToExpr = def "conditionalExpressionTernaryCondToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:ConditionalExpression_TernaryCond"

conditionalExpressionTernaryLambdaToExpr :: TTermDefinition (Java.ConditionalExpression_TernaryLambda -> Expr)
conditionalExpressionTernaryLambdaToExpr = def "conditionalExpressionTernaryLambdaToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:ConditionalExpression_TernaryLambda"

constantModifierToExpr :: TTermDefinition (Java.ConstantModifier -> Expr)
constantModifierToExpr = def "constantModifierToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:ConstantModifier"

doStatementToExpr :: TTermDefinition (Java.DoStatement -> Expr)
doStatementToExpr = def "doStatementToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:DoStatement"

enumDeclarationToExpr :: TTermDefinition (Java.EnumDeclaration -> Expr)
enumDeclarationToExpr = def "enumDeclarationToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:EnumDeclaration"

explicitConstructorInvocationToExpr :: TTermDefinition (Java.ExplicitConstructorInvocation -> Expr)
explicitConstructorInvocationToExpr = def "explicitConstructorInvocationToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:ExplicitConstructorInvocation"

forStatementToExpr :: TTermDefinition (Java.ForStatement -> Expr)
forStatementToExpr = def "forStatementToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:ForStatement"

ifThenElseStatementToExpr :: TTermDefinition (Java.IfThenElseStatement -> Expr)
ifThenElseStatementToExpr = def "ifThenElseStatementToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:IfThenElseStatement"

instanceInitializerToExpr :: TTermDefinition (Java.InstanceInitializer -> Expr)
instanceInitializerToExpr = def "instanceInitializerToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:InstanceInitializer"

labeledStatementToExpr :: TTermDefinition (Java.LabeledStatement -> Expr)
labeledStatementToExpr = def "labeledStatementToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:LabeledStatement"

methodReferenceToExpr :: TTermDefinition (Java.MethodReference -> Expr)
methodReferenceToExpr = def "methodReferenceToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:MethodReference"

postDecrementExpressionToExpr :: TTermDefinition (Java.PostDecrementExpression -> Expr)
postDecrementExpressionToExpr = def "postDecrementExpressionToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:PostDecrementExpression"

postIncrementExpressionToExpr :: TTermDefinition (Java.PostIncrementExpression -> Expr)
postIncrementExpressionToExpr = def "postIncrementExpressionToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:PostIncrementExpression"

preDecrementExpressionToExpr :: TTermDefinition (Java.PreDecrementExpression -> Expr)
preDecrementExpressionToExpr = def "preDecrementExpressionToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:PreDecrementExpression"

preIncrementExpressionToExpr :: TTermDefinition (Java.PreIncrementExpression -> Expr)
preIncrementExpressionToExpr = def "preIncrementExpressionToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:PreIncrementExpression"

receiverParameterToExpr :: TTermDefinition (Java.ReceiverParameter -> Expr)
receiverParameterToExpr = def "receiverParameterToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:ReceiverParameter"

staticInitializerToExpr :: TTermDefinition (Java.StaticInitializer -> Expr)
staticInitializerToExpr = def "staticInitializerToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:StaticInitializer"

switchStatementToExpr :: TTermDefinition (Java.SwitchStatement -> Expr)
switchStatementToExpr = def "switchStatementToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:SwitchStatement"

synchronizedStatementToExpr :: TTermDefinition (Java.SynchronizedStatement -> Expr)
synchronizedStatementToExpr = def "synchronizedStatementToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:SynchronizedStatement"

throwsToExpr :: TTermDefinition (Java.Throws -> Expr)
throwsToExpr = def "throwsToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:Throws"

tryStatementToExpr :: TTermDefinition (Java.TryStatement -> Expr)
tryStatementToExpr = def "tryStatementToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:TryStatement"

variableArityParameterToExpr :: TTermDefinition (Java.VariableArityParameter -> Expr)
variableArityParameterToExpr = def "variableArityParameterToExpr" $
  lambda "_" $ Serialization.cst @@ string "STUB:VariableArityParameter"

whileStatementToExpr :: TTermDefinition (Java.WhileStatement -> Expr)
whileStatementToExpr = def "whileStatementToExpr" $
  lambda "ws" $ lets [
    "mcond">: project Java._WhileStatement Java._WhileStatement_cond @@ var "ws",
    "body">: project Java._WhileStatement Java._WhileStatement_body @@ var "ws",
    "condSer">: Maybes.maybe
      (Serialization.cst @@ string "true")
      (lambda "c" $ expressionToExpr @@ var "c")
      (var "mcond")] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "while",
      Serialization.parenList @@ false @@ list [var "condSer"],
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@ (statementToExpr @@ var "body")]


typeToExpr :: TTermDefinition (Java.Type -> Expr)
typeToExpr = def "typeToExpr" $
  lambda "t" $
    cases Java._Type (var "t") Nothing [
      Java._Type_primitive>>: lambda "pt" $ primitiveTypeWithAnnotationsToExpr @@ var "pt",
      Java._Type_reference>>: lambda "rt" $ referenceTypeToExpr @@ var "rt"]

annotationToExpr :: TTermDefinition (Java.Annotation -> Expr)
annotationToExpr = def "annotationToExpr" $
  lambda "ann" $
    cases Java._Annotation (var "ann") Nothing [
      Java._Annotation_normal>>: lambda "n" $ normalAnnotationToExpr @@ var "n",
      Java._Annotation_marker>>: lambda "m" $ markerAnnotationToExpr @@ var "m",
      Java._Annotation_singleElement>>: lambda "s" $ singleElementAnnotationToExpr @@ var "s"]

primitiveTypeToExpr :: TTermDefinition (Java.PrimitiveType -> Expr)
primitiveTypeToExpr = def "primitiveTypeToExpr" $
  lambda "pt" $
    cases Java._PrimitiveType (var "pt") Nothing [
      Java._PrimitiveType_numeric>>: lambda "nt" $ numericTypeToExpr @@ var "nt",
      Java._PrimitiveType_boolean>>: constant $ Serialization.cst @@ string "boolean"]

numericTypeToExpr :: TTermDefinition (Java.NumericType -> Expr)
numericTypeToExpr = def "numericTypeToExpr" $
  lambda "nt" $
    cases Java._NumericType (var "nt") Nothing [
      Java._NumericType_integral>>: lambda "it" $ integralTypeToExpr @@ var "it",
      Java._NumericType_floatingPoint>>: lambda "ft" $ floatingPointTypeToExpr @@ var "ft"]

integralTypeToExpr :: TTermDefinition (Java.IntegralType -> Expr)
integralTypeToExpr = def "integralTypeToExpr" $
  lambda "t" $
    cases Java._IntegralType (var "t") Nothing [
      Java._IntegralType_byte>>: constant $ Serialization.cst @@ string "byte",
      Java._IntegralType_short>>: constant $ Serialization.cst @@ string "short",
      Java._IntegralType_int>>: constant $ Serialization.cst @@ string "int",
      Java._IntegralType_long>>: constant $ Serialization.cst @@ string "long",
      Java._IntegralType_char>>: constant $ Serialization.cst @@ string "char"]

floatingPointTypeToExpr :: TTermDefinition (Java.FloatingPointType -> Expr)
floatingPointTypeToExpr = def "floatingPointTypeToExpr" $
  lambda "ft" $
    cases Java._FloatingPointType (var "ft") Nothing [
      Java._FloatingPointType_float>>: constant $ Serialization.cst @@ string "float",
      Java._FloatingPointType_double>>: constant $ Serialization.cst @@ string "double"]

referenceTypeToExpr :: TTermDefinition (Java.ReferenceType -> Expr)
referenceTypeToExpr = def "referenceTypeToExpr" $
  lambda "rt" $
    cases Java._ReferenceType (var "rt") Nothing [
      Java._ReferenceType_classOrInterface>>: lambda "cit" $ classOrInterfaceTypeToExpr @@ var "cit",
      Java._ReferenceType_variable>>: lambda "v" $ typeVariableToExpr @@ var "v",
      Java._ReferenceType_array>>: lambda "at" $ arrayTypeToExpr @@ var "at"]

classOrInterfaceTypeToExpr :: TTermDefinition (Java.ClassOrInterfaceType -> Expr)
classOrInterfaceTypeToExpr = def "classOrInterfaceTypeToExpr" $
  lambda "cit" $
    cases Java._ClassOrInterfaceType (var "cit") Nothing [
      Java._ClassOrInterfaceType_class>>: lambda "ct" $ classTypeToExpr @@ var "ct",
      Java._ClassOrInterfaceType_interface>>: lambda "it" $ interfaceTypeToExpr @@ var "it"]

expressionToExpr :: TTermDefinition (Java.Expression -> Expr)
expressionToExpr = def "expressionToExpr" $
  lambda "e" $
    cases Java._Expression (var "e") Nothing [
      Java._Expression_lambda>>: lambda "l" $ lambdaExpressionToExpr @@ var "l",
      Java._Expression_assignment>>: lambda "a" $ assignmentExpressionToExpr @@ var "a"]

assignmentExpressionToExpr :: TTermDefinition (Java.AssignmentExpression -> Expr)
assignmentExpressionToExpr = def "assignmentExpressionToExpr" $
  lambda "e" $
    cases Java._AssignmentExpression (var "e") Nothing [
      Java._AssignmentExpression_conditional>>: lambda "c" $ conditionalExpressionToExpr @@ var "c",
      Java._AssignmentExpression_assignment>>: lambda "a" $ assignmentToExpr @@ var "a"]

conditionalExpressionToExpr :: TTermDefinition (Java.ConditionalExpression -> Expr)
conditionalExpressionToExpr = def "conditionalExpressionToExpr" $
  lambda "c" $
    cases Java._ConditionalExpression (var "c") Nothing [
      Java._ConditionalExpression_simple>>: lambda "co" $ conditionalOrExpressionToExpr @@ var "co",
      Java._ConditionalExpression_ternaryCond>>: lambda "tc" $ conditionalExpressionTernaryCondToExpr @@ var "tc",
      Java._ConditionalExpression_ternaryLambda>>: lambda "tl" $ conditionalExpressionTernaryLambdaToExpr @@ var "tl"]

resultToExpr :: TTermDefinition (Java.Result -> Expr)
resultToExpr = def "resultToExpr" $
  lambda "r" $
    cases Java._Result (var "r") Nothing [
      Java._Result_type>>: lambda "t" $ unannTypeToExpr @@ var "t",
      Java._Result_void>>: constant $ Serialization.cst @@ string "void"]

methodBodyToExpr :: TTermDefinition (Java.MethodBody -> Expr)
methodBodyToExpr = def "methodBodyToExpr" $
  lambda "b" $
    cases Java._MethodBody (var "b") Nothing [
      Java._MethodBody_block>>: lambda "block" $ blockToExpr @@ var "block",
      Java._MethodBody_none>>: constant $ Serialization.cst @@ string ";"]

classDeclarationToExpr :: TTermDefinition (Java.ClassDeclaration -> Expr)
classDeclarationToExpr = def "classDeclarationToExpr" $
  lambda "d" $
    cases Java._ClassDeclaration (var "d") Nothing [
      Java._ClassDeclaration_normal>>: lambda "nd" $ normalClassDeclarationToExpr @@ var "nd",
      Java._ClassDeclaration_enum>>: lambda "ed" $ enumDeclarationToExpr @@ var "ed"]

interfaceDeclarationToExpr :: TTermDefinition (Java.InterfaceDeclaration -> Expr)
interfaceDeclarationToExpr = def "interfaceDeclarationToExpr" $
  lambda "d" $
    cases Java._InterfaceDeclaration (var "d") Nothing [
      Java._InterfaceDeclaration_normalInterface>>: lambda "n" $ normalInterfaceDeclarationToExpr @@ var "n",
      Java._InterfaceDeclaration_annotationType>>: lambda "a" $ annotationTypeDeclarationToExpr @@ var "a"]

castExpressionToExpr :: TTermDefinition (Java.CastExpression -> Expr)
castExpressionToExpr = def "castExpressionToExpr" $
  lambda "e" $
    cases Java._CastExpression (var "e") Nothing [
      Java._CastExpression_primitive>>: lambda "p" $ castExpressionPrimitiveToExpr @@ var "p",
      Java._CastExpression_notPlusMinus>>: lambda "npm" $ castExpressionNotPlusMinusToExpr @@ var "npm",
      Java._CastExpression_lambda>>: lambda "l" $ castExpressionLambdaToExpr @@ var "l"]

formalParameterToExpr :: TTermDefinition (Java.FormalParameter -> Expr)
formalParameterToExpr = def "formalParameterToExpr" $
  lambda "p" $
    cases Java._FormalParameter (var "p") Nothing [
      Java._FormalParameter_simple>>: lambda "s" $ formalParameterSimpleToExpr @@ var "s",
      Java._FormalParameter_variableArity>>: lambda "v" $ variableArityParameterToExpr @@ var "v"]

localNameToExpr :: TTermDefinition (Java.LocalVariableType -> Expr)
localNameToExpr = def "localNameToExpr" $
  lambda "t" $
    cases Java._LocalVariableType (var "t") Nothing [
      Java._LocalVariableType_type>>: lambda "ut" $ unannTypeToExpr @@ var "ut",
      Java._LocalVariableType_var>>: constant $ Serialization.cst @@ string "var"]

variableInitializerToExpr :: TTermDefinition (Java.VariableInitializer -> Expr)
variableInitializerToExpr = def "variableInitializerToExpr" $
  lambda "i" $
    cases Java._VariableInitializer (var "i") Nothing [
      Java._VariableInitializer_expression>>: lambda "e" $ expressionToExpr @@ var "e",
      Java._VariableInitializer_arrayInitializer>>: lambda "ai" $ arrayInitializerToExpr @@ var "ai"]

variableModifierToExpr :: TTermDefinition (Java.VariableModifier -> Expr)
variableModifierToExpr = def "variableModifierToExpr" $
  lambda "m" $
    cases Java._VariableModifier (var "m") Nothing [
      Java._VariableModifier_annotation>>: lambda "ann" $ annotationToExpr @@ var "ann",
      Java._VariableModifier_final>>: constant $ Serialization.cst @@ string "final"]

leftHandSideToExpr :: TTermDefinition (Java.LeftHandSide -> Expr)
leftHandSideToExpr = def "leftHandSideToExpr" $
  lambda "lhs" $
    cases Java._LeftHandSide (var "lhs") Nothing [
      Java._LeftHandSide_expressionName>>: lambda "en" $ expressionNameToExpr @@ var "en",
      Java._LeftHandSide_fieldAccess>>: lambda "fa" $ fieldAccessToExpr @@ var "fa",
      Java._LeftHandSide_arrayAccess>>: lambda "aa" $ arrayAccessToExpr @@ var "aa"]

lambdaBodyToExpr :: TTermDefinition (Java.LambdaBody -> Expr)
lambdaBodyToExpr = def "lambdaBodyToExpr" $
  lambda "b" $
    cases Java._LambdaBody (var "b") Nothing [
      Java._LambdaBody_expression>>: lambda "e" $ expressionToExpr @@ var "e",
      Java._LambdaBody_block>>: lambda "b" $ blockToExpr @@ var "b"]

lambdaParametersToExpr :: TTermDefinition (Java.LambdaParameters -> Expr)
lambdaParametersToExpr = def "lambdaParametersToExpr" $
  lambda "p" $
    cases Java._LambdaParameters (var "p") Nothing [
      Java._LambdaParameters_tuple>>: lambda "l" $ Serialization.parenList @@ false @@ Lists.map lambdaParametersToExpr (var "l"),
      Java._LambdaParameters_single>>: lambda "id" $ identifierToExpr @@ var "id"]

typeArgumentToExpr :: TTermDefinition (Java.TypeArgument -> Expr)
typeArgumentToExpr = def "typeArgumentToExpr" $
  lambda "a" $
    cases Java._TypeArgument (var "a") Nothing [
      Java._TypeArgument_reference>>: lambda "rt" $ referenceTypeToExpr @@ var "rt",
      Java._TypeArgument_wildcard>>: lambda "w" $ wildcardToExpr @@ var "w"]

typeDeclarationToExpr :: TTermDefinition (Java.TypeDeclaration -> Expr)
typeDeclarationToExpr = def "typeDeclarationToExpr" $
  lambda "d" $
    cases Java._TypeDeclaration (var "d") Nothing [
      Java._TypeDeclaration_class>>: lambda "d" $ classDeclarationToExpr @@ var "d",
      Java._TypeDeclaration_interface>>: lambda "d" $ interfaceDeclarationToExpr @@ var "d",
      Java._TypeDeclaration_none>>: constant $ Serialization.cst @@ string ";"]

primaryToExpr :: TTermDefinition (Java.Primary -> Expr)
primaryToExpr = def "primaryToExpr" $
  lambda "p" $
    cases Java._Primary (var "p") Nothing [
      Java._Primary_noNewArray>>: lambda "n" $ primaryNoNewArrayExpressionExpressionToExpr @@ var "n",
      Java._Primary_arrayCreation>>: lambda "a" $ arrayCreationExpressionToExpr @@ var "a"]

wildcardBoundsToExpr :: TTermDefinition (Java.WildcardBounds -> Expr)
wildcardBoundsToExpr = def "wildcardBoundsToExpr" $
  lambda "b" $
    cases Java._WildcardBounds (var "b") Nothing [
      Java._WildcardBounds_extends>>: lambda "rt" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "extends", referenceTypeToExpr @@ var "rt"],
      Java._WildcardBounds_super>>: lambda "rt" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "super", referenceTypeToExpr @@ var "rt"]]

typeArgumentsOrDiamondToExpr :: TTermDefinition (Java.TypeArgumentsOrDiamond -> Expr)
typeArgumentsOrDiamondToExpr = def "typeArgumentsOrDiamondToExpr" $
  lambda "targs" $
    cases Java._TypeArgumentsOrDiamond (var "targs") Nothing [
      Java._TypeArgumentsOrDiamond_arguments>>: lambda "args" $
        Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map typeArgumentToExpr (var "args"),
      Java._TypeArgumentsOrDiamond_diamond>>: constant $ Serialization.cst @@ string "<>"]


floatingPointLiteralToExpr :: TTermDefinition (Java.FloatingPointLiteral -> Expr)
floatingPointLiteralToExpr = def "floatingPointLiteralToExpr" $
  lambda "fl" $
    Serialization.cst @@ (javaFloatLiteralText @@ LibLiterals.showBigfloat (unwrap Java._FloatingPointLiteral @@ var "fl"))

-- | Convert a showBigfloat result into valid Java source syntax, mapping
-- NaN and ±Infinity to Double.NaN / Double.POSITIVE_INFINITY / Double.NEGATIVE_INFINITY.
javaFloatLiteralText :: TTermDefinition (String -> String)
javaFloatLiteralText = def "javaFloatLiteralText" $
  lambda "s" $
    Logic.ifElse (Equality.equal (var "s") (string "NaN")) (string "Double.NaN") $
    Logic.ifElse (Equality.equal (var "s") (string "Infinity")) (string "Double.POSITIVE_INFINITY") $
    Logic.ifElse (Equality.equal (var "s") (string "-Infinity")) (string "Double.NEGATIVE_INFINITY")
      (var "s")

integerLiteralToExpr :: TTermDefinition (Java.IntegerLiteral -> Expr)
integerLiteralToExpr = def "integerLiteralToExpr" $
  lambda "il" $ lets [
    "i">: unwrap Java._IntegerLiteral @@ var "il",
    "suffix">: Logic.ifElse (Logic.or
        (Equality.gt (var "i") (bigint 2147483647))
        (Equality.lt (var "i") (bigint (-2147483648))))
      (string "L")
      (string "")] $
    Serialization.cst @@ Strings.cat2 (LibLiterals.showBigint (var "i")) (var "suffix")

stringLiteralToExpr :: TTermDefinition (Java.StringLiteral -> Expr)
stringLiteralToExpr = def "stringLiteralToExpr" $
  doc "Serialize a Java string literal with proper Unicode escaping." $
  lambda "sl" $
    "s" <~ unwrap Java._StringLiteral @@ var "sl" $
    Serialization.cst @@ Strings.cat2 (string "\"") (Strings.cat2 (escapeJavaString @@ var "s") (string "\""))

literalToExpr :: TTermDefinition (Java.Literal -> Expr)
literalToExpr = def "literalToExpr" $
  lambda "l" $
    cases Java._Literal (var "l") Nothing [
      Java._Literal_null>>: constant $ Serialization.cst @@ string "null",
      Java._Literal_integer>>: lambda "il" $ integerLiteralToExpr @@ var "il",
      Java._Literal_floatingPoint>>: lambda "fl" $ floatingPointLiteralToExpr @@ var "fl",
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
      Java._Literal_string>>: lambda "sl" $ stringLiteralToExpr @@ var "sl"]


ambiguousNameToExpr :: TTermDefinition (Java.AmbiguousName -> Expr)
ambiguousNameToExpr = def "ambiguousNameToExpr" $
  lambda "an" $
    Serialization.dotSep @@ Lists.map identifierToExpr (unwrap Java._AmbiguousName @@ var "an")

expressionNameToExpr :: TTermDefinition (Java.ExpressionName -> Expr)
expressionNameToExpr = def "expressionNameToExpr" $
  lambda "en" $ lets [
    "mqual">: project Java._ExpressionName Java._ExpressionName_qualifier @@ var "en",
    "id">: project Java._ExpressionName Java._ExpressionName_identifier @@ var "en"] $
    Serialization.dotSep @@ Maybes.cat (list [
      Maybes.map ambiguousNameToExpr (var "mqual"),
      just $ identifierToExpr @@ var "id"])

typeNameToExpr :: TTermDefinition (Java.TypeName -> Expr)
typeNameToExpr = def "typeNameToExpr" $
  lambda "tn" $ lets [
    "id">: project Java._TypeName Java._TypeName_identifier @@ var "tn",
    "mqual">: project Java._TypeName Java._TypeName_qualifier @@ var "tn"] $
    Serialization.dotSep @@ Maybes.cat (list [
      Maybes.map packageOrTypeNameToExpr (var "mqual"),
      just $ typeIdentifierToExpr @@ var "id"])

packageNameToExpr :: TTermDefinition (Java.PackageName -> Expr)
packageNameToExpr = def "packageNameToExpr" $
  lambda "pn" $
    Serialization.dotSep @@ Lists.map identifierToExpr (unwrap Java._PackageName @@ var "pn")

packageOrTypeNameToExpr :: TTermDefinition (Java.PackageOrTypeName -> Expr)
packageOrTypeNameToExpr = def "packageOrTypeNameToExpr" $
  lambda "potn" $
    Serialization.dotSep @@ Lists.map identifierToExpr (unwrap Java._PackageOrTypeName @@ var "potn")

annotatedIdentifierToExpr :: TTermDefinition (Java.AnnotatedIdentifier -> Expr)
annotatedIdentifierToExpr = def "annotatedIdentifierToExpr" $
  lambda "ai" $
    identifierToExpr @@ (project Java._AnnotatedIdentifier Java._AnnotatedIdentifier_identifier @@ var "ai")

typeVariableToExpr :: TTermDefinition (Java.TypeVariable -> Expr)
typeVariableToExpr = def "typeVariableToExpr" $
  lambda "tv" $ lets [
    "anns">: project Java._TypeVariable Java._TypeVariable_annotations @@ var "tv",
    "id">: project Java._TypeVariable Java._TypeVariable_identifier @@ var "tv"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "anns"))
        nothing
        (just $ Serialization.spaceSep @@ Lists.map annotationToExpr (var "anns")),
      just $ typeIdentifierToExpr @@ var "id"])


classModifierToExpr :: TTermDefinition (Java.ClassModifier -> Expr)
classModifierToExpr = def "classModifierToExpr" $
  lambda "m" $
    cases Java._ClassModifier (var "m") Nothing [
      Java._ClassModifier_annotation>>: lambda "ann" $ annotationToExpr @@ var "ann",
      Java._ClassModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._ClassModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._ClassModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._ClassModifier_abstract>>: constant $ Serialization.cst @@ string "abstract",
      Java._ClassModifier_static>>: constant $ Serialization.cst @@ string "static",
      Java._ClassModifier_final>>: constant $ Serialization.cst @@ string "final",
      Java._ClassModifier_strictfp>>: constant $ Serialization.cst @@ string "strictfp"]

fieldModifierToExpr :: TTermDefinition (Java.FieldModifier -> Expr)
fieldModifierToExpr = def "fieldModifierToExpr" $
  lambda "m" $
    cases Java._FieldModifier (var "m") Nothing [
      Java._FieldModifier_annotation>>: lambda "ann" $ annotationToExpr @@ var "ann",
      Java._FieldModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._FieldModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._FieldModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._FieldModifier_static>>: constant $ Serialization.cst @@ string "static",
      Java._FieldModifier_final>>: constant $ Serialization.cst @@ string "final",
      Java._FieldModifier_transient>>: constant $ Serialization.cst @@ string "transient",
      Java._FieldModifier_volatile>>: constant $ Serialization.cst @@ string "volatile"]

methodModifierToExpr :: TTermDefinition (Java.MethodModifier -> Expr)
methodModifierToExpr = def "methodModifierToExpr" $
  lambda "m" $
    cases Java._MethodModifier (var "m") Nothing [
      Java._MethodModifier_annotation>>: lambda "ann" $ annotationToExpr @@ var "ann",
      Java._MethodModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._MethodModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._MethodModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._MethodModifier_abstract>>: constant $ Serialization.cst @@ string "abstract",
      Java._MethodModifier_final>>: constant $ Serialization.cst @@ string "final",
      Java._MethodModifier_synchronized>>: constant $ Serialization.cst @@ string "synchronized",
      Java._MethodModifier_native>>: constant $ Serialization.cst @@ string "native",
      Java._MethodModifier_strictfb>>: constant $ Serialization.cst @@ string "strictfb"]

constructorModifierToExpr :: TTermDefinition (Java.ConstructorModifier -> Expr)
constructorModifierToExpr = def "constructorModifierToExpr" $
  lambda "m" $
    cases Java._ConstructorModifier (var "m") Nothing [
      Java._ConstructorModifier_annotation>>: lambda "ann" $ annotationToExpr @@ var "ann",
      Java._ConstructorModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._ConstructorModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._ConstructorModifier_private>>: constant $ Serialization.cst @@ string "private"]

interfaceModifierToExpr :: TTermDefinition (Java.InterfaceModifier -> Expr)
interfaceModifierToExpr = def "interfaceModifierToExpr" $
  lambda "m" $
    cases Java._InterfaceModifier (var "m") Nothing [
      Java._InterfaceModifier_annotation>>: lambda "a" $ annotationToExpr @@ var "a",
      Java._InterfaceModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._InterfaceModifier_protected>>: constant $ Serialization.cst @@ string "protected",
      Java._InterfaceModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._InterfaceModifier_abstract>>: constant $ Serialization.cst @@ string "abstract",
      Java._InterfaceModifier_static>>: constant $ Serialization.cst @@ string "static",
      Java._InterfaceModifier_strictfb>>: constant $ Serialization.cst @@ string "strictfb"]

interfaceMethodModifierToExpr :: TTermDefinition (Java.InterfaceMethodModifier -> Expr)
interfaceMethodModifierToExpr = def "interfaceMethodModifierToExpr" $
  lambda "m" $
    cases Java._InterfaceMethodModifier (var "m") Nothing [
      Java._InterfaceMethodModifier_annotation>>: lambda "a" $ annotationToExpr @@ var "a",
      Java._InterfaceMethodModifier_public>>: constant $ Serialization.cst @@ string "public",
      Java._InterfaceMethodModifier_private>>: constant $ Serialization.cst @@ string "private",
      Java._InterfaceMethodModifier_abstract>>: constant $ Serialization.cst @@ string "abstract",
      Java._InterfaceMethodModifier_default>>: constant $ Serialization.cst @@ string "default",
      Java._InterfaceMethodModifier_static>>: constant $ Serialization.cst @@ string "static",
      Java._InterfaceMethodModifier_strictfp>>: constant $ Serialization.cst @@ string "strictfp"]


additionalBoundToExpr :: TTermDefinition (Java.AdditionalBound -> Expr)
additionalBoundToExpr = def "additionalBoundToExpr" $
  lambda "ab" $
    Serialization.spaceSep @@ list [Serialization.cst @@ string "&", interfaceTypeToExpr @@ (unwrap Java._AdditionalBound @@ var "ab")]

additiveExpressionToExpr :: TTermDefinition (Java.AdditiveExpression -> Expr)
additiveExpressionToExpr = def "additiveExpressionToExpr" $
  lambda "e" $
    cases Java._AdditiveExpression (var "e") Nothing [
      Java._AdditiveExpression_unary>>: lambda "m" $ multiplicativeExpressionToExpr @@ var "m",
      Java._AdditiveExpression_plus>>: lambda "b" $
        Serialization.infixWs @@ string "+" @@
          (additiveExpressionToExpr @@ (project Java._AdditiveExpression_Binary Java._AdditiveExpression_Binary_lhs @@ var "b")) @@
          (multiplicativeExpressionToExpr @@ (project Java._AdditiveExpression_Binary Java._AdditiveExpression_Binary_rhs @@ var "b")),
      Java._AdditiveExpression_minus>>: lambda "b" $
        Serialization.infixWs @@ string "-" @@
          (additiveExpressionToExpr @@ (project Java._AdditiveExpression_Binary Java._AdditiveExpression_Binary_lhs @@ var "b")) @@
          (multiplicativeExpressionToExpr @@ (project Java._AdditiveExpression_Binary Java._AdditiveExpression_Binary_rhs @@ var "b"))]

multiplicativeExpressionToExpr :: TTermDefinition (Java.MultiplicativeExpression -> Expr)
multiplicativeExpressionToExpr = def "multiplicativeExpressionToExpr" $
  lambda "e" $
    cases Java._MultiplicativeExpression (var "e") Nothing [
      Java._MultiplicativeExpression_unary>>: lambda "u" $ unaryExpressionToExpr @@ var "u",
      Java._MultiplicativeExpression_times>>: lambda "b" $
        Serialization.infixWs @@ string "*" @@
          (multiplicativeExpressionToExpr @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_lhs @@ var "b")) @@
          (unaryExpressionToExpr @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_rhs @@ var "b")),
      Java._MultiplicativeExpression_divide>>: lambda "b" $
        Serialization.infixWs @@ string "/" @@
          (multiplicativeExpressionToExpr @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_lhs @@ var "b")) @@
          (unaryExpressionToExpr @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_rhs @@ var "b")),
      Java._MultiplicativeExpression_mod>>: lambda "b" $
        Serialization.infixWs @@ string "%" @@
          (multiplicativeExpressionToExpr @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_lhs @@ var "b")) @@
          (unaryExpressionToExpr @@ (project Java._MultiplicativeExpression_Binary Java._MultiplicativeExpression_Binary_rhs @@ var "b"))]

unaryExpressionToExpr :: TTermDefinition (Java.UnaryExpression -> Expr)
unaryExpressionToExpr = def "unaryExpressionToExpr" $
  lambda "e" $
    cases Java._UnaryExpression (var "e") Nothing [
      Java._UnaryExpression_preIncrement>>: lambda "pi" $ preIncrementExpressionToExpr @@ var "pi",
      Java._UnaryExpression_preDecrement>>: lambda "pd" $ preDecrementExpressionToExpr @@ var "pd",
      Java._UnaryExpression_plus>>: lambda "p" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "+", unaryExpressionToExpr @@ var "p"],
      Java._UnaryExpression_minus>>: lambda "m" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "-", unaryExpressionToExpr @@ var "m"],
      Java._UnaryExpression_other>>: lambda "o" $ unaryExpressionNotPlusMinusToExpr @@ var "o"]

unaryExpressionNotPlusMinusToExpr :: TTermDefinition (Java.UnaryExpressionNotPlusMinus -> Expr)
unaryExpressionNotPlusMinusToExpr = def "unaryExpressionNotPlusMinusToExpr" $
  lambda "e" $
    cases Java._UnaryExpressionNotPlusMinus (var "e") Nothing [
      Java._UnaryExpressionNotPlusMinus_postfix>>: lambda "p" $ postfixExpressionToExpr @@ var "p",
      Java._UnaryExpressionNotPlusMinus_tilde>>: lambda "u" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "~", unaryExpressionToExpr @@ var "u"],
      Java._UnaryExpressionNotPlusMinus_not>>: lambda "u" $ Serialization.noSep @@ list [Serialization.cst @@ string "!", unaryExpressionToExpr @@ var "u"],
      Java._UnaryExpressionNotPlusMinus_cast>>: lambda "c" $ castExpressionToExpr @@ var "c"]

postfixExpressionToExpr :: TTermDefinition (Java.PostfixExpression -> Expr)
postfixExpressionToExpr = def "postfixExpressionToExpr" $
  lambda "e" $
    cases Java._PostfixExpression (var "e") Nothing [
      Java._PostfixExpression_primary>>: lambda "p" $ primaryToExpr @@ var "p",
      Java._PostfixExpression_name>>: lambda "en" $ expressionNameToExpr @@ var "en",
      Java._PostfixExpression_postIncrement>>: lambda "pi" $ postIncrementExpressionToExpr @@ var "pi",
      Java._PostfixExpression_postDecrement>>: lambda "pd" $ postDecrementExpressionToExpr @@ var "pd"]

andExpressionToExpr :: TTermDefinition (Java.AndExpression -> Expr)
andExpressionToExpr = def "andExpressionToExpr" $
  lambda "ae" $
    Serialization.infixWsList @@ string "&" @@ Lists.map equalityExpressionToExpr (unwrap Java._AndExpression @@ var "ae")

exclusiveOrExpressionToExpr :: TTermDefinition (Java.ExclusiveOrExpression -> Expr)
exclusiveOrExpressionToExpr = def "exclusiveOrExpressionToExpr" $
  lambda "eoe" $
    Serialization.infixWsList @@ string "^" @@ Lists.map andExpressionToExpr (unwrap Java._ExclusiveOrExpression @@ var "eoe")

inclusiveOrExpressionToExpr :: TTermDefinition (Java.InclusiveOrExpression -> Expr)
inclusiveOrExpressionToExpr = def "inclusiveOrExpressionToExpr" $
  lambda "ioe" $
    Serialization.infixWsList @@ string "|" @@ Lists.map exclusiveOrExpressionToExpr (unwrap Java._InclusiveOrExpression @@ var "ioe")

conditionalAndExpressionToExpr :: TTermDefinition (Java.ConditionalAndExpression -> Expr)
conditionalAndExpressionToExpr = def "conditionalAndExpressionToExpr" $
  lambda "cae" $
    Serialization.infixWsList @@ string "&&" @@ Lists.map inclusiveOrExpressionToExpr (unwrap Java._ConditionalAndExpression @@ var "cae")

conditionalOrExpressionToExpr :: TTermDefinition (Java.ConditionalOrExpression -> Expr)
conditionalOrExpressionToExpr = def "conditionalOrExpressionToExpr" $
  lambda "coe" $
    Serialization.infixWsList @@ string "||" @@ Lists.map conditionalAndExpressionToExpr (unwrap Java._ConditionalOrExpression @@ var "coe")

equalityExpressionToExpr :: TTermDefinition (Java.EqualityExpression -> Expr)
equalityExpressionToExpr = def "equalityExpressionToExpr" $
  lambda "e" $
    cases Java._EqualityExpression (var "e") Nothing [
      Java._EqualityExpression_unary>>: lambda "r" $ relationalExpressionToExpr @@ var "r",
      Java._EqualityExpression_equal>>: lambda "b" $
        Serialization.infixWs @@ string "==" @@
          (equalityExpressionToExpr @@ (project Java._EqualityExpression_Binary Java._EqualityExpression_Binary_lhs @@ var "b")) @@
          (relationalExpressionToExpr @@ (project Java._EqualityExpression_Binary Java._EqualityExpression_Binary_rhs @@ var "b")),
      Java._EqualityExpression_notEqual>>: lambda "b" $
        Serialization.infixWs @@ string "!=" @@
          (equalityExpressionToExpr @@ (project Java._EqualityExpression_Binary Java._EqualityExpression_Binary_lhs @@ var "b")) @@
          (relationalExpressionToExpr @@ (project Java._EqualityExpression_Binary Java._EqualityExpression_Binary_rhs @@ var "b"))]

relationalExpressionToExpr :: TTermDefinition (Java.RelationalExpression -> Expr)
relationalExpressionToExpr = def "relationalExpressionToExpr" $
  lambda "e" $
    cases Java._RelationalExpression (var "e") Nothing [
      Java._RelationalExpression_simple>>: lambda "s" $ shiftExpressionToExpr @@ var "s",
      Java._RelationalExpression_lessThan>>: lambda "lt" $ relationalExpressionLessThanToExpr @@ var "lt",
      Java._RelationalExpression_greaterThan>>: lambda "gt" $ relationalExpressionGreaterThanToExpr @@ var "gt",
      Java._RelationalExpression_lessThanEqual>>: lambda "lte" $ relationalExpressionLessThanEqualToExpr @@ var "lte",
      Java._RelationalExpression_greaterThanEqual>>: lambda "gte" $ relationalExpressionGreaterThanEqualToExpr @@ var "gte",
      Java._RelationalExpression_instanceof>>: lambda "i" $ relationalExpressionInstanceOfToExpr @@ var "i"]

relationalExpressionLessThanToExpr :: TTermDefinition (Java.RelationalExpression_LessThan -> Expr)
relationalExpressionLessThanToExpr = def "relationalExpressionLessThanToExpr" $
  lambda "lt" $
    Serialization.infixWs @@ string "<" @@
      (relationalExpressionToExpr @@ (project Java._RelationalExpression_LessThan Java._RelationalExpression_LessThan_lhs @@ var "lt")) @@
      (shiftExpressionToExpr @@ (project Java._RelationalExpression_LessThan Java._RelationalExpression_LessThan_rhs @@ var "lt"))

relationalExpressionGreaterThanToExpr :: TTermDefinition (Java.RelationalExpression_GreaterThan -> Expr)
relationalExpressionGreaterThanToExpr = def "relationalExpressionGreaterThanToExpr" $
  lambda "gt" $
    Serialization.infixWs @@ string ">" @@
      (relationalExpressionToExpr @@ (project Java._RelationalExpression_GreaterThan Java._RelationalExpression_GreaterThan_lhs @@ var "gt")) @@
      (shiftExpressionToExpr @@ (project Java._RelationalExpression_GreaterThan Java._RelationalExpression_GreaterThan_rhs @@ var "gt"))

relationalExpressionLessThanEqualToExpr :: TTermDefinition (Java.RelationalExpression_LessThanEqual -> Expr)
relationalExpressionLessThanEqualToExpr = def "relationalExpressionLessThanEqualToExpr" $
  lambda "lte" $
    Serialization.infixWs @@ string "<=" @@
      (relationalExpressionToExpr @@ (project Java._RelationalExpression_LessThanEqual Java._RelationalExpression_LessThanEqual_lhs @@ var "lte")) @@
      (shiftExpressionToExpr @@ (project Java._RelationalExpression_LessThanEqual Java._RelationalExpression_LessThanEqual_rhs @@ var "lte"))

relationalExpressionGreaterThanEqualToExpr :: TTermDefinition (Java.RelationalExpression_GreaterThanEqual -> Expr)
relationalExpressionGreaterThanEqualToExpr = def "relationalExpressionGreaterThanEqualToExpr" $
  lambda "gte" $
    Serialization.infixWs @@ string ">=" @@
      (relationalExpressionToExpr @@ (project Java._RelationalExpression_GreaterThanEqual Java._RelationalExpression_GreaterThanEqual_lhs @@ var "gte")) @@
      (shiftExpressionToExpr @@ (project Java._RelationalExpression_GreaterThanEqual Java._RelationalExpression_GreaterThanEqual_rhs @@ var "gte"))

relationalExpressionInstanceOfToExpr :: TTermDefinition (Java.RelationalExpression_InstanceOf -> Expr)
relationalExpressionInstanceOfToExpr = def "relationalExpressionInstanceOfToExpr" $
  lambda "io" $
    Serialization.infixWs @@ string "instanceof" @@
      (relationalExpressionToExpr @@ (project Java._RelationalExpression_InstanceOf Java._RelationalExpression_InstanceOf_lhs @@ var "io")) @@
      (referenceTypeToExpr @@ (project Java._RelationalExpression_InstanceOf Java._RelationalExpression_InstanceOf_rhs @@ var "io"))

shiftExpressionToExpr :: TTermDefinition (Java.ShiftExpression -> Expr)
shiftExpressionToExpr = def "shiftExpressionToExpr" $
  lambda "e" $
    cases Java._ShiftExpression (var "e") Nothing [
      Java._ShiftExpression_unary>>: lambda "a" $ additiveExpressionToExpr @@ var "a",
      Java._ShiftExpression_shiftLeft>>: lambda "b" $
        Serialization.infixWs @@ string "<<" @@
          (shiftExpressionToExpr @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_lhs @@ var "b")) @@
          (additiveExpressionToExpr @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_rhs @@ var "b")),
      Java._ShiftExpression_shiftRight>>: lambda "b" $
        Serialization.infixWs @@ string ">>" @@
          (shiftExpressionToExpr @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_lhs @@ var "b")) @@
          (additiveExpressionToExpr @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_rhs @@ var "b")),
      Java._ShiftExpression_shiftRightZeroFill>>: lambda "b" $
        Serialization.infixWs @@ string ">>>" @@
          (shiftExpressionToExpr @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_lhs @@ var "b")) @@
          (additiveExpressionToExpr @@ (project Java._ShiftExpression_Binary Java._ShiftExpression_Binary_rhs @@ var "b"))]

lambdaExpressionToExpr :: TTermDefinition (Java.LambdaExpression -> Expr)
lambdaExpressionToExpr = def "lambdaExpressionToExpr" $
  lambda "le" $ lets [
    "params">: project Java._LambdaExpression Java._LambdaExpression_parameters @@ var "le",
    "body">: project Java._LambdaExpression Java._LambdaExpression_body @@ var "le"] $
    Serialization.infixWs @@ string "->" @@ (lambdaParametersToExpr @@ var "params") @@ (lambdaBodyToExpr @@ var "body")

assignmentToExpr :: TTermDefinition (Java.Assignment -> Expr)
assignmentToExpr = def "assignmentToExpr" $
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
    Serialization.infixWs @@ var "ctop" @@ (leftHandSideToExpr @@ var "lhs") @@ (expressionToExpr @@ var "rhs")


statementToExpr :: TTermDefinition (Java.Statement -> Expr)
statementToExpr = def "statementToExpr" $
  lambda "s" $
    cases Java._Statement (var "s") Nothing [
      Java._Statement_withoutTrailing>>: lambda "s" $ statementWithoutTrailingSubstatementToExpr @@ var "s",
      Java._Statement_labeled>>: lambda "l" $ labeledStatementToExpr @@ var "l",
      Java._Statement_ifThen>>: lambda "it" $ ifThenStatementToExpr @@ var "it",
      Java._Statement_ifThenElse>>: lambda "ite" $ ifThenElseStatementToExpr @@ var "ite",
      Java._Statement_while>>: lambda "w" $ whileStatementToExpr @@ var "w",
      Java._Statement_for>>: lambda "f" $ forStatementToExpr @@ var "f"]

statementWithoutTrailingSubstatementToExpr :: TTermDefinition (Java.StatementWithoutTrailingSubstatement -> Expr)
statementWithoutTrailingSubstatementToExpr = def "statementWithoutTrailingSubstatementToExpr" $
  lambda "s" $
    cases Java._StatementWithoutTrailingSubstatement (var "s") Nothing [
      Java._StatementWithoutTrailingSubstatement_block>>: lambda "b" $ blockToExpr @@ var "b",
      Java._StatementWithoutTrailingSubstatement_empty>>: constant $ Serialization.cst @@ string ";",
      Java._StatementWithoutTrailingSubstatement_expression>>: lambda "e" $ expressionStatementToExpr @@ var "e",
      Java._StatementWithoutTrailingSubstatement_assert>>: lambda "a" $ assertStatementToExpr @@ var "a",
      Java._StatementWithoutTrailingSubstatement_switch>>: lambda "s" $ switchStatementToExpr @@ var "s",
      Java._StatementWithoutTrailingSubstatement_do>>: lambda "d" $ doStatementToExpr @@ var "d",
      Java._StatementWithoutTrailingSubstatement_break>>: lambda "b" $ breakStatementToExpr @@ var "b",
      Java._StatementWithoutTrailingSubstatement_continue>>: lambda "c" $ continueStatementToExpr @@ var "c",
      Java._StatementWithoutTrailingSubstatement_return>>: lambda "r" $ returnStatementToExpr @@ var "r",
      Java._StatementWithoutTrailingSubstatement_synchronized>>: lambda "s" $ synchronizedStatementToExpr @@ var "s",
      Java._StatementWithoutTrailingSubstatement_throw>>: lambda "t" $ throwStatementToExpr @@ var "t",
      Java._StatementWithoutTrailingSubstatement_try>>: lambda "t" $ tryStatementToExpr @@ var "t"]

statementExpressionToExpr :: TTermDefinition (Java.StatementExpression -> Expr)
statementExpressionToExpr = def "statementExpressionToExpr" $
  lambda "e" $
    cases Java._StatementExpression (var "e") Nothing [
      Java._StatementExpression_assignment>>: lambda "a" $ assignmentToExpr @@ var "a",
      Java._StatementExpression_preIncrement>>: lambda "pi" $ preIncrementExpressionToExpr @@ var "pi",
      Java._StatementExpression_preDecrement>>: lambda "pd" $ preDecrementExpressionToExpr @@ var "pd",
      Java._StatementExpression_postIncrement>>: lambda "pi" $ postIncrementExpressionToExpr @@ var "pi",
      Java._StatementExpression_postDecrement>>: lambda "pd" $ postDecrementExpressionToExpr @@ var "pd",
      Java._StatementExpression_methodInvocation>>: lambda "m" $ methodInvocationToExpr @@ var "m",
      Java._StatementExpression_classInstanceCreation>>: lambda "cic" $ classInstanceCreationExpressionToExpr @@ var "cic"]

expressionStatementToExpr :: TTermDefinition (Java.ExpressionStatement -> Expr)
expressionStatementToExpr = def "expressionStatementToExpr" $
  lambda "es" $
    Serialization.withSemi @@ (statementExpressionToExpr @@ (unwrap Java._ExpressionStatement @@ var "es"))

blockToExpr :: TTermDefinition (Java.Block -> Expr)
blockToExpr = def "blockToExpr" $
  lambda "b" $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@ (Serialization.newlineSep @@ Lists.map blockStatementToExpr (unwrap Java._Block @@ var "b"))

blockStatementToExpr :: TTermDefinition (Java.BlockStatement -> Expr)
blockStatementToExpr = def "blockStatementToExpr" $
  lambda "s" $
    cases Java._BlockStatement (var "s") Nothing [
      Java._BlockStatement_localVariableDeclaration>>: lambda "d" $ localVariableDeclarationStatementToExpr @@ var "d",
      Java._BlockStatement_class>>: lambda "cd" $ classDeclarationToExpr @@ var "cd",
      Java._BlockStatement_statement>>: lambda "s" $ statementToExpr @@ var "s"]

localVariableDeclarationStatementToExpr :: TTermDefinition (Java.LocalVariableDeclarationStatement -> Expr)
localVariableDeclarationStatementToExpr = def "localVariableDeclarationStatementToExpr" $
  lambda "lvds" $
    Serialization.withSemi @@ (localVariableDeclarationToExpr @@ (unwrap Java._LocalVariableDeclarationStatement @@ var "lvds"))

localVariableDeclarationToExpr :: TTermDefinition (Java.LocalVariableDeclaration -> Expr)
localVariableDeclarationToExpr = def "localVariableDeclarationToExpr" $
  lambda "lvd" $ lets [
    "mods">: project Java._LocalVariableDeclaration Java._LocalVariableDeclaration_modifiers @@ var "lvd",
    "t">: project Java._LocalVariableDeclaration Java._LocalVariableDeclaration_type @@ var "lvd",
    "decls">: project Java._LocalVariableDeclaration Java._LocalVariableDeclaration_declarators @@ var "lvd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map variableModifierToExpr (var "mods")),
      just $ localNameToExpr @@ var "t",
      just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map variableDeclaratorToExpr (var "decls")])

returnStatementToExpr :: TTermDefinition (Java.ReturnStatement -> Expr)
returnStatementToExpr = def "returnStatementToExpr" $
  lambda "rs" $ lets [
    "mex">: unwrap Java._ReturnStatement @@ var "rs"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "return",
      Maybes.map expressionToExpr (var "mex")]))

throwStatementToExpr :: TTermDefinition (Java.ThrowStatement -> Expr)
throwStatementToExpr = def "throwStatementToExpr" $
  lambda "ts" $
    Serialization.withSemi @@ (Serialization.spaceSep @@ list [
      Serialization.cst @@ string "throw",
      expressionToExpr @@ (unwrap Java._ThrowStatement @@ var "ts")])

breakStatementToExpr :: TTermDefinition (Java.BreakStatement -> Expr)
breakStatementToExpr = def "breakStatementToExpr" $
  lambda "bs" $ lets [
    "mlabel">: unwrap Java._BreakStatement @@ var "bs"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "break",
      Maybes.map identifierToExpr (var "mlabel")]))

continueStatementToExpr :: TTermDefinition (Java.ContinueStatement -> Expr)
continueStatementToExpr = def "continueStatementToExpr" $
  lambda "cs" $ lets [
    "mlabel">: unwrap Java._ContinueStatement @@ var "cs"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "continue",
      Maybes.map identifierToExpr (var "mlabel")]))

ifThenStatementToExpr :: TTermDefinition (Java.IfThenStatement -> Expr)
ifThenStatementToExpr = def "ifThenStatementToExpr" $
  lambda "its" $ lets [
    "cond">: project Java._IfThenStatement Java._IfThenStatement_expression @@ var "its",
    "thn">: project Java._IfThenStatement Java._IfThenStatement_statement @@ var "its"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "if",
      Serialization.parenList @@ false @@ list [expressionToExpr @@ var "cond"],
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@ (statementToExpr @@ var "thn")]


primaryNoNewArrayExpressionExpressionToExpr :: TTermDefinition (Java.PrimaryNoNewArrayExpression -> Expr)
primaryNoNewArrayExpressionExpressionToExpr = def "primaryNoNewArrayExpressionExpressionToExpr" $
  lambda "p" $
    cases Java._PrimaryNoNewArrayExpression (var "p") Nothing [
      Java._PrimaryNoNewArrayExpression_literal>>: lambda "l" $ literalToExpr @@ var "l",
      Java._PrimaryNoNewArrayExpression_classLiteral>>: lambda "cl" $ classLiteralToExpr @@ var "cl",
      Java._PrimaryNoNewArrayExpression_this>>: constant $ Serialization.cst @@ string "this",
      Java._PrimaryNoNewArrayExpression_dotThis>>: lambda "n" $ Serialization.dotSep @@ list [typeNameToExpr @@ var "n", Serialization.cst @@ string "this"],
      Java._PrimaryNoNewArrayExpression_parens>>: lambda "e" $ Serialization.parenList @@ false @@ list [expressionToExpr @@ var "e"],
      Java._PrimaryNoNewArrayExpression_classInstance>>: lambda "ci" $ classInstanceCreationExpressionToExpr @@ var "ci",
      Java._PrimaryNoNewArrayExpression_fieldAccess>>: lambda "fa" $ fieldAccessToExpr @@ var "fa",
      Java._PrimaryNoNewArrayExpression_arrayAccess>>: lambda "aa" $ arrayAccessToExpr @@ var "aa",
      Java._PrimaryNoNewArrayExpression_methodInvocation>>: lambda "mi" $ methodInvocationToExpr @@ var "mi",
      Java._PrimaryNoNewArrayExpression_methodReference>>: lambda "mr" $ methodReferenceToExpr @@ var "mr"]

fieldAccessToExpr :: TTermDefinition (Java.FieldAccess -> Expr)
fieldAccessToExpr = def "fieldAccessToExpr" $
  lambda "fa" $ lets [
    "qual">: project Java._FieldAccess Java._FieldAccess_qualifier @@ var "fa",
    "id">: project Java._FieldAccess Java._FieldAccess_identifier @@ var "fa"] $
    cases Java._FieldAccess_Qualifier (var "qual") Nothing [
      Java._FieldAccess_Qualifier_primary>>: lambda "p" $ Serialization.dotSep @@ list [primaryToExpr @@ var "p", identifierToExpr @@ var "id"],
      Java._FieldAccess_Qualifier_super>>: constant $ Serialization.dotSep @@ list [Serialization.cst @@ string "super", identifierToExpr @@ var "id"],
      Java._FieldAccess_Qualifier_typed>>: lambda "tn" $ Serialization.dotSep @@ list [typeNameToExpr @@ var "tn", Serialization.cst @@ string "super", identifierToExpr @@ var "id"]]


markerAnnotationToExpr :: TTermDefinition (Java.MarkerAnnotation -> Expr)
markerAnnotationToExpr = def "markerAnnotationToExpr" $
  lambda "ma" $
    Serialization.prefix @@ string "@" @@ (typeNameToExpr @@ (unwrap Java._MarkerAnnotation @@ var "ma"))

normalAnnotationToExpr :: TTermDefinition (Java.NormalAnnotation -> Expr)
normalAnnotationToExpr = def "normalAnnotationToExpr" $
  lambda "na" $ lets [
    "tname">: project Java._NormalAnnotation Java._NormalAnnotation_typeName @@ var "na",
    "pairs">: project Java._NormalAnnotation Java._NormalAnnotation_pairs @@ var "na"] $
    Serialization.prefix @@ string "@" @@ (Serialization.noSep @@ list [
      typeNameToExpr @@ var "tname",
      Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map elementValuePairToExpr (var "pairs")])

singleElementAnnotationToExpr :: TTermDefinition (Java.SingleElementAnnotation -> Expr)
singleElementAnnotationToExpr = def "singleElementAnnotationToExpr" $
  lambda "sea" $ lets [
    "tname">: project Java._SingleElementAnnotation Java._SingleElementAnnotation_name @@ var "sea",
    "mv">: project Java._SingleElementAnnotation Java._SingleElementAnnotation_value @@ var "sea"] $
    Maybes.maybe
      (markerAnnotationToExpr @@ (wrap Java._MarkerAnnotation (var "tname")))
      (lambda "v" $ Serialization.prefix @@ string "@" @@ (Serialization.noSep @@ list [
        typeNameToExpr @@ var "tname",
        Serialization.parenList @@ false @@ list [elementValueToExpr @@ var "v"]]))
      (var "mv")

elementValueToExpr :: TTermDefinition (Java.ElementValue -> Expr)
elementValueToExpr = def "elementValueToExpr" $
  lambda "ev" $
    cases Java._ElementValue (var "ev") Nothing [
      Java._ElementValue_conditionalExpression>>: lambda "c" $ conditionalExpressionToExpr @@ var "c",
      Java._ElementValue_elementValueArrayInitializer>>: lambda "evai" $
        Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map elementValueToExpr (unwrap Java._ElementValueArrayInitializer @@ var "evai"),
      Java._ElementValue_annotation>>: lambda "ann" $ annotationToExpr @@ var "ann"]

elementValuePairToExpr :: TTermDefinition (Java.ElementValuePair -> Expr)
elementValuePairToExpr = def "elementValuePairToExpr" $
  lambda "evp" $ lets [
    "k">: project Java._ElementValuePair Java._ElementValuePair_key @@ var "evp",
    "v">: project Java._ElementValuePair Java._ElementValuePair_value @@ var "evp"] $
    Serialization.infixWs @@ string "=" @@ (identifierToExpr @@ var "k") @@ (elementValueToExpr @@ var "v")


primitiveTypeWithAnnotationsToExpr :: TTermDefinition (Java.PrimitiveTypeWithAnnotations -> Expr)
primitiveTypeWithAnnotationsToExpr = def "primitiveTypeWithAnnotationsToExpr" $
  lambda "ptwa" $ lets [
    "pt">: project Java._PrimitiveTypeWithAnnotations Java._PrimitiveTypeWithAnnotations_type @@ var "ptwa",
    "anns">: project Java._PrimitiveTypeWithAnnotations Java._PrimitiveTypeWithAnnotations_annotations @@ var "ptwa"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "anns")) nothing (just $ Serialization.spaceSep @@ Lists.map annotationToExpr (var "anns")),
      just $ primitiveTypeToExpr @@ var "pt"])

classTypeToExpr :: TTermDefinition (Java.ClassType -> Expr)
classTypeToExpr = def "classTypeToExpr" $
  lambda "ct" $ lets [
    "anns">: project Java._ClassType Java._ClassType_annotations @@ var "ct",
    "qual">: project Java._ClassType Java._ClassType_qualifier @@ var "ct",
    "id">: project Java._ClassType Java._ClassType_identifier @@ var "ct",
    "args">: project Java._ClassType Java._ClassType_arguments @@ var "ct",
    "qualifiedId">: cases Java._ClassTypeQualifier (var "qual") Nothing [
      Java._ClassTypeQualifier_none>>: constant $ typeIdentifierToExpr @@ var "id",
      Java._ClassTypeQualifier_package>>: lambda "pkg" $ Serialization.dotSep @@ list [packageNameToExpr @@ var "pkg", typeIdentifierToExpr @@ var "id"],
      Java._ClassTypeQualifier_parent>>: lambda "cit" $ Serialization.dotSep @@ list [classOrInterfaceTypeToExpr @@ var "cit", typeIdentifierToExpr @@ var "id"]]] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ Serialization.spaceSep @@ Maybes.cat (list [
        Logic.ifElse (Lists.null (var "anns")) nothing (just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map annotationToExpr (var "anns")),
        just $ var "qualifiedId"]),
      Logic.ifElse (Lists.null (var "args")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map typeArgumentToExpr (var "args"))])

arrayTypeToExpr :: TTermDefinition (Java.ArrayType -> Expr)
arrayTypeToExpr = def "arrayTypeToExpr" $
  lambda "at" $ lets [
    "dims">: project Java._ArrayType Java._ArrayType_dims @@ var "at",
    "variant">: project Java._ArrayType Java._ArrayType_variant @@ var "at",
    "varExpr">: cases Java._ArrayType_Variant (var "variant") Nothing [
      Java._ArrayType_Variant_primitive>>: lambda "pt" $ primitiveTypeWithAnnotationsToExpr @@ var "pt",
      Java._ArrayType_Variant_classOrInterface>>: lambda "cit" $ classOrInterfaceTypeToExpr @@ var "cit",
      Java._ArrayType_Variant_variable>>: lambda "tv" $ typeVariableToExpr @@ var "tv"]] $
    Serialization.noSep @@ list [var "varExpr", dimsToExpr @@ var "dims"]

dimsToExpr :: TTermDefinition (Java.Dims -> Expr)
dimsToExpr = def "dimsToExpr" $
  lambda "d" $
    Serialization.noSep @@ Lists.map (lambda "_" $ Serialization.cst @@ string "[]") (unwrap Java._Dims @@ var "d")

typeBoundToExpr :: TTermDefinition (Java.TypeBound -> Expr)
typeBoundToExpr = def "typeBoundToExpr" $
  lambda "b" $
    cases Java._TypeBound (var "b") Nothing [
      Java._TypeBound_variable>>: lambda "tv" $ typeVariableToExpr @@ var "tv",
      Java._TypeBound_classOrInterface>>: lambda "ci" $ lets [
        "cit">: project Java._TypeBound_ClassOrInterface Java._TypeBound_ClassOrInterface_type @@ var "ci",
        "additional">: project Java._TypeBound_ClassOrInterface Java._TypeBound_ClassOrInterface_additional @@ var "ci"] $
        Logic.ifElse (Lists.null (var "additional"))
          (classOrInterfaceTypeToExpr @@ var "cit")
          (Serialization.spaceSep @@ Lists.cons (classOrInterfaceTypeToExpr @@ var "cit") (Lists.map additionalBoundToExpr (var "additional")))]

typeParameterToExpr :: TTermDefinition (Java.TypeParameter -> Expr)
typeParameterToExpr = def "typeParameterToExpr" $
  lambda "tp" $ lets [
    "mods">: project Java._TypeParameter Java._TypeParameter_modifiers @@ var "tp",
    "id">: project Java._TypeParameter Java._TypeParameter_identifier @@ var "tp",
    "bound">: project Java._TypeParameter Java._TypeParameter_bound @@ var "tp"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map typeParameterModifierToExpr (var "mods")),
      just $ typeIdentifierToExpr @@ var "id",
      Maybes.map (lambda "b" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "extends", typeBoundToExpr @@ var "b"]) (var "bound")])

wildcardToExpr :: TTermDefinition (Java.Wildcard -> Expr)
wildcardToExpr = def "wildcardToExpr" $
  lambda "w" $ lets [
    "anns">: project Java._Wildcard Java._Wildcard_annotations @@ var "w",
    "mbounds">: project Java._Wildcard Java._Wildcard_wildcard @@ var "w"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "anns")) nothing (just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map annotationToExpr (var "anns")),
      just $ Serialization.cst @@ string "*",
      Maybes.map wildcardBoundsToExpr (var "mbounds")])


classBodyToExpr :: TTermDefinition (Java.ClassBody -> Expr)
classBodyToExpr = def "classBodyToExpr" $
  lambda "cb" $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
      (Serialization.doubleNewlineSep @@ Lists.map classBodyDeclarationWithCommentsToExpr (unwrap Java._ClassBody @@ var "cb"))

classBodyDeclarationToExpr :: TTermDefinition (Java.ClassBodyDeclaration -> Expr)
classBodyDeclarationToExpr = def "classBodyDeclarationToExpr" $
  lambda "d" $
    cases Java._ClassBodyDeclaration (var "d") Nothing [
      Java._ClassBodyDeclaration_classMember>>: lambda "d" $ classMemberDeclarationToExpr @@ var "d",
      Java._ClassBodyDeclaration_instanceInitializer>>: lambda "i" $ instanceInitializerToExpr @@ var "i",
      Java._ClassBodyDeclaration_staticInitializer>>: lambda "i" $ staticInitializerToExpr @@ var "i",
      Java._ClassBodyDeclaration_constructorDeclaration>>: lambda "d" $ constructorDeclarationToExpr @@ var "d"]

classBodyDeclarationWithCommentsToExpr :: TTermDefinition (Java.ClassBodyDeclarationWithComments -> Expr)
classBodyDeclarationWithCommentsToExpr = def "classBodyDeclarationWithCommentsToExpr" $
  lambda "cbdwc" $ lets [
    "d">: project Java._ClassBodyDeclarationWithComments Java._ClassBodyDeclarationWithComments_value @@ var "cbdwc",
    "mc">: project Java._ClassBodyDeclarationWithComments Java._ClassBodyDeclarationWithComments_comments @@ var "cbdwc"] $
    withComments @@ var "mc" @@ (classBodyDeclarationToExpr @@ var "d")

classMemberDeclarationToExpr :: TTermDefinition (Java.ClassMemberDeclaration -> Expr)
classMemberDeclarationToExpr = def "classMemberDeclarationToExpr" $
  lambda "d" $
    cases Java._ClassMemberDeclaration (var "d") Nothing [
      Java._ClassMemberDeclaration_field>>: lambda "fd" $ fieldDeclarationToExpr @@ var "fd",
      Java._ClassMemberDeclaration_method>>: lambda "md" $ methodDeclarationToExpr @@ var "md",
      Java._ClassMemberDeclaration_class>>: lambda "cd" $ classDeclarationToExpr @@ var "cd",
      Java._ClassMemberDeclaration_interface>>: lambda "id" $ interfaceDeclarationToExpr @@ var "id",
      Java._ClassMemberDeclaration_none>>: constant $ Serialization.cst @@ string ";"]

fieldDeclarationToExpr :: TTermDefinition (Java.FieldDeclaration -> Expr)
fieldDeclarationToExpr = def "fieldDeclarationToExpr" $
  lambda "fd" $ lets [
    "mods">: project Java._FieldDeclaration Java._FieldDeclaration_modifiers @@ var "fd",
    "typ">: project Java._FieldDeclaration Java._FieldDeclaration_unannType @@ var "fd",
    "vars">: project Java._FieldDeclaration Java._FieldDeclaration_variableDeclarators @@ var "fd"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map fieldModifierToExpr (var "mods")),
      just $ unannTypeToExpr @@ var "typ",
      just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map variableDeclaratorToExpr (var "vars")]))

variableDeclaratorToExpr :: TTermDefinition (Java.VariableDeclarator -> Expr)
variableDeclaratorToExpr = def "variableDeclaratorToExpr" $
  lambda "vd" $ lets [
    "id">: project Java._VariableDeclarator Java._VariableDeclarator_id @@ var "vd",
    "minit">: project Java._VariableDeclarator Java._VariableDeclarator_initializer @@ var "vd",
    "idSec">: variableDeclaratorIdToExpr @@ var "id"] $
    Maybes.maybe (var "idSec")
      (lambda "init" $ Serialization.infixWs @@ string "=" @@ var "idSec" @@ (variableInitializerToExpr @@ var "init"))
      (var "minit")

variableDeclaratorIdToExpr :: TTermDefinition (Java.VariableDeclaratorId -> Expr)
variableDeclaratorIdToExpr = def "variableDeclaratorIdToExpr" $
  lambda "vdi" $ lets [
    "id">: project Java._VariableDeclaratorId Java._VariableDeclaratorId_identifier @@ var "vdi",
    "mdims">: project Java._VariableDeclaratorId Java._VariableDeclaratorId_dims @@ var "vdi"] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ identifierToExpr @@ var "id",
      Maybes.map dimsToExpr (var "mdims")])

methodDeclarationToExpr :: TTermDefinition (Java.MethodDeclaration -> Expr)
methodDeclarationToExpr = def "methodDeclarationToExpr" $
  lambda "md" $ lets [
    "anns">: project Java._MethodDeclaration Java._MethodDeclaration_annotations @@ var "md",
    "mods">: project Java._MethodDeclaration Java._MethodDeclaration_modifiers @@ var "md",
    "header">: project Java._MethodDeclaration Java._MethodDeclaration_header @@ var "md",
    "body">: project Java._MethodDeclaration Java._MethodDeclaration_body @@ var "md",
    "headerAndBody">: Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map methodModifierToExpr (var "mods")),
      just $ methodHeaderToExpr @@ var "header",
      just $ methodBodyToExpr @@ var "body"])] $
    Serialization.newlineSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "anns")) nothing (just $ Serialization.newlineSep @@ Lists.map annotationToExpr (var "anns")),
      just $ var "headerAndBody"])

methodHeaderToExpr :: TTermDefinition (Java.MethodHeader -> Expr)
methodHeaderToExpr = def "methodHeaderToExpr" $
  lambda "mh" $ lets [
    "params">: project Java._MethodHeader Java._MethodHeader_parameters @@ var "mh",
    "result">: project Java._MethodHeader Java._MethodHeader_result @@ var "mh",
    "decl">: project Java._MethodHeader Java._MethodHeader_declarator @@ var "mh",
    "mthrows">: project Java._MethodHeader Java._MethodHeader_throws @@ var "mh"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "params")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map typeParameterToExpr (var "params")),
      just $ resultToExpr @@ var "result",
      just $ methodDeclaratorToExpr @@ var "decl",
      Maybes.map throwsToExpr (var "mthrows")])

methodDeclaratorToExpr :: TTermDefinition (Java.MethodDeclarator -> Expr)
methodDeclaratorToExpr = def "methodDeclaratorToExpr" $
  lambda "md" $ lets [
    "id">: project Java._MethodDeclarator Java._MethodDeclarator_identifier @@ var "md",
    "params">: project Java._MethodDeclarator Java._MethodDeclarator_formalParameters @@ var "md"] $
    Serialization.noSep @@ list [
      identifierToExpr @@ var "id",
      Serialization.parenListAdaptive @@ Lists.map formalParameterToExpr (var "params")]

formalParameterSimpleToExpr :: TTermDefinition (Java.FormalParameter_Simple -> Expr)
formalParameterSimpleToExpr = def "formalParameterSimpleToExpr" $
  lambda "fps" $ lets [
    "mods">: project Java._FormalParameter_Simple Java._FormalParameter_Simple_modifiers @@ var "fps",
    "typ">: project Java._FormalParameter_Simple Java._FormalParameter_Simple_type @@ var "fps",
    "id">: project Java._FormalParameter_Simple Java._FormalParameter_Simple_id @@ var "fps"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map variableModifierToExpr (var "mods")),
      just $ unannTypeToExpr @@ var "typ",
      just $ variableDeclaratorIdToExpr @@ var "id"])

constructorDeclarationToExpr :: TTermDefinition (Java.ConstructorDeclaration -> Expr)
constructorDeclarationToExpr = def "constructorDeclarationToExpr" $
  lambda "cd" $ lets [
    "mods">: project Java._ConstructorDeclaration Java._ConstructorDeclaration_modifiers @@ var "cd",
    "cons">: project Java._ConstructorDeclaration Java._ConstructorDeclaration_constructor @@ var "cd",
    "mthrows">: project Java._ConstructorDeclaration Java._ConstructorDeclaration_throws @@ var "cd",
    "body">: project Java._ConstructorDeclaration Java._ConstructorDeclaration_body @@ var "cd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map constructorModifierToExpr (var "mods")),
      just $ constructorDeclaratorToExpr @@ var "cons",
      Maybes.map throwsToExpr (var "mthrows"),
      just $ constructorBodyToExpr @@ var "body"])

constructorDeclaratorToExpr :: TTermDefinition (Java.ConstructorDeclarator -> Expr)
constructorDeclaratorToExpr = def "constructorDeclaratorToExpr" $
  lambda "cd" $ lets [
    "tparams">: project Java._ConstructorDeclarator Java._ConstructorDeclarator_parameters @@ var "cd",
    "name">: project Java._ConstructorDeclarator Java._ConstructorDeclarator_name @@ var "cd",
    "fparams">: project Java._ConstructorDeclarator Java._ConstructorDeclarator_formalParameters @@ var "cd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "tparams")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map typeParameterToExpr (var "tparams")),
      just $ simpleTypeNameToExpr @@ var "name",
      just $ Serialization.parenListAdaptive @@ Lists.map formalParameterToExpr (var "fparams")])

constructorBodyToExpr :: TTermDefinition (Java.ConstructorBody -> Expr)
constructorBodyToExpr = def "constructorBodyToExpr" $
  lambda "cb" $ lets [
    "minvoc">: project Java._ConstructorBody Java._ConstructorBody_invocation @@ var "cb",
    "stmts">: project Java._ConstructorBody Java._ConstructorBody_statements @@ var "cb"] $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@ (Serialization.doubleNewlineSep @@ Maybes.cat (list [
      Maybes.map explicitConstructorInvocationToExpr (var "minvoc"),
      just $ Serialization.newlineSep @@ Lists.map blockStatementToExpr (var "stmts")]))


interfaceBodyToExpr :: TTermDefinition (Java.InterfaceBody -> Expr)
interfaceBodyToExpr = def "interfaceBodyToExpr" $
  lambda "ib" $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
      (Serialization.doubleNewlineSep @@ Lists.map interfaceMemberDeclarationToExpr (unwrap Java._InterfaceBody @@ var "ib"))

interfaceMemberDeclarationToExpr :: TTermDefinition (Java.InterfaceMemberDeclaration -> Expr)
interfaceMemberDeclarationToExpr = def "interfaceMemberDeclarationToExpr" $
  lambda "d" $
    cases Java._InterfaceMemberDeclaration (var "d") Nothing [
      Java._InterfaceMemberDeclaration_constant>>: lambda "c" $ constantDeclarationToExpr @@ var "c",
      Java._InterfaceMemberDeclaration_interfaceMethod>>: lambda "im" $ interfaceMethodDeclarationToExpr @@ var "im",
      Java._InterfaceMemberDeclaration_class>>: lambda "cd" $ classDeclarationToExpr @@ var "cd",
      Java._InterfaceMemberDeclaration_interface>>: lambda "id" $ interfaceDeclarationToExpr @@ var "id"]

interfaceMethodDeclarationToExpr :: TTermDefinition (Java.InterfaceMethodDeclaration -> Expr)
interfaceMethodDeclarationToExpr = def "interfaceMethodDeclarationToExpr" $
  lambda "imd" $ lets [
    "mods">: project Java._InterfaceMethodDeclaration Java._InterfaceMethodDeclaration_modifiers @@ var "imd",
    "header">: project Java._InterfaceMethodDeclaration Java._InterfaceMethodDeclaration_header @@ var "imd",
    "body">: project Java._InterfaceMethodDeclaration Java._InterfaceMethodDeclaration_body @@ var "imd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map interfaceMethodModifierToExpr (var "mods")),
      just $ methodHeaderToExpr @@ var "header",
      just $ methodBodyToExpr @@ var "body"])

constantDeclarationToExpr :: TTermDefinition (Java.ConstantDeclaration -> Expr)
constantDeclarationToExpr = def "constantDeclarationToExpr" $
  lambda "cd" $ lets [
    "mods">: project Java._ConstantDeclaration Java._ConstantDeclaration_modifiers @@ var "cd",
    "typ">: project Java._ConstantDeclaration Java._ConstantDeclaration_type @@ var "cd",
    "vars">: project Java._ConstantDeclaration Java._ConstantDeclaration_variables @@ var "cd"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map constantModifierToExpr (var "mods")),
      just $ unannTypeToExpr @@ var "typ",
      just $ Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map variableDeclaratorToExpr (var "vars")]))


normalClassDeclarationToExpr :: TTermDefinition (Java.NormalClassDeclaration -> Expr)
normalClassDeclarationToExpr = def "normalClassDeclarationToExpr" $
  lambda "ncd" $ lets [
    "mods">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_modifiers @@ var "ncd",
    "id">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_identifier @@ var "ncd",
    "tparams">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_parameters @@ var "ncd",
    "msuperc">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_extends @@ var "ncd",
    "superi">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_implements @@ var "ncd",
    "body">: project Java._NormalClassDeclaration Java._NormalClassDeclaration_body @@ var "ncd"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map classModifierToExpr (var "mods")),
      just $ Serialization.cst @@ string "class",
      just $ Serialization.noSep @@ Maybes.cat (list [
        just $ typeIdentifierToExpr @@ var "id",
        Logic.ifElse (Lists.null (var "tparams")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map typeParameterToExpr (var "tparams"))]),
      Maybes.map (lambda "c" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "extends", classTypeToExpr @@ var "c"]) (var "msuperc"),
      Logic.ifElse (Lists.null (var "superi")) nothing
        (just $ Serialization.spaceSep @@ list [Serialization.cst @@ string "implements", Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map interfaceTypeToExpr (var "superi")]),
      just $ classBodyToExpr @@ var "body"])

normalInterfaceDeclarationToExpr :: TTermDefinition (Java.NormalInterfaceDeclaration -> Expr)
normalInterfaceDeclarationToExpr = def "normalInterfaceDeclarationToExpr" $
  lambda "nid" $ lets [
    "mods">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_modifiers @@ var "nid",
    "id">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_identifier @@ var "nid",
    "tparams">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_parameters @@ var "nid",
    "extends">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_extends @@ var "nid",
    "body">: project Java._NormalInterfaceDeclaration Java._NormalInterfaceDeclaration_body @@ var "nid"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map interfaceModifierToExpr (var "mods")),
      just $ Serialization.cst @@ string "interface",
      just $ Serialization.noSep @@ Maybes.cat (list [
        just $ typeIdentifierToExpr @@ var "id",
        Logic.ifElse (Lists.null (var "tparams")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map typeParameterToExpr (var "tparams"))]),
      Logic.ifElse (Lists.null (var "extends")) nothing
        (just $ Serialization.spaceSep @@ list [Serialization.cst @@ string "extends", Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map interfaceTypeToExpr (var "extends")]),
      just $ interfaceBodyToExpr @@ var "body"])

typeDeclarationWithCommentsToExpr :: TTermDefinition (Java.TypeDeclarationWithComments -> Expr)
typeDeclarationWithCommentsToExpr = def "typeDeclarationWithCommentsToExpr" $
  lambda "tdwc" $ lets [
    "d">: project Java._TypeDeclarationWithComments Java._TypeDeclarationWithComments_value @@ var "tdwc",
    "mc">: project Java._TypeDeclarationWithComments Java._TypeDeclarationWithComments_comments @@ var "tdwc"] $
    withComments @@ var "mc" @@ (typeDeclarationToExpr @@ var "d")


packageDeclarationToExpr :: TTermDefinition (Java.PackageDeclaration -> Expr)
packageDeclarationToExpr = def "packageDeclarationToExpr" $
  lambda "pd" $ lets [
    "mods">: project Java._PackageDeclaration Java._PackageDeclaration_modifiers @@ var "pd",
    "ids">: project Java._PackageDeclaration Java._PackageDeclaration_identifiers @@ var "pd"] $
    Serialization.withSemi @@ (Serialization.spaceSep @@ Maybes.cat (list [
      Logic.ifElse (Lists.null (var "mods")) nothing (just $ Serialization.spaceSep @@ Lists.map packageModifierToExpr (var "mods")),
      just $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "package",
        Serialization.cst @@ (Strings.intercalate (string ".") (Lists.map (lambda "id" $ unwrap Java._Identifier @@ var "id") (var "ids")))]]))

importDeclarationToExpr :: TTermDefinition (Java.ImportDeclaration -> Expr)
importDeclarationToExpr = def "importDeclarationToExpr" $
  lambda "imp" $
    cases Java._ImportDeclaration (var "imp") Nothing [
      Java._ImportDeclaration_singleType>>: lambda "st" $
        Serialization.withSemi @@ (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "import",
          typeNameToExpr @@ (unwrap Java._SingleTypeImportDeclaration @@ var "st")]),
      Java._ImportDeclaration_typeImportOnDemand>>: lambda "_" $ Serialization.cst @@ string "STUB:ImportDeclarationTypeImportOnDemand",
      Java._ImportDeclaration_singleStaticImport>>: lambda "_" $ Serialization.cst @@ string "STUB:ImportDeclarationSingleStaticImport",
      Java._ImportDeclaration_staticImportOnDemand>>: lambda "_" $ Serialization.cst @@ string "STUB:ImportDeclarationStaticImportOnDemand"]

compilationUnitToExpr :: TTermDefinition (Java.CompilationUnit -> Expr)
compilationUnitToExpr = def "compilationUnitToExpr" $
  lambda "u" $
    cases Java._CompilationUnit (var "u") Nothing [
      Java._CompilationUnit_ordinary>>: lambda "ocu" $ lets [
        "mpkg">: project Java._OrdinaryCompilationUnit Java._OrdinaryCompilationUnit_package @@ var "ocu",
        "imports">: project Java._OrdinaryCompilationUnit Java._OrdinaryCompilationUnit_imports @@ var "ocu",
        "types">: project Java._OrdinaryCompilationUnit Java._OrdinaryCompilationUnit_types @@ var "ocu",
        "warning">: just $ singleLineComment @@ Constants.warningAutoGeneratedFile,
        "pkgSec">: Maybes.map packageDeclarationToExpr (var "mpkg"),
        "importsSec">: Logic.ifElse (Lists.null (var "imports")) nothing
          (just $ Serialization.newlineSep @@ Lists.map importDeclarationToExpr (var "imports")),
        "typesSec">: Logic.ifElse (Lists.null (var "types")) nothing
          (just $ Serialization.doubleNewlineSep @@ Lists.map typeDeclarationWithCommentsToExpr (var "types"))] $
        Serialization.doubleNewlineSep @@ Maybes.cat (list [var "warning", var "pkgSec", var "importsSec", var "typesSec"])]


methodInvocationToExpr :: TTermDefinition (Java.MethodInvocation -> Expr)
methodInvocationToExpr = def "methodInvocationToExpr" $
  lambda "mi" $ lets [
    "header">: project Java._MethodInvocation Java._MethodInvocation_header @@ var "mi",
    "args">: project Java._MethodInvocation Java._MethodInvocation_arguments @@ var "mi",
    "argSec">: Serialization.parenListAdaptive @@ Lists.map expressionToExpr (var "args"),
    "headerSec">: cases Java._MethodInvocation_Header (var "header") Nothing [
      Java._MethodInvocation_Header_simple>>: lambda "mname" $ methodNameToExpr @@ var "mname",
      Java._MethodInvocation_Header_complex>>: lambda "cx" $ lets [
        "cvar">: project Java._MethodInvocation_Complex Java._MethodInvocation_Complex_variant @@ var "cx",
        "targs">: project Java._MethodInvocation_Complex Java._MethodInvocation_Complex_typeArguments @@ var "cx",
        "cid">: project Java._MethodInvocation_Complex Java._MethodInvocation_Complex_identifier @@ var "cx",
        "idSec">: Serialization.noSep @@ Maybes.cat (list [
          Logic.ifElse (Lists.null (var "targs")) nothing
            (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map typeArgumentToExpr (var "targs")),
          just $ identifierToExpr @@ var "cid"])] $
        cases Java._MethodInvocation_Variant (var "cvar") Nothing [
          Java._MethodInvocation_Variant_type>>: lambda "tname" $ Serialization.dotSep @@ list [typeNameToExpr @@ var "tname", var "idSec"],
          Java._MethodInvocation_Variant_expression>>: lambda "en" $ Serialization.dotSep @@ list [expressionNameToExpr @@ var "en", var "idSec"],
          Java._MethodInvocation_Variant_primary>>: lambda "p" $ Serialization.dotSep @@ list [primaryToExpr @@ var "p", var "idSec"],
          Java._MethodInvocation_Variant_super>>: constant $ Serialization.dotSep @@ list [Serialization.cst @@ string "super", var "idSec"],
          Java._MethodInvocation_Variant_typeSuper>>: lambda "tname" $ Serialization.dotSep @@ list [typeNameToExpr @@ var "tname", Serialization.cst @@ string "super", var "idSec"]]]] $
    Serialization.noSep @@ list [var "headerSec", var "argSec"]


castExpressionNotPlusMinusToExpr :: TTermDefinition (Java.CastExpression_NotPlusMinus -> Expr)
castExpressionNotPlusMinusToExpr = def "castExpressionNotPlusMinusToExpr" $
  lambda "npm" $ lets [
    "rb">: project Java._CastExpression_NotPlusMinus Java._CastExpression_NotPlusMinus_refAndBounds @@ var "npm",
    "ex">: project Java._CastExpression_NotPlusMinus Java._CastExpression_NotPlusMinus_expression @@ var "npm"] $
    Serialization.spaceSep @@ list [castExpressionRefAndBoundsToExpr @@ var "rb", unaryExpressionToExpr @@ var "ex"]

castExpressionRefAndBoundsToExpr :: TTermDefinition (Java.CastExpression_RefAndBounds -> Expr)
castExpressionRefAndBoundsToExpr = def "castExpressionRefAndBoundsToExpr" $
  lambda "rab" $ lets [
    "rt">: project Java._CastExpression_RefAndBounds Java._CastExpression_RefAndBounds_type @@ var "rab",
    "adds">: project Java._CastExpression_RefAndBounds Java._CastExpression_RefAndBounds_bounds @@ var "rab"] $
    Serialization.parenList @@ false @@ list [Serialization.spaceSep @@ Maybes.cat (list [
      just $ referenceTypeToExpr @@ var "rt",
      Logic.ifElse (Lists.null (var "adds")) nothing (just $ Serialization.spaceSep @@ Lists.map additionalBoundToExpr (var "adds"))])]

castExpressionPrimitiveToExpr :: TTermDefinition (Java.CastExpression_Primitive -> Expr)
castExpressionPrimitiveToExpr = def "castExpressionPrimitiveToExpr" $
  lambda "cp" $ lets [
    "pt">: project Java._CastExpression_Primitive Java._CastExpression_Primitive_type @@ var "cp",
    "ex">: project Java._CastExpression_Primitive Java._CastExpression_Primitive_expression @@ var "cp"] $
    Serialization.spaceSep @@ list [
      Serialization.parenList @@ false @@ list [primitiveTypeWithAnnotationsToExpr @@ var "pt"],
      unaryExpressionToExpr @@ var "ex"]


classInstanceCreationExpressionToExpr :: TTermDefinition (Java.ClassInstanceCreationExpression -> Expr)
classInstanceCreationExpressionToExpr = def "classInstanceCreationExpressionToExpr" $
  lambda "cice" $ lets [
    "mqual">: project Java._ClassInstanceCreationExpression Java._ClassInstanceCreationExpression_qualifier @@ var "cice",
    "e">: project Java._ClassInstanceCreationExpression Java._ClassInstanceCreationExpression_expression @@ var "cice"] $
    Maybes.maybe
      (unqualifiedClassInstanceCreationExpressionToExpr @@ var "e")
      (lambda "q" $ Serialization.dotSep @@ list [classInstanceCreationExpressionQualifierToExpr @@ var "q", unqualifiedClassInstanceCreationExpressionToExpr @@ var "e"])
      (var "mqual")

classInstanceCreationExpressionQualifierToExpr :: TTermDefinition (Java.ClassInstanceCreationExpression_Qualifier -> Expr)
classInstanceCreationExpressionQualifierToExpr = def "classInstanceCreationExpressionQualifierToExpr" $
  lambda "q" $
    cases Java._ClassInstanceCreationExpression_Qualifier (var "q") Nothing [
      Java._ClassInstanceCreationExpression_Qualifier_expression>>: lambda "en" $ expressionNameToExpr @@ var "en",
      Java._ClassInstanceCreationExpression_Qualifier_primary>>: lambda "p" $ primaryToExpr @@ var "p"]

unqualifiedClassInstanceCreationExpressionToExpr :: TTermDefinition (Java.UnqualifiedClassInstanceCreationExpression -> Expr)
unqualifiedClassInstanceCreationExpressionToExpr = def "unqualifiedClassInstanceCreationExpressionToExpr" $
  lambda "ucice" $ lets [
    "targs">: project Java._UnqualifiedClassInstanceCreationExpression Java._UnqualifiedClassInstanceCreationExpression_typeArguments @@ var "ucice",
    "cit">: project Java._UnqualifiedClassInstanceCreationExpression Java._UnqualifiedClassInstanceCreationExpression_classOrInterface @@ var "ucice",
    "args">: project Java._UnqualifiedClassInstanceCreationExpression Java._UnqualifiedClassInstanceCreationExpression_arguments @@ var "ucice",
    "mbody">: project Java._UnqualifiedClassInstanceCreationExpression Java._UnqualifiedClassInstanceCreationExpression_body @@ var "ucice"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "new",
      Logic.ifElse (Lists.null (var "targs")) nothing (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ Lists.map typeArgumentToExpr (var "targs")),
      just $ Serialization.noSep @@ list [classOrInterfaceTypeToInstantiateToExpr @@ var "cit", Serialization.parenList @@ false @@ Lists.map expressionToExpr (var "args")],
      Maybes.map classBodyToExpr (var "mbody")])

classOrInterfaceTypeToInstantiateToExpr :: TTermDefinition (Java.ClassOrInterfaceTypeToInstantiate -> Expr)
classOrInterfaceTypeToInstantiateToExpr = def "classOrInterfaceTypeToInstantiateToExpr" $
  lambda "coitti" $ lets [
    "ids">: project Java._ClassOrInterfaceTypeToInstantiate Java._ClassOrInterfaceTypeToInstantiate_identifiers @@ var "coitti",
    "margs">: project Java._ClassOrInterfaceTypeToInstantiate Java._ClassOrInterfaceTypeToInstantiate_typeArguments @@ var "coitti"] $
    Serialization.noSep @@ Maybes.cat (list [
      just $ Serialization.dotSep @@ Lists.map annotatedIdentifierToExpr (var "ids"),
      Maybes.map typeArgumentsOrDiamondToExpr (var "margs")])


arrayCreationExpressionToExpr :: TTermDefinition (Java.ArrayCreationExpression -> Expr)
arrayCreationExpressionToExpr = def "arrayCreationExpressionToExpr" $
  lambda "ace" $
    cases Java._ArrayCreationExpression (var "ace") Nothing [
      Java._ArrayCreationExpression_primitiveArray>>: lambda "pa" $ lets [
        "pt">: project Java._ArrayCreationExpression_PrimitiveArray Java._ArrayCreationExpression_PrimitiveArray_type @@ var "pa",
        "ai">: project Java._ArrayCreationExpression_PrimitiveArray Java._ArrayCreationExpression_PrimitiveArray_array @@ var "pa"] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "new",
          Serialization.noSep @@ list [primitiveTypeWithAnnotationsToExpr @@ var "pt", Serialization.cst @@ string "[]"],
          arrayInitializerToExpr @@ var "ai"],
      Java._ArrayCreationExpression_classOrInterfaceArray>>: lambda "_" $ Serialization.cst @@ string "STUB:ArrayCreationExpression",
      Java._ArrayCreationExpression_primitive>>: lambda "_" $ Serialization.cst @@ string "STUB:ArrayCreationExpression",
      Java._ArrayCreationExpression_classOrInterface>>: lambda "_" $ Serialization.cst @@ string "STUB:ArrayCreationExpression"]

arrayInitializerToExpr :: TTermDefinition (Java.ArrayInitializer -> Expr)
arrayInitializerToExpr = def "arrayInitializerToExpr" $
  lambda "ai" $ lets [
    "groups">: unwrap Java._ArrayInitializer @@ var "ai"] $
    Maybes.fromMaybe (Serialization.cst @@ string "{}") (Maybes.map
      (lambda "firstGroup" $
        Logic.ifElse (Equality.equal (Lists.length (var "groups")) (int32 1))
          (Serialization.noSep @@ list [
            Serialization.cst @@ string "{",
            Serialization.commaSep @@ Serialization.inlineStyle @@ Lists.map variableInitializerToExpr (var "firstGroup"),
            Serialization.cst @@ string "}"])
          (Serialization.cst @@ string "{}"))
      (Lists.maybeHead (var "groups")))
