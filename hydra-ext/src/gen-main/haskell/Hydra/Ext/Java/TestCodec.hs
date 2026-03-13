-- Note: this is an automatically generated file. Do not edit.

-- | Java test code generation codec for JUnit-based generation tests

module Hydra.Ext.Java.TestCodec where

import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Java.Coder as Coder
import qualified Hydra.Ext.Java.Helpers as Helpers
import qualified Hydra.Ext.Java.Serde as Serde
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Testing as Testing
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a Hydra term to a Java expression string
termToJava :: (Core.Term -> Graph.Graph -> Either String String)
termToJava term g = (Eithers.bimap (\ic -> Error.unOtherError (Context.inContextObject ic)) (\arg_ -> (\arg_ -> Serialization.printExpr (Serialization.parenthesize arg_)) (Serde.writeExpression arg_)) (Coder.encodeTerm (Helpers.JavaEnvironment {
  Helpers.javaEnvironmentAliases = Helpers.Aliases {
    Helpers.aliasesCurrentNamespace = (Module.Namespace "test"),
    Helpers.aliasesPackages = Maps.empty,
    Helpers.aliasesBranchVars = Sets.empty,
    Helpers.aliasesRecursiveVars = Sets.empty,
    Helpers.aliasesInScopeTypeParams = Sets.empty,
    Helpers.aliasesPolymorphicLocals = Sets.empty,
    Helpers.aliasesInScopeJavaVars = Sets.empty,
    Helpers.aliasesVarRenames = Maps.empty,
    Helpers.aliasesLambdaVars = Sets.empty,
    Helpers.aliasesTypeVarSubst = Maps.empty,
    Helpers.aliasesTrustedTypeVars = Sets.empty,
    Helpers.aliasesMethodCodomain = Nothing,
    Helpers.aliasesThunkedVars = Sets.empty},
  Helpers.javaEnvironmentGraph = g}) term (Context.Context {
  Context.contextTrace = [],
  Context.contextMessages = [],
  Context.contextOther = Maps.empty}) g))

-- | Convert a Hydra type to a Java type expression string (placeholder returning Object)
typeToJava :: (t0 -> t1 -> Either t2 String)
typeToJava _t _g = (Right "Object")

-- | Create a Java TestCodec for JUnit-based test generation
javaTestCodec :: Testing.TestCodec
javaTestCodec = Testing.TestCodec {
  Testing.testCodecLanguage = (Coders.LanguageName "java"),
  Testing.testCodecFileExtension = (Module.FileExtension "java"),
  Testing.testCodecEncodeTerm = termToJava,
  Testing.testCodecEncodeType = typeToJava,
  Testing.testCodecFormatTestName = formatJavaTestName,
  Testing.testCodecFormatModuleName = namespaceToJavaClassName,
  Testing.testCodecTestCaseTemplate = javaTestCaseTemplate,
  Testing.testCodecTestGroupTemplate = javaTestGroupTemplate,
  Testing.testCodecModuleTemplate = javaModuleTemplate,
  Testing.testCodecImportTemplate = javaImportTemplate,
  Testing.testCodecFindImports = (\_names -> findJavaImports)}

-- | Format a test name for Java (PascalCase method name with 'test' prefix)
formatJavaTestName :: (String -> String)
formatJavaTestName name =  
  let replaced = (Strings.intercalate " Neg" (Strings.splitOn "-" (Strings.intercalate "Dot" (Strings.splitOn "." (Strings.intercalate " Plus" (Strings.splitOn "+" (Strings.intercalate " Div" (Strings.splitOn "/" (Strings.intercalate " Mul" (Strings.splitOn "*" (Strings.intercalate " Num" (Strings.splitOn "#" name)))))))))))) 
      sanitized = (Formatting.nonAlnumToUnderscores replaced)
      pascal_ = (Formatting.convertCase Util.CaseConventionLowerSnake Util.CaseConventionPascal sanitized)
  in (Strings.cat2 "test" pascal_)

-- | Convert namespace to Java class name
namespaceToJavaClassName :: (Module.Namespace -> String)
namespaceToJavaClassName ns_ = (Strings.intercalate "." (Lists.map Formatting.capitalize (Strings.splitOn "." (Module.unNamespace ns_))))

-- | Template for JUnit test case assertions
javaTestCaseTemplate :: String
javaTestCaseTemplate = (Strings.intercalate "\n" [
  "    @Test",
  "    public void {name}() {",
  "        assertEquals({output}, {input});",
  "    }"])

-- | Template for JUnit test group comments
javaTestGroupTemplate :: String
javaTestGroupTemplate = "// {groupName}"

-- | Template for JUnit test module structure
javaModuleTemplate :: String
javaModuleTemplate = (Strings.intercalate "\n" [
  Strings.cat2 "// " Constants.warningAutoGeneratedFile,
  "",
  "package {package};",
  "",
  "{imports}",
  "",
  "public class {className} {",
  "    {testCases}",
  "}"])

-- | Template for Java import statements
javaImportTemplate :: String
javaImportTemplate = "import {namespace};"

-- | Standard imports for Java JUnit test files
findJavaImports :: [String]
findJavaImports = [
  "import org.junit.jupiter.api.Test;",
  "import static org.junit.jupiter.api.Assertions.*;",
  "import java.util.*;"]

-- | Generate test hierarchy for Java with nested subgroups
generateJavaTestGroupHierarchy :: (Graph.Graph -> Testing.TestCodec -> [String] -> Testing.TestGroup -> Either String String)
generateJavaTestGroupHierarchy g codec groupPath testGroup =  
  let cases_ = (Testing.testGroupCases testGroup) 
      subgroups = (Testing.testGroupSubgroups testGroup)
  in (Eithers.bind (Eithers.map (\lines_ -> Strings.intercalate "\n\n" (Lists.concat lines_)) (Eithers.mapList (\tc -> generateJavaTestCase g codec groupPath tc) cases_)) (\testCasesStr -> Eithers.map (\subgroupsStr -> Strings.cat [
    testCasesStr,
    (Logic.ifElse (Logic.or (Equality.equal testCasesStr "") (Equality.equal subgroupsStr "")) "" "\n\n"),
    subgroupsStr]) (Eithers.map (\blocks -> Strings.intercalate "\n\n" blocks) (Eithers.mapList (\subgroup ->  
    let groupName = (Testing.testGroupName subgroup) 
        header = (Strings.cat2 "    // " groupName)
    in (Eithers.map (\content -> Strings.cat [
      header,
      "\n\n",
      content]) (generateJavaTestGroupHierarchy g codec (Lists.concat2 groupPath [
      groupName]) subgroup))) subgroups))))

-- | Generate a single JUnit test case from a test case with metadata
generateJavaTestCase :: (Graph.Graph -> Testing.TestCodec -> [String] -> Testing.TestCaseWithMetadata -> Either String [String])
generateJavaTestCase g codec groupPath tcm =  
  let name_ = (Testing.testCaseWithMetadataName tcm) 
      tcase = (Testing.testCaseWithMetadataCase tcm)
  in ((\x -> case x of
    Testing.TestCaseDelegatedEvaluation v0 ->  
      let input_ = (Testing.delegatedEvaluationTestCaseInput v0) 
          output_ = (Testing.delegatedEvaluationTestCaseOutput v0)
          fullName = (Logic.ifElse (Lists.null groupPath) name_ (Strings.intercalate "_" (Lists.concat2 groupPath [
                  name_])))
          formattedName = (Testing.testCodecFormatTestName codec fullName)
          assertType = (getAssertionType output_)
      in  
        let typeVars = (Lists.sort (Lists.filter isInferenceVar (Sets.toList (Sets.union (Rewriting.freeTypeVariablesInTerm input_) (Rewriting.freeTypeVariablesInTerm output_))))) 
            typeParamsStr = (Logic.ifElse (Lists.null typeVars) "" (Strings.cat [
                    "<",
                    (Strings.intercalate ", " (Lists.map (\n_ -> Formatting.capitalize (Core.unName n_)) typeVars)),
                    "> "]))
        in (Eithers.bind (Testing.testCodecEncodeTerm codec input_ g) (\inputCode -> Eithers.map (\outputCode ->  
          let assertionLines = (generateAssertion assertType outputCode inputCode)
          in (Lists.concat2 [
            "    @Test",
            (Strings.cat [
              "    public ",
              typeParamsStr,
              "void ",
              formattedName,
              "() {"])] (Lists.concat2 assertionLines [
            "    }"]))) (Testing.testCodecEncodeTerm codec output_ g)))
    _ -> (Right [])) tcase)

-- | Determine the assertion type based on the output term structure (returns a string tag)
getAssertionType :: (Core.Term -> String)
getAssertionType term = ((\x -> case x of
  Core.TermLiteral v0 -> ((\x -> case x of
    Core.LiteralBinary _ -> "assertArrayEquals"
    Core.LiteralFloat v1 -> ((\x -> case x of
      Core.FloatValueBigfloat _ -> "assertBigDecimalEquals"
      _ -> "assertDoubleEquals") v1)
    _ -> "assertEquals") v0)
  _ -> "assertEquals") (Rewriting.deannotateTerm term))

-- | Generate assertion code lines based on assertion type, output code, and input code
generateAssertion :: (String -> String -> String -> [String])
generateAssertion assertType outputCode inputCode = (Logic.ifElse (Equality.equal assertType "assertArrayEquals") [
  "        assertArrayEquals(",
  (Strings.cat [
    "            ",
    outputCode,
    ","]),
  (Strings.cat [
    "            ",
    inputCode,
    ");"])] (Logic.ifElse (Equality.equal assertType "assertBigDecimalEquals") [
  Strings.cat [
    "        assertEquals(0, (",
    outputCode,
    ").compareTo(",
    inputCode,
    "));"]] (Logic.ifElse (Equality.equal assertType "assertDoubleEquals") [
  "        assertEquals(",
  (Strings.cat [
    "            ",
    outputCode,
    ","]),
  (Strings.cat [
    "            ",
    inputCode,
    ","]),
  "            1e-15);"] [
  "        assertEquals(",
  (Strings.cat [
    "            ",
    outputCode,
    ","]),
  (Strings.cat [
    "            ",
    inputCode,
    ");"])])))

-- | Check if a Name is an unresolved inference variable (matches pattern t followed by digits)
isInferenceVar :: (Core.Name -> Bool)
isInferenceVar n =  
  let s = (Core.unName n) 
      chars = (Strings.toList s)
  in (Logic.and (Equality.equal (Strings.charAt 0 s) 116) (Logic.and (Logic.not (Equality.equal (Strings.length s) 1)) (Lists.null (Lists.filter (\c -> Logic.not (Logic.and (Equality.gte c 48) (Equality.lte c 57))) (Lists.drop 1 chars)))))

-- | Generate a complete test file using the Java codec
generateTestFileWithJavaCodec :: (Testing.TestCodec -> Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String))
generateTestFileWithJavaCodec codec testModule testGroup g = (Eithers.map (\testBody ->  
  let testModuleContent = (buildJavaTestModule codec testModule testGroup testBody) 
      ns_ = (Module.moduleNamespace testModule)
      parts = (Strings.splitOn "." (Module.unNamespace ns_))
      dirParts = (Lists.drop 1 (Lists.init parts))
      className_ = (Strings.cat2 (Formatting.capitalize (Lists.last parts)) "Test")
      fileName = (Strings.cat2 className_ ".java")
      filePath = (Strings.cat [
              Strings.intercalate "/" dirParts,
              "/",
              fileName])
  in (filePath, testModuleContent)) (generateJavaTestGroupHierarchy g codec [] testGroup))

-- | Build the complete Java test module content
buildJavaTestModule :: (t0 -> Module.Module -> Testing.TestGroup -> String -> String)
buildJavaTestModule codec testModule testGroup testBody =  
  let ns_ = (Module.moduleNamespace testModule) 
      parts = (Strings.splitOn "." (Module.unNamespace ns_))
      packageName = (Strings.intercalate "." (Lists.init parts))
      className_ = (Strings.cat2 (Formatting.capitalize (Lists.last parts)) "Test")
      groupName_ = (Testing.testGroupName testGroup)
      standardImports = [
              "import org.junit.jupiter.api.Test;",
              "import static org.junit.jupiter.api.Assertions.*;",
              "import java.util.*;",
              "import hydra.util.*;"]
      header = (Strings.cat [
              Strings.cat2 "// " Constants.warningAutoGeneratedFile,
              "\n",
              (Strings.cat2 "// " groupName_),
              "\n\n",
              (Strings.cat [
                "package ",
                packageName,
                ";\n\n"]),
              (Strings.intercalate "\n" standardImports),
              "\n\n",
              (Strings.cat [
                "public class ",
                className_,
                " {\n\n"])])
  in (Strings.cat [
    header,
    testBody,
    "\n}\n"])

-- | Generate a Java test file for a test group, with type inference
generateJavaTestFile :: (Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String))
generateJavaTestFile testModule testGroup g = (Eithers.bind (inferTestGroupTerms g testGroup) (\inferredTestGroup -> generateTestFileWithJavaCodec javaTestCodec testModule inferredTestGroup g))

-- | Run type inference on all terms in a TestGroup to ensure lambdas have domain types
inferTestGroupTerms :: (Graph.Graph -> Testing.TestGroup -> Either String Testing.TestGroup)
inferTestGroupTerms g tg =  
  let name_ = (Testing.testGroupName tg) 
      desc = (Testing.testGroupDescription tg)
      subgroups = (Testing.testGroupSubgroups tg)
      cases_ = (Testing.testGroupCases tg)
  in (Eithers.bind (Eithers.mapList (\sg -> inferTestGroupTerms g sg) subgroups) (\inferredSubgroups -> Eithers.map (\inferredCases -> Testing.TestGroup {
    Testing.testGroupName = name_,
    Testing.testGroupDescription = desc,
    Testing.testGroupSubgroups = inferredSubgroups,
    Testing.testGroupCases = inferredCases}) (Eithers.mapList (\tc -> inferTestCase g tc) cases_)))

-- | Run type inference on the terms in a test case
inferTestCase :: (Graph.Graph -> Testing.TestCaseWithMetadata -> Either String Testing.TestCaseWithMetadata)
inferTestCase g tcm =  
  let name_ = (Testing.testCaseWithMetadataName tcm) 
      tcase = (Testing.testCaseWithMetadataCase tcm)
      desc = (Testing.testCaseWithMetadataDescription tcm)
      tags_ = (Testing.testCaseWithMetadataTags tcm)
  in (Eithers.map (\inferredCase -> Testing.TestCaseWithMetadata {
    Testing.testCaseWithMetadataName = name_,
    Testing.testCaseWithMetadataCase = inferredCase,
    Testing.testCaseWithMetadataDescription = desc,
    Testing.testCaseWithMetadataTags = tags_}) ((\x -> case x of
    Testing.TestCaseDelegatedEvaluation v0 ->  
      let input_ = (Testing.delegatedEvaluationTestCaseInput v0) 
          output_ = (Testing.delegatedEvaluationTestCaseOutput v0)
      in (Eithers.bind (inferTerm g input_) (\inferredInput -> Eithers.map (\inferredOutput -> Testing.TestCaseDelegatedEvaluation (Testing.DelegatedEvaluationTestCase {
        Testing.delegatedEvaluationTestCaseInput = inferredInput,
        Testing.delegatedEvaluationTestCaseOutput = inferredOutput})) (inferTerm g output_)))
    _ -> (Right tcase)) tcase))

-- | Run type inference on a single term
inferTerm :: (Graph.Graph -> Core.Term -> Either String Core.Term)
inferTerm g term = (Eithers.bimap (\ic -> Error.unOtherError (Context.inContextObject ic)) (\x -> Typing.inferenceResultTerm x) (Inference.inferInGraphContext (Context.Context {
  Context.contextTrace = [],
  Context.contextMessages = [],
  Context.contextOther = Maps.empty}) g term))
