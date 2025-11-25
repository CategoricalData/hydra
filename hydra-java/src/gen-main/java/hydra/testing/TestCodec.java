// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A codec for generating compiled test files from test groups into a target programming language
 */
public class TestCodec implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TestCodec");
  
  public static final hydra.core.Name FIELD_NAME_LANGUAGE = new hydra.core.Name("language");
  
  public static final hydra.core.Name FIELD_NAME_FILE_EXTENSION = new hydra.core.Name("fileExtension");
  
  public static final hydra.core.Name FIELD_NAME_ENCODE_TERM = new hydra.core.Name("encodeTerm");
  
  public static final hydra.core.Name FIELD_NAME_ENCODE_TYPE = new hydra.core.Name("encodeType");
  
  public static final hydra.core.Name FIELD_NAME_FORMAT_TEST_NAME = new hydra.core.Name("formatTestName");
  
  public static final hydra.core.Name FIELD_NAME_FORMAT_MODULE_NAME = new hydra.core.Name("formatModuleName");
  
  public static final hydra.core.Name FIELD_NAME_TEST_CASE_TEMPLATE = new hydra.core.Name("testCaseTemplate");
  
  public static final hydra.core.Name FIELD_NAME_TEST_GROUP_TEMPLATE = new hydra.core.Name("testGroupTemplate");
  
  public static final hydra.core.Name FIELD_NAME_MODULE_TEMPLATE = new hydra.core.Name("moduleTemplate");
  
  public static final hydra.core.Name FIELD_NAME_IMPORT_TEMPLATE = new hydra.core.Name("importTemplate");
  
  public static final hydra.core.Name FIELD_NAME_FIND_IMPORTS = new hydra.core.Name("findImports");
  
  /**
   * The name of the target programming language
   */
  public final hydra.coders.LanguageName language;
  
  /**
   * The file extension for test files (e.g., 'hs', 'java', 'py')
   */
  public final hydra.module.FileExtension fileExtension;
  
  /**
   * A function for encoding Hydra terms into the target language
   */
  public final java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, String>> encodeTerm;
  
  /**
   * A function for encoding Hydra types into the target language
   */
  public final java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, String>> encodeType;
  
  /**
   * A function for formatting test case names according to the target language's conventions
   */
  public final java.util.function.Function<String, String> formatTestName;
  
  /**
   * A function for formatting module names according to the target language's conventions
   */
  public final java.util.function.Function<hydra.module.Namespace, String> formatModuleName;
  
  /**
   * A template string for individual test case assertions
   */
  public final String testCaseTemplate;
  
  /**
   * A template string for wrapping a group of test cases
   */
  public final String testGroupTemplate;
  
  /**
   * A template string for the overall test module structure
   */
  public final String moduleTemplate;
  
  /**
   * A template string for import/include statements
   */
  public final String importTemplate;
  
  /**
   * A function that determines the necessary imports for a given set of dependencies
   */
  public final java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<String>> findImports;
  
  public TestCodec (hydra.coders.LanguageName language, hydra.module.FileExtension fileExtension, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, String>> encodeTerm, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, String>> encodeType, java.util.function.Function<String, String> formatTestName, java.util.function.Function<hydra.module.Namespace, String> formatModuleName, String testCaseTemplate, String testGroupTemplate, String moduleTemplate, String importTemplate, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<String>> findImports) {
    java.util.Objects.requireNonNull((language));
    java.util.Objects.requireNonNull((fileExtension));
    java.util.Objects.requireNonNull((encodeTerm));
    java.util.Objects.requireNonNull((encodeType));
    java.util.Objects.requireNonNull((formatTestName));
    java.util.Objects.requireNonNull((formatModuleName));
    java.util.Objects.requireNonNull((testCaseTemplate));
    java.util.Objects.requireNonNull((testGroupTemplate));
    java.util.Objects.requireNonNull((moduleTemplate));
    java.util.Objects.requireNonNull((importTemplate));
    java.util.Objects.requireNonNull((findImports));
    this.language = language;
    this.fileExtension = fileExtension;
    this.encodeTerm = encodeTerm;
    this.encodeType = encodeType;
    this.formatTestName = formatTestName;
    this.formatModuleName = formatModuleName;
    this.testCaseTemplate = testCaseTemplate;
    this.testGroupTemplate = testGroupTemplate;
    this.moduleTemplate = moduleTemplate;
    this.importTemplate = importTemplate;
    this.findImports = findImports;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TestCodec)) {
      return false;
    }
    TestCodec o = (TestCodec) (other);
    return language.equals(o.language) && fileExtension.equals(o.fileExtension) && encodeTerm.equals(o.encodeTerm) && encodeType.equals(o.encodeType) && formatTestName.equals(o.formatTestName) && formatModuleName.equals(o.formatModuleName) && testCaseTemplate.equals(o.testCaseTemplate) && testGroupTemplate.equals(o.testGroupTemplate) && moduleTemplate.equals(o.moduleTemplate) && importTemplate.equals(o.importTemplate) && findImports.equals(o.findImports);
  }
  
  @Override
  public int hashCode() {
    return 2 * language.hashCode() + 3 * fileExtension.hashCode() + 5 * encodeTerm.hashCode() + 7 * encodeType.hashCode() + 11 * formatTestName.hashCode() + 13 * formatModuleName.hashCode() + 17 * testCaseTemplate.hashCode() + 19 * testGroupTemplate.hashCode() + 23 * moduleTemplate.hashCode() + 29 * importTemplate.hashCode() + 31 * findImports.hashCode();
  }
  
  public TestCodec withLanguage(hydra.coders.LanguageName language) {
    java.util.Objects.requireNonNull((language));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withFileExtension(hydra.module.FileExtension fileExtension) {
    java.util.Objects.requireNonNull((fileExtension));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withEncodeTerm(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, String>> encodeTerm) {
    java.util.Objects.requireNonNull((encodeTerm));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withEncodeType(java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, String>> encodeType) {
    java.util.Objects.requireNonNull((encodeType));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withFormatTestName(java.util.function.Function<String, String> formatTestName) {
    java.util.Objects.requireNonNull((formatTestName));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withFormatModuleName(java.util.function.Function<hydra.module.Namespace, String> formatModuleName) {
    java.util.Objects.requireNonNull((formatModuleName));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withTestCaseTemplate(String testCaseTemplate) {
    java.util.Objects.requireNonNull((testCaseTemplate));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withTestGroupTemplate(String testGroupTemplate) {
    java.util.Objects.requireNonNull((testGroupTemplate));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withModuleTemplate(String moduleTemplate) {
    java.util.Objects.requireNonNull((moduleTemplate));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withImportTemplate(String importTemplate) {
    java.util.Objects.requireNonNull((importTemplate));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withFindImports(java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<String>> findImports) {
    java.util.Objects.requireNonNull((findImports));
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
}
