// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A codec for generating compiled test files from test groups into a target programming language
 */
public class TestCodec implements Serializable, Comparable<TestCodec> {
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
    return java.util.Objects.equals(
      this.language,
      o.language) && java.util.Objects.equals(
      this.fileExtension,
      o.fileExtension) && java.util.Objects.equals(
      this.encodeTerm,
      o.encodeTerm) && java.util.Objects.equals(
      this.encodeType,
      o.encodeType) && java.util.Objects.equals(
      this.formatTestName,
      o.formatTestName) && java.util.Objects.equals(
      this.formatModuleName,
      o.formatModuleName) && java.util.Objects.equals(
      this.testCaseTemplate,
      o.testCaseTemplate) && java.util.Objects.equals(
      this.testGroupTemplate,
      o.testGroupTemplate) && java.util.Objects.equals(
      this.moduleTemplate,
      o.moduleTemplate) && java.util.Objects.equals(
      this.importTemplate,
      o.importTemplate) && java.util.Objects.equals(
      this.findImports,
      o.findImports);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(language) + 3 * java.util.Objects.hashCode(fileExtension) + 5 * java.util.Objects.hashCode(encodeTerm) + 7 * java.util.Objects.hashCode(encodeType) + 11 * java.util.Objects.hashCode(formatTestName) + 13 * java.util.Objects.hashCode(formatModuleName) + 17 * java.util.Objects.hashCode(testCaseTemplate) + 19 * java.util.Objects.hashCode(testGroupTemplate) + 23 * java.util.Objects.hashCode(moduleTemplate) + 29 * java.util.Objects.hashCode(importTemplate) + 31 * java.util.Objects.hashCode(findImports);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TestCodec other) {
    int cmp = 0;
    cmp = ((Comparable) (language)).compareTo(other.language);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (fileExtension)).compareTo(other.fileExtension);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      encodeTerm.hashCode(),
      other.encodeTerm.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      encodeType.hashCode(),
      other.encodeType.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      formatTestName.hashCode(),
      other.formatTestName.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      formatModuleName.hashCode(),
      other.formatModuleName.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (testCaseTemplate)).compareTo(other.testCaseTemplate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (testGroupTemplate)).compareTo(other.testGroupTemplate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (moduleTemplate)).compareTo(other.moduleTemplate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (importTemplate)).compareTo(other.importTemplate);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      findImports.hashCode(),
      other.findImports.hashCode());
  }
  
  public TestCodec withLanguage(hydra.coders.LanguageName language) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withFileExtension(hydra.module.FileExtension fileExtension) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withEncodeTerm(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, String>> encodeTerm) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withEncodeType(java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, String>> encodeType) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withFormatTestName(java.util.function.Function<String, String> formatTestName) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withFormatModuleName(java.util.function.Function<hydra.module.Namespace, String> formatModuleName) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withTestCaseTemplate(String testCaseTemplate) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withTestGroupTemplate(String testGroupTemplate) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withModuleTemplate(String moduleTemplate) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withImportTemplate(String importTemplate) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
  
  public TestCodec withFindImports(java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<String>> findImports) {
    return new TestCodec(language, fileExtension, encodeTerm, encodeType, formatTestName, formatModuleName, testCaseTemplate, testGroupTemplate, moduleTemplate, importTemplate, findImports);
  }
}
