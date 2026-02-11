// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags
 */
public class TestCaseWithMetadata implements Serializable, Comparable<TestCaseWithMetadata> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TestCaseWithMetadata");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_CASE = new hydra.core.Name("case");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_TAGS = new hydra.core.Name("tags");
  
  /**
   * The name of the test case
   */
  public final String name;
  
  /**
   * The test case itself
   */
  public final hydra.testing.TestCase case_;
  
  /**
   * An optional description of the test
   */
  public final hydra.util.Maybe<String> description;
  
  /**
   * Zero or more tags for categorizing the test
   */
  public final java.util.List<hydra.testing.Tag> tags;
  
  public TestCaseWithMetadata (String name, hydra.testing.TestCase case_, hydra.util.Maybe<String> description, java.util.List<hydra.testing.Tag> tags) {
    this.name = name;
    this.case_ = case_;
    this.description = description;
    this.tags = tags;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TestCaseWithMetadata)) {
      return false;
    }
    TestCaseWithMetadata o = (TestCaseWithMetadata) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.case_,
      o.case_) && java.util.Objects.equals(
      this.description,
      o.description) && java.util.Objects.equals(
      this.tags,
      o.tags);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(case_) + 5 * java.util.Objects.hashCode(description) + 7 * java.util.Objects.hashCode(tags);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TestCaseWithMetadata other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) case_).compareTo(other.case_);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      description.hashCode(),
      other.description.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      tags.hashCode(),
      other.tags.hashCode());
  }
  
  public TestCaseWithMetadata withName(String name) {
    return new TestCaseWithMetadata(name, case_, description, tags);
  }
  
  public TestCaseWithMetadata withCase(hydra.testing.TestCase case_) {
    return new TestCaseWithMetadata(name, case_, description, tags);
  }
  
  public TestCaseWithMetadata withDescription(hydra.util.Maybe<String> description) {
    return new TestCaseWithMetadata(name, case_, description, tags);
  }
  
  public TestCaseWithMetadata withTags(java.util.List<hydra.testing.Tag> tags) {
    return new TestCaseWithMetadata(name, case_, description, tags);
  }
}
