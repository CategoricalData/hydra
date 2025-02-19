// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags
 */
public class TestCaseWithMetadata implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TestCaseWithMetadata");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_CASE = new hydra.core.Name("case");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_TAGS = new hydra.core.Name("tags");
  
  public final String name;
  
  public final hydra.testing.TestCase case_;
  
  public final hydra.util.Opt<String> description;
  
  public final java.util.List<hydra.testing.Tag> tags;
  
  public TestCaseWithMetadata (String name, hydra.testing.TestCase case_, hydra.util.Opt<String> description, java.util.List<hydra.testing.Tag> tags) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((case_));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((tags));
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
    TestCaseWithMetadata o = (TestCaseWithMetadata) (other);
    return name.equals(o.name) && case_.equals(o.case_) && description.equals(o.description) && tags.equals(o.tags);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * case_.hashCode() + 5 * description.hashCode() + 7 * tags.hashCode();
  }
  
  public TestCaseWithMetadata withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new TestCaseWithMetadata(name, case_, description, tags);
  }
  
  public TestCaseWithMetadata withCase(hydra.testing.TestCase case_) {
    java.util.Objects.requireNonNull((case_));
    return new TestCaseWithMetadata(name, case_, description, tags);
  }
  
  public TestCaseWithMetadata withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new TestCaseWithMetadata(name, case_, description, tags);
  }
  
  public TestCaseWithMetadata withTags(java.util.List<hydra.testing.Tag> tags) {
    java.util.Objects.requireNonNull((tags));
    return new TestCaseWithMetadata(name, case_, description, tags);
  }
}