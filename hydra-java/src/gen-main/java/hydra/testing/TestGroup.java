// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A collection of test cases with a name and optional description
 */
public class TestGroup implements Serializable, Comparable<TestGroup> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TestGroup");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_SUBGROUPS = new hydra.core.Name("subgroups");
  
  public static final hydra.core.Name FIELD_NAME_CASES = new hydra.core.Name("cases");
  
  /**
   * The name of the test group
   */
  public final String name;
  
  /**
   * An optional description of the group
   */
  public final hydra.util.Maybe<String> description;
  
  /**
   * Nested test groups
   */
  public final java.util.List<hydra.testing.TestGroup> subgroups;
  
  /**
   * The test cases in this group
   */
  public final java.util.List<hydra.testing.TestCaseWithMetadata> cases;
  
  public TestGroup (String name, hydra.util.Maybe<String> description, java.util.List<hydra.testing.TestGroup> subgroups, java.util.List<hydra.testing.TestCaseWithMetadata> cases) {
    this.name = name;
    this.description = description;
    this.subgroups = subgroups;
    this.cases = cases;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TestGroup)) {
      return false;
    }
    TestGroup o = (TestGroup) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.description,
      o.description) && java.util.Objects.equals(
      this.subgroups,
      o.subgroups) && java.util.Objects.equals(
      this.cases,
      o.cases);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(description) + 5 * java.util.Objects.hashCode(subgroups) + 7 * java.util.Objects.hashCode(cases);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TestGroup other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      description.hashCode(),
      other.description.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      subgroups.hashCode(),
      other.subgroups.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      cases.hashCode(),
      other.cases.hashCode());
  }
  
  public TestGroup withName(String name) {
    return new TestGroup(name, description, subgroups, cases);
  }
  
  public TestGroup withDescription(hydra.util.Maybe<String> description) {
    return new TestGroup(name, description, subgroups, cases);
  }
  
  public TestGroup withSubgroups(java.util.List<hydra.testing.TestGroup> subgroups) {
    return new TestGroup(name, description, subgroups, cases);
  }
  
  public TestGroup withCases(java.util.List<hydra.testing.TestCaseWithMetadata> cases) {
    return new TestGroup(name, description, subgroups, cases);
  }
}
