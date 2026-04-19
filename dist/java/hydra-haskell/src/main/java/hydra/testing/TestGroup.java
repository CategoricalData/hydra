// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A collection of test cases with a name and optional description
 */
public class TestGroup implements Serializable, Comparable<TestGroup> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.TestGroup");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name DESCRIPTION = new hydra.core.Name("description");

  public static final hydra.core.Name SUBGROUPS = new hydra.core.Name("subgroups");

  public static final hydra.core.Name CASES = new hydra.core.Name("cases");

  /**
   * A short name for the test group
   */
  public final String name;

  /**
   * An optional longer description of the test group
   */
  public final hydra.util.Maybe<String> description;

  /**
   * Zero or more subgroups
   */
  public final java.util.List<hydra.testing.TestGroup> subgroups;

  /**
   * Zero or more test cases
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
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      description,
      other.description);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      subgroups,
      other.subgroups);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      cases,
      other.cases);
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
