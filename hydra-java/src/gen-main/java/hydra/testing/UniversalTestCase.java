// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A universal test case: the actual and expected values are both strings
 */
public class UniversalTestCase implements Serializable, Comparable<UniversalTestCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.UniversalTestCase");

  public static final hydra.core.Name ACTUAL = new hydra.core.Name("actual");

  public static final hydra.core.Name EXPECTED = new hydra.core.Name("expected");

  /**
   * The actual result (a string-valued expression)
   */
  public final String actual;

  /**
   * The expected result (a string literal)
   */
  public final String expected;

  public UniversalTestCase (String actual, String expected) {
    this.actual = actual;
    this.expected = expected;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UniversalTestCase)) {
      return false;
    }
    UniversalTestCase o = (UniversalTestCase) other;
    return java.util.Objects.equals(
      this.actual,
      o.actual) && java.util.Objects.equals(
      this.expected,
      o.expected);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(actual) + 3 * java.util.Objects.hashCode(expected);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UniversalTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) actual).compareTo(other.actual);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expected).compareTo(other.expected);
  }

  public UniversalTestCase withActual(String actual) {
    return new UniversalTestCase(actual, expected);
  }

  public UniversalTestCase withExpected(String expected) {
    return new UniversalTestCase(actual, expected);
  }
}
