// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * A term, type, literal, or related value had a shape other than the one expected
 */
public class UnexpectedShapeError implements Serializable, Comparable<UnexpectedShapeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.UnexpectedShapeError");

  public static final hydra.core.Name EXPECTED = new hydra.core.Name("expected");

  public static final hydra.core.Name ACTUAL = new hydra.core.Name("actual");

  /**
   * A description of the expected shape
   */
  public final String expected;

  /**
   * A description of the shape actually encountered
   */
  public final String actual;

  public UnexpectedShapeError (String expected, String actual) {
    this.expected = expected;
    this.actual = actual;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnexpectedShapeError)) {
      return false;
    }
    UnexpectedShapeError o = (UnexpectedShapeError) other;
    return java.util.Objects.equals(
      this.expected,
      o.expected) && java.util.Objects.equals(
      this.actual,
      o.actual);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expected) + 3 * java.util.Objects.hashCode(actual);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnexpectedShapeError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expected,
      other.expected);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      actual,
      other.actual);
  }

  public UnexpectedShapeError withExpected(String expected) {
    return new UnexpectedShapeError(expected, actual);
  }

  public UnexpectedShapeError withActual(String actual) {
    return new UnexpectedShapeError(expected, actual);
  }
}
