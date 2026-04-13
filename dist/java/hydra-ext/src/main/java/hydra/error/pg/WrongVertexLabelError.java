// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An error indicating that a vertex has the wrong label
 */
public class WrongVertexLabelError implements Serializable, Comparable<WrongVertexLabelError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.WrongVertexLabelError");

  public static final hydra.core.Name EXPECTED = new hydra.core.Name("expected");

  public static final hydra.core.Name ACTUAL = new hydra.core.Name("actual");

  /**
   * The expected vertex label
   */
  public final hydra.pg.model.VertexLabel expected;

  /**
   * The actual vertex label
   */
  public final hydra.pg.model.VertexLabel actual;

  public WrongVertexLabelError (hydra.pg.model.VertexLabel expected, hydra.pg.model.VertexLabel actual) {
    this.expected = expected;
    this.actual = actual;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WrongVertexLabelError)) {
      return false;
    }
    WrongVertexLabelError o = (WrongVertexLabelError) other;
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
  public int compareTo(WrongVertexLabelError other) {
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

  public WrongVertexLabelError withExpected(hydra.pg.model.VertexLabel expected) {
    return new WrongVertexLabelError(expected, actual);
  }

  public WrongVertexLabelError withActual(hydra.pg.model.VertexLabel actual) {
    return new WrongVertexLabelError(expected, actual);
  }
}
