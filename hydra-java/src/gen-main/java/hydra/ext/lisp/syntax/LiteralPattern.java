// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A pattern matching a literal value
 */
public class LiteralPattern implements Serializable, Comparable<LiteralPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.LiteralPattern");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  /**
   * The literal to match
   */
  public final hydra.ext.lisp.syntax.Literal value;

  public LiteralPattern (hydra.ext.lisp.syntax.Literal value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LiteralPattern)) {
      return false;
    }
    LiteralPattern o = (LiteralPattern) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LiteralPattern other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
