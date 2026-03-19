// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A destructuring binding in a let expression
 */
public class DestructuringBinding implements Serializable, Comparable<DestructuringBinding> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.DestructuringBinding");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  /**
   * The destructuring pattern
   */
  public final hydra.ext.lisp.syntax.DestructuringPattern pattern;

  /**
   * The value to destructure
   */
  public final hydra.ext.lisp.syntax.Expression value;

  public DestructuringBinding (hydra.ext.lisp.syntax.DestructuringPattern pattern, hydra.ext.lisp.syntax.Expression value) {
    this.pattern = pattern;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DestructuringBinding)) {
      return false;
    }
    DestructuringBinding o = (DestructuringBinding) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DestructuringBinding other) {
    int cmp = 0;
    cmp = ((Comparable) pattern).compareTo(other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }

  public DestructuringBinding withPattern(hydra.ext.lisp.syntax.DestructuringPattern pattern) {
    return new DestructuringBinding(pattern, value);
  }

  public DestructuringBinding withValue(hydra.ext.lisp.syntax.Expression value) {
    return new DestructuringBinding(pattern, value);
  }
}
