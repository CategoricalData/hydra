// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A simple name-value binding in a let expression
 */
public class SimpleBinding implements Serializable, Comparable<SimpleBinding> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.SimpleBinding");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  /**
   * The bound variable
   */
  public final hydra.lisp.syntax.Symbol name;

  /**
   * The value expression
   */
  public final hydra.lisp.syntax.Expression value;

  public SimpleBinding (hydra.lisp.syntax.Symbol name, hydra.lisp.syntax.Expression value) {
    this.name = name;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleBinding)) {
      return false;
    }
    SimpleBinding o = (SimpleBinding) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SimpleBinding other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public SimpleBinding withName(hydra.lisp.syntax.Symbol name) {
    return new SimpleBinding(name, value);
  }

  public SimpleBinding withValue(hydra.lisp.syntax.Expression value) {
    return new SimpleBinding(name, value);
  }
}
