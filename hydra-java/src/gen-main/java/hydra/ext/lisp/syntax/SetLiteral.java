// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A set literal. Serializes as #{1 2 3} in Clojure. Other dialects use a list-based construction.
 */
public class SetLiteral implements Serializable, Comparable<SetLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.SetLiteral");

  public static final hydra.core.Name ELEMENTS = new hydra.core.Name("elements");

  /**
   * The set elements
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> elements;

  public SetLiteral (hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> elements) {
    this.elements = elements;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SetLiteral)) {
      return false;
    }
    SetLiteral o = (SetLiteral) other;
    return java.util.Objects.equals(
      this.elements,
      o.elements);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(elements);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SetLiteral other) {
    return ((Comparable) elements).compareTo(other.elements);
  }
}
