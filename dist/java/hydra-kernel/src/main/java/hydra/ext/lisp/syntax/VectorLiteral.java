// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A vector literal. Serializes as [1 2 3] in Clojure and Emacs Lisp, #(1 2 3) in Common Lisp and Scheme
 */
public class VectorLiteral implements Serializable, Comparable<VectorLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.VectorLiteral");

  public static final hydra.core.Name ELEMENTS = new hydra.core.Name("elements");

  /**
   * The vector elements
   */
  public final java.util.List<hydra.ext.lisp.syntax.Expression> elements;

  public VectorLiteral (java.util.List<hydra.ext.lisp.syntax.Expression> elements) {
    this.elements = elements;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VectorLiteral)) {
      return false;
    }
    VectorLiteral o = (VectorLiteral) other;
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
  public int compareTo(VectorLiteral other) {
    return hydra.util.Comparing.compare(
      elements,
      other.elements);
  }
}
