// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A list literal: '(1 2 3) or (list 1 2 3)
 */
public class ListLiteral implements Serializable, Comparable<ListLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.ListLiteral");

  public static final hydra.core.Name ELEMENTS = new hydra.core.Name("elements");

  public static final hydra.core.Name QUOTED = new hydra.core.Name("quoted");

  /**
   * The list elements
   */
  public final java.util.List<hydra.ext.lisp.syntax.Expression> elements;

  /**
   * Whether to use quote syntax vs constructor syntax
   */
  public final Boolean quoted;

  public ListLiteral (java.util.List<hydra.ext.lisp.syntax.Expression> elements, Boolean quoted) {
    this.elements = elements;
    this.quoted = quoted;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListLiteral)) {
      return false;
    }
    ListLiteral o = (ListLiteral) other;
    return java.util.Objects.equals(
      this.elements,
      o.elements) && java.util.Objects.equals(
      this.quoted,
      o.quoted);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(elements) + 3 * java.util.Objects.hashCode(quoted);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ListLiteral other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      elements,
      other.elements);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      quoted,
      other.quoted);
  }

  public ListLiteral withElements(java.util.List<hydra.ext.lisp.syntax.Expression> elements) {
    return new ListLiteral(elements, quoted);
  }

  public ListLiteral withQuoted(Boolean quoted) {
    return new ListLiteral(elements, quoted);
  }
}
