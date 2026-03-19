// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A constant definition. Serializes as (def ^:const name value) in Clojure, (defconst name value) in Emacs Lisp, (defconstant name value) in Common Lisp. Scheme has no dedicated constant form; uses define.
 */
public class ConstantDefinition implements Serializable, Comparable<ConstantDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.ConstantDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name DOC = new hydra.core.Name("doc");

  /**
   * The constant name
   */
  public final hydra.ext.lisp.syntax.Symbol name;

  /**
   * The constant value
   */
  public final hydra.ext.lisp.syntax.Expression value;

  /**
   * Optional docstring
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc;

  public ConstantDefinition (hydra.ext.lisp.syntax.Symbol name, hydra.ext.lisp.syntax.Expression value, hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc) {
    this.name = name;
    this.value = value;
    this.doc = doc;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstantDefinition)) {
      return false;
    }
    ConstantDefinition o = (ConstantDefinition) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.doc,
      o.doc);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(value) + 5 * java.util.Objects.hashCode(doc);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstantDefinition other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) value).compareTo(other.value);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) doc).compareTo(other.doc);
  }

  public ConstantDefinition withName(hydra.ext.lisp.syntax.Symbol name) {
    return new ConstantDefinition(name, value, doc);
  }

  public ConstantDefinition withValue(hydra.ext.lisp.syntax.Expression value) {
    return new ConstantDefinition(name, value, doc);
  }

  public ConstantDefinition withDoc(hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc) {
    return new ConstantDefinition(name, value, doc);
  }
}
