// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A global variable definition. Serializes as (def name value) in Clojure, (defvar name value) in Emacs Lisp and Common Lisp, (define name value) in Scheme
 */
public class VariableDefinition implements Serializable, Comparable<VariableDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.VariableDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name DOC = new hydra.core.Name("doc");

  /**
   * The variable name
   */
  public final hydra.ext.lisp.syntax.Symbol name;

  /**
   * The initial value
   */
  public final hydra.ext.lisp.syntax.Expression value;

  /**
   * Optional docstring
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc;

  public VariableDefinition (hydra.ext.lisp.syntax.Symbol name, hydra.ext.lisp.syntax.Expression value, hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc) {
    this.name = name;
    this.value = value;
    this.doc = doc;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableDefinition)) {
      return false;
    }
    VariableDefinition o = (VariableDefinition) other;
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
  public int compareTo(VariableDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      doc,
      other.doc);
  }

  public VariableDefinition withName(hydra.ext.lisp.syntax.Symbol name) {
    return new VariableDefinition(name, value, doc);
  }

  public VariableDefinition withValue(hydra.ext.lisp.syntax.Expression value) {
    return new VariableDefinition(name, value, doc);
  }

  public VariableDefinition withDoc(hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc) {
    return new VariableDefinition(name, value, doc);
  }
}
