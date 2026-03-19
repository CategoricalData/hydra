// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A reference to a variable by name
 */
public class VariableReference implements Serializable, Comparable<VariableReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.VariableReference");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name FUNCTION_NAMESPACE = new hydra.core.Name("functionNamespace");

  /**
   * The variable name
   */
  public final hydra.ext.lisp.syntax.Symbol name;

  /**
   * Whether to reference from the function namespace. In Lisp-2 dialects (Common Lisp), this emits #'name. In Lisp-1 dialects, this has no effect.
   */
  public final Boolean functionNamespace;

  public VariableReference (hydra.ext.lisp.syntax.Symbol name, Boolean functionNamespace) {
    this.name = name;
    this.functionNamespace = functionNamespace;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableReference)) {
      return false;
    }
    VariableReference o = (VariableReference) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.functionNamespace,
      o.functionNamespace);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(functionNamespace);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VariableReference other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) functionNamespace).compareTo(other.functionNamespace);
  }

  public VariableReference withName(hydra.ext.lisp.syntax.Symbol name) {
    return new VariableReference(name, functionNamespace);
  }

  public VariableReference withFunctionNamespace(Boolean functionNamespace) {
    return new VariableReference(name, functionNamespace);
  }
}
