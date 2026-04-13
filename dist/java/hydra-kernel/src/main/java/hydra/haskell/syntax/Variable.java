// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A type variable
 */
public class Variable implements Serializable, Comparable<Variable> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.Variable");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.haskell.syntax.Name value;

  public Variable (hydra.haskell.syntax.Name value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Variable)) {
      return false;
    }
    Variable o = (Variable) other;
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
  public int compareTo(Variable other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
