// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * Type variables that appear free in a type but are not bound in scope
 */
public class UnboundTypeVariablesError implements Serializable, Comparable<UnboundTypeVariablesError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.UnboundTypeVariablesError");
  
  public static final hydra.core.Name VARIABLES = new hydra.core.Name("variables");
  
  public static final hydra.core.Name TYPE = new hydra.core.Name("type");
  
  /**
   * The set of unbound type variable names
   */
  public final java.util.Set<hydra.core.Name> variables;
  
  /**
   * The type containing the unbound variables
   */
  public final hydra.core.Type type;
  
  public UnboundTypeVariablesError (java.util.Set<hydra.core.Name> variables, hydra.core.Type type) {
    this.variables = variables;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnboundTypeVariablesError)) {
      return false;
    }
    UnboundTypeVariablesError o = (UnboundTypeVariablesError) other;
    return java.util.Objects.equals(
      this.variables,
      o.variables) && java.util.Objects.equals(
      this.type,
      o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variables) + 3 * java.util.Objects.hashCode(type);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnboundTypeVariablesError other) {
    int cmp = 0;
    cmp = Integer.compare(
      variables.hashCode(),
      other.variables.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) type).compareTo(other.type);
  }
  
  public UnboundTypeVariablesError withVariables(java.util.Set<hydra.core.Name> variables) {
    return new UnboundTypeVariablesError(variables, type);
  }
  
  public UnboundTypeVariablesError withType(hydra.core.Type type) {
    return new UnboundTypeVariablesError(variables, type);
  }
}
