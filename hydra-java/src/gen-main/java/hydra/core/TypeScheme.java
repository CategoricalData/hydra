package hydra.core;

public class TypeScheme {
  public final java.util.List<hydra.core.TypeVariable> variables;
  
  public final hydra.core.Type type;
  
  /**
   * Constructs an immutable TypeScheme object
   */
  public TypeScheme(java.util.List<hydra.core.TypeVariable> variables, hydra.core.Type type) {
    this.variables = variables;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeScheme)) {
        return false;
    }
    TypeScheme o = (TypeScheme) other;
    return variables.equals(o.variables)
        && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * variables.hashCode()
        + 3 * type.hashCode();
  }
  
  /**
   * Construct a new immutable TypeScheme object in which variables is overridden
   */
  public TypeScheme withVariables(java.util.List<hydra.core.TypeVariable> variables) {
    return new TypeScheme(variables, type);
  }
  
  /**
   * Construct a new immutable TypeScheme object in which type is overridden
   */
  public TypeScheme withType(hydra.core.Type type) {
    return new TypeScheme(variables, type);
  }
}
