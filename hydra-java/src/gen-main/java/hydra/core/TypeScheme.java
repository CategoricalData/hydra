package hydra.core;

/**
 * A type expression together with free type variables occurring in the expression
 */
public class TypeScheme<M> {
  public final java.util.List<VariableType> variables;
  
  public final Type<M> type;
  
  public TypeScheme (java.util.List<VariableType> variables, Type<M> type) {
    this.variables = variables;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeScheme)) {
      return false;
    }
    TypeScheme o = (TypeScheme) (other);
    return variables.equals(o.variables) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * variables.hashCode() + 3 * type.hashCode();
  }
  
  public TypeScheme withVariables(java.util.List<VariableType> variables) {
    return new TypeScheme(variables, type);
  }
  
  public TypeScheme withType(Type<M> type) {
    return new TypeScheme(variables, type);
  }
}