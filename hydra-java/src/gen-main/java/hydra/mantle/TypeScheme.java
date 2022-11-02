package hydra.mantle;

/**
 * A type expression together with free type variables occurring in the expression
 */
public class TypeScheme<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/mantle.TypeScheme");
  
  public final java.util.List<hydra.core.VariableType> variables;
  
  public final hydra.core.Type<M> type;
  
  public TypeScheme (java.util.List<hydra.core.VariableType> variables, hydra.core.Type<M> type) {
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
  
  public TypeScheme withVariables(java.util.List<hydra.core.VariableType> variables) {
    return new TypeScheme(variables, type);
  }
  
  public TypeScheme withType(hydra.core.Type<M> type) {
    return new TypeScheme(variables, type);
  }
}