package hydra.ext.graphql.syntax;

public class Variable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.Variable");
  
  public final hydra.ext.graphql.syntax.Name value;
  
  public Variable (hydra.ext.graphql.syntax.Name value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Variable)) {
      return false;
    }
    Variable o = (Variable) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}