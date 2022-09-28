package hydra.ext.graphql.syntax;

public class TypeSystemDocment {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.TypeSystemDocment");
  
  public final java.util.List<hydra.ext.graphql.syntax.TypeSystemDefinition> value;
  
  public TypeSystemDocment (java.util.List<hydra.ext.graphql.syntax.TypeSystemDefinition> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSystemDocment)) {
      return false;
    }
    TypeSystemDocment o = (TypeSystemDocment) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}