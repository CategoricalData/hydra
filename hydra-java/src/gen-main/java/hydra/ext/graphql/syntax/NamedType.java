package hydra.ext.graphql.syntax;

public class NamedType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.NamedType");
  
  public final hydra.ext.graphql.syntax.Name value;
  
  public NamedType (hydra.ext.graphql.syntax.Name value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamedType)) {
      return false;
    }
    NamedType o = (NamedType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}