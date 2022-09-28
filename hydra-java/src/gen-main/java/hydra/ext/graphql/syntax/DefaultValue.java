package hydra.ext.graphql.syntax;

public class DefaultValue {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.DefaultValue");
  
  public final hydra.ext.graphql.syntax.Value value;
  
  public DefaultValue (hydra.ext.graphql.syntax.Value value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DefaultValue)) {
      return false;
    }
    DefaultValue o = (DefaultValue) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}