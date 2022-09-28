package hydra.ext.graphql.syntax;

public class Description {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.Description");
  
  public final hydra.ext.graphql.syntax.StringValue value;
  
  public Description (hydra.ext.graphql.syntax.StringValue value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Description)) {
      return false;
    }
    Description o = (Description) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}