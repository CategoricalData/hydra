package hydra.ext.graphql.syntax;

public class ListType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ListType");
  
  public final hydra.ext.graphql.syntax.Type type;
  
  public ListType (hydra.ext.graphql.syntax.Type type) {
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListType)) {
      return false;
    }
    ListType o = (ListType) (other);
    return type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode();
  }
}