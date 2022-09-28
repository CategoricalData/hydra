package hydra.ext.graphql.syntax;

public class NonNullType_List {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.NonNullType.List");
  
  public final hydra.ext.graphql.syntax.ListType listType;
  
  public NonNullType_List (hydra.ext.graphql.syntax.ListType listType) {
    this.listType = listType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonNullType_List)) {
      return false;
    }
    NonNullType_List o = (NonNullType_List) (other);
    return listType.equals(o.listType);
  }
  
  @Override
  public int hashCode() {
    return 2 * listType.hashCode();
  }
}