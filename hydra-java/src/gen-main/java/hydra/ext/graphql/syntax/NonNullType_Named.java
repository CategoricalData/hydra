package hydra.ext.graphql.syntax;

public class NonNullType_Named {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.NonNullType.Named");
  
  public final hydra.ext.graphql.syntax.NamedType namedType;
  
  public NonNullType_Named (hydra.ext.graphql.syntax.NamedType namedType) {
    this.namedType = namedType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonNullType_Named)) {
      return false;
    }
    NonNullType_Named o = (NonNullType_Named) (other);
    return namedType.equals(o.namedType);
  }
  
  @Override
  public int hashCode() {
    return 2 * namedType.hashCode();
  }
}