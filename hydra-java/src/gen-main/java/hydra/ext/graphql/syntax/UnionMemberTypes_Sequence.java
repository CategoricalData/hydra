package hydra.ext.graphql.syntax;

public class UnionMemberTypes_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.UnionMemberTypes.Sequence");
  
  public final hydra.ext.graphql.syntax.UnionMemberTypes unionMemberTypes;
  
  public final hydra.ext.graphql.syntax.NamedType namedType;
  
  public UnionMemberTypes_Sequence (hydra.ext.graphql.syntax.UnionMemberTypes unionMemberTypes, hydra.ext.graphql.syntax.NamedType namedType) {
    this.unionMemberTypes = unionMemberTypes;
    this.namedType = namedType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionMemberTypes_Sequence)) {
      return false;
    }
    UnionMemberTypes_Sequence o = (UnionMemberTypes_Sequence) (other);
    return unionMemberTypes.equals(o.unionMemberTypes) && namedType.equals(o.namedType);
  }
  
  @Override
  public int hashCode() {
    return 2 * unionMemberTypes.hashCode() + 3 * namedType.hashCode();
  }
  
  public UnionMemberTypes_Sequence withUnionMemberTypes(hydra.ext.graphql.syntax.UnionMemberTypes unionMemberTypes) {
    return new UnionMemberTypes_Sequence(unionMemberTypes, namedType);
  }
  
  public UnionMemberTypes_Sequence withNamedType(hydra.ext.graphql.syntax.NamedType namedType) {
    return new UnionMemberTypes_Sequence(unionMemberTypes, namedType);
  }
}