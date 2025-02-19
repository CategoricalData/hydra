// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class UnionMemberTypes_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.UnionMemberTypes_Sequence");
  
  public static final hydra.core.Name FIELD_NAME_UNION_MEMBER_TYPES = new hydra.core.Name("unionMemberTypes");
  
  public static final hydra.core.Name FIELD_NAME_NAMED_TYPE = new hydra.core.Name("namedType");
  
  public final hydra.ext.org.graphql.syntax.UnionMemberTypes unionMemberTypes;
  
  public final hydra.ext.org.graphql.syntax.NamedType namedType;
  
  public UnionMemberTypes_Sequence (hydra.ext.org.graphql.syntax.UnionMemberTypes unionMemberTypes, hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((unionMemberTypes));
    java.util.Objects.requireNonNull((namedType));
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
  
  public UnionMemberTypes_Sequence withUnionMemberTypes(hydra.ext.org.graphql.syntax.UnionMemberTypes unionMemberTypes) {
    java.util.Objects.requireNonNull((unionMemberTypes));
    return new UnionMemberTypes_Sequence(unionMemberTypes, namedType);
  }
  
  public UnionMemberTypes_Sequence withNamedType(hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((namedType));
    return new UnionMemberTypes_Sequence(unionMemberTypes, namedType);
  }
}