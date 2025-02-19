// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class UnionMemberTypes_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.UnionMemberTypes_Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_OR = new hydra.core.Name("or");
  
  public static final hydra.core.Name FIELD_NAME_NAMED_TYPE = new hydra.core.Name("namedType");
  
  public final hydra.util.Opt<java.lang.Void> or;
  
  public final hydra.ext.org.graphql.syntax.NamedType namedType;
  
  public UnionMemberTypes_Sequence2 (hydra.util.Opt<java.lang.Void> or, hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((or));
    java.util.Objects.requireNonNull((namedType));
    this.or = or;
    this.namedType = namedType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionMemberTypes_Sequence2)) {
      return false;
    }
    UnionMemberTypes_Sequence2 o = (UnionMemberTypes_Sequence2) (other);
    return or.equals(o.or) && namedType.equals(o.namedType);
  }
  
  @Override
  public int hashCode() {
    return 2 * or.hashCode() + 3 * namedType.hashCode();
  }
  
  public UnionMemberTypes_Sequence2 withOr(hydra.util.Opt<java.lang.Void> or) {
    java.util.Objects.requireNonNull((or));
    return new UnionMemberTypes_Sequence2(or, namedType);
  }
  
  public UnionMemberTypes_Sequence2 withNamedType(hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((namedType));
    return new UnionMemberTypes_Sequence2(or, namedType);
  }
}