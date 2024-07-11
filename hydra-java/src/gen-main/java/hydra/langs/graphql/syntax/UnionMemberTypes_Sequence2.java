// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class UnionMemberTypes_Sequence2 implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.UnionMemberTypes.Sequence2");
  
  public final hydra.util.Opt<java.lang.Void> or;
  
  public final hydra.langs.graphql.syntax.NamedType namedType;
  
  public UnionMemberTypes_Sequence2 (hydra.util.Opt<java.lang.Void> or, hydra.langs.graphql.syntax.NamedType namedType) {
    if (or == null) {
      throw new IllegalArgumentException("null value for 'or' argument");
    }
    if (namedType == null) {
      throw new IllegalArgumentException("null value for 'namedType' argument");
    }
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
    if (or == null) {
      throw new IllegalArgumentException("null value for 'or' argument");
    }
    return new UnionMemberTypes_Sequence2(or, namedType);
  }
  
  public UnionMemberTypes_Sequence2 withNamedType(hydra.langs.graphql.syntax.NamedType namedType) {
    if (namedType == null) {
      throw new IllegalArgumentException("null value for 'namedType' argument");
    }
    return new UnionMemberTypes_Sequence2(or, namedType);
  }
}