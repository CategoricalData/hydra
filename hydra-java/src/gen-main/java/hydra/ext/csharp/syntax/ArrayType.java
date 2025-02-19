// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ArrayType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ArrayType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_RANK = new hydra.core.Name("rank");
  
  public final hydra.ext.csharp.syntax.NonArrayType type;
  
  public final java.util.List<hydra.ext.csharp.syntax.RankSpecifier> rank;
  
  public ArrayType (hydra.ext.csharp.syntax.NonArrayType type, java.util.List<hydra.ext.csharp.syntax.RankSpecifier> rank) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((rank));
    this.type = type;
    this.rank = rank;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayType)) {
      return false;
    }
    ArrayType o = (ArrayType) (other);
    return type.equals(o.type) && rank.equals(o.rank);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * rank.hashCode();
  }
  
  public ArrayType withType(hydra.ext.csharp.syntax.NonArrayType type) {
    java.util.Objects.requireNonNull((type));
    return new ArrayType(type, rank);
  }
  
  public ArrayType withRank(java.util.List<hydra.ext.csharp.syntax.RankSpecifier> rank) {
    java.util.Objects.requireNonNull((rank));
    return new ArrayType(type, rank);
  }
}