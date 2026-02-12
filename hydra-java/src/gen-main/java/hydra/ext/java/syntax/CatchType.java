// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CatchType implements Serializable, Comparable<CatchType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CatchType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  public final hydra.ext.java.syntax.UnannClassType type;
  
  public final java.util.List<hydra.ext.java.syntax.ClassType> types;
  
  public CatchType (hydra.ext.java.syntax.UnannClassType type, java.util.List<hydra.ext.java.syntax.ClassType> types) {
    this.type = type;
    this.types = types;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatchType)) {
      return false;
    }
    CatchType o = (CatchType) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.types,
      o.types);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(types);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CatchType other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      types.hashCode(),
      other.types.hashCode());
  }
  
  public CatchType withType(hydra.ext.java.syntax.UnannClassType type) {
    return new CatchType(type, types);
  }
  
  public CatchType withTypes(java.util.List<hydra.ext.java.syntax.ClassType> types) {
    return new CatchType(type, types);
  }
}
