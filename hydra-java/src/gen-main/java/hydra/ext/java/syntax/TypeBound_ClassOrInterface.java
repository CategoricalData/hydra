// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TypeBound_ClassOrInterface implements Serializable, Comparable<TypeBound_ClassOrInterface> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.TypeBound_ClassOrInterface");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ADDITIONAL = new hydra.core.Name("additional");
  
  public final hydra.ext.java.syntax.ClassOrInterfaceType type;
  
  public final java.util.List<hydra.ext.java.syntax.AdditionalBound> additional;
  
  public TypeBound_ClassOrInterface (hydra.ext.java.syntax.ClassOrInterfaceType type, java.util.List<hydra.ext.java.syntax.AdditionalBound> additional) {
    this.type = type;
    this.additional = additional;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeBound_ClassOrInterface)) {
      return false;
    }
    TypeBound_ClassOrInterface o = (TypeBound_ClassOrInterface) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.additional,
      o.additional);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(additional);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeBound_ClassOrInterface other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      additional.hashCode(),
      other.additional.hashCode());
  }
  
  public TypeBound_ClassOrInterface withType(hydra.ext.java.syntax.ClassOrInterfaceType type) {
    return new TypeBound_ClassOrInterface(type, additional);
  }
  
  public TypeBound_ClassOrInterface withAdditional(java.util.List<hydra.ext.java.syntax.AdditionalBound> additional) {
    return new TypeBound_ClassOrInterface(type, additional);
  }
}
