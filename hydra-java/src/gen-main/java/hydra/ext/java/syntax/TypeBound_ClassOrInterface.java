// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TypeBound_ClassOrInterface implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.TypeBound_ClassOrInterface");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ADDITIONAL = new hydra.core.Name("additional");
  
  public final hydra.ext.java.syntax.ClassOrInterfaceType type;
  
  public final java.util.List<hydra.ext.java.syntax.AdditionalBound> additional;
  
  public TypeBound_ClassOrInterface (hydra.ext.java.syntax.ClassOrInterfaceType type, java.util.List<hydra.ext.java.syntax.AdditionalBound> additional) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((additional));
    this.type = type;
    this.additional = additional;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeBound_ClassOrInterface)) {
      return false;
    }
    TypeBound_ClassOrInterface o = (TypeBound_ClassOrInterface) (other);
    return type.equals(o.type) && additional.equals(o.additional);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * additional.hashCode();
  }
  
  public TypeBound_ClassOrInterface withType(hydra.ext.java.syntax.ClassOrInterfaceType type) {
    java.util.Objects.requireNonNull((type));
    return new TypeBound_ClassOrInterface(type, additional);
  }
  
  public TypeBound_ClassOrInterface withAdditional(java.util.List<hydra.ext.java.syntax.AdditionalBound> additional) {
    java.util.Objects.requireNonNull((additional));
    return new TypeBound_ClassOrInterface(type, additional);
  }
}