// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class TypeBound_ClassOrInterface implements Serializable, Comparable<TypeBound_ClassOrInterface> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.TypeBound_ClassOrInterface");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name ADDITIONAL = new hydra.core.Name("additional");

  public final hydra.java.syntax.ClassOrInterfaceType type;

  public final java.util.List<hydra.java.syntax.AdditionalBound> additional;

  public TypeBound_ClassOrInterface (hydra.java.syntax.ClassOrInterfaceType type, java.util.List<hydra.java.syntax.AdditionalBound> additional) {
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
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      additional,
      other.additional);
  }

  public TypeBound_ClassOrInterface withType(hydra.java.syntax.ClassOrInterfaceType type) {
    return new TypeBound_ClassOrInterface(type, additional);
  }

  public TypeBound_ClassOrInterface withAdditional(java.util.List<hydra.java.syntax.AdditionalBound> additional) {
    return new TypeBound_ClassOrInterface(type, additional);
  }
}
