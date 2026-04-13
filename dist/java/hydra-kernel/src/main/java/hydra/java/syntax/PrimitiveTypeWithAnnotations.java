// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class PrimitiveTypeWithAnnotations implements Serializable, Comparable<PrimitiveTypeWithAnnotations> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.PrimitiveTypeWithAnnotations");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public final hydra.java.syntax.PrimitiveType type;

  public final java.util.List<hydra.java.syntax.Annotation> annotations;

  public PrimitiveTypeWithAnnotations (hydra.java.syntax.PrimitiveType type, java.util.List<hydra.java.syntax.Annotation> annotations) {
    this.type = type;
    this.annotations = annotations;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimitiveTypeWithAnnotations)) {
      return false;
    }
    PrimitiveTypeWithAnnotations o = (PrimitiveTypeWithAnnotations) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.annotations,
      o.annotations);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(annotations);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PrimitiveTypeWithAnnotations other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      annotations,
      other.annotations);
  }

  public PrimitiveTypeWithAnnotations withType(hydra.java.syntax.PrimitiveType type) {
    return new PrimitiveTypeWithAnnotations(type, annotations);
  }

  public PrimitiveTypeWithAnnotations withAnnotations(java.util.List<hydra.java.syntax.Annotation> annotations) {
    return new PrimitiveTypeWithAnnotations(type, annotations);
  }
}
