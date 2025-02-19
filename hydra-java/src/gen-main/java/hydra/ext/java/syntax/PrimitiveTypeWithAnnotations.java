// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class PrimitiveTypeWithAnnotations implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.PrimitiveTypeWithAnnotations");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public final hydra.ext.java.syntax.PrimitiveType type;
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public PrimitiveTypeWithAnnotations (hydra.ext.java.syntax.PrimitiveType type, java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((annotations));
    this.type = type;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimitiveTypeWithAnnotations)) {
      return false;
    }
    PrimitiveTypeWithAnnotations o = (PrimitiveTypeWithAnnotations) (other);
    return type.equals(o.type) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * annotations.hashCode();
  }
  
  public PrimitiveTypeWithAnnotations withType(hydra.ext.java.syntax.PrimitiveType type) {
    java.util.Objects.requireNonNull((type));
    return new PrimitiveTypeWithAnnotations(type, annotations);
  }
  
  public PrimitiveTypeWithAnnotations withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new PrimitiveTypeWithAnnotations(type, annotations);
  }
}