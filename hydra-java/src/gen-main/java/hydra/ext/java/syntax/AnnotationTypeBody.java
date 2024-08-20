// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class AnnotationTypeBody implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.AnnotationTypeBody");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<java.util.List<hydra.ext.java.syntax.AnnotationTypeMemberDeclaration>> value;
  
  public AnnotationTypeBody (java.util.List<java.util.List<hydra.ext.java.syntax.AnnotationTypeMemberDeclaration>> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationTypeBody)) {
      return false;
    }
    AnnotationTypeBody o = (AnnotationTypeBody) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
