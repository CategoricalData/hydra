// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class AnnotatedIdentifier implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.AnnotatedIdentifier");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public AnnotatedIdentifier (java.util.List<hydra.ext.java.syntax.Annotation> annotations, hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((identifier));
    this.annotations = annotations;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedIdentifier)) {
      return false;
    }
    AnnotatedIdentifier o = (AnnotatedIdentifier) (other);
    return annotations.equals(o.annotations) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * identifier.hashCode();
  }
  
  public AnnotatedIdentifier withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new AnnotatedIdentifier(annotations, identifier);
  }
  
  public AnnotatedIdentifier withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new AnnotatedIdentifier(annotations, identifier);
  }
}
