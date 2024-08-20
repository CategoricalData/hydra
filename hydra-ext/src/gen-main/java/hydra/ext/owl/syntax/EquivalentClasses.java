// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class EquivalentClasses implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.EquivalentClasses");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_CLASSES = new hydra.core.Name("classes");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.ext.owl.syntax.ClassExpression> classes;
  
  public EquivalentClasses (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, java.util.List<hydra.ext.owl.syntax.ClassExpression> classes) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((classes));
    this.annotations = annotations;
    this.classes = classes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EquivalentClasses)) {
      return false;
    }
    EquivalentClasses o = (EquivalentClasses) (other);
    return annotations.equals(o.annotations) && classes.equals(o.classes);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * classes.hashCode();
  }
  
  public EquivalentClasses withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new EquivalentClasses(annotations, classes);
  }
  
  public EquivalentClasses withClasses(java.util.List<hydra.ext.owl.syntax.ClassExpression> classes) {
    java.util.Objects.requireNonNull((classes));
    return new EquivalentClasses(annotations, classes);
  }
}