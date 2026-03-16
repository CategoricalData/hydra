// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class EquivalentClasses implements Serializable, Comparable<EquivalentClasses> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.EquivalentClasses");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name CLASSES = new hydra.core.Name("classes");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> classes;
  
  public EquivalentClasses (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> classes) {
    this.annotations = annotations;
    this.classes = classes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EquivalentClasses)) {
      return false;
    }
    EquivalentClasses o = (EquivalentClasses) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.classes,
      o.classes);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(classes);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EquivalentClasses other) {
    int cmp = 0;
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) classes).compareTo(other.classes);
  }
  
  public EquivalentClasses withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new EquivalentClasses(annotations, classes);
  }
  
  public EquivalentClasses withClasses(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> classes) {
    return new EquivalentClasses(annotations, classes);
  }
}
