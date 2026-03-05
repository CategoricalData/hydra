// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DisjointClasses implements Serializable, Comparable<DisjointClasses> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DisjointClasses");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name CLASSES = new hydra.core.Name("classes");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.ClassExpression> classes;
  
  public DisjointClasses (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, java.util.List<hydra.ext.org.w3.owl.syntax.ClassExpression> classes) {
    this.annotations = annotations;
    this.classes = classes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DisjointClasses)) {
      return false;
    }
    DisjointClasses o = (DisjointClasses) other;
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
  public int compareTo(DisjointClasses other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      classes.hashCode(),
      other.classes.hashCode());
  }
  
  public DisjointClasses withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new DisjointClasses(annotations, classes);
  }
  
  public DisjointClasses withClasses(java.util.List<hydra.ext.org.w3.owl.syntax.ClassExpression> classes) {
    return new DisjointClasses(annotations, classes);
  }
}
