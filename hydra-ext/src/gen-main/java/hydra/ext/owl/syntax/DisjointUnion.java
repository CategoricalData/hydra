// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions
 */
public class DisjointUnion implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.DisjointUnion");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_CLASSES = new hydra.core.Name("classes");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.Class_ class_;
  
  public final java.util.List<hydra.ext.owl.syntax.ClassExpression> classes;
  
  public DisjointUnion (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.Class_ class_, java.util.List<hydra.ext.owl.syntax.ClassExpression> classes) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((class_));
    java.util.Objects.requireNonNull((classes));
    this.annotations = annotations;
    this.class_ = class_;
    this.classes = classes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DisjointUnion)) {
      return false;
    }
    DisjointUnion o = (DisjointUnion) (other);
    return annotations.equals(o.annotations) && class_.equals(o.class_) && classes.equals(o.classes);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * class_.hashCode() + 5 * classes.hashCode();
  }
  
  public DisjointUnion withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new DisjointUnion(annotations, class_, classes);
  }
  
  public DisjointUnion withClass(hydra.ext.owl.syntax.Class_ class_) {
    java.util.Objects.requireNonNull((class_));
    return new DisjointUnion(annotations, class_, classes);
  }
  
  public DisjointUnion withClasses(java.util.List<hydra.ext.owl.syntax.ClassExpression> classes) {
    java.util.Objects.requireNonNull((classes));
    return new DisjointUnion(annotations, class_, classes);
  }
}