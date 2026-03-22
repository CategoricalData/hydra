// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions
 */
public class DisjointUnion implements Serializable, Comparable<DisjointUnion> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DisjointUnion");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name CLASS = new hydra.core.Name("class");

  public static final hydra.core.Name CLASSES = new hydra.core.Name("classes");

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.Class_ class_;

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> classes;

  public DisjointUnion (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.Class_ class_, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> classes) {
    this.annotations = annotations;
    this.class_ = class_;
    this.classes = classes;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DisjointUnion)) {
      return false;
    }
    DisjointUnion o = (DisjointUnion) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.class_,
      o.class_) && java.util.Objects.equals(
      this.classes,
      o.classes);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(class_) + 5 * java.util.Objects.hashCode(classes);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DisjointUnion other) {
    int cmp = 0;
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) class_).compareTo(other.class_);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) classes).compareTo(other.classes);
  }

  public DisjointUnion withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new DisjointUnion(annotations, class_, classes);
  }

  public DisjointUnion withClass(hydra.ext.org.w3.owl.syntax.Class_ class_) {
    return new DisjointUnion(annotations, class_, classes);
  }

  public DisjointUnion withClasses(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> classes) {
    return new DisjointUnion(annotations, class_, classes);
  }
}
