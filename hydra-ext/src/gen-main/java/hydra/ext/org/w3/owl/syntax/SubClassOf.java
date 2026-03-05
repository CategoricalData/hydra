// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class SubClassOf implements Serializable, Comparable<SubClassOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.SubClassOf");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name SUB_CLASS = new hydra.core.Name("subClass");
  
  public static final hydra.core.Name SUPER_CLASS = new hydra.core.Name("superClass");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.ClassExpression subClass;
  
  public final hydra.ext.org.w3.owl.syntax.ClassExpression superClass;
  
  public SubClassOf (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ClassExpression subClass, hydra.ext.org.w3.owl.syntax.ClassExpression superClass) {
    this.annotations = annotations;
    this.subClass = subClass;
    this.superClass = superClass;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubClassOf)) {
      return false;
    }
    SubClassOf o = (SubClassOf) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.subClass,
      o.subClass) && java.util.Objects.equals(
      this.superClass,
      o.superClass);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(subClass) + 5 * java.util.Objects.hashCode(superClass);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SubClassOf other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) subClass).compareTo(other.subClass);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) superClass).compareTo(other.superClass);
  }
  
  public SubClassOf withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new SubClassOf(annotations, subClass, superClass);
  }
  
  public SubClassOf withSubClass(hydra.ext.org.w3.owl.syntax.ClassExpression subClass) {
    return new SubClassOf(annotations, subClass, superClass);
  }
  
  public SubClassOf withSuperClass(hydra.ext.org.w3.owl.syntax.ClassExpression superClass) {
    return new SubClassOf(annotations, subClass, superClass);
  }
}
