// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class SubClassOf implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.SubClassOf");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_SUB_CLASS = new hydra.core.Name("subClass");
  
  public static final hydra.core.Name FIELD_NAME_SUPER_CLASS = new hydra.core.Name("superClass");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.ClassExpression subClass;
  
  public final hydra.ext.owl.syntax.ClassExpression superClass;
  
  public SubClassOf (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.ClassExpression subClass, hydra.ext.owl.syntax.ClassExpression superClass) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((subClass));
    java.util.Objects.requireNonNull((superClass));
    this.annotations = annotations;
    this.subClass = subClass;
    this.superClass = superClass;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubClassOf)) {
      return false;
    }
    SubClassOf o = (SubClassOf) (other);
    return annotations.equals(o.annotations) && subClass.equals(o.subClass) && superClass.equals(o.superClass);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * subClass.hashCode() + 5 * superClass.hashCode();
  }
  
  public SubClassOf withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new SubClassOf(annotations, subClass, superClass);
  }
  
  public SubClassOf withSubClass(hydra.ext.owl.syntax.ClassExpression subClass) {
    java.util.Objects.requireNonNull((subClass));
    return new SubClassOf(annotations, subClass, superClass);
  }
  
  public SubClassOf withSuperClass(hydra.ext.owl.syntax.ClassExpression superClass) {
    java.util.Objects.requireNonNull((superClass));
    return new SubClassOf(annotations, subClass, superClass);
  }
}
