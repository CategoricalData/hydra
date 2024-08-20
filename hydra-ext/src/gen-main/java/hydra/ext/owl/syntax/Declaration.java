// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class Declaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.Declaration");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_ENTITY = new hydra.core.Name("entity");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.Entity entity;
  
  public Declaration (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.Entity entity) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((entity));
    this.annotations = annotations;
    this.entity = entity;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Declaration)) {
      return false;
    }
    Declaration o = (Declaration) (other);
    return annotations.equals(o.annotations) && entity.equals(o.entity);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * entity.hashCode();
  }
  
  public Declaration withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new Declaration(annotations, entity);
  }
  
  public Declaration withEntity(hydra.ext.owl.syntax.Entity entity) {
    java.util.Objects.requireNonNull((entity));
    return new Declaration(annotations, entity);
  }
}