// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class Declaration implements Serializable, Comparable<Declaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.Declaration");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name ENTITY = new hydra.core.Name("entity");

  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.Entity entity;

  public Declaration (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.Entity entity) {
    this.annotations = annotations;
    this.entity = entity;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Declaration)) {
      return false;
    }
    Declaration o = (Declaration) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.entity,
      o.entity);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(entity);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Declaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      entity,
      other.entity);
  }

  public Declaration withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new Declaration(annotations, entity);
  }

  public Declaration withEntity(hydra.ext.org.w3.owl.syntax.Entity entity) {
    return new Declaration(annotations, entity);
  }
}
