package hydra.ext.owl.syntax;

public class Declaration {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.Declaration");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.Entity entity;
  
  public Declaration (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.Entity entity) {
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
    return new Declaration(annotations, entity);
  }
  
  public Declaration withEntity(hydra.ext.owl.syntax.Entity entity) {
    return new Declaration(annotations, entity);
  }
}