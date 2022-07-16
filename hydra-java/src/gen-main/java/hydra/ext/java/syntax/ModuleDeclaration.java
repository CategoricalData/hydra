package hydra.ext.java.syntax;

public class ModuleDeclaration {
  public final java.util.List<Annotation> annotations;
  
  public final Boolean open;
  
  public final java.util.List<Identifier> identifiers;
  
  public final java.util.List<java.util.List<ModuleDirective>> directives;
  
  public ModuleDeclaration (java.util.List<Annotation> annotations, Boolean open, java.util.List<Identifier> identifiers, java.util.List<java.util.List<ModuleDirective>> directives) {
    this.annotations = annotations;
    this.open = open;
    this.identifiers = identifiers;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleDeclaration)) {
      return false;
    }
    ModuleDeclaration o = (ModuleDeclaration) (other);
    return annotations.equals(o.annotations) && open.equals(o.open) && identifiers.equals(o.identifiers) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * open.hashCode() + 5 * identifiers.hashCode() + 7 * directives.hashCode();
  }
  
  public ModuleDeclaration withAnnotations(java.util.List<Annotation> annotations) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withOpen(Boolean open) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withIdentifiers(java.util.List<Identifier> identifiers) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withDirectives(java.util.List<java.util.List<ModuleDirective>> directives) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
}