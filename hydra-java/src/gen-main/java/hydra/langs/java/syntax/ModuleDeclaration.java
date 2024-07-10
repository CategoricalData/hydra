// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ModuleDeclaration implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ModuleDeclaration");
  
  public final java.util.List<hydra.langs.java.syntax.Annotation> annotations;
  
  public final Boolean open;
  
  public final java.util.List<hydra.langs.java.syntax.Identifier> identifiers;
  
  public final java.util.List<java.util.List<hydra.langs.java.syntax.ModuleDirective>> directives;
  
  public ModuleDeclaration (java.util.List<hydra.langs.java.syntax.Annotation> annotations, Boolean open, java.util.List<hydra.langs.java.syntax.Identifier> identifiers, java.util.List<java.util.List<hydra.langs.java.syntax.ModuleDirective>> directives) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (open == null) {
      throw new IllegalArgumentException("null value for 'open' argument");
    }
    if (identifiers == null) {
      throw new IllegalArgumentException("null value for 'identifiers' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
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
  
  public ModuleDeclaration withAnnotations(java.util.List<hydra.langs.java.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withOpen(Boolean open) {
    if (open == null) {
      throw new IllegalArgumentException("null value for 'open' argument");
    }
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withIdentifiers(java.util.List<hydra.langs.java.syntax.Identifier> identifiers) {
    if (identifiers == null) {
      throw new IllegalArgumentException("null value for 'identifiers' argument");
    }
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withDirectives(java.util.List<java.util.List<hydra.langs.java.syntax.ModuleDirective>> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
}