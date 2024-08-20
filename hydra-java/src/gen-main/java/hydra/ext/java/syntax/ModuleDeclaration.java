// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModuleDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.ModuleDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_OPEN = new hydra.core.Name("open");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIERS = new hydra.core.Name("identifiers");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final Boolean open;
  
  public final java.util.List<hydra.ext.java.syntax.Identifier> identifiers;
  
  public final java.util.List<java.util.List<hydra.ext.java.syntax.ModuleDirective>> directives;
  
  public ModuleDeclaration (java.util.List<hydra.ext.java.syntax.Annotation> annotations, Boolean open, java.util.List<hydra.ext.java.syntax.Identifier> identifiers, java.util.List<java.util.List<hydra.ext.java.syntax.ModuleDirective>> directives) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((open));
    java.util.Objects.requireNonNull((identifiers));
    java.util.Objects.requireNonNull((directives));
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
  
  public ModuleDeclaration withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withOpen(Boolean open) {
    java.util.Objects.requireNonNull((open));
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withIdentifiers(java.util.List<hydra.ext.java.syntax.Identifier> identifiers) {
    java.util.Objects.requireNonNull((identifiers));
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withDirectives(java.util.List<java.util.List<hydra.ext.java.syntax.ModuleDirective>> directives) {
    java.util.Objects.requireNonNull((directives));
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
}
