// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModuleDeclaration implements Serializable, Comparable<ModuleDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ModuleDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_OPEN = new hydra.core.Name("open");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIERS = new hydra.core.Name("identifiers");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final Boolean open;
  
  public final java.util.List<hydra.ext.java.syntax.Identifier> identifiers;
  
  public final java.util.List<java.util.List<hydra.ext.java.syntax.ModuleDirective>> directives;
  
  public ModuleDeclaration (java.util.List<hydra.ext.java.syntax.Annotation> annotations, Boolean open, java.util.List<hydra.ext.java.syntax.Identifier> identifiers, java.util.List<java.util.List<hydra.ext.java.syntax.ModuleDirective>> directives) {
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
    ModuleDeclaration o = (ModuleDeclaration) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.open,
      o.open) && java.util.Objects.equals(
      this.identifiers,
      o.identifiers) && java.util.Objects.equals(
      this.directives,
      o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(open) + 5 * java.util.Objects.hashCode(identifiers) + 7 * java.util.Objects.hashCode(directives);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModuleDeclaration other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) open).compareTo(other.open);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      identifiers.hashCode(),
      other.identifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      directives.hashCode(),
      other.directives.hashCode());
  }
  
  public ModuleDeclaration withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withOpen(Boolean open) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withIdentifiers(java.util.List<hydra.ext.java.syntax.Identifier> identifiers) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withDirectives(java.util.List<java.util.List<hydra.ext.java.syntax.ModuleDirective>> directives) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
}
