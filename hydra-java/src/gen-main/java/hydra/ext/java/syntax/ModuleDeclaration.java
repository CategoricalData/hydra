// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModuleDeclaration implements Serializable, Comparable<ModuleDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.ModuleDeclaration");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name OPEN = new hydra.core.Name("open");
  
  public static final hydra.core.Name IDENTIFIERS = new hydra.core.Name("identifiers");
  
  public static final hydra.core.Name DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.Annotation> annotations;
  
  public final Boolean open;
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.Identifier> identifiers;
  
  public final hydra.util.ConsList<hydra.util.ConsList<hydra.ext.java.syntax.ModuleDirective>> directives;
  
  public ModuleDeclaration (hydra.util.ConsList<hydra.ext.java.syntax.Annotation> annotations, Boolean open, hydra.util.ConsList<hydra.ext.java.syntax.Identifier> identifiers, hydra.util.ConsList<hydra.util.ConsList<hydra.ext.java.syntax.ModuleDirective>> directives) {
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
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) open).compareTo(other.open);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) identifiers).compareTo(other.identifiers);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) directives).compareTo(other.directives);
  }
  
  public ModuleDeclaration withAnnotations(hydra.util.ConsList<hydra.ext.java.syntax.Annotation> annotations) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withOpen(Boolean open) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withIdentifiers(hydra.util.ConsList<hydra.ext.java.syntax.Identifier> identifiers) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
  
  public ModuleDeclaration withDirectives(hydra.util.ConsList<hydra.util.ConsList<hydra.ext.java.syntax.ModuleDirective>> directives) {
    return new ModuleDeclaration(annotations, open, identifiers, directives);
  }
}
