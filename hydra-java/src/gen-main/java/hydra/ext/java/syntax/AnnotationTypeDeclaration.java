// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class AnnotationTypeDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.AnnotationTypeDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.java.syntax.InterfaceModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final hydra.ext.java.syntax.AnnotationTypeBody body;
  
  public AnnotationTypeDeclaration (java.util.List<hydra.ext.java.syntax.InterfaceModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, hydra.ext.java.syntax.AnnotationTypeBody body) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((body));
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationTypeDeclaration)) {
      return false;
    }
    AnnotationTypeDeclaration o = (AnnotationTypeDeclaration) (other);
    return modifiers.equals(o.modifiers) && identifier.equals(o.identifier) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * identifier.hashCode() + 5 * body.hashCode();
  }
  
  public AnnotationTypeDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.InterfaceModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new AnnotationTypeDeclaration(modifiers, identifier, body);
  }
  
  public AnnotationTypeDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new AnnotationTypeDeclaration(modifiers, identifier, body);
  }
  
  public AnnotationTypeDeclaration withBody(hydra.ext.java.syntax.AnnotationTypeBody body) {
    java.util.Objects.requireNonNull((body));
    return new AnnotationTypeDeclaration(modifiers, identifier, body);
  }
}
