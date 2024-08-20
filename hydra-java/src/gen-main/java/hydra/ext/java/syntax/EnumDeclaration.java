// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnumDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.EnumDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTS = new hydra.core.Name("implements");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final java.util.List<hydra.ext.java.syntax.InterfaceType> implements_;
  
  public final hydra.ext.java.syntax.EnumBody body;
  
  public EnumDeclaration (java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, java.util.List<hydra.ext.java.syntax.InterfaceType> implements_, hydra.ext.java.syntax.EnumBody body) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((implements_));
    java.util.Objects.requireNonNull((body));
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.implements_ = implements_;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumDeclaration)) {
      return false;
    }
    EnumDeclaration o = (EnumDeclaration) (other);
    return modifiers.equals(o.modifiers) && identifier.equals(o.identifier) && implements_.equals(o.implements_) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * identifier.hashCode() + 5 * implements_.hashCode() + 7 * body.hashCode();
  }
  
  public EnumDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withImplements(java.util.List<hydra.ext.java.syntax.InterfaceType> implements_) {
    java.util.Objects.requireNonNull((implements_));
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withBody(hydra.ext.java.syntax.EnumBody body) {
    java.util.Objects.requireNonNull((body));
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
}
