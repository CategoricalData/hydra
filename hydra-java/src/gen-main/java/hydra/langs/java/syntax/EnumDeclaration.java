// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class EnumDeclaration implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.EnumDeclaration");
  
  public final java.util.List<hydra.langs.java.syntax.ClassModifier> modifiers;
  
  public final hydra.langs.java.syntax.TypeIdentifier identifier;
  
  public final java.util.List<hydra.langs.java.syntax.InterfaceType> implements_;
  
  public final hydra.langs.java.syntax.EnumBody body;
  
  public EnumDeclaration (java.util.List<hydra.langs.java.syntax.ClassModifier> modifiers, hydra.langs.java.syntax.TypeIdentifier identifier, java.util.List<hydra.langs.java.syntax.InterfaceType> implements_, hydra.langs.java.syntax.EnumBody body) {
    if (modifiers == null) {
      throw new IllegalArgumentException("null value for 'modifiers' argument");
    }
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    if (implements_ == null) {
      throw new IllegalArgumentException("null value for 'implements' argument");
    }
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
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
  
  public EnumDeclaration withModifiers(java.util.List<hydra.langs.java.syntax.ClassModifier> modifiers) {
    if (modifiers == null) {
      throw new IllegalArgumentException("null value for 'modifiers' argument");
    }
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withIdentifier(hydra.langs.java.syntax.TypeIdentifier identifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withImplements(java.util.List<hydra.langs.java.syntax.InterfaceType> implements_) {
    if (implements_ == null) {
      throw new IllegalArgumentException("null value for 'implements' argument");
    }
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withBody(hydra.langs.java.syntax.EnumBody body) {
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
}