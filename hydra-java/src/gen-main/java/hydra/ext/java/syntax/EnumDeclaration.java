// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnumDeclaration implements Serializable, Comparable<EnumDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.EnumDeclaration");
  
  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name IMPLEMENTS = new hydra.core.Name("implements");
  
  public static final hydra.core.Name BODY = new hydra.core.Name("body");
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.ClassModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> implements_;
  
  public final hydra.ext.java.syntax.EnumBody body;
  
  public EnumDeclaration (hydra.util.ConsList<hydra.ext.java.syntax.ClassModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> implements_, hydra.ext.java.syntax.EnumBody body) {
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
    EnumDeclaration o = (EnumDeclaration) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.implements_,
      o.implements_) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(identifier) + 5 * java.util.Objects.hashCode(implements_) + 7 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EnumDeclaration other) {
    int cmp = 0;
    cmp = Integer.compare(
      modifiers.hashCode(),
      other.modifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      implements_.hashCode(),
      other.implements_.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public EnumDeclaration withModifiers(hydra.util.ConsList<hydra.ext.java.syntax.ClassModifier> modifiers) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withImplements(hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> implements_) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withBody(hydra.ext.java.syntax.EnumBody body) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
}
