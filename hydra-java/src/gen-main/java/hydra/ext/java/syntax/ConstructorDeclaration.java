// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ConstructorDeclaration implements Serializable, Comparable<ConstructorDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ConstructorDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRUCTOR = new hydra.core.Name("constructor");
  
  public static final hydra.core.Name FIELD_NAME_THROWS = new hydra.core.Name("throws");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.java.syntax.ConstructorModifier> modifiers;
  
  public final hydra.ext.java.syntax.ConstructorDeclarator constructor;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Throws> throws_;
  
  public final hydra.ext.java.syntax.ConstructorBody body;
  
  public ConstructorDeclaration (java.util.List<hydra.ext.java.syntax.ConstructorModifier> modifiers, hydra.ext.java.syntax.ConstructorDeclarator constructor, hydra.util.Maybe<hydra.ext.java.syntax.Throws> throws_, hydra.ext.java.syntax.ConstructorBody body) {
    this.modifiers = modifiers;
    this.constructor = constructor;
    this.throws_ = throws_;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorDeclaration)) {
      return false;
    }
    ConstructorDeclaration o = (ConstructorDeclaration) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.constructor,
      o.constructor) && java.util.Objects.equals(
      this.throws_,
      o.throws_) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(constructor) + 5 * java.util.Objects.hashCode(throws_) + 7 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstructorDeclaration other) {
    int cmp = 0;
    cmp = Integer.compare(
      modifiers.hashCode(),
      other.modifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) constructor).compareTo(other.constructor);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      throws_.hashCode(),
      other.throws_.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public ConstructorDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.ConstructorModifier> modifiers) {
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
  
  public ConstructorDeclaration withConstructor(hydra.ext.java.syntax.ConstructorDeclarator constructor) {
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
  
  public ConstructorDeclaration withThrows(hydra.util.Maybe<hydra.ext.java.syntax.Throws> throws_) {
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
  
  public ConstructorDeclaration withBody(hydra.ext.java.syntax.ConstructorBody body) {
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
}
