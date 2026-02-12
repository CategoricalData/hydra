// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class NormalClassDeclaration implements Serializable, Comparable<NormalClassDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.NormalClassDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_EXTENDS = new hydra.core.Name("extends");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTS = new hydra.core.Name("implements");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final java.util.List<hydra.ext.java.syntax.TypeParameter> parameters;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ClassType> extends_;
  
  public final java.util.List<hydra.ext.java.syntax.InterfaceType> implements_;
  
  public final hydra.ext.java.syntax.ClassBody body;
  
  public NormalClassDeclaration (java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, java.util.List<hydra.ext.java.syntax.TypeParameter> parameters, hydra.util.Maybe<hydra.ext.java.syntax.ClassType> extends_, java.util.List<hydra.ext.java.syntax.InterfaceType> implements_, hydra.ext.java.syntax.ClassBody body) {
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.parameters = parameters;
    this.extends_ = extends_;
    this.implements_ = implements_;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalClassDeclaration)) {
      return false;
    }
    NormalClassDeclaration o = (NormalClassDeclaration) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.parameters,
      o.parameters) && java.util.Objects.equals(
      this.extends_,
      o.extends_) && java.util.Objects.equals(
      this.implements_,
      o.implements_) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(identifier) + 5 * java.util.Objects.hashCode(parameters) + 7 * java.util.Objects.hashCode(extends_) + 11 * java.util.Objects.hashCode(implements_) + 13 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NormalClassDeclaration other) {
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
      parameters.hashCode(),
      other.parameters.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      extends_.hashCode(),
      other.extends_.hashCode());
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
  
  public NormalClassDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withParameters(java.util.List<hydra.ext.java.syntax.TypeParameter> parameters) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withExtends(hydra.util.Maybe<hydra.ext.java.syntax.ClassType> extends_) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withImplements(java.util.List<hydra.ext.java.syntax.InterfaceType> implements_) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withBody(hydra.ext.java.syntax.ClassBody body) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
}
