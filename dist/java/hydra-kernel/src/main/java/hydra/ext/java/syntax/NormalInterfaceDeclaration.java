// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class NormalInterfaceDeclaration implements Serializable, Comparable<NormalInterfaceDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.NormalInterfaceDeclaration");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public static final hydra.core.Name PARAMETERS = new hydra.core.Name("parameters");

  public static final hydra.core.Name EXTENDS = new hydra.core.Name("extends");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final java.util.List<hydra.ext.java.syntax.InterfaceModifier> modifiers;

  public final hydra.ext.java.syntax.TypeIdentifier identifier;

  public final java.util.List<hydra.ext.java.syntax.TypeParameter> parameters;

  public final java.util.List<hydra.ext.java.syntax.InterfaceType> extends_;

  public final hydra.ext.java.syntax.InterfaceBody body;

  public NormalInterfaceDeclaration (java.util.List<hydra.ext.java.syntax.InterfaceModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, java.util.List<hydra.ext.java.syntax.TypeParameter> parameters, java.util.List<hydra.ext.java.syntax.InterfaceType> extends_, hydra.ext.java.syntax.InterfaceBody body) {
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.parameters = parameters;
    this.extends_ = extends_;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalInterfaceDeclaration)) {
      return false;
    }
    NormalInterfaceDeclaration o = (NormalInterfaceDeclaration) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.parameters,
      o.parameters) && java.util.Objects.equals(
      this.extends_,
      o.extends_) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(identifier) + 5 * java.util.Objects.hashCode(parameters) + 7 * java.util.Objects.hashCode(extends_) + 11 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NormalInterfaceDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      modifiers,
      other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      identifier,
      other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      parameters,
      other.parameters);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      extends_,
      other.extends_);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public NormalInterfaceDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.InterfaceModifier> modifiers) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }

  public NormalInterfaceDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }

  public NormalInterfaceDeclaration withParameters(java.util.List<hydra.ext.java.syntax.TypeParameter> parameters) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }

  public NormalInterfaceDeclaration withExtends(java.util.List<hydra.ext.java.syntax.InterfaceType> extends_) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }

  public NormalInterfaceDeclaration withBody(hydra.ext.java.syntax.InterfaceBody body) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
}
