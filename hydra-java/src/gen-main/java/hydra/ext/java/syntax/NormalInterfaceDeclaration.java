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
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.InterfaceModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> parameters;
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> extends_;
  
  public final hydra.ext.java.syntax.InterfaceBody body;
  
  public NormalInterfaceDeclaration (hydra.util.ConsList<hydra.ext.java.syntax.InterfaceModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> parameters, hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> extends_, hydra.ext.java.syntax.InterfaceBody body) {
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
    cmp = ((Comparable) modifiers).compareTo(other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) parameters).compareTo(other.parameters);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) extends_).compareTo(other.extends_);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public NormalInterfaceDeclaration withModifiers(hydra.util.ConsList<hydra.ext.java.syntax.InterfaceModifier> modifiers) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
  
  public NormalInterfaceDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
  
  public NormalInterfaceDeclaration withParameters(hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> parameters) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
  
  public NormalInterfaceDeclaration withExtends(hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> extends_) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
  
  public NormalInterfaceDeclaration withBody(hydra.ext.java.syntax.InterfaceBody body) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
}
