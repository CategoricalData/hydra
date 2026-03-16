// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class NormalClassDeclaration implements Serializable, Comparable<NormalClassDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.NormalClassDeclaration");
  
  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name EXTENDS = new hydra.core.Name("extends");
  
  public static final hydra.core.Name IMPLEMENTS = new hydra.core.Name("implements");
  
  public static final hydra.core.Name BODY = new hydra.core.Name("body");
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.ClassModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> parameters;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ClassType> extends_;
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> implements_;
  
  public final hydra.ext.java.syntax.ClassBody body;
  
  public NormalClassDeclaration (hydra.util.ConsList<hydra.ext.java.syntax.ClassModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> parameters, hydra.util.Maybe<hydra.ext.java.syntax.ClassType> extends_, hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> implements_, hydra.ext.java.syntax.ClassBody body) {
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
    cmp = ((Comparable) implements_).compareTo(other.implements_);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public NormalClassDeclaration withModifiers(hydra.util.ConsList<hydra.ext.java.syntax.ClassModifier> modifiers) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withParameters(hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> parameters) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withExtends(hydra.util.Maybe<hydra.ext.java.syntax.ClassType> extends_) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withImplements(hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> implements_) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withBody(hydra.ext.java.syntax.ClassBody body) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
}
