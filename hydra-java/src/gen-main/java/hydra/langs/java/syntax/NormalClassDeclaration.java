// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class NormalClassDeclaration implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.NormalClassDeclaration");
  
  public final java.util.List<hydra.langs.java.syntax.ClassModifier> modifiers;
  
  public final hydra.langs.java.syntax.TypeIdentifier identifier;
  
  public final java.util.List<hydra.langs.java.syntax.TypeParameter> parameters;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.ClassType> extends_;
  
  public final java.util.List<hydra.langs.java.syntax.InterfaceType> implements_;
  
  public final hydra.langs.java.syntax.ClassBody body;
  
  public NormalClassDeclaration (java.util.List<hydra.langs.java.syntax.ClassModifier> modifiers, hydra.langs.java.syntax.TypeIdentifier identifier, java.util.List<hydra.langs.java.syntax.TypeParameter> parameters, hydra.util.Opt<hydra.langs.java.syntax.ClassType> extends_, java.util.List<hydra.langs.java.syntax.InterfaceType> implements_, hydra.langs.java.syntax.ClassBody body) {
    if (modifiers == null) {
      throw new IllegalArgumentException("null value for 'modifiers' argument");
    }
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    if (parameters == null) {
      throw new IllegalArgumentException("null value for 'parameters' argument");
    }
    if (extends_ == null) {
      throw new IllegalArgumentException("null value for 'extends' argument");
    }
    if (implements_ == null) {
      throw new IllegalArgumentException("null value for 'implements' argument");
    }
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
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
    NormalClassDeclaration o = (NormalClassDeclaration) (other);
    return modifiers.equals(o.modifiers) && identifier.equals(o.identifier) && parameters.equals(o.parameters) && extends_.equals(o.extends_) && implements_.equals(o.implements_) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * identifier.hashCode() + 5 * parameters.hashCode() + 7 * extends_.hashCode() + 11 * implements_.hashCode() + 13 * body.hashCode();
  }
  
  public NormalClassDeclaration withModifiers(java.util.List<hydra.langs.java.syntax.ClassModifier> modifiers) {
    if (modifiers == null) {
      throw new IllegalArgumentException("null value for 'modifiers' argument");
    }
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withIdentifier(hydra.langs.java.syntax.TypeIdentifier identifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withParameters(java.util.List<hydra.langs.java.syntax.TypeParameter> parameters) {
    if (parameters == null) {
      throw new IllegalArgumentException("null value for 'parameters' argument");
    }
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withExtends(hydra.util.Opt<hydra.langs.java.syntax.ClassType> extends_) {
    if (extends_ == null) {
      throw new IllegalArgumentException("null value for 'extends' argument");
    }
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withImplements(java.util.List<hydra.langs.java.syntax.InterfaceType> implements_) {
    if (implements_ == null) {
      throw new IllegalArgumentException("null value for 'implements' argument");
    }
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withBody(hydra.langs.java.syntax.ClassBody body) {
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
}