// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class NormalClassDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.NormalClassDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_EXTENDS = new hydra.core.Name("extends");
  
  public static final hydra.core.Name FIELD_NAME_IMPLEMENTS = new hydra.core.Name("implements");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.langs.java.syntax.ClassModifier> modifiers;
  
  public final hydra.langs.java.syntax.TypeIdentifier identifier;
  
  public final java.util.List<hydra.langs.java.syntax.TypeParameter> parameters;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.ClassType> extends_;
  
  public final java.util.List<hydra.langs.java.syntax.InterfaceType> implements_;
  
  public final hydra.langs.java.syntax.ClassBody body;
  
  public NormalClassDeclaration (java.util.List<hydra.langs.java.syntax.ClassModifier> modifiers, hydra.langs.java.syntax.TypeIdentifier identifier, java.util.List<hydra.langs.java.syntax.TypeParameter> parameters, hydra.util.Opt<hydra.langs.java.syntax.ClassType> extends_, java.util.List<hydra.langs.java.syntax.InterfaceType> implements_, hydra.langs.java.syntax.ClassBody body) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((extends_));
    java.util.Objects.requireNonNull((implements_));
    java.util.Objects.requireNonNull((body));
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
    java.util.Objects.requireNonNull((modifiers));
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withIdentifier(hydra.langs.java.syntax.TypeIdentifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withParameters(java.util.List<hydra.langs.java.syntax.TypeParameter> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withExtends(hydra.util.Opt<hydra.langs.java.syntax.ClassType> extends_) {
    java.util.Objects.requireNonNull((extends_));
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withImplements(java.util.List<hydra.langs.java.syntax.InterfaceType> implements_) {
    java.util.Objects.requireNonNull((implements_));
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withBody(hydra.langs.java.syntax.ClassBody body) {
    java.util.Objects.requireNonNull((body));
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
}