// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ConstructorDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ConstructorDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRUCTOR = new hydra.core.Name("constructor");
  
  public static final hydra.core.Name FIELD_NAME_THROWS = new hydra.core.Name("throws");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.java.syntax.ConstructorModifier> modifiers;
  
  public final hydra.ext.java.syntax.ConstructorDeclarator constructor;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Throws> throws_;
  
  public final hydra.ext.java.syntax.ConstructorBody body;
  
  public ConstructorDeclaration (java.util.List<hydra.ext.java.syntax.ConstructorModifier> modifiers, hydra.ext.java.syntax.ConstructorDeclarator constructor, hydra.util.Opt<hydra.ext.java.syntax.Throws> throws_, hydra.ext.java.syntax.ConstructorBody body) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((constructor));
    java.util.Objects.requireNonNull((throws_));
    java.util.Objects.requireNonNull((body));
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
    ConstructorDeclaration o = (ConstructorDeclaration) (other);
    return modifiers.equals(o.modifiers) && constructor.equals(o.constructor) && throws_.equals(o.throws_) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * constructor.hashCode() + 5 * throws_.hashCode() + 7 * body.hashCode();
  }
  
  public ConstructorDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.ConstructorModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
  
  public ConstructorDeclaration withConstructor(hydra.ext.java.syntax.ConstructorDeclarator constructor) {
    java.util.Objects.requireNonNull((constructor));
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
  
  public ConstructorDeclaration withThrows(hydra.util.Opt<hydra.ext.java.syntax.Throws> throws_) {
    java.util.Objects.requireNonNull((throws_));
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
  
  public ConstructorDeclaration withBody(hydra.ext.java.syntax.ConstructorBody body) {
    java.util.Objects.requireNonNull((body));
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
}