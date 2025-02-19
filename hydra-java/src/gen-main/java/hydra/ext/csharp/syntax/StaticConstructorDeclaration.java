// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class StaticConstructorDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StaticConstructorDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final hydra.ext.csharp.syntax.StaticConstructorModifiers modifiers;
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.ext.csharp.syntax.StaticConstructorBody body;
  
  public StaticConstructorDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, hydra.ext.csharp.syntax.StaticConstructorModifiers modifiers, hydra.ext.csharp.syntax.Identifier name, hydra.ext.csharp.syntax.StaticConstructorBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.name = name;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StaticConstructorDeclaration)) {
      return false;
    }
    StaticConstructorDeclaration o = (StaticConstructorDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && name.equals(o.name) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * name.hashCode() + 7 * body.hashCode();
  }
  
  public StaticConstructorDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new StaticConstructorDeclaration(attributes, modifiers, name, body);
  }
  
  public StaticConstructorDeclaration withModifiers(hydra.ext.csharp.syntax.StaticConstructorModifiers modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new StaticConstructorDeclaration(attributes, modifiers, name, body);
  }
  
  public StaticConstructorDeclaration withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new StaticConstructorDeclaration(attributes, modifiers, name, body);
  }
  
  public StaticConstructorDeclaration withBody(hydra.ext.csharp.syntax.StaticConstructorBody body) {
    java.util.Objects.requireNonNull((body));
    return new StaticConstructorDeclaration(attributes, modifiers, name, body);
  }
}