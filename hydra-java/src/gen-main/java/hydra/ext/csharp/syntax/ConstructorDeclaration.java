// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ConstructorDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ConstructorDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATOR = new hydra.core.Name("declarator");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.ConstructorModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.ConstructorDeclarator declarator;
  
  public final hydra.ext.csharp.syntax.ConstructorBody body;
  
  public ConstructorDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.ConstructorModifier> modifiers, hydra.ext.csharp.syntax.ConstructorDeclarator declarator, hydra.ext.csharp.syntax.ConstructorBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((declarator));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.declarator = declarator;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorDeclaration)) {
      return false;
    }
    ConstructorDeclaration o = (ConstructorDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && declarator.equals(o.declarator) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * declarator.hashCode() + 7 * body.hashCode();
  }
  
  public ConstructorDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new ConstructorDeclaration(attributes, modifiers, declarator, body);
  }
  
  public ConstructorDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.ConstructorModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new ConstructorDeclaration(attributes, modifiers, declarator, body);
  }
  
  public ConstructorDeclaration withDeclarator(hydra.ext.csharp.syntax.ConstructorDeclarator declarator) {
    java.util.Objects.requireNonNull((declarator));
    return new ConstructorDeclaration(attributes, modifiers, declarator, body);
  }
  
  public ConstructorDeclaration withBody(hydra.ext.csharp.syntax.ConstructorBody body) {
    java.util.Objects.requireNonNull((body));
    return new ConstructorDeclaration(attributes, modifiers, declarator, body);
  }
}