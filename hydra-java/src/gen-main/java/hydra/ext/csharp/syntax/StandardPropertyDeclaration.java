// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class StandardPropertyDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StandardPropertyDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.PropertyModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.MemberName name;
  
  public final hydra.ext.csharp.syntax.PropertyBody body;
  
  public StandardPropertyDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.PropertyModifier> modifiers, hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.MemberName name, hydra.ext.csharp.syntax.PropertyBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.type = type;
    this.name = name;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StandardPropertyDeclaration)) {
      return false;
    }
    StandardPropertyDeclaration o = (StandardPropertyDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && type.equals(o.type) && name.equals(o.name) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * type.hashCode() + 7 * name.hashCode() + 11 * body.hashCode();
  }
  
  public StandardPropertyDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new StandardPropertyDeclaration(attributes, modifiers, type, name, body);
  }
  
  public StandardPropertyDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.PropertyModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new StandardPropertyDeclaration(attributes, modifiers, type, name, body);
  }
  
  public StandardPropertyDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new StandardPropertyDeclaration(attributes, modifiers, type, name, body);
  }
  
  public StandardPropertyDeclaration withName(hydra.ext.csharp.syntax.MemberName name) {
    java.util.Objects.requireNonNull((name));
    return new StandardPropertyDeclaration(attributes, modifiers, type, name, body);
  }
  
  public StandardPropertyDeclaration withBody(hydra.ext.csharp.syntax.PropertyBody body) {
    java.util.Objects.requireNonNull((body));
    return new StandardPropertyDeclaration(attributes, modifiers, type, name, body);
  }
}