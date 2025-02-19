// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class EnumDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.EnumDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_BASE = new hydra.core.Name("base");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.EnumModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.EnumBase> base;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.EnumBody> body;
  
  public EnumDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.EnumModifier> modifiers, hydra.ext.csharp.syntax.Identifier name, hydra.util.Opt<hydra.ext.csharp.syntax.EnumBase> base, hydra.util.Opt<hydra.ext.csharp.syntax.EnumBody> body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((base));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.name = name;
    this.base = base;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumDeclaration)) {
      return false;
    }
    EnumDeclaration o = (EnumDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && name.equals(o.name) && base.equals(o.base) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * name.hashCode() + 7 * base.hashCode() + 11 * body.hashCode();
  }
  
  public EnumDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new EnumDeclaration(attributes, modifiers, name, base, body);
  }
  
  public EnumDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.EnumModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new EnumDeclaration(attributes, modifiers, name, base, body);
  }
  
  public EnumDeclaration withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new EnumDeclaration(attributes, modifiers, name, base, body);
  }
  
  public EnumDeclaration withBase(hydra.util.Opt<hydra.ext.csharp.syntax.EnumBase> base) {
    java.util.Objects.requireNonNull((base));
    return new EnumDeclaration(attributes, modifiers, name, base, body);
  }
  
  public EnumDeclaration withBody(hydra.util.Opt<hydra.ext.csharp.syntax.EnumBody> body) {
    java.util.Objects.requireNonNull((body));
    return new EnumDeclaration(attributes, modifiers, name, base, body);
  }
}