// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class AccessorsEventDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AccessorsEventDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ACCESSORS = new hydra.core.Name("accessors");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.EventModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.MemberName name;
  
  public final hydra.ext.csharp.syntax.EventAccessorDeclarations accessors;
  
  public AccessorsEventDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.EventModifier> modifiers, hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.MemberName name, hydra.ext.csharp.syntax.EventAccessorDeclarations accessors) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((accessors));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.type = type;
    this.name = name;
    this.accessors = accessors;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AccessorsEventDeclaration)) {
      return false;
    }
    AccessorsEventDeclaration o = (AccessorsEventDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && type.equals(o.type) && name.equals(o.name) && accessors.equals(o.accessors);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * type.hashCode() + 7 * name.hashCode() + 11 * accessors.hashCode();
  }
  
  public AccessorsEventDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new AccessorsEventDeclaration(attributes, modifiers, type, name, accessors);
  }
  
  public AccessorsEventDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.EventModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new AccessorsEventDeclaration(attributes, modifiers, type, name, accessors);
  }
  
  public AccessorsEventDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new AccessorsEventDeclaration(attributes, modifiers, type, name, accessors);
  }
  
  public AccessorsEventDeclaration withName(hydra.ext.csharp.syntax.MemberName name) {
    java.util.Objects.requireNonNull((name));
    return new AccessorsEventDeclaration(attributes, modifiers, type, name, accessors);
  }
  
  public AccessorsEventDeclaration withAccessors(hydra.ext.csharp.syntax.EventAccessorDeclarations accessors) {
    java.util.Objects.requireNonNull((accessors));
    return new AccessorsEventDeclaration(attributes, modifiers, type, name, accessors);
  }
}