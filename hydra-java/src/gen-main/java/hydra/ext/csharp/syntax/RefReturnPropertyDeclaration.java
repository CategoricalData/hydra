// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RefReturnPropertyDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefReturnPropertyDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_REF_KIND = new hydra.core.Name("refKind");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.PropertyModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.RefKind refKind;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.MemberName name;
  
  public final hydra.ext.csharp.syntax.RefPropertyBody body;
  
  public RefReturnPropertyDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.PropertyModifier> modifiers, hydra.ext.csharp.syntax.RefKind refKind, hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.MemberName name, hydra.ext.csharp.syntax.RefPropertyBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((refKind));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.refKind = refKind;
    this.type = type;
    this.name = name;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RefReturnPropertyDeclaration)) {
      return false;
    }
    RefReturnPropertyDeclaration o = (RefReturnPropertyDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && refKind.equals(o.refKind) && type.equals(o.type) && name.equals(o.name) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * refKind.hashCode() + 7 * type.hashCode() + 11 * name.hashCode() + 13 * body.hashCode();
  }
  
  public RefReturnPropertyDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new RefReturnPropertyDeclaration(attributes, modifiers, refKind, type, name, body);
  }
  
  public RefReturnPropertyDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.PropertyModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new RefReturnPropertyDeclaration(attributes, modifiers, refKind, type, name, body);
  }
  
  public RefReturnPropertyDeclaration withRefKind(hydra.ext.csharp.syntax.RefKind refKind) {
    java.util.Objects.requireNonNull((refKind));
    return new RefReturnPropertyDeclaration(attributes, modifiers, refKind, type, name, body);
  }
  
  public RefReturnPropertyDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new RefReturnPropertyDeclaration(attributes, modifiers, refKind, type, name, body);
  }
  
  public RefReturnPropertyDeclaration withName(hydra.ext.csharp.syntax.MemberName name) {
    java.util.Objects.requireNonNull((name));
    return new RefReturnPropertyDeclaration(attributes, modifiers, refKind, type, name, body);
  }
  
  public RefReturnPropertyDeclaration withBody(hydra.ext.csharp.syntax.RefPropertyBody body) {
    java.util.Objects.requireNonNull((body));
    return new RefReturnPropertyDeclaration(attributes, modifiers, refKind, type, name, body);
  }
}