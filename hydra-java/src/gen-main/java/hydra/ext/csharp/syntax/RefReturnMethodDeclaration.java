// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RefReturnMethodDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefReturnMethodDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_KIND = new hydra.core.Name("kind");
  
  public static final hydra.core.Name FIELD_NAME_RETURN_TYPE = new hydra.core.Name("returnType");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.RefMethodModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.RefKind kind;
  
  public final hydra.ext.csharp.syntax.ReturnType returnType;
  
  public final hydra.ext.csharp.syntax.MethodHeader header;
  
  public final hydra.ext.csharp.syntax.RefMethodBody body;
  
  public RefReturnMethodDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.RefMethodModifier> modifiers, hydra.ext.csharp.syntax.RefKind kind, hydra.ext.csharp.syntax.ReturnType returnType, hydra.ext.csharp.syntax.MethodHeader header, hydra.ext.csharp.syntax.RefMethodBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((kind));
    java.util.Objects.requireNonNull((returnType));
    java.util.Objects.requireNonNull((header));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.kind = kind;
    this.returnType = returnType;
    this.header = header;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RefReturnMethodDeclaration)) {
      return false;
    }
    RefReturnMethodDeclaration o = (RefReturnMethodDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && kind.equals(o.kind) && returnType.equals(o.returnType) && header.equals(o.header) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * kind.hashCode() + 7 * returnType.hashCode() + 11 * header.hashCode() + 13 * body.hashCode();
  }
  
  public RefReturnMethodDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new RefReturnMethodDeclaration(attributes, modifiers, kind, returnType, header, body);
  }
  
  public RefReturnMethodDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.RefMethodModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new RefReturnMethodDeclaration(attributes, modifiers, kind, returnType, header, body);
  }
  
  public RefReturnMethodDeclaration withKind(hydra.ext.csharp.syntax.RefKind kind) {
    java.util.Objects.requireNonNull((kind));
    return new RefReturnMethodDeclaration(attributes, modifiers, kind, returnType, header, body);
  }
  
  public RefReturnMethodDeclaration withReturnType(hydra.ext.csharp.syntax.ReturnType returnType) {
    java.util.Objects.requireNonNull((returnType));
    return new RefReturnMethodDeclaration(attributes, modifiers, kind, returnType, header, body);
  }
  
  public RefReturnMethodDeclaration withHeader(hydra.ext.csharp.syntax.MethodHeader header) {
    java.util.Objects.requireNonNull((header));
    return new RefReturnMethodDeclaration(attributes, modifiers, kind, returnType, header, body);
  }
  
  public RefReturnMethodDeclaration withBody(hydra.ext.csharp.syntax.RefMethodBody body) {
    java.util.Objects.requireNonNull((body));
    return new RefReturnMethodDeclaration(attributes, modifiers, kind, returnType, header, body);
  }
}