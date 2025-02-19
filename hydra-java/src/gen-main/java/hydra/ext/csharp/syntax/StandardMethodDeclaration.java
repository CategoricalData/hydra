// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class StandardMethodDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StandardMethodDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_RETURN_TYPE = new hydra.core.Name("returnType");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.MethodModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.ReturnType returnType;
  
  public final hydra.ext.csharp.syntax.MethodHeader header;
  
  public final hydra.ext.csharp.syntax.MethodBody body;
  
  public StandardMethodDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.MethodModifier> modifiers, hydra.ext.csharp.syntax.ReturnType returnType, hydra.ext.csharp.syntax.MethodHeader header, hydra.ext.csharp.syntax.MethodBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((returnType));
    java.util.Objects.requireNonNull((header));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.returnType = returnType;
    this.header = header;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StandardMethodDeclaration)) {
      return false;
    }
    StandardMethodDeclaration o = (StandardMethodDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && returnType.equals(o.returnType) && header.equals(o.header) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * returnType.hashCode() + 7 * header.hashCode() + 11 * body.hashCode();
  }
  
  public StandardMethodDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new StandardMethodDeclaration(attributes, modifiers, returnType, header, body);
  }
  
  public StandardMethodDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.MethodModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new StandardMethodDeclaration(attributes, modifiers, returnType, header, body);
  }
  
  public StandardMethodDeclaration withReturnType(hydra.ext.csharp.syntax.ReturnType returnType) {
    java.util.Objects.requireNonNull((returnType));
    return new StandardMethodDeclaration(attributes, modifiers, returnType, header, body);
  }
  
  public StandardMethodDeclaration withHeader(hydra.ext.csharp.syntax.MethodHeader header) {
    java.util.Objects.requireNonNull((header));
    return new StandardMethodDeclaration(attributes, modifiers, returnType, header, body);
  }
  
  public StandardMethodDeclaration withBody(hydra.ext.csharp.syntax.MethodBody body) {
    java.util.Objects.requireNonNull((body));
    return new StandardMethodDeclaration(attributes, modifiers, returnType, header, body);
  }
}