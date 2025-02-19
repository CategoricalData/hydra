// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RefLocalFunctionDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefLocalFunctionDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_REF_KIND = new hydra.core.Name("refKind");
  
  public static final hydra.core.Name FIELD_NAME_RETURN_TYPE = new hydra.core.Name("returnType");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.csharp.syntax.RefLocalFunctionModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.RefKind refKind;
  
  public final hydra.ext.csharp.syntax.Type returnType;
  
  public final hydra.ext.csharp.syntax.LocalFunctionHeader header;
  
  public final hydra.ext.csharp.syntax.RefLocalFunctionBody body;
  
  public RefLocalFunctionDeclaration (java.util.List<hydra.ext.csharp.syntax.RefLocalFunctionModifier> modifiers, hydra.ext.csharp.syntax.RefKind refKind, hydra.ext.csharp.syntax.Type returnType, hydra.ext.csharp.syntax.LocalFunctionHeader header, hydra.ext.csharp.syntax.RefLocalFunctionBody body) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((refKind));
    java.util.Objects.requireNonNull((returnType));
    java.util.Objects.requireNonNull((header));
    java.util.Objects.requireNonNull((body));
    this.modifiers = modifiers;
    this.refKind = refKind;
    this.returnType = returnType;
    this.header = header;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RefLocalFunctionDeclaration)) {
      return false;
    }
    RefLocalFunctionDeclaration o = (RefLocalFunctionDeclaration) (other);
    return modifiers.equals(o.modifiers) && refKind.equals(o.refKind) && returnType.equals(o.returnType) && header.equals(o.header) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * refKind.hashCode() + 5 * returnType.hashCode() + 7 * header.hashCode() + 11 * body.hashCode();
  }
  
  public RefLocalFunctionDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.RefLocalFunctionModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new RefLocalFunctionDeclaration(modifiers, refKind, returnType, header, body);
  }
  
  public RefLocalFunctionDeclaration withRefKind(hydra.ext.csharp.syntax.RefKind refKind) {
    java.util.Objects.requireNonNull((refKind));
    return new RefLocalFunctionDeclaration(modifiers, refKind, returnType, header, body);
  }
  
  public RefLocalFunctionDeclaration withReturnType(hydra.ext.csharp.syntax.Type returnType) {
    java.util.Objects.requireNonNull((returnType));
    return new RefLocalFunctionDeclaration(modifiers, refKind, returnType, header, body);
  }
  
  public RefLocalFunctionDeclaration withHeader(hydra.ext.csharp.syntax.LocalFunctionHeader header) {
    java.util.Objects.requireNonNull((header));
    return new RefLocalFunctionDeclaration(modifiers, refKind, returnType, header, body);
  }
  
  public RefLocalFunctionDeclaration withBody(hydra.ext.csharp.syntax.RefLocalFunctionBody body) {
    java.util.Objects.requireNonNull((body));
    return new RefLocalFunctionDeclaration(modifiers, refKind, returnType, header, body);
  }
}