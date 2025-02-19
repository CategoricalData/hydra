// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class StandardLocalFunctionDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StandardLocalFunctionDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_RETURN_TYPE = new hydra.core.Name("returnType");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.csharp.syntax.LocalFunctionModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.ReturnType returnType;
  
  public final hydra.ext.csharp.syntax.LocalFunctionHeader header;
  
  public final hydra.ext.csharp.syntax.LocalFunctionBody body;
  
  public StandardLocalFunctionDeclaration (java.util.List<hydra.ext.csharp.syntax.LocalFunctionModifier> modifiers, hydra.ext.csharp.syntax.ReturnType returnType, hydra.ext.csharp.syntax.LocalFunctionHeader header, hydra.ext.csharp.syntax.LocalFunctionBody body) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((returnType));
    java.util.Objects.requireNonNull((header));
    java.util.Objects.requireNonNull((body));
    this.modifiers = modifiers;
    this.returnType = returnType;
    this.header = header;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StandardLocalFunctionDeclaration)) {
      return false;
    }
    StandardLocalFunctionDeclaration o = (StandardLocalFunctionDeclaration) (other);
    return modifiers.equals(o.modifiers) && returnType.equals(o.returnType) && header.equals(o.header) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * returnType.hashCode() + 5 * header.hashCode() + 7 * body.hashCode();
  }
  
  public StandardLocalFunctionDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.LocalFunctionModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new StandardLocalFunctionDeclaration(modifiers, returnType, header, body);
  }
  
  public StandardLocalFunctionDeclaration withReturnType(hydra.ext.csharp.syntax.ReturnType returnType) {
    java.util.Objects.requireNonNull((returnType));
    return new StandardLocalFunctionDeclaration(modifiers, returnType, header, body);
  }
  
  public StandardLocalFunctionDeclaration withHeader(hydra.ext.csharp.syntax.LocalFunctionHeader header) {
    java.util.Objects.requireNonNull((header));
    return new StandardLocalFunctionDeclaration(modifiers, returnType, header, body);
  }
  
  public StandardLocalFunctionDeclaration withBody(hydra.ext.csharp.syntax.LocalFunctionBody body) {
    java.util.Objects.requireNonNull((body));
    return new StandardLocalFunctionDeclaration(modifiers, returnType, header, body);
  }
}