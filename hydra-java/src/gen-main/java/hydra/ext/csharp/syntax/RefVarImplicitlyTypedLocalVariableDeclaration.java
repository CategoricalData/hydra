// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RefVarImplicitlyTypedLocalVariableDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefVarImplicitlyTypedLocalVariableDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_REF_KIND = new hydra.core.Name("refKind");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATOR = new hydra.core.Name("declarator");
  
  public final hydra.ext.csharp.syntax.RefKind refKind;
  
  public final hydra.ext.csharp.syntax.RefLocalVariableDeclarator declarator;
  
  public RefVarImplicitlyTypedLocalVariableDeclaration (hydra.ext.csharp.syntax.RefKind refKind, hydra.ext.csharp.syntax.RefLocalVariableDeclarator declarator) {
    java.util.Objects.requireNonNull((refKind));
    java.util.Objects.requireNonNull((declarator));
    this.refKind = refKind;
    this.declarator = declarator;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RefVarImplicitlyTypedLocalVariableDeclaration)) {
      return false;
    }
    RefVarImplicitlyTypedLocalVariableDeclaration o = (RefVarImplicitlyTypedLocalVariableDeclaration) (other);
    return refKind.equals(o.refKind) && declarator.equals(o.declarator);
  }
  
  @Override
  public int hashCode() {
    return 2 * refKind.hashCode() + 3 * declarator.hashCode();
  }
  
  public RefVarImplicitlyTypedLocalVariableDeclaration withRefKind(hydra.ext.csharp.syntax.RefKind refKind) {
    java.util.Objects.requireNonNull((refKind));
    return new RefVarImplicitlyTypedLocalVariableDeclaration(refKind, declarator);
  }
  
  public RefVarImplicitlyTypedLocalVariableDeclaration withDeclarator(hydra.ext.csharp.syntax.RefLocalVariableDeclarator declarator) {
    java.util.Objects.requireNonNull((declarator));
    return new RefVarImplicitlyTypedLocalVariableDeclaration(refKind, declarator);
  }
}