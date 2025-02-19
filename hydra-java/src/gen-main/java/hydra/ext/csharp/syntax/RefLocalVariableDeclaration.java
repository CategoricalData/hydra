// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RefLocalVariableDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefLocalVariableDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_REF_KIND = new hydra.core.Name("refKind");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATORS = new hydra.core.Name("declarators");
  
  public final hydra.ext.csharp.syntax.RefKind refKind;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final java.util.List<hydra.ext.csharp.syntax.RefLocalVariableDeclarator> declarators;
  
  public RefLocalVariableDeclaration (hydra.ext.csharp.syntax.RefKind refKind, hydra.ext.csharp.syntax.Type type, java.util.List<hydra.ext.csharp.syntax.RefLocalVariableDeclarator> declarators) {
    java.util.Objects.requireNonNull((refKind));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((declarators));
    this.refKind = refKind;
    this.type = type;
    this.declarators = declarators;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RefLocalVariableDeclaration)) {
      return false;
    }
    RefLocalVariableDeclaration o = (RefLocalVariableDeclaration) (other);
    return refKind.equals(o.refKind) && type.equals(o.type) && declarators.equals(o.declarators);
  }
  
  @Override
  public int hashCode() {
    return 2 * refKind.hashCode() + 3 * type.hashCode() + 5 * declarators.hashCode();
  }
  
  public RefLocalVariableDeclaration withRefKind(hydra.ext.csharp.syntax.RefKind refKind) {
    java.util.Objects.requireNonNull((refKind));
    return new RefLocalVariableDeclaration(refKind, type, declarators);
  }
  
  public RefLocalVariableDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new RefLocalVariableDeclaration(refKind, type, declarators);
  }
  
  public RefLocalVariableDeclaration withDeclarators(java.util.List<hydra.ext.csharp.syntax.RefLocalVariableDeclarator> declarators) {
    java.util.Objects.requireNonNull((declarators));
    return new RefLocalVariableDeclaration(refKind, type, declarators);
  }
}