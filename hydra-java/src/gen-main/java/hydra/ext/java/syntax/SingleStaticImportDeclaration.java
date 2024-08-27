// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SingleStaticImportDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.SingleStaticImportDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.java.syntax.TypeName typeName;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public SingleStaticImportDeclaration (hydra.ext.java.syntax.TypeName typeName, hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((identifier));
    this.typeName = typeName;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleStaticImportDeclaration)) {
      return false;
    }
    SingleStaticImportDeclaration o = (SingleStaticImportDeclaration) (other);
    return typeName.equals(o.typeName) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * identifier.hashCode();
  }
  
  public SingleStaticImportDeclaration withTypeName(hydra.ext.java.syntax.TypeName typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new SingleStaticImportDeclaration(typeName, identifier);
  }
  
  public SingleStaticImportDeclaration withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new SingleStaticImportDeclaration(typeName, identifier);
  }
}