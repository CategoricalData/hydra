// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class SingleStaticImportDeclaration implements Serializable, Comparable<SingleStaticImportDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.SingleStaticImportDeclaration");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public final hydra.java.syntax.TypeName typeName;

  public final hydra.java.syntax.Identifier identifier;

  public SingleStaticImportDeclaration (hydra.java.syntax.TypeName typeName, hydra.java.syntax.Identifier identifier) {
    this.typeName = typeName;
    this.identifier = identifier;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleStaticImportDeclaration)) {
      return false;
    }
    SingleStaticImportDeclaration o = (SingleStaticImportDeclaration) other;
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.identifier,
      o.identifier);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(identifier);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SingleStaticImportDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typeName,
      other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      identifier,
      other.identifier);
  }

  public SingleStaticImportDeclaration withTypeName(hydra.java.syntax.TypeName typeName) {
    return new SingleStaticImportDeclaration(typeName, identifier);
  }

  public SingleStaticImportDeclaration withIdentifier(hydra.java.syntax.Identifier identifier) {
    return new SingleStaticImportDeclaration(typeName, identifier);
  }
}
