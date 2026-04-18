// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A type synonym declaration
 */
public class TypeDeclaration implements Serializable, Comparable<TypeDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.TypeDeclaration");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  /**
   * The declaration head
   */
  public final hydra.haskell.syntax.DeclarationHead name;

  /**
   * The type being defined
   */
  public final hydra.haskell.syntax.Type type;

  public TypeDeclaration (hydra.haskell.syntax.DeclarationHead name, hydra.haskell.syntax.Type type) {
    this.name = name;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeDeclaration)) {
      return false;
    }
    TypeDeclaration o = (TypeDeclaration) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public TypeDeclaration withName(hydra.haskell.syntax.DeclarationHead name) {
    return new TypeDeclaration(name, type);
  }

  public TypeDeclaration withType(hydra.haskell.syntax.Type type) {
    return new TypeDeclaration(name, type);
  }
}
