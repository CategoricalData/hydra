// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A type signature
 */
public class TypeSignature implements Serializable, Comparable<TypeSignature> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.TypeSignature");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  /**
   * The name being typed
   */
  public final hydra.haskell.syntax.Name name;

  /**
   * The type
   */
  public final hydra.haskell.syntax.Type type;

  public TypeSignature (hydra.haskell.syntax.Name name, hydra.haskell.syntax.Type type) {
    this.name = name;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSignature)) {
      return false;
    }
    TypeSignature o = (TypeSignature) other;
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
  public int compareTo(TypeSignature other) {
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

  public TypeSignature withName(hydra.haskell.syntax.Name name) {
    return new TypeSignature(name, type);
  }

  public TypeSignature withType(hydra.haskell.syntax.Type type) {
    return new TypeSignature(name, type);
  }
}
