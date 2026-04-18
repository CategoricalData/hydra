// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class MethodReference_Primary implements Serializable, Comparable<MethodReference_Primary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.MethodReference_Primary");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public final hydra.java.syntax.Primary primary;

  public final java.util.List<hydra.java.syntax.TypeArgument> typeArguments;

  public final hydra.java.syntax.Identifier identifier;

  public MethodReference_Primary (hydra.java.syntax.Primary primary, java.util.List<hydra.java.syntax.TypeArgument> typeArguments, hydra.java.syntax.Identifier identifier) {
    this.primary = primary;
    this.typeArguments = typeArguments;
    this.identifier = identifier;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_Primary)) {
      return false;
    }
    MethodReference_Primary o = (MethodReference_Primary) other;
    return java.util.Objects.equals(
      this.primary,
      o.primary) && java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments) && java.util.Objects.equals(
      this.identifier,
      o.identifier);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(primary) + 3 * java.util.Objects.hashCode(typeArguments) + 5 * java.util.Objects.hashCode(identifier);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodReference_Primary other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      primary,
      other.primary);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      typeArguments,
      other.typeArguments);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      identifier,
      other.identifier);
  }

  public MethodReference_Primary withPrimary(hydra.java.syntax.Primary primary) {
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }

  public MethodReference_Primary withTypeArguments(java.util.List<hydra.java.syntax.TypeArgument> typeArguments) {
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }

  public MethodReference_Primary withIdentifier(hydra.java.syntax.Identifier identifier) {
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
}
