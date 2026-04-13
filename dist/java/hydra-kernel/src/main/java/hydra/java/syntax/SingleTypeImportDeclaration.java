// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class SingleTypeImportDeclaration implements Serializable, Comparable<SingleTypeImportDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.SingleTypeImportDeclaration");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.java.syntax.TypeName value;

  public SingleTypeImportDeclaration (hydra.java.syntax.TypeName value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleTypeImportDeclaration)) {
      return false;
    }
    SingleTypeImportDeclaration o = (SingleTypeImportDeclaration) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SingleTypeImportDeclaration other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
