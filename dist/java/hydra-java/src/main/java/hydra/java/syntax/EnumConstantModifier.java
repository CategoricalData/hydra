// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class EnumConstantModifier implements Serializable, Comparable<EnumConstantModifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.EnumConstantModifier");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.java.syntax.Annotation value;

  public EnumConstantModifier (hydra.java.syntax.Annotation value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumConstantModifier)) {
      return false;
    }
    EnumConstantModifier o = (EnumConstantModifier) other;
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
  public int compareTo(EnumConstantModifier other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
