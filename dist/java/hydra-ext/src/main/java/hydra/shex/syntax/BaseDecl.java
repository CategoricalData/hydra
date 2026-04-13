// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class BaseDecl implements Serializable, Comparable<BaseDecl> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.BaseDecl");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.shex.syntax.IriRef value;

  public BaseDecl (hydra.shex.syntax.IriRef value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BaseDecl)) {
      return false;
    }
    BaseDecl o = (BaseDecl) other;
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
  public int compareTo(BaseDecl other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
