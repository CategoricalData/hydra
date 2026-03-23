// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Macro implements Serializable, Comparable<Pat_Macro> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Pat_Macro");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.ext.scala.meta.Data body;

  public Pat_Macro (hydra.ext.scala.meta.Data body) {
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Macro)) {
      return false;
    }
    Pat_Macro o = (Pat_Macro) other;
    return java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pat_Macro other) {
    return ((Comparable) body).compareTo(other.body);
  }
}
