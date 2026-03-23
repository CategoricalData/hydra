// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Macro implements Serializable, Comparable<Type_Macro> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_Macro");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.ext.scala.meta.Data body;

  public Type_Macro (hydra.ext.scala.meta.Data body) {
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Macro)) {
      return false;
    }
    Type_Macro o = (Type_Macro) other;
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
  public int compareTo(Type_Macro other) {
    return ((Comparable) body).compareTo(other.body);
  }
}
