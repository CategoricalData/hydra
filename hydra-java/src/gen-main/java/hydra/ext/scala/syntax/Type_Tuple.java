// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Type_Tuple implements Serializable, Comparable<Type_Tuple> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Type_Tuple");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final java.util.List<hydra.ext.scala.syntax.Type> args;

  public Type_Tuple (java.util.List<hydra.ext.scala.syntax.Type> args) {
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Tuple)) {
      return false;
    }
    Type_Tuple o = (Type_Tuple) other;
    return java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Tuple other) {
    return hydra.util.Comparing.compare(
      args,
      other.args);
  }
}
