// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Type_Var implements Serializable, Comparable<Type_Var> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Type_Var");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.ext.scala.syntax.Type_Name name;

  public Type_Var (hydra.ext.scala.syntax.Type_Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Var)) {
      return false;
    }
    Type_Var o = (Type_Var) other;
    return java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Var other) {
    return ((Comparable) name).compareTo(other.name);
  }
}
