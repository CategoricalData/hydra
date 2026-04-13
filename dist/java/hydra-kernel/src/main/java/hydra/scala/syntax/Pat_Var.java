// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Pat_Var implements Serializable, Comparable<Pat_Var> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Pat_Var");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.scala.syntax.Data_Name name;

  public Pat_Var (hydra.scala.syntax.Data_Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Var)) {
      return false;
    }
    Pat_Var o = (Pat_Var) other;
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
  public int compareTo(Pat_Var other) {
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }
}
