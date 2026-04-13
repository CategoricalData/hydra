// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Pat_Repeated implements Serializable, Comparable<Pat_Repeated> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Pat_Repeated");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.scala.syntax.Data_Name name;

  public Pat_Repeated (hydra.scala.syntax.Data_Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Repeated)) {
      return false;
    }
    Pat_Repeated o = (Pat_Repeated) other;
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
  public int compareTo(Pat_Repeated other) {
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }
}
