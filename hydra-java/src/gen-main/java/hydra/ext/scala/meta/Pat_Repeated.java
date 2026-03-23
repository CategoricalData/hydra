// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Repeated implements Serializable, Comparable<Pat_Repeated> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Pat_Repeated");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.ext.scala.meta.Data_Name name;

  public Pat_Repeated (hydra.ext.scala.meta.Data_Name name) {
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
    return ((Comparable) name).compareTo(other.name);
  }
}
