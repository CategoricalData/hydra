// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Defn_Object implements Serializable, Comparable<Defn_Object> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Defn_Object");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.scala.syntax.Data_Name name;

  public Defn_Object (hydra.scala.syntax.Data_Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Object)) {
      return false;
    }
    Defn_Object o = (Defn_Object) other;
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
  public int compareTo(Defn_Object other) {
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }
}
