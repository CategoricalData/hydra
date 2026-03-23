// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Importee_Name implements Serializable, Comparable<Importee_Name> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Importee_Name");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.ext.scala.meta.Name name;

  public Importee_Name (hydra.ext.scala.meta.Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Name)) {
      return false;
    }
    Importee_Name o = (Importee_Name) other;
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
  public int compareTo(Importee_Name other) {
    return ((Comparable) name).compareTo(other.name);
  }
}
