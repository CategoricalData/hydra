// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Importee_Unimport implements Serializable, Comparable<Importee_Unimport> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Importee_Unimport");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.ext.scala.syntax.Name name;

  public Importee_Unimport (hydra.ext.scala.syntax.Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Unimport)) {
      return false;
    }
    Importee_Unimport o = (Importee_Unimport) other;
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
  public int compareTo(Importee_Unimport other) {
    return ((Comparable) name).compareTo(other.name);
  }
}
