// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Importee_Rename implements Serializable, Comparable<Importee_Rename> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Importee_Rename");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name RENAME = new hydra.core.Name("rename");

  public final hydra.ext.scala.syntax.Name name;

  public final hydra.ext.scala.syntax.Name rename;

  public Importee_Rename (hydra.ext.scala.syntax.Name name, hydra.ext.scala.syntax.Name rename) {
    this.name = name;
    this.rename = rename;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Rename)) {
      return false;
    }
    Importee_Rename o = (Importee_Rename) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.rename,
      o.rename);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(rename);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Importee_Rename other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rename).compareTo(other.rename);
  }

  public Importee_Rename withName(hydra.ext.scala.syntax.Name name) {
    return new Importee_Rename(name, rename);
  }

  public Importee_Rename withRename(hydra.ext.scala.syntax.Name rename) {
    return new Importee_Rename(name, rename);
  }
}
