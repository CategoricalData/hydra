// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class ScalaSymbol implements Serializable, Comparable<ScalaSymbol> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.ScalaSymbol");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final String name;

  public ScalaSymbol (String name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScalaSymbol)) {
      return false;
    }
    ScalaSymbol o = (ScalaSymbol) other;
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
  public int compareTo(ScalaSymbol other) {
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }
}
