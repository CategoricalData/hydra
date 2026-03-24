// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Pkg implements Serializable, Comparable<Pkg> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Pkg");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name REF = new hydra.core.Name("ref");

  public static final hydra.core.Name STATS = new hydra.core.Name("stats");

  public final hydra.ext.scala.syntax.Data_Name name;

  public final hydra.ext.scala.syntax.Data_Ref ref;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Stat> stats;

  public Pkg (hydra.ext.scala.syntax.Data_Name name, hydra.ext.scala.syntax.Data_Ref ref, hydra.util.ConsList<hydra.ext.scala.syntax.Stat> stats) {
    this.name = name;
    this.ref = ref;
    this.stats = stats;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pkg)) {
      return false;
    }
    Pkg o = (Pkg) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.ref,
      o.ref) && java.util.Objects.equals(
      this.stats,
      o.stats);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(ref) + 5 * java.util.Objects.hashCode(stats);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pkg other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) ref).compareTo(other.ref);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) stats).compareTo(other.stats);
  }

  public Pkg withName(hydra.ext.scala.syntax.Data_Name name) {
    return new Pkg(name, ref, stats);
  }

  public Pkg withRef(hydra.ext.scala.syntax.Data_Ref ref) {
    return new Pkg(name, ref, stats);
  }

  public Pkg withStats(hydra.util.ConsList<hydra.ext.scala.syntax.Stat> stats) {
    return new Pkg(name, ref, stats);
  }
}
