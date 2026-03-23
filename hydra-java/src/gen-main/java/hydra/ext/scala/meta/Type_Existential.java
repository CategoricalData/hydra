// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Existential implements Serializable, Comparable<Type_Existential> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_Existential");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public static final hydra.core.Name STATS = new hydra.core.Name("stats");

  public final hydra.ext.scala.meta.Type tpe;

  public final hydra.util.ConsList<hydra.ext.scala.meta.Stat> stats;

  public Type_Existential (hydra.ext.scala.meta.Type tpe, hydra.util.ConsList<hydra.ext.scala.meta.Stat> stats) {
    this.tpe = tpe;
    this.stats = stats;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Existential)) {
      return false;
    }
    Type_Existential o = (Type_Existential) other;
    return java.util.Objects.equals(
      this.tpe,
      o.tpe) && java.util.Objects.equals(
      this.stats,
      o.stats);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tpe) + 3 * java.util.Objects.hashCode(stats);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Existential other) {
    int cmp = 0;
    cmp = ((Comparable) tpe).compareTo(other.tpe);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) stats).compareTo(other.stats);
  }

  public Type_Existential withTpe(hydra.ext.scala.meta.Type tpe) {
    return new Type_Existential(tpe, stats);
  }

  public Type_Existential withStats(hydra.util.ConsList<hydra.ext.scala.meta.Stat> stats) {
    return new Type_Existential(tpe, stats);
  }
}
