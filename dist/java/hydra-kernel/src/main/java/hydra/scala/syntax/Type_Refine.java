// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_Refine implements Serializable, Comparable<Type_Refine> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_Refine");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public static final hydra.core.Name STATS = new hydra.core.Name("stats");

  public final hydra.util.Maybe<hydra.scala.syntax.Type> tpe;

  public final java.util.List<hydra.scala.syntax.Stat> stats;

  public Type_Refine (hydra.util.Maybe<hydra.scala.syntax.Type> tpe, java.util.List<hydra.scala.syntax.Stat> stats) {
    this.tpe = tpe;
    this.stats = stats;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Refine)) {
      return false;
    }
    Type_Refine o = (Type_Refine) other;
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
  public int compareTo(Type_Refine other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      tpe,
      other.tpe);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      stats,
      other.stats);
  }

  public Type_Refine withTpe(hydra.util.Maybe<hydra.scala.syntax.Type> tpe) {
    return new Type_Refine(tpe, stats);
  }

  public Type_Refine withStats(java.util.List<hydra.scala.syntax.Stat> stats) {
    return new Type_Refine(tpe, stats);
  }
}
