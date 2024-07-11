// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Type_Existential implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Type.Existential");
  
  public final hydra.langs.scala.meta.Type tpe;
  
  public final java.util.List<hydra.langs.scala.meta.Stat> stats;
  
  public Type_Existential (hydra.langs.scala.meta.Type tpe, java.util.List<hydra.langs.scala.meta.Stat> stats) {
    java.util.Objects.requireNonNull((tpe));
    java.util.Objects.requireNonNull((stats));
    this.tpe = tpe;
    this.stats = stats;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Existential)) {
      return false;
    }
    Type_Existential o = (Type_Existential) (other);
    return tpe.equals(o.tpe) && stats.equals(o.stats);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode() + 3 * stats.hashCode();
  }
  
  public Type_Existential withTpe(hydra.langs.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tpe));
    return new Type_Existential(tpe, stats);
  }
  
  public Type_Existential withStats(java.util.List<hydra.langs.scala.meta.Stat> stats) {
    java.util.Objects.requireNonNull((stats));
    return new Type_Existential(tpe, stats);
  }
}