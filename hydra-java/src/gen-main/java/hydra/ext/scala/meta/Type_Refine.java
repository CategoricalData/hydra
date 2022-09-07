package hydra.ext.scala.meta;

public class Type_Refine {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Refine");
  
  public final java.util.Optional<hydra.ext.scala.meta.Type> tpe;
  
  public final java.util.List<hydra.ext.scala.meta.Stat> stats;
  
  public Type_Refine (java.util.Optional<hydra.ext.scala.meta.Type> tpe, java.util.List<hydra.ext.scala.meta.Stat> stats) {
    this.tpe = tpe;
    this.stats = stats;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Refine)) {
      return false;
    }
    Type_Refine o = (Type_Refine) (other);
    return tpe.equals(o.tpe) && stats.equals(o.stats);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode() + 3 * stats.hashCode();
  }
  
  public Type_Refine withTpe(java.util.Optional<hydra.ext.scala.meta.Type> tpe) {
    return new Type_Refine(tpe, stats);
  }
  
  public Type_Refine withStats(java.util.List<hydra.ext.scala.meta.Stat> stats) {
    return new Type_Refine(tpe, stats);
  }
}