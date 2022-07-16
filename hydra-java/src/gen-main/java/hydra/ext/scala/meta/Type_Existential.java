package hydra.ext.scala.meta;

public class Type_Existential {
  public final Type tpe;
  
  public final java.util.List<Stat> stats;
  
  public Type_Existential (Type tpe, java.util.List<Stat> stats) {
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
  
  public Type_Existential withTpe(Type tpe) {
    return new Type_Existential(tpe, stats);
  }
  
  public Type_Existential withStats(java.util.List<Stat> stats) {
    return new Type_Existential(tpe, stats);
  }
}