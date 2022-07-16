package hydra.ext.scala.meta;

public class Pkg {
  public final Data_Name name;
  
  public final Data_Ref ref;
  
  public final java.util.List<Stat> stats;
  
  public Pkg (Data_Name name, Data_Ref ref, java.util.List<Stat> stats) {
    this.name = name;
    this.ref = ref;
    this.stats = stats;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pkg)) {
      return false;
    }
    Pkg o = (Pkg) (other);
    return name.equals(o.name) && ref.equals(o.ref) && stats.equals(o.stats);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * ref.hashCode() + 5 * stats.hashCode();
  }
  
  public Pkg withName(Data_Name name) {
    return new Pkg(name, ref, stats);
  }
  
  public Pkg withRef(Data_Ref ref) {
    return new Pkg(name, ref, stats);
  }
  
  public Pkg withStats(java.util.List<Stat> stats) {
    return new Pkg(name, ref, stats);
  }
}