package hydra.ext.scala.meta;

public class Ctor_Secondary {
  public final java.util.List<Mod> mods;
  
  public final Name name;
  
  public final java.util.List<java.util.List<Data_Param>> paramss;
  
  public final Init init;
  
  public final java.util.List<Stat> stats;
  
  public Ctor_Secondary (java.util.List<Mod> mods, Name name, java.util.List<java.util.List<Data_Param>> paramss, Init init, java.util.List<Stat> stats) {
    this.mods = mods;
    this.name = name;
    this.paramss = paramss;
    this.init = init;
    this.stats = stats;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ctor_Secondary)) {
      return false;
    }
    Ctor_Secondary o = (Ctor_Secondary) (other);
    return mods.equals(o.mods) && name.equals(o.name) && paramss.equals(o.paramss) && init.equals(o.init) && stats.equals(o.stats);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * paramss.hashCode() + 7 * init.hashCode() + 11 * stats.hashCode();
  }
  
  public Ctor_Secondary withMods(java.util.List<Mod> mods) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
  
  public Ctor_Secondary withName(Name name) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
  
  public Ctor_Secondary withParamss(java.util.List<java.util.List<Data_Param>> paramss) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
  
  public Ctor_Secondary withInit(Init init) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
  
  public Ctor_Secondary withStats(java.util.List<Stat> stats) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
}