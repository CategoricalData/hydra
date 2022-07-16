package hydra.ext.scala.meta;

public class Ctor_Primary {
  public final java.util.List<Mod> mods;
  
  public final Name name;
  
  public final java.util.List<java.util.List<Data_Param>> paramss;
  
  public Ctor_Primary (java.util.List<Mod> mods, Name name, java.util.List<java.util.List<Data_Param>> paramss) {
    this.mods = mods;
    this.name = name;
    this.paramss = paramss;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ctor_Primary)) {
      return false;
    }
    Ctor_Primary o = (Ctor_Primary) (other);
    return mods.equals(o.mods) && name.equals(o.name) && paramss.equals(o.paramss);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * paramss.hashCode();
  }
  
  public Ctor_Primary withMods(java.util.List<Mod> mods) {
    return new Ctor_Primary(mods, name, paramss);
  }
  
  public Ctor_Primary withName(Name name) {
    return new Ctor_Primary(mods, name, paramss);
  }
  
  public Ctor_Primary withParamss(java.util.List<java.util.List<Data_Param>> paramss) {
    return new Ctor_Primary(mods, name, paramss);
  }
}