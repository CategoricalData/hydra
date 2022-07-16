package hydra.ext.scala.meta;

public class Decl_Type {
  public final java.util.List<Mod> mods;
  
  public final Type_Name name;
  
  public final java.util.List<Type_Param> tparams;
  
  public final Type_Bounds bounds;
  
  public Decl_Type (java.util.List<Mod> mods, Type_Name name, java.util.List<Type_Param> tparams, Type_Bounds bounds) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.bounds = bounds;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Decl_Type)) {
      return false;
    }
    Decl_Type o = (Decl_Type) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && bounds.equals(o.bounds);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * bounds.hashCode();
  }
  
  public Decl_Type withMods(java.util.List<Mod> mods) {
    return new Decl_Type(mods, name, tparams, bounds);
  }
  
  public Decl_Type withName(Type_Name name) {
    return new Decl_Type(mods, name, tparams, bounds);
  }
  
  public Decl_Type withTparams(java.util.List<Type_Param> tparams) {
    return new Decl_Type(mods, name, tparams, bounds);
  }
  
  public Decl_Type withBounds(Type_Bounds bounds) {
    return new Decl_Type(mods, name, tparams, bounds);
  }
}