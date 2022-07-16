package hydra.ext.scala.meta;

public class Type_Param {
  public final java.util.List<Mod> mods;
  
  public final Name name;
  
  public final java.util.List<Type_Param> tparams;
  
  public final java.util.List<Type_Bounds> tbounds;
  
  public final java.util.List<Type> vbounds;
  
  public final java.util.List<Type> cbounds;
  
  public Type_Param (java.util.List<Mod> mods, Name name, java.util.List<Type_Param> tparams, java.util.List<Type_Bounds> tbounds, java.util.List<Type> vbounds, java.util.List<Type> cbounds) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.tbounds = tbounds;
    this.vbounds = vbounds;
    this.cbounds = cbounds;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Param)) {
      return false;
    }
    Type_Param o = (Type_Param) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && tbounds.equals(o.tbounds) && vbounds.equals(o.vbounds) && cbounds.equals(o.cbounds);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * tbounds.hashCode() + 11 * vbounds.hashCode() + 13 * cbounds.hashCode();
  }
  
  public Type_Param withMods(java.util.List<Mod> mods) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withName(Name name) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withTparams(java.util.List<Type_Param> tparams) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withTbounds(java.util.List<Type_Bounds> tbounds) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withVbounds(java.util.List<Type> vbounds) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withCbounds(java.util.List<Type> cbounds) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
}