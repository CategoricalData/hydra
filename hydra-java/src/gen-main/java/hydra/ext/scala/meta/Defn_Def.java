package hydra.ext.scala.meta;

public class Defn_Def {
  public final java.util.List<Mod> mods;
  
  public final Data_Name name;
  
  public final java.util.List<Type_Param> tparams;
  
  public final java.util.List<java.util.List<Data_Param>> paramss;
  
  public final java.util.Optional<Type> decltpe;
  
  public final Data body;
  
  public Defn_Def (java.util.List<Mod> mods, Data_Name name, java.util.List<Type_Param> tparams, java.util.List<java.util.List<Data_Param>> paramss, java.util.Optional<Type> decltpe, Data body) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.paramss = paramss;
    this.decltpe = decltpe;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Def)) {
      return false;
    }
    Defn_Def o = (Defn_Def) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && paramss.equals(o.paramss) && decltpe.equals(o.decltpe) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * paramss.hashCode() + 11 * decltpe.hashCode() + 13 * body.hashCode();
  }
  
  public Defn_Def withMods(java.util.List<Mod> mods) {
    return new Defn_Def(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Def withName(Data_Name name) {
    return new Defn_Def(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Def withTparams(java.util.List<Type_Param> tparams) {
    return new Defn_Def(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Def withParamss(java.util.List<java.util.List<Data_Param>> paramss) {
    return new Defn_Def(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Def withDecltpe(java.util.Optional<Type> decltpe) {
    return new Defn_Def(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Def withBody(Data body) {
    return new Defn_Def(mods, name, tparams, paramss, decltpe, body);
  }
}