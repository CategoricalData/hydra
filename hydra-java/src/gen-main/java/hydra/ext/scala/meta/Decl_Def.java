package hydra.ext.scala.meta;

public class Decl_Def {
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final hydra.ext.scala.meta.Data_Name name;
  
  public final java.util.List<hydra.ext.scala.meta.Type_Param> tparams;
  
  public final java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> paramss;
  
  public final hydra.ext.scala.meta.Type decltpe;
  
  public Decl_Def (java.util.List<hydra.ext.scala.meta.Mod> mods, hydra.ext.scala.meta.Data_Name name, java.util.List<hydra.ext.scala.meta.Type_Param> tparams, java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> paramss, hydra.ext.scala.meta.Type decltpe) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.paramss = paramss;
    this.decltpe = decltpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Decl_Def)) {
      return false;
    }
    Decl_Def o = (Decl_Def) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && paramss.equals(o.paramss) && decltpe.equals(o.decltpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * paramss.hashCode() + 11 * decltpe.hashCode();
  }
  
  public Decl_Def withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withName(hydra.ext.scala.meta.Data_Name name) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withTparams(java.util.List<hydra.ext.scala.meta.Type_Param> tparams) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withParamss(java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> paramss) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withDecltpe(hydra.ext.scala.meta.Type decltpe) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
}