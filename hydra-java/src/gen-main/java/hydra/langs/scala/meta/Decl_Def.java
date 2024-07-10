// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Decl_Def implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Decl.Def");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Data_Name name;
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss;
  
  public final hydra.langs.scala.meta.Type decltpe;
  
  public Decl_Def (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Data_Name name, java.util.List<hydra.langs.scala.meta.Type_Param> tparams, java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss, hydra.langs.scala.meta.Type decltpe) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    if (paramss == null) {
      throw new IllegalArgumentException("null value for 'paramss' argument");
    }
    if (decltpe == null) {
      throw new IllegalArgumentException("null value for 'decltpe' argument");
    }
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
  
  public Decl_Def withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withName(hydra.langs.scala.meta.Data_Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withParamss(java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss) {
    if (paramss == null) {
      throw new IllegalArgumentException("null value for 'paramss' argument");
    }
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withDecltpe(hydra.langs.scala.meta.Type decltpe) {
    if (decltpe == null) {
      throw new IllegalArgumentException("null value for 'decltpe' argument");
    }
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
}