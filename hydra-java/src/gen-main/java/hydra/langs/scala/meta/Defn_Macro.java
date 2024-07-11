// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_Macro implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.Macro");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Data_Name name;
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss;
  
  public final hydra.util.Opt<hydra.langs.scala.meta.Type> decltpe;
  
  public final hydra.langs.scala.meta.Data body;
  
  public Defn_Macro (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Data_Name name, java.util.List<hydra.langs.scala.meta.Type_Param> tparams, java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss, hydra.util.Opt<hydra.langs.scala.meta.Type> decltpe, hydra.langs.scala.meta.Data body) {
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
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.paramss = paramss;
    this.decltpe = decltpe;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Macro)) {
      return false;
    }
    Defn_Macro o = (Defn_Macro) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && paramss.equals(o.paramss) && decltpe.equals(o.decltpe) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * paramss.hashCode() + 11 * decltpe.hashCode() + 13 * body.hashCode();
  }
  
  public Defn_Macro withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withName(hydra.langs.scala.meta.Data_Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withParamss(java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss) {
    if (paramss == null) {
      throw new IllegalArgumentException("null value for 'paramss' argument");
    }
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withDecltpe(hydra.util.Opt<hydra.langs.scala.meta.Type> decltpe) {
    if (decltpe == null) {
      throw new IllegalArgumentException("null value for 'decltpe' argument");
    }
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withBody(hydra.langs.scala.meta.Data body) {
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
}