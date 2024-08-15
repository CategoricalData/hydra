// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_Macro implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.Macro");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_PARAMSS = new hydra.core.Name("paramss");
  
  public static final hydra.core.Name FIELD_NAME_DECLTPE = new hydra.core.Name("decltpe");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Data_Name name;
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss;
  
  public final hydra.util.Opt<hydra.langs.scala.meta.Type> decltpe;
  
  public final hydra.langs.scala.meta.Data body;
  
  public Defn_Macro (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Data_Name name, java.util.List<hydra.langs.scala.meta.Type_Param> tparams, java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss, hydra.util.Opt<hydra.langs.scala.meta.Type> decltpe, hydra.langs.scala.meta.Data body) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((paramss));
    java.util.Objects.requireNonNull((decltpe));
    java.util.Objects.requireNonNull((body));
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
    java.util.Objects.requireNonNull((mods));
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withName(hydra.langs.scala.meta.Data_Name name) {
    java.util.Objects.requireNonNull((name));
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withParamss(java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss) {
    java.util.Objects.requireNonNull((paramss));
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withDecltpe(hydra.util.Opt<hydra.langs.scala.meta.Type> decltpe) {
    java.util.Objects.requireNonNull((decltpe));
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
  
  public Defn_Macro withBody(hydra.langs.scala.meta.Data body) {
    java.util.Objects.requireNonNull((body));
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
}