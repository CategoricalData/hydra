// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Decl_Def implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Decl.Def");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_PARAMSS = new hydra.core.Name("paramss");
  
  public static final hydra.core.Name FIELD_NAME_DECLTPE = new hydra.core.Name("decltpe");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Data_Name name;
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss;
  
  public final hydra.langs.scala.meta.Type decltpe;
  
  public Decl_Def (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Data_Name name, java.util.List<hydra.langs.scala.meta.Type_Param> tparams, java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss, hydra.langs.scala.meta.Type decltpe) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((paramss));
    java.util.Objects.requireNonNull((decltpe));
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
    java.util.Objects.requireNonNull((mods));
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withName(hydra.langs.scala.meta.Data_Name name) {
    java.util.Objects.requireNonNull((name));
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withParamss(java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss) {
    java.util.Objects.requireNonNull((paramss));
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
  
  public Decl_Def withDecltpe(hydra.langs.scala.meta.Type decltpe) {
    java.util.Objects.requireNonNull((decltpe));
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
}