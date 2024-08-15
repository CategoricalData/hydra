// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_Type implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.Type");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Type_Name name;
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final hydra.langs.scala.meta.Type body;
  
  public Defn_Type (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Type_Name name, java.util.List<hydra.langs.scala.meta.Type_Param> tparams, hydra.langs.scala.meta.Type body) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((body));
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Type)) {
      return false;
    }
    Defn_Type o = (Defn_Type) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * body.hashCode();
  }
  
  public Defn_Type withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Defn_Type(mods, name, tparams, body);
  }
  
  public Defn_Type withName(hydra.langs.scala.meta.Type_Name name) {
    java.util.Objects.requireNonNull((name));
    return new Defn_Type(mods, name, tparams, body);
  }
  
  public Defn_Type withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Defn_Type(mods, name, tparams, body);
  }
  
  public Defn_Type withBody(hydra.langs.scala.meta.Type body) {
    java.util.Objects.requireNonNull((body));
    return new Defn_Type(mods, name, tparams, body);
  }
}