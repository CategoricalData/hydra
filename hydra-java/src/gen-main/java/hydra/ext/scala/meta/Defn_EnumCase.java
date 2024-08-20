// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Defn_EnumCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Defn.EnumCase");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_CTOR = new hydra.core.Name("ctor");
  
  public static final hydra.core.Name FIELD_NAME_INITS = new hydra.core.Name("inits");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final hydra.ext.scala.meta.Data_Name name;
  
  public final java.util.List<hydra.ext.scala.meta.Type_Param> tparams;
  
  public final hydra.ext.scala.meta.Ctor_Primary ctor;
  
  public final java.util.List<hydra.ext.scala.meta.Init> inits;
  
  public Defn_EnumCase (java.util.List<hydra.ext.scala.meta.Mod> mods, hydra.ext.scala.meta.Data_Name name, java.util.List<hydra.ext.scala.meta.Type_Param> tparams, hydra.ext.scala.meta.Ctor_Primary ctor, java.util.List<hydra.ext.scala.meta.Init> inits) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((ctor));
    java.util.Objects.requireNonNull((inits));
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.ctor = ctor;
    this.inits = inits;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_EnumCase)) {
      return false;
    }
    Defn_EnumCase o = (Defn_EnumCase) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && ctor.equals(o.ctor) && inits.equals(o.inits);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * ctor.hashCode() + 11 * inits.hashCode();
  }
  
  public Defn_EnumCase withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }
  
  public Defn_EnumCase withName(hydra.ext.scala.meta.Data_Name name) {
    java.util.Objects.requireNonNull((name));
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }
  
  public Defn_EnumCase withTparams(java.util.List<hydra.ext.scala.meta.Type_Param> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }
  
  public Defn_EnumCase withCtor(hydra.ext.scala.meta.Ctor_Primary ctor) {
    java.util.Objects.requireNonNull((ctor));
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }
  
  public Defn_EnumCase withInits(java.util.List<hydra.ext.scala.meta.Init> inits) {
    java.util.Objects.requireNonNull((inits));
    return new Defn_EnumCase(mods, name, tparams, ctor, inits);
  }
}
