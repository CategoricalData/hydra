// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Param implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Param");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_TBOUNDS = new hydra.core.Name("tbounds");
  
  public static final hydra.core.Name FIELD_NAME_VBOUNDS = new hydra.core.Name("vbounds");
  
  public static final hydra.core.Name FIELD_NAME_CBOUNDS = new hydra.core.Name("cbounds");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final hydra.ext.scala.meta.Name name;
  
  public final java.util.List<hydra.ext.scala.meta.Type_Param> tparams;
  
  public final java.util.List<hydra.ext.scala.meta.Type_Bounds> tbounds;
  
  public final java.util.List<hydra.ext.scala.meta.Type> vbounds;
  
  public final java.util.List<hydra.ext.scala.meta.Type> cbounds;
  
  public Type_Param (java.util.List<hydra.ext.scala.meta.Mod> mods, hydra.ext.scala.meta.Name name, java.util.List<hydra.ext.scala.meta.Type_Param> tparams, java.util.List<hydra.ext.scala.meta.Type_Bounds> tbounds, java.util.List<hydra.ext.scala.meta.Type> vbounds, java.util.List<hydra.ext.scala.meta.Type> cbounds) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((tbounds));
    java.util.Objects.requireNonNull((vbounds));
    java.util.Objects.requireNonNull((cbounds));
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
  
  public Type_Param withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withName(hydra.ext.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withTparams(java.util.List<hydra.ext.scala.meta.Type_Param> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withTbounds(java.util.List<hydra.ext.scala.meta.Type_Bounds> tbounds) {
    java.util.Objects.requireNonNull((tbounds));
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withVbounds(java.util.List<hydra.ext.scala.meta.Type> vbounds) {
    java.util.Objects.requireNonNull((vbounds));
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
  
  public Type_Param withCbounds(java.util.List<hydra.ext.scala.meta.Type> cbounds) {
    java.util.Objects.requireNonNull((cbounds));
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
}
