// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Decl_Type implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Decl.Type");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Type_Name name;
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final hydra.langs.scala.meta.Type_Bounds bounds;
  
  public Decl_Type (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Type_Name name, java.util.List<hydra.langs.scala.meta.Type_Param> tparams, hydra.langs.scala.meta.Type_Bounds bounds) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    if (bounds == null) {
      throw new IllegalArgumentException("null value for 'bounds' argument");
    }
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.bounds = bounds;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Decl_Type)) {
      return false;
    }
    Decl_Type o = (Decl_Type) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && bounds.equals(o.bounds);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * bounds.hashCode();
  }
  
  public Decl_Type withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    return new Decl_Type(mods, name, tparams, bounds);
  }
  
  public Decl_Type withName(hydra.langs.scala.meta.Type_Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Decl_Type(mods, name, tparams, bounds);
  }
  
  public Decl_Type withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    return new Decl_Type(mods, name, tparams, bounds);
  }
  
  public Decl_Type withBounds(hydra.langs.scala.meta.Type_Bounds bounds) {
    if (bounds == null) {
      throw new IllegalArgumentException("null value for 'bounds' argument");
    }
    return new Decl_Type(mods, name, tparams, bounds);
  }
}