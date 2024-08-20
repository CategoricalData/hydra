// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Decl_Type implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Decl.Type");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_BOUNDS = new hydra.core.Name("bounds");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final hydra.ext.scala.meta.Type_Name name;
  
  public final java.util.List<hydra.ext.scala.meta.Type_Param> tparams;
  
  public final hydra.ext.scala.meta.Type_Bounds bounds;
  
  public Decl_Type (java.util.List<hydra.ext.scala.meta.Mod> mods, hydra.ext.scala.meta.Type_Name name, java.util.List<hydra.ext.scala.meta.Type_Param> tparams, hydra.ext.scala.meta.Type_Bounds bounds) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((bounds));
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
  
  public Decl_Type withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Decl_Type(mods, name, tparams, bounds);
  }
  
  public Decl_Type withName(hydra.ext.scala.meta.Type_Name name) {
    java.util.Objects.requireNonNull((name));
    return new Decl_Type(mods, name, tparams, bounds);
  }
  
  public Decl_Type withTparams(java.util.List<hydra.ext.scala.meta.Type_Param> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Decl_Type(mods, name, tparams, bounds);
  }
  
  public Decl_Type withBounds(hydra.ext.scala.meta.Type_Bounds bounds) {
    java.util.Objects.requireNonNull((bounds));
    return new Decl_Type(mods, name, tparams, bounds);
  }
}
