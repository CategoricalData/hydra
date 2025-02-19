// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Decl_Val implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Decl_Val");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_PATS = new hydra.core.Name("pats");
  
  public static final hydra.core.Name FIELD_NAME_DECLTPE = new hydra.core.Name("decltpe");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final java.util.List<hydra.ext.scala.meta.Pat> pats;
  
  public final hydra.ext.scala.meta.Type decltpe;
  
  public Decl_Val (java.util.List<hydra.ext.scala.meta.Mod> mods, java.util.List<hydra.ext.scala.meta.Pat> pats, hydra.ext.scala.meta.Type decltpe) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((pats));
    java.util.Objects.requireNonNull((decltpe));
    this.mods = mods;
    this.pats = pats;
    this.decltpe = decltpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Decl_Val)) {
      return false;
    }
    Decl_Val o = (Decl_Val) (other);
    return mods.equals(o.mods) && pats.equals(o.pats) && decltpe.equals(o.decltpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * pats.hashCode() + 5 * decltpe.hashCode();
  }
  
  public Decl_Val withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Decl_Val(mods, pats, decltpe);
  }
  
  public Decl_Val withPats(java.util.List<hydra.ext.scala.meta.Pat> pats) {
    java.util.Objects.requireNonNull((pats));
    return new Decl_Val(mods, pats, decltpe);
  }
  
  public Decl_Val withDecltpe(hydra.ext.scala.meta.Type decltpe) {
    java.util.Objects.requireNonNull((decltpe));
    return new Decl_Val(mods, pats, decltpe);
  }
}