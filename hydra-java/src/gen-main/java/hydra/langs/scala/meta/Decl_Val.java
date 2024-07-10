// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Decl_Val implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Decl.Val");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final java.util.List<hydra.langs.scala.meta.Pat> pats;
  
  public final hydra.langs.scala.meta.Type decltpe;
  
  public Decl_Val (java.util.List<hydra.langs.scala.meta.Mod> mods, java.util.List<hydra.langs.scala.meta.Pat> pats, hydra.langs.scala.meta.Type decltpe) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    if (pats == null) {
      throw new IllegalArgumentException("null value for 'pats' argument");
    }
    if (decltpe == null) {
      throw new IllegalArgumentException("null value for 'decltpe' argument");
    }
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
  
  public Decl_Val withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    return new Decl_Val(mods, pats, decltpe);
  }
  
  public Decl_Val withPats(java.util.List<hydra.langs.scala.meta.Pat> pats) {
    if (pats == null) {
      throw new IllegalArgumentException("null value for 'pats' argument");
    }
    return new Decl_Val(mods, pats, decltpe);
  }
  
  public Decl_Val withDecltpe(hydra.langs.scala.meta.Type decltpe) {
    if (decltpe == null) {
      throw new IllegalArgumentException("null value for 'decltpe' argument");
    }
    return new Decl_Val(mods, pats, decltpe);
  }
}