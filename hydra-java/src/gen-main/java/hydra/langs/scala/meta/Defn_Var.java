// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_Var implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.Var");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final java.util.List<hydra.langs.scala.meta.Pat> pats;
  
  public final hydra.langs.scala.meta.Type decltpe;
  
  public final java.util.Optional<hydra.langs.scala.meta.Data> rhs;
  
  public Defn_Var (java.util.List<hydra.langs.scala.meta.Mod> mods, java.util.List<hydra.langs.scala.meta.Pat> pats, hydra.langs.scala.meta.Type decltpe, java.util.Optional<hydra.langs.scala.meta.Data> rhs) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    if (pats == null) {
      throw new IllegalArgumentException("null value for 'pats' argument");
    }
    if (decltpe == null) {
      throw new IllegalArgumentException("null value for 'decltpe' argument");
    }
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    this.mods = mods;
    this.pats = pats;
    this.decltpe = decltpe;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Var)) {
      return false;
    }
    Defn_Var o = (Defn_Var) (other);
    return mods.equals(o.mods) && pats.equals(o.pats) && decltpe.equals(o.decltpe) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * pats.hashCode() + 5 * decltpe.hashCode() + 7 * rhs.hashCode();
  }
  
  public Defn_Var withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withPats(java.util.List<hydra.langs.scala.meta.Pat> pats) {
    if (pats == null) {
      throw new IllegalArgumentException("null value for 'pats' argument");
    }
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withDecltpe(hydra.langs.scala.meta.Type decltpe) {
    if (decltpe == null) {
      throw new IllegalArgumentException("null value for 'decltpe' argument");
    }
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withRhs(java.util.Optional<hydra.langs.scala.meta.Data> rhs) {
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
}