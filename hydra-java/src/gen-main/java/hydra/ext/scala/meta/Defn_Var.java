// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Defn_Var implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Defn.Var");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_PATS = new hydra.core.Name("pats");
  
  public static final hydra.core.Name FIELD_NAME_DECLTPE = new hydra.core.Name("decltpe");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final java.util.List<hydra.ext.scala.meta.Pat> pats;
  
  public final hydra.ext.scala.meta.Type decltpe;
  
  public final hydra.util.Opt<hydra.ext.scala.meta.Data> rhs;
  
  public Defn_Var (java.util.List<hydra.ext.scala.meta.Mod> mods, java.util.List<hydra.ext.scala.meta.Pat> pats, hydra.ext.scala.meta.Type decltpe, hydra.util.Opt<hydra.ext.scala.meta.Data> rhs) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((pats));
    java.util.Objects.requireNonNull((decltpe));
    java.util.Objects.requireNonNull((rhs));
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
  
  public Defn_Var withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withPats(java.util.List<hydra.ext.scala.meta.Pat> pats) {
    java.util.Objects.requireNonNull((pats));
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withDecltpe(hydra.ext.scala.meta.Type decltpe) {
    java.util.Objects.requireNonNull((decltpe));
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withRhs(hydra.util.Opt<hydra.ext.scala.meta.Data> rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
}
