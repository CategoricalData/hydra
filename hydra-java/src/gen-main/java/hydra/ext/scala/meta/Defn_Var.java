package hydra.ext.scala.meta;

public class Defn_Var {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Defn.Var");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final java.util.List<hydra.ext.scala.meta.Pat> pats;
  
  public final hydra.ext.scala.meta.Type decltpe;
  
  public final java.util.Optional<hydra.ext.scala.meta.Data> rhs;
  
  public Defn_Var (java.util.List<hydra.ext.scala.meta.Mod> mods, java.util.List<hydra.ext.scala.meta.Pat> pats, hydra.ext.scala.meta.Type decltpe, java.util.Optional<hydra.ext.scala.meta.Data> rhs) {
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
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withPats(java.util.List<hydra.ext.scala.meta.Pat> pats) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withDecltpe(hydra.ext.scala.meta.Type decltpe) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withRhs(java.util.Optional<hydra.ext.scala.meta.Data> rhs) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
}