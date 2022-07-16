package hydra.ext.scala.meta;

public class Defn_Var {
  public final java.util.List<Mod> mods;
  
  public final java.util.List<Pat> pats;
  
  public final Type decltpe;
  
  public final java.util.Optional<Data> rhs;
  
  public Defn_Var (java.util.List<Mod> mods, java.util.List<Pat> pats, Type decltpe, java.util.Optional<Data> rhs) {
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
  
  public Defn_Var withMods(java.util.List<Mod> mods) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withPats(java.util.List<Pat> pats) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withDecltpe(Type decltpe) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
  
  public Defn_Var withRhs(java.util.Optional<Data> rhs) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
}