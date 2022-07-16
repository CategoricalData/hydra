package hydra.ext.scala.meta;

public class Defn_Val {
  public final java.util.List<Mod> mods;
  
  public final java.util.List<Pat> pats;
  
  public final java.util.Optional<Type> decltpe;
  
  public final Data rhs;
  
  public Defn_Val (java.util.List<Mod> mods, java.util.List<Pat> pats, java.util.Optional<Type> decltpe, Data rhs) {
    this.mods = mods;
    this.pats = pats;
    this.decltpe = decltpe;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Val)) {
      return false;
    }
    Defn_Val o = (Defn_Val) (other);
    return mods.equals(o.mods) && pats.equals(o.pats) && decltpe.equals(o.decltpe) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * pats.hashCode() + 5 * decltpe.hashCode() + 7 * rhs.hashCode();
  }
  
  public Defn_Val withMods(java.util.List<Mod> mods) {
    return new Defn_Val(mods, pats, decltpe, rhs);
  }
  
  public Defn_Val withPats(java.util.List<Pat> pats) {
    return new Defn_Val(mods, pats, decltpe, rhs);
  }
  
  public Defn_Val withDecltpe(java.util.Optional<Type> decltpe) {
    return new Defn_Val(mods, pats, decltpe, rhs);
  }
  
  public Defn_Val withRhs(Data rhs) {
    return new Defn_Val(mods, pats, decltpe, rhs);
  }
}