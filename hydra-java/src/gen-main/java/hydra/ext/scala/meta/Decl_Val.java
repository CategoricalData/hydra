package hydra.ext.scala.meta;

public class Decl_Val {
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final java.util.List<hydra.ext.scala.meta.Pat> pats;
  
  public final hydra.ext.scala.meta.Type decltpe;
  
  public Decl_Val (java.util.List<hydra.ext.scala.meta.Mod> mods, java.util.List<hydra.ext.scala.meta.Pat> pats, hydra.ext.scala.meta.Type decltpe) {
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
    return new Decl_Val(mods, pats, decltpe);
  }
  
  public Decl_Val withPats(java.util.List<hydra.ext.scala.meta.Pat> pats) {
    return new Decl_Val(mods, pats, decltpe);
  }
  
  public Decl_Val withDecltpe(hydra.ext.scala.meta.Type decltpe) {
    return new Decl_Val(mods, pats, decltpe);
  }
}