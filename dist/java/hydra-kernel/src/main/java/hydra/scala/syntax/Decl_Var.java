// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Decl_Var implements Serializable, Comparable<Decl_Var> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Decl_Var");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name PATS = new hydra.core.Name("pats");

  public static final hydra.core.Name DECLTPE = new hydra.core.Name("decltpe");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final java.util.List<hydra.scala.syntax.Pat> pats;

  public final hydra.scala.syntax.Type decltpe;

  public Decl_Var (java.util.List<hydra.scala.syntax.Mod> mods, java.util.List<hydra.scala.syntax.Pat> pats, hydra.scala.syntax.Type decltpe) {
    this.mods = mods;
    this.pats = pats;
    this.decltpe = decltpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Decl_Var)) {
      return false;
    }
    Decl_Var o = (Decl_Var) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.pats,
      o.pats) && java.util.Objects.equals(
      this.decltpe,
      o.decltpe);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(pats) + 5 * java.util.Objects.hashCode(decltpe);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Decl_Var other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      mods,
      other.mods);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      pats,
      other.pats);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      decltpe,
      other.decltpe);
  }

  public Decl_Var withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Decl_Var(mods, pats, decltpe);
  }

  public Decl_Var withPats(java.util.List<hydra.scala.syntax.Pat> pats) {
    return new Decl_Var(mods, pats, decltpe);
  }

  public Decl_Var withDecltpe(hydra.scala.syntax.Type decltpe) {
    return new Decl_Var(mods, pats, decltpe);
  }
}
