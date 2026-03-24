// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Defn_Var implements Serializable, Comparable<Defn_Var> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Defn_Var");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name PATS = new hydra.core.Name("pats");

  public static final hydra.core.Name DECLTPE = new hydra.core.Name("decltpe");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Pat> pats;

  public final hydra.ext.scala.syntax.Type decltpe;

  public final hydra.util.Maybe<hydra.ext.scala.syntax.Data> rhs;

  public Defn_Var (hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods, hydra.util.ConsList<hydra.ext.scala.syntax.Pat> pats, hydra.ext.scala.syntax.Type decltpe, hydra.util.Maybe<hydra.ext.scala.syntax.Data> rhs) {
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
    Defn_Var o = (Defn_Var) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.pats,
      o.pats) && java.util.Objects.equals(
      this.decltpe,
      o.decltpe) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(pats) + 5 * java.util.Objects.hashCode(decltpe) + 7 * java.util.Objects.hashCode(rhs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Defn_Var other) {
    int cmp = 0;
    cmp = ((Comparable) mods).compareTo(other.mods);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) pats).compareTo(other.pats);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) decltpe).compareTo(other.decltpe);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }

  public Defn_Var withMods(hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }

  public Defn_Var withPats(hydra.util.ConsList<hydra.ext.scala.syntax.Pat> pats) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }

  public Defn_Var withDecltpe(hydra.ext.scala.syntax.Type decltpe) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }

  public Defn_Var withRhs(hydra.util.Maybe<hydra.ext.scala.syntax.Data> rhs) {
    return new Defn_Var(mods, pats, decltpe, rhs);
  }
}
