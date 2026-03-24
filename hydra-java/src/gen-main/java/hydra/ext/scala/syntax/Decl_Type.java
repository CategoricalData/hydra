// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Decl_Type implements Serializable, Comparable<Decl_Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Decl_Type");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name BOUNDS = new hydra.core.Name("bounds");

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods;

  public final hydra.ext.scala.syntax.Type_Name name;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param> tparams;

  public final hydra.ext.scala.syntax.TypeBounds bounds;

  public Decl_Type (hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods, hydra.ext.scala.syntax.Type_Name name, hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param> tparams, hydra.ext.scala.syntax.TypeBounds bounds) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.bounds = bounds;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Decl_Type)) {
      return false;
    }
    Decl_Type o = (Decl_Type) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.bounds,
      o.bounds);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(bounds);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Decl_Type other) {
    int cmp = 0;
    cmp = ((Comparable) mods).compareTo(other.mods);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) tparams).compareTo(other.tparams);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) bounds).compareTo(other.bounds);
  }

  public Decl_Type withMods(hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods) {
    return new Decl_Type(mods, name, tparams, bounds);
  }

  public Decl_Type withName(hydra.ext.scala.syntax.Type_Name name) {
    return new Decl_Type(mods, name, tparams, bounds);
  }

  public Decl_Type withTparams(hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param> tparams) {
    return new Decl_Type(mods, name, tparams, bounds);
  }

  public Decl_Type withBounds(hydra.ext.scala.syntax.TypeBounds bounds) {
    return new Decl_Type(mods, name, tparams, bounds);
  }
}
