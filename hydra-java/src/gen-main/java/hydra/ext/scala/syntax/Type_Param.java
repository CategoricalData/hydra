// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Type_Param implements Serializable, Comparable<Type_Param> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Type_Param");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name TBOUNDS = new hydra.core.Name("tbounds");

  public static final hydra.core.Name VBOUNDS = new hydra.core.Name("vbounds");

  public static final hydra.core.Name CBOUNDS = new hydra.core.Name("cbounds");

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods;

  public final hydra.ext.scala.syntax.Name name;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param> tparams;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.TypeBounds> tbounds;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Type> vbounds;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Type> cbounds;

  public Type_Param (hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods, hydra.ext.scala.syntax.Name name, hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param> tparams, hydra.util.ConsList<hydra.ext.scala.syntax.TypeBounds> tbounds, hydra.util.ConsList<hydra.ext.scala.syntax.Type> vbounds, hydra.util.ConsList<hydra.ext.scala.syntax.Type> cbounds) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.tbounds = tbounds;
    this.vbounds = vbounds;
    this.cbounds = cbounds;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Param)) {
      return false;
    }
    Type_Param o = (Type_Param) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.tbounds,
      o.tbounds) && java.util.Objects.equals(
      this.vbounds,
      o.vbounds) && java.util.Objects.equals(
      this.cbounds,
      o.cbounds);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(tbounds) + 11 * java.util.Objects.hashCode(vbounds) + 13 * java.util.Objects.hashCode(cbounds);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Param other) {
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
    cmp = ((Comparable) tbounds).compareTo(other.tbounds);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) vbounds).compareTo(other.vbounds);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) cbounds).compareTo(other.cbounds);
  }

  public Type_Param withMods(hydra.util.ConsList<hydra.ext.scala.syntax.Mod> mods) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withName(hydra.ext.scala.syntax.Name name) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withTparams(hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param> tparams) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withTbounds(hydra.util.ConsList<hydra.ext.scala.syntax.TypeBounds> tbounds) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withVbounds(hydra.util.ConsList<hydra.ext.scala.syntax.Type> vbounds) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withCbounds(hydra.util.ConsList<hydra.ext.scala.syntax.Type> cbounds) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
}
