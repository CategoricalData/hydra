// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_Param implements Serializable, Comparable<Type_Param> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_Param");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name TBOUNDS = new hydra.core.Name("tbounds");

  public static final hydra.core.Name VBOUNDS = new hydra.core.Name("vbounds");

  public static final hydra.core.Name CBOUNDS = new hydra.core.Name("cbounds");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final hydra.scala.syntax.Name name;

  public final java.util.List<hydra.scala.syntax.Type_Param> tparams;

  public final java.util.List<hydra.scala.syntax.TypeBounds> tbounds;

  public final java.util.List<hydra.scala.syntax.Type> vbounds;

  public final java.util.List<hydra.scala.syntax.Type> cbounds;

  public Type_Param (java.util.List<hydra.scala.syntax.Mod> mods, hydra.scala.syntax.Name name, java.util.List<hydra.scala.syntax.Type_Param> tparams, java.util.List<hydra.scala.syntax.TypeBounds> tbounds, java.util.List<hydra.scala.syntax.Type> vbounds, java.util.List<hydra.scala.syntax.Type> cbounds) {
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
    cmp = hydra.util.Comparing.compare(
      mods,
      other.mods);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      tparams,
      other.tparams);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      tbounds,
      other.tbounds);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      vbounds,
      other.vbounds);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      cbounds,
      other.cbounds);
  }

  public Type_Param withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withName(hydra.scala.syntax.Name name) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withTparams(java.util.List<hydra.scala.syntax.Type_Param> tparams) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withTbounds(java.util.List<hydra.scala.syntax.TypeBounds> tbounds) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withVbounds(java.util.List<hydra.scala.syntax.Type> vbounds) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }

  public Type_Param withCbounds(java.util.List<hydra.scala.syntax.Type> cbounds) {
    return new Type_Param(mods, name, tparams, tbounds, vbounds, cbounds);
  }
}
