// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Defn_Type implements Serializable, Comparable<Defn_Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Defn_Type");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final hydra.scala.syntax.Type_Name name;

  public final java.util.List<hydra.scala.syntax.Type_Param> tparams;

  public final hydra.scala.syntax.Type body;

  public Defn_Type (java.util.List<hydra.scala.syntax.Mod> mods, hydra.scala.syntax.Type_Name name, java.util.List<hydra.scala.syntax.Type_Param> tparams, hydra.scala.syntax.Type body) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Type)) {
      return false;
    }
    Defn_Type o = (Defn_Type) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Defn_Type other) {
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
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public Defn_Type withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Defn_Type(mods, name, tparams, body);
  }

  public Defn_Type withName(hydra.scala.syntax.Type_Name name) {
    return new Defn_Type(mods, name, tparams, body);
  }

  public Defn_Type withTparams(java.util.List<hydra.scala.syntax.Type_Param> tparams) {
    return new Defn_Type(mods, name, tparams, body);
  }

  public Defn_Type withBody(hydra.scala.syntax.Type body) {
    return new Defn_Type(mods, name, tparams, body);
  }
}
