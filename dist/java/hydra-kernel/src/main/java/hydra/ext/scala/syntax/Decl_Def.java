// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Decl_Def implements Serializable, Comparable<Decl_Def> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Decl_Def");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name PARAMSS = new hydra.core.Name("paramss");

  public static final hydra.core.Name DECLTPE = new hydra.core.Name("decltpe");

  public final java.util.List<hydra.ext.scala.syntax.Mod> mods;

  public final hydra.ext.scala.syntax.Data_Name name;

  public final java.util.List<hydra.ext.scala.syntax.Type_Param> tparams;

  public final java.util.List<java.util.List<hydra.ext.scala.syntax.Data_Param>> paramss;

  public final hydra.ext.scala.syntax.Type decltpe;

  public Decl_Def (java.util.List<hydra.ext.scala.syntax.Mod> mods, hydra.ext.scala.syntax.Data_Name name, java.util.List<hydra.ext.scala.syntax.Type_Param> tparams, java.util.List<java.util.List<hydra.ext.scala.syntax.Data_Param>> paramss, hydra.ext.scala.syntax.Type decltpe) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.paramss = paramss;
    this.decltpe = decltpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Decl_Def)) {
      return false;
    }
    Decl_Def o = (Decl_Def) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.paramss,
      o.paramss) && java.util.Objects.equals(
      this.decltpe,
      o.decltpe);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(paramss) + 11 * java.util.Objects.hashCode(decltpe);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Decl_Def other) {
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
      paramss,
      other.paramss);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      decltpe,
      other.decltpe);
  }

  public Decl_Def withMods(java.util.List<hydra.ext.scala.syntax.Mod> mods) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }

  public Decl_Def withName(hydra.ext.scala.syntax.Data_Name name) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }

  public Decl_Def withTparams(java.util.List<hydra.ext.scala.syntax.Type_Param> tparams) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }

  public Decl_Def withParamss(java.util.List<java.util.List<hydra.ext.scala.syntax.Data_Param>> paramss) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }

  public Decl_Def withDecltpe(hydra.ext.scala.syntax.Type decltpe) {
    return new Decl_Def(mods, name, tparams, paramss, decltpe);
  }
}
