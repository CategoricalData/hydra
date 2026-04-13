// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Defn_Macro implements Serializable, Comparable<Defn_Macro> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Defn_Macro");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name PARAMSS = new hydra.core.Name("paramss");

  public static final hydra.core.Name DECLTPE = new hydra.core.Name("decltpe");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final hydra.scala.syntax.Data_Name name;

  public final java.util.List<hydra.scala.syntax.Type_Param> tparams;

  public final java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss;

  public final hydra.util.Maybe<hydra.scala.syntax.Type> decltpe;

  public final hydra.scala.syntax.Data body;

  public Defn_Macro (java.util.List<hydra.scala.syntax.Mod> mods, hydra.scala.syntax.Data_Name name, java.util.List<hydra.scala.syntax.Type_Param> tparams, java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss, hydra.util.Maybe<hydra.scala.syntax.Type> decltpe, hydra.scala.syntax.Data body) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.paramss = paramss;
    this.decltpe = decltpe;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Macro)) {
      return false;
    }
    Defn_Macro o = (Defn_Macro) other;
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
      o.decltpe) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(paramss) + 11 * java.util.Objects.hashCode(decltpe) + 13 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Defn_Macro other) {
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
    cmp = hydra.util.Comparing.compare(
      decltpe,
      other.decltpe);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public Defn_Macro withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }

  public Defn_Macro withName(hydra.scala.syntax.Data_Name name) {
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }

  public Defn_Macro withTparams(java.util.List<hydra.scala.syntax.Type_Param> tparams) {
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }

  public Defn_Macro withParamss(java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss) {
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }

  public Defn_Macro withDecltpe(hydra.util.Maybe<hydra.scala.syntax.Type> decltpe) {
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }

  public Defn_Macro withBody(hydra.scala.syntax.Data body) {
    return new Defn_Macro(mods, name, tparams, paramss, decltpe, body);
  }
}
