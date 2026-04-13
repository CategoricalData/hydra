// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Defn_Given implements Serializable, Comparable<Defn_Given> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Defn_Given");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name SPARAMS = new hydra.core.Name("sparams");

  public static final hydra.core.Name TEMPL = new hydra.core.Name("templ");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final hydra.scala.syntax.Name name;

  public final java.util.List<java.util.List<hydra.scala.syntax.Type_Param>> tparams;

  public final java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> sparams;

  public final hydra.scala.syntax.Template templ;

  public Defn_Given (java.util.List<hydra.scala.syntax.Mod> mods, hydra.scala.syntax.Name name, java.util.List<java.util.List<hydra.scala.syntax.Type_Param>> tparams, java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> sparams, hydra.scala.syntax.Template templ) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.sparams = sparams;
    this.templ = templ;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Given)) {
      return false;
    }
    Defn_Given o = (Defn_Given) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.sparams,
      o.sparams) && java.util.Objects.equals(
      this.templ,
      o.templ);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(sparams) + 11 * java.util.Objects.hashCode(templ);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Defn_Given other) {
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
      sparams,
      other.sparams);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      templ,
      other.templ);
  }

  public Defn_Given withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }

  public Defn_Given withName(hydra.scala.syntax.Name name) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }

  public Defn_Given withTparams(java.util.List<java.util.List<hydra.scala.syntax.Type_Param>> tparams) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }

  public Defn_Given withSparams(java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> sparams) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }

  public Defn_Given withTempl(hydra.scala.syntax.Template templ) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
}
