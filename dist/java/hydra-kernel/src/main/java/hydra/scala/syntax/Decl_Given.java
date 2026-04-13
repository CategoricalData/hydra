// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Decl_Given implements Serializable, Comparable<Decl_Given> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Decl_Given");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name SPARAMS = new hydra.core.Name("sparams");

  public static final hydra.core.Name DECLTPE = new hydra.core.Name("decltpe");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final hydra.scala.syntax.Data_Name name;

  public final java.util.List<hydra.scala.syntax.Type_Param> tparams;

  public final java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> sparams;

  public final hydra.scala.syntax.Type decltpe;

  public Decl_Given (java.util.List<hydra.scala.syntax.Mod> mods, hydra.scala.syntax.Data_Name name, java.util.List<hydra.scala.syntax.Type_Param> tparams, java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> sparams, hydra.scala.syntax.Type decltpe) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.sparams = sparams;
    this.decltpe = decltpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Decl_Given)) {
      return false;
    }
    Decl_Given o = (Decl_Given) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.sparams,
      o.sparams) && java.util.Objects.equals(
      this.decltpe,
      o.decltpe);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(sparams) + 11 * java.util.Objects.hashCode(decltpe);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Decl_Given other) {
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
      decltpe,
      other.decltpe);
  }

  public Decl_Given withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }

  public Decl_Given withName(hydra.scala.syntax.Data_Name name) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }

  public Decl_Given withTparams(java.util.List<hydra.scala.syntax.Type_Param> tparams) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }

  public Decl_Given withSparams(java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> sparams) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }

  public Decl_Given withDecltpe(hydra.scala.syntax.Type decltpe) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }
}
