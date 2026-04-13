// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Defn_GivenAlias implements Serializable, Comparable<Defn_GivenAlias> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Defn_GivenAlias");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name SPARAMS = new hydra.core.Name("sparams");

  public static final hydra.core.Name DECLTPE = new hydra.core.Name("decltpe");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final hydra.scala.syntax.Name name;

  public final java.util.List<java.util.List<hydra.scala.syntax.Type_Param>> tparams;

  public final java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> sparams;

  public final hydra.scala.syntax.Type decltpe;

  public final hydra.scala.syntax.Data body;

  public Defn_GivenAlias (java.util.List<hydra.scala.syntax.Mod> mods, hydra.scala.syntax.Name name, java.util.List<java.util.List<hydra.scala.syntax.Type_Param>> tparams, java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> sparams, hydra.scala.syntax.Type decltpe, hydra.scala.syntax.Data body) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.sparams = sparams;
    this.decltpe = decltpe;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_GivenAlias)) {
      return false;
    }
    Defn_GivenAlias o = (Defn_GivenAlias) other;
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
      o.decltpe) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(sparams) + 11 * java.util.Objects.hashCode(decltpe) + 13 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Defn_GivenAlias other) {
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

  public Defn_GivenAlias withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withName(hydra.scala.syntax.Name name) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withTparams(java.util.List<java.util.List<hydra.scala.syntax.Type_Param>> tparams) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withSparams(java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> sparams) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withDecltpe(hydra.scala.syntax.Type decltpe) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withBody(hydra.scala.syntax.Data body) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
}
