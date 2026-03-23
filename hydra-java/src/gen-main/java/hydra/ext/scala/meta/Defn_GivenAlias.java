// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Defn_GivenAlias implements Serializable, Comparable<Defn_GivenAlias> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Defn_GivenAlias");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name SPARAMS = new hydra.core.Name("sparams");

  public static final hydra.core.Name DECLTPE = new hydra.core.Name("decltpe");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Mod> mods;

  public final hydra.ext.scala.meta.Name name;

  public final hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>> tparams;

  public final hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>> sparams;

  public final hydra.ext.scala.meta.Type decltpe;

  public final hydra.ext.scala.meta.Data body;

  public Defn_GivenAlias (hydra.util.ConsList<hydra.ext.scala.meta.Mod> mods, hydra.ext.scala.meta.Name name, hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>> tparams, hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>> sparams, hydra.ext.scala.meta.Type decltpe, hydra.ext.scala.meta.Data body) {
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
    cmp = ((Comparable) sparams).compareTo(other.sparams);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) decltpe).compareTo(other.decltpe);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }

  public Defn_GivenAlias withMods(hydra.util.ConsList<hydra.ext.scala.meta.Mod> mods) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withName(hydra.ext.scala.meta.Name name) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withTparams(hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Type_Param>> tparams) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withSparams(hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>> sparams) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withDecltpe(hydra.ext.scala.meta.Type decltpe) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }

  public Defn_GivenAlias withBody(hydra.ext.scala.meta.Data body) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
}
