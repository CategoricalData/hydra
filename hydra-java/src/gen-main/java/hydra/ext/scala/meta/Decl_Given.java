// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Decl_Given implements Serializable, Comparable<Decl_Given> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Decl_Given");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name SPARAMS = new hydra.core.Name("sparams");

  public static final hydra.core.Name DECLTPE = new hydra.core.Name("decltpe");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Mod> mods;

  public final hydra.ext.scala.meta.Data_Name name;

  public final hydra.util.ConsList<hydra.ext.scala.meta.Type_Param> tparams;

  public final hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>> sparams;

  public final hydra.ext.scala.meta.Type decltpe;

  public Decl_Given (hydra.util.ConsList<hydra.ext.scala.meta.Mod> mods, hydra.ext.scala.meta.Data_Name name, hydra.util.ConsList<hydra.ext.scala.meta.Type_Param> tparams, hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>> sparams, hydra.ext.scala.meta.Type decltpe) {
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
    return ((Comparable) decltpe).compareTo(other.decltpe);
  }

  public Decl_Given withMods(hydra.util.ConsList<hydra.ext.scala.meta.Mod> mods) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }

  public Decl_Given withName(hydra.ext.scala.meta.Data_Name name) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }

  public Decl_Given withTparams(hydra.util.ConsList<hydra.ext.scala.meta.Type_Param> tparams) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }

  public Decl_Given withSparams(hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>> sparams) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }

  public Decl_Given withDecltpe(hydra.ext.scala.meta.Type decltpe) {
    return new Decl_Given(mods, name, tparams, sparams, decltpe);
  }
}
