// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Defn_Class implements Serializable, Comparable<Defn_Class> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Defn_Class");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name CTOR = new hydra.core.Name("ctor");

  public static final hydra.core.Name TEMPLATE = new hydra.core.Name("template");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final hydra.scala.syntax.Type_Name name;

  public final java.util.List<hydra.scala.syntax.Type_Param> tparams;

  public final hydra.scala.syntax.Ctor_Primary ctor;

  public final hydra.scala.syntax.Template template;

  public Defn_Class (java.util.List<hydra.scala.syntax.Mod> mods, hydra.scala.syntax.Type_Name name, java.util.List<hydra.scala.syntax.Type_Param> tparams, hydra.scala.syntax.Ctor_Primary ctor, hydra.scala.syntax.Template template) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.ctor = ctor;
    this.template = template;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Class)) {
      return false;
    }
    Defn_Class o = (Defn_Class) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.ctor,
      o.ctor) && java.util.Objects.equals(
      this.template,
      o.template);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(tparams) + 7 * java.util.Objects.hashCode(ctor) + 11 * java.util.Objects.hashCode(template);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Defn_Class other) {
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
      ctor,
      other.ctor);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      template,
      other.template);
  }

  public Defn_Class withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }

  public Defn_Class withName(hydra.scala.syntax.Type_Name name) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }

  public Defn_Class withTparams(java.util.List<hydra.scala.syntax.Type_Param> tparams) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }

  public Defn_Class withCtor(hydra.scala.syntax.Ctor_Primary ctor) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }

  public Defn_Class withTemplate(hydra.scala.syntax.Template template) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
}
