// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Defn_Trait implements Serializable, Comparable<Defn_Trait> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Defn_Trait");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name CTOR = new hydra.core.Name("ctor");

  public static final hydra.core.Name TEMPLATE = new hydra.core.Name("template");

  public final java.util.List<hydra.ext.scala.syntax.Mod> mods;

  public final hydra.ext.scala.syntax.Type_Name name;

  public final java.util.List<hydra.ext.scala.syntax.Type_Param> tparams;

  public final hydra.ext.scala.syntax.Ctor_Primary ctor;

  public final hydra.ext.scala.syntax.Template template;

  public Defn_Trait (java.util.List<hydra.ext.scala.syntax.Mod> mods, hydra.ext.scala.syntax.Type_Name name, java.util.List<hydra.ext.scala.syntax.Type_Param> tparams, hydra.ext.scala.syntax.Ctor_Primary ctor, hydra.ext.scala.syntax.Template template) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.ctor = ctor;
    this.template = template;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Trait)) {
      return false;
    }
    Defn_Trait o = (Defn_Trait) other;
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
  public int compareTo(Defn_Trait other) {
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

  public Defn_Trait withMods(java.util.List<hydra.ext.scala.syntax.Mod> mods) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }

  public Defn_Trait withName(hydra.ext.scala.syntax.Type_Name name) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }

  public Defn_Trait withTparams(java.util.List<hydra.ext.scala.syntax.Type_Param> tparams) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }

  public Defn_Trait withCtor(hydra.ext.scala.syntax.Ctor_Primary ctor) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }

  public Defn_Trait withTemplate(hydra.ext.scala.syntax.Template template) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }
}
