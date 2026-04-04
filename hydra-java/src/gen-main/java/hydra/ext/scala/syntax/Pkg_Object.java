// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Pkg_Object implements Serializable, Comparable<Pkg_Object> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Pkg_Object");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TEMPLATE = new hydra.core.Name("template");

  public final java.util.List<hydra.ext.scala.syntax.Mod> mods;

  public final hydra.ext.scala.syntax.Data_Name name;

  public final hydra.ext.scala.syntax.Template template;

  public Pkg_Object (java.util.List<hydra.ext.scala.syntax.Mod> mods, hydra.ext.scala.syntax.Data_Name name, hydra.ext.scala.syntax.Template template) {
    this.mods = mods;
    this.name = name;
    this.template = template;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pkg_Object)) {
      return false;
    }
    Pkg_Object o = (Pkg_Object) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.template,
      o.template);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(template);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pkg_Object other) {
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
    return hydra.util.Comparing.compare(
      template,
      other.template);
  }

  public Pkg_Object withMods(java.util.List<hydra.ext.scala.syntax.Mod> mods) {
    return new Pkg_Object(mods, name, template);
  }

  public Pkg_Object withName(hydra.ext.scala.syntax.Data_Name name) {
    return new Pkg_Object(mods, name, template);
  }

  public Pkg_Object withTemplate(hydra.ext.scala.syntax.Template template) {
    return new Pkg_Object(mods, name, template);
  }
}
