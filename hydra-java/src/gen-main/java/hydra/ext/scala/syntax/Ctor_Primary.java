// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Ctor_Primary implements Serializable, Comparable<Ctor_Primary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Ctor_Primary");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name PARAMSS = new hydra.core.Name("paramss");

  public final java.util.List<hydra.ext.scala.syntax.Mod> mods;

  public final hydra.ext.scala.syntax.Name name;

  public final java.util.List<java.util.List<hydra.ext.scala.syntax.Data_Param>> paramss;

  public Ctor_Primary (java.util.List<hydra.ext.scala.syntax.Mod> mods, hydra.ext.scala.syntax.Name name, java.util.List<java.util.List<hydra.ext.scala.syntax.Data_Param>> paramss) {
    this.mods = mods;
    this.name = name;
    this.paramss = paramss;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ctor_Primary)) {
      return false;
    }
    Ctor_Primary o = (Ctor_Primary) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.paramss,
      o.paramss);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(paramss);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Ctor_Primary other) {
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
      paramss,
      other.paramss);
  }

  public Ctor_Primary withMods(java.util.List<hydra.ext.scala.syntax.Mod> mods) {
    return new Ctor_Primary(mods, name, paramss);
  }

  public Ctor_Primary withName(hydra.ext.scala.syntax.Name name) {
    return new Ctor_Primary(mods, name, paramss);
  }

  public Ctor_Primary withParamss(java.util.List<java.util.List<hydra.ext.scala.syntax.Data_Param>> paramss) {
    return new Ctor_Primary(mods, name, paramss);
  }
}
