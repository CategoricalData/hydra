// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Ctor_Secondary implements Serializable, Comparable<Ctor_Secondary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Ctor_Secondary");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name PARAMSS = new hydra.core.Name("paramss");

  public static final hydra.core.Name INIT = new hydra.core.Name("init");

  public static final hydra.core.Name STATS = new hydra.core.Name("stats");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final hydra.scala.syntax.Name name;

  public final java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss;

  public final hydra.scala.syntax.Init init;

  public final java.util.List<hydra.scala.syntax.Stat> stats;

  public Ctor_Secondary (java.util.List<hydra.scala.syntax.Mod> mods, hydra.scala.syntax.Name name, java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss, hydra.scala.syntax.Init init, java.util.List<hydra.scala.syntax.Stat> stats) {
    this.mods = mods;
    this.name = name;
    this.paramss = paramss;
    this.init = init;
    this.stats = stats;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ctor_Secondary)) {
      return false;
    }
    Ctor_Secondary o = (Ctor_Secondary) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.paramss,
      o.paramss) && java.util.Objects.equals(
      this.init,
      o.init) && java.util.Objects.equals(
      this.stats,
      o.stats);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(paramss) + 7 * java.util.Objects.hashCode(init) + 11 * java.util.Objects.hashCode(stats);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Ctor_Secondary other) {
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
      paramss,
      other.paramss);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      init,
      other.init);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      stats,
      other.stats);
  }

  public Ctor_Secondary withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }

  public Ctor_Secondary withName(hydra.scala.syntax.Name name) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }

  public Ctor_Secondary withParamss(java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }

  public Ctor_Secondary withInit(hydra.scala.syntax.Init init) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }

  public Ctor_Secondary withStats(java.util.List<hydra.scala.syntax.Stat> stats) {
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
}
