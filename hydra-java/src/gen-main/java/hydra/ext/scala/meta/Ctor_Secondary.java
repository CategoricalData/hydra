// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Ctor_Secondary implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Ctor_Secondary");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PARAMSS = new hydra.core.Name("paramss");
  
  public static final hydra.core.Name FIELD_NAME_INIT = new hydra.core.Name("init");
  
  public static final hydra.core.Name FIELD_NAME_STATS = new hydra.core.Name("stats");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final hydra.ext.scala.meta.Name name;
  
  public final java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> paramss;
  
  public final hydra.ext.scala.meta.Init init;
  
  public final java.util.List<hydra.ext.scala.meta.Stat> stats;
  
  public Ctor_Secondary (java.util.List<hydra.ext.scala.meta.Mod> mods, hydra.ext.scala.meta.Name name, java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> paramss, hydra.ext.scala.meta.Init init, java.util.List<hydra.ext.scala.meta.Stat> stats) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((paramss));
    java.util.Objects.requireNonNull((init));
    java.util.Objects.requireNonNull((stats));
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
    Ctor_Secondary o = (Ctor_Secondary) (other);
    return mods.equals(o.mods) && name.equals(o.name) && paramss.equals(o.paramss) && init.equals(o.init) && stats.equals(o.stats);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * paramss.hashCode() + 7 * init.hashCode() + 11 * stats.hashCode();
  }
  
  public Ctor_Secondary withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
  
  public Ctor_Secondary withName(hydra.ext.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
  
  public Ctor_Secondary withParamss(java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> paramss) {
    java.util.Objects.requireNonNull((paramss));
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
  
  public Ctor_Secondary withInit(hydra.ext.scala.meta.Init init) {
    java.util.Objects.requireNonNull((init));
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
  
  public Ctor_Secondary withStats(java.util.List<hydra.ext.scala.meta.Stat> stats) {
    java.util.Objects.requireNonNull((stats));
    return new Ctor_Secondary(mods, name, paramss, init, stats);
  }
}