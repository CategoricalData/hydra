// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Data_Param implements Serializable, Comparable<Data_Param> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data_Param");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name DECLTPE = new hydra.core.Name("decltpe");

  public static final hydra.core.Name DEFAULT = new hydra.core.Name("default");

  public final java.util.List<hydra.scala.syntax.Mod> mods;

  public final hydra.scala.syntax.Name name;

  public final hydra.util.Maybe<hydra.scala.syntax.Type> decltpe;

  public final hydra.util.Maybe<hydra.scala.syntax.Data> default_;

  public Data_Param (java.util.List<hydra.scala.syntax.Mod> mods, hydra.scala.syntax.Name name, hydra.util.Maybe<hydra.scala.syntax.Type> decltpe, hydra.util.Maybe<hydra.scala.syntax.Data> default_) {
    this.mods = mods;
    this.name = name;
    this.decltpe = decltpe;
    this.default_ = default_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Param)) {
      return false;
    }
    Data_Param o = (Data_Param) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.decltpe,
      o.decltpe) && java.util.Objects.equals(
      this.default_,
      o.default_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(decltpe) + 7 * java.util.Objects.hashCode(default_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Param other) {
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
      decltpe,
      other.decltpe);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      default_,
      other.default_);
  }

  public Data_Param withMods(java.util.List<hydra.scala.syntax.Mod> mods) {
    return new Data_Param(mods, name, decltpe, default_);
  }

  public Data_Param withName(hydra.scala.syntax.Name name) {
    return new Data_Param(mods, name, decltpe, default_);
  }

  public Data_Param withDecltpe(hydra.util.Maybe<hydra.scala.syntax.Type> decltpe) {
    return new Data_Param(mods, name, decltpe, default_);
  }

  public Data_Param withDefault(hydra.util.Maybe<hydra.scala.syntax.Data> default_) {
    return new Data_Param(mods, name, decltpe, default_);
  }
}
