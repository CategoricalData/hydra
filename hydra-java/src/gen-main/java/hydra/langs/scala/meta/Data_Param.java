// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Data_Param implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Data.Param");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Name name;
  
  public final java.util.Optional<hydra.langs.scala.meta.Type> decltpe;
  
  public final java.util.Optional<hydra.langs.scala.meta.Data> default_;
  
  public Data_Param (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Name name, java.util.Optional<hydra.langs.scala.meta.Type> decltpe, java.util.Optional<hydra.langs.scala.meta.Data> default_) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (decltpe == null) {
      throw new IllegalArgumentException("null value for 'decltpe' argument");
    }
    if (default_ == null) {
      throw new IllegalArgumentException("null value for 'default' argument");
    }
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
    Data_Param o = (Data_Param) (other);
    return mods.equals(o.mods) && name.equals(o.name) && decltpe.equals(o.decltpe) && default_.equals(o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * decltpe.hashCode() + 7 * default_.hashCode();
  }
  
  public Data_Param withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    return new Data_Param(mods, name, decltpe, default_);
  }
  
  public Data_Param withName(hydra.langs.scala.meta.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Data_Param(mods, name, decltpe, default_);
  }
  
  public Data_Param withDecltpe(java.util.Optional<hydra.langs.scala.meta.Type> decltpe) {
    if (decltpe == null) {
      throw new IllegalArgumentException("null value for 'decltpe' argument");
    }
    return new Data_Param(mods, name, decltpe, default_);
  }
  
  public Data_Param withDefault(java.util.Optional<hydra.langs.scala.meta.Data> default_) {
    if (default_ == null) {
      throw new IllegalArgumentException("null value for 'default' argument");
    }
    return new Data_Param(mods, name, decltpe, default_);
  }
}