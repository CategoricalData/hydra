// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Data_Param implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Data.Param");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Name name;
  
  public final hydra.util.Opt<hydra.langs.scala.meta.Type> decltpe;
  
  public final hydra.util.Opt<hydra.langs.scala.meta.Data> default_;
  
  public Data_Param (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Name name, hydra.util.Opt<hydra.langs.scala.meta.Type> decltpe, hydra.util.Opt<hydra.langs.scala.meta.Data> default_) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((decltpe));
    java.util.Objects.requireNonNull((default_));
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
    java.util.Objects.requireNonNull((mods));
    return new Data_Param(mods, name, decltpe, default_);
  }
  
  public Data_Param withName(hydra.langs.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Data_Param(mods, name, decltpe, default_);
  }
  
  public Data_Param withDecltpe(hydra.util.Opt<hydra.langs.scala.meta.Type> decltpe) {
    java.util.Objects.requireNonNull((decltpe));
    return new Data_Param(mods, name, decltpe, default_);
  }
  
  public Data_Param withDefault(hydra.util.Opt<hydra.langs.scala.meta.Data> default_) {
    java.util.Objects.requireNonNull((default_));
    return new Data_Param(mods, name, decltpe, default_);
  }
}