package hydra.ext.scala.meta;

public class Data_Param {
  public final java.util.List<Mod> mods;
  
  public final Name name;
  
  public final java.util.Optional<Type> decltpe;
  
  public final java.util.Optional<Data> default_;
  
  public Data_Param (java.util.List<Mod> mods, Name name, java.util.Optional<Type> decltpe, java.util.Optional<Data> default_) {
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
  
  public Data_Param withMods(java.util.List<Mod> mods) {
    return new Data_Param(mods, name, decltpe, default_);
  }
  
  public Data_Param withName(Name name) {
    return new Data_Param(mods, name, decltpe, default_);
  }
  
  public Data_Param withDecltpe(java.util.Optional<Type> decltpe) {
    return new Data_Param(mods, name, decltpe, default_);
  }
  
  public Data_Param withDefault(java.util.Optional<Data> default_) {
    return new Data_Param(mods, name, decltpe, default_);
  }
}