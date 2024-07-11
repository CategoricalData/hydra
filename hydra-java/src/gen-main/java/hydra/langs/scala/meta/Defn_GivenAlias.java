// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_GivenAlias implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.GivenAlias");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Name name;
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Type_Param>> tparams;
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> sparams;
  
  public final hydra.langs.scala.meta.Type decltpe;
  
  public final hydra.langs.scala.meta.Data body;
  
  public Defn_GivenAlias (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Name name, java.util.List<java.util.List<hydra.langs.scala.meta.Type_Param>> tparams, java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> sparams, hydra.langs.scala.meta.Type decltpe, hydra.langs.scala.meta.Data body) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((sparams));
    java.util.Objects.requireNonNull((decltpe));
    java.util.Objects.requireNonNull((body));
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.sparams = sparams;
    this.decltpe = decltpe;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_GivenAlias)) {
      return false;
    }
    Defn_GivenAlias o = (Defn_GivenAlias) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && sparams.equals(o.sparams) && decltpe.equals(o.decltpe) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * sparams.hashCode() + 11 * decltpe.hashCode() + 13 * body.hashCode();
  }
  
  public Defn_GivenAlias withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withName(hydra.langs.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withTparams(java.util.List<java.util.List<hydra.langs.scala.meta.Type_Param>> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withSparams(java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> sparams) {
    java.util.Objects.requireNonNull((sparams));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withDecltpe(hydra.langs.scala.meta.Type decltpe) {
    java.util.Objects.requireNonNull((decltpe));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withBody(hydra.langs.scala.meta.Data body) {
    java.util.Objects.requireNonNull((body));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
}