// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Defn_GivenAlias implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Defn_GivenAlias");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_SPARAMS = new hydra.core.Name("sparams");
  
  public static final hydra.core.Name FIELD_NAME_DECLTPE = new hydra.core.Name("decltpe");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final hydra.ext.scala.meta.Name name;
  
  public final java.util.List<java.util.List<hydra.ext.scala.meta.Type_Param>> tparams;
  
  public final java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> sparams;
  
  public final hydra.ext.scala.meta.Type decltpe;
  
  public final hydra.ext.scala.meta.Data body;
  
  public Defn_GivenAlias (java.util.List<hydra.ext.scala.meta.Mod> mods, hydra.ext.scala.meta.Name name, java.util.List<java.util.List<hydra.ext.scala.meta.Type_Param>> tparams, java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> sparams, hydra.ext.scala.meta.Type decltpe, hydra.ext.scala.meta.Data body) {
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
  
  public Defn_GivenAlias withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withName(hydra.ext.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withTparams(java.util.List<java.util.List<hydra.ext.scala.meta.Type_Param>> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withSparams(java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> sparams) {
    java.util.Objects.requireNonNull((sparams));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withDecltpe(hydra.ext.scala.meta.Type decltpe) {
    java.util.Objects.requireNonNull((decltpe));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withBody(hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((body));
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
}