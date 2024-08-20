// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Defn_Given implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Defn.Given");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_SPARAMS = new hydra.core.Name("sparams");
  
  public static final hydra.core.Name FIELD_NAME_TEMPL = new hydra.core.Name("templ");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final hydra.ext.scala.meta.Name name;
  
  public final java.util.List<java.util.List<hydra.ext.scala.meta.Type_Param>> tparams;
  
  public final java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> sparams;
  
  public final hydra.ext.scala.meta.Template templ;
  
  public Defn_Given (java.util.List<hydra.ext.scala.meta.Mod> mods, hydra.ext.scala.meta.Name name, java.util.List<java.util.List<hydra.ext.scala.meta.Type_Param>> tparams, java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> sparams, hydra.ext.scala.meta.Template templ) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((sparams));
    java.util.Objects.requireNonNull((templ));
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.sparams = sparams;
    this.templ = templ;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Given)) {
      return false;
    }
    Defn_Given o = (Defn_Given) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && sparams.equals(o.sparams) && templ.equals(o.templ);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * sparams.hashCode() + 11 * templ.hashCode();
  }
  
  public Defn_Given withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withName(hydra.ext.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withTparams(java.util.List<java.util.List<hydra.ext.scala.meta.Type_Param>> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withSparams(java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> sparams) {
    java.util.Objects.requireNonNull((sparams));
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withTempl(hydra.ext.scala.meta.Template templ) {
    java.util.Objects.requireNonNull((templ));
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
}
