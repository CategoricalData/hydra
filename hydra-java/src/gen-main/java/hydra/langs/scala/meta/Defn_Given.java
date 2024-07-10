// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_Given implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.Given");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Name name;
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Type_Param>> tparams;
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> sparams;
  
  public final hydra.langs.scala.meta.Template templ;
  
  public Defn_Given (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Name name, java.util.List<java.util.List<hydra.langs.scala.meta.Type_Param>> tparams, java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> sparams, hydra.langs.scala.meta.Template templ) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    if (sparams == null) {
      throw new IllegalArgumentException("null value for 'sparams' argument");
    }
    if (templ == null) {
      throw new IllegalArgumentException("null value for 'templ' argument");
    }
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
  
  public Defn_Given withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withName(hydra.langs.scala.meta.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withTparams(java.util.List<java.util.List<hydra.langs.scala.meta.Type_Param>> tparams) {
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withSparams(java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> sparams) {
    if (sparams == null) {
      throw new IllegalArgumentException("null value for 'sparams' argument");
    }
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withTempl(hydra.langs.scala.meta.Template templ) {
    if (templ == null) {
      throw new IllegalArgumentException("null value for 'templ' argument");
    }
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
}