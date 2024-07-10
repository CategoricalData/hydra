// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_Enum implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.Enum");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Type_Name name;
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final hydra.langs.scala.meta.Ctor_Primary ctor;
  
  public final hydra.langs.scala.meta.Template template;
  
  public Defn_Enum (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Type_Name name, java.util.List<hydra.langs.scala.meta.Type_Param> tparams, hydra.langs.scala.meta.Ctor_Primary ctor, hydra.langs.scala.meta.Template template) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    if (ctor == null) {
      throw new IllegalArgumentException("null value for 'ctor' argument");
    }
    if (template == null) {
      throw new IllegalArgumentException("null value for 'template' argument");
    }
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.ctor = ctor;
    this.template = template;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Enum)) {
      return false;
    }
    Defn_Enum o = (Defn_Enum) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && ctor.equals(o.ctor) && template.equals(o.template);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * ctor.hashCode() + 11 * template.hashCode();
  }
  
  public Defn_Enum withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    return new Defn_Enum(mods, name, tparams, ctor, template);
  }
  
  public Defn_Enum withName(hydra.langs.scala.meta.Type_Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Defn_Enum(mods, name, tparams, ctor, template);
  }
  
  public Defn_Enum withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    return new Defn_Enum(mods, name, tparams, ctor, template);
  }
  
  public Defn_Enum withCtor(hydra.langs.scala.meta.Ctor_Primary ctor) {
    if (ctor == null) {
      throw new IllegalArgumentException("null value for 'ctor' argument");
    }
    return new Defn_Enum(mods, name, tparams, ctor, template);
  }
  
  public Defn_Enum withTemplate(hydra.langs.scala.meta.Template template) {
    if (template == null) {
      throw new IllegalArgumentException("null value for 'template' argument");
    }
    return new Defn_Enum(mods, name, tparams, ctor, template);
  }
}