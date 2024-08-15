// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_Class implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.Class");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_CTOR = new hydra.core.Name("ctor");
  
  public static final hydra.core.Name FIELD_NAME_TEMPLATE = new hydra.core.Name("template");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Type_Name name;
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final hydra.langs.scala.meta.Ctor_Primary ctor;
  
  public final hydra.langs.scala.meta.Template template;
  
  public Defn_Class (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Type_Name name, java.util.List<hydra.langs.scala.meta.Type_Param> tparams, hydra.langs.scala.meta.Ctor_Primary ctor, hydra.langs.scala.meta.Template template) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((ctor));
    java.util.Objects.requireNonNull((template));
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.ctor = ctor;
    this.template = template;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Class)) {
      return false;
    }
    Defn_Class o = (Defn_Class) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && ctor.equals(o.ctor) && template.equals(o.template);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * ctor.hashCode() + 11 * template.hashCode();
  }
  
  public Defn_Class withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
  
  public Defn_Class withName(hydra.langs.scala.meta.Type_Name name) {
    java.util.Objects.requireNonNull((name));
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
  
  public Defn_Class withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
  
  public Defn_Class withCtor(hydra.langs.scala.meta.Ctor_Primary ctor) {
    java.util.Objects.requireNonNull((ctor));
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
  
  public Defn_Class withTemplate(hydra.langs.scala.meta.Template template) {
    java.util.Objects.requireNonNull((template));
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
}