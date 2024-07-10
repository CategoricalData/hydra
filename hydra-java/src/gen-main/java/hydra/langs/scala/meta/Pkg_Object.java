// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Pkg_Object implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Pkg.Object");
  
  public final java.util.List<hydra.langs.scala.meta.Mod> mods;
  
  public final hydra.langs.scala.meta.Data_Name name;
  
  public final hydra.langs.scala.meta.Template template;
  
  public Pkg_Object (java.util.List<hydra.langs.scala.meta.Mod> mods, hydra.langs.scala.meta.Data_Name name, hydra.langs.scala.meta.Template template) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (template == null) {
      throw new IllegalArgumentException("null value for 'template' argument");
    }
    this.mods = mods;
    this.name = name;
    this.template = template;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pkg_Object)) {
      return false;
    }
    Pkg_Object o = (Pkg_Object) (other);
    return mods.equals(o.mods) && name.equals(o.name) && template.equals(o.template);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * template.hashCode();
  }
  
  public Pkg_Object withMods(java.util.List<hydra.langs.scala.meta.Mod> mods) {
    if (mods == null) {
      throw new IllegalArgumentException("null value for 'mods' argument");
    }
    return new Pkg_Object(mods, name, template);
  }
  
  public Pkg_Object withName(hydra.langs.scala.meta.Data_Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Pkg_Object(mods, name, template);
  }
  
  public Pkg_Object withTemplate(hydra.langs.scala.meta.Template template) {
    if (template == null) {
      throw new IllegalArgumentException("null value for 'template' argument");
    }
    return new Pkg_Object(mods, name, template);
  }
}