package hydra.ext.scala.meta;

public class Pkg_Object {
  public final java.util.List<Mod> mods;
  
  public final Data_Name name;
  
  public final Template template;
  
  public Pkg_Object (java.util.List<Mod> mods, Data_Name name, Template template) {
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
  
  public Pkg_Object withMods(java.util.List<Mod> mods) {
    return new Pkg_Object(mods, name, template);
  }
  
  public Pkg_Object withName(Data_Name name) {
    return new Pkg_Object(mods, name, template);
  }
  
  public Pkg_Object withTemplate(Template template) {
    return new Pkg_Object(mods, name, template);
  }
}