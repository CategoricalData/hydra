package hydra.ext.scala.meta;

public class Defn_Class {
  public final java.util.List<Mod> mods;
  
  public final Type_Name name;
  
  public final java.util.List<Type_Param> tparams;
  
  public final Ctor_Primary ctor;
  
  public final Template template;
  
  public Defn_Class (java.util.List<Mod> mods, Type_Name name, java.util.List<Type_Param> tparams, Ctor_Primary ctor, Template template) {
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
  
  public Defn_Class withMods(java.util.List<Mod> mods) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
  
  public Defn_Class withName(Type_Name name) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
  
  public Defn_Class withTparams(java.util.List<Type_Param> tparams) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
  
  public Defn_Class withCtor(Ctor_Primary ctor) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
  
  public Defn_Class withTemplate(Template template) {
    return new Defn_Class(mods, name, tparams, ctor, template);
  }
}