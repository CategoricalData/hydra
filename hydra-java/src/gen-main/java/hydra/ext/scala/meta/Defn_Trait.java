package hydra.ext.scala.meta;

public class Defn_Trait {
  public final java.util.List<Mod> mods;
  
  public final Type_Name name;
  
  public final java.util.List<Type_Param> tparams;
  
  public final Ctor_Primary ctor;
  
  public final Template template;
  
  public Defn_Trait (java.util.List<Mod> mods, Type_Name name, java.util.List<Type_Param> tparams, Ctor_Primary ctor, Template template) {
    this.mods = mods;
    this.name = name;
    this.tparams = tparams;
    this.ctor = ctor;
    this.template = template;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Trait)) {
      return false;
    }
    Defn_Trait o = (Defn_Trait) (other);
    return mods.equals(o.mods) && name.equals(o.name) && tparams.equals(o.tparams) && ctor.equals(o.ctor) && template.equals(o.template);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * name.hashCode() + 5 * tparams.hashCode() + 7 * ctor.hashCode() + 11 * template.hashCode();
  }
  
  public Defn_Trait withMods(java.util.List<Mod> mods) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }
  
  public Defn_Trait withName(Type_Name name) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }
  
  public Defn_Trait withTparams(java.util.List<Type_Param> tparams) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }
  
  public Defn_Trait withCtor(Ctor_Primary ctor) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }
  
  public Defn_Trait withTemplate(Template template) {
    return new Defn_Trait(mods, name, tparams, ctor, template);
  }
}