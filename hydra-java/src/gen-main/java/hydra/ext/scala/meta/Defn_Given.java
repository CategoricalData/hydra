package hydra.ext.scala.meta;

public class Defn_Given {
  public final java.util.List<Mod> mods;
  
  public final Name name;
  
  public final java.util.List<java.util.List<Type_Param>> tparams;
  
  public final java.util.List<java.util.List<Data_Param>> sparams;
  
  public final Template templ;
  
  public Defn_Given (java.util.List<Mod> mods, Name name, java.util.List<java.util.List<Type_Param>> tparams, java.util.List<java.util.List<Data_Param>> sparams, Template templ) {
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
  
  public Defn_Given withMods(java.util.List<Mod> mods) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withName(Name name) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withTparams(java.util.List<java.util.List<Type_Param>> tparams) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withSparams(java.util.List<java.util.List<Data_Param>> sparams) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
  
  public Defn_Given withTempl(Template templ) {
    return new Defn_Given(mods, name, tparams, sparams, templ);
  }
}