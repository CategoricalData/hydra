package hydra.ext.scala.meta;

public class Defn_GivenAlias {
  public final java.util.List<Mod> mods;
  
  public final Name name;
  
  public final java.util.List<java.util.List<Type_Param>> tparams;
  
  public final java.util.List<java.util.List<Data_Param>> sparams;
  
  public final Type decltpe;
  
  public final Data body;
  
  public Defn_GivenAlias (java.util.List<Mod> mods, Name name, java.util.List<java.util.List<Type_Param>> tparams, java.util.List<java.util.List<Data_Param>> sparams, Type decltpe, Data body) {
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
  
  public Defn_GivenAlias withMods(java.util.List<Mod> mods) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withName(Name name) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withTparams(java.util.List<java.util.List<Type_Param>> tparams) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withSparams(java.util.List<java.util.List<Data_Param>> sparams) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withDecltpe(Type decltpe) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
  
  public Defn_GivenAlias withBody(Data body) {
    return new Defn_GivenAlias(mods, name, tparams, sparams, decltpe, body);
  }
}