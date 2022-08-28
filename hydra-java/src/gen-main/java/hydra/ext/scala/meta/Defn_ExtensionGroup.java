package hydra.ext.scala.meta;

public class Defn_ExtensionGroup {
  public final java.util.List<hydra.ext.scala.meta.Type_Param> tparams;
  
  public final java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> parmss;
  
  public final hydra.ext.scala.meta.Stat body;
  
  public Defn_ExtensionGroup (java.util.List<hydra.ext.scala.meta.Type_Param> tparams, java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> parmss, hydra.ext.scala.meta.Stat body) {
    this.tparams = tparams;
    this.parmss = parmss;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_ExtensionGroup)) {
      return false;
    }
    Defn_ExtensionGroup o = (Defn_ExtensionGroup) (other);
    return tparams.equals(o.tparams) && parmss.equals(o.parmss) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * tparams.hashCode() + 3 * parmss.hashCode() + 5 * body.hashCode();
  }
  
  public Defn_ExtensionGroup withTparams(java.util.List<hydra.ext.scala.meta.Type_Param> tparams) {
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
  
  public Defn_ExtensionGroup withParmss(java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> parmss) {
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
  
  public Defn_ExtensionGroup withBody(hydra.ext.scala.meta.Stat body) {
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
}