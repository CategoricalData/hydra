// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_ExtensionGroup implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.ExtensionGroup");
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> parmss;
  
  public final hydra.langs.scala.meta.Stat body;
  
  public Defn_ExtensionGroup (java.util.List<hydra.langs.scala.meta.Type_Param> tparams, java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> parmss, hydra.langs.scala.meta.Stat body) {
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    if (parmss == null) {
      throw new IllegalArgumentException("null value for 'parmss' argument");
    }
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
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
  
  public Defn_ExtensionGroup withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
  
  public Defn_ExtensionGroup withParmss(java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> parmss) {
    if (parmss == null) {
      throw new IllegalArgumentException("null value for 'parmss' argument");
    }
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
  
  public Defn_ExtensionGroup withBody(hydra.langs.scala.meta.Stat body) {
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
}