// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Defn_ExtensionGroup implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Defn_ExtensionGroup");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_PARMSS = new hydra.core.Name("parmss");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.scala.meta.Type_Param> tparams;
  
  public final java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> parmss;
  
  public final hydra.ext.scala.meta.Stat body;
  
  public Defn_ExtensionGroup (java.util.List<hydra.ext.scala.meta.Type_Param> tparams, java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> parmss, hydra.ext.scala.meta.Stat body) {
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((parmss));
    java.util.Objects.requireNonNull((body));
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
    java.util.Objects.requireNonNull((tparams));
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
  
  public Defn_ExtensionGroup withParmss(java.util.List<java.util.List<hydra.ext.scala.meta.Data_Param>> parmss) {
    java.util.Objects.requireNonNull((parmss));
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
  
  public Defn_ExtensionGroup withBody(hydra.ext.scala.meta.Stat body) {
    java.util.Objects.requireNonNull((body));
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
}