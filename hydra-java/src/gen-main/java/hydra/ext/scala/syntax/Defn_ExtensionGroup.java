// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Defn_ExtensionGroup implements Serializable, Comparable<Defn_ExtensionGroup> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Defn_ExtensionGroup");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name PARMSS = new hydra.core.Name("parmss");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final java.util.List<hydra.ext.scala.syntax.Type_Param> tparams;

  public final java.util.List<java.util.List<hydra.ext.scala.syntax.Data_Param>> parmss;

  public final hydra.ext.scala.syntax.Stat body;

  public Defn_ExtensionGroup (java.util.List<hydra.ext.scala.syntax.Type_Param> tparams, java.util.List<java.util.List<hydra.ext.scala.syntax.Data_Param>> parmss, hydra.ext.scala.syntax.Stat body) {
    this.tparams = tparams;
    this.parmss = parmss;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_ExtensionGroup)) {
      return false;
    }
    Defn_ExtensionGroup o = (Defn_ExtensionGroup) other;
    return java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.parmss,
      o.parmss) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tparams) + 3 * java.util.Objects.hashCode(parmss) + 5 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Defn_ExtensionGroup other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      tparams,
      other.tparams);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      parmss,
      other.parmss);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public Defn_ExtensionGroup withTparams(java.util.List<hydra.ext.scala.syntax.Type_Param> tparams) {
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }

  public Defn_ExtensionGroup withParmss(java.util.List<java.util.List<hydra.ext.scala.syntax.Data_Param>> parmss) {
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }

  public Defn_ExtensionGroup withBody(hydra.ext.scala.syntax.Stat body) {
    return new Defn_ExtensionGroup(tparams, parmss, body);
  }
}
