// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_PolyFunction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Type_PolyFunction");
  
  public static final hydra.core.Name FIELD_NAME_TPARAMS = new hydra.core.Name("tparams");
  
  public static final hydra.core.Name FIELD_NAME_TPE = new hydra.core.Name("tpe");
  
  public final java.util.List<hydra.ext.scala.meta.Type_Param> tparams;
  
  public final hydra.ext.scala.meta.Type tpe;
  
  public Type_PolyFunction (java.util.List<hydra.ext.scala.meta.Type_Param> tparams, hydra.ext.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tparams));
    java.util.Objects.requireNonNull((tpe));
    this.tparams = tparams;
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_PolyFunction)) {
      return false;
    }
    Type_PolyFunction o = (Type_PolyFunction) (other);
    return tparams.equals(o.tparams) && tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tparams.hashCode() + 3 * tpe.hashCode();
  }
  
  public Type_PolyFunction withTparams(java.util.List<hydra.ext.scala.meta.Type_Param> tparams) {
    java.util.Objects.requireNonNull((tparams));
    return new Type_PolyFunction(tparams, tpe);
  }
  
  public Type_PolyFunction withTpe(hydra.ext.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tpe));
    return new Type_PolyFunction(tparams, tpe);
  }
}