// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Type_Lambda implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Type.Lambda");
  
  public final java.util.List<hydra.langs.scala.meta.Type_Param> tparams;
  
  public final hydra.langs.scala.meta.Type tpe;
  
  public Type_Lambda (java.util.List<hydra.langs.scala.meta.Type_Param> tparams, hydra.langs.scala.meta.Type tpe) {
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    if (tpe == null) {
      throw new IllegalArgumentException("null value for 'tpe' argument");
    }
    this.tparams = tparams;
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Lambda)) {
      return false;
    }
    Type_Lambda o = (Type_Lambda) (other);
    return tparams.equals(o.tparams) && tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tparams.hashCode() + 3 * tpe.hashCode();
  }
  
  public Type_Lambda withTparams(java.util.List<hydra.langs.scala.meta.Type_Param> tparams) {
    if (tparams == null) {
      throw new IllegalArgumentException("null value for 'tparams' argument");
    }
    return new Type_Lambda(tparams, tpe);
  }
  
  public Type_Lambda withTpe(hydra.langs.scala.meta.Type tpe) {
    if (tpe == null) {
      throw new IllegalArgumentException("null value for 'tpe' argument");
    }
    return new Type_Lambda(tparams, tpe);
  }
}