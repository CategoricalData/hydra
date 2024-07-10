// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Type_Method implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Type.Method");
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss;
  
  public final hydra.langs.scala.meta.Type tpe;
  
  public Type_Method (java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss, hydra.langs.scala.meta.Type tpe) {
    if (paramss == null) {
      throw new IllegalArgumentException("null value for 'paramss' argument");
    }
    if (tpe == null) {
      throw new IllegalArgumentException("null value for 'tpe' argument");
    }
    this.paramss = paramss;
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Method)) {
      return false;
    }
    Type_Method o = (Type_Method) (other);
    return paramss.equals(o.paramss) && tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramss.hashCode() + 3 * tpe.hashCode();
  }
  
  public Type_Method withParamss(java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss) {
    if (paramss == null) {
      throw new IllegalArgumentException("null value for 'paramss' argument");
    }
    return new Type_Method(paramss, tpe);
  }
  
  public Type_Method withTpe(hydra.langs.scala.meta.Type tpe) {
    if (tpe == null) {
      throw new IllegalArgumentException("null value for 'tpe' argument");
    }
    return new Type_Method(paramss, tpe);
  }
}