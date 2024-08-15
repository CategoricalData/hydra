// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Type_Method implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Type.Method");
  
  public static final hydra.core.Name FIELD_NAME_PARAMSS = new hydra.core.Name("paramss");
  
  public static final hydra.core.Name FIELD_NAME_TPE = new hydra.core.Name("tpe");
  
  public final java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss;
  
  public final hydra.langs.scala.meta.Type tpe;
  
  public Type_Method (java.util.List<java.util.List<hydra.langs.scala.meta.Data_Param>> paramss, hydra.langs.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((paramss));
    java.util.Objects.requireNonNull((tpe));
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
    java.util.Objects.requireNonNull((paramss));
    return new Type_Method(paramss, tpe);
  }
  
  public Type_Method withTpe(hydra.langs.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tpe));
    return new Type_Method(paramss, tpe);
  }
}