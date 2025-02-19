// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_If implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_If");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_THENP = new hydra.core.Name("thenp");
  
  public static final hydra.core.Name FIELD_NAME_ELSEP = new hydra.core.Name("elsep");
  
  public final hydra.ext.scala.meta.Data cond;
  
  public final hydra.ext.scala.meta.Data thenp;
  
  public final hydra.ext.scala.meta.Data elsep;
  
  public Data_If (hydra.ext.scala.meta.Data cond, hydra.ext.scala.meta.Data thenp, hydra.ext.scala.meta.Data elsep) {
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((thenp));
    java.util.Objects.requireNonNull((elsep));
    this.cond = cond;
    this.thenp = thenp;
    this.elsep = elsep;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_If)) {
      return false;
    }
    Data_If o = (Data_If) (other);
    return cond.equals(o.cond) && thenp.equals(o.thenp) && elsep.equals(o.elsep);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * thenp.hashCode() + 5 * elsep.hashCode();
  }
  
  public Data_If withCond(hydra.ext.scala.meta.Data cond) {
    java.util.Objects.requireNonNull((cond));
    return new Data_If(cond, thenp, elsep);
  }
  
  public Data_If withThenp(hydra.ext.scala.meta.Data thenp) {
    java.util.Objects.requireNonNull((thenp));
    return new Data_If(cond, thenp, elsep);
  }
  
  public Data_If withElsep(hydra.ext.scala.meta.Data elsep) {
    java.util.Objects.requireNonNull((elsep));
    return new Data_If(cond, thenp, elsep);
  }
}