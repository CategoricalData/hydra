// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Tuple implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.Tuple");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  public final java.util.List<hydra.ext.scala.meta.Data> args;
  
  public Data_Tuple (java.util.List<hydra.ext.scala.meta.Data> args) {
    java.util.Objects.requireNonNull((args));
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Tuple)) {
      return false;
    }
    Data_Tuple o = (Data_Tuple) (other);
    return args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * args.hashCode();
  }
}
