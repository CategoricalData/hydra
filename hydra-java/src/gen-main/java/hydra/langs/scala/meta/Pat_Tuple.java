// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Pat_Tuple implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Pat.Tuple");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  public final java.util.List<hydra.langs.scala.meta.Pat> args;
  
  public Pat_Tuple (java.util.List<hydra.langs.scala.meta.Pat> args) {
    java.util.Objects.requireNonNull((args));
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Tuple)) {
      return false;
    }
    Pat_Tuple o = (Pat_Tuple) (other);
    return args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * args.hashCode();
  }
}