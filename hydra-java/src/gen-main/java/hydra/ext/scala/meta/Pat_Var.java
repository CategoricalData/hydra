// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Var implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Pat_Var");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.ext.scala.meta.Data_Name name;
  
  public Pat_Var (hydra.ext.scala.meta.Data_Name name) {
    java.util.Objects.requireNonNull((name));
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Var)) {
      return false;
    }
    Pat_Var o = (Pat_Var) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}