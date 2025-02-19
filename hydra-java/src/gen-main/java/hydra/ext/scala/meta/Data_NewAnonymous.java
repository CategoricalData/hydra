// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_NewAnonymous implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_NewAnonymous");
  
  public static final hydra.core.Name FIELD_NAME_TEMPL = new hydra.core.Name("templ");
  
  public final hydra.ext.scala.meta.Template templ;
  
  public Data_NewAnonymous (hydra.ext.scala.meta.Template templ) {
    java.util.Objects.requireNonNull((templ));
    this.templ = templ;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_NewAnonymous)) {
      return false;
    }
    Data_NewAnonymous o = (Data_NewAnonymous) (other);
    return templ.equals(o.templ);
  }
  
  @Override
  public int hashCode() {
    return 2 * templ.hashCode();
  }
}