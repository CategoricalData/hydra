// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Data_QuotedMacroType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Data.QuotedMacroType");
  
  public static final hydra.core.Name FIELD_NAME_TPE = new hydra.core.Name("tpe");
  
  public final hydra.langs.scala.meta.Type tpe;
  
  public Data_QuotedMacroType (hydra.langs.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tpe));
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_QuotedMacroType)) {
      return false;
    }
    Data_QuotedMacroType o = (Data_QuotedMacroType) (other);
    return tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode();
  }
}