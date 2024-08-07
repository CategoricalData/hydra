// Note: this is an automatically generated file. Do not edit.

package hydra.langs.xml.schema;

import java.io.Serializable;

public class Date implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/xml/schema.Date");
  
  public final String value;
  
  public Date (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Date)) {
      return false;
    }
    Date o = (Date) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}