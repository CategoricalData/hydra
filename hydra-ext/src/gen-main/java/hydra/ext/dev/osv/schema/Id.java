// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

/**
 * A string of the format &lt;DB&gt;-&lt;ENTRYID&gt;, where DB names the database and ENTRYID is in the format used by the database. For example: OSV-2020-111, CVE-2021-3114, or GHSA-vp9c-fpxx-744v
 */
public class Id implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/dev/osv/schema.Id");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public Id (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Id)) {
      return false;
    }
    Id o = (Id) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}