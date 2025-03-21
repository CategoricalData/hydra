// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

/**
 * One of ADVISORY, ARTICLE, REPORT, FIX, GIT, PACKAGE, EVIDENCE, or WEB
 */
public class ReferenceType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.dev.osv.schema.ReferenceType");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public ReferenceType (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReferenceType)) {
      return false;
    }
    ReferenceType o = (ReferenceType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}