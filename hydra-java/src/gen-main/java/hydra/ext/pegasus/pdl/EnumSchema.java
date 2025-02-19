// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

public class EnumSchema implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.pegasus.pdl.EnumSchema");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final java.util.List<hydra.ext.pegasus.pdl.EnumField> fields;
  
  public EnumSchema (java.util.List<hydra.ext.pegasus.pdl.EnumField> fields) {
    java.util.Objects.requireNonNull((fields));
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumSchema)) {
      return false;
    }
    EnumSchema o = (EnumSchema) (other);
    return fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * fields.hashCode();
  }
}