// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

public class RecordSchema implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.pegasus.pdl.RecordSchema");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public static final hydra.core.Name FIELD_NAME_INCLUDES = new hydra.core.Name("includes");
  
  public final java.util.List<hydra.ext.pegasus.pdl.RecordField> fields;
  
  public final java.util.List<hydra.ext.pegasus.pdl.NamedSchema> includes;
  
  public RecordSchema (java.util.List<hydra.ext.pegasus.pdl.RecordField> fields, java.util.List<hydra.ext.pegasus.pdl.NamedSchema> includes) {
    java.util.Objects.requireNonNull((fields));
    java.util.Objects.requireNonNull((includes));
    this.fields = fields;
    this.includes = includes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordSchema)) {
      return false;
    }
    RecordSchema o = (RecordSchema) (other);
    return fields.equals(o.fields) && includes.equals(o.includes);
  }
  
  @Override
  public int hashCode() {
    return 2 * fields.hashCode() + 3 * includes.hashCode();
  }
  
  public RecordSchema withFields(java.util.List<hydra.ext.pegasus.pdl.RecordField> fields) {
    java.util.Objects.requireNonNull((fields));
    return new RecordSchema(fields, includes);
  }
  
  public RecordSchema withIncludes(java.util.List<hydra.ext.pegasus.pdl.NamedSchema> includes) {
    java.util.Objects.requireNonNull((includes));
    return new RecordSchema(fields, includes);
  }
}