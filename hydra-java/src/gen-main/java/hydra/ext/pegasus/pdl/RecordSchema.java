package hydra.ext.pegasus.pdl;

public class RecordSchema {
  public final java.util.List<RecordField> fields;
  
  public final java.util.List<NamedSchema> includes;
  
  public RecordSchema (java.util.List<RecordField> fields, java.util.List<NamedSchema> includes) {
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
  
  public RecordSchema withFields(java.util.List<RecordField> fields) {
    return new RecordSchema(fields, includes);
  }
  
  public RecordSchema withIncludes(java.util.List<NamedSchema> includes) {
    return new RecordSchema(fields, includes);
  }
}