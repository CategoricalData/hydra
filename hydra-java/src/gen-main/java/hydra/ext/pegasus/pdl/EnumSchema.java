package hydra.ext.pegasus.pdl;

public class EnumSchema {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.EnumSchema");
  
  public final java.util.List<hydra.ext.pegasus.pdl.EnumField> fields;
  
  public EnumSchema (java.util.List<hydra.ext.pegasus.pdl.EnumField> fields) {
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