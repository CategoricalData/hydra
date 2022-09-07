package hydra.ext.pegasus.pdl;

public class EnumFieldName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.EnumFieldName");
  
  public final String value;
  
  public EnumFieldName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumFieldName)) {
      return false;
    }
    EnumFieldName o = (EnumFieldName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}