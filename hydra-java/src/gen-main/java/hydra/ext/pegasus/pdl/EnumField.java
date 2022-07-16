package hydra.ext.pegasus.pdl;

public class EnumField {
  public final EnumFieldName name;
  
  public final Annotations annotations;
  
  public EnumField (EnumFieldName name, Annotations annotations) {
    this.name = name;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumField)) {
      return false;
    }
    EnumField o = (EnumField) (other);
    return name.equals(o.name) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * annotations.hashCode();
  }
  
  public EnumField withName(EnumFieldName name) {
    return new EnumField(name, annotations);
  }
  
  public EnumField withAnnotations(Annotations annotations) {
    return new EnumField(name, annotations);
  }
}