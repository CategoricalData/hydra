package hydra.ext.pegasus.pdl;

public class RecordField {
  public final FieldName name;
  
  public final Schema value;
  
  public final Boolean optional;
  
  public final java.util.Optional<hydra.ext.json.model.Value> default_;
  
  public final Annotations annotations;
  
  public RecordField (FieldName name, Schema value, Boolean optional, java.util.Optional<hydra.ext.json.model.Value> default_, Annotations annotations) {
    this.name = name;
    this.value = value;
    this.optional = optional;
    this.default_ = default_;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordField)) {
      return false;
    }
    RecordField o = (RecordField) (other);
    return name.equals(o.name) && value.equals(o.value) && optional.equals(o.optional) && default_.equals(o.default_) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode() + 5 * optional.hashCode() + 7 * default_.hashCode() + 11 * annotations.hashCode();
  }
  
  public RecordField withName(FieldName name) {
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withValue(Schema value) {
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withOptional(Boolean optional) {
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withDefault(java.util.Optional<hydra.ext.json.model.Value> default_) {
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withAnnotations(Annotations annotations) {
    return new RecordField(name, value, optional, default_, annotations);
  }
}