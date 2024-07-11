// Note: this is an automatically generated file. Do not edit.

package hydra.langs.pegasus.pdl;

import java.io.Serializable;

public class RecordField implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/pegasus/pdl.RecordField");
  
  public final hydra.langs.pegasus.pdl.FieldName name;
  
  public final hydra.langs.pegasus.pdl.Schema value;
  
  public final Boolean optional;
  
  public final hydra.util.Opt<hydra.json.Value> default_;
  
  public final hydra.langs.pegasus.pdl.Annotations annotations;
  
  public RecordField (hydra.langs.pegasus.pdl.FieldName name, hydra.langs.pegasus.pdl.Schema value, Boolean optional, hydra.util.Opt<hydra.json.Value> default_, hydra.langs.pegasus.pdl.Annotations annotations) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    if (optional == null) {
      throw new IllegalArgumentException("null value for 'optional' argument");
    }
    if (default_ == null) {
      throw new IllegalArgumentException("null value for 'default' argument");
    }
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
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
  
  public RecordField withName(hydra.langs.pegasus.pdl.FieldName name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withValue(hydra.langs.pegasus.pdl.Schema value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withOptional(Boolean optional) {
    if (optional == null) {
      throw new IllegalArgumentException("null value for 'optional' argument");
    }
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withDefault(hydra.util.Opt<hydra.json.Value> default_) {
    if (default_ == null) {
      throw new IllegalArgumentException("null value for 'default' argument");
    }
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withAnnotations(hydra.langs.pegasus.pdl.Annotations annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new RecordField(name, value, optional, default_, annotations);
  }
}