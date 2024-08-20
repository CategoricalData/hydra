// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

public class RecordField implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.RecordField");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONAL = new hydra.core.Name("optional");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public final hydra.ext.pegasus.pdl.FieldName name;
  
  public final hydra.ext.pegasus.pdl.Schema value;
  
  public final Boolean optional;
  
  public final hydra.util.Opt<hydra.json.Value> default_;
  
  public final hydra.ext.pegasus.pdl.Annotations annotations;
  
  public RecordField (hydra.ext.pegasus.pdl.FieldName name, hydra.ext.pegasus.pdl.Schema value, Boolean optional, hydra.util.Opt<hydra.json.Value> default_, hydra.ext.pegasus.pdl.Annotations annotations) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((optional));
    java.util.Objects.requireNonNull((default_));
    java.util.Objects.requireNonNull((annotations));
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
  
  public RecordField withName(hydra.ext.pegasus.pdl.FieldName name) {
    java.util.Objects.requireNonNull((name));
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withValue(hydra.ext.pegasus.pdl.Schema value) {
    java.util.Objects.requireNonNull((value));
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withOptional(Boolean optional) {
    java.util.Objects.requireNonNull((optional));
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withDefault(hydra.util.Opt<hydra.json.Value> default_) {
    java.util.Objects.requireNonNull((default_));
    return new RecordField(name, value, optional, default_, annotations);
  }
  
  public RecordField withAnnotations(hydra.ext.pegasus.pdl.Annotations annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new RecordField(name, value, optional, default_, annotations);
  }
}
