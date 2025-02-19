// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

public class EnumField implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.pegasus.pdl.EnumField");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public final hydra.ext.pegasus.pdl.EnumFieldName name;
  
  public final hydra.ext.pegasus.pdl.Annotations annotations;
  
  public EnumField (hydra.ext.pegasus.pdl.EnumFieldName name, hydra.ext.pegasus.pdl.Annotations annotations) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((annotations));
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
  
  public EnumField withName(hydra.ext.pegasus.pdl.EnumFieldName name) {
    java.util.Objects.requireNonNull((name));
    return new EnumField(name, annotations);
  }
  
  public EnumField withAnnotations(hydra.ext.pegasus.pdl.Annotations annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new EnumField(name, annotations);
  }
}