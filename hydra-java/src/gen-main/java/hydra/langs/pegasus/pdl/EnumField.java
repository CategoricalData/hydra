// Note: this is an automatically generated file. Do not edit.

package hydra.langs.pegasus.pdl;

import java.io.Serializable;

public class EnumField implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/pegasus/pdl.EnumField");
  
  public final hydra.langs.pegasus.pdl.EnumFieldName name;
  
  public final hydra.langs.pegasus.pdl.Annotations annotations;
  
  public EnumField (hydra.langs.pegasus.pdl.EnumFieldName name, hydra.langs.pegasus.pdl.Annotations annotations) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
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
  
  public EnumField withName(hydra.langs.pegasus.pdl.EnumFieldName name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new EnumField(name, annotations);
  }
  
  public EnumField withAnnotations(hydra.langs.pegasus.pdl.Annotations annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new EnumField(name, annotations);
  }
}