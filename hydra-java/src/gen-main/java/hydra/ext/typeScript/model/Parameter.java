// Note: this is an automatically generated file. Do not edit.

package hydra.ext.typeScript.model;

import java.io.Serializable;

public class Parameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.typeScript.model.Parameter");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final String name;
  
  public final hydra.ext.typeScript.model.Type type;
  
  public Parameter (String name, hydra.ext.typeScript.model.Type type) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((type));
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Parameter)) {
      return false;
    }
    Parameter o = (Parameter) (other);
    return name.equals(o.name) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode();
  }
  
  public Parameter withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Parameter(name, type);
  }
  
  public Parameter withType(hydra.ext.typeScript.model.Type type) {
    java.util.Objects.requireNonNull((type));
    return new Parameter(name, type);
  }
}