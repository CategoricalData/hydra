// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.proto3;

import java.io.Serializable;

/**
 * Enum value definition
 */
public class EnumValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.proto3.EnumValue");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER = new hydra.core.Name("number");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONS = new hydra.core.Name("options");
  
  /**
   * Enum value name
   */
  public final hydra.ext.protobuf.proto3.EnumValueName name;
  
  /**
   * Enum value number
   */
  public final Integer number;
  
  /**
   * Protocol buffer options
   */
  public final java.util.List<hydra.ext.protobuf.proto3.Option> options;
  
  public EnumValue (hydra.ext.protobuf.proto3.EnumValueName name, Integer number, java.util.List<hydra.ext.protobuf.proto3.Option> options) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((number));
    java.util.Objects.requireNonNull((options));
    this.name = name;
    this.number = number;
    this.options = options;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumValue)) {
      return false;
    }
    EnumValue o = (EnumValue) (other);
    return name.equals(o.name) && number.equals(o.number) && options.equals(o.options);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * number.hashCode() + 5 * options.hashCode();
  }
  
  public EnumValue withName(hydra.ext.protobuf.proto3.EnumValueName name) {
    java.util.Objects.requireNonNull((name));
    return new EnumValue(name, number, options);
  }
  
  public EnumValue withNumber(Integer number) {
    java.util.Objects.requireNonNull((number));
    return new EnumValue(name, number, options);
  }
  
  public EnumValue withOptions(java.util.List<hydra.ext.protobuf.proto3.Option> options) {
    java.util.Objects.requireNonNull((options));
    return new EnumValue(name, number, options);
  }
}