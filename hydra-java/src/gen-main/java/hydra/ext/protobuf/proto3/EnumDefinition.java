// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.proto3;

import java.io.Serializable;

/**
 * Enum type definition
 */
public class EnumDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/protobuf/proto3.EnumDefinition");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUES = new hydra.core.Name("values");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONS = new hydra.core.Name("options");
  
  /**
   * Enum type name
   */
  public final hydra.ext.protobuf.proto3.TypeName name;
  
  /**
   * Enum value definitions
   */
  public final java.util.List<hydra.ext.protobuf.proto3.EnumValue> values;
  
  /**
   * Protocol buffer options
   */
  public final java.util.List<hydra.ext.protobuf.proto3.Option> options;
  
  public EnumDefinition (hydra.ext.protobuf.proto3.TypeName name, java.util.List<hydra.ext.protobuf.proto3.EnumValue> values, java.util.List<hydra.ext.protobuf.proto3.Option> options) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((values));
    java.util.Objects.requireNonNull((options));
    this.name = name;
    this.values = values;
    this.options = options;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumDefinition)) {
      return false;
    }
    EnumDefinition o = (EnumDefinition) (other);
    return name.equals(o.name) && values.equals(o.values) && options.equals(o.options);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * values.hashCode() + 5 * options.hashCode();
  }
  
  public EnumDefinition withName(hydra.ext.protobuf.proto3.TypeName name) {
    java.util.Objects.requireNonNull((name));
    return new EnumDefinition(name, values, options);
  }
  
  public EnumDefinition withValues(java.util.List<hydra.ext.protobuf.proto3.EnumValue> values) {
    java.util.Objects.requireNonNull((values));
    return new EnumDefinition(name, values, options);
  }
  
  public EnumDefinition withOptions(java.util.List<hydra.ext.protobuf.proto3.Option> options) {
    java.util.Objects.requireNonNull((options));
    return new EnumDefinition(name, values, options);
  }
}
