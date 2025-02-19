// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.proto3;

import java.io.Serializable;

/**
 * A protocol buffer option, which can be attached to a message, field, enumeration, etc
 */
public class Option implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.proto3.Option");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  /**
   * The option's name. For protobuf built-in options (options defined in descriptor.proto), this is the short name. For example, `"map_entry"`. For custom options, it should be the fully-qualified name. For example, `"google.api.http"`.
   */
  public final String name;
  
  /**
   * The option's value
   */
  public final hydra.ext.protobuf.proto3.Value value;
  
  public Option (String name, hydra.ext.protobuf.proto3.Value value) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Option)) {
      return false;
    }
    Option o = (Option) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public Option withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Option(name, value);
  }
  
  public Option withValue(hydra.ext.protobuf.proto3.Value value) {
    java.util.Objects.requireNonNull((value));
    return new Option(name, value);
  }
}