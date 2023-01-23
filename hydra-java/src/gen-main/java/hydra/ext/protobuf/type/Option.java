package hydra.ext.protobuf.type;

/**
 * A protocol buffer option, which can be attached to a message, field, enumeration, etc.
 */
public class Option {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/protobuf/type.Option");
  
  /**
   * The option's name. For protobuf built-in options (options defined in descriptor.proto), this is the short name. For example, `"map_entry"`. For custom options, it should be the fully-qualified name. For example, `"google.api.http"`.
   */
  public final String name;
  
  /**
   * The option's value packed in an Any message. If the value is a primitive, the corresponding wrapper type defined in google/protobuf/wrappers.proto should be used. If the value is an enum, it should be stored as an int32 value using the google.protobuf.Int32Value type.
   */
  public final hydra.ext.protobuf.any.Any value;
  
  public Option (String name, hydra.ext.protobuf.any.Any value) {
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
    return new Option(name, value);
  }
  
  public Option withValue(hydra.ext.protobuf.any.Any value) {
    return new Option(name, value);
  }
}