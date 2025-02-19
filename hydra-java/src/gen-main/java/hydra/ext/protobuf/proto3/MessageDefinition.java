// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.proto3;

import java.io.Serializable;

/**
 * A protocol buffer message type
 */
public class MessageDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.proto3.MessageDefinition");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONS = new hydra.core.Name("options");
  
  /**
   * The fully qualified message name
   */
  public final hydra.ext.protobuf.proto3.TypeName name;
  
  /**
   * The list of fields
   */
  public final java.util.List<hydra.ext.protobuf.proto3.Field> fields;
  
  /**
   * The protocol buffer options
   */
  public final java.util.List<hydra.ext.protobuf.proto3.Option> options;
  
  public MessageDefinition (hydra.ext.protobuf.proto3.TypeName name, java.util.List<hydra.ext.protobuf.proto3.Field> fields, java.util.List<hydra.ext.protobuf.proto3.Option> options) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((fields));
    java.util.Objects.requireNonNull((options));
    this.name = name;
    this.fields = fields;
    this.options = options;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MessageDefinition)) {
      return false;
    }
    MessageDefinition o = (MessageDefinition) (other);
    return name.equals(o.name) && fields.equals(o.fields) && options.equals(o.options);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode() + 5 * options.hashCode();
  }
  
  public MessageDefinition withName(hydra.ext.protobuf.proto3.TypeName name) {
    java.util.Objects.requireNonNull((name));
    return new MessageDefinition(name, fields, options);
  }
  
  public MessageDefinition withFields(java.util.List<hydra.ext.protobuf.proto3.Field> fields) {
    java.util.Objects.requireNonNull((fields));
    return new MessageDefinition(name, fields, options);
  }
  
  public MessageDefinition withOptions(java.util.List<hydra.ext.protobuf.proto3.Option> options) {
    java.util.Objects.requireNonNull((options));
    return new MessageDefinition(name, fields, options);
  }
}