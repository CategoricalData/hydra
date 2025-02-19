// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.proto3;

import java.io.Serializable;

/**
 * A single field of a message type
 */
public class Field implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.proto3.Field");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_JSON_NAME = new hydra.core.Name("jsonName");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER = new hydra.core.Name("number");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONS = new hydra.core.Name("options");
  
  /**
   * The field name
   */
  public final hydra.ext.protobuf.proto3.FieldName name;
  
  /**
   * The field JSON name
   */
  public final hydra.util.Opt<String> jsonName;
  
  /**
   * The datatype of the field
   */
  public final hydra.ext.protobuf.proto3.FieldType type;
  
  /**
   * The field number
   */
  public final Integer number;
  
  /**
   * The protocol buffer options
   */
  public final java.util.List<hydra.ext.protobuf.proto3.Option> options;
  
  public Field (hydra.ext.protobuf.proto3.FieldName name, hydra.util.Opt<String> jsonName, hydra.ext.protobuf.proto3.FieldType type, Integer number, java.util.List<hydra.ext.protobuf.proto3.Option> options) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((jsonName));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((number));
    java.util.Objects.requireNonNull((options));
    this.name = name;
    this.jsonName = jsonName;
    this.type = type;
    this.number = number;
    this.options = options;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
      return false;
    }
    Field o = (Field) (other);
    return name.equals(o.name) && jsonName.equals(o.jsonName) && type.equals(o.type) && number.equals(o.number) && options.equals(o.options);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * jsonName.hashCode() + 5 * type.hashCode() + 7 * number.hashCode() + 11 * options.hashCode();
  }
  
  public Field withName(hydra.ext.protobuf.proto3.FieldName name) {
    java.util.Objects.requireNonNull((name));
    return new Field(name, jsonName, type, number, options);
  }
  
  public Field withJsonName(hydra.util.Opt<String> jsonName) {
    java.util.Objects.requireNonNull((jsonName));
    return new Field(name, jsonName, type, number, options);
  }
  
  public Field withType(hydra.ext.protobuf.proto3.FieldType type) {
    java.util.Objects.requireNonNull((type));
    return new Field(name, jsonName, type, number, options);
  }
  
  public Field withNumber(Integer number) {
    java.util.Objects.requireNonNull((number));
    return new Field(name, jsonName, type, number, options);
  }
  
  public Field withOptions(java.util.List<hydra.ext.protobuf.proto3.Option> options) {
    java.util.Objects.requireNonNull((options));
    return new Field(name, jsonName, type, number, options);
  }
}