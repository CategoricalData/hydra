// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * An EnumValue describes an element of an Enum.
 */
public class EnumValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.EnumValue");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ENUM_VALUE = new hydra.core.Name("enumValue");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  /**
   * The 'programming' name of the enum value. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$.
   */
  public final String name;
  
  /**
   * The on-the-wire value that maps to the EnumValue. EnumValue may be either an integer or a string and must be unique for all enum values in this enum.
   */
  public final hydra.ext.com.microsoft.azure.dtld.IntegerOrString enumValue;
  
  /**
   * The ID of the enum value. If no @id is provided, the digital twin interface processor will assign one.
   */
  public final hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id;
  
  /**
   * A comment for model authors
   */
  public final hydra.util.Opt<String> comment;
  
  /**
   * A localizable description for display
   */
  public final hydra.util.Opt<String> description;
  
  /**
   * A localizable name for display
   */
  public final hydra.util.Opt<String> displayName;
  
  public EnumValue (String name, hydra.ext.com.microsoft.azure.dtld.IntegerOrString enumValue, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((enumValue));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    this.name = name;
    this.enumValue = enumValue;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumValue)) {
      return false;
    }
    EnumValue o = (EnumValue) (other);
    return name.equals(o.name) && enumValue.equals(o.enumValue) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * enumValue.hashCode() + 5 * id.hashCode() + 7 * comment.hashCode() + 11 * description.hashCode() + 13 * displayName.hashCode();
  }
  
  public EnumValue withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new EnumValue(name, enumValue, id, comment, description, displayName);
  }
  
  public EnumValue withEnumValue(hydra.ext.com.microsoft.azure.dtld.IntegerOrString enumValue) {
    java.util.Objects.requireNonNull((enumValue));
    return new EnumValue(name, enumValue, id, comment, description, displayName);
  }
  
  public EnumValue withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new EnumValue(name, enumValue, id, comment, description, displayName);
  }
  
  public EnumValue withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new EnumValue(name, enumValue, id, comment, description, displayName);
  }
  
  public EnumValue withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new EnumValue(name, enumValue, id, comment, description, displayName);
  }
  
  public EnumValue withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new EnumValue(name, enumValue, id, comment, description, displayName);
  }
}