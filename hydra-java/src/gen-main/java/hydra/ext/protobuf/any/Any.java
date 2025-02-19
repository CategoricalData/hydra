// Note: this is an automatically generated file. Do not edit.

package hydra.ext.protobuf.any;

import java.io.Serializable;

/**
 * `Any` contains an arbitrary serialized protocol buffer message along with a URL that describes the type of the serialized message.
 */
public class Any implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.protobuf.any.Any");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_URL = new hydra.core.Name("typeUrl");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  /**
   * A URL/resource name that uniquely identifies the type of the serialized protocol buffer message.
   */
  public final String typeUrl;
  
  /**
   * Must be a valid serialized protocol buffer of the above specified type.
   */
  public final String value;
  
  public Any (String typeUrl, String value) {
    java.util.Objects.requireNonNull((typeUrl));
    java.util.Objects.requireNonNull((value));
    this.typeUrl = typeUrl;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Any)) {
      return false;
    }
    Any o = (Any) (other);
    return typeUrl.equals(o.typeUrl) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeUrl.hashCode() + 3 * value.hashCode();
  }
  
  public Any withTypeUrl(String typeUrl) {
    java.util.Objects.requireNonNull((typeUrl));
    return new Any(typeUrl, value);
  }
  
  public Any withValue(String value) {
    java.util.Objects.requireNonNull((value));
    return new Any(typeUrl, value);
  }
}