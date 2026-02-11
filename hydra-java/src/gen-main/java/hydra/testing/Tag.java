// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A tag for categorizing test cases
 */
public class Tag implements Serializable, Comparable<Tag> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.Tag");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public Tag (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Tag)) {
      return false;
    }
    Tag o = (Tag) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Tag other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
