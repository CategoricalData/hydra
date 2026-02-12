// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class CapturePattern implements Serializable, Comparable<CapturePattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.CapturePattern");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.PatternCaptureTarget value;
  
  public CapturePattern (hydra.ext.python.syntax.PatternCaptureTarget value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CapturePattern)) {
      return false;
    }
    CapturePattern o = (CapturePattern) other;
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
  public int compareTo(CapturePattern other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
