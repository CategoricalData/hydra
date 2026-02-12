// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class PatternCaptureTarget implements Serializable, Comparable<PatternCaptureTarget> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.PatternCaptureTarget");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.Name value;
  
  public PatternCaptureTarget (hydra.ext.python.syntax.Name value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternCaptureTarget)) {
      return false;
    }
    PatternCaptureTarget o = (PatternCaptureTarget) other;
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
  public int compareTo(PatternCaptureTarget other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
