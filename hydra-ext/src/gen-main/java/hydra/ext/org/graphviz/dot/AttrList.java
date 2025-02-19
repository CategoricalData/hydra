// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public class AttrList implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphviz.dot.AttrList");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<java.util.List<hydra.ext.org.graphviz.dot.EqualityPair>> value;
  
  public AttrList (java.util.List<java.util.List<hydra.ext.org.graphviz.dot.EqualityPair>> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AttrList)) {
      return false;
    }
    AttrList o = (AttrList) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}