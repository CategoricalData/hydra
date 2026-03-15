// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class IriRef implements Serializable, Comparable<IriRef> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.IriRef");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.IriRef_Elmt> value;
  
  public IriRef (hydra.util.ConsList<hydra.ext.io.shex.syntax.IriRef_Elmt> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IriRef)) {
      return false;
    }
    IriRef o = (IriRef) other;
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
  public int compareTo(IriRef other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
