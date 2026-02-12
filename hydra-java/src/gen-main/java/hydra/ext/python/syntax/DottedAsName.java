// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class DottedAsName implements Serializable, Comparable<DottedAsName> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.DottedAsName");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.python.syntax.DottedName name;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Name> as;
  
  public DottedAsName (hydra.ext.python.syntax.DottedName name, hydra.util.Maybe<hydra.ext.python.syntax.Name> as) {
    this.name = name;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DottedAsName)) {
      return false;
    }
    DottedAsName o = (DottedAsName) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.as,
      o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(as);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DottedAsName other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      as.hashCode(),
      other.as.hashCode());
  }
  
  public DottedAsName withName(hydra.ext.python.syntax.DottedName name) {
    return new DottedAsName(name, as);
  }
  
  public DottedAsName withAs(hydra.util.Maybe<hydra.ext.python.syntax.Name> as) {
    return new DottedAsName(name, as);
  }
}
