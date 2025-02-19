// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TPrimaryAndName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndName");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.ext.python.syntax.TPrimary primary;
  
  public final hydra.ext.python.syntax.Name name;
  
  public TPrimaryAndName (hydra.ext.python.syntax.TPrimary primary, hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((primary));
    java.util.Objects.requireNonNull((name));
    this.primary = primary;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TPrimaryAndName)) {
      return false;
    }
    TPrimaryAndName o = (TPrimaryAndName) (other);
    return primary.equals(o.primary) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * name.hashCode();
  }
  
  public TPrimaryAndName withPrimary(hydra.ext.python.syntax.TPrimary primary) {
    java.util.Objects.requireNonNull((primary));
    return new TPrimaryAndName(primary, name);
  }
  
  public TPrimaryAndName withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new TPrimaryAndName(primary, name);
  }
}