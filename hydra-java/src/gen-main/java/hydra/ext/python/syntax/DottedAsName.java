// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class DottedAsName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.DottedAsName");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.python.syntax.DottedName name;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Name> as;
  
  public DottedAsName (hydra.ext.python.syntax.DottedName name, hydra.util.Opt<hydra.ext.python.syntax.Name> as) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((as));
    this.name = name;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DottedAsName)) {
      return false;
    }
    DottedAsName o = (DottedAsName) (other);
    return name.equals(o.name) && as.equals(o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * as.hashCode();
  }
  
  public DottedAsName withName(hydra.ext.python.syntax.DottedName name) {
    java.util.Objects.requireNonNull((name));
    return new DottedAsName(name, as);
  }
  
  public DottedAsName withAs(hydra.util.Opt<hydra.ext.python.syntax.Name> as) {
    java.util.Objects.requireNonNull((as));
    return new DottedAsName(name, as);
  }
}