// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ImportFromAsName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ImportFromAsName");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Name> as;
  
  public ImportFromAsName (hydra.ext.python.syntax.Name name, hydra.util.Opt<hydra.ext.python.syntax.Name> as) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((as));
    this.name = name;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportFromAsName)) {
      return false;
    }
    ImportFromAsName o = (ImportFromAsName) (other);
    return name.equals(o.name) && as.equals(o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * as.hashCode();
  }
  
  public ImportFromAsName withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ImportFromAsName(name, as);
  }
  
  public ImportFromAsName withAs(hydra.util.Opt<hydra.ext.python.syntax.Name> as) {
    java.util.Objects.requireNonNull((as));
    return new ImportFromAsName(name, as);
  }
}