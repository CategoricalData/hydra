// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ImportFromAsName implements Serializable, Comparable<ImportFromAsName> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ImportFromAsName");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Name> as;
  
  public ImportFromAsName (hydra.ext.python.syntax.Name name, hydra.util.Maybe<hydra.ext.python.syntax.Name> as) {
    this.name = name;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportFromAsName)) {
      return false;
    }
    ImportFromAsName o = (ImportFromAsName) other;
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
  public int compareTo(ImportFromAsName other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      as.hashCode(),
      other.as.hashCode());
  }
  
  public ImportFromAsName withName(hydra.ext.python.syntax.Name name) {
    return new ImportFromAsName(name, as);
  }
  
  public ImportFromAsName withAs(hydra.util.Maybe<hydra.ext.python.syntax.Name> as) {
    return new ImportFromAsName(name, as);
  }
}
