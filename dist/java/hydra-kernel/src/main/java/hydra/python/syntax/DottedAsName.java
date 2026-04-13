// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class DottedAsName implements Serializable, Comparable<DottedAsName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.DottedAsName");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public final hydra.python.syntax.DottedName name;

  public final hydra.util.Maybe<hydra.python.syntax.Name> as;

  public DottedAsName (hydra.python.syntax.DottedName name, hydra.util.Maybe<hydra.python.syntax.Name> as) {
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
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      as,
      other.as);
  }

  public DottedAsName withName(hydra.python.syntax.DottedName name) {
    return new DottedAsName(name, as);
  }

  public DottedAsName withAs(hydra.util.Maybe<hydra.python.syntax.Name> as) {
    return new DottedAsName(name, as);
  }
}
