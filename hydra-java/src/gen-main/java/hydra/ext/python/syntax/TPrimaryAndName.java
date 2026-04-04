// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TPrimaryAndName implements Serializable, Comparable<TPrimaryAndName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndName");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.ext.python.syntax.TPrimary primary;

  public final hydra.ext.python.syntax.Name name;

  public TPrimaryAndName (hydra.ext.python.syntax.TPrimary primary, hydra.ext.python.syntax.Name name) {
    this.primary = primary;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TPrimaryAndName)) {
      return false;
    }
    TPrimaryAndName o = (TPrimaryAndName) other;
    return java.util.Objects.equals(
      this.primary,
      o.primary) && java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(primary) + 3 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TPrimaryAndName other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      primary,
      other.primary);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }

  public TPrimaryAndName withPrimary(hydra.ext.python.syntax.TPrimary primary) {
    return new TPrimaryAndName(primary, name);
  }

  public TPrimaryAndName withName(hydra.ext.python.syntax.Name name) {
    return new TPrimaryAndName(primary, name);
  }
}
