// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class StartActions implements Serializable, Comparable<StartActions> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.StartActions");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.ext.io.shex.syntax.CodeDecl> value;

  public StartActions (java.util.List<hydra.ext.io.shex.syntax.CodeDecl> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StartActions)) {
      return false;
    }
    StartActions o = (StartActions) other;
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
  public int compareTo(StartActions other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
