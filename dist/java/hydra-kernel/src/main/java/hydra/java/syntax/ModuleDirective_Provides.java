// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ModuleDirective_Provides implements Serializable, Comparable<ModuleDirective_Provides> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ModuleDirective_Provides");

  public static final hydra.core.Name TO = new hydra.core.Name("to");

  public static final hydra.core.Name WITH = new hydra.core.Name("with");

  public final hydra.java.syntax.TypeName to;

  /**
   * At least one type
   */
  public final java.util.List<hydra.java.syntax.TypeName> with;

  public ModuleDirective_Provides (hydra.java.syntax.TypeName to, java.util.List<hydra.java.syntax.TypeName> with) {
    this.to = to;
    this.with = with;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleDirective_Provides)) {
      return false;
    }
    ModuleDirective_Provides o = (ModuleDirective_Provides) other;
    return java.util.Objects.equals(
      this.to,
      o.to) && java.util.Objects.equals(
      this.with,
      o.with);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(to) + 3 * java.util.Objects.hashCode(with);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModuleDirective_Provides other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      to,
      other.to);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      with,
      other.with);
  }

  public ModuleDirective_Provides withTo(hydra.java.syntax.TypeName to) {
    return new ModuleDirective_Provides(to, with);
  }

  public ModuleDirective_Provides withWith(java.util.List<hydra.java.syntax.TypeName> with) {
    return new ModuleDirective_Provides(to, with);
  }
}
