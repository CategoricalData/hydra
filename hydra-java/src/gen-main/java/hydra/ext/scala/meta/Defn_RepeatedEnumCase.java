// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Defn_RepeatedEnumCase implements Serializable, Comparable<Defn_RepeatedEnumCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Defn_RepeatedEnumCase");

  public static final hydra.core.Name MODS = new hydra.core.Name("mods");

  public static final hydra.core.Name CASES = new hydra.core.Name("cases");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Mod> mods;

  public final hydra.util.ConsList<hydra.ext.scala.meta.Data_Name> cases;

  public Defn_RepeatedEnumCase (hydra.util.ConsList<hydra.ext.scala.meta.Mod> mods, hydra.util.ConsList<hydra.ext.scala.meta.Data_Name> cases) {
    this.mods = mods;
    this.cases = cases;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_RepeatedEnumCase)) {
      return false;
    }
    Defn_RepeatedEnumCase o = (Defn_RepeatedEnumCase) other;
    return java.util.Objects.equals(
      this.mods,
      o.mods) && java.util.Objects.equals(
      this.cases,
      o.cases);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(mods) + 3 * java.util.Objects.hashCode(cases);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Defn_RepeatedEnumCase other) {
    int cmp = 0;
    cmp = ((Comparable) mods).compareTo(other.mods);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) cases).compareTo(other.cases);
  }

  public Defn_RepeatedEnumCase withMods(hydra.util.ConsList<hydra.ext.scala.meta.Mod> mods) {
    return new Defn_RepeatedEnumCase(mods, cases);
  }

  public Defn_RepeatedEnumCase withCases(hydra.util.ConsList<hydra.ext.scala.meta.Data_Name> cases) {
    return new Defn_RepeatedEnumCase(mods, cases);
  }
}
