// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Defn_RepeatedEnumCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Defn_RepeatedEnumCase");
  
  public static final hydra.core.Name FIELD_NAME_MODS = new hydra.core.Name("mods");
  
  public static final hydra.core.Name FIELD_NAME_CASES = new hydra.core.Name("cases");
  
  public final java.util.List<hydra.ext.scala.meta.Mod> mods;
  
  public final java.util.List<hydra.ext.scala.meta.Data_Name> cases;
  
  public Defn_RepeatedEnumCase (java.util.List<hydra.ext.scala.meta.Mod> mods, java.util.List<hydra.ext.scala.meta.Data_Name> cases) {
    java.util.Objects.requireNonNull((mods));
    java.util.Objects.requireNonNull((cases));
    this.mods = mods;
    this.cases = cases;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_RepeatedEnumCase)) {
      return false;
    }
    Defn_RepeatedEnumCase o = (Defn_RepeatedEnumCase) (other);
    return mods.equals(o.mods) && cases.equals(o.cases);
  }
  
  @Override
  public int hashCode() {
    return 2 * mods.hashCode() + 3 * cases.hashCode();
  }
  
  public Defn_RepeatedEnumCase withMods(java.util.List<hydra.ext.scala.meta.Mod> mods) {
    java.util.Objects.requireNonNull((mods));
    return new Defn_RepeatedEnumCase(mods, cases);
  }
  
  public Defn_RepeatedEnumCase withCases(java.util.List<hydra.ext.scala.meta.Data_Name> cases) {
    java.util.Objects.requireNonNull((cases));
    return new Defn_RepeatedEnumCase(mods, cases);
  }
}