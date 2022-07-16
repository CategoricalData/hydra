package hydra.ext.scala.meta;

public class Defn_RepeatedEnumCase {
  public final java.util.List<Mod> mods;
  
  public final java.util.List<Data_Name> cases;
  
  public Defn_RepeatedEnumCase (java.util.List<Mod> mods, java.util.List<Data_Name> cases) {
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
  
  public Defn_RepeatedEnumCase withMods(java.util.List<Mod> mods) {
    return new Defn_RepeatedEnumCase(mods, cases);
  }
  
  public Defn_RepeatedEnumCase withCases(java.util.List<Data_Name> cases) {
    return new Defn_RepeatedEnumCase(mods, cases);
  }
}