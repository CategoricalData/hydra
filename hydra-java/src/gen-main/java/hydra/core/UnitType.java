package hydra.core;

public class UnitType {
    public static final Name NAME = new Name("hydra/core.UnitType");

  public UnitType () {

  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnitType)) {
      return false;
    }
    UnitType o = (UnitType) (other);
    return true;
  }

  @Override
  public int hashCode() {
    return 0;
  }
}