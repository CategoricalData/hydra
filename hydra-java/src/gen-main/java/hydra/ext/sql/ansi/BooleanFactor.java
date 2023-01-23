package hydra.ext.sql.ansi;

public class BooleanFactor {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.BooleanFactor");
  
  public final java.util.Optional<java.lang.Void> nOT;
  
  public final hydra.ext.sql.ansi.BooleanTest booleanTest;
  
  public BooleanFactor (java.util.Optional<java.lang.Void> nOT, hydra.ext.sql.ansi.BooleanTest booleanTest) {
    this.nOT = nOT;
    this.booleanTest = booleanTest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BooleanFactor)) {
      return false;
    }
    BooleanFactor o = (BooleanFactor) (other);
    return nOT.equals(o.nOT) && booleanTest.equals(o.booleanTest);
  }
  
  @Override
  public int hashCode() {
    return 2 * nOT.hashCode() + 3 * booleanTest.hashCode();
  }
  
  public BooleanFactor withNOT(java.util.Optional<java.lang.Void> nOT) {
    return new BooleanFactor(nOT, booleanTest);
  }
  
  public BooleanFactor withBooleanTest(hydra.ext.sql.ansi.BooleanTest booleanTest) {
    return new BooleanFactor(nOT, booleanTest);
  }
}