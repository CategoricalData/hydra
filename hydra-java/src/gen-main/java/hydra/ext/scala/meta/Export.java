package hydra.ext.scala.meta;

public class Export {
  public final java.util.List<Importer> importers;
  
  public Export (java.util.List<Importer> importers) {
    this.importers = importers;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Export)) {
      return false;
    }
    Export o = (Export) (other);
    return importers.equals(o.importers);
  }
  
  @Override
  public int hashCode() {
    return 2 * importers.hashCode();
  }
}