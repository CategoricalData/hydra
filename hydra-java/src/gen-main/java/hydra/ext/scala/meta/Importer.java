package hydra.ext.scala.meta;

public class Importer {
  public final Data_Ref ref;
  
  public final java.util.List<Importee> importees;
  
  public Importer (Data_Ref ref, java.util.List<Importee> importees) {
    this.ref = ref;
    this.importees = importees;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importer)) {
      return false;
    }
    Importer o = (Importer) (other);
    return ref.equals(o.ref) && importees.equals(o.importees);
  }
  
  @Override
  public int hashCode() {
    return 2 * ref.hashCode() + 3 * importees.hashCode();
  }
  
  public Importer withRef(Data_Ref ref) {
    return new Importer(ref, importees);
  }
  
  public Importer withImportees(java.util.List<Importee> importees) {
    return new Importer(ref, importees);
  }
}