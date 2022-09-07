package hydra.ext.java.syntax;

public class StaticImportOnDemandDeclaration {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.StaticImportOnDemandDeclaration");
  
  public final hydra.ext.java.syntax.TypeName value;
  
  public StaticImportOnDemandDeclaration (hydra.ext.java.syntax.TypeName value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StaticImportOnDemandDeclaration)) {
      return false;
    }
    StaticImportOnDemandDeclaration o = (StaticImportOnDemandDeclaration) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}