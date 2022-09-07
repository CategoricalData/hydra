package hydra.ext.java.syntax;

public class SingleTypeImportDeclaration {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.SingleTypeImportDeclaration");
  
  public final hydra.ext.java.syntax.TypeName value;
  
  public SingleTypeImportDeclaration (hydra.ext.java.syntax.TypeName value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleTypeImportDeclaration)) {
      return false;
    }
    SingleTypeImportDeclaration o = (SingleTypeImportDeclaration) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}