package hydra.ext.java.syntax;

public class SingleTypeImportDeclaration {
  public final TypeName value;
  
  public SingleTypeImportDeclaration (TypeName value) {
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