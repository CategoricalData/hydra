package hydra.langs.java.syntax;

public class TypeIdentifier {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.TypeIdentifier");
  
  public final hydra.langs.java.syntax.Identifier value;
  
  public TypeIdentifier (hydra.langs.java.syntax.Identifier value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeIdentifier)) {
      return false;
    }
    TypeIdentifier o = (TypeIdentifier) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}