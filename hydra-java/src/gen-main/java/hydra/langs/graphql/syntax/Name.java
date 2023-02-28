package hydra.langs.graphql.syntax;

public class Name {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.Name");
  
  public final String value;
  
  public Name (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Name)) {
      return false;
    }
    Name o = (Name) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}