package hydra.langs.graphql.syntax;

public class NullValue {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.NullValue");
  
  public NullValue () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullValue)) {
      return false;
    }
    NullValue o = (NullValue) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}