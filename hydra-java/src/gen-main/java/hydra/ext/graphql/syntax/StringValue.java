package hydra.ext.graphql.syntax;

public class StringValue {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.StringValue");
  
  public final String value;
  
  public StringValue (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringValue)) {
      return false;
    }
    StringValue o = (StringValue) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}