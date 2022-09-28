package hydra.ext.graphql.syntax;

public class FragmentName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.FragmentName");
  
  public final hydra.ext.graphql.syntax.Name value;
  
  public FragmentName (hydra.ext.graphql.syntax.Name value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FragmentName)) {
      return false;
    }
    FragmentName o = (FragmentName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}