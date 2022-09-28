package hydra.ext.graphql.syntax;

public class Directives {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.Directives");
  
  public final java.util.List<hydra.ext.graphql.syntax.Directive> value;
  
  public Directives (java.util.List<hydra.ext.graphql.syntax.Directive> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Directives)) {
      return false;
    }
    Directives o = (Directives) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}