package hydra.ext.graphql.syntax;

public class ExecutableDocument {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ExecutableDocument");
  
  public final java.util.List<hydra.ext.graphql.syntax.ExecutableDefinition> value;
  
  public ExecutableDocument (java.util.List<hydra.ext.graphql.syntax.ExecutableDefinition> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExecutableDocument)) {
      return false;
    }
    ExecutableDocument o = (ExecutableDocument) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}