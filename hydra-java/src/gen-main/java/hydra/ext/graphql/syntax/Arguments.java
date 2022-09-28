package hydra.ext.graphql.syntax;

public class Arguments {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.Arguments");
  
  public final java.util.List<hydra.ext.graphql.syntax.Argument> listOfArgument;
  
  public Arguments (java.util.List<hydra.ext.graphql.syntax.Argument> listOfArgument) {
    this.listOfArgument = listOfArgument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Arguments)) {
      return false;
    }
    Arguments o = (Arguments) (other);
    return listOfArgument.equals(o.listOfArgument);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfArgument.hashCode();
  }
}