package hydra.ext.graphql.syntax;

public class DirectiveLocations_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.DirectiveLocations.Sequence");
  
  public final hydra.ext.graphql.syntax.DirectiveLocations directiveLocations;
  
  public final hydra.ext.graphql.syntax.DirectiveLocation directiveLocation;
  
  public DirectiveLocations_Sequence (hydra.ext.graphql.syntax.DirectiveLocations directiveLocations, hydra.ext.graphql.syntax.DirectiveLocation directiveLocation) {
    this.directiveLocations = directiveLocations;
    this.directiveLocation = directiveLocation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DirectiveLocations_Sequence)) {
      return false;
    }
    DirectiveLocations_Sequence o = (DirectiveLocations_Sequence) (other);
    return directiveLocations.equals(o.directiveLocations) && directiveLocation.equals(o.directiveLocation);
  }
  
  @Override
  public int hashCode() {
    return 2 * directiveLocations.hashCode() + 3 * directiveLocation.hashCode();
  }
  
  public DirectiveLocations_Sequence withDirectiveLocations(hydra.ext.graphql.syntax.DirectiveLocations directiveLocations) {
    return new DirectiveLocations_Sequence(directiveLocations, directiveLocation);
  }
  
  public DirectiveLocations_Sequence withDirectiveLocation(hydra.ext.graphql.syntax.DirectiveLocation directiveLocation) {
    return new DirectiveLocations_Sequence(directiveLocations, directiveLocation);
  }
}