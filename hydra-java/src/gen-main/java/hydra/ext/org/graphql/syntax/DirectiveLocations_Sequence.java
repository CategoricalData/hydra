// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class DirectiveLocations_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.DirectiveLocations_Sequence");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVE_LOCATIONS = new hydra.core.Name("directiveLocations");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVE_LOCATION = new hydra.core.Name("directiveLocation");
  
  public final hydra.ext.org.graphql.syntax.DirectiveLocations directiveLocations;
  
  public final hydra.ext.org.graphql.syntax.DirectiveLocation directiveLocation;
  
  public DirectiveLocations_Sequence (hydra.ext.org.graphql.syntax.DirectiveLocations directiveLocations, hydra.ext.org.graphql.syntax.DirectiveLocation directiveLocation) {
    java.util.Objects.requireNonNull((directiveLocations));
    java.util.Objects.requireNonNull((directiveLocation));
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
  
  public DirectiveLocations_Sequence withDirectiveLocations(hydra.ext.org.graphql.syntax.DirectiveLocations directiveLocations) {
    java.util.Objects.requireNonNull((directiveLocations));
    return new DirectiveLocations_Sequence(directiveLocations, directiveLocation);
  }
  
  public DirectiveLocations_Sequence withDirectiveLocation(hydra.ext.org.graphql.syntax.DirectiveLocation directiveLocation) {
    java.util.Objects.requireNonNull((directiveLocation));
    return new DirectiveLocations_Sequence(directiveLocations, directiveLocation);
  }
}