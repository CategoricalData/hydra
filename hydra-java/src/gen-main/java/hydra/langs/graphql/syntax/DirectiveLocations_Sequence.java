// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class DirectiveLocations_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.DirectiveLocations.Sequence");
  
  public final hydra.langs.graphql.syntax.DirectiveLocations directiveLocations;
  
  public final hydra.langs.graphql.syntax.DirectiveLocation directiveLocation;
  
  public DirectiveLocations_Sequence (hydra.langs.graphql.syntax.DirectiveLocations directiveLocations, hydra.langs.graphql.syntax.DirectiveLocation directiveLocation) {
    if (directiveLocations == null) {
      throw new IllegalArgumentException("null value for 'directiveLocations' argument");
    }
    if (directiveLocation == null) {
      throw new IllegalArgumentException("null value for 'directiveLocation' argument");
    }
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
  
  public DirectiveLocations_Sequence withDirectiveLocations(hydra.langs.graphql.syntax.DirectiveLocations directiveLocations) {
    if (directiveLocations == null) {
      throw new IllegalArgumentException("null value for 'directiveLocations' argument");
    }
    return new DirectiveLocations_Sequence(directiveLocations, directiveLocation);
  }
  
  public DirectiveLocations_Sequence withDirectiveLocation(hydra.langs.graphql.syntax.DirectiveLocation directiveLocation) {
    if (directiveLocation == null) {
      throw new IllegalArgumentException("null value for 'directiveLocation' argument");
    }
    return new DirectiveLocations_Sequence(directiveLocations, directiveLocation);
  }
}