// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class DirectiveLocations_Sequence2 implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.DirectiveLocations.Sequence2");
  
  public final java.util.Optional<java.lang.Void> or;
  
  public final hydra.langs.graphql.syntax.DirectiveLocation directiveLocation;
  
  public DirectiveLocations_Sequence2 (java.util.Optional<java.lang.Void> or, hydra.langs.graphql.syntax.DirectiveLocation directiveLocation) {
    if (or == null) {
      throw new IllegalArgumentException("null value for 'or' argument");
    }
    if (directiveLocation == null) {
      throw new IllegalArgumentException("null value for 'directiveLocation' argument");
    }
    this.or = or;
    this.directiveLocation = directiveLocation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DirectiveLocations_Sequence2)) {
      return false;
    }
    DirectiveLocations_Sequence2 o = (DirectiveLocations_Sequence2) (other);
    return or.equals(o.or) && directiveLocation.equals(o.directiveLocation);
  }
  
  @Override
  public int hashCode() {
    return 2 * or.hashCode() + 3 * directiveLocation.hashCode();
  }
  
  public DirectiveLocations_Sequence2 withOr(java.util.Optional<java.lang.Void> or) {
    if (or == null) {
      throw new IllegalArgumentException("null value for 'or' argument");
    }
    return new DirectiveLocations_Sequence2(or, directiveLocation);
  }
  
  public DirectiveLocations_Sequence2 withDirectiveLocation(hydra.langs.graphql.syntax.DirectiveLocation directiveLocation) {
    if (directiveLocation == null) {
      throw new IllegalArgumentException("null value for 'directiveLocation' argument");
    }
    return new DirectiveLocations_Sequence2(or, directiveLocation);
  }
}