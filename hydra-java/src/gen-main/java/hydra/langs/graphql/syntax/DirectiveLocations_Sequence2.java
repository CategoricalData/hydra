// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class DirectiveLocations_Sequence2 implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.DirectiveLocations.Sequence2");
  
  public final hydra.util.Opt<java.lang.Void> or;
  
  public final hydra.langs.graphql.syntax.DirectiveLocation directiveLocation;
  
  public DirectiveLocations_Sequence2 (hydra.util.Opt<java.lang.Void> or, hydra.langs.graphql.syntax.DirectiveLocation directiveLocation) {
    java.util.Objects.requireNonNull((or));
    java.util.Objects.requireNonNull((directiveLocation));
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
  
  public DirectiveLocations_Sequence2 withOr(hydra.util.Opt<java.lang.Void> or) {
    java.util.Objects.requireNonNull((or));
    return new DirectiveLocations_Sequence2(or, directiveLocation);
  }
  
  public DirectiveLocations_Sequence2 withDirectiveLocation(hydra.langs.graphql.syntax.DirectiveLocation directiveLocation) {
    java.util.Objects.requireNonNull((directiveLocation));
    return new DirectiveLocations_Sequence2(or, directiveLocation);
  }
}