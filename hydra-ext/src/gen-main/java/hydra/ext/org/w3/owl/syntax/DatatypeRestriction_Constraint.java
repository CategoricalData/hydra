// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DatatypeRestriction_Constraint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/owl/syntax.DatatypeRestriction.Constraint");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINING_FACET = new hydra.core.Name("constrainingFacet");
  
  public static final hydra.core.Name FIELD_NAME_RESTRICTION_VALUE = new hydra.core.Name("restrictionValue");
  
  public final hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet constrainingFacet;
  
  public final hydra.ext.org.w3.rdf.syntax.Literal restrictionValue;
  
  public DatatypeRestriction_Constraint (hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet constrainingFacet, hydra.ext.org.w3.rdf.syntax.Literal restrictionValue) {
    java.util.Objects.requireNonNull((constrainingFacet));
    java.util.Objects.requireNonNull((restrictionValue));
    this.constrainingFacet = constrainingFacet;
    this.restrictionValue = restrictionValue;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatatypeRestriction_Constraint)) {
      return false;
    }
    DatatypeRestriction_Constraint o = (DatatypeRestriction_Constraint) (other);
    return constrainingFacet.equals(o.constrainingFacet) && restrictionValue.equals(o.restrictionValue);
  }
  
  @Override
  public int hashCode() {
    return 2 * constrainingFacet.hashCode() + 3 * restrictionValue.hashCode();
  }
  
  public DatatypeRestriction_Constraint withConstrainingFacet(hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet constrainingFacet) {
    java.util.Objects.requireNonNull((constrainingFacet));
    return new DatatypeRestriction_Constraint(constrainingFacet, restrictionValue);
  }
  
  public DatatypeRestriction_Constraint withRestrictionValue(hydra.ext.org.w3.rdf.syntax.Literal restrictionValue) {
    java.util.Objects.requireNonNull((restrictionValue));
    return new DatatypeRestriction_Constraint(constrainingFacet, restrictionValue);
  }
}
