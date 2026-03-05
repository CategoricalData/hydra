// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DatatypeRestriction_Constraint implements Serializable, Comparable<DatatypeRestriction_Constraint> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint");
  
  public static final hydra.core.Name CONSTRAINING_FACET = new hydra.core.Name("constrainingFacet");
  
  public static final hydra.core.Name RESTRICTION_VALUE = new hydra.core.Name("restrictionValue");
  
  public final hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet constrainingFacet;
  
  public final hydra.ext.org.w3.rdf.syntax.Literal restrictionValue;
  
  public DatatypeRestriction_Constraint (hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet constrainingFacet, hydra.ext.org.w3.rdf.syntax.Literal restrictionValue) {
    this.constrainingFacet = constrainingFacet;
    this.restrictionValue = restrictionValue;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatatypeRestriction_Constraint)) {
      return false;
    }
    DatatypeRestriction_Constraint o = (DatatypeRestriction_Constraint) other;
    return java.util.Objects.equals(
      this.constrainingFacet,
      o.constrainingFacet) && java.util.Objects.equals(
      this.restrictionValue,
      o.restrictionValue);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(constrainingFacet) + 3 * java.util.Objects.hashCode(restrictionValue);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DatatypeRestriction_Constraint other) {
    int cmp = 0;
    cmp = ((Comparable) constrainingFacet).compareTo(other.constrainingFacet);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) restrictionValue).compareTo(other.restrictionValue);
  }
  
  public DatatypeRestriction_Constraint withConstrainingFacet(hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet constrainingFacet) {
    return new DatatypeRestriction_Constraint(constrainingFacet, restrictionValue);
  }
  
  public DatatypeRestriction_Constraint withRestrictionValue(hydra.ext.org.w3.rdf.syntax.Literal restrictionValue) {
    return new DatatypeRestriction_Constraint(constrainingFacet, restrictionValue);
  }
}
