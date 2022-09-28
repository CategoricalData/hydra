package hydra.ext.owl.syntax;

public class DatatypeRestriction_Constraint {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.DatatypeRestriction.Constraint");
  
  public final hydra.ext.owl.syntax.DatatypeRestriction_ConstrainingFacet constrainingFacet;
  
  public final hydra.ext.rdf.syntax.Literal restrictionValue;
  
  public DatatypeRestriction_Constraint (hydra.ext.owl.syntax.DatatypeRestriction_ConstrainingFacet constrainingFacet, hydra.ext.rdf.syntax.Literal restrictionValue) {
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
  
  public DatatypeRestriction_Constraint withConstrainingFacet(hydra.ext.owl.syntax.DatatypeRestriction_ConstrainingFacet constrainingFacet) {
    return new DatatypeRestriction_Constraint(constrainingFacet, restrictionValue);
  }
  
  public DatatypeRestriction_Constraint withRestrictionValue(hydra.ext.rdf.syntax.Literal restrictionValue) {
    return new DatatypeRestriction_Constraint(constrainingFacet, restrictionValue);
  }
}