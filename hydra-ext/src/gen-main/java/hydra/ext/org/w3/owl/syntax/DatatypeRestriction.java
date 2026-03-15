// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions
 */
public class DatatypeRestriction implements Serializable, Comparable<DatatypeRestriction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DatatypeRestriction");
  
  public static final hydra.core.Name DATATYPE = new hydra.core.Name("datatype");
  
  public static final hydra.core.Name CONSTRAINTS = new hydra.core.Name("constraints");
  
  public final hydra.ext.org.w3.owl.syntax.Datatype datatype;
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint> constraints;
  
  public DatatypeRestriction (hydra.ext.org.w3.owl.syntax.Datatype datatype, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint> constraints) {
    this.datatype = datatype;
    this.constraints = constraints;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatatypeRestriction)) {
      return false;
    }
    DatatypeRestriction o = (DatatypeRestriction) other;
    return java.util.Objects.equals(
      this.datatype,
      o.datatype) && java.util.Objects.equals(
      this.constraints,
      o.constraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(datatype) + 3 * java.util.Objects.hashCode(constraints);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DatatypeRestriction other) {
    int cmp = 0;
    cmp = ((Comparable) datatype).compareTo(other.datatype);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      constraints.hashCode(),
      other.constraints.hashCode());
  }
  
  public DatatypeRestriction withDatatype(hydra.ext.org.w3.owl.syntax.Datatype datatype) {
    return new DatatypeRestriction(datatype, constraints);
  }
  
  public DatatypeRestriction withConstraints(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint> constraints) {
    return new DatatypeRestriction(datatype, constraints);
  }
}
