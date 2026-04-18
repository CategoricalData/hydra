// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions
 */
public class DatatypeRestriction implements Serializable, Comparable<DatatypeRestriction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.DatatypeRestriction");

  public static final hydra.core.Name DATATYPE = new hydra.core.Name("datatype");

  public static final hydra.core.Name CONSTRAINTS = new hydra.core.Name("constraints");

  public final hydra.owl.syntax.Datatype datatype;

  public final java.util.List<hydra.owl.syntax.DatatypeRestriction_Constraint> constraints;

  public DatatypeRestriction (hydra.owl.syntax.Datatype datatype, java.util.List<hydra.owl.syntax.DatatypeRestriction_Constraint> constraints) {
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
    cmp = hydra.util.Comparing.compare(
      datatype,
      other.datatype);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      constraints,
      other.constraints);
  }

  public DatatypeRestriction withDatatype(hydra.owl.syntax.Datatype datatype) {
    return new DatatypeRestriction(datatype, constraints);
  }

  public DatatypeRestriction withConstraints(java.util.List<hydra.owl.syntax.DatatypeRestriction_Constraint> constraints) {
    return new DatatypeRestriction(datatype, constraints);
  }
}
