// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions
 */
public class DatatypeRestriction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/owl/syntax.DatatypeRestriction");
  
  public static final hydra.core.Name FIELD_NAME_DATATYPE = new hydra.core.Name("datatype");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public final hydra.langs.owl.syntax.Datatype datatype;
  
  public final java.util.List<hydra.langs.owl.syntax.DatatypeRestriction_Constraint> constraints;
  
  public DatatypeRestriction (hydra.langs.owl.syntax.Datatype datatype, java.util.List<hydra.langs.owl.syntax.DatatypeRestriction_Constraint> constraints) {
    java.util.Objects.requireNonNull((datatype));
    java.util.Objects.requireNonNull((constraints));
    this.datatype = datatype;
    this.constraints = constraints;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatatypeRestriction)) {
      return false;
    }
    DatatypeRestriction o = (DatatypeRestriction) (other);
    return datatype.equals(o.datatype) && constraints.equals(o.constraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * datatype.hashCode() + 3 * constraints.hashCode();
  }
  
  public DatatypeRestriction withDatatype(hydra.langs.owl.syntax.Datatype datatype) {
    java.util.Objects.requireNonNull((datatype));
    return new DatatypeRestriction(datatype, constraints);
  }
  
  public DatatypeRestriction withConstraints(java.util.List<hydra.langs.owl.syntax.DatatypeRestriction_Constraint> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new DatatypeRestriction(datatype, constraints);
  }
}