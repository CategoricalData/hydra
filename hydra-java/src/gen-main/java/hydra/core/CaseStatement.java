// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A union elimination; a case statement
 */
public class CaseStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/core.CaseStatement");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public static final hydra.core.Name FIELD_NAME_CASES = new hydra.core.Name("cases");
  
  public final hydra.core.Name typeName;
  
  public final hydra.util.Opt<hydra.core.Term> default_;
  
  public final java.util.List<hydra.core.Field> cases;
  
  public CaseStatement (hydra.core.Name typeName, hydra.util.Opt<hydra.core.Term> default_, java.util.List<hydra.core.Field> cases) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((default_));
    java.util.Objects.requireNonNull((cases));
    this.typeName = typeName;
    this.default_ = default_;
    this.cases = cases;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseStatement)) {
      return false;
    }
    CaseStatement o = (CaseStatement) (other);
    return typeName.equals(o.typeName) && default_.equals(o.default_) && cases.equals(o.cases);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * default_.hashCode() + 5 * cases.hashCode();
  }
  
  public CaseStatement withTypeName(hydra.core.Name typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new CaseStatement(typeName, default_, cases);
  }
  
  public CaseStatement withDefault(hydra.util.Opt<hydra.core.Term> default_) {
    java.util.Objects.requireNonNull((default_));
    return new CaseStatement(typeName, default_, cases);
  }
  
  public CaseStatement withCases(java.util.List<hydra.core.Field> cases) {
    java.util.Objects.requireNonNull((cases));
    return new CaseStatement(typeName, default_, cases);
  }
}