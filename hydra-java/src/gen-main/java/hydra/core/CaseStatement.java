// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A union elimination; a case statement
 */
public class CaseStatement implements Serializable, Comparable<CaseStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.CaseStatement");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public static final hydra.core.Name FIELD_NAME_CASES = new hydra.core.Name("cases");
  
  /**
   * The name of the union type
   */
  public final hydra.core.Name typeName;
  
  /**
   * An optional default case, used if none of the explicit cases match
   */
  public final hydra.util.Maybe<hydra.core.Term> default_;
  
  /**
   * A list of case alternatives, one per union field
   */
  public final java.util.List<hydra.core.Field> cases;
  
  public CaseStatement (hydra.core.Name typeName, hydra.util.Maybe<hydra.core.Term> default_, java.util.List<hydra.core.Field> cases) {
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
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.default_,
      o.default_) && java.util.Objects.equals(
      this.cases,
      o.cases);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(default_) + 5 * java.util.Objects.hashCode(cases);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CaseStatement other) {
    int cmp = 0;
    cmp = ((Comparable) (typeName)).compareTo(other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      default_.hashCode(),
      other.default_.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      cases.hashCode(),
      other.cases.hashCode());
  }
  
  public CaseStatement withTypeName(hydra.core.Name typeName) {
    return new CaseStatement(typeName, default_, cases);
  }
  
  public CaseStatement withDefault(hydra.util.Maybe<hydra.core.Term> default_) {
    return new CaseStatement(typeName, default_, cases);
  }
  
  public CaseStatement withCases(java.util.List<hydra.core.Field> cases) {
    return new CaseStatement(typeName, default_, cases);
  }
}
