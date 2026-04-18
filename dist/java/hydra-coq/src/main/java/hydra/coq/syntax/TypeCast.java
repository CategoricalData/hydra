// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class TypeCast implements Serializable, Comparable<TypeCast> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.TypeCast");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public final hydra.coq.syntax.Term10 term;

  public final hydra.coq.syntax.Type type;

  public final hydra.coq.syntax.TypeCastOperator operator;

  public TypeCast (hydra.coq.syntax.Term10 term, hydra.coq.syntax.Type type, hydra.coq.syntax.TypeCastOperator operator) {
    this.term = term;
    this.type = type;
    this.operator = operator;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeCast)) {
      return false;
    }
    TypeCast o = (TypeCast) other;
    return java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.operator,
      o.operator);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(term) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(operator);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeCast other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      term,
      other.term);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      operator,
      other.operator);
  }

  public TypeCast withTerm(hydra.coq.syntax.Term10 term) {
    return new TypeCast(term, type, operator);
  }

  public TypeCast withType(hydra.coq.syntax.Type type) {
    return new TypeCast(term, type, operator);
  }

  public TypeCast withOperator(hydra.coq.syntax.TypeCastOperator operator) {
    return new TypeCast(term, type, operator);
  }
}
